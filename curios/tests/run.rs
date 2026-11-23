//! The native CLI selects a target before forwarding opaque arguments through the runtime.

use {
    curios::CABINET,
    curios_common::test_support::*,
    std::{
        fs,
        io::{BufRead, BufReader, Read, Write},
        path::Path,
        process::{Command, Stdio},
        sync::mpsc,
        thread,
        time::Duration,
    },
};

const ECHO: &str = r#"
use /std/{List, Str, Nat, Bytes};
let render(b: Bytes) -> Str = match Str/of_bytes(b)
    | success(s) => s
    | failure(_) => Str/concat("bytes:", Nat/to_str(Bytes/to_nat(b)))
    end;
let entry() -> /std/Io({}) =
    let args = /std/Proc/args()!;
    let tail = match args | [] => [] | [_, ..rest] => rest end;
    /std/print(Str/join("|", List/map(tail, render)));
entry()
"#;

fn package(root: &Path) {
    fs::create_dir_all(root).unwrap();
    fs::write(
        root.join("curios.toml"),
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"echo\"\ndefault = \"chosen\"\n[executables]\nchosen = {}\nother = {}\n"
        ),
    )
    .unwrap();
    fs::write(root.join("chosen.crs"), ECHO).unwrap();
    fs::write(root.join("other.crs"), "/std/print(\"wrong executable\")").unwrap();
}

#[test]
fn another_compiler_version_refuses_a_previously_cached_executable() {
    let root = Temporary::new("run", "compiler-version");
    package(&root);
    let run = || {
        Command::new(env!("CARGO_BIN_EXE_curios"))
            .env_remove(CABINET)
            .current_dir(&root)
            .arg("run")
            .output()
            .unwrap()
    };
    let first = run();
    assert!(first.status.success(), "{first:?}");
    let manifest = root.join("curios.toml");
    let major = env!("CARGO_PKG_VERSION")
        .split('.')
        .next()
        .unwrap()
        .parse::<u64>()
        .unwrap()
        + 1;
    let required = format!("{major}.0.0");
    let source = fs::read_to_string(&manifest).unwrap().replace(
        concat!("compiler = \"", env!("CARGO_PKG_VERSION"), "\""),
        &format!("compiler = \"{required}\""),
    );
    fs::write(&manifest, source).unwrap();
    let refused = run();
    assert!(!refused.status.success(), "{refused:?}");
    let error = String::from_utf8_lossy(&refused.stderr);
    assert!(
        error.contains(&format!("this package requires Curios `{required}`\n")),
        "{error}"
    );
    assert!(error.contains(env!("CARGO_PKG_VERSION")), "{error}");
    assert!(error.contains("curios.toml"), "{error}");
}

#[test]
fn the_default_and_explicit_programs_receive_their_own_options() {
    let root = Temporary::new("run", "arguments");
    package(&root);
    for (words, expected) in [
        (vec!["run", "--", "mock"], "mock"),
        (
            vec![
                "run",
                "chosen",
                "--help",
                "--budget",
                "0",
                "--manifest",
                "absent",
            ],
            "--help|--budget|0|--manifest|absent",
        ),
        (vec!["run", "chosen.crs", "--", "--version"], "--version"),
        (vec!["run", "--", "--", "mock"], "--|mock"),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_curios"))
            .env_remove(CABINET)
            .current_dir(&root)
            .args(words)
            .output()
            .unwrap();
        assert!(output.status.success(), "{output:?}");
        assert_eq!(output.stdout, expected.as_bytes());
    }
}

#[test]
fn a_program_on_standard_input_receives_its_trailing_arguments() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .args(["run", "-", "--", "mock", "--help"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(ECHO.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"mock|--help");
}

#[cfg(unix)]
#[test]
fn a_non_utf8_argument_reaches_the_default_program_as_bytes() {
    use std::{ffi::OsString, os::unix::ffi::OsStringExt};

    let root = Temporary::new("run", "bytes");
    package(&root);
    let output = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(&root)
        .args(["run", "--"])
        .arg(OsString::from_vec(vec![0xff]))
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"bytes:255");
}

/// A package of one executable, `main`, whose source is `program`.
fn single(root: &Path, program: &str) {
    fs::create_dir_all(root).unwrap();
    fs::write(
        root.join("curios.toml"),
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"streams\"\ndefault = \"main\"\n[executables]\nmain = {}\n"
        ),
    )
    .unwrap();
    fs::write(root.join("main.crs"), program).unwrap();
}

const FINISHED: &str = r#"
use /std/{Str, Try, Io, Proc};
let entry() -> Io({}) =
    let _ = Try/run(Io/write(Proc/stdout, Str/to_bytes("before\n")))!;
    let _ = Try/run(Io/finish(Proc/stdout))!;
    let input = Io/Reader/new(Proc/stdin)!;
    let _ = Try/run(Io/Reader/rest(input))!;
    Io/pure(());
entry()
"#;

// Finishing standard output lets the stream go while the program still runs: its reader reads the end before the program's own input has ended, which the program waits for. The read runs on a thread and is waited for with a bound, so a close that held the stream on fails rather than hangs.
#[test]
fn finishing_standard_output_gives_its_reader_the_end_while_the_program_runs() {
    let root = Temporary::new("run", "finished-output");
    single(&root, FINISHED);
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(&root)
        .arg("run")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let (sent, read) = mpsc::channel();
    thread::spawn(move || {
        let mut bytes = Vec::new();
        let _ = stdout.read_to_end(&mut bytes);
        let _ = sent.send(bytes);
    });
    let ended = read.recv_timeout(Duration::from_secs(120));
    drop(child.stdin.take());
    let output = child.wait_with_output().unwrap();
    assert_eq!(ended.as_deref(), Ok(&b"before\n"[..]), "{output:?}");
    assert!(output.status.success(), "{output:?}");
}

const PRINTED_AFTER: &str = r#"
use /std/{Try, Io, Proc, print};
let entry() -> Io({}) =
    let _ = Try/run(Io/finish(Proc/stdout))!;
    print("after");
entry()
"#;

// A print after standard output was finished meets a handle that names nothing: the program ends with 1, and standard error says why.
#[test]
fn a_print_after_standard_output_is_finished_ends_the_program_saying_why() {
    let root = Temporary::new("run", "printed-after");
    single(&root, PRINTED_AFTER);
    let output = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(&root)
        .arg("run")
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert_eq!(output.stdout, b"");
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("standard output could not be written: not_found"),
        "{output:?}"
    );
}

const CUT_LAST: &str = r#"
use /std/{Nat, Bytes, Str, Try, Io, Proc};
use /std/time/{Duration};
let grown(b: Bytes, n: Nat) -> Bytes = match n | 0 => b | m + 1 => grown(0x[..b, ..b], m) end;
let entry() -> Io({}) =
    let timed = Io/timeout(Duration/of_millis(10), Try/run(Io/write(Proc/stdout, grown(0x[0x61], 20))))!;
    let said = match timed | none() => "cut\n" | some(_) => "all\n" end;
    let _ = Try/run(Io/write(Proc/stderr, Str/to_bytes(said)))!;
    Io/pure(());
entry()
"#;

// A write to standard output a timeout cuts, the program's last, reaches its reader whole: the program's end hands on what the stream held. The reader reads only once the program has said on standard error, past the command's own progress lines, that the write was cut, so the pipe is full when it is; against an end that discarded what the stream held, it read the part the pipe took before the cut.
#[test]
fn a_cut_write_to_standard_output_reaches_its_reader_at_the_programs_end() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .args(["run", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(CUT_LAST.as_bytes())
        .unwrap();
    let said = BufReader::new(child.stderr.take().unwrap())
        .lines()
        .map(Result::unwrap)
        .find(|line| line == "cut" || line == "all");
    let mut written = Vec::new();
    child
        .stdout
        .take()
        .unwrap()
        .read_to_end(&mut written)
        .unwrap();
    let status = child.wait().unwrap();

    assert_eq!(said.as_deref(), Some("cut"));
    assert_eq!(written.len(), 1 << 20, "{status:?}");
    assert!(status.success(), "{status:?}");
}
