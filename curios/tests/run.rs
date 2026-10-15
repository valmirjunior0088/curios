//! The native CLI selects a target before forwarding opaque arguments through the runtime.

use {
    curios_utilities::test_support::Temporary,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Stdio},
    },
};

const ECHO: &str = r#"
use /std/{List, Str, Nat, Bytes};
let render(b: Bytes) -> Str = match Str/of_bytes(b)
    | success(s) => s
    | failure(_) => Str/concat("bytes:", Nat/to_str(Bytes/to_nat(b)))
    end;
let entry() -> /std/Io({}) =
    let args = /std/proc/args()!;
    let tail = match args | [] => [] | [_, ..rest] => rest end;
    /std/print(Str/join("|", List/map(tail, render)));
entry()
"#;

fn package(root: &Path) {
    fs::create_dir_all(root).unwrap();
    fs::write(root.join("curios.toml"), "name = \"echo\"\ndefault = \"chosen\"\n[[executables]]\nname = \"chosen\"\n[[executables]]\nname = \"other\"\n").unwrap();
    fs::write(root.join("chosen.crs"), ECHO).unwrap();
    fs::write(root.join("other.crs"), "/std/print(\"wrong executable\")").unwrap();
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
        .current_dir(&root)
        .args(["run", "--"])
        .arg(OsString::from_vec(vec![0xff]))
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"bytes:255");
}
