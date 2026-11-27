//! What `curios test` does at the command line: the governing package compiled as test programs, one instantiation per test, each test a node of its subject's tree closed by how it ended, what the test wrote of its own joined by what only the runner knows — the failing body, each subject's tally, the exit code — plus the filter, the cabinet round trip, and `run`'s indifference to it all.

use {
    curios::CABINET,
    curios_common::test_support::*,
    curios_native::test_support::*,
    std::{
        ffi::OsStr,
        fs,
        io::{Read, Write, pipe},
        os::unix::ffi::OsStrExt,
        path::Path,
        process::{Command, ExitStatus, Output, Stdio},
    },
};

/// A directory of its own, shared with no other test and gone with it.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-test", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// The six-outcome package: every rung the report can print, declared in the library, beside an executable with no tests of its own.
///
/// **The trapping row needs a narrowing that still refuses, and a value the folder cannot see.** `Nat` is unbounded at run time, so the only narrowing that refuses is the host wire's: the row hands `2⁷⁰` to `/std/rand/bytes`, whose count crosses as an `i64`, and the refusal fires before the host is called. The operand is tainted by an unset environment variable the way `numeric::test_support::table` taints its rows, since a closed computation never traps (`numeric::unbounded::a_closed_computation_folds_at_the_theory_s_width`); an unset variable rather than stdin because the harness inherits the test runner's stdin, and a read on a terminal would hang.
fn project(name: &str) -> Temporary {
    let root = temporary(name);
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Nat, Str, Bool, Bytes, Option, Io, Eq, Test};

pub let double(n: Nat) -> Nat => n * 2;

test doubling_passes =>
    Test/equal(double(21), 42);

test addition_passes =>
    Test/assert("one and one are two", 1 + 1 == 2);

test equality_fails =>
    let _ = /std/print_err("comparing\n")!; Test/equal(double(2), 5);

test overflow_traps =>
    let v = /std/Proc/env("CURIOS_UNSET_OVERFLOW")!; let _ = /std/rand/bytes(Nat/shl(1, 70) + Bytes/length(Option/unwrap_or(v, 0x[])))!; Test/assert("it holds", true);

test exits_seven =>
    let _ = /std/Proc/exit(@{}, 7)!; Test/assert("it holds", true);

test effect_passes =>
    let s = Io/pure("x")!; Test/equal(s, "x");
"#,
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    root
}

fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(root)
        .args(arguments)
        .output()
        .unwrap()
}

/// [`curios`], with `stdin` on the compiler's standard input.
fn fed(root: &Path, arguments: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(root)
        .args(arguments)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();

    child.wait_with_output().expect("the compiler exits")
}

/// [`fed`], with both of the compiler's output streams on one pipe, as a terminal shows them: how it exited, and what it wrote in the order it wrote it.
fn merged(root: &Path, arguments: &[&str], stdin: &str) -> (ExitStatus, String) {
    let (mut merged, writer) = pipe().unwrap();
    // The command holds the parent's copies of the write end until it is dropped, and the read below ends only once every copy is closed.
    let mut child = {
        let mut command = Command::new(env!("CARGO_BIN_EXE_curios"));
        command.env_remove(CABINET);
        command
            .current_dir(root)
            .args(arguments)
            .stdin(Stdio::piped())
            .stdout(writer.try_clone().unwrap())
            .stderr(writer);
        command.spawn().expect("run the compiler")
    };
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();

    let mut output = Vec::new();
    merged.read_to_end(&mut output).unwrap();
    let status = child.wait().unwrap();

    (status, String::from_utf8(output).unwrap())
}

fn stdout(output: &Output) -> String {
    String::from_utf8(output.stdout.clone()).unwrap()
}

fn stderr(output: &Output) -> String {
    String::from_utf8(output.stderr.clone()).unwrap()
}

/// Each needle's first occurrence sits after the previous one's — the report keeps declaration order.
fn in_order(haystack: &str, needles: &[&str]) {
    let mut from = 0;
    for needle in needles {
        match haystack[from..].find(needle) {
            Some(at) => from += at + needle.len(),
            None => panic!("missing {needle:?} (after byte {from}) in:\n{haystack}"),
        }
    }
}

#[test]
fn every_outcome_reports_in_declaration_order_and_exits_one() {
    let root = project("outcomes");
    let output = curios(&root, &["test"]);

    // The library's unit is checked with its tests and filed; its test program is checked, built and emitted, and its tests follow, a line as each begins and as it ends: what a test writes stands behind its path, its ending after that, closed where the test did not pass on where its name is written, and beneath its ending what only the runner holds — what a failing test found, its values in backticks, and a trap's text. The executable, whose library is compiled again without its tests, declares none, and closes with its tally at once. The run's tally is the last line.
    in_order(
        &stderr(&output),
        &[
            "Checking `/app` · tests\n",
            "Checked `/app` · tests in ",
            "Stored `/app` · tests in ",
            "Checked `/app` · tests in ",
            "Built `/app` · tests in ",
            "Emitted `/app` · tests in ",
            "Testing `/app/doubling_passes`\nTested `/app/doubling_passes`: passed in ",
            "Testing `/app/addition_passes`\nTested `/app/addition_passes`: passed in ",
            "Testing `/app/equality_fails`\n`/app/equality_fails`> comparing\nTested `/app/equality_fails`: failed in ",
            "; lib.crs:11:6\n",
            "  not equal\n    obtained: `4`\n    expected: `5`\n",
            "Tested `/app/overflow_traps`: trapped in ",
            "; lib.crs:14:6\n",
            "  panic: a Nat argument to a foreign function is past what the wire carries",
            "Tested `/app/exits_seven`: exited 7 in ",
            "; lib.crs:17:6\n",
            "Tested `/app/effect_passes`: passed in ",
            "Tested `/app` · tests: 3 passed, 1 failed, 1 trapped, 1 exited in ",
            "Checked `/app` in ",
            "Stored `/app` in ",
            "Checking `app` · tests\n",
            "Checked `app` · tests in ",
            "Tested `app` · tests: 0 passed, 0 failed\n",
        ],
    );
    assert!(
        stderr(&output)
            .lines()
            .last()
            .is_some_and(|last| last.starts_with("3 passed, 1 failed, 1 trapped, 1 exited; ")),
        "the run ends on its tally and what its tests took, stderr: {}",
        stderr(&output)
    );
    assert_eq!(
        stdout(&output),
        "",
        "the report is standard error's, and no test here writes"
    );
    assert_eq!(output.status.code(), Some(1));
}

/// An outcome is what a test program answers, so a test's own exit is never read as one: a test that exits before it reports has not passed at `0`, and at `1` is not the failure a report makes.
#[test]
fn a_test_that_exits_before_it_reports_is_exited_whatever_its_code() {
    let root = temporary("exits");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test, Proc};

test quits_early =>
    let _ = Proc/exit(@{}, 0)!; Test/assert("it holds", true);

test fails_quietly =>
    let _ = Proc/exit(@{}, 1)!; Test/assert("it holds", true);
"#,
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    let output = curios(&root, &["test"]);

    in_order(
        &stderr(&output),
        &[
            "Tested `/app/quits_early`: exited 0 in ",
            "; lib.crs:3:6\n",
            "Tested `/app/fails_quietly`: exited 1 in ",
            "; lib.crs:6:6\n",
            "Tested `/app` · tests: 0 passed, 0 failed, 2 exited in ",
        ],
    );
    assert_eq!(output.status.code(), Some(1));
}

/// What a test writes is never held: it reaches the stream the test wrote it on as it is written, each line behind the test's path, and the test's ending follows on a line of its own — after the end of a line the test left open. The two streams read apart show each its own, and read as one pipe show the test's lines between its beginning and its ending.
#[test]
fn a_test_that_writes_is_closed_beneath_what_it_wrote() {
    let root = temporary("writes");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test, print, print_err};

test quiet =>
    Test/assert("it holds", true);

test draws =>
    let _ = print("drawn\n")!; Test/assert("it holds", true);

test mutters =>
    let _ = print_err("half")!; Test/assert("it holds", true);
"#,
    );

    let apart = curios(&root, &["test"]);
    assert_eq!(
        stdout(&apart),
        "`/app/draws`> drawn\n",
        "stderr: {}",
        stderr(&apart)
    );
    in_order(
        &stderr(&apart),
        &[
            "Testing `/app/quiet`\nTested `/app/quiet`: passed in ",
            "Testing `/app/draws`\nTested `/app/draws`: passed in ",
            "Testing `/app/mutters`\n`/app/mutters`> half\nTested `/app/mutters`: passed in ",
            "Tested `/app` · tests: 3 passed, 0 failed in ",
        ],
    );

    let (mut merged, writer) = pipe().unwrap();
    // The command holds the parent's copies of the write end until it is dropped, and the read below ends only once every copy is closed.
    let mut child = {
        let mut command = Command::new(env!("CARGO_BIN_EXE_curios"));
        command.env_remove(CABINET);
        command
            .current_dir(&root)
            .arg("test")
            .stdout(writer.try_clone().unwrap())
            .stderr(writer);
        command.spawn().expect("run the compiler")
    };
    let mut together = String::new();
    merged.read_to_string(&mut together).unwrap();
    assert!(child.wait().unwrap().success());
    in_order(
        &together,
        &[
            "Testing `/app/quiet`\nTested `/app/quiet`: passed in ",
            "Testing `/app/draws`\n`/app/draws`> drawn\nTested `/app/draws`: passed in ",
            "Testing `/app/mutters`\n`/app/mutters`> half\nTested `/app/mutters`: passed in ",
        ],
    );
}

#[test]
fn a_filter_selects_by_path_prefix_and_a_second_run_reuses_the_payload() {
    let root = project("filter");

    let cold = curios(&root, &["test", "--filter", "/app/addition_passes"]);
    in_order(
        &stderr(&cold),
        &[
            "Emitted `/app` · tests in ",
            "Testing `/app/addition_passes`\nTested `/app/addition_passes`: passed in ",
            "Tested `/app` · tests: 1 passed, 0 failed, 5 skipped in ",
        ],
    );
    assert_eq!(cold.status.code(), Some(0));

    // Each payload comes back whole, so nothing of either subject's build is narrated: a subject says its tests and its tally.
    let warm = curios(&root, &["test", "--filter", "/app/addition_passes"]);
    in_order(
        &stderr(&warm),
        &[
            "Testing `/app/addition_passes`\nTested `/app/addition_passes`: passed in ",
            "Tested `/app` · tests: 1 passed, 0 failed, 5 skipped in ",
            "Tested `app` · tests: 0 passed, 0 failed\n",
            "1 passed, 0 failed, 5 skipped; ",
        ],
    );
    // No step is a check, of a unit with its tests or without.
    assert!(
        !stderr(&warm).contains("Checking"),
        "stderr: {}",
        stderr(&warm)
    );
}

/// A module's tests run together however its declarations interleave with another's, and a subject's tally counts the tests the filter left out, those of a module it selects nothing from included.
#[test]
fn a_modules_tests_run_together_and_the_tally_counts_what_the_filter_skipped() {
    let root = temporary("modules");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test};

test first =>
    Test/assert("it holds", true);

mod Inner
    use /std/{Test};

    test kept =>
        Test/assert("it holds", true);

    test left =>
        Test/assert("it holds", true);
end

test second =>
    Test/assert("it holds", true);

mod Left
    use /std/{Test};

    test out =>
        Test/assert("it holds", true);
end
"#,
    );

    let output = curios(
        &root,
        &[
            "test",
            "--filter",
            "/app/first",
            "--filter",
            "/app/second",
            "--filter",
            "/app/Inner/kept",
        ],
    );

    in_order(
        &stderr(&output),
        &[
            "Tested `/app/first`: passed in ",
            "Tested `/app/second`: passed in ",
            "Tested `/app/Inner/kept`: passed in ",
            "Tested `/app` · tests: 3 passed, 0 failed, 2 skipped in ",
        ],
    );
    // A run of one subject ends on that subject's tally, which is the run's.
    assert!(
        !stderr(&output).contains("\n3 passed"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn a_filter_matching_nothing_exits_one_naming_it() {
    let root = project("nomatch");
    let output = curios(&root, &["test", "--filter", "/nope"]);

    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr(&output).contains("unknown test\n│  no test matches `/nope`\n"),
        "stderr: {}",
        stderr(&output)
    );

    // A filter names whole segments, so the start of a test's name selects nothing, and the refusal names the paths nearest what was asked for, the test it is the start of first.
    let partial = curios(&root, &["test", "--filter", "/app/addition"]);
    assert_eq!(partial.status.code(), Some(1));
    assert!(
        stderr(&partial)
            .contains("no test matches `/app/addition`\n│  the nearest: `/app/addition_passes`"),
        "stderr: {}",
        stderr(&partial)
    );
}

/// A target narrows the run to what it selects, placed as every command places one: an executable by name runs its own tests and not its library's, and a file no unit declares runs its own against the prelude alone — as a module of its own when it has no final term.
#[test]
fn a_target_runs_only_the_tests_of_what_it_selects() {
    let root = temporary("targets");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Test};\n\ntest in_library =>\n    Test/assert(\"it holds\", true);\n",
    );
    write(
        &root,
        "app.crs",
        "use /std/{Test};\n\ntest in_app =>\n    Test/assert(\"it holds\", true);\n\n/std/print(\"ran\\n\")\n",
    );
    write(
        &root,
        "scratch/loose.crs",
        "use /std/{Test};\n\ntest in_loose =>\n    Test/assert(\"it holds\", true);\n",
    );

    let executable = curios(&root, &["test", "app"]);
    in_order(
        &stderr(&executable),
        &[
            "Checking `app` · tests\n",
            "Tested `/in_app`: passed in ",
            "Tested `app` · tests: 1 passed, 0 failed in ",
        ],
    );
    assert!(
        !stderr(&executable).contains("in_library"),
        "stderr: {}",
        stderr(&executable)
    );

    let loose = curios(&root, &["test", "scratch/loose.crs"]);
    in_order(
        &stderr(&loose),
        &[
            "Checking `scratch/loose.crs` · tests\n",
            "Tested `/loose/in_loose`: passed in ",
            "Tested `scratch/loose.crs` · tests: 1 passed, 0 failed in ",
        ],
    );
}

/// Text on standard input written as a module runs its own tests as a file written as one does — a unit of its own, mounted at `/stdin` since it has no stem — and is no program for `run` to run.
#[test]
fn a_module_on_standard_input_runs_its_own_tests_and_is_no_program_to_run() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Test};\n\ntest in_piped =>\n    Test/assert(\"it holds\", true);\n";

    let tested = fed(&root, &["test", "-"], module);
    in_order(
        &stderr(&tested),
        &[
            "Checking `<stdin>` · tests\n",
            "Tested `/stdin/in_piped`: passed in ",
            "Tested `<stdin>` · tests: 1 passed, 0 failed in ",
        ],
    );
    assert_eq!(tested.status.code(), Some(0));

    let ran = fed(&root, &["run", "-"], module);
    assert_eq!(ran.status.code(), Some(1));
    assert!(
        stderr(&ran).contains(
            "not a program\n│  this text is written as a module\n│  it has no final term to compile a program from\n└─ [error] <stdin>"
        ),
        "stderr: {}",
        stderr(&ran)
    );
}

/// A run where a test did not pass closes on where each such test is found, a line each in the order they ran, a blank line above and below, then on its tally, though one subject's tree closed on it already: the tests to go back to are the last thing a long run writes. A run where every test passed ends on its subject's tally alone.
#[test]
fn a_run_closes_on_where_each_test_that_did_not_pass_is_found() {
    let root = temporary("unpassed");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Test};\n\ntest holds =>\n    Test/assert(\"it holds\", true);\n\ntest falls =>\n    Test/assert(\"it falls\", false);\n\ntest falls_too =>\n    Test/assert(\"it falls too\", false);\n";

    let failing = stderr(&fed(&root, &["--progress", "plain", "test", "-"], module));
    let (_, end) = failing
        .split_once("Tested `<stdin>` · tests: 1 passed, 2 failed in ")
        .expect("the subject's tally");
    let end = end.split_once('\n').expect("a line").1;
    assert!(
        end.starts_with("\n<stdin>:6:6\n<stdin>:9:6\n\n1 passed, 2 failed; "),
        "{failing}"
    );

    let passing = stderr(&fed(
        &root,
        &["--progress", "plain", "test", "-"],
        "use /std/{Test};\n\ntest holds =>\n    Test/assert(\"it holds\", true);\n",
    ));
    assert!(
        passing.trim_end().lines().last().is_some_and(
            |last| last.starts_with("Tested `<stdin>` · tests: 1 passed, 0 failed in ")
        ),
        "{passing}"
    );
}

/// `/std/Proc/args` promises opaque byte strings, so an argument that is not UTF-8 reaches the program as its bytes rather than being refused at the command line; the program spells each argument in hex through `Show` on `Bytes`.
#[test]
fn run_forwards_an_argument_that_is_not_utf8_as_its_bytes() {
    let root = temporary("args");
    write(
        &root,
        "args.crs",
        "use /std/{Str, Bytes, List, Show, Io, Proc};\nlet main() -> Io({}) =>\nlet args = Proc/args()!;\nlet rest: List(Bytes) = match args | [] => [] | [first, ..tail] => tail end;\n/std/print(Str/join(\" \", List/map(rest, (a: Bytes) => Show/show(a))));\nmain()\n",
    );
    let output = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(&root)
        .args(["run", "args.crs", "plain"])
        .arg(OsStr::from_bytes(b"\xff"))
        .output()
        .unwrap();

    assert_eq!(
        stdout(&output),
        "706c61696e ff",
        "stderr: {}",
        stderr(&output)
    );
    assert_eq!(output.status.code(), Some(0));
}

/// A program's two output streams reach one pipe in the order it wrote them: standard output is written through its descriptor, one attempt per call, so nothing the program wrote there waits in a buffer while standard error overtakes it. The compiler's own report precedes the program's output on standard error, so what the program wrote is the tail.
#[test]
fn run_delivers_both_streams_in_the_order_the_program_wrote_them() {
    let root = temporary("stream-order");
    fs::create_dir_all(&root).unwrap();
    let (status, output) = merged(
        &root,
        &["run", "-"],
        "let main() -> /std/Io({}) => let _ = /std/print(\"a\")!;\nlet _ = /std/print_err(\"b\")!;\n/std/print(\"c\\n\");\nmain()\n",
    );

    assert!(status.success(), "{output}");
    assert!(output.ends_with("abc\n"), "{output}");
}

/// Live, a run leaves the compile's tree and then what the program wrote: the status is gone before the program's first byte, and none of it is left behind.
#[test]
fn a_live_run_leaves_the_tree_then_what_the_program_wrote() {
    let root = temporary("live-run");
    fs::create_dir_all(&root).unwrap();
    let (status, output) = merged(
        &root,
        &["--progress", "live", "run", "-"],
        "let _ = /std/print(\"a\\n\")!;\n/std/print_err(\"b\\n\")\n",
    );
    assert!(status.success(), "{output}");
    let mut screen = Screen::default();
    screen.write(&output);
    let shown = screen.shown();

    in_order(
        &shown,
        &[
            "<stdin>\n├─ check; ",
            "s\n├─ build; ",
            "s\n└─ emit; ",
            "s\na\nb\n",
        ],
    );
    assert_eq!(shown.lines().count(), 6, "{shown}");
}

/// Live, a test run leaves each subject's tree as the tree drawing writes it, what a test writes quoted beneath its node where the status stood when it wrote, and once the run ends what is done beneath a blank line: no other line of the status is left.
#[test]
fn a_live_test_run_leaves_the_trees_then_the_tally() {
    let root = project("live-outcomes");
    let (status, output) = merged(&root, &["--progress", "live", "test"], "");
    assert_eq!(status.code(), Some(1), "{output}");
    let mut screen = Screen::default();
    screen.write(&output);
    let shown = screen.shown();

    in_order(
        &shown,
        &[
            "/app · tests\n├─ check; ",
            "\n└─ store; ",
            "s\n\n/app · tests\n├─ check; ",
            "\n├─ build; ",
            "\n├─ emit; ",
            "\n├─ /app/doubling_passes; ✓ passed in ",
            "\n├─ /app/addition_passes; ✓ passed in ",
            "\n├─ /app/equality_fails\n│  > comparing\n│  ✗ failed in ",
            "; lib.crs:11:6\n│    not equal\n│      obtained: `4`\n│      expected: `5`\n",
            "├─ /app/overflow_traps; ✗ trapped in ",
            "├─ /app/exits_seven; ✗ exited 7 in ",
            "├─ /app/effect_passes; ✓ passed in ",
            "└─ 3 passed, 1 failed, 1 trapped, 1 exited; ",
            "s\n\n/app\n├─ check; ",
            "\n└─ store; ",
            "s\n\napp · tests\n├─ check; ",
            "\n└─ 0 passed, 0 failed\n\nlib.crs:11:6\nlib.crs:14:6\nlib.crs:17:6\n\n3 passed, 1 failed, 1 trapped, 1 exited; ",
            "s\n\n2 of 2 units; ",
        ],
    );
    // A line the status left would name a node again, or count units a second time.
    assert_eq!(shown.matches("├─ /app/").count(), 6, "{shown}");
    assert_eq!(shown.matches("├─ check; ").count(), 4, "{shown}");
    assert_eq!(shown.matches(" units; ").count(), 1, "{shown}");
    assert!(
        shown
            .lines()
            .last()
            .is_some_and(|last| last.starts_with("2 of 2 units; ")),
        "{shown}"
    );
}

#[test]
fn run_neither_runs_nor_reports_a_test() {
    let root = project("run");
    let output = curios(&root, &["run"]);

    assert_eq!(stdout(&output), "ran\n", "stderr: {}", stderr(&output));
    // The library is checked and filed, then the program checked, built and emitted, its emit the last line before the program's own output: nothing announces the run.
    in_order(
        &stderr(&output),
        &[
            "Checking `/app`\nChecked `/app` in ",
            "Storing `/app`\nStored `/app` in ",
            "Checking `app`\nChecked `app` in ",
            "Building `app`\nBuilt `app` in ",
            "Emitting `app`\nEmitted `app` in ",
        ],
    );
    assert!(
        !stderr(&output).contains("Running"),
        "stderr: {}",
        stderr(&output)
    );
    assert_eq!(output.status.code(), Some(0));
}

#[test]
fn wonder_tests_lists_declared_paths_per_target_form() {
    let root = temporary("wonder-tests");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat, Test};\n\ntest lib_zeta =>\n    Test/assert(\"it holds\", 1 == 1);\n\ntest lib_alpha =>\n    Test/assert(\"it holds\", 2 == 2);\n",
    );
    write(
        &root,
        "app.crs",
        "use /std/{Nat, Str, Io, Test};\n\ntest app_zeta =>\n    Test/assert(\"it holds\", 3 == 3);\n\ntest app_alpha =>\n    Test/assert(\"it holds\", 4 == 4);\n\n/std/print(\"ran\\n\")\n",
    );

    // The package entire: the library's tests, then each executable's, in the order their test programs run them, which is declared rather than sorted — and nothing executes, so the authored entry never prints.
    let whole = curios(&root, &["wonder", "--tests"]);
    assert_eq!(
        stdout(&whole),
        "/app/lib_zeta\n/app/lib_alpha\n/app_zeta\n/app_alpha\n",
        "stderr: {}",
        stderr(&whole)
    );
    assert_eq!(whole.status.code(), Some(0));

    // An executable by name lists its own alone.
    let executable = curios(&root, &["wonder", "--tests", "app"]);
    assert_eq!(stdout(&executable), "/app_zeta\n/app_alpha\n");
    assert_eq!(executable.status.code(), Some(0));

    // A file is placed in the unit that declares it, so the library header answers as the library.
    let file = curios(&root, &["wonder", "--tests", "lib.crs"]);
    assert_eq!(stdout(&file), "/app/lib_zeta\n/app/lib_alpha\n");
    assert_eq!(file.status.code(), Some(0));
}

#[test]
fn wonder_tests_on_a_testless_package_lists_nothing_and_exits_zero() {
    let root = temporary("wonder-none");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(&root, "lib.crs", "pub let zero: /std/Nat = 0;\n");
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    let output = curios(&root, &["wonder", "--tests"]);
    assert_eq!(stdout(&output), "", "stderr: {}", stderr(&output));
    assert_eq!(output.status.code(), Some(0));
}

#[test]
fn every_target_files_its_payload_so_the_second_invocation_reuses_them_all() {
    let root = temporary("payloads");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\nother = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat, Test};\n\ntest lib_holds =>\n    Test/assert(\"it holds\", 1 == 1);\n",
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");
    write(
        &root,
        "other.crs",
        "use /std/{Nat, Test};\n\ntest other_holds =>\n    Test/assert(\"it holds\", 2 == 2);\n\n/std/print(\"ran\\n\")\n",
    );

    let cold = curios(&root, &["test"]);
    assert_eq!(cold.status.code(), Some(0), "stderr: {}", stderr(&cold));

    // Each target is filed against its own chain, so the second invocation compiles nothing: every target's one step is its program's. A cabinet handle shared across targets would carry the library's placement into every later fold, withholding each executable's payload on the invocation that first compiled it.
    let warm = curios(&root, &["test"]);
    in_order(
        &stderr(&warm),
        &[
            "Tested `/app/lib_holds`: passed in ",
            "Tested `/other_holds`: passed in ",
            "\n2 passed, 0 failed; ",
        ],
    );
    // No step is a check, of a unit with its tests or without.
    assert!(
        !stderr(&warm).contains("Checking"),
        "stderr: {}",
        stderr(&warm)
    );
}

/// A unit holding a written goal beside a refused declaration could not be built, and the exit says so: the goal is reported beside the refusal, and the code is the refusal's rather than the goal's.
#[test]
fn a_goal_beside_a_refusal_exits_one_with_both_reported() {
    let root = temporary("mixed");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat};\n\npub let broken : Nat = true;\n\npub let open : Nat = ?;\n",
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    let output = curios(&root, &["test"]);

    assert_eq!(output.status.code(), Some(1));
    let report = stderr(&output);
    assert!(report.contains("pub let broken : Nat = true;"), "{report}");
    assert!(report.contains("└─ [goal]"), "{report}");
}
