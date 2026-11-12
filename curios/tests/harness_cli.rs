//! What `curios test` does at the command line: the governing package compiled as test programs, one instantiation per test, each test a node of its subject's tree closed by how it ended, what the test wrote of its own joined by what only the runner knows — the failing body, each subject's tally, the exit code — plus the filter, the store round trip, and `run`'s indifference to it all.

use {
    curios_common::test_support::*,
    std::{
        ffi::OsStr,
        fs,
        io::{Read, Write, pipe},
        os::unix::ffi::OsStrExt,
        path::Path,
        process::{Command, Output, Stdio},
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
/// **The trapping row needs a narrowing that still refuses, and a value the folder cannot see.** `Nat` is unbounded at run time, so the only narrowing that refuses is the host wire's: the row hands `2⁷⁰` to `/std/rand/bytes`, whose count crosses as an `i64`, and the refusal fires before the host is called. The operand is tainted by an unset environment variable the way `numeric::test_support::table` taints its rows, since a closed computation never traps (`numeric::unbounded_tests::a_closed_computation_folds_at_the_theory_s_width`); an unset variable rather than stdin because the harness inherits the test runner's stdin, and a read on a terminal would hang.
fn project(name: &str) -> Temporary {
    let root = temporary(name);
    write(
        &root,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Nat, Str, Bool, Bytes, Option, Io, Eq, Test};

pub let double(n: Nat) -> Nat = n * 2;

test doubling_passes =
    Test/equal(double(21), 42);

test addition_passes =
    Test/assert(1 + 1 == 2);

test equality_fails =
    Test/equal(double(2), 5);

test overflow_traps =
    let v = /std/Proc/env("CURIOS_UNSET_OVERFLOW")!; let _ = /std/rand/bytes(Nat/shl(1, 70) + Bytes/len(Option/unwrap_or(v, x[])))!; Test/assert(true);

test exits_seven =
    let _ = /std/Proc/exit(@{}, 7)!; Test/assert(true);

test effect_passes =
    let s = Io/pure("x")!; Test/equal(s, "x");
"#,
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    root
}

fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .output()
        .unwrap()
}

/// [`curios`], with `stdin` on the compiler's standard input.
fn fed(root: &Path, arguments: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
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

    // The library's unit is checked and filed; its test program is checked, built and emitted, and its tests follow on the same tree, each closed by how it ended: a failing test says what went wrong itself, behind the frame its own lines stand behind, so its verdict stands beneath that, and a trap's text is the runner's to add. The executable, whose library is compiled again without its tests, declares none, and closes with its tally at once. The run's tally is the last line.
    in_order(
        &stderr(&output),
        &[
            "/app\n",
            "├─ check; ",
            "╰─ store; ",
            "/app\n",
            "├─ check; ",
            "├─ build; ",
            "├─ emit; ",
            "├─ /app/doubling_passes; passed in ",
            "├─ /app/addition_passes; passed in ",
            "├─ /app/equality_fails\n│  ╎ expected 5 but got 4\n│  ╰─ failed in ",
            "├─ /app/overflow_traps; trapped in ",
            "├─ /app/exits_seven; exited 7 in ",
            "├─ /app/effect_passes; passed in ",
            "╰─ 3 passed, 1 failed, 1 trapped, 1 exited; ",
            "/app\n",
            "├─ check; ",
            "╰─ store; ",
            "app\n",
            "├─ check; ",
            "╰─ 0 passed, 0 failed\n",
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

/// A verdict is what a test program answers, so a test's own exit is never read as one: a test that exits before it reports has not passed at `0`, and at `1` is not the failure a report makes.
#[test]
fn a_test_that_exits_before_it_reports_is_exited_whatever_its_code() {
    let root = temporary("exits");
    write(
        &root,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test, Proc};

test quits_early =
    let _ = Proc/exit(@{}, 0)!; Test/assert(true);

test fails_quietly =
    let _ = Proc/exit(@{}, 1)!; Test/assert(true);
"#,
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    let output = curios(&root, &["test"]);

    in_order(
        &stderr(&output),
        &[
            "├─ /app/quits_early; exited 0 in ",
            "├─ /app/fails_quietly; exited 1 in ",
            "╰─ 0 passed, 0 failed, 2 exited; ",
        ],
    );
    assert_eq!(output.status.code(), Some(1));
}

/// What a test writes is never held: it reaches the stream the test wrote it on as it is written, its first byte ends the line the test's node opened on, and the node closes beneath it — after the end of a line the test left open. The frame is standard error's whichever stream the test wrote on, so the two streams read apart show each its own and read as one pipe show the test's bytes inside its node.
#[test]
fn a_test_that_writes_is_closed_beneath_what_it_wrote() {
    let root = temporary("writes");
    write(
        &root,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test, print, print_err};

test quiet =
    Test/assert(true);

test draws =
    let _ = print("drawn\n")!; Test/assert(true);

test mutters =
    let _ = print_err("half")!; Test/assert(true);
"#,
    );

    let apart = curios(&root, &["test"]);
    assert_eq!(stdout(&apart), "│  ╎ drawn\n", "stderr: {}", stderr(&apart));
    in_order(
        &stderr(&apart),
        &[
            "├─ /app/quiet; passed in ",
            "├─ /app/draws\n│  ╰─ passed in ",
            "├─ /app/mutters\n│  ╎ half\n│  ╰─ passed in ",
            "╰─ 3 passed, 0 failed; ",
        ],
    );

    let (mut merged, writer) = pipe().unwrap();
    // The command holds the parent's copies of the write end until it is dropped, and the read below ends only once every copy is closed.
    let mut child = {
        let mut command = Command::new(env!("CARGO_BIN_EXE_curios"));
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
            "├─ /app/quiet; passed in ",
            "├─ /app/draws\n│  ╎ drawn\n│  ╰─ passed in ",
            "├─ /app/mutters\n│  ╎ half\n│  ╰─ passed in ",
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
            "├─ emit; ",
            "├─ /app/addition_passes; passed in ",
            "╰─ 1 passed, 0 failed, 5 skipped; ",
        ],
    );
    assert_eq!(cold.status.code(), Some(0));

    // Each payload comes back whole, so nothing of either subject's build is narrated: a subject's tree is its tests and its tally.
    let warm = curios(&root, &["test", "--filter", "/app/addition_passes"]);
    in_order(
        &stderr(&warm),
        &[
            "/app\n├─ /app/addition_passes; passed in ",
            "╰─ 1 passed, 0 failed, 5 skipped; ",
            "app\n╰─ 0 passed, 0 failed\n",
            "1 passed, 0 failed, 5 skipped; ",
        ],
    );
    assert!(
        !stderr(&warm).contains("check;"),
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
        "compiler = \"0.0.0\"\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        r#"use /std/{Test};

test first =
    Test/assert(true);

mod Inner
    use /std/{Test};

    test kept =
        Test/assert(true);

    test left =
        Test/assert(true);
end

test second =
    Test/assert(true);

mod Left
    use /std/{Test};

    test out =
        Test/assert(true);
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
            "├─ /app/first; passed in ",
            "├─ /app/second; passed in ",
            "├─ /app/Inner/kept; passed in ",
            "╰─ 3 passed, 0 failed, 2 skipped; ",
            "\n3 passed, 0 failed, 2 skipped; ",
        ],
    );
}

#[test]
fn a_filter_matching_nothing_exits_one_naming_it() {
    let root = project("nomatch");
    let output = curios(&root, &["test", "--filter", "/nope"]);

    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr(&output).contains("no test matches '/nope'"),
        "stderr: {}",
        stderr(&output)
    );

    // A filter names whole segments, so the start of a test's name selects nothing, and the refusal names the paths nearest what was asked for, the test it is the start of first.
    let partial = curios(&root, &["test", "--filter", "/app/addition"]);
    assert_eq!(partial.status.code(), Some(1));
    assert!(
        stderr(&partial)
            .contains("no test matches '/app/addition'; the nearest are '/app/addition_passes'"),
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
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Test};\n\ntest in_library =\n    Test/assert(true);\n",
    );
    write(
        &root,
        "app.crs",
        "use /std/{Test};\n\ntest in_app =\n    Test/assert(true);\n\n/std/print(\"ran\\n\")\n",
    );
    write(
        &root,
        "scratch/loose.crs",
        "use /std/{Test};\n\ntest in_loose =\n    Test/assert(true);\n",
    );

    let executable = curios(&root, &["test", "app"]);
    in_order(
        &stderr(&executable),
        &["app\n", "├─ /in_app; passed in ", "╰─ 1 passed, 0 failed; "],
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
            "scratch/loose.crs\n",
            "├─ /loose/in_loose; passed in ",
            "╰─ 1 passed, 0 failed; ",
        ],
    );
}

/// Text on standard input written as a module runs its own tests as a file written as one does — a unit of its own, mounted at `/stdin` since it has no stem — and is no program for `run` to run.
#[test]
fn a_module_on_standard_input_runs_its_own_tests_and_is_no_program_to_run() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Test};\n\ntest in_piped =\n    Test/assert(true);\n";

    let tested = fed(&root, &["test", "-"], module);
    in_order(
        &stderr(&tested),
        &[
            "<stdin>\n",
            "├─ /stdin/in_piped; passed in ",
            "╰─ 1 passed, 0 failed; ",
        ],
    );
    assert_eq!(tested.status.code(), Some(0));

    let ran = fed(&root, &["run", "-"], module);
    assert_eq!(ran.status.code(), Some(1));
    assert!(
        stderr(&ran).contains(
            "<stdin> is written as a module, with no final term to compile a program from"
        ),
        "stderr: {}",
        stderr(&ran)
    );
}

/// `/std/Proc/args` promises opaque byte strings, so an argument that is not UTF-8 reaches the program as its bytes rather than being refused at the command line; the program spells each argument in hex through `Show` on `Bytes`.
#[test]
fn run_forwards_an_argument_that_is_not_utf8_as_its_bytes() {
    let root = temporary("args");
    write(
        &root,
        "args.crs",
        "use /std/{Str, Bytes, List, Show, Io, Proc};\nlet main() -> Io({}) =\nlet args = Proc/args()!;\nlet rest: List(Bytes) = match args | [] => [] | [first, ..tail] => tail end;\n/std/print(Str/join(\" \", List/map(rest, (a: Bytes) => Show/show(a))));\nmain()\n",
    );
    let output = Command::new(env!("CARGO_BIN_EXE_curios"))
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
    let (mut merged, writer) = pipe().unwrap();
    // The command holds the parent's copies of the write end until it is dropped, and the read below ends only once every copy is closed.
    let mut child = {
        let mut command = Command::new(env!("CARGO_BIN_EXE_curios"));
        command
            .current_dir(&root)
            .args(["run", "-"])
            .stdin(Stdio::piped())
            .stdout(writer.try_clone().unwrap())
            .stderr(writer);
        command.spawn().expect("run the compiler")
    };
    child
        .stdin
        .take()
        .unwrap()
        .write_all(
            b"let main() -> /std/Io({}) = let _ = /std/print(\"a\")!;\nlet _ = /std/print_err(\"b\")!;\n/std/print(\"c\\n\");\nmain()\n",
        )
        .unwrap();

    let mut output = Vec::new();
    merged.read_to_end(&mut output).unwrap();
    assert!(child.wait().unwrap().success());

    let output = String::from_utf8(output).unwrap();
    assert!(output.ends_with("abc\n"), "{output}");
}

#[test]
fn run_neither_runs_nor_reports_a_test() {
    let root = project("run");
    let output = curios(&root, &["run"]);

    assert_eq!(stdout(&output), "ran\n", "stderr: {}", stderr(&output));
    // The library is checked and filed, then the program checked, built and emitted, its `emit` the last node before the program's own output: nothing announces the run.
    in_order(
        &stderr(&output),
        &[
            "/app\n",
            "├─ check; ",
            "╰─ store; ",
            "app\n",
            "├─ check; ",
            "├─ build; ",
            "╰─ emit; ",
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
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat, Test};\n\ntest lib_zeta =\n    Test/assert(1 == 1);\n\ntest lib_alpha =\n    Test/assert(2 == 2);\n",
    );
    write(
        &root,
        "app.crs",
        "use /std/{Nat, Str, Io, Test};\n\ntest app_zeta =\n    Test/assert(3 == 3);\n\ntest app_alpha =\n    Test/assert(4 == 4);\n\n/std/print(\"ran\\n\")\n",
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
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
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
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n\n[[executables]]\nname = \"other\"\n",
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat, Test};\n\ntest lib_holds =\n    Test/assert(1 == 1);\n",
    );
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");
    write(
        &root,
        "other.crs",
        "use /std/{Nat, Test};\n\ntest other_holds =\n    Test/assert(2 == 2);\n\n/std/print(\"ran\\n\")\n",
    );

    let cold = curios(&root, &["test"]);
    assert_eq!(cold.status.code(), Some(0), "stderr: {}", stderr(&cold));

    // Each target is filed against its own chain, so the second invocation compiles nothing: every target's one step is its program's. A store handle shared across targets would carry the library's placement into every later fold, withholding each executable's payload on the invocation that first compiled it.
    let warm = curios(&root, &["test"]);
    in_order(
        &stderr(&warm),
        &[
            "├─ /app/lib_holds; passed in ",
            "├─ /other_holds; passed in ",
            "\n2 passed, 0 failed; ",
        ],
    );
    assert!(
        !stderr(&warm).contains("check;"),
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
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
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
    assert!(report.contains("╰─ [goal]"), "{report}");
}
