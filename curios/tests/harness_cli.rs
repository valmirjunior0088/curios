//! What `curios test` does at the command line: the governing package compiled as test programs, one instantiation per test, the guest's outcome lines joined by what only the runner knows — the failing body, the count line, the exit code — plus the filter, the store round trip, and `run`'s indifference to it all.

use std::{
    env,
    ffi::OsStr,
    fs,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    process::{self, Command, Output},
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    env::temp_dir().join(format!("curios-cli-test-{name}-{}-{millis}", process::id()))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// The six-outcome package: every rung the report can print, declared in the library, beside an executable with no tests of its own.
///
/// **The trapping row needs a value the folder cannot see, and that is why it reads an unset environment variable.** It was `Test/assert(Nat/shl(1, 40) == 0)` while the erased carriers were `u32`: the fold refused past the width, so the shift survived to the backend and the i31 envelope trapped on it. Unbounded, the whole expression folds to `false` and the row reports *failed* — a passing suite that no longer exercises `trapped` at all. A closed computation never traps now (`numeric::envelope_tests::a_closed_computation_folds_at_the_theory_s_width`), so the row taints its operand the way `numeric::test_support::table` does, and keeps the shift closed with the taint added after it so the trap is the envelope's rather than a declined fold's. An unset variable rather than stdin because the harness inherits the test runner's stdin, and a read on a terminal would hang.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);
    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
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
    Test/perform(() => let v = /std/proc/env("CURIOS_UNSET_OVERFLOW")!; Io/pure(Test/assert(Nat/shl(1, 40) + Bytes/len(Option/unwrap_or(v, x[])) == 0)));

test exits_seven =
    Test/perform(() => let _ = /std/proc/exit(@{}, 7)!; Io/pure(Test/assert(true)));

test effect_passes =
    Test/perform(() => let s = Io/pure("x")!; Io/pure(Test/equal(s, "x")));
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

    in_order(
        &stdout(&output),
        &[
            "/app/doubling_passes: passed\n",
            "/app/addition_passes: passed\n",
            "/app/equality_fails: failed\n  expected 5 but got 4\n    Test/equal(double(2), 5)\n",
            "/app/overflow_traps: trapped\n",
            "    Test/perform(() => let v = /std/proc/env(\"CURIOS_UNSET_OVERFLOW\")!; Io/pure(Test/assert(Nat/shl(1, 40) + Bytes/len(Option/unwrap_or(v, x[])) == 0)))\n",
            "/app/exits_seven: exited 7\n    Test/perform(() => let _ = /std/proc/exit(@{}, 7)!; Io/pure(Test/assert(true)))\n",
            "/app/effect_passes: passed\n",
            "3 passed, 1 failed, 1 trapped, 1 exited\n",
        ],
    );
    // The library is taken on, compiled, tested and tallied; the executable, which declares no tests, is taken on and compiled and nothing more.
    in_order(
        &stderr(&output),
        &[
            "Processing     /app\n",
            "↳ Compiling    /app; ",
            "↳ Testing      /app\n",
            "↳ Tested       /app; 3 passed, 1 failed, 1 trapped, 1 exited\n",
            "Processing     app\n",
            "↳ Compiling    /app; ",
        ],
    );
    assert!(
        !stderr(&output).contains("Testing      app"),
        "a unit with nothing to run reports no testing step, stderr: {}",
        stderr(&output)
    );
    assert_eq!(output.status.code(), Some(1));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn a_filter_selects_by_path_prefix_and_a_second_run_reuses_the_payload() {
    let root = project("filter");

    let cold = curios(&root, &["test", "/app/addition"]);
    assert_eq!(
        stdout(&cold),
        "/app/addition_passes: passed\n1 passed, 0 failed\n",
        "stderr: {}",
        stderr(&cold)
    );
    assert_eq!(cold.status.code(), Some(0));

    let warm = curios(&root, &["test", "/app/addition"]);
    assert_eq!(stdout(&warm), stdout(&cold));
    // Each payload comes back whole, so each target's one step names the target rather than a unit.
    in_order(
        &stderr(&warm),
        &[
            "Processing     /app\n",
            "↳ Compiling    /app; reused\n",
            "↳ Testing      /app\n",
            "↳ Tested       /app; 1 passed, 0 failed\n",
            "Processing     app\n",
            "↳ Compiling    app; reused\n",
        ],
    );

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn a_filter_matching_nothing_exits_one_naming_it() {
    let root = project("nomatch");
    let output = curios(&root, &["test", "/nope"]);

    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr(&output).contains("no test matches '/nope'"),
        "stderr: {}",
        stderr(&output)
    );

    fs::remove_dir_all(root).unwrap();
}

/// `/std/proc/args` promises opaque byte strings, so an argument that is not UTF-8 reaches the program as its bytes rather than being refused at the command line; the program spells each argument in hex through `Show` on `Bytes`.
#[test]
fn run_forwards_an_argument_that_is_not_utf8_as_its_bytes() {
    let root = temporary("args");
    write(
        &root,
        "args.crs",
        "use /std/{Str, Bytes, List, Show, Io, proc};\nlet args = proc/args!;\nlet rest: List(Bytes) = match args | [] => [] | [first, ..tail] => tail end;\n/std/print(Str/join(\" \", List/map(rest, (a: Bytes) => Show/show(a))))\n",
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

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn run_neither_runs_nor_reports_a_test() {
    let root = project("run");
    let output = curios(&root, &["run"]);

    assert_eq!(stdout(&output), "ran\n", "stderr: {}", stderr(&output));
    // The target is taken on, its library compiled as a step, and the handover is the last step before the program's own output.
    in_order(
        &stderr(&output),
        &[
            "Processing     app\n",
            "↳ Compiling    /app; ",
            "↳ Running      app\n",
        ],
    );
    assert_eq!(output.status.code(), Some(0));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn wonder_tests_lists_declared_paths_per_target_form() {
    let root = temporary("wonder-tests");
    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(
        &root,
        "lib.crs",
        "use /std/{Nat, Test};\n\ntest lib_first =\n    Test/assert(1 == 1);\n\ntest lib_second =\n    Test/assert(2 == 2);\n",
    );
    write(
        &root,
        "app.crs",
        "use /std/{Nat, Str, Io, Test};\n\ntest app_holds =\n    Test/assert(3 == 3);\n\n/std/print(\"ran\\n\")\n",
    );

    // The package entire: the library's tests, then each executable's, in declaration order — and nothing executes, so the authored entry never prints.
    let whole = curios(&root, &["wonder", "tests"]);
    assert_eq!(
        stdout(&whole),
        "/app/lib_first\n/app/lib_second\n/app_holds\n",
        "stderr: {}",
        stderr(&whole)
    );
    assert_eq!(whole.status.code(), Some(0));

    // An executable by name lists its own alone.
    let executable = curios(&root, &["wonder", "tests", "app"]);
    assert_eq!(stdout(&executable), "/app_holds\n");
    assert_eq!(executable.status.code(), Some(0));

    // A file is placed in the unit that declares it, so the library header answers as the library.
    let file = curios(&root, &["wonder", "tests", "lib.crs"]);
    assert_eq!(stdout(&file), "/app/lib_first\n/app/lib_second\n");
    assert_eq!(file.status.code(), Some(0));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn wonder_tests_on_a_testless_package_lists_nothing_and_exits_zero() {
    let root = temporary("wonder-none");
    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", "pub let zero: /std/Nat = 0;\n");
    write(&root, "app.crs", "/std/print(\"ran\\n\")\n");

    let output = curios(&root, &["wonder", "tests"]);
    assert_eq!(stdout(&output), "", "stderr: {}", stderr(&output));
    assert_eq!(output.status.code(), Some(0));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn every_target_files_its_payload_so_the_second_invocation_reuses_them_all() {
    let root = temporary("payloads");
    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n\n[[executables]]\nname = \"other\"\n",
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

    // Each target is filed against its own chain, so the second invocation compiles nothing: every target's one step names the target itself. A store handle shared across targets used to carry the library's placement into every later fold, withholding each executable's payload on the invocation that first compiled it.
    let warm = curios(&root, &["test"]);
    assert_eq!(stdout(&warm), stdout(&cold));
    in_order(
        &stderr(&warm),
        &[
            "Processing     /app\n",
            "↳ Compiling    /app; reused\n",
            "Processing     app\n",
            "↳ Compiling    app; reused\n",
            "Processing     other\n",
            "↳ Compiling    other; reused\n",
        ],
    );

    fs::remove_dir_all(root).unwrap();
}
