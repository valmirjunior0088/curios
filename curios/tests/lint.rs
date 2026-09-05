//! What `curios lint` does at the command line: the report on stdout, the exit code it turns on, and the one lint decided over a package rather than a unit.

use std::{
    env, fs,
    io::Write,
    path::{Path, PathBuf},
    process::{self, Command, Output, Stdio},
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    env::temp_dir()
        .canonicalize()
        .unwrap()
        .join(format!("curios-cli-lint-{name}-{}-{millis}", process::id()))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
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
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// A lint is reported on stdout as `wonder diagnostics` reports it, and turns the exit into 1; a clean program exits 0 with nothing said.
#[test]
fn a_lint_is_reported_on_stdout_with_exit_one_and_a_clean_program_exits_zero() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let linted = curios(
        &root,
        &["lint", "-"],
        "use /std/{Bool};\n/std/print(\"\")\n",
    );
    assert_eq!(linted.status.code(), Some(1));
    let text = stdout(&linted);
    assert!(
        text.starts_with("unused import `Bool`; delete it"),
        "{text}"
    );
    assert!(text.contains("--> <stdin>:1:11"), "{text}");
    assert!(linted.stderr.is_empty(), "nothing is narrated");

    let clean = curios(&root, &["lint", "-"], "/std/print(\"\")\n");
    assert!(clean.status.success());
    assert!(clean.stdout.is_empty());

    fs::remove_dir_all(root).unwrap();
}

/// Goals alone are the incomplete state every subcommand exits 2 on; a goal beside a lint is 1, since the lint is what there is to act on.
#[test]
fn goals_alone_exit_two_and_a_lint_beside_one_exits_one() {
    let root = temporary("goals");
    fs::create_dir_all(&root).unwrap();

    let goals = curios(
        &root,
        &["lint", "-"],
        "pub let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );
    assert_eq!(goals.status.code(), Some(2), "{}", stdout(&goals));

    let both = curios(
        &root,
        &["lint", "-"],
        "let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );
    assert_eq!(both.status.code(), Some(1));
    let text = stdout(&both);
    assert!(text.starts_with("goal `?`"), "{text}");
    assert!(text.contains("unused declaration `m`"), "{text}");

    fs::remove_dir_all(root).unwrap();
}

/// A dependency nothing in the package reached is reported by the package-entire form alone, against the manifest, and one the library reaches is not.
#[test]
fn an_unused_dependency_is_reported_for_the_package_entire() {
    let root = temporary("dependency");
    write(&root, "shape/curios.toml", "name = \"shape\"\n");
    write(&root, "shape/lib.crs", "pub let sides : /std/Nat = 4;\n");
    write(&root, "json/curios.toml", "name = \"json\"\n");
    write(&root, "json/lib.crs", "pub let depth : /std/Nat = 1;\n");
    write(
        &root,
        "app/curios.toml",
        "name = \"app\"\n\n[dependencies]\nshape = { source = \"path\", path = \"../shape\" }\njson = { source = \"path\", path = \"../json\" }\n",
    );
    write(
        &root,
        "app/lib.crs",
        "pub let count : /std/Nat = /shape/sides;\n",
    );
    write(&root, "app/exe.crs", "/std/print(\"\")\n");
    let app = root.join("app");

    let entire = curios(&app, &["lint"], "");
    assert_eq!(entire.status.code(), Some(1), "{}", stdout(&entire));
    let text = stdout(&entire);
    assert!(
        text.contains("unused dependency `json`; delete its row"),
        "{text}"
    );
    assert!(!text.contains("`shape`"), "{text}");

    // A file is linted as its unit and says nothing about the package's rows.
    let one = curios(&app, &["lint", "lib.crs"], "");
    assert!(one.status.success(), "{}", stdout(&one));
    assert!(one.stdout.is_empty());

    fs::remove_dir_all(root).unwrap();
}
