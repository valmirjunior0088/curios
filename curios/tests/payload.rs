//! What the CLI does across two invocations of one target — the half of payload reuse that is a command-line decision rather than a store mechanism.
//!
//! The store's own behaviour is covered where it lives, in `cache::payload`'s tests: those decide when a slot may be believed. These decide what the *subcommands* do with the answer — that one slot serves `run` and `compile` alike, that a hit reports itself, that a program's output and exit code do not depend on whether it was compiled just now, that a stage query files nothing, and that a bare file has nothing to do with any of it.
//!
//! Not `#[ignore]`d, unlike `bundle`: nothing here execs a produced executable. `curios run` runs its program in-process.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!(
        "curios-cli-payload-{name}-{}-{millis}",
        std::process::id()
    ))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package declaring one executable that prints and then exits with a code of its own.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);

    write(
        &root,
        "curios.toml",
        "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", "pub let word : /std/Str = \"reused\";\n");
    write(
        &root,
        "app.crs",
        "let _ = /std/print(/app/word)!;\n\n/std/proc/exit(7)\n",
    );

    root
}

/// Run the compiler in `root`, with the arguments given.
fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .output()
        .expect("run the compiler")
}

/// Whether an invocation was served from the store, read off the report it wrote — which is the same question, asked the way a person at a terminal asks it.
fn reused(output: &Output) -> bool {
    String::from_utf8_lossy(&output.stderr).contains("; reused")
}

/// The payload slots the project at `root` holds.
fn slots(root: &Path) -> usize {
    fs::read_dir(root.join(".curios").join("payload")).map_or(0, Iterator::count)
}

/// The point of the thing, end to end: what the program does must not depend on whether it was compiled just now.
#[test]
fn running_twice_reuses_and_behaves_identically() {
    let root = project("twice");

    let first = curios(&root, &["run"]);
    let second = curios(&root, &["run"]);

    assert!(
        !reused(&first),
        "nothing is stored for the first invocation"
    );
    assert!(
        reused(&second),
        "and the second says so where a reader sees it"
    );

    assert_eq!(first.stdout, b"reused", "the program ran");
    assert_eq!(second.stdout, first.stdout, "and printed the same thing");
    assert_eq!(
        second.status.code(),
        first.status.code(),
        "and exited the same way — a payload from the store is the payload a compile produced"
    );
    assert_eq!(
        first.status.code(),
        Some(7),
        "which is the program's own code"
    );

    fs::remove_dir_all(root).unwrap();
}

/// One slot serves both subcommands, which is what makes `compile` after `run` a file write.
#[test]
fn one_slot_serves_run_and_compile() {
    let root = project("both");

    assert!(!reused(&curios(&root, &["run"])));
    assert!(
        reused(&curios(&root, &["compile"])),
        "`compile` wraps what `run` filed rather than compiling it again"
    );
    assert!(
        reused(&curios(&root, &["run"])),
        "and `run` afterwards finds the same slot"
    );
    assert_eq!(slots(&root), 1, "one program, one slot, whoever asked");

    assert!(
        root.join(".curios/bin/app/app").exists(),
        "and the bundle was written from it"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A stage is a question, and a question never writes the store: `wonder stage` compiles to answer and files nothing, so the plain invocation after it still compiles — where `--print`, which it replaced, filed what it built.
#[test]
fn asking_for_a_stage_compiles_and_files_nothing() {
    let root = project("asking");

    let shown = curios(&root, &["wonder", "stage", "wasm"]);
    assert!(
        shown.status.success(),
        "{}",
        String::from_utf8_lossy(&shown.stderr)
    );
    assert!(
        String::from_utf8_lossy(&shown.stdout).starts_with("(module"),
        "the answer is the rung, on stdout"
    );
    assert!(shown.stderr.is_empty(), "and nothing was narrated");

    assert!(
        !reused(&curios(&root, &["run"])),
        "and nothing was filed, so the next plain invocation compiles"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A bare file has no project, hence no store and no slot — the declared-versus-bare split, unchanged.
#[test]
fn a_bare_file_files_nothing() {
    let root = temporary("bare");
    write(&root, "scratch.crs", "/std/print(\"standalone\")\n");

    let first = curios(&root, &["run", "scratch.crs"]);
    assert_eq!(first.stdout, b"standalone");
    assert!(!reused(&first));

    let second = curios(&root, &["run", "scratch.crs"]);
    assert!(
        !reused(&second),
        "a file argument brings no project, so there is nothing to reuse"
    );

    assert!(
        !root.join(".curios").exists(),
        "and nothing was written beside it"
    );

    fs::remove_dir_all(root).unwrap();
}
