//! What `curios check` does at the command line: the report on stdout, the exit code it turns on, and the one lint decided over a package rather than a unit.

use {
    curios::CABINET,
    curios_common::test_support::*,
    curios_package::Cabinet,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Output, Stdio},
    },
};

/// A directory of its own, shared with no other test and gone with it.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-check", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
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

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// A file a package holds and no `mod` declares is checked on its own, with nothing said of that: a clean one exits 0 and reports nothing.
#[test]
fn a_clean_file_no_mod_declares_lints_clean_on_its_own() {
    let root = temporary("unreached");
    write(
        &root,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n"
        ),
    );
    write(&root, "lib.crs", "");
    write(&root, "stray.crs", "pub let word : /std/Str = \"clean\";\n");

    let checked = curios(&root, &["check", "stray.crs"], "");
    let text = stdout(&checked);
    assert_eq!(checked.status.code(), Some(0), "{text}");
    assert!(text.is_empty(), "{text}");
}

/// A lint is reported on stdout as `run` reports an error, and turns the exit into 1; a clean program exits 0 with nothing on stdout. Standard error narrates the check either way, and ends on the count.
#[test]
fn a_lint_is_reported_on_stdout_with_exit_one_and_a_clean_program_exits_zero() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let checked = curios(
        &root,
        &["check", "-"],
        "use /std/{Bool};\n/std/print(\"\")\n",
    );
    assert_eq!(checked.status.code(), Some(1));
    let text = stdout(&checked);
    assert!(
        text.starts_with("unused import\n│  `Bool` is never used\n│  delete it"),
        "{text}"
    );
    assert!(text.contains("└─ [lint] <stdin>:1:11"), "{text}");
    // The program is checked under its own name, a blank line parts the narration from the report, and the count closes.
    let narrated = String::from_utf8_lossy(&checked.stderr);
    assert!(
        narrated.starts_with("Checking `<stdin>`\nChecked `<stdin>` in "),
        "{narrated}"
    );
    assert!(narrated.ends_with("s\n\n1 lint\n"), "{narrated}");

    let clean = curios(&root, &["check", "-"], "/std/print(\"\")\n");
    assert!(clean.status.success());
    assert!(clean.stdout.is_empty());
    let narrated = String::from_utf8_lossy(&clean.stderr);
    assert!(
        narrated.starts_with("Checking `<stdin>`\nChecked `<stdin>` in "),
        "{narrated}"
    );
    assert!(narrated.ends_with("s\n0 lints\n"), "{narrated}");
}

/// Several subjects are one report: each file checked under its own name in the order named, their lints together on stdout, one count over all of them closing the narration, and the exit any one of them turns. One fact reached by several subjects is one finding.
#[test]
fn several_subjects_are_checked_into_one_report() {
    let root = temporary("several");
    write(&root, "clean.crs", "/std/print(\"\")\n");
    write(&root, "first.crs", "use /std/{Bool};\n/std/print(\"\")\n");
    write(&root, "second.crs", "use /std/{Nat};\n/std/print(\"\")\n");

    let checked = curios(
        &root,
        &["check", "first.crs", "clean.crs", "second.crs"],
        "",
    );
    assert_eq!(checked.status.code(), Some(1));
    let text = stdout(&checked);
    let first = text.find("└─ [lint] first.crs:1:11").expect(&text);
    let second = text.find("└─ [lint] second.crs:1:11").expect(&text);
    assert!(first < second, "{text}");
    let narrated = String::from_utf8_lossy(&checked.stderr);
    let checked = ["first.crs", "clean.crs", "second.crs"].map(|file| {
        narrated
            .find(&format!("Checking `{file}`"))
            .unwrap_or_else(|| panic!("{narrated}"))
    });
    assert!(checked.is_sorted(), "{narrated}");
    assert!(narrated.ends_with("s\n\n2 lints\n"), "{narrated}");

    let clean = curios(&root, &["check", "clean.crs", "clean.crs"], "");
    assert!(clean.status.success(), "{}", stdout(&clean));
    assert!(clean.stdout.is_empty());

    // One fact is one finding however many subjects reach it, as a library's is reached by every executable compiled against it.
    let twice = curios(&root, &["check", "first.crs", "first.crs"], "");
    assert_eq!(
        stdout(&twice).matches("└─ [lint] first.crs:1:11").count(),
        1,
        "{}",
        stdout(&twice)
    );
    assert!(String::from_utf8_lossy(&twice.stderr).ends_with("\n1 lint\n"));
}

/// Goals alone are the incomplete state every subcommand exits 2 on; a goal beside a lint is 1, since the lint is what there is to act on.
#[test]
fn goals_alone_exit_two_and_a_lint_beside_one_exits_one() {
    let root = temporary("goals");
    fs::create_dir_all(&root).unwrap();

    let goals = curios(
        &root,
        &["check", "-"],
        "pub let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );
    assert_eq!(goals.status.code(), Some(2), "{}", stdout(&goals));

    let both = curios(
        &root,
        &["check", "-"],
        "let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );
    assert_eq!(both.status.code(), Some(1));
    let text = stdout(&both);
    assert!(text.starts_with("`/std/Nat` expected"), "{text}");
    assert!(
        text.contains("unused declaration\n│  `m` is never used"),
        "{text}"
    );
}

/// A dependency nothing in the package reached is reported by the package-entire form alone, against the manifest, and one the library reaches is not.
#[test]
fn an_unused_dependency_is_reported_for_the_package_entire() {
    let root = temporary("dependency");
    write(
        &root,
        "shape/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"shape\"\n"
        ),
    );
    write(&root, "shape/lib.crs", "pub let sides : /std/Nat = 4;\n");
    write(
        &root,
        "json/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"json\"\n"
        ),
    );
    write(&root, "json/lib.crs", "pub let depth : /std/Nat = 1;\n");
    write(
        &root,
        "app/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[dependencies]\nshape = { path = \"../shape\" }\njson = { path = \"../json\" }\n"
        ),
    );
    write(
        &root,
        "app/lib.crs",
        "pub let count : /std/Nat = /shape/sides;\n",
    );
    write(&root, "app/exe.crs", "/std/print(\"\")\n");
    let app = root.join("app");

    let entire = curios(&app, &["check"], "");
    assert_eq!(entire.status.code(), Some(1), "{}", stdout(&entire));
    let text = stdout(&entire);
    assert!(
        text.contains("unused dependency\n│  `json` is never used\n│  delete its row"),
        "{text}"
    );
    assert!(!text.contains("`shape`"), "{text}");

    // A file is checked as its unit and says nothing about the package's rows.
    let one = curios(&app, &["check", "lib.crs"], "");
    assert!(one.status.success(), "{}", stdout(&one));
    assert!(one.stdout.is_empty());
}

/// A dependency is reached by the path a reference writes, never by the package it resolves into: `use /shape/m/{x}`, where `shape` re-exports `json`'s module, reaches `shape`, whose row the package needs, and leaves `json` unused, whose row it does not, `json` staying mounted through `shape`.
#[test]
fn a_dependency_is_reached_by_the_path_written_and_not_by_what_it_resolves_into() {
    let root = temporary("re-exported");
    write(
        &root,
        "json/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"json\"\n"
        ),
    );
    write(
        &root,
        "json/lib.crs",
        "pub mod m\n    pub let x : /std/Nat = 1;\nend\n",
    );
    write(
        &root,
        "shape/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"shape\"\n\n[dependencies]\njson = { path = \"../json\" }\n"
        ),
    );
    write(&root, "shape/lib.crs", "pub use /json/{mod m};\n");
    write(
        &root,
        "app/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[dependencies]\nshape = { path = \"../shape\" }\njson = { path = \"../json\" }\n"
        ),
    );
    write(
        &root,
        "app/lib.crs",
        "use /shape/m/{x};\n\npub let count : /std/Nat = x;\n",
    );
    write(&root, "app/exe.crs", "/std/print(\"\")\n");
    let app = root.join("app");

    let entire = curios(&app, &["check"], "");
    let text = stdout(&entire);
    assert!(
        text.contains("unused dependency\n│  `json` is never used\n│  delete its row"),
        "{text}"
    );
    assert!(!text.contains("`shape`"), "{text}");
}

/// A cabinet that cannot be written costs the reuse and never the answer: `check` reports and exits as it would have, each unit's store says it was refused, and why is said once, beneath the first of them, however many subjects the package has.
#[test]
fn a_cabinet_that_cannot_be_written_is_said_once_and_changes_nothing() {
    let package = |name: &str| {
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
        write(&root, "lib.crs", "pub let word : /std/Str = \"clean\";\n");
        write(&root, "app.crs", "/std/print(/app/word)\n");
        root
    };

    let taken = package("stored");
    let stored = curios(&taken, &["check"], "");
    assert_eq!(stored.status.code(), Some(0), "{}", stdout(&stored));
    let filed = String::from_utf8_lossy(&stored.stderr);
    assert!(
        filed.contains("Stored `/app` in ") && !filed.contains("refused"),
        "a cabinet that took the units says nothing of itself: {filed}"
    );

    let refused = package("unstored");
    // A file where the objects would go.
    let objects = Cabinet::of(&refused, None).objects();
    fs::create_dir_all(objects.parent().unwrap()).unwrap();
    fs::write(&objects, "").unwrap();
    let unstored = curios(&refused, &["check"], "");
    let narrated = String::from_utf8_lossy(&unstored.stderr);

    assert_eq!(unstored.status.code(), stored.status.code(), "{narrated}");
    assert_eq!(stdout(&unstored), stdout(&stored), "the report is the same");
    // Each unit's own store says the cabinet took nothing, and the reason stands beneath the first alone.
    let lines = narrated.lines().collect::<Vec<_>>();
    let refused = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| line.starts_with("Stored ") && line.contains(": refused in "))
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    assert!(refused.len() > 1, "a library and a program: {narrated}");
    let beneath = lines[refused[0] + 1];
    assert!(
        beneath.starts_with("  "),
        "a line beneath the store's, no step of its own: {narrated}"
    );
    let reason = beneath.trim_start();
    assert_eq!(
        narrated.matches(reason).count(),
        1,
        "once, for a library and a program asked of one cabinet: {narrated}"
    );
}
