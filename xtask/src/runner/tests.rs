//! What a run reports, from task files of the test's own whose commands are `true` and `false`, which every platform this crate builds for has.

use {super::*, crate::*, curios_common::fill};

fn tree(steps: &str) -> Tree {
    Tree::parse(&[
        ("xtask/tasks.toml", "description = \"The top\"\n"),
        (
            "xtask/tasks/a.toml",
            &format!("description = \"A\"\n{steps}"),
        ),
    ])
    .unwrap()
}

fn group() -> Name {
    vec!["a".to_string()]
}

/// A step behind one that did not pass is not run, since what it reads is not there, and a step that reads nothing of the failure goes on.
#[test]
fn a_step_behind_one_that_failed_is_not_run_and_the_rest_go_on() {
    let tree = tree(
        "[broken]\ndescription = \"B\"\nrun = [[\"false\"]]\n[behind]\ndescription = \"Behind\"\nrequires = [\".a.broken\"]\nrun = [[\"true\"]]\n[apart]\ndescription = \"Apart\"\nrun = [[\"true\"]]\n",
    );

    assert_eq!(
        perform(&tree, &group(), &GivenValues::new()),
        Err("failed: a broken; not run: a behind".to_string())
    );
}

#[test]
fn a_step_that_leaves_no_file_it_produces_fails() {
    let tree = tree(
        "[maker]\ndescription = \"M\"\nproduces = { out = \"xtask/.artifacts/never-written\" }\nrun = [[\"true\"]]\n",
    );

    assert_eq!(
        perform(&tree, &group(), &GivenValues::new()),
        Err("failed: a maker".to_string())
    );
}

#[test]
fn a_run_of_steps_that_pass_passes() {
    let tree = tree(
        "[first]\ndescription = \"F\"\nrun = [[\"true\"], [\"true\"]]\n[second]\ndescription = \"S\"\nrequires = [\".a.first\"]\nrun = [[\"true\"]]\n",
    );

    assert_eq!(perform(&tree, &group(), &GivenValues::new()), Ok(()));
}

#[test]
fn a_write_leaves_its_file_holding_its_contents() {
    let written = std::env::temp_dir()
        .join(format!("curios-xtask-write-{}", std::process::id()))
        .join("written");
    let tree = tree(&format!(
        "[writer]\ndescription = \"W\"\nrun = [{{ write = \"{}\", contents = \"held\" }}]\n",
        written.display()
    ));

    let performed = perform(&tree, &group(), &GivenValues::new());
    let held = std::fs::read_to_string(&written);
    let _ = std::fs::remove_dir_all(written.parent().expect("the file is in a directory"));

    assert_eq!(performed, Ok(()));
    assert_eq!(held.expect("the file is written"), "held");
}

/// A step runs with the directory of each tool it names that is kept as a directory first on its `PATH`, so a launch inside the tool finds what ships beside it: here a shell that finds the first directory on its `PATH` to be the one holding the tool's executable.
#[test]
fn a_step_runs_with_its_tools_directories_first_on_its_path() {
    let name = format!("shelf-{}", std::process::id());
    let kept = artifacts().join(format!("tools/{name}-1.0"));
    fs::create_dir_all(kept.join("bin")).unwrap();
    let digest = "0".repeat(64);
    let tree = Tree::parse(&[
        (
            "xtask/tasks.toml",
            &fill!(
                r#"description = "The top"
[tools.${name}]
version = "1.0"
url.${HOST_TRIPLE} = "https://example.org/${name}.tar.gz"
sha256.${HOST_TRIPLE} = "${digest}"
path = "bin/${name}"
"#,
                name,
                HOST_TRIPLE,
                digest
            ),
        ),
        (
            "xtask/tasks/a.toml",
            &fill!(
                r#"description = "A"
[s]
description = "S"
run = [["sh", "-c", 'test "$(echo "$PATH" | cut -d: -f1)" = "$(dirname "$0")"', "{.tools.${name}}"]]
"#,
                name
            ),
        ),
    ])
    .unwrap();

    let outcome = perform(&tree, &group(), &GivenValues::new());

    fs::remove_dir_all(&kept).unwrap();
    assert_eq!(outcome, Ok(()));
}
