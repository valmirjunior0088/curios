//! What a run reports, from task files of the test's own whose commands are `true` and `false`, which every platform this crate builds for has.

use {super::*, crate::*};

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
        "[broken]\ndescription = \"B\"\nrun = [[\"false\"]]\n[behind]\ndescription = \"Behind\"\nrequires = [\"broken\"]\nrun = [[\"true\"]]\n[apart]\ndescription = \"Apart\"\nrun = [[\"true\"]]\n",
    );

    assert_eq!(
        perform(&tree, &group(), &Given::new()),
        Err("failed: a broken; not run: a behind".to_string())
    );
}

#[test]
fn a_step_that_leaves_no_file_it_produces_fails() {
    let tree = tree(
        "[maker]\ndescription = \"M\"\nproduces = { out = \"xtask/.artifacts/never-written\" }\nrun = [[\"true\"]]\n",
    );

    assert_eq!(
        perform(&tree, &group(), &Given::new()),
        Err("failed: a maker".to_string())
    );
}

#[test]
fn a_run_of_steps_that_pass_passes() {
    let tree = tree(
        "[first]\ndescription = \"F\"\nrun = [[\"true\"], [\"true\"]]\n[second]\ndescription = \"S\"\nrequires = [\"first\"]\nrun = [[\"true\"]]\n",
    );

    assert_eq!(perform(&tree, &group(), &Given::new()), Ok(()));
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

    let performed = perform(&tree, &group(), &Given::new());
    let held = std::fs::read_to_string(&written);
    let _ = std::fs::remove_dir_all(written.parent().expect("the file is in a directory"));

    assert_eq!(performed, Ok(()));
    assert_eq!(held.expect("the file is written"), "held");
}
