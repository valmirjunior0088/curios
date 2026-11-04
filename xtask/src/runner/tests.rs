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
