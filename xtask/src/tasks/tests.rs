//! How a run is spelled: its selections as cargo's flags, its texts as they are.

use super::*;

#[test]
fn the_shipped_workspace_is_every_member_but_the_tools() {
    assert_eq!(
        Arg::Selection(Selection::Shipped).spelled(),
        [
            "--workspace",
            "--exclude",
            "xtask",
            "--exclude",
            "xbench",
            "--exclude",
            "xboard"
        ]
    );
}

#[test]
fn packages_are_each_selected_by_name() {
    assert_eq!(
        Arg::Selection(Selection::tools()).spelled(),
        [
            "--package",
            "xtask",
            "--package",
            "xbench",
            "--package",
            "xboard"
        ]
    );
}

#[test]
fn a_selection_stands_between_a_command_and_its_options() {
    let Task::Run(run) = Task::cargo(
        Tree::Workspace,
        &["build", "--release"],
        Some(Selection::package("curios")),
        &["--keep-going"],
    ) else {
        panic!("cargo is a run");
    };

    assert_eq!(
        run.spelled(),
        ["build", "--release", "--package", "curios", "--keep-going"]
    );
}
