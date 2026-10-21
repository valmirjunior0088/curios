//! How a run is spelled, and what a task reads read off it.

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
    let run = Run::cargo(
        Tree::Workspace,
        &["build", "--release"],
        Some(Selection::package("curios")),
        &["--keep-going"],
    );

    assert_eq!(
        run.spelled(),
        ["build", "--release", "--package", "curios", "--keep-going"]
    );
}

#[test]
fn an_npm_script_reads_its_tree_s_install_and_the_install_reads_none() {
    let checkout = Checkout::default();
    let script = Run::npm(Tree::Grammar, &["test"]);
    let install =
        Run::npm(Tree::Grammar, &["clean-install"]).leaving(Product::Installed(Tree::Grammar));

    assert_eq!(
        script.reads(&checkout),
        Ok(vec![Product::Installed(Tree::Grammar)])
    );
    assert_eq!(install.reads(&checkout), Ok(Vec::new()));
}

#[test]
fn a_product_handed_to_a_run_is_read_and_spelled_as_its_path() {
    let run = Run::new(Tool::Node, Tree::Workspace, Vec::new())
        .with("CURIOS", Arg::Product(Product::Compiler));

    assert_eq!(run.reads(&Checkout::default()), Ok(vec![Product::Compiler]));
    assert_eq!(
        run.environment[0].1.spelled(),
        [Product::Compiler.path().to_string_lossy()]
    );
}

/// What a build reads is cargo's graph's to say: the launcher is read by a build of `curios`, which the shipped workspace builds and the native host does not.
#[test]
fn a_build_reads_the_launcher_only_where_it_builds_the_compiler() {
    let checkout = Checkout::default();
    let reads = |selection| {
        Run::cargo(Tree::Workspace, &["clippy"], Some(selection), &[])
            .reads(&checkout)
            .unwrap()
    };

    assert_eq!(reads(Selection::Shipped), [Product::Launcher]);
    assert_eq!(reads(Selection::package("curios-native")), []);
}
