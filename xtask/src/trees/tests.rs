//! Which paths a tree holds, over a listing written here.

use super::*;

const TRACKED: [&str; 9] = [
    "Cargo.toml",
    "biome.json",
    "curios-js/src/harness.js",
    "curios-js/tests/harness.test.mjs",
    "editors/grammar/grammar.js",
    "editors/grammar/src/parser.c",
    "editors/vscode/extension.js",
    "editors/zed/src/lib.rs",
    "programs/chain/chain.rs",
];

#[test]
fn a_tree_inside_the_workspace_is_its_own_directory() {
    assert_eq!(
        Tree::Vscode.holding(TRACKED.into_iter()),
        ["editors/vscode"]
    );
}

#[test]
fn the_workspace_is_every_tracked_path_no_other_tree_stands_under() {
    assert_eq!(
        Tree::Workspace.holding(TRACKED.into_iter()),
        ["Cargo.toml", "biome.json", "curios-js", "programs"]
    );
}

/// A file beside the other trees, under a directory that holds them, is the workspace's by its own path, never by the directory's.
#[test]
fn a_file_beside_another_tree_is_held_by_its_own_path() {
    let tracked = ["editors/README.md", "editors/zed/Cargo.toml"];

    assert_eq!(
        Tree::Workspace.holding(tracked.into_iter()),
        ["editors/README.md"]
    );
}
