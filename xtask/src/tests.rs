//! What holds this workspace of its own to the compiler's beside it: the lints it is held to are the workspace's, and the bindings generator it calls is the version of the bindings `curios-js` builds against.

use {
    crate::root,
    std::{collections::BTreeMap, fs},
    toml_edit::{DocumentMut, Item},
};

/// A manifest of the repository, read and parsed, by its path from the root.
fn manifest(path: &str) -> DocumentMut {
    let at = root().join(path);

    fs::read_to_string(&at)
        .unwrap_or_else(|error| panic!("{}: {error}", at.display()))
        .parse()
        .unwrap_or_else(|error| panic!("{}: {error}", at.display()))
}

/// Every lint a lint table states, by tool, with its level as written.
fn lints(table: Option<&Item>) -> BTreeMap<String, BTreeMap<String, String>> {
    table
        .and_then(Item::as_table_like)
        .into_iter()
        .flat_map(|tools| tools.iter())
        .map(|(tool, levels)| {
            let levels = levels
                .as_table_like()
                .into_iter()
                .flat_map(|levels| levels.iter())
                .map(|(lint, level)| (lint.to_string(), level.to_string().trim().to_string()))
                .collect();

            (tool.to_string(), levels)
        })
        .collect()
}

/// The version a manifest's dependency row states.
fn version(manifest: &DocumentMut, dependency: &str) -> Option<String> {
    manifest
        .get("dependencies")?
        .get(dependency)?
        .get("version")?
        .as_str()
        .map(str::to_string)
}

/// These recipes are a workspace of their own, so they cannot inherit the compiler's lint table and state a copy. This holds the copy to its original: a lint the workspace gains, drops or moves fails here until the recipes follow.
#[test]
fn the_lint_table_is_the_workspace_s() {
    let workspace = manifest("Cargo.toml");
    let held = lints(
        workspace
            .get("workspace")
            .and_then(|table| table.get("lints")),
    );

    assert!(!held.is_empty(), "the workspace states a lint table");
    assert_eq!(lints(manifest("xtask/Cargo.toml").get("lints")), held);
}

/// `wasm-bindgen-cli-support` generates the glue for the `wasm-bindgen` that `curios-js` builds against, and the two refuse each other across versions. They sit in two workspaces, so no one row can state them: this holds the two pins equal.
#[test]
fn the_bindings_generator_is_the_bindings_version() {
    let generator = version(&manifest("xtask/Cargo.toml"), "wasm-bindgen-cli-support");

    assert!(generator.is_some(), "xtask pins the bindings generator");
    assert_eq!(
        generator,
        version(&manifest("curios-js/Cargo.toml"), "wasm-bindgen")
    );
}
