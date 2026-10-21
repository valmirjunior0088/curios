//! Which packages a selection builds, over a graph written here as `cargo metadata` writes one.

use super::*;

/// `alpha` depends on `beta`, which needs `curios` only for its tests; `xtask` depends on `curios` outright.
const METADATA: &str = r#"{
    "packages": [
        {"id": "a", "name": "alpha"},
        {"id": "b", "name": "beta"},
        {"id": "c", "name": "curios"},
        {"id": "x", "name": "xtask"}
    ],
    "workspace_members": ["a", "b", "c", "x"],
    "resolve": {"nodes": [
        {"id": "a", "deps": [{"pkg": "b", "dep_kinds": [{"kind": null, "target": null}]}]},
        {"id": "b", "deps": [{"pkg": "c", "dep_kinds": [{"kind": "dev", "target": null}]}]},
        {"id": "c", "deps": []},
        {"id": "x", "deps": [{"pkg": "c", "dep_kinds": [{"kind": null, "target": null}]}]}
    ]}
}"#;

fn graph() -> Graph {
    Graph::read(METADATA).unwrap()
}

#[test]
fn a_selected_package_builds_what_its_tests_depend_on() {
    assert!(graph().builds(&Selection::package("beta"), "curios"));
}

#[test]
fn a_dependency_s_tests_are_not_built_with_it() {
    assert!(!graph().builds(&Selection::package("alpha"), "curios"));
    assert!(graph().builds(&Selection::package("alpha"), "beta"));
}

#[test]
fn a_workspace_selection_builds_its_members() {
    assert!(graph().builds(&Selection::Shipped, "curios"));
    assert!(graph().builds(&Selection::Every, "xtask"));
    assert!(!graph().builds(&Selection::Shipped, "xtask"));
}

#[test]
fn metadata_without_a_resolved_graph_is_refused() {
    assert!(Graph::read(r#"{"packages": [], "workspace_members": []}"#).is_err());
}
