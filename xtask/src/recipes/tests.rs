//! What a workspace step hands cargo for a selection.

use super::*;

#[test]
fn no_selection_is_the_whole_workspace() {
    assert_eq!(
        testing(None, None),
        [
            "nextest",
            "run",
            "--workspace",
            "--all-targets",
            "--no-fail-fast",
            "--no-tests=warn",
        ]
    );
}

#[test]
fn a_package_takes_the_place_of_the_workspace() {
    let arguments = testing(Some("curios-abi"), None);

    assert_eq!(
        arguments[..4],
        ["nextest", "run", "--package", "curios-abi"]
    );
    assert!(!arguments.contains(&"--workspace"), "{arguments:?}");
}

#[test]
fn a_filter_comes_last() {
    let arguments = testing(Some("curios"), Some("universes"));

    assert_eq!(arguments.last(), Some(&"universes"));
}

/// The workspace is checked as it ships: no step of it turns every feature on, which is `instrumented`'s.
#[test]
fn a_workspace_step_names_no_feature() {
    for arguments in [
        linting(None),
        testing(None, None),
        doctesting(Some("curios-core")),
    ] {
        assert!(
            arguments
                .iter()
                .all(|argument| !argument.contains("feature")),
            "{arguments:?}"
        );
    }
}

#[test]
fn formatting_writes_unless_it_checks() {
    assert_eq!(formatting(false), ["fmt", "--all"]);
    assert_eq!(formatting(true), ["fmt", "--all", "--", "--check"]);
}
