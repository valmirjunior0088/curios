//! What a workspace step hands cargo for a selection, and what a run takes on before it starts anything.

use {super::*, crate::Subject};

#[test]
fn a_step_brings_what_it_is_run_with_first() {
    assert_eq!(
        closed(&[Step::WorkspaceTest]),
        [Step::LauncherBuild, Step::WorkspaceTest]
    );
    assert_eq!(
        closed(&[Step::BundleTest]),
        [Step::BundleBuild, Step::BundleTest]
    );
}

/// A bare subject is its steps in the table's order, behind what they are run with first and nothing else.
#[test]
fn a_bare_subject_brings_no_step_of_another_but_its_prerequisites() {
    assert_eq!(
        closed(&Step::about(Subject::Compiler)),
        [Step::LauncherBuild, Step::CompilerBuild]
    );
}

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

#[test]
fn a_lint_goes_on_past_a_crate_that_fails() {
    assert!(linting(None).contains(&KEEP_GOING));
    assert!(linting(Some("curios-core")).contains(&KEEP_GOING));
}

/// The workspace is checked as it ships: no step of it turns every feature on, which is `instrumented`'s.
#[test]
fn a_workspace_step_names_no_feature() {
    for arguments in [linting(None), testing(None, None)] {
        assert!(
            arguments
                .iter()
                .all(|argument| !argument.contains("feature")),
            "{arguments:?}"
        );
    }
}

/// The recorder's tests are one package's under one feature, so their run compiles nothing of the compiler's suite.
#[test]
fn the_recorder_s_tests_build_one_package() {
    assert_eq!(
        RECORDER_TESTING[..6],
        [
            "nextest",
            "run",
            "--package",
            "curios-profile",
            "--features",
            "enabled"
        ]
    );
    assert!(!RECORDER_TESTING.contains(&"--workspace"));
}

#[test]
fn formatting_writes_unless_it_checks() {
    assert_eq!(formatting(false), ["fmt", "--all"]);
    assert_eq!(formatting(true), ["fmt", "--all", "--", "--check"]);
}
