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

/// A step whose prerequisite failed is not run, and neither is one behind it: what each reads is not there.
#[test]
fn a_step_behind_one_that_did_not_pass_is_not_run() {
    assert_eq!(
        blocked(Step::ZedTest, &[Step::GrammarBuild], &[]),
        Some(Step::GrammarBuild)
    );
    assert_eq!(
        blocked(Step::BundleTest, &[], &[Step::BundleBuild]),
        Some(Step::BundleBuild)
    );
    assert_eq!(blocked(Step::ZedTest, &[Step::BundleBuild], &[]), None);
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
        testing(None, &Request::whole()).unwrap(),
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
    let arguments = testing(Some("curios-abi"), &Request::whole()).unwrap();

    assert_eq!(
        arguments[..4],
        ["nextest", "run", "--package", "curios-abi"]
    );
    assert!(
        !arguments.iter().any(|argument| argument == "--workspace"),
        "{arguments:?}"
    );
}

/// A filter picks tests out of the workspace's one build by their path, and a binary by its name, and neither changes what is built.
#[test]
fn a_name_selects_tests_and_keeps_the_workspace() {
    let request = Request::narrowed(None, Vec::new(), vec!["bundle".to_string()]);
    let arguments = testing(None, &request).unwrap();

    assert!(
        arguments.iter().any(|argument| argument == "--workspace"),
        "{arguments:?}"
    );
    assert_eq!(arguments[arguments.len() - 2..], ["-E", "binary(=bundle)"]);
}

#[test]
fn names_are_a_union() {
    let request = Request::narrowed(
        None,
        vec!["cli".to_string(), "selection".to_string()],
        vec!["bundle".to_string()],
    );

    assert_eq!(
        testing(None, &request).unwrap().last().map(String::as_str),
        Some("test(~cli) | test(~selection) | binary(=bundle)")
    );
}

#[test]
fn a_name_holding_the_grammar_s_characters_is_refused() {
    let request = Request::narrowed(None, vec!["a)b".to_string()], Vec::new());

    assert!(testing(None, &request).is_err());
}

#[test]
fn a_lint_goes_on_past_a_crate_that_fails() {
    assert!(linting(None).contains(&KEEP_GOING));
    assert!(linting(Some("curios-core")).contains(&KEEP_GOING));
}

/// The workspace is checked as it ships: no step of it turns every feature on, which is `instrumented`'s.
#[test]
fn a_workspace_step_names_no_feature() {
    let tested = testing(None, &Request::whole()).unwrap();
    let linted = linting(None)
        .into_iter()
        .map(str::to_string)
        .collect::<Vec<_>>();

    for arguments in [linted, tested] {
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
