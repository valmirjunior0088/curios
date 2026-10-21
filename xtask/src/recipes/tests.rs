//! What a step's plan hands its tools for a selection.

use super::*;

#[test]
fn no_selection_is_the_workspace_but_its_tools() {
    assert_eq!(
        testing(None, &Request::whole()).unwrap().spelled(),
        [
            "run",
            "--workspace",
            "--exclude",
            "xtask",
            "--exclude",
            "xbench",
            "--exclude",
            "xboard",
            "--all-targets",
            "--no-fail-fast",
            "--no-tests=warn",
        ]
    );
}

#[test]
fn a_package_takes_the_place_of_the_workspace() {
    let arguments = testing(Some("curios-abi"), &Request::whole())
        .unwrap()
        .spelled();

    assert_eq!(arguments[..3], ["run", "--package", "curios-abi"]);
    assert!(
        !arguments.iter().any(|argument| argument == "--workspace"),
        "{arguments:?}"
    );
}

/// A filter picks tests out of the workspace's one build by their path, and a binary by its name, and neither changes what is built.
#[test]
fn a_name_selects_tests_and_keeps_the_workspace() {
    let request = Request::narrowed(None, Vec::new(), vec!["bundle".to_string()]);
    let arguments = testing(None, &request).unwrap().spelled();

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
        testing(None, &request)
            .unwrap()
            .spelled()
            .last()
            .map(String::as_str),
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
    assert!(linting(None).spelled().contains(&KEEP_GOING.to_string()));
    assert!(
        linting(Some("curios-core"))
            .spelled()
            .contains(&KEEP_GOING.to_string())
    );
}

/// The workspace is checked as it ships: no step of it turns every feature on, which is `instrumented`'s.
#[test]
fn a_workspace_step_names_no_feature() {
    let tested = testing(None, &Request::whole()).unwrap().spelled();
    let linted = linting(None).spelled();

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
    let arguments = recorder_testing().spelled();

    assert_eq!(
        arguments[..5],
        [
            "run",
            "--package",
            "curios-profile",
            "--features",
            "enabled"
        ]
    );
    assert!(!arguments.iter().any(|argument| argument == "--workspace"));
}

#[test]
fn formatting_writes_unless_it_checks() {
    assert_eq!(formatting(false), ["fmt", "--all"]);
    assert_eq!(formatting(true), ["fmt", "--all", "--", "--check"]);
}

/// A lint narrowed to a package plans nothing beside its Rust: the workspace's JavaScript is no package's.
#[test]
fn a_lint_narrowed_to_a_package_plans_no_web_lint() {
    let request = Request::narrowed(Some("curios-native".to_string()), Vec::new(), Vec::new());

    assert_eq!(
        Step::WorkspaceLintWeb.plan(&request, &Checkout::default()),
        Ok(Vec::new())
    );
}
