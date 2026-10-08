//! What the test recipe hands nextest for a selection, and how it refuses where nextest is missing.

use super::*;

#[test]
fn no_selection_is_the_whole_workspace() {
    assert_eq!(
        test_arguments(None, None),
        [
            "nextest",
            "run",
            "--workspace",
            "--all-targets",
            "--all-features",
            "--no-fail-fast",
            "--no-tests=warn",
        ]
    );
}

#[test]
fn a_package_takes_the_place_of_the_workspace() {
    let arguments = test_arguments(Some("curios-abi"), None);

    assert_eq!(
        arguments[..4],
        ["nextest", "run", "--package", "curios-abi"]
    );
    assert!(!arguments.contains(&"--workspace"), "{arguments:?}");
}

#[test]
fn a_filter_comes_last() {
    let arguments = test_arguments(Some("curios"), Some("universes"));

    assert_eq!(arguments.last(), Some(&"universes"));
}

#[test]
fn nextest_is_listed_by_its_own_name_and_no_other() {
    let listing = "Installed Commands:\n    new                  Create a new cargo package at <path>\n    nextest\n    x                    alias: run --package xtask --\n";

    assert!(lists_nextest(listing));
    assert!(!lists_nextest(&listing.replace("nextest", "nextest-next")));
    assert!(!lists_nextest(
        "    search               Search packages, cargo-nextest among them\n"
    ));
}

#[test]
fn a_refusal_puts_the_fix_on_a_line_of_its_own() {
    assert_eq!(
        nextest_refusal(),
        format!(
            "test needs cargo-nextest, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
        )
    );
}
