//! What the test recipe hands nextest for a selection, which shards it admits, and which nextest it refuses and how.

use super::*;

/// What nextest answers `--version` with, for the version these three numbers spell.
fn answer(version: [u32; 3]) -> String {
    let [major, minor, patch] = version;

    format!(
        "cargo-nextest {major}.{minor}.{patch} (8af696ddc 2026-09-21)\nrelease: {major}.{minor}.{patch}\n"
    )
}

#[test]
fn no_selection_is_the_whole_workspace() {
    assert_eq!(
        test_arguments(None, None, None),
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
    let arguments = test_arguments(Some("curios-abi"), None, None);

    assert_eq!(
        arguments[..4],
        ["nextest", "run", "--package", "curios-abi"]
    );
    assert!(
        !arguments.contains(&"--workspace".to_owned()),
        "{arguments:?}"
    );
}

#[test]
fn a_shard_is_a_slice_and_the_filter_still_comes_last() {
    let shard = Shard { index: 2, total: 3 };
    let arguments = test_arguments(None, Some("universes"), Some(shard));

    assert_eq!(
        arguments[arguments.len() - 3..],
        ["--partition", "slice:2/3", "universes"]
    );
}

#[test]
fn a_shard_counts_from_one_up_to_its_total() {
    assert_eq!("2/3".parse(), Ok(Shard { index: 2, total: 3 }));
    assert_eq!("1/1".parse(), Ok(Shard { index: 1, total: 1 }));

    for refused in ["", "3", "0/3", "4/3", "1/0", "a/3", "1/b", "1/3/5", "-1/3"] {
        assert!(refused.parse::<Shard>().is_err(), "{refused:?}");
    }
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
fn the_version_is_the_three_numbers_the_answer_leads_with() {
    assert_eq!(nextest_version(&answer([0, 9, 146])), Some([0, 9, 146]));
    assert_eq!(
        nextest_version("cargo-nextest 0.9.147-b.1 (8af696ddc 2026-09-21)"),
        Some([0, 9, 147])
    );

    for unread in [
        "",
        "cargo-nextest",
        "cargo-nextest 0.9",
        "cargo-nextest 0.9.1.2",
        "cargo-nextest latest",
    ] {
        assert_eq!(nextest_version(unread), None, "{unread:?}");
    }
}

#[test]
fn the_floor_is_admitted_and_the_release_before_it_is_not() {
    let [major, minor, patch] = NEXTEST_FLOOR;

    assert!(admits_nextest(&answer(NEXTEST_FLOOR)));
    assert!(!admits_nextest(&answer([major, minor, patch - 1])));
    // Compared as numbers: a later minor with an earlier patch is still later.
    assert!(admits_nextest(&answer([major, minor + 1, 0])));
    assert!(!admits_nextest("cargo-nextest latest"));
}

#[test]
fn a_refusal_names_the_floor_and_puts_the_fix_on_a_line_of_its_own() {
    let [major, minor, patch] = NEXTEST_FLOOR;
    let refusal = nextest_refusal("it is not installed", "Install");

    assert_eq!(
        refusal,
        format!(
            "test needs cargo-nextest {major}.{minor}.{patch} or later, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
        )
    );
}
