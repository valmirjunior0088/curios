//! The programs the task files pin, each at the version a run needs.

use {super::test_support::*, super::*};

/// A program is its version, written beside its name: exact, or a minimum after `>=`, spaced or not.
#[test]
fn a_program_is_pinned_at_its_version_exactly_or_at_least() {
    let tree = tree(&[(
        "a",
        "description = \"A\"\nprograms.tool = \"1.2.3\"\nprograms.other = \">= 22\"\nprograms.unspaced = \">=1.2\"\n",
    )])
    .unwrap();

    assert_eq!(tree.programs["tool"], Pin::Exact("1.2.3".to_string()));
    assert_eq!(tree.programs["other"], Pin::Minimum("22".to_string()));
    assert_eq!(tree.programs["unspaced"], Pin::Minimum("1.2".to_string()));
}

/// A program is its version alone, so a table of keys, a version beside what installs it among them, is refused.
#[test]
fn a_program_that_is_no_version_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[programs.tool]\nversion = \"1\"\ninstall = \"get tool\"\n"
        )])
        .contains("a program is pinned at its version, a string")
    );
}

#[test]
fn a_pin_that_is_not_numbers_between_dots_is_refused() {
    assert!(
        refused(&[("a", "description = \"A\"\nprograms.tool = \"^1.2\"\n")])
            .contains("a version is numbers between dots")
    );
}

#[test]
fn a_program_declared_in_two_files_is_refused() {
    assert!(
        refused(&[
            ("a", "description = \"A\"\nprograms.tool = \"1\"\n"),
            ("b", "description = \"B\"\nprograms.tool = \">= 2\"\n"),
        ])
        .contains("program `tool` is declared twice")
    );
}

#[test]
fn programs_below_a_file_s_top_are_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nprograms.tool = \"1\"\nrun = [[\"true\"]]\n"
        )])
        .contains("`programs` belongs at a file's top")
    );
}
