//! The programs the task files declare: their pins and the lines that install them.

use {super::test_support::*, super::*};

#[test]
fn a_program_s_install_line_is_filled_with_its_pin() {
    let tree = tree(&[(
        "a",
        "description = \"A\"\n[programs.tool]\nversion = \"1.2.3\"\ninstall = \"get tool@{version}\"\n[programs.other]\nversion = \">= 22\"\n[programs.unspaced]\nversion = \">=1.2\"\n",
    )])
    .unwrap();

    assert_eq!(
        tree.programs["tool"].pin,
        Some(Pin::Exact("1.2.3".to_string()))
    );
    assert_eq!(
        tree.programs["tool"].install.as_deref(),
        Some("get tool@1.2.3")
    );
    assert_eq!(
        tree.programs["other"].pin,
        Some(Pin::Minimum("22".to_string()))
    );
    assert_eq!(tree.programs["other"].install, None);
    assert_eq!(
        tree.programs["unspaced"].pin,
        Some(Pin::Minimum("1.2".to_string()))
    );
}

#[test]
fn a_key_no_program_has_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[programs.tool]\nminimum = \"1\"\n"
        )])
        .contains("`minimum` is no key of a program")
    );
}

#[test]
fn a_pin_that_is_not_numbers_between_dots_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[programs.tool]\nversion = \"^1.2\"\n"
        )])
        .contains("a version is numbers between dots")
    );
}

#[test]
fn an_install_line_naming_a_pin_it_lacks_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[programs.tool]\ninstall = \"get tool@{version}\"\n"
        )])
        .contains("this program has none")
    );
}

#[test]
fn a_program_declared_in_two_files_is_refused() {
    assert!(
        refused(&[
            (
                "a",
                "description = \"A\"\n[programs.tool]\nversion = \"1\"\n"
            ),
            (
                "b",
                "description = \"B\"\n[programs.tool]\nversion = \">= 2\"\n"
            ),
        ])
        .contains("program `tool` is declared twice")
    );
}

#[test]
fn programs_below_a_file_s_top_are_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nprograms.tool.version = \"1\"\nrun = [[\"true\"]]\n"
        )])
        .contains("`programs` belongs at a file's top")
    );
}
