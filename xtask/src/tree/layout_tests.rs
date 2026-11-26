//! The layout the reader holds the task files to: a file is a group of steps, each key where its kind sits, and a refusal placed where it stands.

use {super::test_support::*, super::*};

#[test]
fn a_file_is_a_group_holding_its_steps_in_order() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\n[lint]\nwhat = \"Lint\"\n[lint.second]\nwhat = \"Two\"\nrun = [[\"true\"]]\n[lint.first]\nwhat = \"One\"\nrun = [[\"true\"]]\n[test]\nwhat = \"Three\"\nrun = [[\"true\"]]\n",
    )])
    .unwrap();

    let steps = |group: &str| {
        tree.steps_of(&name(group))
            .into_iter()
            .map(|step| dotted(&tree.steps[step].name))
            .collect::<Vec<_>>()
    };
    assert_eq!(steps("a"), ["a.lint.second", "a.lint.first", "a.test"]);
    assert_eq!(steps("a.lint"), ["a.lint.second", "a.lint.first"]);
    assert_eq!(steps("a.test"), ["a.test"]);
    assert_eq!(tree.groups[&Vec::new()].children, [name("a")]);
}

#[test]
fn a_key_out_of_place_at_a_file_s_top_is_refused() {
    assert!(refused(&[("a", "what = \"A\"\nrun = [[\"true\"]]\n")]).contains("out of place"));
}

#[test]
fn a_value_below_a_file_s_top_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[s]\nwhat = \"S\"\nx = \"y\"\nrun = [[\"true\"]]\n"
        )])
        .contains("a value belongs at a file's top")
    );
}

#[test]
fn a_table_inside_a_step_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[s]\nwhat = \"S\"\nrun = [[\"true\"]]\n[s.t]\nwhat = \"T\"\nrun = [[\"true\"]]\n"
        )])
        .contains("a table inside a step")
    );
}

#[test]
fn a_group_holding_anything_but_steps_and_groups_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[g]\nwhat = \"G\"\nx = 1\n[g.s]\nwhat = \"S\"\nrun = [[\"true\"]]\n"
        )])
        .contains("a group holds its `what` and its steps and groups")
    );
}

/// A group states what it is for, as a file's top does, so the command line describes it by its own line rather than its steps' joined.
#[test]
fn a_group_without_a_description_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[g.s]\nwhat = \"S\"\nrun = [[\"true\"]]\n"
        )])
        .contains("group `.a.g` has no `what`")
    );
}

#[test]
fn a_step_without_its_what_is_refused() {
    assert!(refused(&[("a", "what = \"A\"\n[s]\nrun = [[\"true\"]]\n")]).contains("has no `what`"));
}

#[test]
fn a_file_without_its_what_is_refused() {
    assert!(refused(&[("a", "[s]\nwhat = \"S\"\nrun = [[\"true\"]]\n")]).contains("has no `what`"));
}

/// A refusal is drawn as what it says, the line it is about with the place marked, and the file, line and column it closes on.
#[test]
fn a_refusal_marks_its_place_and_closes_on_its_file_line_and_column() {
    let refusal = tree(&[(
        "a",
        "what = \"A\"\n[s]\nwhat = \"S\"\nrequires = [\".nothing\"]\n",
    )])
    .unwrap_err()
    .rendered(false);

    assert_eq!(
        refusal,
        [
            "`.nothing` names nothing",
            "│",
            "│  4 │ requires = [\".nothing\"]",
            "│    │             ^^^^^^^^^^",
            "│",
            "└─ [error] xtask/tasks/a.toml:4:13",
        ]
        .join("\n")
    );
}
