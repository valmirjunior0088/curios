//! The layout the reader holds the task files to: a file is a group of steps, each key where its kind sits, and a refusal placed where it stands.

use {super::test_support::*, super::*};

#[test]
fn a_file_is_a_group_holding_its_steps_in_order() {
    let tree = tree(&[(
        "a",
        "description = \"A\"\n[lint.second]\ndescription = \"Two\"\nrun = [[\"true\"]]\n[lint.first]\ndescription = \"One\"\nrun = [[\"true\"]]\n[test]\ndescription = \"Three\"\nrun = [[\"true\"]]\n",
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
    assert!(
        refused(&[("a", "description = \"A\"\nrun = [[\"true\"]]\n")]).contains("out of place")
    );
}

#[test]
fn variables_below_a_file_s_top_are_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nvariables.x = \"y\"\nrun = [[\"true\"]]\n"
        )])
        .contains("belongs at a file's top")
    );
}

#[test]
fn a_table_inside_a_step_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nrun = [[\"true\"]]\n[s.t]\ndescription = \"T\"\nrun = [[\"true\"]]\n"
        )])
        .contains("a table inside a step")
    );
}

#[test]
fn a_group_holding_anything_but_steps_and_groups_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[g]\nx = 1\n[g.s]\ndescription = \"S\"\nrun = [[\"true\"]]\n"
        )])
        .contains("a group holds only steps and groups")
    );
}

#[test]
fn a_step_without_a_description_is_refused() {
    assert!(
        refused(&[("a", "description = \"A\"\n[s]\nrun = [[\"true\"]]\n")])
            .contains("has no `description`")
    );
}

#[test]
fn a_file_without_a_description_is_refused() {
    assert!(
        refused(&[("a", "[s]\ndescription = \"S\"\nrun = [[\"true\"]]\n")])
            .contains("has no `description`")
    );
}

#[test]
fn a_task_named_like_a_command_outside_the_tree_is_refused() {
    let refusal = Tree::parse(&[(
        ROOT_FILE,
        "description = \"Top\"\n[release]\ndescription = \"R\"\nrun = [[\"true\"]]\n",
    )])
    .unwrap_err();

    assert!(refusal.message.contains("outside the tree"), "{refusal}");
}

#[test]
fn a_refusal_names_its_file_line_and_column_and_marks_the_place() {
    let refusal = tree(&[(
        "a",
        "description = \"A\"\n[s]\ndescription = \"S\"\nrequires = [\"nothing\"]\n",
    )])
    .unwrap_err()
    .to_string();

    assert!(
        refusal.starts_with("xtask/tasks/a.toml:4:13: `nothing` names nothing"),
        "{refusal}"
    );
    assert!(refusal.ends_with("^^^^^^^^^"), "{refusal}");
}
