//! What the reader makes of task files, and every refusal it can make, each from files of the test's own.

use super::*;

const TOP: &str = "description = \"The top\"\n";

/// The tree the files `(name, text)` of [`DIRECTORY`] make, under a top holding nothing of its own.
fn tree(files: &[(&str, &str)]) -> Result<Tree, Refusal> {
    let paths = files
        .iter()
        .map(|(name, _)| format!("{DIRECTORY}/{name}.toml"))
        .collect::<Vec<_>>();
    let mut all = vec![(ROOT_FILE, TOP)];
    all.extend(
        paths
            .iter()
            .map(String::as_str)
            .zip(files.iter().map(|(_, text)| *text)),
    );

    Tree::parse(&all)
}

/// The message the reader refuses the files with.
fn refused(files: &[(&str, &str)]) -> String {
    tree(files).unwrap_err().message
}

fn name(text: &str) -> Name {
    text.split('.').map(str::to_string).collect()
}

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
fn a_name_without_a_dot_is_its_file_s_and_with_dots_a_path_from_the_top() {
    let tree = tree(&[
        (
            "a",
            "description = \"A\"\n[build]\ndescription = \"Build\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"Test\"\nconsumes = [\"binary\"]\nrun = [[\"true\", \"{binary}\"]]\n",
        ),
        (
            "b",
            "description = \"B\"\n[test]\ndescription = \"Test\"\nconsumes = [\"a.binary\"]\nrun = [[\"true\", \"{a.binary}\"]]\n",
        ),
    ])
    .unwrap();

    for step in ["a.test", "b.test"] {
        let step = tree.steps_of(&name(step))[0];
        assert_eq!(tree.steps[step].consumes, [name("a.binary")]);
        assert_eq!(tree.prerequisites(step), tree.steps_of(&name("a.build")));
    }
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
fn a_name_declared_twice_in_a_file_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\nvariables.build = \"x\"\n[build]\ndescription = \"B\"\nrun = [[\"true\"]]\n"
        )])
        .contains("declared twice")
    );
}

#[test]
fn a_name_that_names_nothing_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nrequires = [\"nothing\"]\n"
        )])
        .contains("`nothing` names nothing")
    );
}

#[test]
fn a_file_in_requires_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nrequires = [\"binary\"]\n"
        )])
        .contains("belongs in `consumes`")
    );
}

#[test]
fn a_step_in_consumes_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nconsumes = [\"build\"]\nrun = [[\"true\"]]\n"
        )])
        .contains("belongs in `requires`")
    );
}

/// A command that reads a produced file its step does not consume would run without the edge that orders it after the step making the file.
#[test]
fn a_produced_file_read_without_consuming_it_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nrun = [[\"true\", \"{binary}\"]]\n"
        )])
        .contains("neither consumes nor produces")
    );
}

#[test]
fn a_step_or_group_in_braces_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nrun = [[\"true\", \"{build}\"]]\n"
        )])
        .contains("has no value")
    );
}

#[test]
fn steps_that_run_after_each_other_are_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[x]\ndescription = \"X\"\nrequires = [\"y\"]\nrun = [[\"true\"]]\n[y]\ndescription = \"Y\"\nrequires = [\"x\"]\nrun = [[\"true\"]]\n"
        )])
        .contains("run after each other")
    );
}

/// A list stands for several words, a flag for two or none, and a command's output for as many as it prints, so none can sit beside text in one word.
#[test]
fn a_value_of_several_words_inside_a_word_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\nvariables.list = [\"x\", \"y\"]\n[s]\ndescription = \"S\"\nrun = [[\"true\", \"--{list}\"]]\n"
        )])
        .contains("a word of its own")
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
fn a_value_the_runner_fills_cannot_be_declared() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\nvariables.host = \"x\"\n[s]\ndescription = \"S\"\nrun = [[\"true\"]]\n"
        )])
        .contains("the runner fills")
    );
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
