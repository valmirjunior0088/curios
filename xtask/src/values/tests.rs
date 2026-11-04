//! What braces stand for in a run, each from task files of the test's own.

use {super::*, crate::*};

/// The one step of the file `a`, whose top is `top` and whose step runs `command`, under a top holding nothing of its own.
fn step(top: &str, command: &str) -> Tree {
    Tree::parse(&[
        ("xtask/tasks.toml", "description = \"The top\"\n"),
        (
            "xtask/tasks/a.toml",
            &format!("description = \"A\"\n{top}\n[s]\ndescription = \"S\"\nrun = [{command}]\n"),
        ),
    ])
    .unwrap()
}

/// The words the step's one command is under `given`.
fn words(tree: &Tree, given: &[(&str, &str)]) -> Option<Vec<String>> {
    let given = given
        .iter()
        .map(|(name, value)| {
            (
                name.split('.').map(str::to_string).collect(),
                value.to_string(),
            )
        })
        .collect::<Given>();

    Values::new(tree, &given)
        .command(&tree.steps[0].run[0])
        .unwrap()
}

#[test]
fn a_list_stands_for_its_words() {
    let tree = step(
        "variables.list = [\"x\", \"y\"]",
        "[\"echo\", \"{list}\", \"z\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "x", "y", "z"]);
}

#[test]
fn a_string_sits_inside_a_word_beside_text() {
    let tree = step("variables.name = \"x\"", "[\"echo\", \"--{name}=1\"]");

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "--x=1"]);
}

#[test]
fn a_flag_is_its_flag_and_value_where_given_and_nothing_where_not() {
    let tree = step(
        "variables.only = { flag = \"-E\" }",
        "[\"echo\", \"{only}\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo"]);
    assert_eq!(
        words(&tree, &[("a.only", "test(~x)")]).unwrap(),
        ["echo", "-E", "test(~x)"]
    );
}

#[test]
fn an_argument_is_its_default_where_none_is_given() {
    let tree = step(
        "variables.file = { argument = \"PATH\", else = \"default.crs\" }",
        "[\"echo\", \"{file}\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "default.crs"]);
    assert_eq!(
        words(&tree, &[("a.file", "other.crs")]).unwrap(),
        ["echo", "other.crs"]
    );
}

/// A tool handed no paths reads its standard input or the whole directory, so a command whose list came back empty has nothing to run on.
#[test]
fn a_command_whose_printed_list_is_empty_is_not_run() {
    let empty = step(
        "variables.files = { run = [\"true\"] }",
        "[\"echo\", \"{files}\"]",
    );
    let printed = step(
        "variables.files = { run = [\"echo\", \"one\"] }",
        "[\"echo\", \"{files}\"]",
    );

    assert_eq!(words(&empty, &[]), None);
    assert_eq!(words(&printed, &[]).unwrap(), ["echo", "one"]);
}

#[test]
fn a_produced_file_is_its_path_from_the_root() {
    let tree = Tree::parse(&[
        ("xtask/tasks.toml", "description = \"The top\"\n"),
        (
            "xtask/tasks/a.toml",
            "description = \"A\"\n[s]\ndescription = \"S\"\nproduces = { out = \"some/{host}.d\" }\nrun = [[\"touch\", \"{out}\"]]\n",
        ),
    ])
    .unwrap();

    assert_eq!(
        words(&tree, &[]).unwrap(),
        [
            "touch".to_string(),
            root()
                .join(format!("some/{HOST_TRIPLE}.d"))
                .to_string_lossy()
                .into_owned()
        ]
    );
}
