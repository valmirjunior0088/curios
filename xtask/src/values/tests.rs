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
    let mut gathered = GivenValues::new();
    for (name, value) in given {
        gathered
            .entry(name.split('.').map(str::to_string).collect())
            .or_default()
            .push(value.to_string());
    }
    let given = gathered;

    let Instruction::Command(command) = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };

    Values::new(tree, &given).command(command).unwrap()
}

#[test]
fn a_list_stands_for_its_words() {
    let tree = step(
        "variables.list = [\"x\", \"y\"]",
        "[\"echo\", \"{.a.variables.list}\", \"z\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "x", "y", "z"]);
}

#[test]
fn a_string_sits_inside_a_word_beside_text() {
    let tree = step(
        "variables.name = \"x\"",
        "[\"echo\", \"--{.a.variables.name}=1\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "--x=1"]);
}

#[test]
fn a_flag_is_its_flag_and_value_where_given_and_nothing_where_not() {
    let tree = step(
        "given.only = { flag = \"-E\" }",
        "[\"echo\", \"{.a.given.only}\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo"]);
    assert_eq!(
        words(&tree, &[("a.given.only", "test(~x)")]).unwrap(),
        ["echo", "-E", "test(~x)"]
    );
}

/// A flag given more than once is handed on each time, in the order given, so a command that takes several, as nextest takes several packages, is handed every one: whether it takes more than one is the command's to say.
#[test]
fn a_flag_given_twice_is_handed_on_twice() {
    let tree = step(
        "given.package = { flag = [\"-p\", \"--package\"] }",
        "[\"echo\", \"{.a.given.package}\"]",
    );

    assert_eq!(
        words(
            &tree,
            &[("a.given.package", "first"), ("a.given.package", "second")]
        )
        .unwrap(),
        ["echo", "--package", "first", "--package", "second"]
    );
}

/// A flag of a short spelling and a long one is handed to its command by the long one, so the echoed line reads by itself.
#[test]
fn a_flag_of_two_spellings_is_handed_on_by_its_long_one() {
    let tree = step(
        "given.only = { flag = [\"-E\", \"--filterset\"] }",
        "[\"echo\", \"{.a.given.only}\"]",
    );

    assert_eq!(
        words(&tree, &[("a.given.only", "test(~x)")]).unwrap(),
        ["echo", "--filterset", "test(~x)"]
    );
}

#[test]
fn an_argument_is_its_default_where_none_is_given() {
    let tree = step(
        "given.file = { argument = \"PATH\", else = \"default.crs\" }",
        "[\"echo\", \"{.a.given.file}\"]",
    );

    assert_eq!(words(&tree, &[]).unwrap(), ["echo", "default.crs"]);
    assert_eq!(
        words(&tree, &[("a.given.file", "other.crs")]).unwrap(),
        ["echo", "other.crs"]
    );
}

/// A tool handed no paths reads its standard input or the whole directory, so a command whose list came back empty has nothing to run on.
#[test]
fn a_command_whose_printed_list_is_empty_is_not_run() {
    let empty = step(
        "variables.files = { run = [\"true\"] }",
        "[\"echo\", \"{.a.variables.files}\"]",
    );
    let printed = step(
        "variables.files = { run = [\"echo\", \"one\"] }",
        "[\"echo\", \"{.a.variables.files}\"]",
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
            "description = \"A\"\n[s]\ndescription = \"S\"\nproduces = { out = \"some/{!host_triple}.d\" }\nrun = [[\"touch\", \"{.a.s.produces.out}\"]]\n",
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

/// A variable's command is launched as a step's is, so a program it names that is not there is refused, naming the variable and the program.
#[test]
fn a_variables_command_naming_a_missing_program_is_refused_naming_both() {
    let tree = Tree::parse(&[
        ("xtask/tasks.toml", "description = \"The top\"\n"),
        (
            "xtask/tasks/a.toml",
            "description = \"A\"\nvariables.listed.run = [\"no-such-lister\"]\n[s]\ndescription = \"S\"\nrun = [[\"echo\", \"{.a.variables.listed}\"]]\n",
        ),
    ])
    .unwrap();
    let Instruction::Command(command) = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };

    let refusal = Values::new(&tree, &GivenValues::new())
        .command(command)
        .unwrap_err();
    assert!(
        refusal.starts_with(
            "`.a.variables.listed` is what `no-such-lister` prints: cannot run no-such-lister:"
        ),
        "{refusal}"
    );
}

/// The file `contents` makes, in a directory of its own named for `test`, by its absolute path, which a step reads from the root as it stands.
fn manifest(test: &str, contents: &str) -> std::path::PathBuf {
    let directory =
        std::env::temp_dir().join(format!("curios-xtask-{test}-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let path = directory.join("manifest.toml");
    std::fs::write(&path, contents).unwrap();

    path
}

#[test]
fn a_key_of_a_file_stands_for_the_string_it_holds() {
    let path = manifest("read", "[workspace.package]\nedition = \"2024\"\n");
    let tree = step(
        &format!(
            "variables.edition = {{ read = \"{}\", key = \"workspace.package.edition\" }}",
            path.display()
        ),
        "[\"rustfmt\", \"--edition={.a.variables.edition}\"]",
    );

    let read = words(&tree, &[]);
    let _ = std::fs::remove_dir_all(path.parent().expect("the file is in a directory"));
    assert_eq!(read.unwrap(), ["rustfmt", "--edition=2024"]);
}

/// A key the file does not hold, or holds as something other than a string, is refused, naming the variable, the file and the key.
#[test]
fn a_key_a_file_does_not_hold_as_a_string_is_refused_naming_both() {
    let path = manifest("unread", "[workspace.package]\nedition = 2024\n");
    let refusal = |key: &str| {
        let tree = step(
            &format!(
                "variables.edition = {{ read = \"{}\", key = \"{key}\" }}",
                path.display()
            ),
            "[\"rustfmt\", \"{.a.variables.edition}\"]",
        );
        let Instruction::Command(command) = &tree.steps[0].run[0] else {
            panic!("the step runs a command");
        };

        Values::new(&tree, &GivenValues::new())
            .command(command)
            .unwrap_err()
    };

    let keys = ["workspace.package.edition", "workspace.edition"];
    let refused = keys.map(refusal);
    let _ = std::fs::remove_dir_all(path.parent().expect("the file is in a directory"));
    for (refusal, key) in refused.iter().zip(keys) {
        assert_eq!(
            refusal,
            &format!(
                "`.a.variables.edition` is what `{}` holds at `{key}`: no string stands there",
                path.display()
            )
        );
    }
}

// Data stands for the directory it is kept as, under its name and version: with the runner's own where a step's command reads it, and beside the crate its table names where that crate's build does.
#[test]
fn data_stands_for_its_kept_directory() {
    let tree = Tree::parse(&[
        (
            "xtask/tasks.toml",
            "description = \"The top\"\n[data.cases]\nversion = \"abc\"\nurl = \"https://example.invalid/abc.tar.gz\"\npath = \"cases\"\nsha256 = \"0000000000000000000000000000000000000000000000000000000000000000\"\n[data.tables]\nversion = \"7.0\"\nurl = \"https://example.invalid/ucd\"\nfiles = [\"one.txt\"]\nsha256 = \"0000000000000000000000000000000000000000000000000000000000000000\"\nbeside = \"some-crate\"\n",
        ),
        (
            "xtask/tasks/a.toml",
            "description = \"A\"\n[s]\ndescription = \"S\"\nrun = [[\"ls\", \"{.data.cases}\", \"{.data.tables}\"]]\n",
        ),
    ])
    .unwrap();

    assert_eq!(
        words(&tree, &[]).unwrap(),
        [
            "ls".to_string(),
            artifacts()
                .join("data/cases-abc")
                .to_string_lossy()
                .into_owned(),
            root()
                .join("some-crate/.artifacts/tables-7.0")
                .to_string_lossy()
                .into_owned()
        ]
    );
}
