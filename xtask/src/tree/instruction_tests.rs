//! What a step runs: commands, writes of a file whole, and the checks the runner holds, each held to the shape it takes.

use super::{test_support::*, *};

/// The refusal of the one step `s` of the file `a`, which runs `run` under a top declaring `variables`.
fn run_refused(variables: &str, run: &str) -> String {
    refused(&[(
        "a",
        &format!("description = \"A\"\n{variables}[s]\ndescription = \"S\"\nrun = {run}\n"),
    )])
}

#[test]
fn a_write_names_its_file_and_what_it_holds_beside_the_commands() {
    let tree = tree(&[(
        "a",
        "description = \"A\"\nvariables.landing = \"held\"\n[s]\ndescription = \"S\"\nrun = [[\"true\"], { write = \"{!cargo_target_dir}/landing\", contents = \"{.a.variables.landing}\" }]\n",
    )])
    .unwrap();

    let run = &tree.steps[0].run;
    assert!(matches!(run[0], Instruction::Command(_)));
    let Instruction::Write { path, contents } = &run[1] else {
        panic!("the second instruction is a write");
    };
    assert_eq!(
        path.0,
        [
            Piece::Value(Fill::CargoTargetDir),
            Piece::Text("/landing".to_string())
        ]
    );
    assert_eq!(
        contents.alone(),
        Some(&Fill::Variable(name("a.variables.landing")))
    );
}

#[test]
fn a_write_holds_its_file_and_contents_and_nothing_else() {
    assert!(
        run_refused("", "[{ write = \"a\", contents = \"b\", mode = \"x\" }]")
            .contains("`mode` is neither")
    );
    assert!(run_refused("", "[{ write = \"a\" }]").contains("lacks `contents`"));
    assert!(run_refused("", "[{ contents = \"b\" }]").contains("lacks `write`"));
}

/// A flag stands for two words or none and a command's output for as many as it prints, so neither is the one string a file's name or contents is.
#[test]
fn a_write_of_a_value_that_may_be_several_words_is_refused() {
    let flag = "variables.only.flag = \"-E\"\nvariables.only.help = \"H\"\n";

    assert!(
        run_refused(
            flag,
            "[{ write = \"a\", contents = \"{.a.variables.only}\" }]"
        )
        .contains("may stand for several words or none")
    );
}

/// A flag is a short spelling, a long one, or one of each: two of a kind, a word with no dash, a dash with no name and a list of none are refused where they are written.
#[test]
fn a_flag_that_is_no_short_or_long_spelling_is_refused() {
    for flag in [
        "\"E\"",
        "\"---\"",
        "[]",
        "[\"-E\", \"-F\"]",
        "[\"--one\", \"--other\"]",
    ] {
        let variables = ["variables.only.flag = ", flag, "\n"].concat();

        assert!(
            run_refused(&variables, "[\"echo\", \"{.a.variables.only}\"]")
                .contains("a flag is one spelling"),
            "{flag}"
        );
    }
}

#[test]
fn an_instruction_neither_a_command_nor_a_write_nor_a_check_is_refused() {
    assert!(run_refused("", "[\"true\"]").contains("an instruction is a command"));
}

#[test]
fn a_check_names_a_judgment_the_runner_holds() {
    let tree = tree(&[(
        "a",
        "description = \"A\"\n[s]\ndescription = \"S\"\nrun = [{ check = \"zed\" }]\n",
    )])
    .unwrap();

    let [Instruction::Check(check)] = tree.steps[0].run.as_slice() else {
        panic!("the one instruction is a check");
    };
    assert_eq!(check.name, "zed");
}

/// A name the runner holds no check under is refused naming those it holds, and a check holds its name and nothing else.
#[test]
fn a_check_the_runner_does_not_hold_is_refused_naming_those_it_does() {
    assert_eq!(
        run_refused("", "[{ check = \"nothing\" }]"),
        "`nothing` is no check the runner holds, which are `agents`, `zed`"
    );
    assert!(run_refused("", "[{ check = \"zed\", dir = \"x\" }]").contains("`dir` is not it"));
}
