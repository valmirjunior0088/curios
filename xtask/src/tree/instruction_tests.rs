//! What a step runs: commands, and writes of a file whole, each held to the shape it takes.

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
        "description = \"A\"\nvariables.landing = \"held\"\n[s]\ndescription = \"S\"\nrun = [[\"true\"], { write = \"{target}/landing\", contents = \"{landing}\" }]\n",
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
            Piece::Value(Fill::Target),
            Piece::Text("/landing".to_string())
        ]
    );
    assert_eq!(contents.alone(), Some(&Fill::Variable(name("a.landing"))));
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
        run_refused(flag, "[{ write = \"a\", contents = \"{only}\" }]")
            .contains("may stand for several words or none")
    );
}

#[test]
fn an_instruction_neither_a_command_nor_a_write_is_refused() {
    assert!(run_refused("", "[\"true\"]").contains("an instruction is a command"));
}
