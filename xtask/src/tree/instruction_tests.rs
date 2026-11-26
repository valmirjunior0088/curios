//! What a step runs: commands and writes of a file whole, each held to the shape it takes.

use super::{test_support::*, *};

/// The refusal of the one step `s` of the file `a`, which runs `run` under a top declaring `variables`.
fn run_refused(variables: &str, run: &str) -> String {
    refused(&[(
        "a",
        &format!("what = \"A\"\n{variables}[s]\nwhat = \"S\"\nrun = {run}\n"),
    )])
}

/// A command written as a table carries the directory it runs in beside its words, and one written as a list runs at the root.
#[test]
fn a_command_carries_its_directory_where_it_is_a_table() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\n[s]\nwhat = \"S\"\nrun = [[\"true\"], { in = \"editors/zed\", do = [\"cargo\", \"fmt\"] }]\n",
    )])
    .unwrap();

    let run = &tree.steps[0].run;
    assert!(matches!(
        run[0],
        Instruction::Command {
            directory: None,
            ..
        }
    ));
    let Instruction::Command { words, directory } = &run[1] else {
        panic!("the second instruction is a command");
    };
    assert_eq!(
        directory.as_ref().map(Word::written).as_deref(),
        Some("editors/zed")
    );
    assert_eq!(words.len(), 2);
}

/// A command's table holds its words and its directory and nothing else.
#[test]
fn a_command_table_holding_another_key_is_refused() {
    assert!(
        run_refused("", "[{ in = \"x\", do = [\"true\"], dir = \"y\" }]")
            .contains("`dir` is neither")
    );
}

#[test]
fn a_write_names_its_file_and_what_it_holds_beside_the_commands() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\nlanding = \"held\"\n[s]\nwhat = \"S\"\nrun = [[\"true\"], { write = \"{!cargo_target_dir}/landing\", contents = \"{.a.landing}\" }]\n",
    )])
    .unwrap();

    let run = &tree.steps[0].run;
    assert!(matches!(run[0], Instruction::Command { .. }));
    let Instruction::Write { path, contents, .. } = &run[1] else {
        panic!("the second instruction is a write");
    };
    assert_eq!(
        path.0,
        [
            Piece::Value(Fill::CargoTargetDir),
            Piece::Text("/landing".to_string())
        ]
    );
    assert_eq!(contents.alone(), Some(&Fill::Variable(name("a.landing"))));
}

#[test]
fn a_write_holds_its_file_and_its_contents_or_template_and_nothing_else() {
    assert!(
        run_refused("", "[{ write = \"a\", contents = \"b\", mode = \"x\" }]")
            .contains("`mode` is none of them")
    );
    assert!(
        run_refused("", "[{ write = \"a\" }]").contains("as its `contents` or as a `template`")
    );
    assert!(
        run_refused(
            "",
            "[{ write = \"a\", contents = \"b\", template = \"c\" }]"
        )
        .contains("one of the two")
    );
    assert!(run_refused("", "[{ contents = \"b\" }]").contains("names the file it writes"));
}

/// A flag stands for two words or none and a command's output for as many as it prints, so neither is the one string a file's name or contents is.
#[test]
fn a_write_of_a_value_that_may_be_several_words_is_refused() {
    let flag = "only.flag = \"-E\"\nonly.help = \"H\"\n";

    assert!(
        run_refused(flag, "[{ write = \"a\", contents = \"{.a.only}\" }]")
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
        let variables = ["only.flag = ", flag, "\n"].concat();

        assert!(
            run_refused(&variables, "[\"echo\", \"{.a.only}\"]").contains("a flag is one spelling"),
            "{flag}"
        );
    }
}

#[test]
fn an_instruction_neither_a_command_nor_a_write_is_refused() {
    assert!(run_refused("", "[\"true\"]").contains("an instruction is a command"));
}
