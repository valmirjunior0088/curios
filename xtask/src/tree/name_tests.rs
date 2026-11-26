//! Names and values: how a name resolves, what a word may hold, and the order what a step reads puts it in.

use super::{test_support::*, *};

/// A name is the path from the top to where it is declared, a key at a time, so it is spelled alike in its own file and in another, and a variable sits under its file's `variables` and a produced file under its step's `produces`.
#[test]
fn a_name_is_the_path_from_the_top_to_its_declaration_in_every_file() {
    let tree = tree(&[
        (
            "a",
            "what = \"A\"\nflavor = \"plain\"\n[build]\nwhat = \"Build\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\nwhat = \"Test\"\nconsumes = [\".a.build.produces.binary\"]\nrun = [[\"true\", \"{.a.build.produces.binary}\", \"{.a.flavor}\"]]\n",
        ),
        (
            "b",
            "what = \"B\"\n[test]\nwhat = \"Test\"\nconsumes = [\".a.build.produces.binary\"]\nrun = [[\"true\", \"{.a.build.produces.binary}\", \"{.a.flavor}\"]]\n",
        ),
    ])
    .unwrap();

    for step in ["a.test", "b.test"] {
        let step = tree.steps_of(&name(step))[0];
        assert_eq!(tree.steps[step].consumes, [name("a.build.produces.binary")]);
        assert_eq!(tree.prerequisites(step), tree.steps_of(&name("a.build")));
        let Instruction::Command { words, .. } = &tree.steps[step].run[0] else {
            panic!("the step runs a command");
        };
        assert_eq!(words[2].0, [Piece::Value(Fill::Variable(name("a.flavor")))]);
    }
}

/// A command is said as its task file writes it, each value between its braces by its path from the top or its built-in name, so a reader of the transcript looks it up where it is written: text around a value stays as it stands.
#[test]
fn a_word_is_written_back_as_its_task_file_writes_it() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\nflavor = \"plain\"\n[test]\nwhat = \"Test\"\nrun = [[\"true\", \"--flavor={.a.flavor}\", \"{!host_triple}\"]]\n",
    )])
    .unwrap();

    let step = tree.steps_of(&name("a.test"))[0];
    let Instruction::Command { words, .. } = &tree.steps[step].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(
        words.iter().map(Word::written).collect::<Vec<_>>(),
        ["true", "--flavor={.a.flavor}", "{!host_triple}"]
    );
}

/// A name without its leading `.` is refused, whatever it would once have named, so no name is read relative to the file it is written in.
#[test]
fn a_name_without_its_leading_dot_is_refused() {
    for (requires, run) in [
        ("[\"build\"]", "[]"),
        ("[]", "[[\"true\", \"{a.flavor}\"]]"),
    ] {
        let refusal = refused(&[(
            "a",
            &format!(
                "what = \"A\"\nflavor = \"plain\"\n[build]\nwhat = \"B\"\nrun = [[\"true\"]]\n[s]\nwhat = \"S\"\nrequires = {requires}\nrun = {run}\n"
            ),
        )]);
        assert!(
            refusal.contains("is no path: a name is written from the top with a leading `.`"),
            "{refusal}"
        );
    }
}

/// A table at a file's top that states no `what` and is no value is a table of values, data like any other, each value named by its path below it, and a table that states its `what` is a task. Mutation-checked: with no table read as a table of values, `scripts` is a group without its `what`.
#[test]
fn a_table_of_values_names_each_value_by_its_path() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\nscripts.pins = \"pins.sh\"\n[s]\nwhat = \"S\"\nrun = [[\"sh\", \"{.a.scripts.pins}\"]]\n",
    )])
    .unwrap();

    let Instruction::Command { words, .. } = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(
        words[1].0,
        [Piece::Value(Fill::Variable(name("a.scripts.pins")))]
    );
    assert_eq!(tree.steps.len(), 1, "a table of values was read as a task");
}

/// A name is where it is declared, so two declarations meet only where a file is named after a key of the top: the top's value `x` and the file `x`'s group are both `.x`.
#[test]
fn a_name_declared_twice_is_refused() {
    let refusal = Tree::parse(&[
        (ROOT_FILE, "what = \"The top\"\nx = \"held\"\n"),
        (
            &format!("{DIRECTORY}/x.toml"),
            "what = \"V\"\n[x]\nwhat = \"X\"\nrun = [[\"true\"]]\n",
        ),
    ])
    .unwrap_err();
    assert!(
        refusal.headline.contains("`.x` is declared twice"),
        "{}",
        refusal.rendered(false)
    );
}

#[test]
fn a_name_that_names_nothing_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[s]\nwhat = \"S\"\nrequires = [\".nothing\"]\n"
        )])
        .contains("`.nothing` names nothing")
    );
}

/// A variable's word names nothing of the tree, only what the runner fills, so no variable reads another and a variable is a constant whatever step reads it.
#[test]
fn a_variables_word_naming_a_value_of_the_tree_is_refused() {
    let refused = refused(&[("a", "what = \"A\"\nbase = \"x\"\nboth = \"{.a.base}-y\"\n")]);

    assert!(
        refused.contains("where a variable's words hold only what the runner fills"),
        "{refused}"
    );
}

#[test]
fn a_file_in_requires_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[build]\nwhat = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\nwhat = \"T\"\nrequires = [\".a.build.produces.binary\"]\n"
        )])
        .contains("belongs in `consumes`")
    );
}

#[test]
fn a_step_in_consumes_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[build]\nwhat = \"B\"\nrun = [[\"true\"]]\n[test]\nwhat = \"T\"\nconsumes = [\".a.build\"]\nrun = [[\"true\"]]\n"
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
            "what = \"A\"\n[build]\nwhat = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\nwhat = \"T\"\nrun = [[\"true\", \"{.a.build.produces.binary}\"]]\n"
        )])
        .contains("neither consumes nor produces")
    );
}

#[test]
fn a_step_or_group_in_braces_is_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[build]\nwhat = \"B\"\nrun = [[\"true\", \"{.a.build}\"]]\n"
        )])
        .contains("has no value")
    );
}

#[test]
fn steps_that_run_after_each_other_are_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[x]\nwhat = \"X\"\nrequires = [\".a.y\"]\nrun = [[\"true\"]]\n[y]\nwhat = \"Y\"\nrequires = [\".a.x\"]\nrun = [[\"true\"]]\n"
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
            "what = \"A\"\nlist = [\"x\", \"y\"]\n[s]\nwhat = \"S\"\nrun = [[\"true\", \"--{.a.list}\"]]\n"
        )])
        .contains("a word of its own")
    );
}

/// A value the runner fills is written after a `!`, apart from every name a file declares, and one it does not fill is refused with those it does.
#[test]
fn a_value_the_runner_does_not_fill_is_refused_naming_those_it_does() {
    let refusal = refused(&[(
        "a",
        "what = \"A\"\n[s]\nwhat = \"S\"\nrun = [[\"true\", \"{!host}\"]]\n",
    )]);
    assert!(
        refusal.contains("`!host` is no value the runner fills")
            && refusal
                .contains("`!host_triple`, `!cargo_target_dir`, `!cargo_home`, `!rust_sysroot`"),
        "{refusal}"
    );
}
