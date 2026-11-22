//! Names and values: how a name resolves, what a word may hold, and the order what a step reads puts it in.

use super::{test_support::*, *};

/// A name is the path from the top to where it is declared, a key at a time, so it is spelled alike in its own file and in another, and a variable sits under its file's `variables` and a produced file under its step's `produces`.
#[test]
fn a_name_is_the_path_from_the_top_to_its_declaration_in_every_file() {
    let tree = tree(&[
        (
            "a",
            "description = \"A\"\nvariables.flavor = \"plain\"\n[build]\ndescription = \"Build\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"Test\"\nconsumes = [\".a.build.produces.binary\"]\nrun = [[\"true\", \"{.a.build.produces.binary}\", \"{.a.variables.flavor}\"]]\n",
        ),
        (
            "b",
            "description = \"B\"\n[test]\ndescription = \"Test\"\nconsumes = [\".a.build.produces.binary\"]\nrun = [[\"true\", \"{.a.build.produces.binary}\", \"{.a.variables.flavor}\"]]\n",
        ),
    ])
    .unwrap();

    for step in ["a.test", "b.test"] {
        let step = tree.steps_of(&name(step))[0];
        assert_eq!(tree.steps[step].consumes, [name("a.build.produces.binary")]);
        assert_eq!(tree.prerequisites(step), tree.steps_of(&name("a.build")));
        let Instruction::Command(words) = &tree.steps[step].run[0] else {
            panic!("the step runs a command");
        };
        assert_eq!(
            words[2].0,
            [Piece::Value(Fill::Variable(name("a.variables.flavor")))]
        );
    }
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
                "description = \"A\"\nvariables.flavor = \"plain\"\n[build]\ndescription = \"B\"\nrun = [[\"true\"]]\n[s]\ndescription = \"S\"\nrequires = {requires}\nrun = {run}\n"
            ),
        )]);
        assert!(
            refusal.contains("is no path: a name is written from the top with a leading `.`"),
            "{refusal}"
        );
    }
}

/// A name is where it is declared, so two declarations meet only where a file is named after a key of the top: the top's variable `x` and the step `x` of the file `variables` are both `.variables.x`.
#[test]
fn a_name_declared_twice_is_refused() {
    let refusal = Tree::parse(&[
        (
            ROOT_FILE,
            "description = \"The top\"\nvariables.x = \"held\"\n",
        ),
        (
            &format!("{DIRECTORY}/variables.toml"),
            "description = \"V\"\n[x]\ndescription = \"X\"\nrun = [[\"true\"]]\n",
        ),
    ])
    .unwrap_err();
    assert!(
        refusal
            .headline
            .contains("`.variables.x` is declared twice"),
        "{}",
        refusal.render()
    );
}

#[test]
fn a_name_that_names_nothing_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[s]\ndescription = \"S\"\nrequires = [\".nothing\"]\n"
        )])
        .contains("`.nothing` names nothing")
    );
}

#[test]
fn a_file_in_requires_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nrequires = [\".a.build.produces.binary\"]\n"
        )])
        .contains("belongs in `consumes`")
    );
}

#[test]
fn a_step_in_consumes_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nconsumes = [\".a.build\"]\nrun = [[\"true\"]]\n"
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
            "description = \"A\"\n[build]\ndescription = \"B\"\nproduces = { binary = \"out\" }\nrun = [[\"true\"]]\n[test]\ndescription = \"T\"\nrun = [[\"true\", \"{.a.build.produces.binary}\"]]\n"
        )])
        .contains("neither consumes nor produces")
    );
}

#[test]
fn a_step_or_group_in_braces_is_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[build]\ndescription = \"B\"\nrun = [[\"true\", \"{.a.build}\"]]\n"
        )])
        .contains("has no value")
    );
}

#[test]
fn steps_that_run_after_each_other_are_refused() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\n[x]\ndescription = \"X\"\nrequires = [\".a.y\"]\nrun = [[\"true\"]]\n[y]\ndescription = \"Y\"\nrequires = [\".a.x\"]\nrun = [[\"true\"]]\n"
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
            "description = \"A\"\nvariables.list = [\"x\", \"y\"]\n[s]\ndescription = \"S\"\nrun = [[\"true\", \"--{.a.variables.list}\"]]\n"
        )])
        .contains("a word of its own")
    );
}

/// A value the runner fills is written after a `!`, apart from every name a file declares, and one it does not fill is refused with those it does.
#[test]
fn a_value_the_runner_does_not_fill_is_refused_naming_those_it_does() {
    let refusal = refused(&[(
        "a",
        "description = \"A\"\n[s]\ndescription = \"S\"\nrun = [[\"true\", \"{!host}\"]]\n",
    )]);
    assert!(
        refusal.contains("`!host` is no value the runner fills")
            && refusal
                .contains("`!host_triple`, `!cargo_target_dir`, `!cargo_home`, `!rust_sysroot`"),
        "{refusal}"
    );
}
