//! Names and values: how a name resolves, what a word may hold, and the order what a step reads puts it in.

use super::test_support::*;

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
fn a_value_the_runner_fills_cannot_be_declared() {
    assert!(
        refused(&[(
            "a",
            "description = \"A\"\nvariables.host = \"x\"\n[s]\ndescription = \"S\"\nrun = [[\"true\"]]\n"
        )])
        .contains("the runner fills")
    );
}
