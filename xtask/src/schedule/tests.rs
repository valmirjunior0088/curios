//! What a run takes, and in what order, over task files of each test's own.

use {super::*, crate::tree::test_support::*, crate::*};

/// A subject `l` whose one step produces a file, which the trees below consume.
const PRODUCER: (&str, &str) = (
    "l",
    r#"
description = "L"

[build]
description = "Build"
produces.binary = "out/binary"
run = [["true"]]
"#,
);

/// The steps a run of `asked` over `tree` performs, by their command-line spelling, in order.
fn scheduled(tree: &Tree, asked: &str, given: &[(&str, &str)]) -> Vec<String> {
    let given = given
        .iter()
        .map(|(given, value)| (name(given), vec![value.to_string()]))
        .collect::<GivenValues>();
    let asked = asked.split(' ').map(str::to_string).collect::<Name>();

    schedule(tree, &asked, &given)
        .into_iter()
        .map(|step| spelled(&tree.steps[step].name))
        .collect()
}

#[test]
fn a_subject_brings_no_step_of_another_but_the_producers_of_what_it_consumes() {
    let tree = tree(&[
        PRODUCER,
        (
            "c",
            r#"
description = "C"

[build]
description = "Build"
consumes = [".l.build.produces.binary"]
produces.binary = "out/c"
run = [["true"]]

[test]
description = "Test"
consumes = [".c.build.produces.binary"]
run = [["true"]]
"#,
        ),
        (
            "o",
            r#"
description = "O"

[test]
description = "Test"
run = [["true"]]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(scheduled(&tree, "c", &[]), ["l build", "c build", "c test"]);
}

#[test]
fn a_group_s_steps_run_in_its_file_s_order_each_after_the_producer_of_what_it_consumes() {
    let tree = tree(&[
        PRODUCER,
        (
            "w",
            r#"
description = "W"

[lint]
description = "Lint"

[lint.first]
description = "First"
run = [["true"]]

[lint.second]
description = "Second"
consumes = [".l.build.produces.binary"]
run = [["true"]]

[lint.third]
description = "Third"
run = [["true"]]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(
        scheduled(&tree, "w lint", &[]),
        ["w lint first", "l build", "w lint second", "w lint third"]
    );
}

#[test]
fn a_step_that_neither_requires_nor_consumes_runs_alone() {
    let tree = tree(&[
        PRODUCER,
        (
            "n",
            r#"
description = "N"

[lint]
description = "Lint"
run = [["true"]]

[test]
description = "Test"
run = [["true"]]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(scheduled(&tree, "n", &[]), ["n lint", "n test"]);
}

#[test]
fn a_step_runs_after_the_steps_it_requires() {
    let tree = tree(&[(
        "t",
        r#"
description = "T"

[test]
description = "Test"
requires = [".t.install"]
run = [["true"]]

[install]
description = "Install"
run = [["true"]]
"#,
    )])
    .unwrap();

    assert_eq!(scheduled(&tree, "t test", &[]), ["t install", "t test"]);
}

#[test]
fn a_step_runs_after_the_producer_of_each_file_it_consumes_in_the_order_it_names_them() {
    let tree = tree(&[
        (
            "g",
            r#"
description = "G"

[build]
description = "Build"

[build.base]
description = "Base"
produces.parser = "out/base"
run = [["true"]]

[build.core]
description = "Core"
produces.parser = "out/core"
run = [["true"]]
"#,
        ),
        (
            "z",
            r#"
description = "Z"

[check]
description = "Check"
consumes = [".g.build.core.produces.parser", ".g.build.base.produces.parser"]
run = [["true"]]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(
        scheduled(&tree, "z check", &[]),
        ["g build core", "g build base", "z check"]
    );
}

/// A variable given narrows what was asked to the steps whose commands use it, and leaves the rest of the subject, which asks other questions.
#[test]
fn a_variable_given_runs_only_the_steps_that_use_it() {
    let tree = tree(&[
        PRODUCER,
        (
            "w",
            r#"
description = "W"
given.expression.flag = ["-E", "--filterset"]

[lint]
description = "Lint"
consumes = [".l.build.produces.binary"]
run = [["true"]]

[test]
description = "Test"
consumes = [".l.build.produces.binary"]
run = [["true", "{.w.given.expression}"]]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(
        scheduled(&tree, "w", &[("w.given.expression", "test(~cli)")]),
        ["l build", "w test"]
    );
    assert_eq!(scheduled(&tree, "w", &[]), ["l build", "w lint", "w test"]);
}

#[test]
fn a_step_that_requires_subjects_runs_every_step_of_each_in_the_order_it_names_them_and_itself_last()
 {
    let tree = tree(&[
        (
            "a",
            r#"
description = "A"

[lint]
description = "Lint"
run = [["true"]]

[test]
description = "Test"
run = [["true"]]
"#,
        ),
        (
            "b",
            r#"
description = "B"

[test]
description = "Test"
run = [["true"]]
"#,
        ),
        (
            "h",
            r#"
description = "H"

[all]
description = "All"
requires = [".b", ".a"]
"#,
        ),
    ])
    .unwrap();

    assert_eq!(
        scheduled(&tree, "h all", &[]),
        ["b test", "a lint", "a test", "h all"]
    );
}
