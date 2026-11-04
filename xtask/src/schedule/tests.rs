//! What a run of the repository's own task files takes, and in what order.

use {super::*, crate::*};

/// The steps a run of `asked` performs, by their command-line spelling, in order.
fn scheduled(asked: &str, given: &[(&str, &str)]) -> Vec<String> {
    let tree = Tree::read().unwrap();
    let given = given
        .iter()
        .map(|(name, value)| {
            (
                name.split('.').map(str::to_string).collect(),
                value.to_string(),
            )
        })
        .collect::<Given>();
    let asked = asked.split(' ').map(str::to_string).collect::<Name>();

    schedule(&tree, &asked, &given)
        .into_iter()
        .map(|step| spelled(&tree.steps[step].name))
        .collect()
}

#[test]
fn a_subject_brings_no_step_of_another_but_what_it_consumes() {
    assert_eq!(
        scheduled("compiler", &[]),
        ["launcher build", "compiler build", "compiler test"]
    );
}

#[test]
fn a_lint_of_the_workspace_builds_the_launcher_first() {
    assert_eq!(
        scheduled("workspace lint", &[]),
        [
            "workspace lint rustfmt",
            "workspace lint biome-format",
            "launcher build",
            "workspace lint clippy",
            "workspace lint biome-lint"
        ]
    );
}

/// The native host's checks build no `curios`, so they consume no launcher and run without one being built.
#[test]
fn the_native_host_s_checks_build_no_launcher() {
    assert_eq!(scheduled("native", &[]), ["native lint", "native test"]);
}

#[test]
fn an_npm_script_runs_after_its_tree_s_install() {
    assert_eq!(
        scheduled("vscode test", &[]),
        ["vscode install", "vscode test"]
    );
}

/// `zed test` hashes the parser `grammar build` generates, which the grammar's own tools generate.
#[test]
fn the_extension_s_test_runs_after_the_grammar_it_hashes() {
    assert_eq!(
        scheduled("zed test", &[]),
        ["grammar install", "grammar build", "zed test"]
    );
}

/// A filter given to the tests picks the suite, whose run takes it, and leaves the Binaryen check, which asks about another crate's build.
#[test]
fn a_variable_given_runs_only_the_steps_that_use_it() {
    assert_eq!(
        scheduled("workspace test", &[("workspace.expression", "test(~cli)")]),
        ["launcher build", "workspace test suite"]
    );
    assert_eq!(
        scheduled("workspace test", &[]),
        [
            "launcher build",
            "workspace test suite",
            "workspace test binaryen-cache"
        ]
    );
}

#[test]
fn the_gate_runs_every_subject_s_steps_in_the_order_it_names_them() {
    let tree = Tree::read().unwrap();
    let gate = scheduled("gate", &[]);

    assert_eq!(gate.first().map(String::as_str), Some("launcher build"));
    assert_eq!(gate.last().map(String::as_str), Some("gate"));
    for subject in [
        "launcher",
        "workspace",
        "instrumented",
        "docs",
        "compiler",
        "bundle",
        "grammar",
        "vscode",
        "zed",
        "tools",
    ] {
        for step in tree.steps_of(&[subject.to_string()]) {
            assert!(gate.contains(&spelled(&tree.steps[step].name)), "{subject}");
        }
    }
    for subject in ["format", "native", "profile"] {
        assert!(
            !gate
                .iter()
                .any(|step| step.starts_with(&format!("{subject} "))),
            "{subject}"
        );
    }
}
