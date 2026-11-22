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
        scheduled("textmate test", &[]),
        ["textmate install", "textmate test"]
    );
}

/// `zed test` hashes the parser `tree-sitter build` generates, which the grammar's own tools generate.
#[test]
fn the_extension_s_test_runs_after_the_grammar_it_hashes() {
    assert_eq!(
        scheduled("zed test", &[]),
        ["tree-sitter install", "tree-sitter build", "zed test"]
    );
}

/// A filter given to the workspace picks the suite, whose run takes it, and leaves every other step, the lint and the cache checks, which ask about other crates' builds.
#[test]
fn a_variable_given_runs_only_the_steps_that_use_it() {
    assert_eq!(
        scheduled(
            "workspace",
            &[("workspace.variables.expression", "test(~cli)")]
        ),
        ["launcher build", "workspace test suite"]
    );
    let whole = scheduled("workspace", &[]);
    for step in ["workspace lint clippy", "workspace caches prelude"] {
        assert!(whole.iter().any(|run| run == step), "{whole:?}");
    }
}

#[test]
fn the_handoff_runs_every_subject_s_steps_in_the_order_it_names_them() {
    let tree = Tree::read().unwrap();
    let handoff = scheduled("handoff", &[]);

    assert_eq!(handoff.first().map(String::as_str), Some("launcher build"));
    assert_eq!(handoff.last().map(String::as_str), Some("handoff"));
    for subject in [
        "launcher",
        "workspace",
        "docs",
        "compiler",
        "bundle",
        "tree-sitter",
        "textmate",
        "vscode",
        "zed",
        "tools",
    ] {
        for step in tree.steps_of(&[subject.to_string()]) {
            assert!(
                handoff.contains(&spelled(&tree.steps[step].name)),
                "{subject}"
            );
        }
    }
    for subject in ["format", "native", "profile"] {
        assert!(
            !handoff
                .iter()
                .any(|step| step.starts_with(&format!("{subject} "))),
            "{subject}"
        );
    }
}
