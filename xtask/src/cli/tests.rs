//! What the command line a tree reads as accepts, over task files of the tests' own.

use {super::*, crate::tree::test_support::*, crate::*};

/// A subject holding a group, a step beside it, and a flag one step of the group takes.
const SUBJECT: (&str, &str) = (
    "a",
    r#"
description = "A"
variables.expression.flag = ["-E", "--filterset"]
variables.expression.help = "Run only what the expression selects"

[lint.clippy]
description = "Clippy"
run = [["true"]]

[test.suite]
description = "Suite"
run = [["true", "{.a.variables.expression}"]]

[build]
description = "Build"
run = [["true"]]
"#,
);

fn parsed(words: &[&str]) -> Result<Asked, clap::Error> {
    let tree = tree(&[SUBJECT]).unwrap();

    command(&tree)
        .try_get_matches_from(["cargo xtask"].into_iter().chain(words.iter().copied()))
        .map(|matches| asked(&matches))
}

#[test]
fn a_path_names_a_task_at_any_depth() {
    for path in ["a", "a lint", "a lint clippy", "a build"] {
        assert_eq!(
            parsed(&path.split(' ').collect::<Vec<_>>()).unwrap(),
            Asked::Task {
                name: path.split(' ').map(str::to_string).collect(),
                given: Given::new()
            }
        );
    }
}

/// An action a subject lacks is the parser's own error rather than a refusal written here.
#[test]
fn a_path_the_tree_does_not_hold_is_refused() {
    assert!(parsed(&["a", "bundle"]).is_err());
    assert!(parsed(&["b"]).is_err());
}

#[test]
fn a_flag_is_taken_where_a_step_under_the_path_uses_it() {
    let expected = Asked::Task {
        name: vec!["a".to_string(), "test".to_string()],
        given: [(name("a.variables.expression"), "test(~cli)".to_string())]
            .into_iter()
            .collect(),
    };

    assert_eq!(
        parsed(&["a", "test", "-E", "test(~cli)"]).unwrap(),
        expected
    );
    assert_eq!(
        parsed(&["a", "test", "--filterset", "test(~cli)"]).unwrap(),
        expected
    );
    assert!(parsed(&["a", "lint", "-E", "test(~cli)"]).is_err());
}

#[test]
fn the_commands_outside_the_tree_take_their_own_arguments() {
    assert_eq!(
        parsed(&["installer", "1.2.3"]).unwrap(),
        Asked::Installer {
            version: "1.2.3".to_string()
        }
    );
    assert_eq!(parsed(&["release"]).unwrap(), Asked::Release { bump: None });
}
