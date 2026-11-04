//! What the command line the repository's task files read as accepts.

use {super::*, crate::*};

fn parsed(words: &[&str]) -> Result<Asked, clap::Error> {
    let tree = Tree::read().unwrap();

    command(&tree)
        .try_get_matches_from(["cargo xtask"].into_iter().chain(words.iter().copied()))
        .map(|matches| asked(&matches))
}

fn name(text: &str) -> Name {
    text.split(' ').map(str::to_string).collect()
}

#[test]
fn a_path_names_a_task_at_any_depth() {
    for path in [
        "workspace",
        "workspace lint",
        "workspace lint clippy",
        "gate",
    ] {
        assert_eq!(
            parsed(&path.split(' ').collect::<Vec<_>>()).unwrap(),
            Asked::Task {
                name: name(path),
                given: Given::new()
            }
        );
    }
}

/// An action a subject lacks is the parser's own error rather than a refusal written here.
#[test]
fn a_path_the_tree_does_not_hold_is_refused() {
    assert!(parsed(&["bundle", "lint"]).is_err());
}

#[test]
fn a_flag_is_taken_where_a_step_under_the_path_uses_it() {
    let expected = Asked::Task {
        name: name("workspace test"),
        given: [(name("workspace expression"), "test(~cli)".to_string())]
            .into_iter()
            .map(|(name, value)| {
                (
                    name.join(".").split('.').map(str::to_string).collect(),
                    value,
                )
            })
            .collect(),
    };

    assert_eq!(
        parsed(&["workspace", "test", "-E", "test(~cli)"]).unwrap(),
        expected
    );
    assert!(parsed(&["workspace", "lint", "-E", "test(~cli)"]).is_err());
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
