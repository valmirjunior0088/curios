//! What the command line a tree reads as accepts, over task files of the tests' own.

use {super::*, crate::tree::test_support::*, crate::*};

/// A subject holding a group, a step beside it, and a flag one step of the group takes.
const SUBJECT: (&str, &str) = (
    "a",
    r#"
what = "A"
expression.flag = ["-E", "--filterset"]
expression.help = "Run only what the expression selects"

[lint]
what = "Lint"

[lint.clippy]
what = "Clippy"
run = [["true"]]

[test]
what = "Test"

[test.suite]
what = "Suite"
run = [["true", "{.a.expression}"]]

[build]
what = "Build"
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
            Asked {
                name: path.split(' ').map(str::to_string).collect(),
                given: GivenValues::new()
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
    let expected = Asked {
        name: vec!["a".to_string(), "test".to_string()],
        given: [(name("a.expression"), vec!["test(~cli)".to_string()])]
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

/// A flag given more than once is gathered each time, in the order given, under either spelling, so a run hands every value on.
#[test]
fn a_flag_given_twice_is_gathered_twice() {
    assert_eq!(
        parsed(&["a", "test", "-E", "test(~a)", "--filterset", "test(~b)"]).unwrap(),
        Asked {
            name: vec!["a".to_string(), "test".to_string()],
            given: [(
                name("a.expression"),
                vec!["test(~a)".to_string(), "test(~b)".to_string()]
            )]
            .into_iter()
            .collect(),
        }
    );
}

/// An argument that states no `else` is required: the command line refuses it missing, with its usage, before anything runs. Mutation-checked: with every argument optional, a run that leaves it out is taken.
#[test]
fn an_argument_stating_no_else_is_required() {
    let tree = tree(&[(
        "a",
        "what = \"A\"\nneeded.argument = \"NEEDED\"\n[s]\nwhat = \"S\"\nrun = [[\"true\", \"{.a.needed}\"]]\n",
    )])
    .unwrap();
    let parsed = |words: &[&str]| {
        command(&tree)
            .try_get_matches_from(["cargo xtask"].into_iter().chain(words.iter().copied()))
            .map(|matches| asked(&matches))
    };

    assert!(
        parsed(&["a", "s"]).is_err(),
        "a required argument left out was taken"
    );
    assert_eq!(
        parsed(&["a", "s", "given"]).unwrap().given,
        GivenValues::from([(
            vec!["a".to_string(), "needed".to_string()],
            vec!["given".to_string()]
        )])
    );
}
