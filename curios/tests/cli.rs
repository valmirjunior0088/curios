//! The command line before any command runs: a line that does not parse fails as a command that could not run fails, and help and the version are answers.

use std::process::{Command, Output};

fn curios(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .args(args)
        .output()
        .unwrap()
}

/// A line that does not parse exits 1, as a command that could not run does, with its refusal as a report on standard error: 2 is the tri-state's own, saying written goals stopped a program.
#[test]
fn a_line_that_does_not_parse_exits_one() {
    let refused = curios(&["--no-such-option"]);

    assert_eq!(refused.status.code(), Some(1), "{refused:?}");
    assert!(
        String::from_utf8_lossy(&refused.stderr)
            .starts_with("unknown flag\n│  `--no-such-option` is no flag of `curios`\n"),
        "{refused:?}"
    );
    assert!(refused.stdout.is_empty(), "{refused:?}");
}

/// Help and the version are answers: printed to standard output, exiting 0.
#[test]
fn help_and_the_version_exit_zero() {
    for asked in ["--help", "--version"] {
        let answered = curios(&[asked]);

        assert_eq!(answered.status.code(), Some(0), "{asked}: {answered:?}");
        assert!(!answered.stdout.is_empty(), "{asked} printed nothing");
    }
}
