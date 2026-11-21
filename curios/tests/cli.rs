//! The command line before any command runs: a line that does not parse fails as a command that could not run fails, help and the version are answers, and the drawing a command narrates in is chosen.

use {
    curios::CABINET,
    std::{
        io::Write,
        process::{Command, Output, Stdio},
    },
};

fn curios(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
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

/// The environment variable naming the drawing where the command line does not.
const PROGRESS: &str = "CURIOS_PROGRESS";

/// What `run -` of a program that prints nothing says on standard error in a pipe, given `args` and, where it is `Some`, the environment's drawing `named`.
fn narrated(args: &[&str], named: Option<&str>) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_curios"));
    command.env_remove(CABINET).env_remove(PROGRESS);
    if let Some(named) = named {
        command.env(PROGRESS, named);
    }
    let mut child = command
        .args(args)
        .args(["run", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"/std/print(\"\")\n")
        .unwrap();

    child.wait_with_output().expect("the compiler exits")
}

/// A pipe is drawn plain, a line as each step begins and as it ends, unless `--progress` asks for the tree or `CURIOS_PROGRESS` names it; the flag stands over the variable, and a name that spells no drawing is refused as a flag's value is.
#[test]
fn a_pipe_is_drawn_plain_unless_the_flag_or_the_variable_asks_for_the_tree() {
    let plain = "Checking `<stdin>`\nChecked `<stdin>` in ";
    let tree = "<stdin>\n├─ check; ";
    let drawn = |args: &[&str], named: Option<&str>| {
        let output = narrated(args, named);
        assert!(output.status.success(), "{output:?}");

        String::from_utf8(output.stderr).unwrap()
    };

    assert!(drawn(&[], None).starts_with(plain));
    assert!(drawn(&["--progress", "live"], None).starts_with(tree));
    assert!(drawn(&[], Some("live")).starts_with(tree));
    assert!(drawn(&[], Some("plain")).starts_with(plain));
    assert!(drawn(&["--progress", "plain"], Some("live")).starts_with(plain));

    let refused = narrated(&[], Some("tree"));
    assert_eq!(refused.status.code(), Some(1), "{refused:?}");
    assert!(
        String::from_utf8_lossy(&refused.stderr).starts_with(
            "invalid value\n│  written: `tree`\n│  `CURIOS_PROGRESS` takes one of: `plain`, `live`\n"
        ),
        "{refused:?}"
    );
}
