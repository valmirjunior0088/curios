//! What a question gets back, and what either kind of spawn does with a command that fails or is not there.
//!
//! The subjects are `echo` and `false`, which every platform this crate builds for has, so what is under test is this module rather than a tool.

use {super::*, crate::*};

#[test]
fn asking_a_command_returns_what_it_printed() {
    assert_eq!(ask(Command::new("echo"), &["hello"]), Ok("hello\n".into()));
}

#[test]
fn a_command_that_fails_is_an_error_naming_it_however_it_was_spawned() {
    let asked = ask(Command::new("false"), &[]).unwrap_err();
    assert!(asked.starts_with("false exited with"), "{asked}");

    let launched = launch_in(root(), launch(&["false".to_string()]).unwrap(), "false").unwrap_err();
    assert!(launched.starts_with("false exited with"), "{launched}");
}

#[test]
fn a_program_that_is_not_there_is_an_error_naming_it() {
    let missing = ask(Command::new("curios-xtask-no-such-program"), &[]).unwrap_err();

    assert!(
        missing.starts_with("cannot run curios-xtask-no-such-program:"),
        "{missing}"
    );
}
