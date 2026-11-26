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

/// A command's rows open and close on its written form, each with one guillemet's direction, and a step's row is drawn apart from both: what a reader scrolling either way and the handoff's monitor find the edges of work by.
#[test]
fn a_commands_rows_face_its_output_and_a_steps_row_stands_apart() {
    let took = std::time::Duration::from_millis(440);

    assert_eq!(opener("cargo fmt --all"), "««« `cargo fmt --all`");
    assert_eq!(
        closer("cargo fmt --all", &ending(true, took)),
        "»»» `cargo fmt --all`: ✓ 0.4s"
    );
    assert_eq!(
        concluded("compiler lint", &ending(false, took)),
        "=== `compiler lint`: ✗ 0.4s"
    );
}
