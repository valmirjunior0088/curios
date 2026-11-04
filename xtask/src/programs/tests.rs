//! What a command's words launch, and what a refusal says of a program that is not installed.

use super::*;

fn words(text: &str) -> Vec<String> {
    text.split(' ').map(str::to_string).collect()
}

fn launched(text: &str) -> (String, Vec<String>, Vec<(String, String)>) {
    let launch = launch(&words(text)).unwrap();
    let environment = launch
        .command
        .get_envs()
        .map(|(name, value)| {
            (
                name.to_string_lossy().into_owned(),
                value.unwrap_or_default().to_string_lossy().into_owned(),
            )
        })
        .collect();

    (launch.program, launch.arguments, environment)
}

#[test]
fn leading_assignments_set_the_environment() {
    assert_eq!(
        launched("CURIOS=/bin/curios node --test page.test.mjs"),
        (
            "node".to_string(),
            words("--test page.test.mjs"),
            vec![("CURIOS".to_string(), "/bin/curios".to_string())]
        )
    );
}

/// An assignment is one only before the program: a word holding `=` after it is an argument.
#[test]
fn an_equals_sign_after_the_program_is_an_argument() {
    assert_eq!(
        launched("cargo nextest run --no-tests=warn").1,
        words("run --no-tests=warn")
    );
}

/// Nextest is launched as its own program, so an absent one fails to spawn as every other program does.
#[test]
fn cargo_nextest_is_launched_as_its_own_program() {
    let launch = launch(&words("cargo nextest run")).unwrap();

    assert_eq!(launch.program, "cargo-nextest");
    assert_eq!(launch.command.get_program(), "cargo-nextest");
    assert_eq!(launch.command.get_args().collect::<Vec<_>>(), ["nextest"]);
    assert_eq!(launch.arguments, words("run"));
}

#[test]
fn a_refusal_puts_the_fix_on_a_line_of_its_own() {
    assert_eq!(
        missing("cargo-nextest"),
        format!(
            "cargo-nextest is needed, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
        )
    );
}

#[test]
fn a_program_no_one_command_installs_is_named_alone() {
    assert_eq!(missing("node"), "node is needed, and it is not installed.");
}
