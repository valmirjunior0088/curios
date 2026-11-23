//! What a command's words launch.

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

    (
        launch.command.get_program().to_string_lossy().into_owned(),
        launch.arguments,
        environment,
    )
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
        words("nextest run --no-tests=warn")
    );
}
