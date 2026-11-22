//! What a command's words launch, and what a refusal says of a program that is not installed.

use {super::*, crate::*};

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

/// Nextest is launched through its own executable, so an absent one fails to spawn as every other program does.
#[test]
fn nextest_is_launched_through_its_own_executable() {
    let launch = launch(&words("cargo nextest run")).unwrap();

    assert_eq!(launch.program, "nextest");
    assert_eq!(launch.command.get_program(), "cargo-nextest");
    assert_eq!(launch.command.get_args().collect::<Vec<_>>(), ["nextest"]);
    assert_eq!(launch.arguments, words("run"));
}

/// A program that is there and cannot say its version is refused with what happened, not as missing: here a file that is no executable.
#[test]
fn a_program_that_cannot_say_its_version_is_refused_with_what_happened() {
    let unrunnable = root().join("xtask/Cargo.toml");
    let refusal =
        check_version(&unrunnable.to_string_lossy(), &Pin::Exact("1".to_string())).unwrap_err();

    assert!(
        refusal.contains("cannot run") && !refusal.contains("not installed"),
        "{refusal}"
    );
}

/// A missing program the task files pin is refused with the version they need of it, exact or a minimum, and one they do not pin is named alone.
#[test]
fn a_missing_program_is_refused_with_the_version_its_pin_needs() {
    assert_eq!(
        missing("nextest", Some(&Pin::Exact("0.9.149".to_string()))),
        "nextest is needed, and it is not installed: the task files pin 0.9.149."
    );
    assert_eq!(
        missing("node", Some(&Pin::Minimum("22".to_string()))),
        "node is needed, and it is not installed: the task files need 22 or later."
    );
    assert_eq!(
        missing("git", None),
        "git is needed, and it is not installed."
    );
}

#[test]
fn a_version_is_read_past_the_words_before_it() {
    assert_eq!(version_in("Version: 2.6.2"), Some(vec![2, 6, 2]));
    assert_eq!(version_in("v22.12.0\n"), Some(vec![22, 12, 0]));
    assert_eq!(
        version_in("cargo-nextest 0.9.149 (abc1234 2026-10-01)"),
        Some(vec![0, 9, 149])
    );
    assert_eq!(version_in("wasm-bindgen 0.2.129."), Some(vec![0, 2, 129]));
}

#[test]
fn a_program_that_prints_no_number_has_no_version() {
    assert_eq!(version_in("unknown"), None);
}

#[test]
fn an_exact_pin_is_met_by_its_version_alone() {
    let pin = Pin::Exact("2.6.2".to_string());

    assert!(meets(&[2, 6, 2], &pin));
    assert!(!meets(&[2, 6, 3], &pin));
    assert!(!meets(&[2, 6, 1], &pin));
}

/// A minimum compares number by number, not as text: `0.9.1000` is later than `0.9.149`, and a pin of `22` is met by any `22.x`.
#[test]
fn a_minimum_is_met_by_its_version_and_every_later_one() {
    let pin = Pin::Minimum("0.9.149".to_string());

    assert!(meets(&[0, 9, 149], &pin));
    assert!(meets(&[0, 9, 1000], &pin));
    assert!(meets(&[0, 10, 0], &pin));
    assert!(!meets(&[0, 9, 99], &pin));
    assert!(meets(&[22, 12, 0], &Pin::Minimum("22".to_string())));
    assert!(!meets(&[21, 9, 9], &Pin::Minimum("22".to_string())));
}
