//! What a command's words launch, what version of a program a run needs, and what a refusal says where a program is not installed or not the version the task files pin.
//!
//! **A command is the words a person would type.** Leading `NAME=value` words set the environment, as a shell reads them, and the next word names the program. `cargo` is the cargo that launched this run: `cargo xtask` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a step cannot resolve a second time and land somewhere else. `cargo nextest` is launched as nextest's own program, `cargo-nextest nextest`, as cargo itself launches it, so an absent one fails to spawn as every other program does.
//!
//! **A program is asked about where the files pin it, and nowhere else.** The task files' `programs` say what version a run needs of each program that is no part of the Rust toolchain, exact where output moves between releases and a minimum where it does not, and what installs it. The first time a step launches a pinned program in a run, [`check_version`] reads what it prints for `--version`; a program no step launches is never asked, so what a step needs is what it runs as it was asked. Where a program is not there, the launch is refused with what installs it.

use {
    crate::{Pin, Program, ask},
    std::{env, process::Command},
};

#[cfg(test)]
mod tests;

/// What a command's words launch.
pub(crate) struct Launch {
    /// The program as a refusal names it.
    pub(crate) program: String,
    pub(crate) command: Command,
    pub(crate) arguments: Vec<String>,
}

/// Whether `word` sets the environment, as a leading `NAME=value` word does.
fn is_assignment(word: &str) -> bool {
    word.split_once('=').is_some_and(|(name, _)| {
        !name.is_empty()
            && name.chars().all(|character| {
                character.is_ascii_uppercase() || character.is_ascii_digit() || character == '_'
            })
    })
}

/// What `words` launch: the environment their leading assignments set, and the program the first other word names, with the rest its arguments.
pub(crate) fn launch(words: &[String]) -> Result<Launch, String> {
    let assignments = words.iter().take_while(|word| is_assignment(word)).count();
    let (environment, words) = words.split_at(assignments);
    let (program, arguments) = words
        .split_first()
        .ok_or_else(|| "a command sets its environment and names no program".to_string())?;

    let (program, arguments) = match (program.as_str(), arguments) {
        ("cargo", [nextest, rest @ ..]) if nextest == "nextest" => ("cargo-nextest", rest),
        (program, rest) => (program, rest),
    };
    let mut command = command_of(program);
    let arguments = arguments.to_vec();
    for assignment in environment {
        let (name, value) = assignment
            .split_once('=')
            .expect("an assignment holds an equals sign");
        command.env(name, value);
    }

    Ok(Launch {
        program: program.to_string(),
        command,
        arguments,
    })
}

/// The command that runs `program` as a step launches it, before the step's own arguments: nextest's is `cargo-nextest nextest`, and cargo's the one that launched this run.
fn command_of(program: &str) -> Command {
    match program {
        "cargo-nextest" => {
            let mut command = Command::new("cargo-nextest");
            command.arg("nextest");
            command
        }
        "cargo" => Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
        program => Command::new(program),
    }
}

/// The first version a program's `--version` prints, as its numbers: `Version: 2.6.2` and `v22.12.0` alike.
pub(crate) fn version_in(printed: &str) -> Option<Vec<u64>> {
    let start = printed.find(|character: char| character.is_ascii_digit())?;
    let written = printed[start..]
        .split(|character: char| !character.is_ascii_digit() && character != '.')
        .next()?
        .trim_end_matches('.');

    written.split('.').map(|part| part.parse().ok()).collect()
}

/// Whether `installed` meets `pin`: the same numbers for an exact pin, the same or later for a minimum, a missing number read as zero.
pub(crate) fn meets(installed: &[u64], pin: &Pin) -> bool {
    let Some(wanted) = version_in(pin.version()) else {
        return false;
    };
    let length = installed.len().max(wanted.len());
    let padded = |numbers: &[u64]| {
        let mut numbers = numbers.to_vec();
        numbers.resize(length, 0);
        numbers
    };

    match pin {
        Pin::Exact(_) => padded(installed) == padded(&wanted),
        Pin::Minimum(_) => padded(installed) >= padded(&wanted),
    }
}

/// Refuse `program` where `declared` pins it and the version it prints does not meet the pin.
pub(crate) fn check_version(program: &str, declared: &Program) -> Result<(), String> {
    let Some(pin) = &declared.pin else {
        return Ok(());
    };
    let advice = |said: String| match &declared.install {
        Some(install) => format!("{said} Install it with:\n\n    {install}"),
        None => said,
    };

    let printed = ask(command_of(program), &["--version"])
        .map_err(|_| missing(program, declared.install.as_deref()))?;
    let installed = version_in(&printed)
        .ok_or_else(|| format!("{program} printed no version for `--version`: {printed}"))?;
    if meets(&installed, pin) {
        return Ok(());
    }

    let installed = installed
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(".");
    Err(advice(match pin {
        Pin::Exact(version) => {
            format!("{program} {installed} is installed, and the task files pin {version}.")
        }
        Pin::Minimum(version) => format!(
            "{program} {installed} is installed, and the task files need {version} or later."
        ),
    }))
}

/// That a step cannot run without `program`, and, where the task files say what installs it, that command — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
pub(crate) fn missing(program: &str, install: Option<&str>) -> String {
    match install {
        Some(install) => {
            format!(
                "{program} is needed, and it is not installed. Install it with:\n\n    {install}"
            )
        }
        None => format!("{program} is needed, and it is not installed."),
    }
}
