//! What a command's words launch, what version of a program a run needs, and what a refusal says where a program is not installed or not the version the task files pin.
//!
//! **A command is the words a person would type.** Leading `NAME=value` words set the environment, as a shell reads them, and the next word names the program. `cargo` is the cargo that launched this run: `cargo xtask` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a step cannot resolve a second time and land somewhere else. `cargo nextest` is launched as the program `nextest`, through its own executable, `cargo-nextest nextest`, as cargo itself launches it, so an absent one fails to spawn as every other program does.
//!
//! **A program is asked about where the files pin it, and nowhere else.** The task files' `programs` say what version a run needs of each program that is no part of the Rust toolchain, exact, or a minimum for one a contributor's system provides. Before a run's first step, [`check_version`] reads what each pinned program the scheduled steps launch prints for `--version`, the program read off the words as written by [`launched`]; a program no scheduled step launches is never asked, so what a run needs is what it runs as it was asked. Where a program is not there, it is refused with the version the files pin, and where it is there and fails, with what happened.

use {
    crate::{Piece, Pin, Word, ask_launched},
    std::{env, process::Command},
};

#[cfg(test)]
mod tests;

/// What a command's words launch.
pub struct Launch {
    /// The program as a refusal names it.
    pub program: String,
    pub command: Command,
    pub arguments: Vec<String>,
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

/// Where the words `written` part: how many lead as assignments, the program the next names, and where its arguments start — `cargo nextest` naming nextest, its arguments after both words.
fn parted<'a>(written: &[&'a str]) -> Option<(usize, &'a str, usize)> {
    let assignments = written
        .iter()
        .take_while(|word| is_assignment(word))
        .count();
    match &written[assignments..] {
        ["cargo", "nextest", ..] => Some((assignments, "nextest", assignments + 2)),
        [program, ..] => Some((assignments, program, assignments + 1)),
        [] => None,
    }
}

/// What `words` launch: the environment their leading assignments set, and the program the first other word names, with the rest its arguments.
pub fn launch(words: &[String]) -> Result<Launch, String> {
    let written = words.iter().map(String::as_str).collect::<Vec<_>>();
    let (assignments, program, start) = parted(&written)
        .ok_or_else(|| "a command sets its environment and names no program".to_string())?;

    let mut command = command_of(program);
    let arguments = words[start..].to_vec();
    for assignment in &words[..assignments] {
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

/// The program a command's `words` launch and the index of the word naming it, read before anything is filled, each value as its braces: the reader holds that word to text, so the name is the one [`launch`] reads off the filled words.
pub fn launched(words: &[Word]) -> Option<(usize, String)> {
    let written = words
        .iter()
        .map(|word| {
            word.0
                .iter()
                .map(|piece| match piece {
                    Piece::Text(text) => text.as_str(),
                    Piece::Value(_) => "{}",
                })
                .collect::<String>()
        })
        .collect::<Vec<_>>();
    let (assignments, program, _) =
        parted(&written.iter().map(String::as_str).collect::<Vec<_>>())?;

    Some((assignments, program.to_string()))
}

/// The command that runs `program` as a step launches it, before the step's own arguments: nextest's is its executable, `cargo-nextest nextest`, and cargo's the one that launched this run.
fn command_of(program: &str) -> Command {
    match program {
        "nextest" => {
            let mut command = Command::new("cargo-nextest");
            command.arg("nextest");
            command
        }
        "cargo" => Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
        program => Command::new(program),
    }
}

/// The first version a program's `--version` prints, as its numbers: `Version: 2.6.2` and `v22.12.0` alike.
pub fn version_in(printed: &str) -> Option<Vec<u64>> {
    let start = printed.find(|character: char| character.is_ascii_digit())?;
    let written = printed[start..]
        .split(|character: char| !character.is_ascii_digit() && character != '.')
        .next()?
        .trim_end_matches('.');

    written.split('.').map(|part| part.parse().ok()).collect()
}

/// Whether `installed` meets `pin`: the same numbers for an exact pin, the same or later for a minimum, a missing number read as zero.
pub fn meets(installed: &[u64], pin: &Pin) -> bool {
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

/// Refuse `program` where the version it prints does not meet `pin`: missing, or failing to say, as what happened says.
pub fn check_version(program: &str, pin: &Pin) -> Result<(), String> {
    let printed = ask_launched(
        launch(&[program.to_string(), "--version".to_string()])?,
        Some(pin),
    )?;
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
    Err(format!(
        "{program} {installed} is installed, and {}.",
        needed(pin)
    ))
}

/// What the task files need of a program, as a refusal says it.
fn needed(pin: &Pin) -> String {
    match pin {
        Pin::Exact(version) => format!("the task files pin {version}"),
        Pin::Minimum(version) => format!("the task files need {version} or later"),
    }
}

/// That a step cannot run without `program`, and, where the task files pin it, the version they need.
pub fn missing(program: &str, pin: Option<&Pin>) -> String {
    match pin {
        Some(pin) => format!(
            "{program} is needed, and it is not installed: {}.",
            needed(pin)
        ),
        None => format!("{program} is needed, and it is not installed."),
    }
}
