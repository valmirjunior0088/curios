//! What a command's words launch, and what a refusal says where its program is not installed.
//!
//! **A command is the words a person would type.** Leading `NAME=value` words set the environment, as a shell reads them, and the next word names the program. `cargo` is the cargo that launched this run: `cargo xtask` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a step cannot resolve a second time and land somewhere else. `cargo nextest` is launched as nextest's own program, `cargo-nextest nextest`, as cargo itself launches it, so an absent one fails to spawn as every other program does.
//!
//! **A program is launched, never asked about.** Where its program is not there, the launch is refused with what installs it, and where it is, nothing ran beforehand to find out, so what a step needs is what it runs as it was asked.

use std::{env, process::Command};

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

    let (program, mut command, arguments) = match (program.as_str(), arguments) {
        ("cargo", [nextest, rest @ ..]) if nextest == "nextest" => {
            let mut command = Command::new("cargo-nextest");
            command.arg("nextest");
            ("cargo-nextest", command, rest.to_vec())
        }
        ("cargo", rest) => (
            "cargo",
            Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
            rest.to_vec(),
        ),
        (program, rest) => (program, Command::new(program), rest.to_vec()),
    };
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

/// What installs nextest where it is absent. Cargo's own install rather than a prebuilt download, because it is one spelling on every platform and needs nothing a clone does not already have.
pub(crate) const NEXTEST_INSTALL: &str = "cargo install cargo-nextest --locked";

/// What installs Biome where it is absent: its npm package, the one spelling on every platform a clone with npm has.
pub(crate) const BIOME_INSTALL: &str = "npm install --global @biomejs/biome";

/// What installs the bindings generator where it is absent, at the version `Cargo.lock` gives `wasm-bindgen`, which the generator must match.
pub(crate) const WASM_BINDGEN_INSTALL: &str =
    "cargo install wasm-bindgen-cli --locked --version <the version Cargo.lock gives wasm-bindgen>";

/// That a step cannot run without `program`, and, where one command installs it, that command — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
pub(crate) fn missing(program: &str) -> String {
    let install = match program {
        "rustfmt" => Some("rustup component add rustfmt"),
        "cargo-nextest" => Some(NEXTEST_INSTALL),
        "biome" => Some(BIOME_INSTALL),
        "wasm-bindgen" => Some(WASM_BINDGEN_INSTALL),
        _ => None,
    };

    match install {
        Some(install) => {
            format!(
                "{program} is needed, and it is not installed. Install it with:\n\n    {install}"
            )
        }
        None => format!("{program} is needed, and it is not installed."),
    }
}
