//! What a step needs of a tool cargo does not bring: how each is asked whether it is there, and what installs it.
//!
//! A step asks only that its tool is there. Which version of it a machine holds is the machine's: the check workflow pins nextest's and Node's for its runners, and a contributor installs what they have. Biome is the exception, since this crate is what fetches it.

use {
    crate::{ask, ask_cargo},
    std::process::Command,
};

#[cfg(test)]
mod tests;

/// What installs nextest where it is absent. Cargo's own install rather than a prebuilt download, because it is one spelling on every platform and needs nothing a clone does not already have.
pub(crate) const NEXTEST_INSTALL: &str = "cargo install cargo-nextest --locked";

/// The package Biome is run from, at the one version every checkout formats and lints with: a formatter's output moves between releases, so the version is exact.
pub(crate) const BIOME: &str = "@biomejs/biome@2.5.15";

/// A tool a step needs beyond cargo.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tool {
    Nextest,
    Node,
    Npm,
}

impl Tool {
    /// `Ok` where the tool is there; otherwise the sentence saying what is missing and, where one command does it, what installs it.
    pub(crate) fn ready(self) -> Result<(), String> {
        match self {
            Tool::Nextest => match lists_nextest(&ask_cargo(&["--list"])?) {
                true => Ok(()),
                false => Err(nextest_refusal()),
            },
            Tool::Node => ask(Command::new("node"), &["--version"])
                .map(drop)
                .map_err(|error| format!("Node is needed, and {error}")),
            Tool::Npm => ask(Command::new("npm"), &["--version"])
                .map(drop)
                .map_err(|error| format!("npm is needed, and {error}")),
        }
    }
}

/// Whether `cargo --list` names nextest among the commands it can run. Each command is a line led by its name, so a command merely named after nextest is not it.
fn lists_nextest(listing: &str) -> bool {
    listing
        .lines()
        .any(|line| line.split_whitespace().next() == Some("nextest"))
}

/// That the suite cannot run without nextest, and the command that installs it — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
fn nextest_refusal() -> String {
    format!(
        "cargo-nextest is needed, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
    )
}
