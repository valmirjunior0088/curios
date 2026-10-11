//! What a step needs of a tool cargo does not bring: how each is asked whether it is there, the oldest version it runs under, and what installs it.
//!
//! Each is spelled once, here, so the step that refuses and the message that says why read the same value.

use {
    crate::{ask, ask_cargo},
    std::process::Command,
};

#[cfg(test)]
mod tests;

/// The oldest Node the browser suite runs under: the first whose engine runs WebAssembly GC unflagged and whose test runner expands a pattern itself. `.node-version` states it, for the check workflow that installs it and for the step that refuses an older one, so the Node the suite is held to is the one a contributor is asked for.
const NODE_VERSION: &str = include_str!("../../.node-version");

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
    /// `Ok` where the tool is there and new enough; otherwise the sentence saying what is missing and what installs it.
    pub(crate) fn ready(self) -> Result<(), String> {
        match self {
            Tool::Nextest => match lists_nextest(&ask_cargo(&["--list"])?) {
                true => Ok(()),
                false => Err(nextest_refusal()),
            },
            Tool::Node => {
                let floor = node_major(NODE_VERSION)
                    .ok_or_else(|| format!("`.node-version` names no version: {NODE_VERSION}"))?;
                let version = ask(Command::new("node"), &["--version"])
                    .map_err(|error| format!("Node {floor} or later is needed, and {error}"))?;
                let version = version.trim();

                match node_major(version).is_some_and(|major| major >= floor) {
                    true => Ok(()),
                    false => Err(format!(
                        "Node {floor} or later is needed, and `node --version` answered {version}"
                    )),
                }
            }
            Tool::Npm => ask(Command::new("npm"), &["--version"])
                .map(drop)
                .map_err(|error| format!("npm is needed, and {error}")),
        }
    }
}

/// The major version `node --version` or `.node-version` names, as in `v22.4.1` and `22`.
fn node_major(version: &str) -> Option<u32> {
    version
        .trim()
        .trim_start_matches('v')
        .split('.')
        .next()
        .and_then(|major| major.parse().ok())
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
