//! How a tool cargo does not bring is launched: asked first whether it is there, and refused with what installs it where it is not.
//!
//! A step asks for a tool by launching it, through [`Tool::command`], so what a step needs is what it runs as it was asked: a lint narrowed to one package launches no Biome and asks for none. A step asks only that its tool is there. Which version of it a machine holds is the machine's: the check workflow pins nextest's, Biome's and Node's for its runners, and a contributor installs what they have.

use {
    crate::{ask, ask_cargo, toolchain_cargo},
    std::process::Command,
};

#[cfg(test)]
mod tests;

/// What installs nextest where it is absent. Cargo's own install rather than a prebuilt download, because it is one spelling on every platform and needs nothing a clone does not already have.
pub(crate) const NEXTEST_INSTALL: &str = "cargo install cargo-nextest --locked";

/// What installs Biome where it is absent: its npm package, the one spelling on every platform a clone with npm has.
pub(crate) const BIOME_INSTALL: &str = "npm install --global @biomejs/biome";

/// A tool a step launches beyond cargo.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tool {
    Nextest,
    Biome,
    Node,
    Npm,
}

impl Tool {
    /// The command that launches the tool, once the tool is found to be there: the one way a step reaches a tool beyond cargo. Where it is absent, the sentence saying what is missing and, where one command does it, what installs it.
    pub(crate) fn command(self) -> Result<Command, String> {
        self.ready()?;

        Ok(self.launcher())
    }

    /// The tool's command line before its arguments. Nextest is a subcommand of the cargo every recipe runs.
    fn launcher(self) -> Command {
        match self {
            Tool::Nextest => {
                let mut cargo = toolchain_cargo();
                cargo.arg("nextest");

                cargo
            }
            Tool::Biome => Command::new("biome"),
            Tool::Node => Command::new("node"),
            Tool::Npm => Command::new("npm"),
        }
    }

    /// `Ok` where the tool is there; otherwise the sentence saying what is missing and, where one command does it, what installs it.
    fn ready(self) -> Result<(), String> {
        match self {
            Tool::Nextest => match lists_nextest(&ask_cargo(&["--list"])?) {
                true => Ok(()),
                false => Err(nextest_refusal()),
            },
            Tool::Biome => ask(self.launcher(), &["--version"])
                .map(drop)
                .map_err(|_| biome_refusal()),
            Tool::Node => ask(self.launcher(), &["--version"])
                .map(drop)
                .map_err(|error| format!("Node is needed, and {error}")),
            Tool::Npm => ask(self.launcher(), &["--version"])
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

/// That a JavaScript, JSON or CSS check cannot run without Biome, and the command that installs it, on a line of its own as [`nextest_refusal`]'s is.
fn biome_refusal() -> String {
    format!("Biome is needed, and it is not installed. Install it with:\n\n    {BIOME_INSTALL}")
}

/// That the suite cannot run without nextest, and the command that installs it — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
fn nextest_refusal() -> String {
    format!(
        "cargo-nextest is needed, and it is not installed. Install it with:\n\n    {NEXTEST_INSTALL}"
    )
}
