//! The tools a step launches beyond cargo, and what a refusal says where one is not installed.
//!
//! **A tool is launched, never asked about.** A step runs a tool's program from the path, and the spawn is the check: where the program is not there, the step is refused with what installs it, and where it is, nothing ran beforehand to find out. So what a step needs is what it runs as it was asked — a lint narrowed to one package launches no Biome and needs none. Which version of a tool a machine holds is the machine's: the check workflow pins nextest's, Biome's and Node's for its runners, and a contributor installs what they have.

use std::process::Command;

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
    /// The command that runs the tool, its program found on the path when it is spawned. Nextest is launched as its own program, `cargo-nextest nextest`, as cargo itself launches it, so an absent one fails to spawn as every other tool does; it builds with the cargo `CARGO` names, the one that launched this crate.
    pub(crate) fn command(self) -> Command {
        match self {
            Tool::Nextest => {
                let mut nextest = Command::new("cargo-nextest");
                nextest.arg("nextest");

                nextest
            }
            Tool::Biome => Command::new("biome"),
            Tool::Node => Command::new("node"),
            Tool::Npm => Command::new("npm"),
        }
    }

    /// That a step cannot run without the tool, and, where one command installs it, that command — on a line of its own, indented, because it is there to be selected and pasted rather than read, as the installer's advice is.
    pub(crate) fn missing(self) -> String {
        let (name, install) = match self {
            Tool::Nextest => ("cargo-nextest", Some(NEXTEST_INSTALL)),
            Tool::Biome => ("Biome", Some(BIOME_INSTALL)),
            Tool::Node => ("Node", None),
            Tool::Npm => ("npm", None),
        };

        match install {
            Some(install) => {
                format!(
                    "{name} is needed, and it is not installed. Install it with:\n\n    {install}"
                )
            }
            None => format!("{name} is needed, and it is not installed."),
        }
    }
}
