//! The programs a step runs, and what a refusal says where one is not installed.
//!
//! **A tool is launched, never asked about.** A step runs a tool's program, and the spawn is the check: where the program is not there, the step is refused with what installs it, and where it is, nothing ran beforehand to find out. So what a step needs is what it runs as it was asked — a lint narrowed to one package launches no Biome and needs none. Cargo and rustfmt come with the toolchain `rust-toolchain.toml` pins; the rest a contributor installs, and which version of one a machine holds is the machine's: the check workflow pins nextest's, Biome's and Node's for its runners.

use std::{env, process::Command};

#[cfg(test)]
mod tests;

/// What installs nextest where it is absent. Cargo's own install rather than a prebuilt download, because it is one spelling on every platform and needs nothing a clone does not already have.
pub(crate) const NEXTEST_INSTALL: &str = "cargo install cargo-nextest --locked";

/// What installs Biome where it is absent: its npm package, the one spelling on every platform a clone with npm has.
pub(crate) const BIOME_INSTALL: &str = "npm install --global @biomejs/biome";

/// A program a step runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tool {
    Cargo,
    Rustfmt,
    Nextest,
    Biome,
    Node,
    Npm,
}

impl Tool {
    /// The command that runs the tool, its program found when it is spawned.
    ///
    /// Cargo is the one that launched this crate: `cargo xtask` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a step cannot resolve a second time and land somewhere else. The fallback is for the other way in, running the built binary directly. Nextest is launched as its own program, `cargo-nextest nextest`, as cargo itself launches it, so an absent one fails to spawn as every other tool does; it builds with the cargo `CARGO` names.
    pub(crate) fn command(self) -> Command {
        match self {
            Tool::Cargo => Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
            Tool::Rustfmt => Command::new("rustfmt"),
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
            Tool::Cargo => ("cargo", None),
            Tool::Rustfmt => ("rustfmt", Some("rustup component add rustfmt")),
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
