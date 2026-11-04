//! The repository's steps, as `cargo xtask <task>`: a runner of the task files `xtask/tasks.toml` and `xtask/tasks/*.toml`.
//!
//! **The files are the plan, and this crate holds only rules.** Every fact about what the repository runs — each step's commands, what it requires and consumes, the files it produces, the values its commands share — is written in the task files, and [`tree`] reads them, refusing before anything runs whatever is out of place or names nothing. What is left here holds for any such tree: [`schedule()`] takes what was asked and everything it runs after, in the order the files write them, [`runner`] performs the steps and reports each, [`values`] fills a command's braces, [`programs`] says what a command's words launch, and [`cli`] reads the command line off the tree. Why the steps are data and the runner is rules is `README.md`'s.
//!
//! **Two commands are not tasks.** `release` cuts a release and `installer` renders the release's install script: each makes judgments of its own rather than running a plan, so each is Rust, and the reader refuses a task of either name.
//!
//! **A dependency of nothing, beside the compiler.** This crate is reached only through the `xtask` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to run the workspace's steps, not to be part of the compiler, so it is a crate of the jig the compiler's checks leave out of their selection, as they leave out the bench and the board. Why is the README's decision.

mod places;
use places::*;

mod commands;
use commands::*;

mod programs;
use programs::*;

mod tree;
use tree::*;

mod values;
use values::*;

mod schedule;
use schedule::*;

mod runner;
use runner::*;

mod cli;
use cli::*;

mod installer;
use installer::*;

mod release;
use release::*;

use std::process::ExitCode;

#[cfg(test)]
mod tests;

fn main() -> ExitCode {
    let tree = match Tree::read() {
        Ok(tree) => tree,
        Err(refusal) => {
            eprintln!("{refusal}");

            return ExitCode::FAILURE;
        }
    };

    let outcome = match asked(&command(&tree).get_matches()) {
        Asked::Task { name, given } => perform(&tree, &name, &given),
        Asked::Release { bump } => release(bump.as_deref()),
        Asked::Installer { version } => installer(&version),
    };

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");

            ExitCode::FAILURE
        }
    }
}
