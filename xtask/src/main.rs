//! The repository's steps, as `cargo xtask <task>`: a runner of the task files `xtask/tasks.toml` and `xtask/tasks/*.toml`.
//!
//! **The files are the plan, and this crate holds the rules any repository's runner would.** Every fact about what the repository runs — each step's commands, what it requires and consumes, the files it produces, the values its commands share, the tools they run and where each is fetched from — is written in the task files, and [`tree`] reads them, refusing before anything runs whatever is out of place or names nothing. What is left here holds for any such tree: [`schedule()`] takes what was asked and everything it runs after, in the order the files write them, [`runner`] performs the steps and reports each, [`values`] fills a command's braces, [`launch`](mod@launch) says what a command's words launch, [`tools`] keeps the tools the steps name, and [`cli`] reads the command line off the tree. Why the steps are data and the runner is rules is `README.md`'s.
//!
//! **A job of the repository's own is a script a step runs.** What no single command states — the release, a pin held to its grammar, the agent files' citations — is a script beside the task file of its step, reached as any program is ([`xtask`'s README](../README.md#a-job-of-this-repositorys-own-is-a-script-a-step-runs)).
//!
//! **A dependency of nothing, beside the compiler.** This crate is reached only through the `xtask` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to run the workspace's steps, not to be part of the compiler, so it is a crate of the jig the compiler's checks leave out of their selection, as they leave out the bench and the board. Why is the README's decision.

mod places;
use places::*;

mod commands;
use commands::*;

mod launch;
use launch::*;

mod refusal;
use refusal::*;

mod download;
use download::*;

mod tree;
use tree::*;

mod values;
use values::*;

mod tools;
use tools::*;

mod schedule;
use schedule::*;

mod runner;
use runner::*;

mod cli;
use cli::*;

use std::{
    env,
    io::{self, IsTerminal},
    process::ExitCode,
};

#[cfg(test)]
mod tests;

fn main() -> ExitCode {
    let tree = match Tree::read() {
        Ok(tree) => tree,
        Err(refusal) => {
            // Painted where standard error is a terminal and `NO_COLOR` says nothing.
            let painted = io::stderr().is_terminal()
                && env::var_os("NO_COLOR").is_none_or(|unpainted| unpainted.is_empty());
            eprintln!("{}", refusal.rendered(painted));

            return ExitCode::FAILURE;
        }
    };

    let Asked { name, given } = asked(&command(&tree).get_matches());
    let outcome = perform(&tree, &name, &given);

    match outcome {
        Ok(true) => ExitCode::SUCCESS,
        // A run whose step failed has said so on the line that closes the step, and one held back on a line of its own.
        Ok(false) => ExitCode::FAILURE,
        Err(message) => {
            eprintln!("{message}");

            ExitCode::FAILURE
        }
    }
}
