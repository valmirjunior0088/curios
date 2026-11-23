//! The checks the runner holds: judgments of the tree no command states, each named by a step's `{ check = "<name>" }` and run in the step's place.
//!
//! **A check is a judgment, as `release`'s refusals are.** What a step can say as a command it says as one; what no command says without a shell between — a revision's tree against the working tree's, a file opening with another, two tables equal, every citation of a document reaching a file — is Rust here, read off the tree's files where they stand, and reports every departure it finds rather than the first. Which step runs a check, and what it runs after, is the task files'.

mod zed;
use zed::*;

/// A check the runner holds: the name a step writes it under, and the judgment it makes, every departure a line of the error.
#[derive(Debug, Clone, Copy)]
pub struct Check {
    pub name: &'static str,
    pub judge: fn() -> Result<(), String>,
}

/// Every check a step may name: the reader refuses any other.
pub const CHECKS: [Check; 1] = [Check {
    name: "zed",
    judge: zed,
}];
