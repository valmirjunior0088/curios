//! What a plan reads of the checkout, asked once a run.
//!
//! **A plan is computed before anything runs, so planning reads and never writes.** Every step's plan is made from the request and the checkout as it stands, and no plan reads what another step writes, so a run's whole plan exists before its first action. A question planning puts to git is asked here, once, and every plan of the run reads the one answer.

use {
    crate::ask,
    std::{cell::OnceCell, process::Command},
};

/// The checkout as a run's plans read it.
#[derive(Default)]
pub(crate) struct Checkout {
    tracked: OnceCell<Vec<String>>,
}

impl Checkout {
    /// Every path git tracks, from the repository's root.
    pub(crate) fn tracked(&self) -> Result<&[String], String> {
        if let Some(tracked) = self.tracked.get() {
            return Ok(tracked);
        }

        let mut git = Command::new("git");
        git.arg("ls-files");
        let listing = ask(git, &[])?;

        Ok(self
            .tracked
            .get_or_init(|| listing.lines().map(str::to_string).collect()))
    }
}
