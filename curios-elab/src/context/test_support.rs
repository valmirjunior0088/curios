//! What a suite reads of a context and no judgment does: what the declaration being elaborated has spent.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {super::Context, curios_core::Consumption};

impl Context<'_> {
    /// What the declaration being elaborated has consumed so far (`Spend::consumed`), which the measurements read between two judgments.
    pub fn consumed(&self) -> Consumption {
        self.typing.spend.consumed()
    }
}
