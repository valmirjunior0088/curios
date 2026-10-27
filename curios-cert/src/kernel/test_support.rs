//! What a suite asks the kernel and no judgment does: what a declaration has spent.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, through which a reader's suite reaches it, so no build that ships holds it.

use {super::Kernel, curios_core::Consumption};

impl Kernel<'_> {
    /// What the declaration under judgment has consumed so far ([`Spend::consumed`](super::Spend::consumed)), which this crate's tests read between two judgments, and a reader's suite through [`Reader::consumed`](crate::Reader::consumed).
    pub(crate) fn consumed(&self) -> Consumption {
        self.spend.consumed()
    }
}
