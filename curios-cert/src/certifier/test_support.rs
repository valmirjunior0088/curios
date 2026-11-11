//! What a suite asks the certifier and no judgment does: what a declaration has spent, and a certifier with its memos off.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, through which a reader's suite reaches it, so no build that ships holds it.

use {super::*, curios_common::SyntaxRegistry, curios_core::Consumption};

impl Certifier<'_> {
    /// A certifier whose evaluation memos are off — every reduction re-derived from scratch. Exists for one purpose: asserting that memoization changes no *semantic* verdict. It may change a resource one, since a term-keyed hit is free and an uncached walk therefore spends at least as much; see the `spend` module's documentation for why that is the whole of what was given up.
    pub fn uncached(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            memos: Memos::new(false),
            ..Self::new(budget, syntax)
        }
    }

    /// What the declaration under judgment has consumed so far ([`Spend::consumed`](super::Spend::consumed)), which this crate's tests read between two judgments, and a reader's suite through [`Reader::consumed`](crate::Reader::consumed).
    pub(crate) fn consumed(&self) -> Consumption {
        self.spend.consumed()
    }
}
