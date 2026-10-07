//! The written binders a proof the elaborator wrote read.
//!
//! Every lint is a zero the lowering decides, and the one reference no written name makes is a proof the elaborator writes from the facts in scope: it reads the binders it sums. A binder one reads is *credited* here, by its declaration and its place among that declaration's written binders, and `unused-binder` does not report it. Appended to as proofs are written, and taken back where the candidate a proof was written inside is rolled back.

use {
    curios_core::{Free, Global, Term},
    std::collections::{BTreeMap, BTreeSet},
};

/// The binders opened while one item elaborates, and the ones proofs have read.
#[derive(Debug, Default)]
pub(crate) struct Credits {
    /// The written binder each local was opened from, kept while one item elaborates: the declaration it was opened in, and the binder's place among that declaration's written binders. The declaration is read from here rather than from what is current when a proof reads the local, since a group's parked bounds are retried once every member has elaborated. The place is kept here rather than on the local, so a scope rebuilt over the local remembers none: a term that reaches another declaration is credited nothing there.
    opened: BTreeMap<Free, (Option<Global>, u32)>,
    /// The written binders a proof the elaborator wrote reads, each by its declaration and its place among that declaration's written binders, in the order they were credited. A list rather than a set so a rollback truncates it with the solutions it was credited beside.
    credited: Vec<(Option<Global>, u32)>,
}

impl Credits {
    /// Record that `local` was opened from the binder written at `written` among `declaration`'s written binders.
    pub(crate) fn open(&mut self, local: Free, declaration: Option<Global>, written: u32) {
        self.opened.insert(local, (declaration, written));
    }

    /// Forget where the previous item's locals were opened: a proof is written only while its bound's item elaborates.
    pub(crate) fn begin_item(&mut self) {
        self.opened.clear();
    }

    /// Credit every written binder `proof` reads: a proof the elaborator wrote, which the author did not, reads it where no written name reaches it.
    pub(crate) fn credit(&mut self, proof: &Term) {
        let read = proof
            .free_vars_shared()
            .iter()
            .filter_map(|local| self.opened.get(local).copied())
            .collect::<Vec<_>>();
        self.credited.extend(read);
    }

    /// The written binders proofs have read, each by its declaration and its place among that declaration's written binders — each used, though no written reference reaches it.
    pub(crate) fn credited(&self) -> BTreeSet<(Option<Global>, u32)> {
        self.credited.iter().copied().collect()
    }

    /// How many binders have been credited: the watermark a rollback takes credits back to.
    pub(crate) fn mark(&self) -> usize {
        self.credited.len()
    }

    /// Take back every credit made since `mark`: a proof written inside what is rolled back reads nothing that stands.
    pub(crate) fn truncate(&mut self, mark: usize) {
        self.credited.truncate(mark);
    }
}
