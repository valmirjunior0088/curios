//! Every term elaboration settled, with the type it settled at.
//!
//! The seed of the erasure obligations, which read them once an item has finished. Recorded where elaboration already knows the answer rather than reconstructed afterwards, because what type a term was checked against is a fact elaboration computes for every term and a later walk can only re-derive incompletely (see `crate::totality`). Appended to as terms settle, and taken back where a refused item or a discarded candidate recorded them.

use {
    curios_core::{Global, Term},
    std::mem,
};

/// The settled terms of the items elaborated so far, each with its type and the definition whose body it was settled in.
#[derive(Debug, Default)]
pub(crate) struct CheckedTerms {
    /// The site travels as the definition's identity, a copy, so recording is two pointer bumps beside it.
    terms: Vec<(Term, Term, Option<Global>)>,
    /// The definition whose body is currently elaborating, for those sites: `None` is the entry.
    site: Option<Global>,
}

impl CheckedTerms {
    /// Record one settled term and the type it settled at — obligation (V)'s seed, collected where elaboration already knows the answer.
    ///
    /// Sort-hood is *not* decided here. The type may still carry unsolved metavariables, and deciding would both reduce on the hot path and risk an answer a later solution invalidates; the gate classifies post-zonk, memoized per distinct type.
    pub(crate) fn record(&mut self, term: &Term, type_: &Term) {
        self.terms.push((term.clone(), type_.clone(), self.site));
    }

    /// Name the definition whose body is elaborating, for (V)'s diagnostics, answering the site that stood: the two halves of the bracket around a body.
    pub(crate) fn set_site(&mut self, site: Option<Global>) -> Option<Global> {
        mem::replace(&mut self.site, site)
    }

    /// The site being elaborated, as [`CheckedTerms::set_site`] last set it.
    pub(crate) fn site(&self) -> Option<Global> {
        self.site
    }

    /// The recorded terms, unconsumed — obligation (T) reads them first, and (V) takes them afterwards.
    pub(crate) fn terms(&self) -> &[(Term, Term, Option<Global>)] {
        &self.terms
    }

    /// How many terms are recorded — where an item begins, so [`CheckedTerms::truncate`] can forget what a refused item recorded.
    pub(crate) fn mark(&self) -> usize {
        self.terms.len()
    }

    /// Forget every term recorded since `mark`: a refused item's, or a discarded candidate's, which no obligation may read.
    pub(crate) fn truncate(&mut self, mark: usize) {
        self.terms.truncate(mark);
    }

    /// Take the terms recorded since `mark`: a published declaration's, or the entry's, which (V) reads last.
    pub(crate) fn split_off(&mut self, mark: usize) -> Vec<(Term, Term, Option<Global>)> {
        self.terms.split_off(mark)
    }

    /// Replace the terms recorded since `mark` by `settled`, the same terms zonked: so the obligations read them once the state they were recorded in is gone.
    pub(crate) fn replace_since(
        &mut self,
        mark: usize,
        settled: Vec<(Term, Term, Option<Global>)>,
    ) {
        self.terms.truncate(mark);
        self.terms.extend(settled);
    }
}
