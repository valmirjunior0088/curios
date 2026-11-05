//! Every term elaboration settled, with the type it settled at.
//!
//! The seed of the erasure obligations, which read them once an item has finished. Recorded where elaboration already knows the answer rather than reconstructed afterwards, because what type a term was checked against is a fact elaboration computes for every term and a later walk can only re-derive incompletely (see `crate::totality`). Appended to as terms settle, and taken back where a refused item or a discarded candidate recorded them.

use {
    curios_core::{Free, Global, Term},
    std::mem,
};

/// The settled terms of the items elaborated so far, each with its type and the definition whose body it was settled in.
#[derive(Debug, Default)]
pub struct CheckedTerms {
    /// The site travels as the definition's identity, a copy, so recording is two pointer bumps beside it.
    terms: Vec<(Term, Term, Option<Global>)>,
    /// The definition whose body is currently elaborating, for those sites: `None` is the entry.
    site: Option<Global>,
    /// What each local binding the items elaborated so far bound stands for, read by value where its block ended, beside how many terms were recorded then: a recorded term names a binding by its label, a local that says nothing of what it stands for, and the obligations read it as its value.
    labels: Vec<(usize, Free, Term)>,
}

impl CheckedTerms {
    /// Record one settled term and the type it settled at — obligation (V)'s seed, collected where elaboration already knows the answer.
    ///
    /// Sort-hood is *not* decided here. The type may still carry unsolved metavariables, and deciding would both reduce on the hot path and risk an answer a later solution invalidates; the gate classifies post-zonk, memoized per distinct type.
    pub fn record(&mut self, term: &Term, type_: &Term) {
        self.terms.push((term.clone(), type_.clone(), self.site));
    }

    /// Name the definition whose body is elaborating, for (V)'s diagnostics, answering the site that stood: the two halves of the bracket around a body.
    pub fn set_site(&mut self, site: Option<Global>) -> Option<Global> {
        mem::replace(&mut self.site, site)
    }

    /// The site being elaborated, as [`CheckedTerms::set_site`] last set it.
    pub fn site(&self) -> Option<Global> {
        self.site
    }

    /// The recorded terms, unconsumed — obligation (T) reads them first, and (V) takes them afterwards.
    pub fn terms(&self) -> &[(Term, Term, Option<Global>)] {
        &self.terms
    }

    /// How many terms are recorded — where an item begins, so [`CheckedTerms::truncate`] can forget what a refused item recorded.
    pub fn mark(&self) -> usize {
        self.terms.len()
    }

    /// Forget every term recorded since `mark`: a refused item's, or a discarded candidate's, which no obligation may read.
    pub fn truncate(&mut self, mark: usize) {
        self.terms.truncate(mark);
        self.drop_labels(mark);
    }

    /// Record that `label` stands for `value`, read by value where the block binding it ends.
    pub fn record_label(&mut self, label: Free, value: Term) {
        self.labels.push((self.terms.len(), label, value));
    }

    /// Each binding recorded since `mark`, with what it stands for.
    pub fn labels_since(&self, mark: usize) -> impl Iterator<Item = &(usize, Free, Term)> {
        self.labels.iter().filter(move |(at, ..)| *at >= mark)
    }

    /// Replace what each binding recorded since `mark` stands for by `settle` of it.
    pub fn restate_labels<E>(
        &mut self,
        mark: usize,
        mut settle: impl FnMut(&Term) -> Result<Term, E>,
    ) -> Result<(), E> {
        for (at, _, value) in &mut self.labels {
            if *at >= mark {
                *value = settle(value)?;
            }
        }

        Ok(())
    }

    /// Forget every binding recorded since `mark`, with the terms that name it.
    pub fn drop_labels(&mut self, mark: usize) {
        self.labels.retain(|(at, ..)| *at < mark);
    }

    /// Take the terms recorded since `mark`: a published declaration's, or the entry's, which (V) reads last.
    pub fn split_off(&mut self, mark: usize) -> Vec<(Term, Term, Option<Global>)> {
        self.terms.split_off(mark)
    }

    /// Replace the terms recorded since `mark` by `settled`, the same terms zonked: so the obligations read them once the state they were recorded in is gone.
    pub fn replace_since(&mut self, mark: usize, settled: Vec<(Term, Term, Option<Global>)>) {
        self.terms.truncate(mark);
        self.terms.extend(settled);
    }
}
