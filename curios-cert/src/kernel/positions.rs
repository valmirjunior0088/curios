//! The erased positions one item's check recorded — the seed obligations (T) and (V) run on.
//!
//! This is the kernel's *output*, not an input: nothing here is consulted to decide whether a term is well-typed. It is collected during the typing walk because that is the only place the answer is available, and drained per item by `Kernel::take_checked`.
//!
//! **Classified when recorded, never afterwards.** A position's type routinely mentions the binders the item opened, and those are retracted the moment the item's check returns, so a later pass cannot ask for their sorts at all — it can only fail, and a quiet failure would leave positions silently unconstrained.
//!
//! Three things make that workable. Which half a type's positions belong to is remembered beside the kernel's sorts and under their lives ([`Memos`](super::Memos)), so classifying at every record site costs one question per type for as long as what its sort was read off stands — and a type an arm's equation makes a proposition is classified under each arm's own, where a memo kept here for the whole item would hand one arm's answer to the next. A classification that could not be decided is kept and surfaced with the drain, since a recording site returns nothing and cannot report it; and the walk is re-entrancy guarded, because deciding a position's erased half types terms of its own and those must not be recorded as positions in turn. The last two are this component's rather than a caller's.

use {super::Error, curios_analysis::Erased, curios_core::Term};

/// One erased position an item's check recorded.
pub(crate) struct Position {
    pub(crate) term: Term,
    pub(crate) erased: Erased,
    /// Whether typing `term` closed a group that does not descend — noted where the group was typed, since the group is typed with the binders around it opened and `term` holds it closed.
    pub(crate) encloses_partial: bool,
}

#[derive(Default)]
pub(super) struct Positions {
    recorded: Vec<Position>,
    /// The first classification that could not be decided.
    failure: Option<Error>,
    /// Re-entrancy guard: set while an erased half is being decided.
    classifying: bool,
}

impl Positions {
    /// Whether a classification is already in flight, in which case this record is one of its own and is dropped.
    pub(super) fn suppressed(&self) -> bool {
        self.classifying
    }

    /// Raise the guard for a classification about to run.
    ///
    /// A bracket rather than a closure, and paired with [`Positions::settle`] — two calls that must agree, which is normally a thing to get wrong. It is unavoidable here and safe for one reason: the middle of the bracket is `erased_half`, which needs the whole [`Kernel`](super::Kernel), and handing this component the kernel that owns it is not a thing Rust will do. So the orchestration lives on `Kernel::record_checked`, which is the sole caller of either half and the only one that can exist.
    pub(super) fn begin(&mut self) {
        self.classifying = true;
    }

    /// Lower the guard and hand back what the type classified as, keeping the first failure to surface with the drain.
    pub(super) fn settle(&mut self, outcome: Result<Option<Erased>, Error>) -> Option<Erased> {
        self.classifying = false;

        match outcome {
            Ok(erased) => erased,
            Err(error) => {
                self.failure.get_or_insert(error);
                None
            }
        }
    }

    /// Record `term` at `erased`, handing back where it was recorded for [`Positions::enclose_partial`].
    pub(super) fn push(&mut self, term: &Term, erased: Erased) -> usize {
        self.recorded.push(Position {
            term: term.clone(),
            erased,
            encloses_partial: false,
        });

        self.recorded.len() - 1
    }

    /// The position recorded at `index` enclosed a group that does not descend.
    pub(super) fn enclose_partial(&mut self, index: usize) {
        if let Some(position) = self.recorded.get_mut(index) {
            position.encloses_partial = true;
        }
    }

    /// Take this item's positions and any classification that could not be decided, leaving both empty for the next item.
    pub(super) fn drain(&mut self) -> (Vec<Position>, Option<Error>) {
        (std::mem::take(&mut self.recorded), self.failure.take())
    }
}
