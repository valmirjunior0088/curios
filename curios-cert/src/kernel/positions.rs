//! The erased positions one item's check recorded — the seed obligations (T) and (V) run on.
//!
//! This is the kernel's *output*, not an input: nothing here is consulted to decide whether a term is well-typed. It is collected during the typing walk because that is the only place the answer is available, and drained per item by `Kernel::take_checked`.
//!
//! **Classified when recorded, never afterwards.** A position's type routinely mentions the binders the item opened, and those are retracted the moment the item's check returns, so a later pass cannot ask for their sorts at all — it can only fail, and failing quietly is how positions across a tenth of the fixed prelude came to be silently unconstrained.
//!
//! Three things make that workable and all three are this component's rather than a caller's: sort-hood is memoized per *distinct type*, so classifying at every record site costs one question per type rather than one per position; a classification that could not be decided is kept and surfaced with the drain, since a recording site returns nothing and cannot report it; and the walk is re-entrancy guarded, because deciding a position's erased half types terms of its own and those must not be recorded as positions in turn.

use {
    super::{Erased, KernelError},
    curios_core::Term,
    std::collections::HashMap,
};

#[derive(Default)]
pub(super) struct Positions {
    recorded: Vec<(Term, Erased)>,
    /// Sort-hood per distinct type. Keyed on terms whose binders are the item's own, which is what makes an entry meaningful only within the item that made it — cleared with the drain.
    memo: HashMap<Term, Option<Erased>>,
    /// The first classification that could not be decided.
    failure: Option<KernelError>,
    /// Re-entrancy guard: set while an erased half is being decided.
    classifying: bool,
}

impl Positions {
    /// Whether a classification is already in flight, in which case this record is one of its own and is dropped.
    pub(super) fn suppressed(&self) -> bool {
        self.classifying
    }

    /// What `type_` was already classified as, or `None` when it has not been asked about yet.
    pub(super) fn remembered(&self, type_: &Term) -> Option<Option<Erased>> {
        self.memo.get(type_).copied()
    }

    /// Raise the guard for a classification about to run.
    ///
    /// A bracket rather than a closure, and paired with [`Positions::settle`] — two calls that must agree, which is normally a thing to get wrong. It is unavoidable here and safe for one reason: the middle of the bracket is `erased_half`, which needs the whole [`Kernel`](super::Kernel), and handing this component the kernel that owns it is not a thing Rust will do. So the orchestration lives on `Kernel::record_checked`, which is the sole caller of either half and the only one that can exist.
    pub(super) fn begin(&mut self) {
        self.classifying = true;
    }

    /// Lower the guard and remember what `type_` classified as, keeping the first failure to surface with the drain.
    pub(super) fn settle(
        &mut self,
        type_: &Term,
        outcome: Result<Option<Erased>, KernelError>,
    ) -> Option<Erased> {
        self.classifying = false;

        let erased = match outcome {
            Ok(erased) => erased,
            Err(error) => {
                self.failure.get_or_insert(error);
                None
            }
        };
        self.memo.insert(type_.clone(), erased);

        erased
    }

    pub(super) fn push(&mut self, term: &Term, erased: Erased) {
        self.recorded.push((term.clone(), erased));
    }

    /// Take this item's positions and any classification that could not be decided, leaving both empty for the next item.
    ///
    /// The memo goes with them: its keys mention the item's own binders, so an entry means nothing once they are retracted.
    pub(super) fn drain(&mut self) -> (Vec<(Term, Erased)>, Option<KernelError>) {
        self.memo.clear();

        (std::mem::take(&mut self.recorded), self.failure.take())
    }
}
