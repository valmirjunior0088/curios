//! The independent kernel: the judgments that decide whether a term is
//! well-typed, written against the representation and nothing else.
//!
//! The elaborator in `curios-elab` is a large, stateful program. It inserts
//! implicit arguments, invents and solves metavariables, parks and wakes
//! conversion goals, resolves witnesses, refines scrutinees inside match arms,
//! and memoizes almost all of it. Every one of those mechanisms exists to make
//! the *surface language* ergonomic, and every one of them is a way for a bad
//! program to be admitted. What this module provides is the other half of the
//! bargain: a second opinion that shares none of that machinery.
//!
//! The independence is structural, not a matter of discipline. This crate does
//! not depend on `curios-elab`, so nothing here can consult a metavariable
//! store, a refinement, or a cached elaboration — not because the code declines
//! to, but because those types are not in scope. A judgment the elaborator gets
//! wrong is re-decided here from the term alone.
//!
//! What the kernel *does* share is the representation: [`Term`], its binder
//! discipline, the primitive roster, and the primitive folds. Sharing a
//! representation is not sharing a judgment. Two checkers that disagree about a
//! term's type while agreeing on what a term *is* still catch each other's
//! mistakes; two that share the rule that admits a bad program catch nothing.
//! That line is why [`Reducer`](super::Reducer) exists, and it is why the match
//! dispatch below is written out again here rather than lifted from the
//! elaborator's reducer, which it closely resembles.

mod whnf;
pub use whnf::*;

use {
    super::{Free, ReduceError, Term, UniverseContext},
    curios_base::Entropy,
    std::collections::HashMap,
};

/// A definition the kernel may unfold, with the universe context it was
/// generalized under.
///
/// The context is not decoration: a definition with universe parameters is
/// *not* unfoldable through a bare occurrence, because such an occurrence
/// denotes no particular instance. It reduces only through a
/// [`UniverseInst`](super::UniverseInst) that says which one.
struct Definition {
    value: Term,
    universes: UniverseContext,
}

/// The kernel's context: what it may unfold, and how much work it may spend
/// doing so.
///
/// Deliberately small. The elaborator's `Context` carries fifteen-odd stores —
/// caches, parked goals, refinement layers, a metavariable heap — and each is a
/// place where an answer can come from something other than the term in hand.
/// The kernel holds definitions and a budget, and that is the whole of its
/// state. Growing this struct is how independence gets lost, so a new field
/// should have to argue for itself.
pub struct Kernel {
    /// Reduction steps a single judgment may spend. Restored at each
    /// declaration boundary by [`Kernel::restore_budget`].
    budget: u64,
    remaining: u64,
    /// Identities for binders the kernel opens itself, during eta-contraction
    /// and under a telescope. Seeded above every index the earlier stages
    /// minted, so a kernel-minted binder can never alias one already in a term.
    fresh_names: Entropy,
    definitions: HashMap<Free, Definition>,
}

impl Kernel {
    /// A kernel that may spend `budget` reduction steps per judgment.
    pub fn new(budget: u64) -> Self {
        Self {
            budget,
            remaining: budget,
            fresh_names: Entropy::new(),
            definitions: HashMap::new(),
        }
    }

    /// Raise the binder counter above every index minted by an earlier stage.
    ///
    /// The lowerer, the elaborator, and the archived prelude all mint into one
    /// identity space; a kernel that started at zero would mint binders that
    /// alias theirs, and an alias between two distinct binders is a capture.
    pub fn set_local_floor(&mut self, floor: usize) {
        self.fresh_names.seed(floor);
    }

    /// Record that `name` denotes `value`, generalized over `universes`.
    pub fn define(&mut self, name: &Free, value: &Term, universes: &UniverseContext) {
        self.definitions.insert(
            name.clone(),
            Definition {
                value: value.clone(),
                universes: universes.clone(),
            },
        );
    }

    /// Charge one reduction step, failing when the budget is spent.
    ///
    /// The kernel is not strongly normalizing and does not pretend to be: a
    /// non-productive `rec` reduces forever. The budget is what makes every
    /// judgment terminate, and it is deterministic — the same program spends
    /// the same steps on every machine — so exhausting it is a fact about the
    /// program, not about the host that checked it.
    fn spend(&mut self) -> Result<(), ReduceError> {
        match self.remaining {
            0 => Err(ReduceError::Exhausted),
            remaining => {
                self.remaining = remaining - 1;
                Ok(())
            }
        }
    }

    /// A fresh binder identity, rendering as `hint`.
    fn fresh(&self, hint: Option<&str>) -> Free {
        let index = u32::try_from(self.fresh_names.fresh()).expect("binder space exhausted");

        Free::local(index, hint)
    }

    /// What `name` unfolds to through a bare occurrence.
    ///
    /// A definition with universe parameters is withheld: see [`Definition`].
    fn value(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .filter(|definition| definition.universes.parameter_count == 0)
            .map(|definition| &definition.value)
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one
    /// position a polymorphic definition may be unfolded from.
    fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .map(|definition| &definition.value)
    }

    /// Restore the full budget for a new judgment.
    pub fn restore_budget(&mut self) {
        self.remaining = self.budget;
    }
}
