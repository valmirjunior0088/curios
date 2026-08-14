//! What a judgment consumes: reduction steps, and binder identities minted.
//!
//! The two are one component because a memo hit has to charge *both*, exactly as a recomputation would have. The budget prices the work a term denotes, performed or replayed; the entropy counter advances as if every eta probe had run, so the whole observable trajectory — refusal payloads, exhaustion points, every later-minted identity — is bit-identical with the evaluation memos on or off. `kernel_memo_parity` is the test that holds that to account, and [`Spend::charge`] is the one place it can be got wrong.
//!
//! [`Memos`](super::Memos) deliberately cannot reach this: it stores [`Replay`]s and hands them back, and charging one is this component's job. A store that could also charge would be a store that could charge twice.

use {
    curios_core::{Free, ReduceError, Term},
    curios_utilities::Entropy,
};

/// One remembered reduction: the reduct, and everything computing it consumed — so a hit can charge exactly what a recomputation would have.
#[derive(Debug, Clone)]
pub(crate) struct Replay {
    pub(super) reduct: Term,
    steps: u64,
    mints: usize,
}

pub(super) struct Spend {
    /// Reduction steps a single judgment may spend. Restored at each declaration boundary by [`Spend::restore_budget`].
    budget: u64,
    remaining: u64,
    /// Identities for binders the kernel opens itself, when comparing under a telescope and when eta-contracting. Seeded above every index the earlier stages minted, so a kernel-minted binder can never alias one in a term.
    minted: Entropy,
}

impl Spend {
    pub(super) fn new(budget: u64) -> Self {
        Self {
            budget,
            remaining: budget,
            minted: Entropy::new(),
        }
    }

    /// Charge one reduction step, failing when the budget is spent.
    ///
    /// The kernel is not strongly normalizing and does not pretend to be: a non-productive `rec` reduces forever. The budget is what makes every judgment terminate, and it is deterministic — the same program spends the same steps on every machine — so exhausting it is a fact about the program, not about the host that checked it.
    pub(super) fn step(&mut self) -> Result<(), ReduceError> {
        match self.remaining {
            0 => Err(ReduceError::Exhausted),
            remaining => {
                self.remaining = remaining - 1;
                Ok(())
            }
        }
    }

    /// Restore the full budget for a new judgment.
    pub(super) fn restore_budget(&mut self) {
        self.remaining = self.budget;
    }

    /// A fresh binder identity, rendering as `hint`.
    pub(super) fn fresh(&self, hint: Option<&str>) -> Free {
        let index = u32::try_from(self.minted.fresh()).expect("binder space exhausted");

        Free::local(index, hint)
    }

    /// Raise the binder counter above every index minted by an earlier stage.
    ///
    /// The lowerer, the elaborator, and the archived prelude all mint into one identity space; a kernel that started at zero would mint binders that alias theirs, and an alias between two distinct binders is a capture.
    pub(super) fn set_local_floor(&mut self, floor: usize) {
        self.minted.seed(floor);
    }

    /// A consumption snapshot, for measuring what a computation charges.
    pub(super) fn snapshot(&self) -> (u64, usize) {
        (self.remaining, self.minted.count())
    }

    /// The [`Replay`] for `reduct`, measured against the snapshot taken before the computation ran.
    pub(super) fn replay_since(&self, reduct: Term, (budget, minted): (u64, usize)) -> Replay {
        Replay {
            reduct,
            steps: budget - self.remaining,
            mints: self.minted.count() - minted,
        }
    }

    /// Charge a replayed computation's whole consumption at once. Exhausting here is exactly where the recomputation would have exhausted mid-chain, and it reports the same error; the entropy advances as if every probe binder had been minted, so later identities land where they would have.
    pub(super) fn charge(&mut self, replay: Replay) -> Result<Term, ReduceError> {
        self.minted.seed(self.minted.count() + replay.mints);

        match self.remaining.checked_sub(replay.steps) {
            Some(remaining) => {
                self.remaining = remaining;
                Ok(replay.reduct)
            }
            None => {
                self.remaining = 0;
                Err(ReduceError::Exhausted)
            }
        }
    }
}
