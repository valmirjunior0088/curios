//! What a judgment consumes: reduction steps, and binder identities minted.
//!
//! The two are one component because replaying a remembered computation has to settle both, and they settle differently. The entropy counter always advances as if every eta probe had run, so a hit mints exactly the identities a recomputation would have and every later one lands where it would have. The budget does not: a term-keyed hit is *free*, and only the name-keyed `unfold` replay is charged. [`Spend::charge`] and [`Spend::charge_nothing`] are that split, and [`Memos`](super::Memos) states which table reaches which.
//!
//! **This module used to claim the opposite, and the claim was the defect.** Charging a hit what a memo-free evaluator would have spent prices work the kernel did not perform, and recorded costs *compound*: if computing `Aₙ` hits the memo for `Aₙ₋₁` twice, `S(n) = 2·S(n−1) + c`. A structure cheap to evaluate with memos was expensive to charge, measured at a 262 144-step budget declared exhausted after 6 547 actual reduction steps. The floors in `curios`' `kernel_memo_charge_measurements` are what that cost a user: this kernel refused at 8–16× the budget the elaborator — whose hits were already free — needed for the same program.
//!
//! **What is given up is exactly one thing: exhaustion points.** A free hit can only *reduce* what a judgment spends, so it can only turn a refusal into an acceptance, and only an exhaustion one — a semantic refusal is budget-independent, and an exhaustion masking a type error reaches that type error with more budget and refuses anyway. So the invariant weakens from "memos change nothing" to **"memos change only resource verdicts, never semantic ones"**, and two of the three things this module used to name survive: refusal payloads for semantic errors, and later-minted identities. `kernel_memo_parity` is what holds that half to account, and `Kernel::uncached` is what lets it be asked.
//!
//! Termination is unaffected, and the reason is structural rather than budgetary: a [`Replay`] is built *after* its reduct exists, so a divergent reduction never completes, never stores, and can never be hit. Every step of one is charged.
//!
//! **What remains is a residue this states rather than claims away.** An `unfold` record is measured over a computation that may itself have taken free term-keyed hits, so it can record less than the same body costs cold, and which declaration first unfolds a name therefore decides what every later declaration is charged for it. That is a dependence on check order, and it survives here because the direction is safe: it can only *undercharge*, so it can only accept, and free hits are monotone against the design they replaced for the same reason. Removing it needs the replay record to carry a priced cost of its own, which is the work of the milestone that prices construction rather than of this one.
//!
//! [`Memos`](super::Memos) deliberately cannot reach this: it stores [`Replay`]s and hands them back, and applying one is this component's job. A store that could also charge would be a store that could charge twice.

use {
    curios_core::{Cost, Free, ReduceError, Term},
    curios_utilities::Entropy,
};

/// One remembered reduction: the reduct, and everything computing it consumed — the identities so that every hit mints what a recomputation would have, and the steps so that a charged hit costs what a recomputation would.
#[derive(Debug, Clone)]
pub(crate) struct Replay {
    pub(super) reduct: Term,
    steps: u64,
    mints: usize,
}

impl Replay {
    /// What retaining this record costs the compilation's allowance: the entry itself, and the logical footprint of the reduct whose lifetime storing it extends.
    ///
    /// The recorded `steps` and `mints` are two words and ride in the entry's fixed cost. The reduct is the part that can be arbitrarily large, and is read off its own cached summary in O(1) rather than walked — see [`Term::footprint`].
    pub(super) fn retention(&self) -> Cost {
        Cost::collection(2).saturating_add(Cost::units(self.reduct.footprint()))
    }
}

pub(super) struct Spend {
    /// Units of reduction work a single judgment may spend. Restored at each declaration boundary by [`Spend::restore_budget`]. A transition costs one; a construction costs what it builds.
    budget: u64,
    remaining: u64,
    /// How many guarded reduction levels are live, and the deepest this judgment has reached. See [`Spend::enter_level`].
    depth: usize,
    peak_depth: usize,
    /// Identities for binders the kernel opens itself, when comparing under a telescope and when eta-contracting. Seeded above every index the earlier stages minted, so a kernel-minted binder can never alias one in a term.
    minted: Entropy,
}

impl Spend {
    pub(super) fn new(budget: u64) -> Self {
        Self {
            budget,
            remaining: budget,
            depth: 0,
            peak_depth: 0,
            minted: Entropy::new(),
        }
    }

    /// Enter one guarded reduction level, charging [`Cost::FRAME`] when it is deeper than any level this judgment has reached before.
    ///
    /// **Per new peak, not per call**, and the difference is the whole of what makes this row mean anything. A level's native frame is reclaimed when the level returns, so charging every `whnf` call would price a stack the reduction is not holding — and reduction calls itself once per operand of a nested intrinsic and once per link of a spine, so a wide, shallow computation would pay for depth it never reached. A high-water mark charges exactly the stack the reduction *peaks* at, which is the resource the row exists to bound, and it never refunds: the mark only rises.
    ///
    /// The two machine-independent alternatives were both worse. Charging when `recurse` actually grows would price the 32 MiB segment exactly, and makes acceptance depend on the host thread's stack size — a two-megabyte test thread grows on its first call and an eight-megabyte main thread does not. Charging nothing leaves depth bounded by the host's stack alone, which is what this row exists to replace.
    pub(super) fn enter_level(&mut self, cost: Cost) -> Result<(), ReduceError> {
        self.depth += 1;

        if self.depth > self.peak_depth {
            self.peak_depth = self.depth;
            self.spend(cost)?;
        }

        Ok(())
    }

    /// Leave a guarded reduction level. The peak stands; only the live count falls.
    pub(super) fn leave_level(&mut self) {
        self.depth -= 1;
    }

    /// Charge `cost` against the budget, failing when it cannot be afforded.
    ///
    /// The kernel is not strongly normalizing and does not pretend to be: a non-productive `rec` reduces forever. The budget is what makes every judgment terminate, and it is deterministic — the same program spends the same units on every machine — so exhausting it is a fact about the program, not about the host that checked it.
    ///
    /// [`Cost::STEP`] is the transition; a construction charge is what the same counter spends so that the budget bounds the memory a reduction builds and not only how many times it moves. A saturated cost is refused without being compared, so a size that overflowed while being computed never looks affordable.
    pub(super) fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() {
            return Err(ReduceError::exhausted(self.remaining, cost));
        }

        match self.remaining.checked_sub(cost.get()) {
            Some(remaining) => {
                self.remaining = remaining;
                Ok(())
            }
            None => {
                // The refusal is built from what is in hand before the budget moves — the category, what was left, and what was asked — so no diagnostic path attempts the allocation that was just refused.
                let refusal = ReduceError::exhausted(self.remaining, cost);
                self.remaining = 0;

                Err(refusal)
            }
        }
    }

    /// Restore the full budget for a new judgment, and with it the depth this judgment may reach before paying again.
    pub(super) fn restore_budget(&mut self) {
        self.remaining = self.budget;
        self.peak_depth = 0;
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

    /// Charge a replayed computation's whole consumption at once, or **decline** when it does not fit.
    ///
    /// Reached by the name-keyed `unfold` replay alone. That table outlives a declaration, so a hit whose price varied with what was already in scope would make a verdict depend on check order — and its entries are what make certifying a whole module affordable, so they are not the ones to make free.
    ///
    /// **Declining is not a refusal.** A caller that cannot afford the replay evaluates the body directly under the actual remaining budget instead, which reaches the same first failing charge and advances exactly the identities reached before it — where refusing from the aggregate would manufacture a diagnostic about a total rather than about the charge that could not be paid. So the declining path spends nothing and mints nothing: the budget is checked before either is touched, and an entry that does not fit leaves the counter exactly as it found it.
    pub(super) fn charge(&mut self, replay: Replay) -> Option<Term> {
        let remaining = self.remaining.checked_sub(replay.steps)?;

        self.remaining = remaining;
        self.minted.seed(self.minted.count() + replay.mints);

        Some(replay.reduct)
    }

    /// Replay a remembered computation and spend nothing for it: the recorded identities are minted exactly as a recomputation would have minted them, and the recorded steps are not taken.
    ///
    /// It cannot fail, and that is the whole of what it concedes — a term-keyed hit can no longer be the point a judgment runs out. Reached by the `whnf`/`forced` tables alone, which [`Memos::begin_declaration`](super::Memos::begin_declaration) clears wherever [`Spend::restore_budget`] fires, so *which* entries are present is a function of the declaration under judgment rather than of the module walk that reached it.
    pub(super) fn charge_nothing(&mut self, replay: Replay) -> Term {
        self.minted.seed(self.minted.count() + replay.mints);

        replay.reduct
    }
}
