//! What a judgment consumes: reduction steps, and binder identities minted.
//!
//! The two are one component because replaying a remembered computation has to settle both, and they settle differently. A remembered reduct's hit advances the entropy counter as if every eta probe had run, so it mints exactly the identities a recomputation would have and every later one lands where it would have. The budget does not: a hit is *free*. [`Spend::charge_nothing`] is that replay, and [`Memos`](super::Memos) states why a free hit decides nothing that another declaration left behind.
//!
//! **A hit is free because a charged one compounds.** Charging a hit what a memo-free evaluator would have spent prices work the kernel did not perform, and recorded costs *compound*: if computing `Aₙ` hits the memo for `Aₙ₋₁` twice, `S(n) = 2·S(n−1) + c`, so a structure cheap to evaluate with memos is expensive to charge — a budget declared exhausted after a small fraction of it was actually reduced, and a kernel refusing at many times the budget the elaborator, whose hits are free too, needs for the same program. `curios`' `kernel_memo_charge_measurements` takes the two checkers' floors.
//!
//! **What is given up is exactly one thing: exhaustion points.** A free hit can only *reduce* what a judgment spends, so it can only turn a refusal into an acceptance, and only an exhaustion one — a semantic refusal is budget-independent, and an exhaustion masking a type error reaches that type error with more budget and refuses anyway. So the invariant is **"memos change only resource verdicts, never semantic ones"**, and it keeps refusal payloads for semantic errors and, up to the caveat below, later-minted identities. `kernel_memo_parity` is what holds that half to account, and `Kernel::uncached` is what lets it be asked.
//!
//! **The table of inferred types gives up one thing more, and settles neither.** Its hits spend nothing, as the reducts' do, and mint nothing, which theirs do not: the table exists to type a graph once per node, and the identities a recomputation would mint are counted per path, which outgrows the identity space on exactly the graphs it serves — [`Kernel::infer_hit`](super::Kernel::infer_hit) has the measurement. So an identity minted after one of its hits no longer lands where a memo-free run's would, and a refusal naming a binder the kernel minted may name it differently with the memos off. Freshness, the one property a judgment reads off an identity, holds regardless, because the counter never falls; so the invariant above stands for verdicts, and the parity of later-minted identities is kept only up to the first such hit.
//!
//! Termination is unaffected, and the reason is structural rather than budgetary: a [`Replay`] is built *after* its reduct exists, so a divergent reduction never completes, never stores, and can never be hit. Every step of one is charged.
//!
//! **Nothing is charged at a recorded price**, because no memo outlives the declaration that filled it — [`Memos`](super::Memos) states why — and `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` carries the rule for both checkers.
//!
//! [`Memos`](super::Memos) deliberately cannot reach this: it stores [`Replay`]s and hands them back, and applying one is this component's job. A store that could also charge would be a store that could charge twice.

use {
    curios_core::{Consumption, Cost, Free, ReduceError, Term},
    curios_utilities::Entropy,
};

/// One remembered reduction: the reduct, and the identities computing it minted, so that every hit mints what a recomputation would have.
#[derive(Debug, Clone)]
pub(crate) struct Replay {
    pub(super) reduct: Term,
    mints: usize,
}

pub(super) struct Spend {
    /// Units of reduction work a single judgment may spend. Restored at each declaration boundary by [`Spend::restore_budget`]. A transition costs one; a construction costs what it builds.
    budget: u64,
    remaining: u64,
    /// How many guarded reduction levels are live, and the deepest this judgment has reached. See [`Spend::enter_level`].
    depth: usize,
    peak_depth: usize,
    /// The heaviest declaration walked so far, for a measurement to read. See [`Consumption`]; nothing in the kernel consults it.
    heaviest: Consumption,
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
            heaviest: Consumption::default(),
            minted: Entropy::new(),
        }
    }

    /// Enter one guarded reduction level, charging [`Cost::FRAME`] when it is deeper than any level this judgment has reached before.
    ///
    /// **Per new peak, not per call**, and the difference is the whole of what makes this row mean anything. A level's native frame is reclaimed when the level returns, so charging every `whnf` call would price a stack the reduction is not holding — and reduction calls itself once per operand of a nested intrinsic and once per link of a spine, so a wide, shallow computation would pay for depth it never reached. A high-water mark charges exactly the stack the reduction *peaks* at, which is the resource the row exists to bound, and it never refunds: the mark only rises.
    ///
    /// The two alternatives are both worse. Charging when `recurse` actually grows would price the 32 MiB segment exactly, and makes acceptance depend on the host thread's stack size — a two-megabyte test thread grows on its first call and an eight-megabyte main thread does not. Charging nothing leaves depth bounded by the host's stack alone, which is what this row exists to replace.
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
    ///
    /// **The live count is reset here too, and it has to be.** [`Spend::enter_level`] increments before it charges and propagates the refusal with `?`, so the level whose frame could not be paid is never left; and the module walk continues past a refused item to judge the next one. Without this line a module that refuses once charges every later declaration [`Cost::FRAME`] for a level nothing is holding, and the leak accumulates per refusal — a cost that is a fact about what failed earlier rather than about the declaration under judgment, which is the one thing the per-declaration budget exists to prevent.
    ///
    /// **What the judgment it closes consumed is sampled under `profile`**, as `budget::consumed` — the profile-side reading of [`Spend::heaviest`], which a fold reports as that sample's `max` beside the elaborator's under the same name — with the nodes its walks looked at beside it, as `term::looks`: the two are sampled at one point, so a declaration whose looks outgrow its units is one a walk paid per path for.
    pub(super) fn restore_budget(&mut self) {
        curios_profile::sample!("budget::consumed", self.consumed().units());
        curios_profile::sample!("term::looks", curios_core::take_looks());
        self.heaviest = self.heaviest.heavier_of(self.consumed());

        self.remaining = self.budget;
        self.depth = 0;
        self.peak_depth = 0;
    }

    /// What the declaration under judgment has consumed so far.
    fn consumed(&self) -> Consumption {
        Consumption::new(self.budget - self.remaining, self.peak_depth)
    }

    /// The heaviest declaration this kernel has walked, including the one under judgment.
    ///
    /// A measurement's read, never a control — see [`Consumption`]. It reports the heaviest rather than a sum because the budget is per declaration, which makes the heaviest the figure a default has to clear.
    pub(super) fn heaviest(&self) -> Consumption {
        self.heaviest.heavier_of(self.consumed())
    }

    /// A fresh binder identity, rendering as `hint`.
    pub(super) fn fresh(&self, hint: Option<&str>) -> Free {
        let index = u32::try_from(self.minted.fresh()).expect("binder space exhausted");

        Free::local(index, hint)
    }

    /// Keep every binder minted from now on clear of `index`: one handed in from outside, which the counter would otherwise reach and hand out again — and an alias between two distinct binders is a capture.
    pub(super) fn reserve(&mut self, index: u32) {
        self.minted.seed(index as usize + 1);
    }

    /// A consumption snapshot, for measuring what a computation charges.
    pub(super) fn snapshot(&self) -> (u64, usize) {
        (self.remaining, self.minted.count())
    }

    /// The [`Replay`] for `reduct`, measured against the snapshot taken before the computation ran. Only the identities are recorded: a hit spends no steps, so the steps a computation took are nothing a replay needs.
    pub(super) fn replay_since(&self, reduct: Term, (_, minted): (u64, usize)) -> Replay {
        Replay {
            reduct,
            mints: self.minted.count() - minted,
        }
    }

    /// Replay a remembered computation and spend nothing for it: the recorded identities are minted exactly as a recomputation would have minted them, and no steps are taken.
    ///
    /// It cannot fail, and that is the whole of what it concedes — a hit is never the point a judgment runs out. Reached by every reduct table, all of which [`Memos::begin_declaration`](super::Memos::begin_declaration) clears wherever [`Spend::restore_budget`] fires, so *which* entries are present is a function of the declaration under judgment rather than of the module walk that reached it.
    pub(super) fn charge_nothing(&mut self, replay: Replay) -> Term {
        self.minted.seed(self.minted.count() + replay.mints);

        replay.reduct
    }
}
