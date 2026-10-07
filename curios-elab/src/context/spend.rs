//! What a judgment may spend.
//!
//! Reduction is bounded by a budget, and the budget is a declaration's: it is restored in full at every declaration boundary, so whether a declaration is accepted is a fact about the declaration and never about how much the ones before it spent. Counting work rather than elapsed time is what makes the answer a fact about the program instead of about the machine that ran it. The [`Spend`] is that budget with what has been drawn from it: the work left, the depth reduction has reached, and the heaviest judgment made so far, which a measurement reads.

use {
    curios_core::{Consumption, Cost, ReduceError},
    std::cell::Cell,
};

/// One context's budget and what its judgments have drawn from it.
#[derive(Debug)]
pub(crate) struct Spend {
    /// Units of reduction work each declaration may spend, restored by [`Spend::restore`] at every declaration boundary. A transition costs one; a construction costs what it builds, per [`Cost`].
    budget: u64,
    /// Work left in the current declaration's budget. `Cell` because the conversion queue spends through a shared borrow.
    remaining: Cell<u64>,
    /// How many guarded reduction levels are live, and the deepest this declaration has reached. `Cell` for `remaining`'s reason; see [`Spend::enter_level`].
    depth: Cell<usize>,
    peak_depth: Cell<usize>,
    /// The heaviest declaration elaborated so far, for a measurement to read. See [`Consumption`]; nothing in elaboration consults it.
    heaviest: Cell<Consumption>,
}

/// A cap on what is in reach ([`Spend::cap`]), to be lifted by [`Spend::lift`].
pub(crate) struct Capped {
    /// What was left before the cap.
    before: u64,
    /// What the cap left in reach: the lesser of that and the allowance.
    granted: u64,
    allowance: u64,
}

impl Capped {
    /// Whether the declaration's own remainder was what bound the work, the allowance being no lower: its exhaustion is then the declaration's verdict and not the cap's.
    pub(crate) fn bound_by_the_remainder(&self) -> bool {
        self.before <= self.allowance
    }
}

impl Spend {
    /// A budget under which each declaration may spend `budget` units.
    pub(crate) fn new(budget: u64) -> Self {
        Self {
            budget,
            remaining: Cell::new(budget),
            depth: Cell::new(0),
            peak_depth: Cell::new(0),
            heaviest: Cell::new(Consumption::default()),
        }
    }

    /// Charge `cost` against the current declaration's budget, failing when it cannot be afforded.
    ///
    /// [`Cost::STEP`] at the three loops that drive reduction and conversion is what makes the budget bound every route into unbounded computation; a construction charge at every allocating fold is what makes it bound the *memory* those routes reach, which a transition count could not see.
    ///
    /// A saturated cost is refused without being compared, so a size that overflowed while being computed can never look affordable. That is the one case where the budget is not consulted at all, and [`Cost`]'s module documentation carries why.
    pub(crate) fn spend(&self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() {
            return Err(ReduceError::exhausted(self.remaining.get(), cost));
        }

        match self.remaining.get().checked_sub(cost.get()) {
            Some(remaining) => {
                self.remaining.set(remaining);
                Ok(())
            }
            None => {
                // Built before the budget moves, from bounded metadata alone — see `curios-cert`'s `Spend::spend`, which does the same thing for the same reason.
                let refusal = ReduceError::exhausted(self.remaining.get(), cost);
                self.remaining.set(0);

                Err(refusal)
            }
        }
    }

    /// Enter one guarded reduction level, charging [`Cost::FRAME`] when it is deeper than any level this declaration has reached before.
    ///
    /// Per new peak rather than per call, for the reason `curios-cert`'s `Spend::enter_level` states in full: a level's native frame is reclaimed when the level returns, and reduction re-enters itself once per operand and once per spine link, so charging every call would price a stack the reduction is not holding. The kernel charges the same row the same way, which is what lets the two checkers' depth limits be compared.
    pub(crate) fn enter_level(&self) -> Result<(), ReduceError> {
        let depth = self.depth.get() + 1;
        self.depth.set(depth);

        if depth > self.peak_depth.get() {
            self.peak_depth.set(depth);
            self.spend(Cost::FRAME)?;
        }

        Ok(())
    }

    /// Leave a guarded reduction level. The peak stands; only the live count falls.
    pub(crate) fn leave_level(&self) {
        self.depth.set(self.depth.get() - 1);
    }

    /// Restore the full budget for a new declaration, and with it the depth this declaration may reach before paying again.
    ///
    /// The live count is reset for the reason `curios-cert`'s `Spend::restore_budget` states in full: [`Spend::enter_level`] increments before it charges and propagates the refusal, so an exhausted level is never left, and elaboration continues to the next declaration. A leaked level costs every later declaration [`Cost::FRAME`] for a frame nothing holds.
    ///
    /// **What the declaration it closes consumed is sampled under `profile`**, as `budget::consumed` — the profile-side reading of [`Spend::heaviest`], whose `max` in a fold is that figure for whatever the profiled run elaborated, and whose distribution says whether it is one declaration or many near it — with `term::looks` beside it, the nodes its walks looked at, as `curios-cert`'s `Spend::restore_budget` samples the pair.
    pub(crate) fn restore(&self) {
        curios_profile::sample!("budget::consumed", self.consumed().units());
        curios_profile::sample!("term::looks", curios_core::take_looks());
        self.heaviest
            .set(self.heaviest.get().heavier_of(self.consumed()));

        self.remaining.set(self.budget);
        self.depth.set(0);
        self.peak_depth.set(0);
    }

    /// What the declaration being elaborated has consumed so far.
    pub(crate) fn consumed(&self) -> Consumption {
        Consumption::new(self.budget - self.remaining.get(), self.peak_depth.get())
    }

    /// The heaviest single judgment this context has made, the one in progress included: a declaration's elaboration, or one of the judgments of its final terms, each of which spends a budget restored for it.
    ///
    /// The counterpart of `curios-cert`'s `Kernel::heaviest_declaration` on the other side of the seam — the two are deliberately the same shape, because comparing them is the point. An observation for a measurement; nothing in elaboration reads it.
    pub(crate) fn heaviest(&self) -> Consumption {
        self.heaviest.get().heavier_of(self.consumed())
    }

    /// Leave at most `allowance` units of this declaration's budget in reach until the cap is lifted ([`Spend::lift`]).
    pub(crate) fn cap(&self, allowance: u64) -> Capped {
        let before = self.remaining.get();
        let granted = before.min(allowance);
        self.remaining.set(granted);

        Capped {
            before,
            granted,
            allowance,
        }
    }

    /// Lift `capped`, putting back what it withheld. **What is spent is spent**: the work done under the cap stays charged, so a cap is a ceiling and never free work.
    pub(crate) fn lift(&self, capped: &Capped) {
        let spent = capped.granted.saturating_sub(self.remaining.get());
        self.remaining.set(capped.before.saturating_sub(spent));
    }
}
