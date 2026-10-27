//! The context's doors to the budget a judgment spends, the trace of what it asks, and the posture it stands in.

use {
    crate::{Answered, Context, Question},
    curios_core::{Consumption, Cost, Free, Global, Minted, ReduceError, Trace},
    curios_utilities::Qualifier,
    std::rc::Rc,
};

impl<'s> Context<'s> {
    /// What the attempt under way asked of the declarations around it, taken: its trace, which is its declaration's where the attempt stands.
    pub(crate) fn take_trace(&mut self) -> Trace<Question, Answered> {
        self.typing.traced.take()
    }

    /// Note that the judgment under way asked `question` of the declaration `name` names, where `name` is a declaration's and not a binder's: the part asked is the declaration's to hold.
    pub(super) fn asks(&self, name: &Free, question: fn(Global) -> Question) {
        if let Free::Global(global) = name {
            self.typing.traced.note(question(*global), Answered::Part);
        }
    }

    /// Take what a unit's lowering minted, declaration by declaration. Each attempt at a declaration begins its state above what that declaration's lowering minted, and the entry's above the entry's ([`Context::begin_state`]), so no identity either mints is one the other already holds.
    ///
    /// `into_core` mints the binders of every lowered scope, a metavariable for every hole and a level for every written type, and elaboration mints more of each; both draw from one identity space per declaration, so the second source starts above the first. Nothing from another declaration or another unit is in that space: no term the scope replays or the context publishes carries a local or a metavariable.
    pub(crate) fn seed(&mut self, minted: &Rc<Minted>) {
        self.underway.minted = Rc::clone(minted);
    }

    /// [`Spend::spend`](super::Spend::spend).
    pub(crate) fn spend(&self, cost: Cost) -> Result<(), ReduceError> {
        self.typing.spend.spend(cost)
    }

    /// [`Spend::enter_level`](super::Spend::enter_level).
    pub(crate) fn enter_level(&self) -> Result<(), ReduceError> {
        self.typing.spend.enter_level()
    }

    /// [`Spend::leave_level`](super::Spend::leave_level).
    pub(crate) fn leave_level(&self) {
        self.typing.spend.leave_level();
    }

    /// Restore the full budget for a new declaration ([`Spend::restore`](super::Spend::restore)), and forget what was memoized under the one it closes: nothing a budget paid for outlives it.
    pub(crate) fn restore_budget(&mut self) {
        self.typing.spend.restore();
        self.typing.caches.begin_declaration();
    }

    /// The heaviest single judgment this context has made, the one in progress included: a declaration's elaboration, or one of the judgments of its final terms, each of which spends a budget restored for it (`Context::restore_budget`).
    ///
    /// The counterpart of `curios-cert`'s `Kernel::heaviest_declaration` on the other side of the seam — the two are deliberately the same shape, because comparing them is the point. An observation for a measurement; nothing in elaboration reads it.
    pub fn heaviest_declaration(&self) -> Consumption {
        self.typing.spend.heaviest()
    }

    /// The module whose item is currently being elaborated (the qualifier prefix of its name; empty for the root), or `None` when no surface item is being elaborated — which suppresses the representation-privacy checks (see the field's invariant).
    pub(crate) fn island(&self) -> Option<&Qualifier> {
        self.underway.posture.island()
    }

    /// Set the current module before elaborating an item (see `elaborate_module_suffix`).
    pub(crate) fn set_island(&mut self, island: Qualifier) {
        // Every item boundary also lands a `define_entry` clear, but this one keeps the cache's soundness independent of that ordering.
        self.typing.caches.invalidate_for_island_change();
        self.underway.posture.stand(Some(island));
    }

    /// Run `f` with no island — suppressing the representation-privacy checks for re-derivation of already-elaborated terms, whose machinery-built projections were never subject to surface privacy in the first place. The bracket is the only way to clear an island (mirroring the parking half of the oracle package), so no context can be left permanently altered.
    pub(crate) fn with_suppressed_privacy<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.underway.posture.stand(None);
        let result = f(self);
        self.underway.posture.stand(previous);

        result
    }

    /// Whether the procedure that proves a bound from the facts in scope is running, so it is not asked again from inside its own candidate.
    pub(crate) fn entailing(&self) -> bool {
        self.underway.posture.entailing()
    }

    /// Run `f` as that procedure, which is not asked again until `f` returns.
    pub(crate) fn with_entailing<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = self.underway.posture.set_entailing(true);
        let result = f(self);
        self.underway.posture.set_entailing(previous);

        result
    }
}
