//! What one attempt at a declaration solves.
//!
//! A declaration is elaborated in a state no other declaration wrote: its metavariables with their solutions, its universe levels with the solver that settles them, and what it noted of the goals it wrote, each counting from what the declaration's own lowering minted (`curios_core::Minted`). What a declaration elaborates to then follows nothing of the declarations elaborated before it — neither how many there were nor what they left solved — and every other declaration is read as it was published, which holds none of these. The context elaborates in one [`Attempt`] at a time. An [`Attempted`] is one set aside, with the binder identities it minted beside it: the one a declaration wrote goals in is kept past the declaration, since the goals are known in it alone.

use {
    super::Solutions,
    crate::UniverseSolver,
    curios_core::{Free, MetavarId, MetavarOrigin, Mints, Term},
    curios_utilities::{Entropy, Span},
    std::collections::{BTreeMap, BTreeSet},
};

/// A conversion the item drain surrendered to the written goals holding it up: the goals, and the two sides as a report displays them.
#[derive(Debug, Clone)]
pub(crate) struct GoalObligation {
    pub goals: BTreeSet<MetavarId>,
    pub this: Term,
    pub that: Term,
}

/// The metavariables one attempt at a declaration mints with their solutions, its universe levels with the solver that settles them, and what it noted of the goals it wrote.
#[derive(Debug)]
pub(crate) struct Attempt {
    // The unification state — metavariable records, the solve journal, and parked work; flat and frame-independent. See [`Solutions`]. Open to the context with the solver beside it, since a write to either stamps or clears a cache and the context is where the two are named together.
    pub(super) solutions: Solutions,
    pub(super) universe_solver: UniverseSolver,
    // Conversions the item drain gave up on because written goals alone held them up — what each such `?` must make true, carried to the goal batch rather than reported as an error. See [`Attempt::note_goal_obligation`].
    goal_obligations: Vec<GoalObligation>,
    // Where each written goal `?` was written, recorded as it is born: a report that names a goal names it by this span, since an occurrence of the goal inside a type may carry the span of the binder it was substituted for — a declaration's, not the `?`'s.
    goal_spans: BTreeMap<MetavarId, Span>,
    /// The member name each recursive slot stands for.
    ///
    /// A slot is the placeholder a `rec` group's member is known by while its body is being checked, and `elaborate_rec` defines the member's *name* to it — so reduction turns a recursive reference into the slot, and a committed solution can carry one. `RecItem::try_new` captures member **names** into the group's binder, so a slot reaching that point is not something the capture can bind: zonk expands it to the member's body instead, the body mentions that solution again, and the walk never ends. Knowing the name lets both substitution walks spell the member as the capturable thing it is.
    rec_slot_names: BTreeMap<MetavarId, Free>,
    /// Why a solve could not commit: the unsolved metavariables `Convert::solve`'s embedded-metavariable guard found riding inside a candidate, with what each one fills and where it was written. The last attempt wins, so a metavariable that never solves leaves the reason it never did.
    ///
    /// This edge is what lets the item drain report the *cause* of an undecided conversion rather than the goal that merely waited on it. An undischarged bound riding inside a helper's unfolded body blocks every metavariable downstream of it, and without the edge the report names the last of them — an implicit the author never wrote, at a line that is not where the fault is — while the bound nothing discharged goes unmentioned entirely.
    solve_blockers: BTreeMap<MetavarId, Vec<(MetavarId, MetavarOrigin, Option<Span>)>>,
}

impl Default for Attempt {
    /// An attempt that has minted nothing, above a lowering that minted nothing: what a context holds before its first declaration, and after one is set aside.
    fn default() -> Self {
        Self::above(&Mints::default())
    }
}

impl Attempt {
    /// An attempt that has minted nothing, each of its counters above what `mints` says a lowering minted.
    fn above(mints: &Mints) -> Self {
        let mut solutions = Solutions::new();
        solutions.seed_floor(mints.metavariables);
        let mut universe_solver = UniverseSolver::new(0);
        universe_solver.seed(&mints.universes);

        Self {
            solutions,
            universe_solver,
            goal_obligations: Vec::new(),
            goal_spans: BTreeMap::new(),
            rec_slot_names: BTreeMap::new(),
            solve_blockers: BTreeMap::new(),
        }
    }

    /// Record what blocked `id`'s solve, replacing any earlier record: only the last attempt's reason is worth reporting, since an earlier one may since have been solved away.
    pub(crate) fn note_solve_blockers(
        &mut self,
        id: MetavarId,
        blockers: Vec<(MetavarId, MetavarOrigin, Option<Span>)>,
    ) {
        self.solve_blockers.insert(id, blockers);
    }

    /// What blocked `id`'s solve when it was last tried; nothing where it was never blocked.
    pub(crate) fn solve_blockers(
        &self,
        id: MetavarId,
    ) -> &[(MetavarId, MetavarOrigin, Option<Span>)] {
        self.solve_blockers.get(&id).map_or(&[], Vec::as_slice)
    }

    /// Record where the written goal `id` was written, at its birth.
    pub(crate) fn note_goal_span(&mut self, id: MetavarId, span: Span) {
        self.goal_spans.entry(id).or_insert(span);
    }

    /// Where the written goal `id` was written, if elaboration reached it.
    pub(crate) fn goal_span(&self, id: MetavarId) -> Option<&Span> {
        self.goal_spans.get(&id)
    }

    /// Whether the attempt wrote a goal.
    pub(crate) fn holds_goals(&self) -> bool {
        !self.goal_spans.is_empty()
    }

    /// Record a conversion the drain dropped because the written goals in `goals` were all that held it up, with its two sides already in display form. A program holding a goal never compiles, so nothing is lost by not deciding it; what is kept is the constraint the hole must satisfy, which the goal's report shows beside its type.
    pub(crate) fn note_goal_obligation(
        &mut self,
        goals: BTreeSet<MetavarId>,
        this: Term,
        that: Term,
    ) {
        self.goal_obligations
            .push(GoalObligation { goals, this, that });
    }

    /// Every obligation [`Attempt::note_goal_obligation`] recorded, in drain order.
    pub(crate) fn goal_obligations(&self) -> &[GoalObligation] {
        &self.goal_obligations
    }

    /// Record that the recursive slot `id` stands for the member `name`.
    pub(crate) fn name_rec_slot(&mut self, id: MetavarId, name: Free) {
        self.rec_slot_names.insert(id, name);
    }

    /// The member name a *filled* recursive slot stands for, which is what substitution must put in its place.
    ///
    /// Only once filled. While a slot is unsolved it is deliberately a blocking dependency — `elaborate_rec` records that a dependency on a later member parks on the unsolved slot — and spelling it as a name would let a conversion commit where it must still wait. Nothing is lost by waiting: the cycle needs substitution to expand a slot into a body, so refusing to do that at all is enough however early the slot leaked.
    pub(crate) fn rec_slot_name(&self, id: MetavarId) -> Option<&Free> {
        match self.solutions.solution(id).is_some() {
            true => self.rec_slot_names.get(&id),
            false => None,
        }
    }
}

/// An [`Attempt`] set aside, with the binder identities it minted: what `Context::exchange` takes and hands back.
#[derive(Debug)]
pub(crate) struct Attempted {
    pub(super) fresh_names: Entropy,
    pub(super) attempt: Attempt,
}

impl Attempted {
    /// A state that has minted nothing, each of its counters above what `mints` says a lowering minted.
    pub(super) fn above(mints: &Mints) -> Self {
        let fresh_names = Entropy::<usize>::new();
        fresh_names.seed(mints.binders);

        Self {
            fresh_names,
            attempt: Attempt::above(mints),
        }
    }
}
