//! What one attempt at a declaration solves.
//!
//! A declaration is elaborated in a state no other declaration wrote: its metavariables with their solutions, its universe levels with the solver that settles them, and what it noted of the goals it wrote, each counting from what the declaration's own lowering minted (`curios_core::Minted`). What a declaration elaborates to then follows nothing of the declarations elaborated before it — neither how many there were nor what they left solved — and every other declaration is read as it was published, which holds none of these. The context elaborates in one [`Attempt`] at a time. An [`Attempted`] is one set aside, with the binder identities it minted beside it: the one a declaration wrote goals in is kept past the declaration, since the goals are known in it alone.

use {
    super::Solutions,
    crate::UniverseSolver,
    curios_core::{Extent, MetavarId, Mints, Term},
    curios_utilities::Entropy,
    std::collections::{BTreeMap, BTreeSet},
};

/// A conversion the item drain surrendered to the written goals holding it up: the goals, and the two sides as a report displays them.
#[derive(Debug, Clone)]
pub struct GoalObligation {
    pub goals: BTreeSet<MetavarId>,
    pub this: Term,
    pub that: Term,
}

/// The metavariables one attempt at a declaration mints with their solutions, its universe levels with the solver that settles them, and what it noted of the goals it wrote: each a store of its own, open to the context, since a write to one stamps or clears a cache and the context is where they are named together.
#[derive(Debug)]
pub struct Attempt {
    pub solutions: Solutions,
    pub universe_solver: UniverseSolver,
    pub goals: Goals,
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
        universe_solver.seed_qualities(&mints.qualities);

        Self {
            solutions,
            universe_solver,
            goals: Goals::default(),
        }
    }
}

/// What an attempt noted of the goals `?` it wrote: where each was written, and the conversions the item drain surrendered to them.
#[derive(Debug, Default)]
pub struct Goals {
    // Where each written goal `?` was written, recorded as it is born: a report that names a goal names it by this extent, since an occurrence of the goal inside a type may carry the extent of the binder it was substituted for — a declaration's, not the `?`'s.
    spans: BTreeMap<MetavarId, Extent>,
    // Conversions the item drain gave up on because written goals alone held them up — what each such `?` must make true, carried to the goal batch rather than reported as an error. See [`Goals::note_obligation`].
    obligations: Vec<GoalObligation>,
}

impl Goals {
    /// Record where the written goal `id` was written, at its birth.
    pub fn note_extent(&mut self, id: MetavarId, extent: Extent) {
        self.spans.entry(id).or_insert(extent);
    }

    /// Where the written goal `id` was written, if elaboration reached it.
    pub fn extent(&self, id: MetavarId) -> Option<&Extent> {
        self.spans.get(&id)
    }

    /// Whether the attempt wrote a goal.
    pub fn any(&self) -> bool {
        !self.spans.is_empty()
    }

    /// Record a conversion the drain dropped because the written goals in `goals` were all that held it up, with its two sides already in display form. A program holding a goal never compiles, so nothing is lost by not deciding it; what is kept is the constraint the hole must satisfy, which the goal's report shows beside its type.
    pub fn note_obligation(&mut self, goals: BTreeSet<MetavarId>, this: Term, that: Term) {
        self.obligations.push(GoalObligation { goals, this, that });
    }

    /// Every obligation [`Goals::note_obligation`] recorded, in drain order.
    pub fn obligations(&self) -> &[GoalObligation] {
        &self.obligations
    }
}

/// An [`Attempt`] set aside, with the binder identities it minted: what `Context::exchange` takes and hands back.
#[derive(Debug)]
pub struct Attempted {
    fresh_names: Entropy,
    attempt: Attempt,
}

impl Attempted {
    pub(super) fn new(fresh_names: Entropy, attempt: Attempt) -> Self {
        Self {
            fresh_names,
            attempt,
        }
    }

    pub(super) fn into_parts(self) -> (Entropy, Attempt) {
        (self.fresh_names, self.attempt)
    }

    /// A state that has minted nothing, each of its counters above what `mints` says a lowering minted.
    pub(super) fn above(mints: &Mints) -> Self {
        let fresh_names = Entropy::new();
        fresh_names.seed(mints.binders);

        Self {
            fresh_names,
            attempt: Attempt::above(mints),
        }
    }
}
