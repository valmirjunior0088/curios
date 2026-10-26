//! What a judgment writes to the universe solver while a declaration elaborates: the constraints a comparison raises, a scheme's instance at an occurrence, a fresh level or sort. Each write that moves the solver advances the elaboration cache's universe stamp, since an elaboration certified pure against the solver as it stood no longer is.
//!
//! Closing a declaration's levels into its scheme is none of these. The context alone holds the solver, so it alone closes them, beside the caches that closing rewrites.

#[cfg(test)]
mod test_support;

use {
    crate::{Occurrence, UniverseSolver, UniverseStateToken},
    curios_core::{
        Extent, Level, Quality, Term, UniverseConstraintKind, UniverseConstraintOrigin,
        UniverseContext, UniverseError, UniverseRole, Universes,
    },
    curios_utilities::Entropy,
};

/// The universe solver borrowed for one judgment's writes.
pub struct UniverseMutation<'a> {
    solver: &'a mut UniverseSolver,
    stamp: &'a Entropy,
    before: UniverseStateToken,
}

impl<'a> UniverseMutation<'a> {
    pub fn new(solver: &'a mut UniverseSolver, stamp: &'a Entropy) -> Self {
        let before = solver.state_token();
        Self {
            solver,
            stamp,
            before,
        }
    }

    pub fn add_eq(
        &mut self,
        left: Level,
        right: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        self.solver.add_eq(left, right, origin)
    }

    pub fn add_leq(
        &mut self,
        lower: Level,
        upper: Level,
        origin: UniverseConstraintOrigin,
    ) -> Result<(), UniverseError> {
        self.solver.add_leq(lower, upper, origin)
    }

    pub fn add_weak_eq(&mut self, left: Level, right: Level) {
        self.solver.add_weak_eq(left, right);
    }

    pub fn settle_leq(
        &mut self,
        lower: Level,
        upper: Level,
        origin: UniverseConstraintOrigin,
    ) -> bool {
        self.solver.settle_leq(lower, upper, origin)
    }

    pub fn unify_qualities(&mut self, this: Quality, that: Quality, site: Option<Extent>) -> bool {
        self.solver.unify_qualities(this, that, site)
    }

    pub fn note_quality_join(&mut self, this: Quality, that: Quality, site: Option<Extent>) {
        self.solver.note_quality_join(this, that, site);
    }

    pub fn instantiate(
        &mut self,
        context: &UniverseContext,
        occurrence: Option<Occurrence>,
    ) -> Result<Universes, UniverseError> {
        self.solver.instantiate(context, occurrence)
    }

    pub fn instantiate_at(
        &mut self,
        context: &UniverseContext,
        universes: &Universes,
        occurrence: Option<Occurrence>,
    ) -> Result<(), UniverseError> {
        self.solver.instantiate_at(context, universes, occurrence)
    }

    /// A fresh level standing for `beside` at an occurrence of its own ([`UniverseSolver::fresh_beside`]).
    pub fn fresh_beside(
        &mut self,
        beside: &Level,
        origin: Option<UniverseConstraintOrigin>,
    ) -> Result<Level, UniverseError> {
        self.solver.fresh_beside(beside, origin).map(Level::meta)
    }

    /// The sort of a type left to inference: its quality and its level each a metavariable of its own ([`UniverseSolver::fresh_quality`]), so the first sort it is held to decides whether it is a proposition's or a type's.
    pub fn fresh_sort(&mut self, kind: &str) -> Term {
        let origin = UniverseConstraintOrigin::new(UniverseConstraintKind::Other(kind.into()));
        let quality = self.solver.fresh_quality(Some(origin.clone()));
        let level = self.solver.fresh(UniverseRole::Flexible, Some(origin));
        Term::sort_at(quality, Level::meta(level))
    }

    /// The sort of a proposition left to inference: [`UniverseMutation::fresh_sort`] with its quality stated, for a part of what is known a proposition — every field of a record of propositions is one.
    pub fn fresh_prop_sort(&mut self, kind: &str) -> Term {
        let origin = UniverseConstraintOrigin::new(UniverseConstraintKind::Other(kind.into()));
        let level = self.solver.fresh(UniverseRole::Flexible, Some(origin));
        Term::sort_at(Quality::Prop, Level::meta(level))
    }
}

impl Drop for UniverseMutation<'_> {
    fn drop(&mut self) {
        stamp_if_moved(self.solver, self.before, self.stamp);
    }
}

/// Advance `stamp` where `solver` no longer stands as it did at `before`: a normalized ground or reflexive comparison reads as no write, so a pure elaboration does not look impure for having asked one.
pub fn stamp_if_moved(solver: &UniverseSolver, before: UniverseStateToken, stamp: &Entropy) {
    if solver.state_token() != before {
        stamp.fresh();
    }
}
