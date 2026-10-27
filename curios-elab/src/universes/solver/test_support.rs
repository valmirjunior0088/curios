//! What the suites read of the universe solver and ask of it that no judgment does: its constraints and open scopes, minimization over the whole store, and an origin to raise a constraint at.

use {super::*, std::collections::BTreeSet};

/// The test surface over the solver's private state, kept beside the tests rather than gated inside the production module.
impl UniverseSolver {
    /// How many constraints the store holds — none between declarations, which is what a refused item's boundary must leave.
    pub fn constraint_count(&self) -> usize {
        self.constraints.len()
    }

    pub fn constraints(&self) -> &[UniverseConstraint] {
        self.constraints.as_slice()
    }

    pub fn open_scopes(&self) -> usize {
        self.constraints.speculation()
    }

    pub fn journal_len(&self) -> usize {
        self.constraints.journal_len()
    }

    /// Minimize flexible metas from their current lower bounds. Repeating to a fixpoint handles classifier chains; unconstrained flexible metas become zero. Generalizable metas are left for declaration finalization. Production reaches minimization through [`UniverseSolver::solve_flexible_in`]; this whole-store form exists for these tests.
    pub fn solve_flexible(&mut self) -> Result<(), UniverseError> {
        let metas = (0..self.metas.len())
            .map(UniverseMetaId)
            .collect::<BTreeSet<_>>();
        self.solve_flexible_in(&metas)
    }
}

pub fn origin(label: &str) -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Other(label.into()))
}
