//! What the suites write to the universe solver and no judgment does: a level metavariable minted bare, which a comparison under test starts from.

use {
    super::UniverseMutation,
    curios_core::{UniverseConstraintOrigin, UniverseMetaId, UniverseRole},
};

impl UniverseMutation<'_> {
    /// A level metavariable in `role` that stands for nothing yet.
    pub fn fresh(
        &mut self,
        role: UniverseRole,
        origin: Option<UniverseConstraintOrigin>,
    ) -> UniverseMetaId {
        self.solver.fresh(role, origin)
    }
}
