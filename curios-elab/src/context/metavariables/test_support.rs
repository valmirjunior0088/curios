//! How a fixture states a metavariable, which no judgment does: over a telescope stated for it rather than the one where the context stands.

use {
    crate::{BirthTelescope, Context, Scoped, context::MetaKind},
    curios_core::{MetavarId, Term},
};

impl Context<'_> {
    /// Materialize a metavariable's birth record over a telescope stated for it ([`Solutions::birth`](crate::context::Solutions::birth), [`Scoped::Stated`]), under the frames that stand, stamping the write: how a fixture states its metavariables, where elaboration births one where it stands ([`Context::birth_here`]).
    pub fn birth_metavar(
        &mut self,
        id: MetavarId,
        telescope: impl Into<BirthTelescope>,
        result: Term,
    ) {
        self.birth_metavar_as(
            id,
            telescope.into(),
            Scoped::Stated,
            result,
            MetaKind::Inference,
        );
    }
}
