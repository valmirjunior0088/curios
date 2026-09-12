//! The registry entry for a concept — a record-shaped interface — carried on the [`Module`](super::Module) beside `induct_decls` and `struct_decls`.
//!
//! A concept lowers to a representation-public nominal structure, whose [`StructDecl`](super::StructDecl) entry drives literals and projections; this entry adds what resolution needs on top: the field labels, the superclass mask, and the parameter telescope. Resolution itself — witness keys, head keys, and the instance search — is elaboration machinery and lives in `curios-elab`.

use {
    super::{Global, Sharing, Telescope, UniverseContext},
    curios_utilities::Plicity,
};

/// One concept declaration's registry entry.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct ConceptDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameter telescope, e.g. `(A : Type)` for `concept Show(A : Type)`. Ends in `()` like a `StructDecl`'s.
    pub params: Telescope<()>,
    /// Field labels in declaration order — the positions witness struct literals fill and method wrappers project.
    pub fields: Vec<String>,
    /// Superclass edges: `(field position, super concept qualified name)` for each `use`-marked field. The graph over all concepts must be acyclic (checked when the registries are seeded).
    pub supers: Vec<(usize, Global)>,
}

impl ConceptDecl {
    /// One mark per field, `Witness` exactly at the superclass edges.
    ///
    /// **The one door onto which fields are `use`-marked.** [`ConceptDecl::supers`] states that fact as positions beside the field list rather than in it, so every reader wanting the marks re-derives the correspondence, and a position list that disagreed with the fields would be found by whichever reader indexed first, if at all. `curios-elab`'s `check_concept_registry` checks the positions against `fields` where the registries are seeded, beside the acyclicity already checked there; this derives the marks from what it checked, so no telescope walk carries its own idea of which entries are edges.
    pub fn field_plicities(&self) -> Vec<Plicity> {
        let mut plicities = vec![Plicity::Explicit; self.fields.len()];

        for (position, _) in &self.supers {
            plicities[*position] = Plicity::Witness;
        }

        plicities
    }

    /// This concept with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub(crate) fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.clone(),
            params: sharing.share(&self.params),
            fields: self.fields.clone(),
            supers: self.supers.clone(),
        }
    }
}
