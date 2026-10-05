//! The registry entry for a concept — a record-shaped interface — carried on the [`Module`](super::Module) beside `induct_decls` and `struct_decls`.
//!
//! A concept lowers to a representation-public nominal structure, whose [`StructDecl`](super::StructDecl) entry drives literals and projections; this entry adds what resolution needs on top: the field labels, what each superclass edge reaches, and the parameter telescope. Resolution itself — witness keys, head keys, and the instance search — is elaboration machinery and lives in `curios-elab`.

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
    /// What each superclass edge reaches: the super concept's qualified name, one per `use` field of the concept's field telescope ([`StructDecl::fields`](super::StructDecl::fields)), in field order. Where an edge stands is its field's mark, which the telescope states; what it reaches is read off the field's elaborated type where the concept's fields are elaborated — so an alias of a concept application is an edge — and is empty until then. The graph over all concepts is acyclic, checked as each concept's edges are recorded.
    pub supers: Vec<Global>,
}

impl ConceptDecl {
    /// Each superclass edge as its position among `fields` — the concept's own field telescope — beside the concept it reaches. A concept whose edges are not recorded yet answers with none.
    pub fn edges(&self, fields: &Telescope<()>) -> Vec<(usize, Global)> {
        fields
            .marks()
            .into_iter()
            .enumerate()
            .filter(|(_, mark)| *mark == Plicity::Witness)
            .map(|(position, _)| position)
            .zip(self.supers.iter().copied())
            .collect()
    }

    /// This concept with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub(crate) fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.unplaced(),
            params: sharing.share(&self.params),
            fields: self.fields.clone(),
            supers: self.supers.clone(),
        }
    }
}
