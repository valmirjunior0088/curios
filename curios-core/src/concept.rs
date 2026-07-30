//! The registry entry for a concept — a record-shaped interface — carried on the [`Module`](super::Module) beside `induct_decls` and `struct_decls`.
//!
//! A concept lowers to a representation-public nominal structure, whose [`StructDecl`](super::StructDecl) entry drives literals and projections; this entry adds what resolution needs on top: the field labels, the superclass mask, and the parameter telescope. Resolution itself — witness keys, head keys, and the instance search — is elaboration machinery and lives in `curios-elab`.

use {
    super::{Global, Sharing, Telescope, UniverseContext},
    curios_base::RootId,
};

/// One concept declaration's registry entry.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Concept {
    pub universe_context: UniverseContext,
    /// The declaration's parameter telescope, e.g. `(A : Type)` for `concept Show(A : Type)`. Ends in `()` like a `StructDecl`'s.
    pub params: Telescope<()>,
    /// Field labels in declaration order — the positions witness struct literals fill and method wrappers project.
    pub fields: Vec<String>,
    /// Superclass edges: `(field position, super concept qualified name)` for each `use`-marked field. The graph over all concepts must be acyclic (checked when the registries are seeded).
    pub supers: Vec<(usize, Global)>,
    /// The compilation root that declares this concept — consulted by the orphan-rule ownership check in `register_witness`.
    pub root: RootId,
}

impl Concept {
    /// This concept with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub(crate) fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.clone(),
            params: sharing.share(&self.params),
            fields: self.fields.clone(),
            supers: self.supers.clone(),
            root: self.root,
        }
    }
}
