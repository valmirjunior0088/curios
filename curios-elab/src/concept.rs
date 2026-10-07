//! Witness registry entries and the keys resolution looks them up by.
//!
//! The [`ConceptDecl`](curios_core::ConceptDecl) entry itself is representation and lives in `curios-core` beside the other registry entries; what stays here is the instance-argument machinery: a [`Witness`] keys an ordinary top-level definition in the program-wide table under `(concept name, tuple of parameter heads)` — the [`WitnessKey`](curios_core::WitnessKey) of [`HeadKey`](curios_core::HeadKey)s — and resolution searches that table. A witness a unit declares is known by that key before it elaborates: its name states it ([`WitnessId`](curios_core::WitnessId)).

use {
    curios_core::{Global, Term, UniverseContext},
    curios_utilities::Qualifier,
};

/// One registered witness: the qualified name of its backing definition and that definition's elaborated type `∀ tele. C(t₁, …)`. Resolution instantiates the telescope fresh at every use.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub(crate) struct Witness {
    pub name: Global,
    /// The module this witness was declared in, carried from its definition's `island`. Witnesses are anonymous, so this — not `name` — is the coordinate a coherence diagnostic reports; recovering it by splitting the compiler-minted `name` would re-derive what the declaration already knew.
    pub module: Qualifier,
    pub universe_context: UniverseContext,
    pub signature: Term,
}
