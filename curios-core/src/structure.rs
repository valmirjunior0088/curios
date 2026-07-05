use {
    super::{Telescope, Term},
    curios_abi::RootId,
};

/// One struct declaration's registry entry: the metadata a `struct`
/// declaration produces alongside its type-former binding.
///
/// A struct is a nominal record — there is no value-constructor function and no
/// tag — so this is an [`Inductive`](super::Inductive) minus the indices and
/// the per-constructor map, plus the privacy metadata the representation
/// boundary needs. Elaboration consults it to check a struct literal's fields
/// and to type a projection; `erase` consults it to lower the fields.
#[derive(Debug, Clone, PartialEq)]
pub struct Structure {
    /// The declaration's parameter telescope, e.g. `(A : Type, B : Type)` for
    /// `struct Pair(A : Type, B : Type)`. Ends in `()` like a `TupleType`'s
    /// telescope: there is no trailing body, only binders.
    pub params: Telescope<()>,
    /// The declaration's *full* field telescope — the parameter binders first
    /// (field types may depend on them), then the field binders, e.g.
    /// `(A : Type, B : Type, fst : A, snd : B)`. Dependent; ends in `()`.
    /// Instantiate at known parameters by peeling the leading `params.len()`
    /// binders, exactly like `Inductive::instantiate`.
    pub fields: Telescope<()>,
    /// The declared result sort — `Type` or `Prop` — the codomain of the
    /// type-former's kind. A fully-applied `StructType { name, .. }` has this
    /// sort, which `sort_of` reads to decide propositional irrelevance.
    pub result_sort: Term,
    /// The declaring module's joined qualified name (e.g. `Foo/Bar`); the root
    /// module is the empty string. Compared against the use-site module for the
    /// representation-privacy checks (§7).
    pub module: String,
    /// The compilation root (`sys`/`syn`/`std`/the entry program/…) that
    /// declares this struct — orthogonal to `module`, which is fine-grained
    /// per-module privacy. Consulted by the orphan-rule ownership check, not
    /// by representation privacy.
    pub root: RootId,
    /// The inner `pub`: whether the representation — construction and
    /// projection — is exported.
    pub rep_public: bool,
}

impl Structure {
    /// Instantiate the field telescope at known parameters, yielding the
    /// field-only telescope: `fields_at([Nat, Bin])` for `Pair` becomes
    /// `(fst : Nat, snd : Bin)`. Peels the leading `params.len()` binders by
    /// opening each with the corresponding parameter — exactly as
    /// `Inductive::instantiate` does for a constructor signature.
    pub fn fields_at(&self, params: &[Term]) -> Telescope<()> {
        self.fields.clone().open_params(params)
    }
}
