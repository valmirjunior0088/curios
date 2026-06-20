use super::{Quantity, Telescope, Term};

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
    /// One erasure quantity per *field* binder (not the leading parameter
    /// binders), aligned with the field-only telescope `fields_at` returns. A
    /// `Zero` field is dropped at erasure — `erase` consults this to lower a
    /// proof-carrying record to its relevant fields (and to a bare field, via the
    /// single-field collapse, when only one remains).
    pub field_quantities: Vec<Quantity>,
    /// The declaring module's joined qualified name (e.g. `Foo/Bar`); the root
    /// module is the empty string. Compared against the use-site module for the
    /// representation-privacy checks (§7).
    pub module: String,
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
        let mut telescope = self.fields.clone();

        for param in params {
            telescope = match telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => return telescope,
            };
        }

        telescope
    }
}
