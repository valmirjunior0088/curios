use {
    super::{Atom, Quantity, Telescope, Term},
    std::collections::BTreeMap,
};

/// One constructor's registry signature: the telescope paired with its payload
/// quantities, bundled so the two can never drift (the registry mirror of how
/// [`super::FuncType`] keeps its `quantities` beside its telescope).
///
/// The telescope is the constructor's *full* signature — the parameter binders
/// first, then the payload binders, terminating in the constructed type. E.g.
/// `success ↦ (A : Type, E : Type, _0 : A) -> InductiveType { Result, [A, E] }`.
/// For an indexed inductive the terminal is *per-case*: its indices are that case's
/// target expressions over the payload binders. Instantiating peels the leading
/// `params.len()` binders by opening each with the corresponding parameter.
///
/// `quantities` is aligned with the *payload* binders that remain after
/// `instantiate` peels the parameters (it excludes the parameter binders, which
/// are always relevant); `erase` drops the `Zero` payload fields from the
/// runtime variant tuple.
#[derive(Debug, Clone, PartialEq)]
pub struct InductiveParam {
    pub telescope: Telescope<Term>,
    pub quantities: Vec<Quantity>,
}

/// One inductive declaration's registry entry: the metadata an `induct`
/// declaration produces alongside its type-constructor and value-constructor
/// function bindings.
///
/// The elaborator consults this when checking an inductive match: each arm's
/// binders are typed directly from the matching constructor's telescope
/// (instantiated at the scrutinee type's parameters), and the arm's binder
/// count is arity-checked against that telescope statically. `erase` consults
/// it again to type constructor payloads and order runtime tags.
#[derive(Debug, Clone, PartialEq)]
pub struct Inductive {
    /// The declaration's parameter telescope, e.g. `(A : Type, E : Type)` for
    /// `induct Result(A : Type, E : Type)`. Ends in `()` like a `TupleType`'s
    /// telescope: there is no trailing body, only binders.
    pub params: Telescope<()>,
    /// The declaration's *full* index telescope — the parameter binders first
    /// (index types may depend on them), then the index binders from the
    /// head's `: (...)` group, e.g. `(T : Type, n : Nat)` for
    /// `induct Vec(T : Type) : (n : Nat)`. Empty-beyond-params for an
    /// unindexed inductive. Like `constructors`, instantiate at known parameters
    /// by peeling the leading `params.len()` binders.
    pub indices: Telescope<()>,
    /// Per-constructor signatures, keyed by tag — each the constructor's
    /// signature telescope paired with its payload quantities (see
    /// [`InductiveParam`]).
    pub constructors: BTreeMap<Atom, InductiveParam>,
    /// The declared result sort — `Type` or `Prop` — the codomain of the
    /// type-constructor's kind. A fully-applied `InductiveType { name, .. }`
    /// has this sort, which `sort_of` reads to decide propositional irrelevance.
    pub result_sort: Term,
}

impl Inductive {
    /// Instantiate `tag`'s signature at the given type parameters, yielding the
    /// payload-only telescope: `success` at `[Nat, Bin]` becomes
    /// `(_0 : Nat) -> InductiveType { Result, [Nat, Bin] }`.
    pub fn instantiate(&self, tag: &Atom, params: &[Term]) -> Option<Telescope<Term>> {
        let mut telescope = self.constructors.get(tag)?.telescope.clone();

        for param in params {
            telescope = match telescope {
                Telescope::Cons(_, rest) => rest.open(&[param]),
                Telescope::Done(_) => return None,
            };
        }

        Some(telescope)
    }

    /// The runtime tag index of `tag`: its position among this inductive's
    /// constructors in sorted (BTreeMap key) order — the order in which a
    /// inductive match's lowered cases are laid out.
    pub fn tag_index(&self, tag: &Atom) -> Option<usize> {
        self.constructors
            .keys()
            .position(|candidate| candidate == tag)
    }
}
