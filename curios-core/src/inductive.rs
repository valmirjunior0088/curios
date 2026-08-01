use {
    super::{Atom, Polarity, Telescope, Term, UniverseContext},
    curios_base::{Plicity, Qualifier, RootId},
};

/// One constructor's registry signature: its full telescope.
///
/// The telescope is the constructor's *full* signature — the parameter binders first, then the payload binders, terminating in the constructed type. E.g. `success ↦ (A : Type, E : Type, _0 : A) -> InductType { Result, [A, E] }`. For an indexed inductive the terminal is *per-case*: its indices are that case's target expressions over the payload binders. Instantiating peels the leading `params.len()` binders by opening each with the corresponding parameter.
///
/// Erasure is sort-driven: `erase` drops a payload field whose type is a proof or a type — no per-payload mark is stored.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct InductParam {
    pub telescope: Telescope<Term>,
    /// One plicity mark per telescope binder — the value constructor's calling convention: every leading declaration parameter is `Implicit` (a value constructor infers them), each payload keeps its declared mark. Parallels `telescope`; `plicities.len()` equals `telescope.len()`.
    pub plicities: Vec<Plicity>,
}

/// One inductive declaration's registry entry: the metadata an `induct` declaration produces alongside its type-constructor and value-constructor function bindings.
///
/// The elaborator consults this when checking an inductive match: each arm's binders are typed directly from the matching constructor's telescope (instantiated at the scrutinee type's parameters), and the arm's binder count is arity-checked against that telescope statically. `erase` consults it again to type constructor payloads and order runtime tags.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct InductDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameters, terminating in its index telescope: `induct Vec(T : Type) : (n : Nat)` is `(T : Type)` ending in `(n : Nat)` ending in `()`.
    ///
    /// Nested rather than two parallel telescopes. The indices are scoped under the parameters *by construction*, so there is no agreement between two encodings to state, nothing to keep in sync, and no malformed pairing for a judgment to refuse — a declaration claiming one parameter and an index telescope that does not lead with it is unspellable rather than rejected.
    pub arity: Telescope<Telescope<()>>,
    /// Per-constructor signatures **in declaration order** — each the constructor's signature telescope (see [`InductParam`]).
    ///
    /// A sequence rather than a map, because the order is load-bearing: a constructor's position here *is* its runtime tag (`Self::constructor_index`). A `BTreeMap<Atom, _>` made that position the collation order over constructor spellings, so renaming a case silently renumbered the emitted tags of every case it sorted past. Declaration order is predictable from the source, stable under a rename, and changes only under an edit that visibly reorders the declaration.
    pub constructors: Vec<(Atom, InductParam)>,
    /// The declared result sort — `Type` or `Prop` — the codomain of the type-constructor's kind. A fully-applied `InductType { name, .. }` has this sort, which `Sort::of` reads to decide propositional irrelevance.
    pub result_sort: Term,
    /// The exact source module that owns construction and elimination rights. This is finer-grained than `root`: nested modules in the same compilation root do not share representation access.
    pub module: Qualifier,
    /// The compilation root that declares this inductive — consulted by the orphan-rule ownership check in `register_witness`.
    pub root: RootId,
    /// Whether construction and elimination are available outside `module`. This metadata is elaboration-only and does not affect erased layouts.
    pub rep_public: bool,
    /// How this inductive uses each of its `params`, one entry per parameter in declaration order — the fact positivity composes through when a recursive occurrence travels via this type. Computed by [`check_positivity`](crate::check_positivity) after elaboration and carried into the prelude archive so the standard library's are derived once per compiler build. Empty until then; read through [`Self::polarity`], never indexed directly.
    pub polarities: Vec<Polarity>,
}

impl InductDecl {
    /// How many of this declaration's binders are uniform parameters.
    pub fn param_count(&self) -> usize {
        self.arity.len()
    }

    /// This family's index telescope at `params` — the domains its index targets must inhabit.
    pub fn indices_at(&self, params: &[Term]) -> Telescope<()> {
        self.arity.open(&params.iter().collect::<Vec<_>>())
    }

    /// How many indices the family is declared over, read without instantiating it.
    pub fn index_count(&self) -> usize {
        let mut telescope = &self.arity;
        loop {
            match telescope {
                Telescope::Cons(_, rest) => telescope = rest.body(),
                Telescope::Done(indices) => return indices.len(),
            }
        }
    }

    /// This declaration's polarity in its `i`th parameter.
    ///
    /// [`Polarity::Mixed`] when the declaration has not been analyzed — the sound default. `Unused` would be the *unsound* one: it claims a parameter is harmless without evidence, which is exactly what lets a negative occurrence through.
    pub fn polarity(&self, i: usize) -> Polarity {
        self.polarities.get(i).copied().unwrap_or(Polarity::Mixed)
    }

    /// This declaration with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub fn shared(&self, sharing: &crate::Sharing) -> Self {
        Self {
            universe_context: self.universe_context.clone(),
            arity: sharing.share(&self.arity),
            constructors: self
                .constructors
                .iter()
                .map(|(tag, constructor)| {
                    (
                        tag.clone(),
                        InductParam {
                            telescope: sharing.share(&constructor.telescope),
                            plicities: constructor.plicities.clone(),
                        },
                    )
                })
                .collect(),
            result_sort: sharing.share(&self.result_sort),
            module: self.module.clone(),
            root: self.root,
            rep_public: self.rep_public,
            polarities: self.polarities.clone(),
        }
    }

    /// Instantiate `tag`'s signature at the given type parameters, yielding the payload-only telescope: `success` at `[Nat, Bin]` becomes `(_0 : Nat) -> InductType { Result, [Nat, Bin] }`.
    pub fn instantiate(&self, tag: &Atom, params: &[Term]) -> Option<Telescope<Term>> {
        Some(self.constructor(tag)?.telescope.clone().open_params(params))
    }

    /// `tag`'s signature entry, or `None` if it is not a case of this inductive. Constructor counts are small, so the scan is cheaper than the tree the collation-ordered map needed.
    pub fn constructor(&self, tag: &Atom) -> Option<&InductParam> {
        self.constructors
            .iter()
            .find(|(candidate, _)| candidate == tag)
            .map(|(_, param)| param)
    }

    /// Every constructor signature, in declaration order.
    pub fn signatures(&self) -> impl Iterator<Item = &InductParam> {
        self.constructors.iter().map(|(_, param)| param)
    }

    /// Every constructor signature mutably, in declaration order.
    pub fn signatures_mut(&mut self) -> impl Iterator<Item = &mut InductParam> {
        self.constructors.iter_mut().map(|(_, param)| param)
    }

    /// The canonical plicities of `tag`'s *payload* binders — the constructor signature plicities past the leading `params.len()` declaration parameters, paralleling the telescope [`Self::instantiate`] peels. `None` if `tag` is not a constructor of this inductive.
    pub fn payload_plicities(&self, tag: &Atom) -> Option<&[Plicity]> {
        let param_count = self.param_count();
        self.constructor(tag)
            .map(|param| &param.plicities[param_count..])
    }

    /// Constructor tags in runtime dispatch order — which is declaration order: position `i` here is the tag `erase` assigns as runtime index `i` (`erase_variant`) and the arm order it builds a `Match` in (`erase_induct_match`). Both sites must derive that correspondence from this one method, not from their own walk over `constructors`, so the two can never disagree about what "index `i`" means.
    pub fn constructor_order(&self) -> impl Iterator<Item = &Atom> {
        self.constructors.iter().map(|(tag, _)| tag)
    }

    /// Whether `tag` is one of this inductive's declared cases.
    pub fn declares(&self, tag: &Atom) -> bool {
        self.constructor(tag).is_some()
    }

    /// `tag`'s position in [`Self::constructor_order`] — the runtime tag `erase_variant` gives a value constructed with it.
    pub fn constructor_index(&self, tag: &Atom) -> Option<usize> {
        self.constructor_order()
            .position(|candidate| candidate == tag)
    }
}
