use {
    super::{
        Atom, Polarity, Quality, Telescope, Term, UniverseContext, UniverseError, Universes,
        Variance, instantiate_qualities, instantiate_universes_scoped,
    },
    crate::Sharing,
    curios_utilities::{Plicity, Qualifier},
    std::{collections::HashMap, fmt, ops::Deref, sync::OnceLock},
};

/// One constructor's registry signature: its full telescope.
///
/// The telescope is the constructor's *full* signature — the parameter binders first, then the payload binders, terminating in the constructed type. E.g. `success ↦ (A : Type, E : Type, A) -> InductType { Result, [A, E] }`. For an indexed inductive the terminal is *per-case*: its indices are that case's target expressions over the payload binders. Instantiating peels the leading `params.len()` binders by opening each with the corresponding parameter.
///
/// Erasure is sort-driven: `erase` drops a payload field whose type is a proof or a type, by its own reading of the signature. Each binder states besides whether it binds a proof ([`Scope::binds_proof`](crate::Scope::binds_proof)), said when the declaration is published: an arm binds under it and the closed machine reads it, and it decides no layout.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct InductParam {
    /// Each binder under its mark — the value constructor's calling convention: every leading declaration parameter is hidden, `Implicit` where it is plain or `@` at the type constructor (a value constructor infers them) and `Witness` where it is a `use` premise, and each payload keeps its declared mark.
    pub telescope: Telescope<Vec<Term>>,
}

impl InductParam {
    /// A signature over `telescope` with its binders under `plicities`, one per binder: for a caller that holds the marks apart from the entries it built.
    pub fn new(telescope: Telescope<Vec<Term>>, plicities: Vec<Plicity>) -> Self {
        Self {
            telescope: telescope.with_marks(&plicities),
        }
    }

    /// The marks, one per binder, as the telescope states them.
    pub fn plicities(&self) -> Vec<Plicity> {
        self.telescope.marks()
    }
}

/// A family's cases in declaration order, each tag beside its signature, with the position of each tag found in constant time.
///
/// A sequence, because the order is load-bearing: a case's position *is* its runtime tag ([`InductDecl::constructor_index`]). A `BTreeMap<Atom, _>` would make that position the collation order over constructor spellings, so renaming a case would silently renumber the emitted tags of every case it sorted past. Declaration order is predictable from the source, stable under a rename, and changes only under an edit that visibly reorders the declaration.
///
/// The positions are found the first time a tag is looked up, and never stored, being read off the cases: a scan for a tag would make every construction and every reader of one case pay the family's width. Nothing reorders or renames a case once the sequence is made, so the positions found stand; equality, and the debug form, are the cases'.
#[derive(Clone, Default)]
#[curios_archive::archived]
pub struct Constructors {
    cases: Vec<(Atom, InductParam)>,
    #[archived_with(curios_archive::Skip)]
    positions: OnceLock<HashMap<Atom, usize>>,
}

impl Constructors {
    /// The position of the first case `tag` names, which is its runtime tag.
    pub fn position(&self, tag: &Atom) -> Option<usize> {
        let positions = self.positions.get_or_init(|| {
            let mut positions = HashMap::with_capacity(self.cases.len());
            for (position, (tag, _)) in self.cases.iter().enumerate() {
                positions.entry(tag.clone()).or_insert(position);
            }
            positions
        });

        positions.get(tag).copied()
    }

    /// The cases, in declaration order.
    pub fn as_slice(&self) -> &[(Atom, InductParam)] {
        &self.cases
    }

    /// Every case's signature mutably, in declaration order: what changes no tag and no position.
    pub fn signatures_mut(&mut self) -> impl Iterator<Item = &mut InductParam> {
        self.cases.iter_mut().map(|(_, param)| param)
    }
}

impl Deref for Constructors {
    type Target = [(Atom, InductParam)];

    fn deref(&self) -> &Self::Target {
        &self.cases
    }
}

impl<'c> IntoIterator for &'c Constructors {
    type Item = &'c (Atom, InductParam);
    type IntoIter = std::slice::Iter<'c, (Atom, InductParam)>;

    fn into_iter(self) -> Self::IntoIter {
        self.cases.iter()
    }
}

impl IntoIterator for Constructors {
    type Item = (Atom, InductParam);
    type IntoIter = std::vec::IntoIter<(Atom, InductParam)>;

    fn into_iter(self) -> Self::IntoIter {
        self.cases.into_iter()
    }
}

impl From<Vec<(Atom, InductParam)>> for Constructors {
    fn from(cases: Vec<(Atom, InductParam)>) -> Self {
        Self {
            cases,
            positions: OnceLock::new(),
        }
    }
}

impl FromIterator<(Atom, InductParam)> for Constructors {
    fn from_iter<I: IntoIterator<Item = (Atom, InductParam)>>(cases: I) -> Self {
        Self::from(cases.into_iter().collect::<Vec<_>>())
    }
}

impl PartialEq for Constructors {
    fn eq(&self, other: &Self) -> bool {
        self.cases == other.cases
    }
}

impl Eq for Constructors {}

impl fmt::Debug for Constructors {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.cases.fmt(formatter)
    }
}

/// One inductive declaration's registry entry: the metadata an `induct` declaration produces alongside its type-constructor and value-constructor function bindings.
///
/// The elaborator consults this when checking an inductive match: each arm's binders are typed directly from the matching constructor's telescope (instantiated at the scrutinee type's parameters), and the arm's binder count is arity-checked against that telescope statically. `erase` consults it again to type constructor payloads and order runtime tags.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct InductDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameters, terminating in its index telescope: `induct Vec(T : Type) : (n : Nat)` is `(T : Type)` ending in `(n : Nat)` ending in `()`. Each parameter stands under the mark it binds as at the type constructor — the marks the former's own function type carries, stated here for the elaborator, which opens the parameters over the indices and the signatures — and every index is plain. The value constructors' marks are each signature's own ([`InductParam`]).
    ///
    /// Nested rather than two parallel telescopes. The indices are scoped under the parameters *by construction*, so there is no agreement between two encodings to state, nothing to keep in sync, and no malformed pairing for a judgment to refuse — a declaration claiming one parameter and an index telescope that does not lead with it is unspellable rather than rejected.
    pub arity: Telescope<Telescope<()>>,
    /// Per-constructor signatures **in declaration order** — each the constructor's signature telescope (see [`InductParam`]), its position its runtime tag ([`Constructors`]).
    pub constructors: Constructors,
    /// The declared result sort — `Type` or `Prop` — the codomain of the type-constructor's kind. A fully-applied `InductType { name, .. }` has this sort, which `Sort::of` reads to decide propositional irrelevance.
    pub result_sort: Term,
    /// The exact source module that owns construction and elimination rights. This is finer-grained than the mount that owns the declaration: nested modules under one mount do not share representation access.
    pub module: Qualifier,
    /// Whether construction and elimination are available outside `module`. This metadata is elaboration-only and does not affect erased layouts.
    pub rep_public: bool,
    /// How this inductive uses each of its `params`, one entry per parameter in declaration order — the fact the elaborator's positivity composes through when a recursive occurrence travels via this type. The certifier composes through the vector its own walk filed ([`Certification::polarities`](super::Certification::polarities)), never this one. Computed by `curios-elab` as the declaration is published, once, and carried into the prelude archive so the standard library's are derived once per compiler build. Empty until then; read through [`Self::polarity`], never indexed directly.
    pub polarities: Vec<Polarity>,
    /// How two instances of this family compare in each of its universe parameters, one entry per parameter of `universe_context`. Computed by `curios-elab` and carried into the prelude archive, as the polarities are. Empty until then; read through [`Self::variance`], never indexed directly.
    pub variances: Vec<Variance>,
}

impl InductDecl {
    /// How many of this declaration's binders are uniform parameters.
    pub fn param_count(&self) -> usize {
        self.arity.len()
    }

    /// This declaration at one assignment of the sorts it leaves to a caller: every telescope it holds read at `qualities`, its level parameters its own still, and no sort left. What the kernel judges the entry at, and what erasure registers its row at, one assignment at a time.
    pub fn at_qualities(&self, qualities: &[Quality]) -> Self {
        let mut at = self.clone();
        at.arity = instantiate_qualities(&at.arity, qualities);
        at.result_sort = instantiate_qualities(&at.result_sort, qualities);
        for constructor in at.signatures_mut() {
            constructor.telescope = instantiate_qualities(&constructor.telescope, qualities);
        }
        at.universe_context.quality_count = 0;

        at
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
                Telescope::Cons(_, _, rest, ..) => telescope = rest.body(),
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

    /// This declaration's variance in its `i`th universe parameter.
    ///
    /// [`Variance::Invariant`] when the declaration has not been analyzed — the sound default, equality being what every level was compared at before a vector existed.
    pub fn variance(&self, i: usize) -> Variance {
        self.variances
            .get(i)
            .copied()
            .unwrap_or(Variance::Invariant)
    }

    /// This declaration with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.unplaced(),
            arity: sharing.share(&self.arity),
            constructors: self
                .constructors
                .iter()
                .map(|(tag, constructor)| {
                    (
                        tag.clone(),
                        InductParam {
                            telescope: sharing.share(&constructor.telescope),
                        },
                    )
                })
                .collect(),
            result_sort: sharing.share(&self.result_sort),
            module: self.module,
            rep_public: self.rep_public,
            polarities: self.polarities.clone(),
            variances: self.variances.clone(),
        }
    }

    /// Instantiate `tag`'s signature at the given type parameters, yielding the payload-only telescope: `success` at `[Nat, Bytes]` becomes `(Nat) -> InductType { Result, [Nat, Bytes] }`.
    pub fn instantiate(&self, tag: &Atom, params: &[Term]) -> Option<Telescope<Vec<Term>>> {
        Some(self.constructor(tag)?.telescope.clone().open_params(params))
    }

    /// `tag`'s signature entry, or `None` if it is not a case of this inductive.
    pub fn constructor(&self, tag: &Atom) -> Option<&InductParam> {
        let position = self.constructors.position(tag)?;

        Some(&self.constructors[position].1)
    }

    /// Every constructor signature, in declaration order.
    pub fn signatures(&self) -> impl Iterator<Item = &InductParam> {
        self.constructors.iter().map(|(_, param)| param)
    }

    /// Every constructor signature mutably, in declaration order.
    pub fn signatures_mut(&mut self) -> impl Iterator<Item = &mut InductParam> {
        self.constructors.signatures_mut()
    }

    /// The canonical plicities of `tag`'s *payload* binders — the constructor signature plicities past the leading `params.len()` declaration parameters, paralleling the telescope [`Self::instantiate`] peels. `None` if `tag` is not a constructor of this inductive.
    pub fn payload_plicities(&self, tag: &Atom) -> Option<Vec<Plicity>> {
        let param_count = self.param_count();
        self.constructor(tag)
            .map(|param| param.plicities().split_off(param_count))
    }

    /// Constructor tags in runtime dispatch order — which is declaration order: position `i` here is the tag `erase` assigns as runtime index `i` (`erase_variant`) and the arm order it builds a `Match` in (`erase_match`). Both sites must derive that correspondence from this one method, not from their own walk over `constructors`, so the two can never disagree about what "index `i`" means.
    pub fn constructor_order(&self) -> impl Iterator<Item = &Atom> {
        self.constructors.iter().map(|(tag, _)| tag)
    }

    /// Whether `tag` is one of this inductive's declared cases.
    pub fn declares(&self, tag: &Atom) -> bool {
        self.constructor(tag).is_some()
    }

    /// `tag`'s position in [`Self::constructor_order`] — the runtime tag `erase_variant` gives a value constructed with it.
    pub fn constructor_index(&self, tag: &Atom) -> Option<usize> {
        self.constructors.position(tag)
    }
}

/// An inductive declaration read at one occurrence's universes: each facet is instantiated where it is read, so a read of the sort, the arity or one constructor pays for that facet and never for every constructor the declaration has. What [`InductDecl::at`] hands back.
#[derive(Clone, Copy)]
pub struct InductInstance<'d> {
    declaration: &'d InductDecl,
    universes: &'d Universes,
}

impl InductDecl {
    /// This declaration at `universes`, read a facet at a time.
    pub fn at<'d>(&'d self, universes: &'d Universes) -> InductInstance<'d> {
        InductInstance {
            declaration: self,
            universes,
        }
    }
}

impl<'d> InductInstance<'d> {
    /// The declaration as declared: what no universe instance changes — its module, its constructors' order, its counts, its vectors.
    pub fn declaration(&self) -> &'d InductDecl {
        self.declaration
    }

    /// The parameter telescope ending in the index telescope, at these universes.
    pub fn arity(&self) -> Result<Telescope<Telescope<()>>, UniverseError> {
        instantiate_universes_scoped(&self.declaration.arity, self.universes)
    }

    /// The index telescope at `params`, at these universes ([`InductDecl::indices_at`]).
    pub fn indices_at(&self, params: &[Term]) -> Result<Telescope<()>, UniverseError> {
        Ok(self.arity()?.open(&params.iter().collect::<Vec<_>>()))
    }

    /// The declared result sort, at these universes.
    pub fn result_sort(&self) -> Result<Term, UniverseError> {
        instantiate_universes_scoped(&self.declaration.result_sort, self.universes)
    }

    /// `tag`'s signature at these universes, or `None` where `tag` is no case of the declaration.
    pub fn constructor(&self, tag: &Atom) -> Option<Result<InductParam, UniverseError>> {
        let param = self.declaration.constructor(tag)?;
        Some(
            instantiate_universes_scoped(&param.telescope, self.universes)
                .map(|telescope| InductParam { telescope }),
        )
    }

    /// `tag`'s payload at `params`, at these universes ([`InductDecl::instantiate`]), or `None` where `tag` is no case of the declaration.
    pub fn payload(
        &self,
        tag: &Atom,
        params: &[Term],
    ) -> Option<Result<Telescope<Vec<Term>>, UniverseError>> {
        Some(
            self.constructor(tag)?
                .map(|constructor| constructor.telescope.open_params(params)),
        )
    }

    /// Every facet at these universes: what a reader of every constructor, as a derivation is, takes. A match reads the constructors its arms name, each by [`Self::payload`].
    pub fn whole(&self) -> Result<InductDecl, UniverseError> {
        let mut instantiated = self.declaration.clone();
        instantiated.arity = self.arity()?;
        instantiated.result_sort = self.result_sort()?;
        for constructor in instantiated.signatures_mut() {
            constructor.telescope =
                instantiate_universes_scoped(&constructor.telescope, self.universes)?;
        }
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }
}
