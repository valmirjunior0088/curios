use {
    super::{
        Atom, Bound, Free, Many, Polarity, Quality, Scope, Telescope, Term, UniverseContext,
        UniverseError, Universes, Variance, instantiate_qualities, instantiate_universes,
    },
    crate::Sharing,
    curios_common::{Plicity, Qualifier},
    std::{collections::HashMap, fmt, ops::Deref, sync::OnceLock},
};

/// One case of a family: its payload under a scope binding the family's parameters. E.g. `success ↦ (A : Type, E : Type) ⊢ (A) -> [ ]`, its parameters `Result`'s. For an indexed family the payload ends in the case's index targets, read over the parameters and the payload binders.
///
/// **A case reads its family's parameters and no others.** The scope binds them and holds no type for them, so their types are the arity's, the marks a value constructor takes them under are read off the arity's ([`InductDecl::parameter_marks`]), and a case standing at other parameters, or at more or fewer, is unspellable rather than refused. The case's whole signature, as a value constructor is declared over, is [`InductDecl::signature`].
///
/// Erasure is sort-driven: `erase` drops a payload field whose type is a proof or a type, by its own reading of the signature. Each payload binder states besides whether it binds a proof ([`Scope::binds_proof`](crate::Scope::binds_proof)), said when the declaration is published: an arm binds under it and the closed machine reads it, and it decides no layout.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct InductParam {
    /// Each payload binder under its mark — the value constructor's calling convention past the family's parameters — ending in the case's index targets, under the family's parameters.
    payload: Scope<Many, Telescope<Vec<Term>>>,
}

impl InductParam {
    /// The case whose payload is `payload`, in which `params` stand for the family's parameters.
    pub fn over(params: &[&Free], payload: Telescope<Vec<Term>>) -> Self {
        Self {
            payload: Scope::close(Many(params.len()), params, payload),
        }
    }

    /// The payload, under the family's parameters.
    pub fn payload(&self) -> &Scope<Many, Telescope<Vec<Term>>> {
        &self.payload
    }

    /// How many parameters the payload is held under, which a family holds to its arity's ([`InductDecl::new`]).
    pub fn param_count(&self) -> usize {
        self.payload.arity()
    }

    /// This case with its payload `rewrite` makes of it, which keeps its binders: a zonk, a sharing, a universe instance.
    ///
    /// # Panics
    ///
    /// Where `rewrite` hands back a payload under another count of parameters.
    pub fn rewritten(
        &self,
        rewrite: impl FnOnce(&Scope<Many, Telescope<Vec<Term>>>) -> Scope<Many, Telescope<Vec<Term>>>,
    ) -> Self {
        let payload = rewrite(&self.payload);
        assert_eq!(
            payload.arity(),
            self.payload.arity(),
            "a rewrite keeps a case's parameters"
        );

        Self { payload }
    }

    /// [`Self::rewritten`] by a rewrite that can refuse.
    pub fn try_rewritten<E>(
        &self,
        rewrite: impl FnOnce(
            &Scope<Many, Telescope<Vec<Term>>>,
        ) -> Result<Scope<Many, Telescope<Vec<Term>>>, E>,
    ) -> Result<Self, E> {
        let payload = rewrite(&self.payload)?;
        assert_eq!(
            payload.arity(),
            self.payload.arity(),
            "a rewrite keeps a case's parameters"
        );

        Ok(Self { payload })
    }

    /// The payload at `params`, the family's parameters as an occurrence states them.
    pub fn open(&self, params: &[Term]) -> Telescope<Vec<Term>> {
        self.payload.open(&params.iter().collect::<Vec<_>>())
    }

    /// The payload binders' marks, in order.
    pub fn plicities(&self) -> Vec<Plicity> {
        self.payload.body().marks()
    }

    /// Whether each payload binder binds a proof, in order.
    pub fn proofs(&self) -> Vec<bool> {
        self.payload.body().proofs()
    }

    /// This case with each payload binder stating whether it binds a proof, one answer per binder in order: the case itself where each states so already.
    pub fn with_proofs(&self, proofs: &[bool]) -> Self {
        if self.proofs() == proofs {
            return self.clone();
        }

        Self {
            payload: self
                .payload
                .map_body(|payload| payload.clone().with_proofs(proofs)),
        }
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
    universe_context: UniverseContext,
    /// The declaration's parameters, terminating in its index telescope: `induct Vec(T : Type) : (n : Nat)` is `(T : Type)` ending in `(n : Nat)` ending in `()`. Each parameter stands under the mark it binds as at the type constructor — the marks the former's own function type carries, stated here for the elaborator, which opens the parameters over the indices and the signatures — and every index is plain. A value constructor takes the parameters hidden ([`Self::parameter_marks`]) and each payload under its case's own mark ([`InductParam`]).
    ///
    /// Nested rather than two parallel telescopes. The indices are scoped under the parameters *by construction*, so there is no agreement between two encodings to state, nothing to keep in sync, and no malformed pairing for a judgment to refuse — a declaration claiming one parameter and an index telescope that does not lead with it is unspellable rather than rejected.
    arity: Telescope<Telescope<()>>,
    /// The cases **in declaration order** — each its payload under the parameters (see [`InductParam`]), its position its runtime tag ([`Constructors`]) — each held under as many parameters as the arity states, by [`Self::new`] and every door after it.
    constructors: Constructors,
    /// The declared result sort — `Type` or `Prop` — the codomain of the type-constructor's kind. A fully-applied `InductType { name, .. }` has this sort, which `Sort::of` reads to decide propositional irrelevance.
    result_sort: Term,
    /// The exact source module that owns construction and elimination rights. This is finer-grained than the mount that owns the declaration: nested modules under one mount do not share representation access.
    module: Qualifier,
    /// Whether construction and elimination are available outside `module`. This metadata is elaboration-only and does not affect erased layouts.
    rep_public: bool,
    /// How this inductive uses each of its `params`, one entry per parameter in declaration order — the fact the elaborator's positivity composes through when a recursive occurrence travels via this type. The certifier composes through the vector its own walk filed ([`Certification::polarities`](super::Certification::polarities)), never this one. Computed by `curios-elab` as the declaration is published, once, and carried into the prelude archive so the standard library's are derived once per compiler build. Empty until then; read through [`Self::polarity`], never indexed directly.
    polarities: Vec<Polarity>,
    /// How two instances of this family compare in each of its universe parameters, one entry per parameter of `universe_context`. Computed by `curios-elab` and carried into the prelude archive, as the polarities are. Empty until then; read through [`Self::variance`], never indexed directly.
    variances: Vec<Variance>,
}

impl InductDecl {
    /// The entry of a family over `arity`, whose cases are `constructors` and which lands in `result_sort` under `universe_context`, its representation `module`'s or, where `rep_public`, everyone's: neither of its vectors computed yet.
    ///
    /// # Panics
    ///
    /// Where a case is held under another count of parameters than `arity` states: a case reads its family's parameters and no others, which this door holds and every other keeps.
    pub fn new(
        universe_context: UniverseContext,
        arity: Telescope<Telescope<()>>,
        constructors: Constructors,
        result_sort: Term,
        module: Qualifier,
        rep_public: bool,
    ) -> Self {
        held_under(&constructors, arity.len());

        Self {
            universe_context,
            arity,
            constructors,
            result_sort,
            module,
            rep_public,
            polarities: Vec::new(),
            variances: Vec::new(),
        }
    }

    /// The universe context the family is declared under.
    pub fn universe_context(&self) -> &UniverseContext {
        &self.universe_context
    }

    /// The family's parameters, ending in its index telescope.
    pub fn arity(&self) -> &Telescope<Telescope<()>> {
        &self.arity
    }

    /// The family's cases, in declaration order.
    pub fn constructors(&self) -> &Constructors {
        &self.constructors
    }

    /// The sort the family lands in.
    pub fn result_sort(&self) -> &Term {
        &self.result_sort
    }

    /// The module whose representation the family is.
    pub fn module(&self) -> Qualifier {
        self.module
    }

    /// Whether the family's representation is everyone's.
    pub fn rep_public(&self) -> bool {
        self.rep_public
    }

    /// The family's polarity vector, as filed; read a parameter's through [`Self::polarity`].
    pub fn polarities(&self) -> &[Polarity] {
        &self.polarities
    }

    /// The family's variance vector, as filed; read a level's through [`Self::variance`].
    pub fn variances(&self) -> &[Variance] {
        &self.variances
    }

    /// Every part of this entry, each borrowed: for a reader that classifies each part, which a part added to the entry does not compile without.
    pub fn parts(&self) -> InductParts<'_> {
        InductParts {
            universe_context: &self.universe_context,
            arity: &self.arity,
            constructors: &self.constructors,
            result_sort: &self.result_sort,
            module: self.module,
            rep_public: self.rep_public,
            polarities: &self.polarities,
            variances: &self.variances,
        }
    }

    /// This entry under `universe_context`.
    pub fn with_universe_context(self, universe_context: UniverseContext) -> Self {
        Self {
            universe_context,
            ..self
        }
    }

    /// This entry over `arity`.
    ///
    /// # Panics
    ///
    /// Where a case is held under another count of parameters than `arity` states ([`Self::new`]).
    pub fn with_arity(self, arity: Telescope<Telescope<()>>) -> Self {
        held_under(&self.constructors, arity.len());

        Self { arity, ..self }
    }

    /// This entry with `constructors` for its cases.
    ///
    /// # Panics
    ///
    /// Where a case is held under another count of parameters than the arity states ([`Self::new`]).
    pub fn with_constructors(self, constructors: Constructors) -> Self {
        held_under(&constructors, self.arity.len());

        Self {
            constructors,
            ..self
        }
    }

    /// This entry with its cases `rewrite` makes of each, which keeps each one's parameters ([`InductParam::rewritten`]).
    pub fn with_cases(self, mut rewrite: impl FnMut(&InductParam) -> InductParam) -> Self {
        let constructors = self
            .constructors
            .iter()
            .map(|(tag, case)| (tag.clone(), rewrite(case)))
            .collect();
        held_under(&constructors, self.arity.len());

        Self {
            constructors,
            ..self
        }
    }

    /// [`Self::with_cases`] by a rewrite that can refuse.
    pub fn try_with_cases<E>(
        self,
        mut rewrite: impl FnMut(&InductParam) -> Result<InductParam, E>,
    ) -> Result<Self, E> {
        let constructors = self
            .constructors
            .iter()
            .map(|(tag, case)| Ok((tag.clone(), rewrite(case)?)))
            .collect::<Result<Constructors, E>>()?;
        held_under(&constructors, self.arity.len());

        Ok(Self {
            constructors,
            ..self
        })
    }

    /// This entry landing in `result_sort`.
    pub fn with_result_sort(self, result_sort: Term) -> Self {
        Self {
            result_sort,
            ..self
        }
    }

    /// This entry with its representation `module`'s.
    pub fn with_module(self, module: Qualifier) -> Self {
        Self { module, ..self }
    }

    /// This entry filing `polarities` as its polarity vector.
    pub fn with_polarities(self, polarities: Vec<Polarity>) -> Self {
        Self { polarities, ..self }
    }

    /// This entry filing `variances` as its variance vector.
    pub fn with_variances(self, variances: Vec<Variance>) -> Self {
        Self { variances, ..self }
    }

    /// How many of this declaration's binders are uniform parameters.
    pub fn param_count(&self) -> usize {
        self.arity.len()
    }

    /// This declaration at one assignment of the sorts it leaves to a caller: every telescope it holds read at `qualities`, its level parameters its own still, and no sort left. What the certifier judges the entry at, and what erasure registers its row at, one assignment at a time.
    pub fn at_qualities(&self, qualities: &[Quality]) -> Self {
        let mut universe_context = self.universe_context.clone();
        universe_context.quality_count = 0;
        let arity = instantiate_qualities(&self.arity, qualities);
        let result_sort = instantiate_qualities(&self.result_sort, qualities);

        self.clone()
            .with_arity(arity)
            .with_result_sort(result_sort)
            .with_cases(|case| case.rewritten(|payload| instantiate_qualities(payload, qualities)))
            .with_universe_context(universe_context)
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

    /// This declaration with every term hash-consed against `sharing`. See [`Definitions::shared`](crate::Definitions::shared).
    pub fn shared(&self, sharing: &Sharing) -> Self {
        let arity = sharing.share(&self.arity);
        let result_sort = sharing.share(&self.result_sort);

        self.clone()
            .with_arity(arity)
            .with_result_sort(result_sort)
            .with_cases(|case| case.rewritten(|payload| sharing.share(payload)))
    }

    /// `tag`'s payload at the given type parameters: `success` at `[Nat, Bytes]` is `(Nat) -> []`.
    pub fn instantiate(&self, tag: &Atom, params: &[Term]) -> Option<Telescope<Vec<Term>>> {
        Some(self.constructor(tag)?.open(params))
    }

    /// The marks a value constructor takes the family's parameters under, one per parameter: hidden, a `use` premise as the witness slot it is at the type constructor and every other parameter implicit, since a value constructor infers them.
    pub fn parameter_marks(&self) -> Vec<Plicity> {
        self.arity
            .marks()
            .into_iter()
            .map(|mark| match mark {
                Plicity::Witness => Plicity::Witness,
                Plicity::Explicit | Plicity::Implicit => Plicity::Implicit,
            })
            .collect()
    }

    /// `case`'s whole signature, as its value constructor is declared over: the family's parameters at the arity's types, under [`Self::parameter_marks`] and the arity's labels, then the case's payload, ending in its index targets.
    pub fn signature(&self, case: &InductParam) -> Telescope<Vec<Term>> {
        let (params, _) = opened(&self.arity, 0);
        let variables = params
            .iter()
            .map(|(_, binder, _, _)| Term::free_var(binder))
            .collect::<Vec<_>>();
        let (payload, targets) = opened(&case.open(&variables), params.len() as u64);
        let proofs = params
            .iter()
            .chain(&payload)
            .map(|(_, _, _, proof)| *proof)
            .collect::<Vec<_>>();
        let entries = params
            .into_iter()
            .zip(self.parameter_marks())
            .map(|((_, binder, domain, _), mark)| (mark, binder, domain))
            .chain(
                payload
                    .into_iter()
                    .map(|(mark, binder, domain, _)| (mark, binder, domain)),
            );

        Telescope::build_marked(entries, targets).with_proofs(&proofs)
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

    /// Which of `tag`'s payload binders bind a proof, as the declaration states it ([`Scope::binds_proof`](crate::Scope::binds_proof)), in payload order. `None` if `tag` is not a constructor of this inductive.
    pub fn payload_proofs(&self, tag: &Atom) -> Option<Vec<bool>> {
        self.constructor(tag).map(InductParam::proofs)
    }

    /// The plicities of `tag`'s payload binders, in the order of the telescope [`Self::instantiate`] opens. `None` if `tag` is not a constructor of this inductive.
    pub fn payload_plicities(&self, tag: &Atom) -> Option<Vec<Plicity>> {
        self.constructor(tag).map(InductParam::plicities)
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
        instantiate_universes(&self.declaration.arity, self.universes)
    }

    /// The index telescope at `params`, at these universes ([`InductDecl::indices_at`]).
    pub fn indices_at(&self, params: &[Term]) -> Result<Telescope<()>, UniverseError> {
        Ok(self.arity()?.open(&params.iter().collect::<Vec<_>>()))
    }

    /// The declared result sort, at these universes.
    pub fn result_sort(&self) -> Result<Term, UniverseError> {
        instantiate_universes(&self.declaration.result_sort, self.universes)
    }

    /// `tag`'s signature at these universes, or `None` where `tag` is no case of the declaration.
    pub fn constructor(&self, tag: &Atom) -> Option<Result<InductParam, UniverseError>> {
        let param = self.declaration.constructor(tag)?;
        Some(
            instantiate_universes(&param.payload, self.universes)
                .map(|payload| InductParam { payload }),
        )
    }

    /// `tag`'s whole signature ([`InductDecl::signature`]) at these universes, or `None` where `tag` is no case of the declaration.
    pub fn signature(&self, tag: &Atom) -> Option<Result<Telescope<Vec<Term>>, UniverseError>> {
        let case = self.declaration.constructor(tag)?;
        Some(instantiate_universes(
            &self.declaration.signature(case),
            self.universes,
        ))
    }

    /// `tag`'s payload at `params`, at these universes ([`InductDecl::instantiate`]), or `None` where `tag` is no case of the declaration.
    pub fn payload(
        &self,
        tag: &Atom,
        params: &[Term],
    ) -> Option<Result<Telescope<Vec<Term>>, UniverseError>> {
        Some(
            self.constructor(tag)?
                .map(|constructor| constructor.open(params)),
        )
    }

    /// Every facet at these universes: what a reader of every constructor, as a derivation is, takes. A match reads the constructors its arms name, each by [`Self::payload`].
    pub fn whole(&self) -> Result<InductDecl, UniverseError> {
        Ok(self
            .declaration
            .clone()
            .with_arity(self.arity()?)
            .with_result_sort(self.result_sort()?)
            .try_with_cases(|case| {
                case.try_rewritten(|payload| instantiate_universes(payload, self.universes))
            })?
            .with_universe_context(UniverseContext::empty()))
    }
}

/// `telescope`'s entries opened at locals numbered from `from`, each keeping its hint and its written place, beside its mark and whether it binds a proof, and what the entries end in.
fn opened<B: Bound>(telescope: &Telescope<B>, from: u64) -> (Vec<(Plicity, Free, Term, bool)>, B) {
    let mut entries = Vec::new();
    let mut cursor = telescope.cursor();
    while let (Some((hint, domain)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let index = from + entries.len() as u64;
        let binder = match cursor.written() {
            Some(written) => Free::local_written(index, hint, written),
            None => Free::local(index, hint),
        };
        let proof = cursor.binds_proof();
        cursor.advance(Term::free_var(&binder));
        entries.push((mark, binder, domain, proof));
    }
    let body = cursor
        .body()
        .expect("a cursor past every entry holds its payload");

    (entries, body)
}

/// Every part of an [`InductDecl`], each borrowed: what [`InductDecl::parts`] hands a reader that takes the entry apart in full.
pub struct InductParts<'a> {
    pub universe_context: &'a UniverseContext,
    pub arity: &'a Telescope<Telescope<()>>,
    pub constructors: &'a Constructors,
    pub result_sort: &'a Term,
    pub module: Qualifier,
    pub rep_public: bool,
    pub polarities: &'a [Polarity],
    pub variances: &'a [Variance],
}

/// Holds every case of `constructors` to being under `params` parameters ([`InductDecl::new`]).
fn held_under(constructors: &Constructors, params: usize) {
    assert!(
        constructors
            .iter()
            .all(|(_, case)| case.param_count() == params),
        "a case is held under its family's parameters"
    );
}
