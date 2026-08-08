#[cfg(test)]
mod tests;

// Deliberately not re-exported: the caches are [`Node`]'s private business.
mod scalars;
use scalars::*;

mod frees;
use frees::*;

use {
    super::{
        Atom, Bound, Enter, Free, Global, Intrinsic, Level, Many, Nat, Scope, SelfReference,
        Spelled, Spelling, Telescope, Three, Two, UniverseContext, UniverseError, UniverseMetaId,
        UniverseScheme, Var, Visit, instantiate_universe_levels_scoped, print_term,
    },
    curios_abi::ForeignFunction,
    curios_base::{
        Grain, Int, Mint, NumOp, Plicity, Span,
        printer::{run_printer, run_printer_within},
    },
    num_bigint::BigUint,
    std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        fmt,
        hash::{Hash, Hasher},
        mem,
        ops::{ControlFlow, Deref},
        rc::Rc,
        sync::Arc,
    },
};

#[cfg(feature = "archive")]
use curios_base::BigUintBytes;

/// The head identity a scrutinee-refinement key gates on: a named free-variable head, or the tag standing in for a comparison intrinsic whose normal form has no named head. Produced only by [`Term::head_key`], which documents the mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeadTag<'a> {
    Name(&'a Free),
    Intrinsic(&'static str),
}

/// A core-calculus term: an `Rc`-shared [`Node`] — a [`Subterm`] plus its lazily-cached, span-independent derivations (a structural hash, `reach`, the free-variable set, and the `has_local_free`/`has_metavar` bits) — with an optional per-occurrence source span. Clones are pointer bumps that share the node's cache, so a subterm shared across occurrences memoizes each derivation once, not once per occurrence. Equality short-circuits first on pointer identity, then on the cached hashes, before falling back to structural comparison — which is what keeps conversion and the reduction memo affordable on heavily shared trees. The span is identity-irrelevant: hash and equality look only at the node, so re-spanning a term never splits a cache.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(
        serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::ser::Sharing, __S::Error: rkyv::rancor::Source),
        deserialize_bounds(__D: rkyv::de::Pooling, __D::Error: rkyv::rancor::Source),
        bytecheck(bounds(__C: rkyv::validation::ArchiveContext + rkyv::validation::SharedContext, __C::Error: rkyv::rancor::Source))
    )
)]
pub struct Term {
    span: Option<Span>,
    #[cfg_attr(feature = "archive", rkyv(omit_bounds))]
    inner: Rc<Node>,
}

/// A [`Subterm`] together with its memoized, span-independent derivations. One per distinct node, behind the shared `Rc` every occurrence bumps, so each derivation fills at most once across the whole DAG. The caches are filled lazily by an iterative post-order walk over the node's descendants (`Term::warm_scalars`/`Term::get_or_init_free_vars`) rather than by native recursion, so a data-shaped spine of any depth memoizes on a bounded stack: filling one node reads its children's already-filled caches in O(children).
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
struct Node {
    /// The eager derivations — hash, `reach`, and the containment flags — packed behind one filled bit; see [`ScalarCache`].
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    scalars: ScalarCache,
    /// The one derivation left lazy. A `BTreeSet<Free>` per node would dominate the archive it is stored in, and unlike the scalars it is wanted by a minority of nodes on a given compilation.
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    frees: FreeCache,
    subterm: Subterm,
}

impl Node {
    fn new(subterm: Subterm) -> Self {
        Node {
            scalars: ScalarCache::default(),
            frees: FreeCache::default(),
            subterm,
        }
    }
}

/// The cells are cache noise; a `Node` prints as its subterm.
impl fmt::Debug for Node {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.subterm.fmt(formatter)
    }
}

impl Term {
    /// Fill the eager scalar derivations together in one post-order [`Term::walk`]. They combine from the children's caches in O(children), and are almost always wanted together, so one shared walk beats independent traversals. Cloning a `Term` bumps the shared `Rc<Node>`, so filling through the walk's clones fills every occurrence; the filled bit is the walk's own memo, and a shared node's later occurrence can only pop after the exit that filled it — every frame between them belongs to its own subtree — so a `Skip`ped node is always already filled.
    fn warm_scalars(&self) {
        self.walk(
            &mut (),
            |_, term| {
                if term.inner.scalars.is_filled() {
                    Enter::Skip(())
                } else {
                    Enter::Descend
                }
            },
            |_, term, _| term.inner.scalars.fill(Scalars::of(&term.inner.subterm)),
        );
    }

    /// This node's memoized scalars, warming the whole subtree on first demand.
    fn scalars(&self) -> Scalars {
        if let Some(scalars) = self.inner.scalars.get() {
            return scalars;
        }
        self.warm_scalars();
        self.inner
            .scalars
            .get()
            .expect("warm_scalars fills the scalar cache")
    }

    fn get_or_init_hash(&self) -> u64 {
        self.scalars().hash
    }

    /// Whether any *free* variable in this term carries an elaborator-minted label — one containing `#`, which cannot occur in a written identifier (`Context::fresh` always embeds it; witness-table names share the convention, deliberately counted here so the elaboration memo stays conservative). Binder labels inside `Scope`s are closed occurrences, not free variables, and never count. Cached per node and computed from the children's cached scalars, so a shared subterm — a DAG-shaped lowered literal — pays O(degree) here, not O(size): the elaboration cache gates every `elaborate` call on this bit and must not re-walk shared chains.
    pub fn has_local_free(&self) -> bool {
        self.scalars().has_local_free
    }

    /// Whether any `Metavar` node occurs in this term. Cached per node like [`has_local_free`](Self::has_local_free) and for the same reason: the elaboration cache's O(1)-per-call gate.
    pub(crate) fn has_metavar(&self) -> bool {
        self.scalars().has_metavar
    }

    /// Whether this term contains an unresolved universe metavariable in a `Type` level, universe instantiation, or nominal universe vector.
    pub fn has_universe_meta(&self) -> bool {
        self.scalars().has_universe_meta
    }

    /// Whether universe erasure or validation must inspect this subtree.
    ///
    /// Cached and filled on the explicit post-order stack like the other scalar derivations, so universe-only passes can structurally share a deep universe-free data spine without consuming one native frame per node.
    pub fn has_universe_data(&self) -> bool {
        self.scalars().has_universe_data
    }

    pub fn universe_metas(&self) -> BTreeSet<UniverseMetaId> {
        super::universe_metas(self)
    }

    /// Whether any universe metavariable in this subtree satisfies `pred`.
    ///
    /// The walk is iterative ([`Term::try_walk`]) and pointer-deduplicated, matching the scalar cache fill: cache eligibility calls this on data-shaped terms and must not put their depth back onto the native stack.
    pub fn any_universe_meta(&self, mut pred: impl FnMut(UniverseMetaId) -> bool) -> bool {
        let mut seen: HashSet<*const Node> = HashSet::new();
        self.try_walk(
            &mut seen,
            |seen, term| {
                if !seen.insert(Rc::as_ptr(&term.inner)) || !term.has_universe_meta() {
                    return ControlFlow::Continue(Enter::Skip(()));
                }
                if term.inner.subterm.any_direct_universe_meta(&mut pred) {
                    return ControlFlow::Break(());
                }
                ControlFlow::Continue(Enter::Descend)
            },
            |_, _, _| (),
        )
        .is_break()
    }

    /// Extend the two dependency sets in one explicit walk without rebuilding the term or warming its unrelated scalar caches. Declaration universe closure uses both sets together: direct level metas join the closure, while term metas lead to their result, telescope, and solved body in the context store. A metavariable's children are its spine entries, whose bare `Var`s carry nothing but dedup for free.
    pub fn collect_universe_dependencies(
        &self,
        universes: &mut BTreeSet<UniverseMetaId>,
        term_metas: &mut BTreeSet<MetaId>,
    ) {
        let mut seen: HashSet<*const Node> = HashSet::new();
        self.walk(
            &mut seen,
            |seen, term| {
                if !seen.insert(Rc::as_ptr(&term.inner)) {
                    return Enter::Skip(());
                }
                term.inner.subterm.any_direct_universe_meta(&mut |meta| {
                    universes.insert(meta);
                    false
                });
                if let Subterm::Metavar(Metavar { id, .. }) = &term.inner.subterm {
                    term_metas.insert(*id);
                }
                Enter::Descend
            },
            |_, _, _| (),
        );
    }

    /// Rewrite this node, if it is an occurrence of one of `names`, to denote the declaration instance `levels`. Returns `None` for every other node, leaving it to ordinary traversal.
    ///
    /// Two occurrence shapes carry an instance. A nominal normal form holds it in its own universe vector; a not-yet-reduced reference to a type former is an ordinary variable, which holds it as a wrapping [`UniverseInst`] — the same node an external use site receives from scheme instantiation. A variable already under a `UniverseInst` has been instantiated and is returned untouched rather than wrapped twice.
    ///
    /// Nominal children are stamped explicitly because a rewrite hook replaces its node wholesale: an occurrence nested in a parameter or index must receive the same instance as the occurrence containing it.
    pub(crate) fn stamp_declaration_node(
        &self,
        names: &BTreeSet<Global>,
        self_reference: SelfReference,
        levels: &[Level],
    ) -> Option<Self> {
        fn stamp(
            terms: &[Term],
            names: &BTreeSet<Global>,
            self_reference: SelfReference,
            levels: &[Level],
        ) -> Vec<Term> {
            terms
                .iter()
                .map(|term| super::stamp_declaration_instance(term, names, self_reference, levels))
                .collect()
        }

        let subterm = match &**self {
            Subterm::InductType(induct) if names.contains(&induct.name) => {
                Subterm::InductType(InductType {
                    name: induct.name.clone(),
                    universes: levels.to_vec(),
                    params: stamp(&induct.params, names, self_reference, levels),
                    indices: stamp(&induct.indices, names, self_reference, levels),
                })
            }
            Subterm::Variant(variant) if names.contains(&variant.name) => {
                Subterm::Variant(Variant {
                    name: variant.name.clone(),
                    universes: levels.to_vec(),
                    params: stamp(&variant.params, names, self_reference, levels),
                    tag: variant.tag.clone(),
                    payload: stamp(&variant.payload, names, self_reference, levels),
                })
            }
            Subterm::StructType(struct_type) if names.contains(&struct_type.name) => {
                Subterm::StructType(StructType {
                    name: struct_type.name.clone(),
                    universes: levels.to_vec(),
                    params: stamp(&struct_type.params, names, self_reference, levels),
                })
            }
            Subterm::Struct(struct_) if names.contains(&struct_.name) => Subterm::Struct(Struct {
                name: struct_.name.clone(),
                universes: levels.to_vec(),
                params: stamp(&struct_.params, names, self_reference, levels),
                fields: stamp(&struct_.fields, names, self_reference, levels),
                entries: struct_.entries.clone(),
            }),
            Subterm::UniverseInst(instance)
                if instance
                    .head
                    .head_name()
                    .and_then(Free::as_global)
                    .is_some_and(|name| names.contains(name)) =>
            {
                return Some(self.clone());
            }
            Subterm::Var(var)
                if self_reference == SelfReference::Free
                    && var
                        .as_free()
                        .and_then(Free::as_global)
                        .is_some_and(|name| names.contains(name)) =>
            {
                return Some(Term::universe_inst(self.clone(), levels.to_vec()));
            }
            _ => return None,
        };

        let stamped = Term::from(subterm);
        Some(match self.span() {
            Some(span) => stamped.with_span(span),
            None => stamped,
        })
    }

    pub fn unwrap_or_clone(this: Self) -> Subterm {
        match Rc::try_unwrap(this.inner) {
            // Swapped out rather than moved out: [`Node`] dismantles itself on drop, and a type with a `Drop` impl cannot have a field moved away. The husk left behind is childless, so dropping it is free.
            Ok(mut node) => mem::replace(&mut node.subterm, Subterm::Prop),
            Err(shared) => shared.subterm.clone(),
        }
    }

    /// The free-variable identity at the head of an application spine, descending through curried `Apply` heads: `classify(c)` and `f(a)(b)` report the name of `classify` / `f`. A bare free variable reports itself; anything else is `None`. Used to cheaply gate scrutinee-refinement canonicalization on the applied symbol before paying for argument reduction.
    pub fn head_name(&self) -> Option<&Free> {
        match &self.inner.subterm {
            Subterm::Apply(Apply { head, .. }) => head.head_name(),
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.head_name(),
            Subterm::Var(var) => var.as_free(),
            _ => None,
        }
    }

    /// Build the structural `Bin` eliminator ([`Carrier::Bin`]) over an already-built motive scope: an empty arm plus a cons arm closed over `(head, tail, ih)` — the induction hypothesis at the tail.
    #[allow(clippy::too_many_arguments)]
    pub fn bin_match_scoped<H, EC, CC>(
        grain: Grain,
        head: H,
        motive: Scope<Many>,
        empty_case: EC,
        head_binder: &Free,
        tail_binder: &Free,
        ih_binder: &Free,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        EC: Into<Term>,
        CC: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Bin {
                    grain,
                    empty_case: empty_case.into(),
                    cons_case: Scope::close(
                        Three,
                        &[head_binder, tail_binder, ih_binder],
                        cons_case.into(),
                    ),
                },
            },
        )
    }

    /// [`Term::bool_match`] over an already-built motive scope.
    pub fn bool_match_scoped<H, F, T>(
        head: H,
        motive: Scope<Many>,
        false_case: F,
        true_case: T,
    ) -> Self
    where
        H: Into<Term>,
        F: Into<Term>,
        T: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::Bool {
                false_case: false_case.into(),
                true_case: true_case.into(),
            },
        )
    }

    /// Build a function literal from `(plicity, label, annotation)` binders, keeping one plicity mark per telescope entry (asserted to line up — the [`Func`] invariant). The all-explicit shorthand is [`Term::func`].
    pub fn func_marked<I, T, B>(params: I, body: B) -> Self
    where
        I: IntoIterator<Item = (Plicity, Free, T)>,
        T: Into<Term>,
        B: Into<Term>,
    {
        let mut plicities = Vec::new();
        let telescope = Telescope::build(
            params.into_iter().map(|(plicity, label, type_)| {
                plicities.push(plicity);
                (label, type_)
            }),
            body.into(),
        );
        assert_eq!(plicities.len(), telescope.len());

        Self::from(Subterm::Func(Func {
            telescope,
            plicities,
        }))
    }

    pub fn induct_type_at<U, I, P, J, Q>(name: Global, universes: U, params: I, indices: J) -> Self
    where
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::InductType(InductType {
            name,
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            indices: indices.into_iter().map(|i| i.into()).collect(),
        }))
    }

    /// [`Term::lst_match`] over an already-built motive scope.
    #[allow(clippy::too_many_arguments)]
    pub fn lst_match_scoped<H, EL, EC, CC>(
        head: H,
        elem: EL,
        motive: Scope<Many>,
        empty_case: EC,
        head_binder: &Free,
        tail_binder: &Free,
        ih_binder: &Free,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        EL: Into<Term>,
        EC: Into<Term>,
        CC: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Lst {
                    elem: elem.into(),
                    empty_case: empty_case.into(),
                    cons_case: Scope::close(
                        Three,
                        &[head_binder, tail_binder, ih_binder],
                        cons_case.into(),
                    ),
                },
            },
        )
    }

    /// [`Term::nat_match`] over an already-built motive scope.
    pub fn nat_match_scoped<H, ZC, SC>(
        head: H,
        motive: Scope<Many>,
        zero_case: ZC,
        pred_binder: &Free,
        ih_binder: &Free,
        succ_case: SC,
    ) -> Self
    where
        H: Into<Term>,
        ZC: Into<Term>,
        SC: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: zero_case.into(),
                    cons_case: Scope::close(Two, &[pred_binder, ih_binder], succ_case.into()),
                },
            },
        )
    }

    pub fn struct_at<U, I, P, J, Q>(name: Global, universes: U, params: I, fields: J) -> Self
    where
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::Struct(Struct {
            name,
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            fields: fields.into_iter().map(|f| f.into()).collect(),
            entries: vec![],
        }))
    }

    /// [`Term::switch`] over an already-built motive scope.
    pub fn switch_scoped<H, I, B, D>(head: H, motive: Scope<Many>, cases: I, default: D) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = (u32, B)>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::Switch {
                cases: cases.into_iter().map(|(n, b)| (n, b.into())).collect(),
                default: default.into(),
            },
        )
    }

    pub fn variant_at<U, I, P, A, J, Q>(
        name: Global,
        universes: U,
        params: I,
        tag: A,
        payload: J,
    ) -> Self
    where
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        A: Into<Atom>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::Variant(Variant {
            name,
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            tag: tag.into(),
            payload: payload.into_iter().map(|p| p.into()).collect(),
        }))
    }

    /// What a scrutinee-refinement key is gated on: the identity at an application spine's head, or the intrinsic standing in for one where the normal form is an `Intrinsic` node rather than an application. Never a name a program could write — the two sides of every comparison come from here, so this only ever has to agree with itself.
    pub fn head_key(&self) -> Option<HeadTag<'_>> {
        match &self.inner.subterm {
            Subterm::Apply(Apply { head, .. }) => head.head_key(),
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.head_key(),
            Subterm::Var(var) => var.as_free().map(HeadTag::Name),
            // A decidable comparison's normal form is an intrinsic node, not an application, so it has no named head. Scrutinee refinement keys on this tag and the reducer's probe gates on it, so an untagged key can be registered but never looked up — which is how an operator-spelled scrutinee loses its arm refinement while the equivalent `Nat/lte(a, b)` keeps it.
            Subterm::Intrinsic(intrinsic) => match intrinsic {
                Intrinsic::BoolEql(..) => Some(HeadTag::Intrinsic("intrinsic:BoolEql")),
                Intrinsic::BoolNeq(..) => Some(HeadTag::Intrinsic("intrinsic:BoolNeq")),
                Intrinsic::NatEql(..) => Some(HeadTag::Intrinsic("intrinsic:NatEql")),
                Intrinsic::NatNeq(..) => Some(HeadTag::Intrinsic("intrinsic:NatNeq")),
                Intrinsic::NatLt(..) => Some(HeadTag::Intrinsic("intrinsic:NatLt")),
                Intrinsic::NatGt(..) => Some(HeadTag::Intrinsic("intrinsic:NatGt")),
                Intrinsic::NatLte(..) => Some(HeadTag::Intrinsic("intrinsic:NatLte")),
                Intrinsic::NatGte(..) => Some(HeadTag::Intrinsic("intrinsic:NatGte")),
                Intrinsic::ByteEql(..) => Some(HeadTag::Intrinsic("intrinsic:ByteEql")),
                Intrinsic::ByteLt(..) => Some(HeadTag::Intrinsic("intrinsic:ByteLt")),
                Intrinsic::ByteLte(..) => Some(HeadTag::Intrinsic("intrinsic:ByteLte")),
                Intrinsic::ByteGt(..) => Some(HeadTag::Intrinsic("intrinsic:ByteGt")),
                Intrinsic::ByteGte(..) => Some(HeadTag::Intrinsic("intrinsic:ByteGte")),
                Intrinsic::IntEql(..) => Some(HeadTag::Intrinsic("intrinsic:IntEql")),
                Intrinsic::IntNeq(..) => Some(HeadTag::Intrinsic("intrinsic:IntNeq")),
                Intrinsic::IntLt(..) => Some(HeadTag::Intrinsic("intrinsic:IntLt")),
                Intrinsic::IntGt(..) => Some(HeadTag::Intrinsic("intrinsic:IntGt")),
                Intrinsic::IntLte(..) => Some(HeadTag::Intrinsic("intrinsic:IntLte")),
                Intrinsic::IntGte(..) => Some(HeadTag::Intrinsic("intrinsic:IntGte")),
                Intrinsic::FltEql(..) => Some(HeadTag::Intrinsic("intrinsic:FltEql")),
                Intrinsic::FltNeq(..) => Some(HeadTag::Intrinsic("intrinsic:FltNeq")),
                Intrinsic::FltLt(..) => Some(HeadTag::Intrinsic("intrinsic:FltLt")),
                Intrinsic::FltGt(..) => Some(HeadTag::Intrinsic("intrinsic:FltGt")),
                Intrinsic::FltLte(..) => Some(HeadTag::Intrinsic("intrinsic:FltLte")),
                Intrinsic::FltGte(..) => Some(HeadTag::Intrinsic("intrinsic:FltGte")),
                Intrinsic::BinEql(..) => Some(HeadTag::Intrinsic("intrinsic:BinEql")),
                Intrinsic::HandleEql(..) => Some(HeadTag::Intrinsic("intrinsic:HandleEql")),
                _ => None,
            },
            _ => None,
        }
    }

    /// Return the canonical target when this term is a straightforward transparent alias body: either a single free variable or its eta-expanded parameterized form `(xs) => Original(xs)`. The text-stage interface audit uses this after name resolution to preserve representation provenance; computed bodies are not classified as aliases.
    pub fn transparent_alias_target(&self) -> Option<&Free> {
        match &self.inner.subterm {
            Subterm::Var(var) => var.as_free(),
            Subterm::Func(Func { telescope, .. }) => {
                // Read the eta-expansion under its binders instead of opening it: the parameters are exactly the innermost de Bruijn indices there, counting outwards, so the shape is decided without minting probe binders that would have to be proven not to collide with the body's own.
                let arity = telescope.len();
                let Subterm::Apply(Apply { head, params, .. }) = &**telescope.terminal() else {
                    return None;
                };
                let eta = params.len() == arity
                    && params.iter().enumerate().all(|(index, param)| {
                        matches!(&**param, Subterm::Var(var) if var.as_bound() == Some(arity - 1 - index))
                    });

                eta.then(|| match &**head {
                    Subterm::Var(target) => target.as_free(),
                    _ => None,
                })
                .flatten()
            }
            _ => None,
        }
    }

    pub fn span(&self) -> Option<Span> {
        self.span.clone()
    }

    /// Attaches a span to this term. If the term already carries a span (the innermost one), it is preserved — innermost wins, matching how `Error::at` keeps the first span it sees as errors propagate up.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }

    /// Ground `Type 0`, used only where the calculus requires that exact universe (intrinsic carriers and the type of `Prop`).
    pub fn type_ground() -> Self {
        Self::type_at(Level::zero())
    }

    /// `Type` at a known internal level.
    pub fn type_at(level: Level) -> Self {
        Self::from(Subterm::Type(level))
    }

    /// The universe of strict propositions, `Prop`.
    pub fn prop() -> Self {
        Self::from(Subterm::Prop)
    }

    /// An intrinsic term — any literal or intrinsic operation that converts into [`Intrinsic`].
    pub fn intrinsic<P: Into<Intrinsic>>(intrinsic: P) -> Self {
        Self::from(Subterm::Intrinsic(intrinsic.into()))
    }

    /// A host call against the ABI row `function` describes.
    pub fn foreign(function: Arc<ForeignFunction>, args: Vec<Term>) -> Self {
        Self::from(Subterm::Foreign(function, args))
    }

    /// A variable occurrence. External callers can only build free variables ([`Var::free`]); bound ones are the scope machinery's business.
    pub fn var(var: Var) -> Self {
        Self::from(Subterm::Var(var))
    }

    pub fn free_var(name: &Free) -> Self {
        Self::var(Var::free(name.clone()))
    }

    /// Instantiate a generalized binding at occurrence-specific levels.
    pub fn universe_inst(head: Term, levels: Vec<Level>) -> Self {
        if levels.is_empty() {
            head
        } else {
            Self::from(Subterm::UniverseInst(UniverseInst { head, levels }))
        }
    }

    /// A bare metavariable, as `into_core` mints one for a desugared hole (an omitted annotation, motive, or lambda domain): empty spine (which resolves as the identity — see [`Metavar::spine`]) and no insertion origin, so its solution is spliced silently at zonk.
    pub fn metavar(id: impl Into<MetaId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: None,
        }))
    }

    /// A metavariable carrying its (optional) provenance mark and birth spine: a hole or goal rebuilt at its birth point with the identity spine over its frozen telescope, or an elaborator insertion minted with its provenance (see [`Metavar::origin`] and [`Metavar::spine`]).
    pub fn metavar_birthed(
        id: impl Into<MetaId>,
        origin: Option<MetavarOrigin>,
        spine: impl Into<Rc<Vec<Term>>>,
    ) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: spine.into(),
            origin,
        }))
    }

    /// `inner` with `span` attached — innermost wins if `inner` already carries one, per `Term::with_span`.
    pub fn spanned<T: Into<Term>>(span: Span, inner: T) -> Self {
        inner.into().with_span(span)
    }

    pub fn func_type<I, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (Free, T)>,
        T: Into<Term>,
        O: Into<Term>,
    {
        Self::func_type_marked(
            params
                .into_iter()
                .map(|(binder, type_)| (Plicity::Explicit, binder, type_)),
            output,
        )
    }

    /// Build a Π-type from `(plicity, label, type)` binders, keeping one plicity mark per telescope entry (asserted to line up — the [`FuncType`] invariant). The all-explicit shorthand is the crate-internal `func_type`.
    pub fn func_type_marked<I, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (Plicity, Free, T)>,
        T: Into<Term>,
        O: Into<Term>,
    {
        let mut plicities = Vec::new();
        let telescope = Telescope::build(
            params.into_iter().map(|(plicity, label, type_)| {
                plicities.push(plicity);
                (label, type_)
            }),
            output.into(),
        );
        assert_eq!(plicities.len(), telescope.len());

        Self::from(Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }))
    }

    /// Build an all-explicit function literal from `(label, annotation)` parameters, closing the body over the labels via a [`Telescope`]. Every binder is stamped [`Plicity::Explicit`] — use [`Term::func_marked`] for a function containing hidden binders. There is deliberately no unmarked "trust me" constructor for a hidden-binder function.
    pub fn func<I, T, B>(params: I, body: B) -> Self
    where
        I: IntoIterator<Item = (Free, T)>,
        T: Into<Term>,
        B: Into<Term>,
    {
        Self::func_marked(
            params
                .into_iter()
                .map(|(binder, type_)| (Plicity::Explicit, binder, type_)),
            body,
        )
    }

    /// Build an application whose arguments are all explicit — the common case; [`Term::apply_marked`] when call-site plicity marks matter.
    pub fn apply<H, I, P>(head: H, params: I) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::apply_marked(
            head,
            params.into_iter().map(|p| (Plicity::Explicit, p.into())),
        )
    }

    /// Build an application with a per-argument plicity mark — the call-site `@`/`use` marks core must carry for the elaborator to decide which binder each argument fills (see [`Apply`]).
    pub fn apply_marked<H, I, P>(head: H, params: I) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = (Plicity, P)>,
        P: Into<Term>,
    {
        let (plicities, params) = params
            .into_iter()
            .map(|(plicity, param)| (plicity, param.into()))
            .unzip();

        Self::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        }))
    }

    pub fn tuple_type_unit() -> Self {
        Self::from(Subterm::TupleType(TupleType {
            telescope: Telescope::done(()),
        }))
    }

    /// Build a dependent tuple (Σ) type from `(label, type)` fields: each field's type is closed over the labels before it — written order mirrors telescope order.
    pub fn tuple_type<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (Free, T)>,
        T: Into<Term>,
    {
        let telescope = Telescope::build(fields, ());

        Self::from(Subterm::TupleType(TupleType { telescope }))
    }

    /// A positional tuple literal — the name-free normal form ([`Tuple::names`] empty) every internally-built and post-elaboration tuple keeps.
    pub fn tuple<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<Term>,
    {
        Self::from(Subterm::Tuple(Tuple {
            fields: fields.into_iter().map(|t| t.into()).collect(),
            names: vec![],
        }))
    }

    /// A positional projection `head.index` — the normal form every post-elaboration projection takes (cf. [`Field`]).
    pub fn proj<H: Into<Term>>(head: H, index: usize) -> Self {
        Self::from(Subterm::Proj(Proj {
            head: head.into(),
            field: Field::Index(index),
        }))
    }

    /// A labelled projection `head.label` — the pre-elaboration form; elaboration resolves the label against the head's tuple type and rebuilds it as [`Term::proj`].
    pub fn proj_label<H: Into<Term>, L: Into<String>>(head: H, label: L) -> Self {
        Self::from(Subterm::Proj(Proj {
            head: head.into(),
            field: Field::Label(label.into()),
        }))
    }

    /// Build an [`InductType`] normal form — the body of the generated type-constructor function. See the type's docs for the `params`/`indices` split.
    pub fn induct_type<I, P, J, Q>(name: Global, params: I, indices: J) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::induct_type_at(name, Vec::<Level>::new(), params, indices)
    }

    /// Build a [`Variant`] normal form — the body of a generated value-constructor function. `name`/`params` are stored redundantly on purpose; see the type's docs.
    pub fn variant<I, P, A, J, Q>(name: Global, params: I, tag: A, payload: J) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        A: Into<Atom>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::variant_at(name, Vec::<Level>::new(), params, tag, payload)
    }

    /// A struct value with no written field names — the positional normal form (post-elaboration and every internal build), mirroring `tuple`.
    pub fn struct_<I, P, J, Q>(name: Global, params: I, fields: J) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::struct_at(name, Vec::<Level>::new(), params, fields)
    }

    /// Build the intrinsic eliminator of a nominal inductive ([`Cases::Induct`]): one arm per constructor tag, each closed over its payload binders (all-explicit). [`Term::induct_match_marked`] carries per-binder plicity.
    pub fn induct_match<H, M, I, A, B>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        cases: I,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<Free>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        Self::induct_match_marked(
            head,
            motive_binder,
            motive,
            cases
                .into_iter()
                .map(|(atom, binders, body)| (atom, explicit_arm(binders), body)),
        )
    }

    /// [`Term::induct_match`] carrying the written constructor-pattern plicity of each payload binder — the matrix compiler's entry point.
    pub(crate) fn induct_match_marked<H, M, I, A, B>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        cases: I,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, Free)>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        Self::induct_match_scoped_marked(
            head,
            Self::motive_scope(motive_binder, motive.into()),
            cases,
            None,
        )
    }

    /// [`Term::induct_match_marked`] over an already-built motive scope, with the optional `| _ =>` catch-all folded in — `into_core`'s single entry point for a nominal-inductive elimination.
    pub fn induct_match_scoped_marked<H, I, A, B>(
        head: H,
        motive: Scope<Many>,
        cases: I,
        default: Option<Term>,
    ) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, Free)>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        Self::match_scoped(
            head.into(),
            motive,
            Cases::Induct {
                cases: Self::induct_cases_marked(cases),
                default,
            },
        )
    }

    /// The intrinsic eliminator of a nominal inductive with an explicit `| _ =>` catch-all ([`Cases::Induct`]'s `default`): the enumerated arms plus a binding-free default standing in for every other constructor tag. The dispatching analogue of [`Term::induct_match`], mirroring how [`Term::switch`] relates to [`Term::nat_match`].
    pub fn induct_match_default<H, M, I, A, B, D>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        cases: I,
        default: D,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<Free>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::induct_match_default_marked(
            head,
            motive_binder,
            motive,
            cases
                .into_iter()
                .map(|(atom, binders, body)| (atom, explicit_arm(binders), body)),
            default,
        )
    }

    /// [`Term::induct_match_default`] carrying the written constructor-pattern plicity of each payload binder — the matrix compiler's entry point.
    pub(crate) fn induct_match_default_marked<H, M, I, A, B, D>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        cases: I,
        default: D,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, Free)>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::induct_match_scoped_marked(
            head,
            Self::motive_scope(motive_binder, motive.into()),
            cases,
            Some(default.into()),
        )
    }

    /// Build the arm map from `(tag, [(plicity, binder)], body)` triples, keeping one plicity mark per payload binder (the [`InductArm`] invariant).
    pub(crate) fn induct_cases_marked<I, A, B>(cases: I) -> Vec<(Atom, InductArm)>
    where
        I: IntoIterator<Item = (A, Vec<(Plicity, Free)>, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        cases
            .into_iter()
            .map(|(atom, binders, body)| {
                let (plicities, names): (Vec<Plicity>, Vec<Free>) = binders.into_iter().unzip();
                let payload = names.iter().collect::<Vec<_>>();
                (
                    atom.into(),
                    InductArm {
                        body: Scope::close(Many(payload.len()), &payload, body.into()),
                        plicities,
                    },
                )
            })
            .collect()
    }

    /// Build a match's arity-1 motive scope from an optional source label: a named scope when the label is present, a constant one when not. Shared by every match constructor whose motive binds just the scrutinee — the canonical elaborated shape for an intrinsic carrier or an unindexed inductive.
    fn motive_scope(motive_binder: Option<&Free>, motive: Term) -> Scope<Many> {
        match motive_binder {
            Some(binder) => Scope::close(Many(1), &[binder], motive),
            None => Scope::constant(Many(1), motive),
        }
    }

    /// Build a match node around an already-built motive scope. The `*_scoped` constructors are `into_core`'s entry points: lowering carries the *written* motive term (see [`Term::match_motive_written`]) rather than a label and a body, because it cannot know the arity to close at. Every label-taking constructor above delegates here after building the canonical arity-1 scope.
    fn match_scoped(head: Term, motive: Scope<Many>, cases: Cases) -> Self {
        Self::from(Subterm::Match(Match {
            head,
            motive,
            cases,
        }))
    }

    /// Build the dependent `Bool` eliminator ([`Cases::Bool`]): a false arm and a true arm, neither binding anything — the motive alone sees the scrutinee.
    pub fn bool_match<H, M, F, T>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        false_case: F,
        true_case: T,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        F: Into<Term>,
        T: Into<Term>,
    {
        Self::bool_match_scoped(
            head,
            Self::motive_scope(motive_binder, motive.into()),
            false_case,
            true_case,
        )
    }

    /// Build the structural `Nat` eliminator ([`Carrier::Nat`]): a zero arm plus a successor arm closed over `(pred, ih)` — `Nat`'s generator carries no payload, so the cons arm binds one fewer variable than `Bin`/`Lst`'s.
    pub fn nat_match<H, M, ZC, SC>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        zero_case: ZC,
        pred_binder: &Free,
        ih_binder: &Free,
        succ_case: SC,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        ZC: Into<Term>,
        SC: Into<Term>,
    {
        Self::nat_match_scoped(
            head,
            Self::motive_scope(motive_binder, motive.into()),
            zero_case,
            pred_binder,
            ih_binder,
            succ_case,
        )
    }

    /// Build the structural `Lst` eliminator ([`Carrier::Lst`]): the element type `elem`, an empty arm, and a cons arm closed over `(head, tail, ih)` — the induction hypothesis at the tail.
    #[allow(clippy::too_many_arguments)]
    pub fn lst_match<H, M, EL, EC, CC>(
        head: H,
        elem: EL,
        motive_binder: Option<&Free>,
        motive: M,
        empty_case: EC,
        head_binder: &Free,
        tail_binder: &Free,
        ih_binder: &Free,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        EL: Into<Term>,
        EC: Into<Term>,
        CC: Into<Term>,
    {
        Self::lst_match_scoped(
            head,
            elem,
            Self::motive_scope(motive_binder, motive.into()),
            empty_case,
            head_binder,
            tail_binder,
            ih_binder,
            cons_case,
        )
    }

    /// Build a [`Cases::Switch`] match: sparse dispatch on specific literal `Nat` values with a mandatory default arm. The arms bind nothing — unlike [`Term::nat_match`], this is a case split, not induction.
    pub fn switch<H, M, I, B, D>(
        head: H,
        motive_binder: Option<&Free>,
        motive: M,
        cases: I,
        default: D,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (u32, B)>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::switch_scoped(
            head,
            Self::motive_scope(motive_binder, motive.into()),
            cases,
            default,
        )
    }

    /// Prepend a single non-recursive binding `binder = body : type_` in front of `tail`. `body` is deliberately *not* closed over `binder` — a `let` is non-recursive; use [`Term::rec`] for self-reference.
    ///
    /// When `tail` is itself a [`Let`] block, the binding is *merged* into it so a run of `let`s becomes one flat block, not a nest: `binder` becomes the block's new outermost binding, every existing binding and the tail step over one more binder (`capture`/reclose shift them by one), and free occurrences of `binder` in them bind to it. Building a block bottom-up — as `into_core` and the elaborator's rebuild both do — therefore yields a single `Let`, and the flatness is what bounds every later walk over it. A `tail` that is not a `Let` (a `!`-bind's `Apply`, a `rec`, a base term) starts a fresh one-binding block, so effect boundaries segment naturally.
    pub fn let_<T, B, U>(binder: &Free, type_: T, body: B, tail: U) -> Self
    where
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let type_ = type_.into();
        let body = body.into();
        let tail = tail.into();

        match Term::unwrap_or_clone(tail) {
            Subterm::Let(Let { bindings, tail }) => {
                let mut merged = Vec::with_capacity(bindings.len() + 1);
                merged.push(LetBinding::new(type_, body));

                for binding in bindings {
                    let (binding_type, binding_value) = binding.into_parts();
                    merged.push(LetBinding::new(
                        binding_type.capture(&[binder]),
                        binding_value.capture(&[binder]),
                    ));
                }

                Self::from(Subterm::Let(Let {
                    bindings: merged,
                    tail: tail.prepend(binder),
                }))
            }
            other => Self::from(Subterm::Let(Let {
                bindings: vec![LetBinding::new(type_, body)],
                tail: Scope::close(Many(1), &[binder], Term::from(other)),
            })),
        }
    }

    /// Build a [`Rec`] block from `(label, type, value)` items: every type, every value, and the tail are closed over the full label list, so the items may reference one another (and themselves) by name.
    pub fn rec<I, T, U, V>(items: I, tail: V) -> Self
    where
        I: IntoIterator<Item = (Free, T, U)>,
        T: Into<Term>,
        U: Into<Term>,
        V: Into<Term>,
    {
        let items = items
            .into_iter()
            .map(|(name, type_, value)| (name, type_.into(), value.into()))
            .collect::<Vec<_>>();

        let members = items.iter().map(|(name, _, _)| name).collect::<Vec<_>>();

        let group = RecGroup::new(
            items
                .iter()
                .map(|(_, type_, value)| RecMemberScopes {
                    type_: Scope::close(Many(members.len()), &members, type_.clone()),
                    body: Scope::close(Many(members.len()), &members, value.clone()),
                })
                .collect(),
        );

        Self::from(Subterm::Rec(Rec {
            group,
            tail: Scope::close(Many(members.len()), &members, tail.into()),
        }))
    }

    /// Member `index` of `group`, spelled as what it is: the group bound, with a tail that selects one member. `rec f and g; f`.
    ///
    /// This is an ordinary [`Rec`] node and not a form of its own, which is what keeps one typing rule from having to be written twice — a member occurrence is checked by the rule that checks the group, because it *is* the group. Opening this tail over the group's members yields the same term back, so a projection is the fixed point of `rec` unfolding and therefore a normal form; [`Term::as_rec_proj`] is how a reducer recognizes one without running the substitution to find out.
    pub fn rec_proj(group: RecGroup, index: usize) -> Self {
        assert!(
            index < group.length(),
            "recursive member index out of bounds"
        );

        let tail = Scope::constant(Many(group.length()), Self::var(Var::bound(index)));

        Self::from(Subterm::Rec(Rec { group, tail }))
    }
}

/// Hold each of `subterm`'s children somewhere else, then stand `subterm` down to a childless node.
///
/// Every child is cloned into `work` *before* the old value is released, so releasing it can only decrement — the reference `work` now holds is what stops the drop cascading. What is left behind is `Prop`: no children, and no allocation, because it is a variant with no payload.
fn detach_children(subterm: &mut Subterm, work: &mut Vec<Term>) {
    let detached = mem::replace(subterm, Subterm::Prop);
    detached.any_child_term(&mut |child| {
        work.push(child.clone());
        false
    });
}

/// Release the node's descendants iteratively.
///
/// A term is an `Rc` chain, so the derived drop recurses once per link and a deep term aborts the process on release exactly as deep equality used to on comparison. Emptying each node *before* it falls out of scope is what keeps its own drop from cascading: the husk left behind has no children to descend into, so every level is retired from this one loop.
///
/// Only a node this drop holds the sole reference to is emptied — `get_mut` answers precisely that question — so a subterm shared with a live term is left untouched and merely loses a reference.
///
/// Nothing here allocates, which matters because releasing terms is constant work in the compiler: standing a node down costs a `Prop` and a refcount bump per child. An earlier version substituted a placeholder `Term` instead, and building one per drop cost about a fifth of a prelude build.
impl Drop for Node {
    fn drop(&mut self) {
        // Nothing to dismantle, and the case every husk left below lands in.
        if !self.subterm.any_child_term(&mut |_| true) {
            return;
        }

        let mut work = Vec::new();
        detach_children(&mut self.subterm, &mut work);

        while let Some(mut term) = work.pop() {
            if let Some(node) = Rc::get_mut(&mut term.inner) {
                detach_children(&mut node.subterm, &mut work);
            }
        }
    }
}

impl Hash for Term {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.get_or_init_hash());
    }
}

/// Structural equality, walked with an explicit worklist.
///
/// The recursion this replaces was native, and a term deep enough overflowed the stack rather than answering — which a kernel must not do, and which the step budget cannot prevent, because depth is not steps. Every other derivation over a term already avoids native depth the same way ([`Term::fill_post_order`], `traverse_rewrite_spine`); this closes the last one that decides acceptance.
///
/// Two shortcuts carry the common cases before any of that: pointer identity (hash-consing makes shared structure genuinely common) and the cached hashes. Only a pair that is distinct-but-hash-equal reaches the walk.
impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        // One visit for the whole comparison: the placeholder is allocated once, and each node's children are taken off it in turn.
        let mut visit = Visit::masking(|_, _| None, Term::from(Subterm::Prop));
        // Entering as a `Subterm` is what keeps the node itself unmasked — the hook fires per `Term`, and the node being compared is not one.
        let mut mask = |subterm: &Subterm| {
            let masked = subterm.traverse(&mut visit);
            (masked, visit.take_masked_children())
        };

        let mut work = vec![(self.clone(), other.clone())];

        while let Some((this, that)) = work.pop() {
            if Rc::ptr_eq(&this.inner, &that.inner) {
                continue;
            }
            if this.get_or_init_hash() != that.get_or_init_hash() {
                return false;
            }

            let (this_masked, this_children) = mask(&this.inner.subterm);
            let (that_masked, that_children) = mask(&that.inner.subterm);

            // Derived equality, over nodes whose children are all placeholders: it compares this node's own payload — variant, names, plicities, levels, scope labels and arities — and bottoms out immediately.
            if this_masked != that_masked {
                return false;
            }
            // The masks agree, so the shapes agree and the child counts with them; the check is kept because equal counts are what makes the zip below a total comparison rather than a prefix of one.
            if this_children.len() != that_children.len() {
                return false;
            }

            work.extend(this_children.into_iter().zip(that_children));
        }

        true
    }
}

impl Eq for Term {}

impl AsRef<Subterm> for Term {
    fn as_ref(&self) -> &Subterm {
        &self.inner.subterm
    }
}

impl Deref for Term {
    type Target = Subterm;

    fn deref(&self) -> &Subterm {
        &self.inner.subterm
    }
}

impl From<Subterm> for Term {
    fn from(term: Subterm) -> Self {
        Self {
            span: None,
            inner: Rc::new(Node::new(term)),
        }
    }
}

/// The faithful rendering: core's own names, every universe shown. A diagnostic wanting source-style spelling goes through [`Term::spelled`].
impl fmt::Display for Term {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(
            print_term(self.clone(), 0, &Rc::new(Spelling::default())),
            formatter,
            4,
        )
    }
}

impl Term {
    /// This term paired with the [`Spelling`] it renders under — the parameter `Display::fmt` cannot take. See the axes documented in `print`.
    pub fn spelled<'a>(&'a self, spelling: &Rc<Spelling>) -> Spelled<'a, Term> {
        Spelled::new(self, spelling)
    }
}

impl fmt::Display for Spelled<'_, Term> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let printer = print_term(self.value().clone(), 0, self.spelling());
        match self.width() {
            // The width is a target, not a guarantee: content with no break point still overruns.
            Some(width) => run_printer_within(printer, formatter, 4, width),
            None => run_printer(printer, formatter, 4),
        }
    }
}

impl Bound for Term {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        if visit.memoizes() {
            let key = Rc::as_ptr(&self.inner) as usize;
            if let Some(hit) = visit.memo_get(key) {
                return hit;
            }
            let rebuilt = self.traverse_unmemoized(visit).canonicalized(visit);
            visit.memo_put(key, rebuilt.clone());
            return rebuilt;
        }

        self.traverse_unmemoized(visit).canonicalized(visit)
    }

    fn reach(&self) -> usize {
        self.scalars().reach
    }

    fn has_metavar(&self) -> bool {
        Term::has_metavar(self)
    }

    /// Cached alongside `hash`/`reach`: a closed subterm that `traverse`'s pruning short-circuit (above) hands back via `Rc::clone` keeps this same cell across every later traversal, so a term shared across many conversion goals — e.g. a `rec` group's own unchanging members, re-enqueued each round an unfolding cycle revisits them — pays this O(size) walk once rather than once per goal. Uniform in every term, not specific to recursive ones; see `Convert::history_key`.
    fn free_vars(&self) -> BTreeSet<Free> {
        self.get_or_init_free_vars().as_ref().clone()
    }
}

impl Term {
    /// This term over the canonical node of its structure, when the traversal is hash-consing; itself otherwise.
    ///
    /// The span is this occurrence's own. It lives on the `Term` wrapper rather than on the shared node, so canonicalizing never moves a span from one occurrence to another — which is what makes sharing by structure safe here at all.
    fn canonicalized<F>(self, visit: &Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match visit.share_structure(&self) {
            Some(canonical) => Term {
                span: self.span,
                inner: canonical.inner,
            },
            None => self,
        }
    }

    fn traverse_unmemoized<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        if let Some(replacement) = visit.rewrite_term(self) {
            return replacement;
        }
        if visit.universes_only() && !self.has_universe_data() {
            return self.clone();
        }
        if visit.prune() && self.reach() <= visit.term_depth() {
            return self.clone();
        }
        if (visit.universes_only() || visit.rewrites_terms())
            && matches!(&**self, Subterm::Apply(_) | Subterm::Variant(_))
        {
            return self.traverse_rewrite_spine(visit);
        }

        self.traverse_children(visit)
    }

    fn traverse_children<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        // Preserve the span across traversal; the rebuilt node is a fresh structure, so its cache starts empty.
        Self {
            span: self.span.clone(),
            inner: Rc::new(Node::new((**self).traverse(visit))),
        }
    }

    /// Rewrite a potentially deep constructor/application spine without putting one native frame per link on the stack. Term hooks and universe-level rewrites are structurally local at these nodes: neither former changes binder depth, and every nested scope still delegates to ordinary traversal.
    fn traverse_rewrite_spine<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        enum Work {
            Enter(Term, bool),
            Exit(Term, usize),
        }

        let mut work = Vec::from([Work::Enter(self.clone(), true)]);
        let mut rewritten = Vec::new();

        while let Some(next) = work.pop() {
            match next {
                Work::Enter(term, prechecked) => {
                    if !prechecked {
                        if visit.memoizes()
                            && let Some(hit) = visit.memo_get(Rc::as_ptr(&term.inner) as usize)
                        {
                            rewritten.push(hit);
                            continue;
                        }
                        if let Some(replacement) = visit.rewrite_term(&term) {
                            rewritten.push(replacement);
                            continue;
                        }
                        if visit.universes_only() && !term.has_universe_data() {
                            rewritten.push(term);
                            continue;
                        }
                    }

                    let children = match &*term {
                        Subterm::Apply(Apply { head, params, .. }) => {
                            let mut children = Vec::with_capacity(params.len() + 1);
                            children.push(head.clone());
                            children.extend(params.iter().cloned());
                            children
                        }
                        Subterm::Variant(Variant {
                            params, payload, ..
                        }) => params.iter().chain(payload).cloned().collect(),
                        _ => {
                            let key = Rc::as_ptr(&term.inner) as usize;
                            let rebuilt = term.traverse_children(visit);
                            visit.memo_put(key, rebuilt.clone());
                            rewritten.push(rebuilt);
                            continue;
                        }
                    };

                    let child_count = children.len();
                    work.push(Work::Exit(term, child_count));
                    work.extend(
                        children
                            .into_iter()
                            .rev()
                            .map(|child| Work::Enter(child, false)),
                    );
                }
                Work::Exit(term, child_count) => {
                    let child_start = rewritten
                        .len()
                        .checked_sub(child_count)
                        .expect("each universe traversal frame owns its child results");
                    let mut children = rewritten.drain(child_start..);
                    let subterm = match &*term {
                        Subterm::Apply(Apply { plicities, .. }) => {
                            let head = children
                                .next()
                                .expect("an application traversal preserves its head");
                            Subterm::Apply(Apply {
                                head,
                                params: children.collect(),
                                plicities: plicities.clone(),
                            })
                        }
                        Subterm::Variant(Variant {
                            name,
                            universes,
                            params,
                            tag,
                            ..
                        }) => {
                            let universes = if visit.erases_universes() {
                                Vec::new()
                            } else {
                                universes
                                    .iter()
                                    .map(|level| visit.visit_level(level))
                                    .collect()
                            };
                            let params = children.by_ref().take(params.len()).collect();
                            Subterm::Variant(Variant {
                                name: name.clone(),
                                universes,
                                params,
                                tag: tag.clone(),
                                payload: children.collect(),
                            })
                        }
                        _ => unreachable!("only spine nodes create universe traversal frames"),
                    };
                    let rebuilt = Self {
                        span: term.span.clone(),
                        inner: Rc::new(Node::new(subterm)),
                    };
                    visit.memo_put(Rc::as_ptr(&term.inner) as usize, rebuilt.clone());
                    rewritten.push(rebuilt);
                }
            }
        }

        rewritten
            .pop()
            .expect("a universe spine traversal returns its root")
    }
}

impl Term {
    /// Fill the memoized free-variable set bottom-up on a post-order [`Term::walk`]: each node's set is its children's sets unioned with its own identity (if it is a free `Var`), so filling reads the children's cached sets in O(children) rather than re-walking the subtree — a deep spine memoizes without native recursion. The filled bit is the walk's memo, exactly as in [`Term::warm_scalars`].
    fn warm_frees(&self) {
        self.walk(
            &mut (),
            |_, term| {
                if term.inner.frees.is_filled() {
                    Enter::Skip(())
                } else {
                    Enter::Descend
                }
            },
            |_, term, _| {
                term.inner
                    .frees
                    .fill(term.inner.subterm.free_vars_from_children())
            },
        );
    }

    fn get_or_init_free_vars(&self) -> &Rc<BTreeSet<Free>> {
        self.warm_frees();
        self.inner
            .frees
            .get()
            .expect("warm_frees fills the free-variable cache")
    }

    /// Whether `name` occurs free in this term, through the same memoized set [`Bound::free_vars`] fills — but as a membership probe instead of a set clone ([`FreeCache::contains`]): `define`'s selective reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk it avoids.
    pub fn mentions_free(&self, name: &Free) -> bool {
        self.warm_frees();
        self.inner.frees.contains(name)
    }

    /// The free-variable identities of this term. Inherent so a `term.free_vars()` call routes through the memoized, iteratively-filled set (this and the [`Bound`] impl agree) rather than deref-ing to the uncached, recursive [`Subterm::free_vars`] when the `Bound` trait is out of scope.
    pub fn free_vars(&self) -> BTreeSet<Free> {
        self.get_or_init_free_vars().as_ref().clone()
    }

    /// The ids of every metavariable in this term. Inherent, and gated on the memoized [`has_metavar`](Self::has_metavar): a ground term (every data spine) short-circuits without walking, so the enumeration only ever recurses through metavariable-bearing structure, whose depth is bounded by the written program.
    pub fn metavars(&self) -> BTreeSet<MetaId> {
        let mut ids = BTreeSet::new();
        if self.has_metavar() {
            self.inner.subterm.collect_metavars(&mut ids);
        }
        ids
    }

    /// Whether any metavariable in this term satisfies `pred`. Inherent and gated on [`has_metavar`](Self::has_metavar) like [`metavars`](Self::metavars), and — since `Subterm::any_metavar`'s recursion re-enters through each child `Term` — every ground subtree it reaches short-circuits too.
    pub fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        self.has_metavar() && self.inner.subterm.any_metavar(pred)
    }
}

/// An unresolved infix application `left <op> right`. Elaboration infers a shared operand type for the two sides and rebuilds the node as a concept method call (`a + b` ≙ `Add/add(a, b)`; `&&`/`||` alone are hardcoded on `Bool` — see `elaborate_infix`); the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Infix {
    pub op: NumOp,
    pub left: Term,
    pub right: Term,
}

/// A polymorphic numeric literal: an integer `magnitude` with an optional written sign. Resolved to a concrete `Nat`/`Int`/`Flt` intrinsic by `elaborate_numlit` once the expected type is known (or defaulted by shape). Decimal literals are *not* `NumLit` — they parse straight to `Intrinsic::Flt`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct NumLit {
    #[cfg_attr(feature = "archive", rkyv(with = BigUintBytes))]
    pub magnitude: BigUint,
    /// A `+`/`-` was written: drops `Nat` from the candidate set and defaults the literal to `Int`.
    pub signed: bool,
    /// The written sign was `-` (a negative literal can never be a `Nat`).
    pub negative: bool,
}

/// A postfix `!` sequencing site, already hoisted by lowering: `action` is the sequenced description, `continuation` the rest of its region as an ordinary one-parameter function (domain a lowering-minted hole). Consumed by `elaborate_bang`, which replaces it with the `/syn/Monad/bind` application the lowerer once spelled directly — the construction moved behind elaboration so the sequencing survives to the stage that can make type-directed decisions about it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Bang {
    pub action: Term,
    pub continuation: Term,
}

/// A lowering-born constructor consumed by elaboration: born in `into_core`, eliminated by `elaborate`, never legitimate in reduced, converted, zonked, or erased terms, and refused at the kernel boundary. Grouping the members under one `Subterm` variant lets every post-elaboration consumer dismiss the class wholesale — one refusal arm at the kernel, one `unreachable!` in each downstream stage — so a future transient extends lowering, elaboration, and display without touching them. `Metavar` is deliberately not a member: conversion parks on metavariables and zonk consumes them, so its lifecycle is elaboration-internal rather than pre-elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Transient {
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_numlit`.
    NumLit(NumLit),
    /// A postfix `!` sequencing site; consumed by `elaborate_bang`.
    Bang(Bang),
}

impl Transient {
    /// The direct child terms, for the structural walks that must traverse a lowered (or display-folded) term. Transients are plain data over their children — none binds a variable of its own (`Bang`'s continuation is an ordinary `Func`, which carries the binder) — so the walks need no scope handling here.
    pub fn subterms(&self) -> impl Iterator<Item = &Term> {
        let children = match self {
            Transient::Infix(Infix { left, right, .. }) => [Some(left), Some(right)],
            Transient::NumLit(_) => [None, None],
            Transient::Bang(Bang {
                action,
                continuation,
            }) => [Some(action), Some(continuation)],
        };
        children.into_iter().flatten()
    }

    /// Rebuild this transient with every child term mapped, for the structural rewrites.
    pub fn map_subterms(&self, f: &mut impl FnMut(&Term) -> Term) -> Transient {
        match self {
            Transient::Infix(Infix { op, left, right }) => Transient::Infix(Infix {
                op: *op,
                left: f(left),
                right: f(right),
            }),
            Transient::NumLit(num_lit) => Transient::NumLit(num_lit.clone()),
            Transient::Bang(Bang {
                action,
                continuation,
            }) => Transient::Bang(Bang {
                action: f(action),
                continuation: f(continuation),
            }),
        }
    }
}

/// `plicities` parallels the telescope, one mark per binder; the builder asserts the lengths agree. `Telescope` itself is unchanged. Erasure is sort-driven (a proof or a type erases), so a function type carries no runtime-multiplicity marks of its own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct FuncType {
    pub telescope: Telescope<Term>,
    pub plicities: Vec<Plicity>,
}

/// A function literal: the parameter annotations and the body as one [`Telescope`] (each entry a parameter type, the `Done` payload the body), with `plicities` paralleling the telescope one mark per binder — the builder asserts the lengths agree. Plicity is part of a function's identity and calling convention: a lambda carries the marks its binders were written with (before elaboration) and the complete canonical marks of its checked type (after elaboration, once omitted hidden binders are inserted). Derived `Eq`/`Hash` include `plicities` so that two lambdas differing only in a written mark never share an elaboration-cache entry.
///
/// Erasure ignores `plicities`; its keep/drop decisions come from the checked function type and sort information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Func {
    pub telescope: Telescope<Term>,
    pub plicities: Vec<Plicity>,
}

/// `plicities` parallels `params`, one mark per argument — the call-site `@` marks. Core must carry them (rather than `into_core` resolving them) because `into_core` is type-blind: only the elaborator, holding the head's function type, can decide which binder an `@`-argument fills.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
    pub plicities: Vec<Plicity>,
}

/// A dependent product (Σ-type). Erasure is sort-driven: a proof or type-valued field is a *subset type* witness — dropped at erasure, leaving the relevant fields (and collapsing to the bare field when only one remains).
///
/// Unlike binder hints elsewhere, field labels are the target of `.label` resolution during elaboration, so they are part of the type's identity: `Eq`/`Hash` reassert them on top of the label-blind [`Telescope`] identity. Otherwise the reduction memo could hand elaboration a twin type whose labels differ, and a well-typed projection would fail to resolve.
#[derive(Debug, Clone, Eq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.telescope == other.telescope && self.telescope.labels() == other.telescope.labels()
    }
}

impl Hash for TupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.telescope.hash(state);
        self.telescope.labels().hash(state);
    }
}

/// `names` carries the literal's written field names (`(status = 0, …)`) from `into_core` to elaboration, which checks them against the expected tuple type's labels and rebuilds the literal name-free. Empty means "no names written" — the invariant for every internally-built and post-elaboration tuple.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Tuple {
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// A projection's field is positional in every post-elaboration term; the `Label` form exists only between `into_core` and `elaborate`, which resolves it against the head's tuple type and rebuilds it as `Index`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Field {
    Index(usize),
    Label(String),
}

/// A projection out of a tuple. See [`Field`] for why the field is positional in every post-elaboration term.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Proj {
    pub head: Term,
    pub field: Field,
}

/// An inductive type as an intrinsic normal form. Built inside the automatically-generated type-constructor function's body. Users never write one directly — they write `Result(A, E)` and the type-constructor function reduces to this. Two `InductType`s are convertible iff same `name` and pointwise-convertible `params` and `indices`.
///
/// `params` are uniform across constructors; `indices` are the per-case constrained binders — each constructor's registry terminal states its own index expressions. Use sites never distinguish them (`Vec(Bin, 3)` is one flat application of the type-constructor function); the split lives here and in the registry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct InductType {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub indices: Vec<Term>,
}

/// A constructor application as an intrinsic normal form. Built inside the automatically-generated value-constructor function's body. Users never write one directly — they write `Result/success(value)` and the constructor function reduces to this.
///
/// `name` and `params` are recoverable from the term's inferred type; they are stored redundantly on purpose, so `convert` stays purely structural (no context lookups mid-comparison).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Variant {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// A struct type as an intrinsic normal form (cf. [`InductType`], no indices). Built inside the generated type-former's body; users write `Pair(A, B)` and the former reduces to this. Convertible iff same `name` and pointwise-convertible `params`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct StructType {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
}

/// One written struct-literal entry, parallel to [`Struct::fields`]: a plain positional field carrying its optional written label, an explicit `use <term>` fill that pairs with the concept's next `use`-marked field position, or a `..base` spread whose paired term is the base to copy the unwritten fields from (riding in `fields` keeps it visible to every term traversal). A `Spread`, if present, is `entries[0]` — enforced at elaboration, not by construction. Pre-elaboration metadata only, like written field names on [`Tuple`]; elaboration rebuilds the value entry-free.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum StructEntry {
    Field(Option<String>),
    Use,
    Spread,
}

/// A struct value as an intrinsic normal form (cf. [`Variant`], no tag). `name`/`params` are recoverable from the inferred type but stored redundantly so `convert` stays purely structural.
///
/// `entries` carries the literal's written entry shapes from `into_core`: elaboration checks plain fields positionally against the declared labels, pairs `use` entries with the concept's `use`-marked positions, and rebuilds the value entry-free. Empty means "all plain, no names written" — the invariant for every internally-built and post-elaboration struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Struct {
    pub name: Global,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub entries: Vec<StructEntry>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive and differs only in its [`Cases`] payload.
///
/// An *elaborated* motive is closed at the eliminator's own arity: the scrutinee's indices in declaration order, then the scrutinee. That is 1 for every intrinsic carrier and for an unindexed inductive, and `n_indices + 1` for an indexed one. Parameters are never abstracted — they are uniform across constructors and fixed by the scrutinee's type, so the motive body refers to them through the ambient scope like any other term.
///
/// Before elaboration the motive is instead the *written term*, carried in an arity-0 scope — see [`Term::match_motive_written`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Match {
    pub head: Term,
    pub motive: Scope<Many>,
    pub cases: Cases,
}

/// One enumerated arm of a [`Cases::Induct`]: the arm body closed over its payload binders, plus a plicity vector paralleling those binders one mark per slot. `plicities.len()` equals `body.arity()`. Before elaboration the marks are the written constructor-pattern plicities; after elaboration they are the constructor's canonical payload plicities. Reduction and erasure open the body positionally and never read the marks; conversion compares them alongside the bodies. Kept beside the body (rather than in a second map) so the two can never drift apart.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct InductArm {
    pub body: Scope<Many>,
    pub plicities: Vec<Plicity>,
}

impl InductArm {
    /// The arm's payload arity — equal to `plicities.len()`.
    pub fn arity(&self) -> usize {
        self.body.arity()
    }

    /// Open the arm body at its payload binders, positionally (plicity is not consulted by reduction or erasure).
    pub fn open(&self, args: &[&Term]) -> Term {
        self.body.open(args)
    }

    /// The arm body's free-variable reach, past its payload binders.
    pub(crate) fn reach(&self) -> usize {
        self.body.reach()
    }

    /// The arm's payload binder hints, in order.
    pub fn hint_iter(&self) -> impl Iterator<Item = Option<&str>> {
        self.body.hint_iter()
    }

    /// The arm's payload binders, in order.
    pub(crate) fn binder_iter(&self) -> impl Iterator<Item = Option<&Free>> {
        self.body.binder_iter()
    }

    /// Rebuild the arm with its whole body scope replaced, preserving the plicity vector (the traversal-side reconstruction helper).
    pub fn with_body(&self, body: Scope<Many>) -> Self {
        InductArm {
            body,
            plicities: self.plicities.clone(),
        }
    }
}

/// The arm payload of a [`Match`] — the only part that differs between the elimination forms (the scrutinee and motive live on `Match` itself). Which variant a match carries decides both its reduction rule and how erasure lowers it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Cases {
    /// Dependent elimination of `Bool`: a false arm and a true arm.
    Bool { false_case: Term, true_case: Term },
    /// Sparse dispatch on specific `Nat` values with a default arm.
    Switch {
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
    /// The intrinsic eliminator of a nominal inductive: one arm per constructor, each arm's arity equal to that constructor's payload arity. `default` is the optional catch-all arm (`| _ =>`, mirroring [`Cases::Switch`]'s): present iff the surface match ended in a bare `_`. It binds nothing and stands in for every constructor tag absent from `cases`; `None` means the arms structurally cover every constructor (a true elimination). The enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones, so a catch-all is legal on an indexed family too.
    Induct {
        /// The enumerated arms, in the owning inductive's *declaration order* — the same order `InductDecl::constructor_order` reports, which is what makes this a canonical form: two matches whose arms are written in different source order elaborate to the same sequence, so arm order never enters term identity. Elaboration establishes that by building the arms from `constructor_order` rather than from the written order (`elaborate_induct_match`). A subsequence is legal — an arm may be absent under a `default` or a Rung-C prune.
        cases: Vec<(Atom, InductArm)>,
        default: Option<Term>,
    },
    /// Structural induction on a native free-monoid intrinsic (`Nat`/`Lst`/ `Bin`): the `carrier` selects the intrinsic and carries both its parameters (`Lst`'s element type) and its two arms — an identity arm plus a cons arm binding the head generator (absent for `Nat`, whose unary generator carries no payload), the tail, and the induction hypothesis at the tail.
    FreeMonoid { carrier: Carrier },
}

/// The native free-monoid intrinsic a `Cases::FreeMonoid` eliminates, with its type parameters and its two eliminator arms. `Nat` is the free monoid on one (payload-less) generator; `Bin` carries none; `Lst` carries its element type. Each variant pairs an identity arm (`empty_case`) with a cons arm whose arity is fixed by the carrier — `Scope<Two>` for `Nat` (predecessor, ih), `Scope<Three>` for `Bin`/`Lst` (head, tail, ih).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Carrier {
    Nat {
        empty_case: Term,
        cons_case: Scope<Two>,
    },
    Bin {
        grain: Grain,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
    Lst {
        elem: Term,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
}

/// A straight-line block of `let` bindings: `bindings` in written order, then a `tail` continuation in scope of all of them. Binding `i` is stored under the `i` binders before it — its `type_` and `value` may reference bindings `0..i` but never binding `i` itself; a `let` is non-recursive, self- and mutual reference is [`Rec`]'s job. A whole run of source `let`s is one `Let`, not a nest, so every walk over it (`traverse`/`reach`/`reduce`/ `erase`/`elaborate`) is a loop over `bindings` rather than one native stack frame per binding — which is what keeps a long local `let` sequence from overflowing the stack.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Let {
    pub bindings: Vec<LetBinding>,
    pub tail: Scope<Many>,
}

/// One non-recursive local binding: its declared type and its value.
///
/// A local binding is monomorphic. Universe polymorphism is a property of *declarations*, which are frozen into the prelude archive and re-instantiated by later programs; a local binding has no such use sites, and cumulativity already admits the uses a local scheme once served — for `let id : (@A : Type, A) -> A` applied to both `Prop` and `Type 0`, a single `A : Type 1` accepts both, and the level order is linear so a sup always exists.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct LetBinding {
    type_: Term,
    value: Term,
}

impl LetBinding {
    pub fn new(type_: Term, value: Term) -> Self {
        Self { type_, value }
    }

    pub fn type_(&self) -> &Term {
        &self.type_
    }

    pub fn value(&self) -> &Term {
        &self.value
    }

    fn into_parts(self) -> (Term, Term) {
        (self.type_, self.value)
    }
}

/// One member of a recursive group as the knot stores it. Both scopes are closed over the whole group, so any member may reference any other.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecMemberScopes {
    pub type_: Scope<Many>,
    pub body: Scope<Many>,
}

/// The shared knot of a mutually-recursive group. Every member type and body is scoped over the full group. `Rc` sharing is an implementation detail; equality and hashing remain structural through the scoped items.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecGroup {
    scheme: UniverseScheme<Rc<Vec<RecMemberScopes>>>,
}

impl RecGroup {
    pub fn new(items: Vec<RecMemberScopes>) -> Self {
        Self {
            scheme: UniverseScheme::monomorphic(Rc::new(items)),
        }
    }

    /// This group with every member's closed scopes rewritten in place.
    ///
    /// The bodies stay closed throughout. Terms under a scope carry loose de Bruijn indices, and two structurally identical such terms — indices included — denote the same thing at the same depth, so canonicalizing them together is sound without opening.
    pub fn map_members(&self, mut map: impl FnMut(&Term) -> Term) -> Self {
        Self {
            scheme: UniverseScheme {
                context: self.scheme.context.clone(),
                value: Rc::new(
                    self.iter()
                        .map(|member| RecMemberScopes {
                            type_: member.type_.map_body(&mut map),
                            body: member.body.map_body(&mut map),
                        })
                        .collect(),
                ),
            },
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RecMemberScopes> + Clone {
        self.scheme.value.iter()
    }

    fn item(&self, index: usize) -> &RecMemberScopes {
        self.scheme
            .value
            .get(index)
            .expect("recursive member index in bounds")
    }

    pub fn universe_context(&self) -> &UniverseContext {
        &self.scheme.context
    }

    pub fn with_universe_context(mut self, universe_context: UniverseContext) -> Self {
        self.scheme.context = universe_context;
        self
    }

    pub fn length(&self) -> usize {
        self.iter().len()
    }

    /// One term per member, each denoting that member and needing nothing in scope to do it — what every scope in this group opens over.
    pub fn members(&self) -> Vec<Term> {
        (0..self.length())
            .map(|index| Term::rec_proj(self.clone(), index))
            .collect()
    }

    pub fn member_type(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).type_.open(&refs)
    }

    pub fn member_body(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).body.open(&refs)
    }

    /// The universe scheme this group was generalized under — what an instance must satisfy.
    pub fn universes(&self) -> &UniverseContext {
        &self.scheme.context
    }

    pub fn instantiate_universes(&self, arguments: &[Level]) -> Result<Self, UniverseError> {
        if arguments.len() != self.scheme.context.parameter_count {
            return Err(UniverseError::InstanceArity {
                expected: self.scheme.context.parameter_count,
                got: arguments.len(),
            });
        }
        Ok(Self {
            scheme: UniverseScheme {
                value: self
                    .iter()
                    .map(|member| {
                        Ok(RecMemberScopes {
                            type_: member.type_.try_map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                            body: member.body.try_map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                        })
                    })
                    .collect::<Result<Vec<_>, UniverseError>>()?
                    .into(),
                context: UniverseContext::empty(),
            },
        })
    }

    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        let universe_arity = self.scheme.context.parameter_count;
        visit.enter_universe_scope(universe_arity);
        let mut result = Self::new(
            self.iter()
                .map(|member| RecMemberScopes {
                    type_: visit.visit_scope(&member.type_),
                    body: visit.visit_scope(&member.body),
                })
                .collect(),
        );
        result.scheme.context = if visit.erases_universes() {
            UniverseContext::empty()
        } else {
            self.scheme
                .context
                .map_levels(|level| visit.visit_level(level))
        };
        visit.leave_universe_scope(universe_arity);
        result
    }

    fn reach(&self) -> usize {
        self.iter()
            .map(|member| member.type_.reach().max(member.body.reach()))
            .max()
            .unwrap_or(0)
    }
}

/// A block of mutually recursive bindings with a tail in scope of the shared group.
///
/// This is the *only* recursion form. A demanded member occurrence is this same node with a tail that selects one member — see [`Term::rec_proj`] — rather than a form of its own, so the rule that checks a group is the rule that checks an occurrence of it. A self-describing occurrence node was the earlier design, and what it cost is worth recording: a node that is well-formed standing alone is a node no scope gates, and the kernel typed one from the group it carried without ever checking that group.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Rec {
    pub group: RecGroup,
    pub tail: Scope<Many>,
}

impl Rec {
    /// The member index this block's tail selects, when it selects one rather than computing something of its own — see [`Term::rec_proj`].
    pub fn as_proj(&self) -> Option<usize> {
        let Subterm::Var(var) = &**self.tail.body() else {
            return None;
        };

        var.as_bound().filter(|index| *index < self.group.length())
    }
}

/// The folded fixed point selecting one member of a [`RecGroup`]. This is a structural term, not an allocation identity: separately allocated alpha-equivalent groups compare equal.
/// Provenance of an inserted implicit argument: the applied function (`func`) had no `@`-argument for its implicit binder `binder` at some call site, so the elaborator filled the slot with a fresh metavariable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct ImplicitOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of an inserted witness argument: the applied function (`func`) had no `use`-argument for its witness binder `binder` at some call site, so the elaborator filled the slot with a fresh metavariable and registered a resolution goal for it. An occurrence still unsolved at zonk reports as a missing witness (naming the goal type from the birth record) rather than an uninferred implicit.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct WitnessOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of a marked metavariable — which mechanism created it, deciding how zonk reports it: an unsolved `Implicit`/`Witness` survivor names the binder it filled, while a `Goal` is reported unconditionally.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum MetavarOrigin {
    Implicit(ImplicitOrigin),
    Witness(WitnessOrigin),
    /// A written goal `?` (`into_core` mints it via [`Term::goal`]): the user asked what elaboration determines here, so zonk errors with the goal's scope, type, and solution — solved or not — instead of splicing.
    Goal,
}

/// A metavariable's identity: a dense index into the `Context`'s `MetaStore`, minted monotonically by an [`Entropy`](Entropy). A newtype so it can never be confused with the other `usize`-shaped notions the kernel juggles (de Bruijn indices, telescope arities, variant tags, `Nat` magnitudes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub struct MetaId(pub usize);

impl From<usize> for MetaId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for MetaId {
    fn mint(entropy: usize) -> Self {
        Self(entropy)
    }
}

impl fmt::Display for MetaId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm, born from a surface hole `?` and (possibly) solved by unification. The solution, when one exists, lives in the `Context`'s `MetaStore`, keyed by `id`, spelled with the *birth telescope's* free names.
///
/// `origin` rides with the node: `Some` iff the metavariable was marked at its mint — an elaborator-inserted implicit/witness argument (zonk's unsolved-hole report then names the binder instead of a bare id) or a written goal `?` (zonk reports it unconditionally). Each id is minted exactly once (`into_core` desugared holes with `None` and written goals with `Some(Goal)`, core insertions above the floor `into_core` returns with `Some`), so every occurrence of an id carries the same origin and the derived equality never splits an id.
///
/// `spine` is the delayed substitution — one term per binder of the birth telescope (`MetaEntry::telescope` order), recording what that binder corresponds to at this occurrence. Identity (`Var::free(name)`) at birth. The entries are ordinary term content: `traverse` walks them, so `close` captures them and `open` substitutes them, and the mapping survives re-closing under fresh names — which is what lets a solution mentioning a sibling binder resolve correctly wherever the occurrence ends up. An empty spine is a not-yet-birthed `into_core` hole and resolves as the identity.
///
/// The spine is `Rc`-shared: every meta born under the same Γ shares one identity-spine allocation (see `Context::identity_snapshot`), which is what keeps minting metavariables O(1) instead of O(|Γ|).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Metavar {
    pub id: MetaId,
    pub spine: Rc<Vec<Term>>,
    pub origin: Option<MetavarOrigin>,
}

/// An internal, occurrence-specific instantiation of a universe-polymorphic binding. The ordinary term binder structure remains entirely in `head`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseInst {
    pub head: Term,
    pub levels: Vec<Level>,
}

/// The actual node of the core term language — one variant per term former. [`Term`] wraps a `Subterm` in an `Rc` with cached hash/reach and an optional span, and `Deref`s here, so pattern matches are written against `Subterm` while construction goes through `Term`'s smart constructors. The final variant groups the elaboration-transient constructors under [`Transient`]: born in `into_core`, consumed by `elaborate`, never seen by reduce/convert/zonk/erase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Subterm {
    Type(Level),
    Prop,
    Intrinsic(Intrinsic),
    /// A store-described host call: the row's [`WireSignature`](curios_abi::WireSignature) fixes the operand types checked at elaboration and the result shape (unit, bare value, or named record). Effectful, so reducing one at the type level is an error; it becomes a host call only at erasure.
    ///
    /// A term former rather than a [`Intrinsic`] variant, because it is the one construct here whose meaning is *not* fixed by the enum that holds it: every intrinsic has a signature this crate spells, while a foreign call reads its own off the ABI row it carries. Nothing about it is closed, so it does not belong in a closed set.
    Foreign(Arc<ForeignFunction>, Vec<Term>),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    InductType(InductType),
    Variant(Variant),
    Match(Match),
    StructType(StructType),
    Struct(Struct),
    Proj(Proj),
    Let(Let),
    Rec(Rec),
    UniverseInst(UniverseInst),
    Var(Var),
    Metavar(Metavar),
    /// The elaboration-transient constructors, grouped so post-elaboration consumers dismiss the class with one arm.
    Transient(Transient),
}

impl Subterm {
    /// The group and index this term projects, when it is a member selection rather than a `rec` block with a tail of its own.
    ///
    /// On [`Subterm`] rather than [`Term`] so both reach it: a `Term` derefs here.
    pub fn as_rec_proj(&self) -> Option<(&RecGroup, usize)> {
        let Subterm::Rec(rec) = self else {
            return None;
        };

        rec.as_proj().map(|index| (&rec.group, index))
    }

    fn any_direct_universe_meta(&self, pred: &mut impl FnMut(UniverseMetaId) -> bool) -> bool {
        let mut level_matches = |level: &Level| level.metas().any(&mut *pred);
        let context_matches =
            |context: &UniverseContext, level_matches: &mut dyn FnMut(&Level) -> bool| {
                context.constraints.iter().any(|constraint| {
                    level_matches(&constraint.lower) || level_matches(&constraint.upper)
                })
            };
        match self {
            Subterm::Type(level) => level_matches(level),
            Subterm::UniverseInst(UniverseInst { levels, .. })
            | Subterm::InductType(InductType {
                universes: levels, ..
            })
            | Subterm::Variant(Variant {
                universes: levels, ..
            })
            | Subterm::StructType(StructType {
                universes: levels, ..
            })
            | Subterm::Struct(Struct {
                universes: levels, ..
            }) => levels.iter().any(level_matches),
            Subterm::Rec(Rec { group, .. }) => {
                context_matches(group.universe_context(), &mut level_matches)
            }
            _ => false,
        }
    }

    pub fn as_nat(&self) -> Option<Nat> {
        match self {
            Subterm::Intrinsic(Intrinsic::Nat(nat)) => Some(nat.clone()),
            _ => None,
        }
    }

    pub(crate) fn as_int(&self) -> Option<Int> {
        match self {
            Subterm::Intrinsic(Intrinsic::Int(value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Subterm::Intrinsic(Intrinsic::Bool(value)) => Some(*value),
            _ => None,
        }
    }

    /// The free-variable identities occurring in this subterm — the inherent-method spelling of [`Bound::free_vars`], callable without importing the trait.
    pub fn free_vars(&self) -> BTreeSet<Free> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect the head name of every inductive/struct *construction* and *type-former normal form* occurring in this subterm. These names are not `Var`s (they live in the registry, not the variable graph), so they do not appear in `free_vars`; the reachability prune (`order_flat_items`) needs them as edges so a definition that *builds* a `Struct`/`Variant` (e.g. the string-literal meta-emitter's `/syn/Str/Str`) keeps the backing type-former and field-type definitions alive even when no `Var` mentions them.
    pub fn construction_names(&self) -> BTreeSet<Global> {
        let mut names = BTreeSet::new();
        self.collect_construction_names(&mut names);
        names
    }

    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => {}
            Subterm::UniverseInst(UniverseInst { head, .. }) => {
                head.collect_construction_names(names);
            }
            Subterm::Transient(transient) => {
                transient
                    .subterms()
                    .for_each(|child| child.collect_construction_names(names));
            }
            Subterm::Metavar(Metavar { spine, .. }) => {
                spine
                    .iter()
                    .for_each(|t| t.collect_construction_names(names));
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.collect_construction_names(names),
            Subterm::Foreign(_, args) => args
                .iter()
                .for_each(|arg| arg.collect_construction_names(names)),
            Subterm::Func(Func { telescope, .. }) => telescope.collect_construction_names(names),
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope.collect_construction_names(names)
            }
            Subterm::Apply(Apply { head, params, .. }) => {
                head.collect_construction_names(names);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::TupleType(TupleType { telescope, .. }) => {
                telescope.collect_construction_names(names)
            }
            Subterm::Tuple(Tuple { fields, .. }) => {
                fields
                    .iter()
                    .for_each(|f| f.collect_construction_names(names));
            }
            Subterm::Proj(Proj { head, .. }) => head.collect_construction_names(names),
            Subterm::InductType(InductType {
                name,
                params,
                indices,
                ..
            }) => {
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                indices
                    .iter()
                    .for_each(|i| i.collect_construction_names(names));
            }
            Subterm::Variant(Variant {
                name,
                params,
                payload,
                ..
            }) => {
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                payload
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::StructType(StructType { name, params, .. }) => {
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::Struct(Struct {
                name,
                params,
                fields,
                ..
            }) => {
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                fields
                    .iter()
                    .for_each(|f| f.collect_construction_names(names));
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                head.collect_construction_names(names);
                motive.body().collect_construction_names(names);
                match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => {
                        false_case.collect_construction_names(names);
                        true_case.collect_construction_names(names);
                    }
                    Cases::Switch { cases, default } => {
                        cases
                            .values()
                            .for_each(|b| b.collect_construction_names(names));
                        default.collect_construction_names(names);
                    }
                    Cases::Induct { cases, default } => {
                        cases
                            .iter()
                            .for_each(|(_, s)| s.body.body().collect_construction_names(names));
                        default
                            .iter()
                            .for_each(|d| d.collect_construction_names(names));
                    }
                    Cases::FreeMonoid { carrier } => match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::Bin {
                            empty_case,
                            cons_case,
                            ..
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::Lst {
                            elem,
                            empty_case,
                            cons_case,
                        } => {
                            elem.collect_construction_names(names);
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                    },
                }
            }
            Subterm::Let(Let { bindings, tail, .. }) => {
                for binding in bindings {
                    binding.type_().collect_construction_names(names);
                    binding.value().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::Rec(Rec { group, tail }) => {
                for member in group.iter() {
                    member.type_.body().collect_construction_names(names);
                    member.body.body().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
        }
    }

    /// Whether any metavariable occurring in this subterm satisfies `pred`, stopping at the first hit. The early-exit dual of `collect_metavars` (which is this with a collector that never stops): the reducer's memo gate uses it to reject caching a WHNF that still names an unsolved metavariable, without allocating the full id set.
    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { id, spine, .. }) => {
                pred(*id) || spine.iter().any(|t| t.any_metavar(pred))
            }
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => false,
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.any_metavar(pred),
            Subterm::Transient(transient) => {
                let mut children = transient.subterms();
                children.any(|child| child.any_metavar(pred))
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.any_metavar(pred),
            Subterm::Foreign(_, args) => args.iter().any(|arg| arg.any_metavar(pred)),
            Subterm::Func(Func { telescope, .. }) => telescope.any_metavar(pred),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.any_metavar(pred),
            Subterm::Apply(Apply { head, params, .. }) => {
                head.any_metavar(pred) || params.iter().any(|p| p.any_metavar(pred))
            }
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.any_metavar(pred),
            Subterm::Tuple(Tuple { fields, .. }) => fields.iter().any(|f| f.any_metavar(pred)),
            Subterm::Proj(Proj { head, .. }) => head.any_metavar(pred),
            Subterm::InductType(InductType {
                params, indices, ..
            }) => {
                params.iter().any(|p| p.any_metavar(pred))
                    || indices.iter().any(|i| i.any_metavar(pred))
            }
            Subterm::Variant(Variant {
                params, payload, ..
            }) => {
                params.iter().any(|p| p.any_metavar(pred))
                    || payload.iter().any(|p| p.any_metavar(pred))
            }
            Subterm::StructType(StructType { params, .. }) => {
                params.iter().any(|p| p.any_metavar(pred))
            }
            Subterm::Struct(Struct { params, fields, .. }) => {
                params.iter().any(|p| p.any_metavar(pred))
                    || fields.iter().any(|f| f.any_metavar(pred))
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                head.any_metavar(pred)
                    || motive.body().any_metavar(pred)
                    || match cases {
                        Cases::Bool {
                            false_case,
                            true_case,
                        } => false_case.any_metavar(pred) || true_case.any_metavar(pred),
                        Cases::Switch { cases, default } => {
                            cases.values().any(|b| b.any_metavar(pred)) || default.any_metavar(pred)
                        }
                        Cases::Induct { cases, default } => {
                            cases.iter().any(|(_, s)| s.body.body().any_metavar(pred))
                                || default.as_ref().is_some_and(|d| d.any_metavar(pred))
                        }
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => empty_case.any_metavar(pred) || cons_case.body().any_metavar(pred),
                            Carrier::Bin {
                                empty_case,
                                cons_case,
                                ..
                            } => empty_case.any_metavar(pred) || cons_case.body().any_metavar(pred),
                            Carrier::Lst {
                                elem,
                                empty_case,
                                cons_case,
                            } => {
                                elem.any_metavar(pred)
                                    || empty_case.any_metavar(pred)
                                    || cons_case.body().any_metavar(pred)
                            }
                        },
                    }
            }
            Subterm::Let(Let { bindings, tail, .. }) => {
                bindings.iter().any(|binding| {
                    binding.type_().any_metavar(pred) || binding.value().any_metavar(pred)
                }) || tail.body().any_metavar(pred)
            }
            Subterm::Rec(Rec { group, tail }) => {
                group.iter().any(|member| {
                    member.type_.body().any_metavar(pred) || member.body.body().any_metavar(pred)
                }) || tail.body().any_metavar(pred)
            }
        }
    }

    /// Whether any direct child `Term` of this subterm satisfies `pred`, short-circuiting on the first hit — the shared structural walk under the cached [`has_local_free`](Self::has_local_free)/[`has_metavar`](Self::has_metavar) bits, which pass a child's own memoized accessor as `pred` so shared subterms are never re-walked. Scope bodies are visited closed: binder occurrences are bound indices there, so binder labels stay invisible to any free-variable predicate.
    ///
    /// Also the descent `positivity` uses for the forms it cannot see through, with a `pred` that always returns `false` so the walk is exhaustive rather than short-circuiting. That reuse is deliberate: it is what keeps the positivity check from silently missing a recursive occurrence when a new term former is added.
    pub fn any_child_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { spine, .. }) => spine.iter().any(&mut *pred),
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => false,
            Subterm::UniverseInst(UniverseInst { head, .. }) => pred(head),
            Subterm::Transient(transient) => {
                let mut children = transient.subterms();
                children.any(&mut *pred)
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.any_term(pred),
            Subterm::Foreign(_, args) => args.iter().any(&mut *pred),
            Subterm::Func(Func { telescope, .. }) => telescope.any_term(pred),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.any_term(pred),
            Subterm::Apply(Apply { head, params, .. }) => {
                pred(head) || params.iter().any(&mut *pred)
            }
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.any_term(pred),
            Subterm::Tuple(Tuple { fields, .. }) => fields.iter().any(&mut *pred),
            Subterm::Proj(Proj { head, .. }) => pred(head),
            Subterm::InductType(InductType {
                params, indices, ..
            }) => params.iter().any(&mut *pred) || indices.iter().any(&mut *pred),
            Subterm::Variant(Variant {
                params, payload, ..
            }) => params.iter().any(&mut *pred) || payload.iter().any(&mut *pred),
            Subterm::StructType(StructType { params, .. }) => params.iter().any(&mut *pred),
            Subterm::Struct(Struct { params, fields, .. }) => {
                params.iter().any(&mut *pred) || fields.iter().any(&mut *pred)
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                pred(head)
                    || pred(motive.body())
                    || match cases {
                        Cases::Bool {
                            false_case,
                            true_case,
                        } => pred(false_case) || pred(true_case),
                        Cases::Switch { cases, default } => {
                            cases.values().any(&mut *pred) || pred(default)
                        }
                        Cases::Induct { cases, default } => {
                            cases.iter().any(|(_, s)| pred(s.body.body()))
                                || default.as_ref().is_some_and(&mut *pred)
                        }
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => pred(empty_case) || pred(cons_case.body()),
                            Carrier::Bin {
                                empty_case,
                                cons_case,
                                ..
                            } => pred(empty_case) || pred(cons_case.body()),
                            Carrier::Lst {
                                elem,
                                empty_case,
                                cons_case,
                            } => pred(elem) || pred(empty_case) || pred(cons_case.body()),
                        },
                    }
            }
            Subterm::Let(Let { bindings, tail, .. }) => {
                bindings
                    .iter()
                    .any(|binding| pred(binding.type_()) || pred(binding.value()))
                    || pred(tail.body())
            }
            Subterm::Rec(Rec { group, tail }) => {
                group
                    .iter()
                    .any(|member| pred(member.type_.body()) || pred(member.body.body()))
                    || pred(tail.body())
            }
        }
    }

    /// Whether any free variable in this subterm is a binder rather than a top-level definition — the uncached spelling of [`Term::has_local_free`], which supplies the per-node memoization.
    ///
    /// A local is a [`Free::Local`], so this is a discriminant test. It used to be a search for a marker character in the spelling, which a compiler-made *global* could set by accident — and once did.
    pub(crate) fn has_local_free(&self) -> bool {
        match self {
            Subterm::Var(var) => var.as_free().is_some_and(Free::is_local),
            _ => self.any_child_term(&mut |t| t.has_local_free()),
        }
    }

    /// Whether any `Metavar` node occurs in this subterm — the uncached spelling of [`Term::has_metavar`], which supplies the per-node memoization.
    pub(crate) fn has_metavar(&self) -> bool {
        match self {
            Subterm::Metavar(_) => true,
            _ => self.any_child_term(&mut |t| t.has_metavar()),
        }
    }

    pub(crate) fn has_universe_meta(&self) -> bool {
        let level_has_meta = |level: &Level| level.metas().next().is_some();
        match self {
            Subterm::Type(level) => level_has_meta(level),
            Subterm::UniverseInst(UniverseInst { head, levels }) => {
                head.has_universe_meta() || levels.iter().any(level_has_meta)
            }
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                universes.iter().any(level_has_meta)
                    || self.any_child_term(&mut |term| term.has_universe_meta())
            }
            _ => self.any_child_term(&mut |term| term.has_universe_meta()),
        }
    }

    pub(crate) fn has_universe_data(&self) -> bool {
        match self {
            Subterm::Type(level) => level != &Level::zero(),
            Subterm::UniverseInst(_) => true,
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                !universes.is_empty() || self.any_child_term(&mut |term| term.has_universe_data())
            }
            Subterm::Rec(Rec { group, .. }) => {
                group.universe_context() != &UniverseContext::empty()
                    || self.any_child_term(&mut |term| term.has_universe_data())
            }
            _ => self.any_child_term(&mut |term| term.has_universe_data()),
        }
    }

    /// This subterm's free-variable set as its own identity (if it is a free `Var`) unioned with its children's already-memoized sets — the child-combining spelling that lets [`Term::get_or_init_free_vars`] fill a deep spine bottom-up in O(children) per node instead of re-walking the subtree. Equivalent to the whole-subtree `Bound::free_vars` walk, since a free name occurs free in exactly the nodes whose subtrees contain it.
    ///
    /// A node that adds no identity of its own and whose free variables all arrive through one child shares that child's allocation ([`FreeVars::Shared`]) instead of copying it: on a chain-shaped term every link above the one free occurrence carries the same set, and copying it per link would cost O(set) where the pass-through costs O(1). The union only materializes once a second carrying child appears.
    fn free_vars_from_children(&self) -> FreeVars {
        if let Subterm::Var(var) = self
            && let Some(name) = var.as_free()
        {
            return FreeVars::Owned(BTreeSet::from([name.clone()]));
        }
        let mut carrier: Option<Rc<BTreeSet<Free>>> = None;
        let mut union: Option<BTreeSet<Free>> = None;
        self.any_child_term(&mut |child| {
            let frees = child.get_or_init_free_vars();
            if frees.is_empty() {
                return false;
            }
            match (&carrier, &mut union) {
                (None, _) => carrier = Some(Rc::clone(frees)),
                (Some(first), None) => {
                    let mut merged = (**first).clone();
                    merged.extend(frees.iter().cloned());
                    union = Some(merged);
                }
                (Some(_), Some(merged)) => merged.extend(frees.iter().cloned()),
            }
            false
        });
        match (carrier, union) {
            (_, Some(merged)) => FreeVars::Owned(merged),
            (Some(shared), None) => FreeVars::Shared(shared),
            (None, None) => FreeVars::Owned(BTreeSet::new()),
        }
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit` only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses cannot piggyback on `free_vars` — this walk (an `any_metavar` whose collector never short-circuits) enumerates them directly.
    fn collect_metavars(&self, ids: &mut BTreeSet<MetaId>) {
        self.any_metavar(&mut |id| {
            ids.insert(id);
            false
        });
    }
}

impl fmt::Display for Subterm {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(
            print_term(self.clone().into(), 0, &Rc::new(Spelling::default())),
            formatter,
            4,
        )
    }
}

impl Bound for Subterm {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Subterm::Type(level) => Subterm::Type(visit.visit_level(level)),
            Subterm::Prop => Subterm::Prop,
            Subterm::Intrinsic(intrinsic) => Subterm::Intrinsic(intrinsic.traverse(visit)),
            Subterm::Foreign(function, args) => Subterm::Foreign(
                Arc::clone(function),
                args.iter().map(|arg| visit.visit_subterm(arg)).collect(),
            ),
            Subterm::FuncType(FuncType {
                telescope,
                plicities,
            }) => Subterm::FuncType(FuncType {
                telescope: telescope.traverse(visit),
                plicities: plicities.clone(),
            }),
            Subterm::Func(Func {
                telescope,
                plicities,
            }) => Subterm::Func(Func {
                telescope: telescope.traverse(visit),
                plicities: plicities.clone(),
            }),
            Subterm::Transient(transient) => {
                Subterm::Transient(transient.map_subterms(&mut |child| visit.visit_subterm(child)))
            }
            Subterm::Apply(Apply {
                head,
                params,
                plicities,
            }) => Subterm::Apply(Apply {
                head: visit.visit_subterm(head),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                plicities: plicities.clone(),
            }),
            Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
                telescope: telescope.traverse(visit),
            }),
            Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                names: names.clone(),
            }),
            Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
                head: visit.visit_subterm(head),
                field: field.clone(),
            }),
            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => Subterm::InductType(InductType {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                indices: indices.iter().map(|i| visit.visit_subterm(i)).collect(),
            }),
            Subterm::Variant(Variant {
                name,
                universes,
                params,
                tag,
                payload,
            }) => Subterm::Variant(Variant {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                tag: tag.clone(),
                payload: payload.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => Subterm::StructType(StructType {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                entries,
            }) => Subterm::Struct(Struct {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                entries: entries.clone(),
            }),
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => Subterm::Match(Match {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                cases: match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => Cases::Bool {
                        false_case: visit.visit_subterm(false_case),
                        true_case: visit.visit_subterm(true_case),
                    },
                    Cases::Switch { cases, default } => Cases::Switch {
                        cases: cases
                            .iter()
                            .map(|(&n, body)| (n, visit.visit_subterm(body)))
                            .collect(),
                        default: visit.visit_subterm(default),
                    },
                    Cases::Induct { cases, default } => Cases::Induct {
                        cases: cases
                            .iter()
                            .map(|(atom, arm)| {
                                (atom.clone(), arm.with_body(visit.visit_scope(&arm.body)))
                            })
                            .collect(),
                        // The default binds nothing — it lives in the enclosing scope, like `head`.
                        default: default.as_ref().map(|d| visit.visit_subterm(d)),
                    },
                    Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                        carrier: match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => Carrier::Nat {
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                            Carrier::Bin {
                                grain,
                                empty_case,
                                cons_case,
                            } => Carrier::Bin {
                                grain: *grain,
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                            Carrier::Lst {
                                elem,
                                empty_case,
                                cons_case,
                            } => Carrier::Lst {
                                elem: visit.visit_subterm(elem),
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                        },
                    },
                },
            }),
            Subterm::Let(Let { bindings, tail }) => {
                // Binding `i` sits under the `i` binders written before it, so bracket the visit at that depth; the enter/leave don't stack with `visit_scope(tail)`, which owns all the binders on its own. A forward loop over `bindings` is what a flat block buys over the old nested chain — no native frame per binding.
                let bindings = bindings
                    .iter()
                    .enumerate()
                    .map(|(i, binding)| {
                        visit.enter_scope(i);
                        let out = LetBinding::new(
                            visit.visit_subterm(binding.type_()),
                            visit.visit_subterm(binding.value()),
                        );
                        visit.leave_scope(i);
                        out
                    })
                    .collect();

                Subterm::Let(Let {
                    bindings,
                    tail: visit.visit_scope(tail),
                })
            }
            Subterm::Rec(Rec { group, tail }) => Subterm::Rec(Rec {
                group: group.traverse(visit),
                tail: visit.visit_scope(tail),
            }),
            Subterm::UniverseInst(UniverseInst { head, .. }) if visit.erases_universes() => {
                (*visit.visit_subterm(head)).clone()
            }
            Subterm::UniverseInst(UniverseInst { head, levels }) => {
                Subterm::UniverseInst(UniverseInst {
                    head: visit.visit_subterm(head),
                    levels: levels
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect(),
                })
            }
            Subterm::Var(var) => visit.call(var).unwrap_or_else(|| Subterm::Var(var.clone())),
            // The spine is ordinary term content: visiting it is what keeps the delayed substitution aligned through `close`/`open`. Spines are wide (one entry per birth binder) and overwhelmingly identity (bare variables a visit does not touch), so entries are copy-on-write — an untouched `Var` is an `Rc` bump, never a rebuild — and an entirely untouched spine reuses its shared allocation. This is what keeps per-traversal cost flat for the common meta instead of O(|Γ|) allocations.
            Subterm::Metavar(Metavar { id, spine, origin }) => {
                let mut touched = false;
                let visited = spine
                    .iter()
                    .map(|t| match &**t {
                        Subterm::Var(var) => match visit.call(var) {
                            Some(rewritten) => {
                                touched = true;
                                Term::from(rewritten)
                            }
                            None => t.clone(),
                        },
                        _ => {
                            let rebuilt = visit.visit_subterm(t);
                            touched = touched || rebuilt != *t;
                            rebuilt
                        }
                    })
                    .collect::<Vec<_>>();
                Subterm::Metavar(Metavar {
                    id: *id,
                    spine: match touched {
                        true => Rc::new(visited),
                        false => spine.clone(),
                    },
                    origin: origin.clone(),
                })
            }
        }
    }

    fn reach(&self) -> usize {
        match self {
            Subterm::Type(_) => 0,
            Subterm::Prop => 0,
            Subterm::Transient(transient) => transient
                .subterms()
                .map(|child| child.reach())
                .fold(0, usize::max),
            Subterm::Metavar(Metavar { spine, .. }) => max_reach(spine.as_slice()),
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.reach(),
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Intrinsic(intrinsic) => intrinsic.reach(),
            Subterm::Foreign(_, args) => max_reach(args),
            Subterm::Func(Func { telescope, .. }) => telescope.reach(),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.reach(),
            Subterm::Apply(Apply { head, params, .. }) => head.reach().max(max_reach(params)),
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.reach(),
            Subterm::Tuple(Tuple { fields, .. }) => max_reach(fields),
            Subterm::Proj(Proj { head, .. }) => head.reach(),
            Subterm::InductType(InductType {
                params, indices, ..
            }) => max_reach(params).max(max_reach(indices)),
            Subterm::Variant(Variant {
                params, payload, ..
            }) => max_reach(params).max(max_reach(payload)),
            Subterm::StructType(StructType { params, .. }) => max_reach(params),
            Subterm::Struct(Struct { params, fields, .. }) => {
                max_reach(params).max(max_reach(fields))
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => head.reach().max(motive.reach()).max(match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => false_case.reach().max(true_case.reach()),
                Cases::Switch { cases, default } => max_reach(cases.values()).max(default.reach()),
                Cases::Induct { cases, default } => cases
                    .iter()
                    .map(|(_, s)| s.reach())
                    .max()
                    .unwrap_or(0)
                    .max(default.as_ref().map_or(0, |d| d.reach())),
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat {
                        empty_case,
                        cons_case,
                    } => empty_case.reach().max(cons_case.reach()),
                    Carrier::Bin {
                        empty_case,
                        cons_case,
                        ..
                    } => empty_case.reach().max(cons_case.reach()),
                    Carrier::Lst {
                        elem,
                        empty_case,
                        cons_case,
                    } => elem.reach().max(empty_case.reach()).max(cons_case.reach()),
                },
            }),
            // Binding `i` sits under `i` binders, so its reach past the block boundary is `reach - i`; `Scope::reach` handles the tail's own arity. A flat forward max — no inner-to-outer unwind — because the block is flat, not a nest of arity-subtracting scopes.
            Subterm::Let(Let { bindings, tail, .. }) => {
                let mut reach = tail.reach();

                for (i, binding) in bindings.iter().enumerate() {
                    reach = reach
                        .max(binding.type_().reach().saturating_sub(i))
                        .max(binding.value().reach().saturating_sub(i));
                }

                reach
            }
            Subterm::Rec(Rec { group, tail }) => group.reach().max(tail.reach()),
        }
    }

    fn has_metavar(&self) -> bool {
        Subterm::has_metavar(self)
    }
}

fn max_reach<'a>(terms: impl IntoIterator<Item = &'a Term>) -> usize {
    terms
        .into_iter()
        .map(|term| term.reach())
        .max()
        .unwrap_or(0)
}

/// Stamp one arm's payload binders with [`Plicity::Explicit`], the shape the `_marked` inductive-match builders consume — the all-explicit builders' per-arm adapter.
fn explicit_arm<L>(binders: Vec<L>) -> Vec<(Plicity, L)> {
    binders
        .into_iter()
        .map(|label| (Plicity::Explicit, label))
        .collect()
}
