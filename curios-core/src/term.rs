#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Bound, Level, Many, Nat, Prim, Scope, SelfReference, Telescope, Three, Two,
        UniverseContext, UniverseError, UniverseMetaId, UniverseScheme, Var, Visit,
        instantiate_universe_levels_scoped, print_term,
    },
    curios_base::{Flt, Grain, Int, Mint, NumOp, Plicity, Span, printer::run_printer},
    num_bigint::BigUint,
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashSet, hash_map::DefaultHasher},
        fmt,
        hash::{Hash, Hasher},
        ops::Deref,
        rc::Rc,
    },
};

#[cfg(feature = "archive")]
use curios_base::BigUintBytes;

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

/// A [`Subterm`] together with its memoized, span-independent derivations. One
/// per distinct node, behind the shared `Rc` every occurrence bumps, so each
/// derivation fills at most once across the whole DAG. The cells are filled
/// lazily by an iterative post-order walk over the node's descendants
/// (`Term::warm_scalars`/`Term::get_or_init_free_vars`) rather than by native
/// recursion, so a data-shaped spine of any depth memoizes on a bounded stack:
/// filling one node reads its children's already-filled cells in O(children).
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
struct Node {
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    hash: OnceCell<u64>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    reach: OnceCell<usize>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    free_vars: OnceCell<Rc<BTreeSet<String>>>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    has_local_free: OnceCell<bool>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    has_metavar: OnceCell<bool>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    has_universe_meta: OnceCell<bool>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    has_universe_data: OnceCell<bool>,
    subterm: Subterm,
}

impl Node {
    fn new(subterm: Subterm) -> Self {
        Node {
            hash: OnceCell::new(),
            reach: OnceCell::new(),
            free_vars: OnceCell::new(),
            has_local_free: OnceCell::new(),
            has_metavar: OnceCell::new(),
            has_universe_meta: OnceCell::new(),
            has_universe_data: OnceCell::new(),
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
    /// Fill a memoized cell on every node of this term's subtree, bottom-up,
    /// on an explicit stack instead of the native one — so a data-shaped spine
    /// of any depth memoizes without recursing per link. `is_filled` reports
    /// whether a node's target cell is set (a filled node — and its whole
    /// subtree — is skipped, so shared chains are walked once); `fill` computes
    /// one node's cell after all its children are filled, reading theirs in
    /// O(children). The walk rides `any_child_term` over owned clones: cloning a
    /// `Term` bumps the shared `Rc<Node>`, so filling a clone's cell fills it
    /// for every occurrence of that node, and `Rc::as_ptr` dedups by node.
    fn fill_post_order(&self, is_filled: impl Fn(&Node) -> bool, mut fill: impl FnMut(&Node)) {
        if is_filled(&self.inner) {
            return;
        }
        let mut seen: HashSet<*const Node> = HashSet::new();
        let mut stack: Vec<(Term, bool)> = vec![(self.clone(), false)];
        while let Some((node, expanded)) = stack.pop() {
            if expanded {
                fill(&node.inner);
            } else if !is_filled(&node.inner) && seen.insert(Rc::as_ptr(&node.inner)) {
                stack.push((node.clone(), true));
                node.inner.subterm.any_child_term(&mut |child| {
                    stack.push((child.clone(), false));
                    false
                });
            }
        }
    }

    /// Fill the cheap scalar cells together in one post-order pass. They
    /// combine from the children's cells in O(children), and are almost always
    /// wanted together, so one shared walk beats independent traversals.
    /// `reach` is the fill marker.
    fn warm_scalars(&self) {
        self.fill_post_order(
            |node| node.reach.get().is_some(),
            |node| {
                node.reach.get_or_init(|| node.subterm.reach());
                node.has_local_free
                    .get_or_init(|| node.subterm.has_local_free());
                node.has_metavar.get_or_init(|| node.subterm.has_metavar());
                node.has_universe_meta
                    .get_or_init(|| node.subterm.has_universe_meta());
                node.has_universe_data
                    .get_or_init(|| node.subterm.has_universe_data());
                node.hash.get_or_init(|| {
                    let mut hasher = DefaultHasher::new();
                    node.subterm.hash(&mut hasher);
                    hasher.finish()
                });
            },
        );
    }

    fn get_or_init_hash(&self) -> u64 {
        if self.inner.hash.get().is_none() {
            self.warm_scalars();
        }
        *self.inner.hash.get().expect("warm_scalars fills hash")
    }

    /// Whether any *free* variable in this term carries an elaborator-minted
    /// label — one containing `#`, which cannot occur in a written identifier
    /// (`Context::fresh` always embeds it; witness-table names share the
    /// convention, deliberately counted here so the elaboration memo stays
    /// conservative). Binder labels inside `Scope`s are closed occurrences,
    /// not free variables, and never count. Cached per node and computed from
    /// the children's cached cells, so a shared subterm — a DAG-shaped lowered
    /// literal — pays O(degree) here, not O(size): the elaboration cache gates
    /// every `elaborate` call on this bit and must not re-walk shared chains.
    pub(crate) fn has_local_free(&self) -> bool {
        if self.inner.has_local_free.get().is_none() {
            self.warm_scalars();
        }
        *self
            .inner
            .has_local_free
            .get()
            .expect("warm_scalars fills has_local_free")
    }

    /// Whether any `Metavar` node occurs in this term. Cached per node like
    /// [`has_local_free`](Self::has_local_free) and for the same reason: the
    /// elaboration cache's O(1)-per-call gate.
    pub(crate) fn has_metavar(&self) -> bool {
        if self.inner.has_metavar.get().is_none() {
            self.warm_scalars();
        }
        *self
            .inner
            .has_metavar
            .get()
            .expect("warm_scalars fills has_metavar")
    }

    /// Whether this term contains an unresolved universe metavariable in a
    /// `Type` level, universe instantiation, or nominal universe vector.
    pub(crate) fn has_universe_meta(&self) -> bool {
        if self.inner.has_universe_meta.get().is_none() {
            self.warm_scalars();
        }
        *self
            .inner
            .has_universe_meta
            .get()
            .expect("warm_scalars fills has_universe_meta")
    }

    /// Whether universe erasure or validation must inspect this subtree.
    ///
    /// Cached and filled on the explicit post-order stack like the other
    /// scalar derivations, so universe-only passes can structurally share a
    /// deep universe-free data spine without consuming one native frame per
    /// node.
    pub(crate) fn has_universe_data(&self) -> bool {
        if self.inner.has_universe_data.get().is_none() {
            self.warm_scalars();
        }
        *self
            .inner
            .has_universe_data
            .get()
            .expect("warm_scalars fills has_universe_data")
    }

    pub(crate) fn universe_metas(&self) -> BTreeSet<UniverseMetaId> {
        super::universe_metas(self)
    }

    /// Whether any universe metavariable in this subtree satisfies `pred`.
    ///
    /// The walk is iterative and pointer-deduplicated, matching the scalar
    /// cache fill: cache eligibility calls this on data-shaped terms and must
    /// not put their depth back onto the native stack.
    pub(crate) fn any_universe_meta(&self, mut pred: impl FnMut(UniverseMetaId) -> bool) -> bool {
        let mut seen: HashSet<*const Node> = HashSet::new();
        let mut pending = vec![self.clone()];
        while let Some(term) = pending.pop() {
            if !seen.insert(Rc::as_ptr(&term.inner)) {
                continue;
            }
            if !term.has_universe_meta() {
                continue;
            }
            if term.inner.subterm.any_direct_universe_meta(&mut pred) {
                return true;
            }
            term.inner.subterm.any_child_term(&mut |child| {
                pending.push(child.clone());
                false
            });
        }
        false
    }

    /// Extend the two dependency sets in one explicit walk without rebuilding
    /// the term or warming its unrelated scalar caches. Declaration universe
    /// closure uses both sets together: direct level metas join the closure,
    /// while term metas lead to their result, telescope, and solved body in
    /// the context store.
    pub(crate) fn collect_universe_dependencies(
        &self,
        universes: &mut BTreeSet<UniverseMetaId>,
        term_metas: &mut BTreeSet<MetaId>,
    ) {
        let mut seen: HashSet<*const Node> = HashSet::new();
        let mut pending = vec![self.clone()];
        while let Some(term) = pending.pop() {
            if !seen.insert(Rc::as_ptr(&term.inner)) {
                continue;
            }
            term.inner.subterm.any_direct_universe_meta(&mut |meta| {
                universes.insert(meta);
                false
            });
            if let Subterm::Metavar(Metavar { id, spine, .. }) = &term.inner.subterm {
                term_metas.insert(*id);
                pending.extend(
                    spine
                        .iter()
                        .filter(|entry| !matches!(&***entry, Subterm::Var(_)))
                        .cloned(),
                );
                continue;
            }
            term.inner.subterm.any_child_term(&mut |child| {
                pending.push(child.clone());
                false
            });
        }
    }

    /// Rewrite this node, if it is an occurrence of one of `names`, to denote
    /// the declaration instance `levels`. Returns `None` for every other node,
    /// leaving it to ordinary traversal.
    ///
    /// Two occurrence shapes carry an instance. A nominal normal form holds it
    /// in its own universe vector; a not-yet-reduced reference to a type former
    /// is an ordinary variable, which holds it as a wrapping [`UniverseInst`] —
    /// the same node an external use site receives from scheme instantiation.
    /// A variable already under a `UniverseInst` has been instantiated and is
    /// returned untouched rather than wrapped twice.
    ///
    /// Nominal children are stamped explicitly because a rewrite hook replaces
    /// its node wholesale: an occurrence nested in a parameter or index must
    /// receive the same instance as the occurrence containing it.
    pub(crate) fn stamp_declaration_node(
        &self,
        names: &BTreeSet<String>,
        self_reference: SelfReference,
        levels: &[Level],
    ) -> Option<Self> {
        fn stamp(
            terms: &[Term],
            names: &BTreeSet<String>,
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
                    .head_label()
                    .is_some_and(|label| names.contains(label)) =>
            {
                return Some(self.clone());
            }
            Subterm::Var(var)
                if self_reference == SelfReference::Free
                    && var.as_free().is_some_and(|label| names.contains(label)) =>
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

    pub(crate) fn unwrap_or_clone(this: Self) -> Subterm {
        match Rc::try_unwrap(this.inner) {
            Ok(node) => node.subterm,
            Err(shared) => shared.subterm.clone(),
        }
    }

    /// The free-variable label at the head of an application spine, descending
    /// through curried `Apply` heads: `classify(c)` and `f(a)(b)` report the
    /// label of `classify` / `f`. A bare free variable reports itself; anything
    /// else is `None`. Used to cheaply gate scrutinee-refinement
    /// canonicalization on the applied symbol before paying for argument
    /// reduction.
    pub(crate) fn head_label(&self) -> Option<&str> {
        match &self.inner.subterm {
            Subterm::Apply(Apply { head, .. }) => head.head_label(),
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.head_label(),
            Subterm::Var(var) => var.as_free(),
            // A decidable comparison's normal form is a primitive node, not an
            // application, so it carries no named head. Scrutinee refinement
            // keys on this label and the reducer's probe gates on it, so an
            // unlabelled key can be registered but never looked up — which is
            // how an operator-spelled scrutinee loses its arm refinement while
            // the equivalent `Nat/lte(a, b)` keeps it.
            Subterm::Prim(prim) => match prim {
                Prim::BoolEql(..) => Some("prim:BoolEql"),
                Prim::BoolNeq(..) => Some("prim:BoolNeq"),
                Prim::NatEql(..) => Some("prim:NatEql"),
                Prim::NatNeq(..) => Some("prim:NatNeq"),
                Prim::NatLt(..) => Some("prim:NatLt"),
                Prim::NatGt(..) => Some("prim:NatGt"),
                Prim::NatLte(..) => Some("prim:NatLte"),
                Prim::NatGte(..) => Some("prim:NatGte"),
                Prim::ByteEql(..) => Some("prim:ByteEql"),
                Prim::ByteLt(..) => Some("prim:ByteLt"),
                Prim::ByteLte(..) => Some("prim:ByteLte"),
                Prim::ByteGt(..) => Some("prim:ByteGt"),
                Prim::ByteGte(..) => Some("prim:ByteGte"),
                Prim::IntEql(..) => Some("prim:IntEql"),
                Prim::IntNeq(..) => Some("prim:IntNeq"),
                Prim::IntLt(..) => Some("prim:IntLt"),
                Prim::IntGt(..) => Some("prim:IntGt"),
                Prim::IntLte(..) => Some("prim:IntLte"),
                Prim::IntGte(..) => Some("prim:IntGte"),
                Prim::FltEql(..) => Some("prim:FltEql"),
                Prim::FltNeq(..) => Some("prim:FltNeq"),
                Prim::FltLt(..) => Some("prim:FltLt"),
                Prim::FltGt(..) => Some("prim:FltGt"),
                Prim::FltLte(..) => Some("prim:FltLte"),
                Prim::FltGte(..) => Some("prim:FltGte"),
                Prim::BinEql(..) => Some("prim:BinEql"),
                Prim::HandleEql(..) => Some("prim:HandleEql"),
                _ => None,
            },
            _ => None,
        }
    }

    /// Return the canonical target when this term is a straightforward
    /// transparent alias body: either a single free variable or its
    /// eta-expanded parameterized form `(xs) => Original(xs)`. The text-stage
    /// interface audit uses this after name resolution to preserve
    /// representation provenance; computed bodies are not classified as aliases.
    pub fn transparent_alias_target(&self) -> Option<String> {
        match &self.inner.subterm {
            Subterm::Var(var) => var.as_free().map(str::to_string),
            Subterm::Func(Func { telescope, .. }) => {
                let fresh = (0..telescope.len())
                    .map(|index| format!("#alias{index}"))
                    .collect::<Vec<_>>();
                let args = fresh.iter().map(Term::free_var).collect::<Vec<_>>();
                let refs = args.iter().collect::<Vec<_>>();
                let Subterm::Apply(Apply { head, params, .. }) =
                    Term::unwrap_or_clone(telescope.open(&refs))
                else {
                    return None;
                };
                let Subterm::Var(target) = &*head else {
                    return None;
                };
                (params.len() == fresh.len()
                    && params.iter().zip(&fresh).all(|(param, label)| {
                        matches!(&**param, Subterm::Var(var) if var.as_free() == Some(label))
                    }))
                .then(|| target.as_free().map(str::to_string))
                .flatten()
            }
            _ => None,
        }
    }

    /// Return the absolute free-variable head of a direct type-family alias.
    ///
    /// The declared type must structurally end in a literal [`Subterm::Type`]
    /// or [`Subterm::Prop`] after peeling only function-type telescopes. The
    /// body is then peeled through function literals and application spines,
    /// again structurally and without reduction or substitution. Computed
    /// heads, local heads, and aliased universe annotations are deliberately
    /// excluded.
    pub fn direct_type_alias_target(&self, declared_type: &Term) -> Option<String> {
        fn ends_in_literal_sort(term: &Term) -> bool {
            match &**term {
                Subterm::Type(_) | Subterm::Prop => true,
                Subterm::FuncType(FuncType { telescope, .. }) => {
                    ends_in_literal_sort(telescope.terminal())
                }
                _ => false,
            }
        }

        fn application_head(term: &Term) -> Option<&str> {
            match &**term {
                Subterm::Apply(Apply { head, .. }) => application_head(head),
                Subterm::Var(var) => var.as_free(),
                _ => None,
            }
        }

        fn direct_head(term: &Term) -> Option<&str> {
            match &**term {
                Subterm::Func(Func { telescope, .. }) => direct_head(telescope.terminal()),
                _ => application_head(term),
            }
        }

        ends_in_literal_sort(declared_type)
            .then(|| direct_head(self))
            .flatten()
            .filter(|target| target.starts_with('/'))
            .map(str::to_string)
    }

    pub(crate) fn span(&self) -> Option<Span> {
        self.span.clone()
    }

    /// Attaches a span to this term. If the term already carries a span (the innermost
    /// one), it is preserved — innermost wins, matching how `Error::at` keeps the first
    /// span it sees as errors propagate up.
    pub(crate) fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }

    /// Ground `Type 0`, used only where the calculus requires that exact
    /// universe (primitive carriers and the type of `Prop`).
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

    /// A primitive term — any literal or primitive operation that converts into [`Prim`].
    pub fn prim<P: Into<Prim>>(prim: P) -> Self {
        Self::from(Subterm::Prim(prim.into()))
    }

    /// A variable occurrence. External callers can only build free variables ([`Var::free`]); bound ones are the scope machinery's business.
    pub fn var(var: Var) -> Self {
        Self::from(Subterm::Var(var))
    }

    pub(crate) fn free_var<A: Into<String>>(label: A) -> Self {
        Self::var(Var::free(label))
    }

    /// Instantiate a generalized binding at occurrence-specific levels.
    pub fn universe_inst(head: Term, levels: Vec<Level>) -> Self {
        if levels.is_empty() {
            head
        } else {
            Self::from(Subterm::UniverseInst(UniverseInst { head, levels }))
        }
    }

    /// An unresolved infix application ([`Infix`]) — elaboration-transient, consumed by `elaborate_infix`.
    pub fn infix(op: NumOp, left: Term, right: Term) -> Self {
        Self::from(Subterm::Infix(Infix { op, left, right }))
    }

    /// A polymorphic numeric literal ([`NumLit`]) — elaboration-transient, resolved to a concrete `Nat`/`Int`/`Flt` primitive by `elaborate_numlit`.
    pub fn num_lit(magnitude: BigUint, signed: bool, negative: bool) -> Self {
        Self::from(Subterm::NumLit(NumLit {
            magnitude,
            signed,
            negative,
        }))
    }

    /// A bare metavariable, as `into_core` mints one for a desugared hole (an omitted annotation, motive, or lambda domain): empty spine (which resolves as the identity — see [`Metavar::spine`]) and no insertion origin, so its solution is spliced silently at zonk.
    pub fn metavar(id: impl Into<MetaId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: None,
        }))
    }

    /// A written goal `?`, as `into_core` mints one: a bare metavariable (empty spine, like [`Term::metavar`]) whose [`MetavarOrigin::Goal`] origin makes zonk *report* what elaboration determined for it — scope, type, and solution — instead of splicing silently.
    pub fn goal(id: impl Into<MetaId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: Some(MetavarOrigin::Goal),
        }))
    }

    /// A metavariable carrying its (optional) provenance mark and birth spine:
    /// a hole or goal rebuilt at its birth point with the identity spine over
    /// its frozen telescope, or an elaborator insertion minted with its
    /// provenance (see [`Metavar::origin`] and [`Metavar::spine`]).
    pub(crate) fn metavar_birthed(
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

    pub(crate) fn func_type<I, L, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
        O: Into<Term>,
    {
        Self::func_type_marked(
            params
                .into_iter()
                .map(|(label, type_)| (Plicity::Explicit, label, type_)),
            output,
        )
    }

    /// Build a Π-type from `(plicity, label, type)` binders, keeping one plicity mark per telescope entry (asserted to line up — the [`FuncType`] invariant). The all-explicit shorthand is the crate-internal `func_type`.
    pub fn func_type_marked<I, L, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (Plicity, L, T)>,
        L: Into<String>,
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

    /// Build an all-explicit function literal from `(label, annotation)`
    /// parameters, closing the body over the labels via a [`Telescope`]. Every
    /// binder is stamped [`Plicity::Explicit`] — use [`Term::func_marked`] for a
    /// function containing hidden binders. There is deliberately no unmarked
    /// "trust me" constructor for a hidden-binder function.
    pub fn func<I, L, T, B>(params: I, body: B) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
    {
        Self::func_marked(
            params
                .into_iter()
                .map(|(label, type_)| (Plicity::Explicit, label, type_)),
            body,
        )
    }

    /// Build a function literal from `(plicity, label, annotation)` binders,
    /// keeping one plicity mark per telescope entry (asserted to line up — the
    /// [`Func`] invariant). The all-explicit shorthand is [`Term::func`].
    pub fn func_marked<I, L, T, B>(params: I, body: B) -> Self
    where
        I: IntoIterator<Item = (Plicity, L, T)>,
        L: Into<String>,
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

    pub(crate) fn tuple_type_unit() -> Self {
        Self::from(Subterm::TupleType(TupleType {
            telescope: Telescope::done(()),
        }))
    }

    /// Build a dependent tuple (Σ) type from `(label, type)` fields: each field's type is closed over the labels before it — written order mirrors telescope order.
    pub fn tuple_type<I, L, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
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

    /// A tuple literal carrying its written field names from `into_core`; elaboration checks them against the expected tuple type's labels and rebuilds the literal name-free. An all-`None` name list collapses to the positional normal form of [`Term::tuple`], so syntactic equality never splits on how the literal was spelled.
    pub fn tuple_named<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (Option<String>, T)>,
        T: Into<Term>,
    {
        let (mut names, fields): (Vec<_>, Vec<_>) = fields
            .into_iter()
            .map(|(name, term)| (name, term.into()))
            .unzip();

        // A literal with no written names is the same term as a positional
        // one — keep the all-positional normal form (`names` empty) so
        // syntactic equality does not split on how the literal was spelled.
        if names.iter().all(Option::is_none) {
            names = vec![];
        }

        Self::from(Subterm::Tuple(Tuple { fields, names }))
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
    pub fn induct_type<N, I, P, J, Q>(name: N, params: I, indices: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::induct_type_at(name, Vec::<Level>::new(), params, indices)
    }

    pub(crate) fn induct_type_at<N, U, I, P, J, Q>(
        name: N,
        universes: U,
        params: I,
        indices: J,
    ) -> Self
    where
        N: Into<String>,
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::InductType(InductType {
            name: name.into(),
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            indices: indices.into_iter().map(|i| i.into()).collect(),
        }))
    }

    /// Build a [`Variant`] normal form — the body of a generated value-constructor function. `name`/`params` are stored redundantly on purpose; see the type's docs.
    pub fn variant<N, I, P, A, J, Q>(name: N, params: I, tag: A, payload: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        A: Into<Atom>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::variant_at(name, Vec::<Level>::new(), params, tag, payload)
    }

    pub(crate) fn variant_at<N, U, I, P, A, J, Q>(
        name: N,
        universes: U,
        params: I,
        tag: A,
        payload: J,
    ) -> Self
    where
        N: Into<String>,
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        A: Into<Atom>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::Variant(Variant {
            name: name.into(),
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            tag: tag.into(),
            payload: payload.into_iter().map(|p| p.into()).collect(),
        }))
    }

    /// Build a [`StructType`] normal form — what the generated type-former's body reduces to. Users never write one directly; see the type's docs.
    pub fn struct_type<N, I, P>(name: N, params: I) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::struct_type_at(name, Vec::<Level>::new(), params)
    }

    pub(crate) fn struct_type_at<N, U, I, P>(name: N, universes: U, params: I) -> Self
    where
        N: Into<String>,
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::from(Subterm::StructType(StructType {
            name: name.into(),
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
        }))
    }

    /// A struct value with no written field names — the positional normal form
    /// (post-elaboration and every internal build), mirroring `tuple`.
    pub fn struct_<N, I, P, J, Q>(name: N, params: I, fields: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::struct_at(name, Vec::<Level>::new(), params, fields)
    }

    pub(crate) fn struct_at<N, U, I, P, J, Q>(name: N, universes: U, params: I, fields: J) -> Self
    where
        N: Into<String>,
        U: IntoIterator<Item = Level>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::Struct(Struct {
            name: name.into(),
            universes: universes.into_iter().collect(),
            params: params.into_iter().map(|p| p.into()).collect(),
            fields: fields.into_iter().map(|f| f.into()).collect(),
            entries: vec![],
        }))
    }

    /// A struct literal carrying the written entry shapes from `into_core`;
    /// elaboration validates them against the declared fields and rebuilds
    /// entry-free, exactly like `tuple_named`.
    pub fn struct_entries<N, I, P, J, T>(name: N, params: I, fields: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = (StructEntry, T)>,
        T: Into<Term>,
    {
        let (mut entries, fields): (Vec<_>, Vec<_>) = fields
            .into_iter()
            .map(|(entry, term)| (entry, term.into()))
            .unzip();

        if entries.iter().all(|e| *e == StructEntry::Field(None)) {
            entries = vec![];
        }

        Self::from(Subterm::Struct(Struct {
            name: name.into(),
            universes: vec![],
            params: params.into_iter().map(|p| p.into()).collect(),
            fields,
            entries,
        }))
    }

    /// Build the primitive eliminator of a nominal inductive ([`Cases::Induct`]): one arm per constructor tag, each closed over its payload binders (all-explicit). [`Term::induct_match_marked`] carries per-binder plicity.
    pub fn induct_match<H, M, I, A, L, B>(
        head: H,
        motive_label: Option<&str>,
        motive: M,
        cases: I,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<L>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
    {
        Self::induct_match_marked(
            head,
            motive_label,
            motive,
            cases
                .into_iter()
                .map(|(atom, binders, body)| (atom, explicit_arm(binders), body)),
        )
    }

    /// [`Term::induct_match`] carrying the written constructor-pattern plicity of each payload binder — the matrix compiler's entry point.
    pub fn induct_match_marked<H, M, I, A, L, B>(
        head: H,
        motive_label: Option<&str>,
        motive: M,
        cases: I,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, L)>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
    {
        Self::induct_match_scoped_marked(
            head,
            Self::motive_scope(motive_label, motive.into()),
            cases,
            None,
        )
    }

    /// [`Term::induct_match_marked`] over an already-built motive scope, with
    /// the optional `| _ =>` catch-all folded in — `into_core`'s single entry
    /// point for a nominal-inductive elimination.
    pub fn induct_match_scoped_marked<H, I, A, L, B>(
        head: H,
        motive: Scope<Many>,
        cases: I,
        default: Option<Term>,
    ) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, L)>, B)>,
        A: Into<Atom>,
        L: Into<String>,
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

    /// The primitive eliminator of a nominal inductive with an explicit `| _ =>`
    /// catch-all ([`Cases::Induct`]'s `default`): the enumerated arms plus a
    /// binding-free default standing in for every other constructor tag. The
    /// dispatching analogue of [`Term::induct_match`], mirroring how
    /// [`Term::switch`] relates to [`Term::nat_match`].
    pub fn induct_match_default<H, M, I, A, L, B, D>(
        head: H,
        motive_label: Option<&str>,
        motive: M,
        cases: I,
        default: D,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<L>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::induct_match_default_marked(
            head,
            motive_label,
            motive,
            cases
                .into_iter()
                .map(|(atom, binders, body)| (atom, explicit_arm(binders), body)),
            default,
        )
    }

    /// [`Term::induct_match_default`] carrying the written constructor-pattern plicity of each payload binder — the matrix compiler's entry point.
    pub fn induct_match_default_marked<H, M, I, A, L, B, D>(
        head: H,
        motive_label: Option<&str>,
        motive: M,
        cases: I,
        default: D,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, Vec<(Plicity, L)>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
        D: Into<Term>,
    {
        Self::induct_match_scoped_marked(
            head,
            Self::motive_scope(motive_label, motive.into()),
            cases,
            Some(default.into()),
        )
    }

    /// Build the arm map from `(tag, [(plicity, binder)], body)` triples, keeping
    /// one plicity mark per payload binder (the [`InductArm`] invariant).
    pub(crate) fn induct_cases_marked<I, A, L, B>(cases: I) -> BTreeMap<Atom, InductArm>
    where
        I: IntoIterator<Item = (A, Vec<(Plicity, L)>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
    {
        cases
            .into_iter()
            .map(|(atom, binders, body)| {
                let (plicities, labels): (Vec<Plicity>, Vec<String>) = binders
                    .into_iter()
                    .map(|(plicity, label)| (plicity, label.into()))
                    .unzip();
                let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
                (
                    atom.into(),
                    InductArm {
                        body: Scope::close(Many(label_refs.len()), &label_refs, body.into()),
                        plicities,
                    },
                )
            })
            .collect()
    }

    /// Build a match's arity-1 motive scope from an optional source label: a
    /// named scope when the label is present, a constant one when not. Shared by
    /// every match constructor whose motive binds just the scrutinee — the
    /// canonical elaborated shape for a primitive carrier or an unindexed
    /// inductive.
    fn motive_scope(motive_label: Option<&str>, motive: Term) -> Scope<Many> {
        match motive_label {
            Some(label) => Scope::close(Many(1), &[label], motive),
            None => Scope::constant(Many(1), motive),
        }
    }

    /// Carry a *written* motive — the surface term `into_core` lowered, before
    /// elaboration has closed it into a scope — as an arity-0 [`Scope`].
    ///
    /// Lowering cannot close the scope itself: the motive's arity is
    /// `n_indices + 1`, and the eliminated family is only known once the
    /// scrutinee's type is inferred. Arity 0 is a free tag for "not yet
    /// scoped" because no elaborated motive can have it — every eliminator
    /// binds at least the scrutinee, so `check_motive` always re-closes at
    /// arity 1 or more. `Scope::constant` performs no capture, so the term
    /// goes in and comes back out of `body()` untouched.
    pub fn match_motive_written<M>(motive: M) -> Scope<Many>
    where
        M: Into<Term>,
    {
        Scope::constant(Many(0), motive.into())
    }

    /// Build a match node around an already-built motive scope. The `*_scoped`
    /// constructors are `into_core`'s entry points: lowering carries the
    /// *written* motive term (see [`Term::match_motive_written`]) rather than a
    /// label and a body, because it cannot know the arity to close at. Every
    /// label-taking constructor above delegates here after building the
    /// canonical arity-1 scope.
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
        motive_label: Option<&str>,
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
            Self::motive_scope(motive_label, motive.into()),
            false_case,
            true_case,
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

    /// Build the structural `Nat` eliminator ([`Carrier::Nat`]): a zero arm plus a successor arm closed over `(pred, ih)` — `Nat`'s generator carries no payload, so the cons arm binds one fewer variable than `Bin`/`Lst`'s.
    pub fn nat_match<H, M, ZC, PL, IL, SC>(
        head: H,
        motive_label: Option<&str>,
        motive: M,
        zero_case: ZC,
        pred_label: PL,
        ih_label: IL,
        succ_case: SC,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        ZC: Into<Term>,
        PL: Into<String>,
        IL: Into<String>,
        SC: Into<Term>,
    {
        Self::nat_match_scoped(
            head,
            Self::motive_scope(motive_label, motive.into()),
            zero_case,
            pred_label,
            ih_label,
            succ_case,
        )
    }

    /// [`Term::nat_match`] over an already-built motive scope.
    pub fn nat_match_scoped<H, ZC, PL, IL, SC>(
        head: H,
        motive: Scope<Many>,
        zero_case: ZC,
        pred_label: PL,
        ih_label: IL,
        succ_case: SC,
    ) -> Self
    where
        H: Into<Term>,
        ZC: Into<Term>,
        PL: Into<String>,
        IL: Into<String>,
        SC: Into<Term>,
    {
        let pred_label = pred_label.into();
        let ih_label = ih_label.into();

        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: zero_case.into(),
                    cons_case: Scope::close(
                        Two,
                        &[pred_label.as_str(), ih_label.as_str()],
                        succ_case.into(),
                    ),
                },
            },
        )
    }

    /// Build the structural `Lst` eliminator ([`Carrier::Lst`]): the element type `elem`, an empty arm, and a cons arm closed over `(head, tail, ih)` — the induction hypothesis at the tail.
    #[allow(clippy::too_many_arguments)]
    pub fn lst_match<H, M, EL, EC, HL, TL, IL, CC>(
        head: H,
        elem: EL,
        motive_label: Option<&str>,
        motive: M,
        empty_case: EC,
        head_label: HL,
        tail_label: TL,
        ih_label: IL,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        EL: Into<Term>,
        EC: Into<Term>,
        HL: Into<String>,
        TL: Into<String>,
        IL: Into<String>,
        CC: Into<Term>,
    {
        Self::lst_match_scoped(
            head,
            elem,
            Self::motive_scope(motive_label, motive.into()),
            empty_case,
            head_label,
            tail_label,
            ih_label,
            cons_case,
        )
    }

    /// [`Term::lst_match`] over an already-built motive scope.
    #[allow(clippy::too_many_arguments)]
    pub fn lst_match_scoped<H, EL, EC, HL, TL, IL, CC>(
        head: H,
        elem: EL,
        motive: Scope<Many>,
        empty_case: EC,
        head_label: HL,
        tail_label: TL,
        ih_label: IL,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        EL: Into<Term>,
        EC: Into<Term>,
        HL: Into<String>,
        TL: Into<String>,
        IL: Into<String>,
        CC: Into<Term>,
    {
        let head_label = head_label.into();
        let tail_label = tail_label.into();
        let ih_label = ih_label.into();

        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Lst {
                    elem: elem.into(),
                    empty_case: empty_case.into(),
                    cons_case: Scope::close(
                        Three,
                        &[head_label.as_str(), tail_label.as_str(), ih_label.as_str()],
                        cons_case.into(),
                    ),
                },
            },
        )
    }

    /// Build the structural `Bin` eliminator ([`Carrier::Bin`]): an empty arm plus a cons arm closed over `(head, tail, ih)` — the induction hypothesis at the tail.
    #[allow(clippy::too_many_arguments)]
    pub fn bin_match<H, M, EC, HL, TL, IL, CC>(
        grain: Grain,
        head: H,
        motive_label: Option<&str>,
        motive: M,
        empty_case: EC,
        head_label: HL,
        tail_label: TL,
        ih_label: IL,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        EC: Into<Term>,
        HL: Into<String>,
        TL: Into<String>,
        IL: Into<String>,
        CC: Into<Term>,
    {
        Self::bin_match_scoped(
            grain,
            head,
            Self::motive_scope(motive_label, motive.into()),
            empty_case,
            head_label,
            tail_label,
            ih_label,
            cons_case,
        )
    }

    /// [`Term::bin_match`] over an already-built motive scope.
    #[allow(clippy::too_many_arguments)]
    pub fn bin_match_scoped<H, EC, HL, TL, IL, CC>(
        grain: Grain,
        head: H,
        motive: Scope<Many>,
        empty_case: EC,
        head_label: HL,
        tail_label: TL,
        ih_label: IL,
        cons_case: CC,
    ) -> Self
    where
        H: Into<Term>,
        EC: Into<Term>,
        HL: Into<String>,
        TL: Into<String>,
        IL: Into<String>,
        CC: Into<Term>,
    {
        let head_label = head_label.into();
        let tail_label = tail_label.into();
        let ih_label = ih_label.into();

        Self::match_scoped(
            head.into(),
            motive,
            Cases::FreeMonoid {
                carrier: Carrier::Bin {
                    grain,
                    empty_case: empty_case.into(),
                    cons_case: Scope::close(
                        Three,
                        &[head_label.as_str(), tail_label.as_str(), ih_label.as_str()],
                        cons_case.into(),
                    ),
                },
            },
        )
    }

    /// Build a [`Cases::Switch`] match: sparse dispatch on specific literal `Nat` values with a mandatory default arm. The arms bind nothing — unlike [`Term::nat_match`], this is a case split, not induction.
    pub fn switch<H, M, I, B, D>(
        head: H,
        motive_label: Option<&str>,
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
            Self::motive_scope(motive_label, motive.into()),
            cases,
            default,
        )
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

    /// Prepend a single non-recursive binding `label = body : type_` in front of
    /// `tail`. `body` is deliberately *not* closed over `label` — a `let` is
    /// non-recursive; use [`Term::rec`] for self-reference.
    ///
    /// When `tail` is itself a [`Let`] block, the binding is *merged* into it so
    /// a run of `let`s becomes one flat block, not a nest: `label` becomes the
    /// block's new outermost binding, every existing binding and the tail step
    /// over one more binder (`capture`/reclose shift them by one), and free
    /// occurrences of `label` in them bind to it. Building a block bottom-up —
    /// as `into_core` and the elaborator's rebuild both do — therefore yields a
    /// single `Let`, and the flatness is what bounds every later walk over it.
    /// A `tail` that is not a `Let` (a `!`-bind's `Apply`, a `rec`, a base term)
    /// starts a fresh one-binding block, so effect boundaries segment naturally.
    pub fn let_<L, T, B, U>(label: L, type_: T, body: B, tail: U) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let label = label.into();
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
                        binding_type.capture(&[label.as_str()]),
                        binding_value.capture(&[label.as_str()]),
                    ));
                }

                Self::from(Subterm::Let(Let {
                    bindings: merged,
                    tail: tail.prepend(label.as_str()),
                }))
            }
            other => Self::from(Subterm::Let(Let {
                bindings: vec![LetBinding::new(type_, body)],
                tail: Scope::close(Many(1), &[label.as_str()], Term::from(other)),
            })),
        }
    }

    /// Build a [`Rec`] block from `(label, type, value)` items: every type, every value, and the tail are closed over the full label list, so the items may reference one another (and themselves) by name.
    pub fn rec<I, L, T, U, V>(items: I, tail: V) -> Self
    where
        I: IntoIterator<Item = (L, T, U)>,
        L: Into<String>,
        T: Into<Term>,
        U: Into<Term>,
        V: Into<Term>,
    {
        let items = items
            .into_iter()
            .map(|(label, type_, value)| (label.into(), type_.into(), value.into()))
            .collect::<Vec<_>>();

        let labels = items
            .iter()
            .map(|(label, _, _)| label.clone())
            .collect::<Vec<_>>();

        let labels = labels
            .iter()
            .map(|label| label.as_str())
            .collect::<Vec<_>>();

        let group = RecGroup::new(
            items
                .into_iter()
                .map(|(_, type_, value)| {
                    (
                        Scope::close(Many(labels.len()), &labels, type_),
                        Scope::close(Many(labels.len()), &labels, value),
                    )
                })
                .collect(),
        );

        Self::from(Subterm::Rec(Rec {
            group,
            tail: Scope::close(Many(labels.len()), &labels, tail.into()),
        }))
    }

    pub(crate) fn rec_member(group: RecGroup, index: usize) -> Self {
        assert!(index < group.len(), "recursive member index out of bounds");
        Self::from(Subterm::RecMember(RecMember { group, index }))
    }
}

impl Hash for Term {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.get_or_init_hash());
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        if Rc::ptr_eq(&self.inner, &other.inner) {
            return true;
        }

        if self.get_or_init_hash() != other.get_or_init_hash() {
            return false;
        }

        self.inner.subterm == other.inner.subterm
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

impl fmt::Display for Term {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_term(self.clone(), 0), formatter, 2)
    }
}

impl Bound for Term {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        if let Some(replacement) = visit.rewrite_term(self) {
            return replacement;
        }
        if visit.universes_only() && !self.has_universe_data() {
            return self.clone();
        }
        if visit.prune() && self.reach() <= visit.depth() {
            return self.clone();
        }
        if (visit.universes_only() || visit.rewrites_terms())
            && matches!(&**self, Subterm::Apply(_) | Subterm::Variant(_))
        {
            return self.traverse_rewrite_spine(visit);
        }

        self.traverse_children(visit)
    }

    fn reach(&self) -> usize {
        if self.inner.reach.get().is_none() {
            self.warm_scalars();
        }
        *self.inner.reach.get().expect("warm_scalars fills reach")
    }

    fn has_metavar(&self) -> bool {
        Term::has_metavar(self)
    }

    /// Cached alongside `hash`/`reach`: a closed subterm that `traverse`'s
    /// pruning short-circuit (above) hands back via `Rc::clone` keeps this
    /// same cell across every later traversal, so a term shared across many
    /// conversion goals — e.g. a `rec` group's own unchanging members,
    /// re-enqueued each round an unfolding cycle revisits them — pays this
    /// O(size) walk once rather than once per goal. Uniform in every term,
    /// not specific to recursive ones; see `Convert::history_key`.
    fn free_vars(&self) -> BTreeSet<String> {
        self.get_or_init_free_vars().as_ref().clone()
    }
}

impl Term {
    fn traverse_children<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        // Preserve the span across traversal; the rebuilt node is a fresh
        // structure, so its cache starts empty.
        Self {
            span: self.span.clone(),
            inner: Rc::new(Node::new((**self).traverse(visit))),
        }
    }

    /// Rewrite a potentially deep constructor/application spine without
    /// putting one native frame per link on the stack. Term hooks and
    /// universe-level rewrites are structurally local at these nodes: neither
    /// former changes binder depth, and every nested scope still delegates to
    /// ordinary traversal.
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
                            rewritten.push(term.traverse_children(visit));
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
                    rewritten.push(Self {
                        span: term.span.clone(),
                        inner: Rc::new(Node::new(subterm)),
                    });
                }
            }
        }

        rewritten
            .pop()
            .expect("a universe spine traversal returns its root")
    }
}

impl Term {
    /// The memoized free-variable set, filled bottom-up on an explicit stack:
    /// each node's set is its children's sets unioned with its own free label
    /// (if it is a free `Var`), so filling reads the children's cached sets in
    /// O(children) rather than re-walking the subtree — a deep spine memoizes
    /// without native recursion. `free_vars` is the fill marker.
    fn get_or_init_free_vars(&self) -> &Rc<BTreeSet<String>> {
        if self.inner.free_vars.get().is_none() {
            self.fill_post_order(
                |node| node.free_vars.get().is_some(),
                |node| {
                    node.free_vars
                        .get_or_init(|| Rc::new(node.subterm.free_vars_from_children()));
                },
            );
        }
        self.inner
            .free_vars
            .get()
            .expect("fill_post_order fills free_vars")
    }

    /// Whether `label` occurs free in this term, through the same memoized
    /// set [`Bound::free_vars`] fills — but as a lookup instead of a set
    /// clone: `define`'s selective reduction-cache invalidation probes every
    /// cached WHNF, and cloning each entry's set there would swamp the walk
    /// it avoids.
    pub(crate) fn mentions_free(&self, label: &str) -> bool {
        self.get_or_init_free_vars().contains(label)
    }

    /// The free-variable labels of this term. Inherent so a `term.free_vars()`
    /// call routes through the memoized, iteratively-filled set (this and the
    /// [`Bound`] impl agree) rather than deref-ing to the uncached, recursive
    /// [`Subterm::free_vars`] when the `Bound` trait is out of scope.
    pub fn free_vars(&self) -> BTreeSet<String> {
        self.get_or_init_free_vars().as_ref().clone()
    }

    /// The ids of every metavariable in this term. Inherent, and gated on the
    /// memoized [`has_metavar`](Self::has_metavar): a ground term (every data
    /// spine) short-circuits without walking, so the enumeration only ever
    /// recurses through metavariable-bearing structure, whose depth is bounded
    /// by the written program.
    pub(crate) fn metavars(&self) -> BTreeSet<MetaId> {
        let mut ids = BTreeSet::new();
        if self.has_metavar() {
            self.inner.subterm.collect_metavars(&mut ids);
        }
        ids
    }

    /// Whether any metavariable in this term satisfies `pred`. Inherent and
    /// gated on [`has_metavar`](Self::has_metavar) like [`metavars`](Self::metavars),
    /// and — since `Subterm::any_metavar`'s recursion re-enters through each
    /// child `Term` — every ground subtree it reaches short-circuits too.
    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        self.has_metavar() && self.inner.subterm.any_metavar(pred)
    }
}

/// An unresolved infix application `left <op> right`. Elaboration infers a
/// shared operand type for the two sides and rebuilds the node as a concept
/// method call (`a + b` ≙ `Add/add(a, b)`; `&&`/`||` alone are hardcoded on
/// `Bool` — see `elaborate_infix`); the node never survives elaboration.
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

/// A polymorphic numeric literal: an integer `magnitude` with an optional
/// written sign. Resolved to a concrete `Nat`/`Int`/`Flt` primitive by
/// `elaborate_numlit` once the expected type is known (or defaulted by shape).
/// Decimal literals are *not* `NumLit` — they parse straight to `Prim::Flt`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct NumLit {
    #[cfg_attr(feature = "archive", rkyv(with = BigUintBytes))]
    pub magnitude: BigUint,
    /// A `+`/`-` was written: drops `Nat` from the candidate set and defaults
    /// the literal to `Int`.
    pub signed: bool,
    /// The written sign was `-` (a negative literal can never be a `Nat`).
    pub negative: bool,
}

/// `plicities` parallels the telescope, one mark per binder; the builder
/// asserts the lengths agree. `Telescope` itself is unchanged. Erasure is
/// sort-driven (a proof or a type erases), so a function type carries no
/// runtime-multiplicity marks of its own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct FuncType {
    pub telescope: Telescope<Term>,
    pub plicities: Vec<Plicity>,
}

/// A function literal: the parameter annotations and the body as one
/// [`Telescope`] (each entry a parameter type, the `Done` payload the body),
/// with `plicities` paralleling the telescope one mark per binder — the builder
/// asserts the lengths agree. Plicity is part of a function's identity and
/// calling convention: a lambda carries the marks its binders were written with
/// (before elaboration) and the complete canonical marks of its checked type
/// (after elaboration, once omitted hidden binders are inserted). Derived
/// `Eq`/`Hash` include `plicities` so that two lambdas differing only in a
/// written mark never share an elaboration-cache entry.
///
/// Erasure ignores `plicities`; its keep/drop decisions come from the checked
/// function type and sort information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Func {
    pub telescope: Telescope<Term>,
    pub plicities: Vec<Plicity>,
}

/// `plicities` parallels `params`, one mark per argument — the call-site `@`
/// marks. Core must carry them (rather than `into_core` resolving them) because
/// `into_core` is type-blind: only the elaborator, holding the head's function
/// type, can decide which binder an `@`-argument fills.
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

/// A dependent product (Σ-type). Erasure is sort-driven: a proof or type-valued
/// field is a *subset type* witness — dropped at erasure, leaving the relevant
/// fields (and collapsing to the bare field when only one remains).
///
/// Unlike binder hints elsewhere, field labels are the target of `.label`
/// resolution during elaboration, so they are part of the type's identity:
/// `Eq`/`Hash` reassert them on top of the label-blind [`Telescope`] identity.
/// Otherwise the reduction memo could hand elaboration a twin type whose
/// labels differ, and a well-typed projection would fail to resolve.
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

/// `names` carries the literal's written field names (`(status = 0, …)`) from
/// `into_core` to elaboration, which checks them against the expected tuple
/// type's labels and rebuilds the literal name-free. Empty means "no names
/// written" — the invariant for every internally-built and post-elaboration
/// tuple.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Tuple {
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// A projection's field is positional in every post-elaboration term; the
/// `Label` form exists only between `into_core` and `elaborate`, which resolves
/// it against the head's tuple type and rebuilds it as `Index`.
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

/// An inductive type as a primitive normal form. Built inside the
/// automatically-generated type-constructor function's body. Users never write
/// one directly — they write `Result(A, E)` and the type-constructor function
/// reduces to this. Two `InductType`s are convertible iff same `name` and
/// pointwise-convertible `params` and `indices`.
///
/// `params` are uniform across constructors; `indices` are the per-case
/// constrained binders — each constructor's registry terminal states its own
/// index expressions. Use sites never distinguish them (`Vec(Bin, 3)` is one
/// flat application of the type-constructor function); the split lives here
/// and in the registry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct InductType {
    pub name: String,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub indices: Vec<Term>,
}

/// A constructor application as a primitive normal form. Built inside the
/// automatically-generated value-constructor function's body. Users never
/// write one directly — they write `Result/success(value)` and the constructor
/// function reduces to this.
///
/// `name` and `params` are recoverable from the term's inferred type; they are
/// stored redundantly on purpose, so `convert` stays purely structural (no
/// context lookups mid-comparison).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Variant {
    pub name: String,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// A struct type as a primitive normal form (cf. [`InductType`], no indices).
/// Built inside the generated type-former's body; users write `Pair(A, B)` and
/// the former reduces to this. Convertible iff same `name` and pointwise-
/// convertible `params`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct StructType {
    pub name: String,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
}

/// One written struct-literal entry, parallel to [`Struct::fields`]: a plain
/// positional field carrying its optional written label, an explicit
/// `use <term>` fill that pairs with the concept's next `use`-marked field
/// position, or a `..base` spread whose paired term is the base to copy the
/// unwritten fields from (riding in `fields` keeps it visible to every term
/// traversal). A `Spread`, if present, is `entries[0]` — enforced at
/// elaboration, not by construction. Pre-elaboration metadata only, like
/// written field names on [`Tuple`]; elaboration rebuilds the value entry-free.
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

/// A struct value as a primitive normal form (cf. [`Variant`], no tag).
/// `name`/`params` are recoverable from the inferred type but stored
/// redundantly so `convert` stays purely structural.
///
/// `entries` carries the literal's written entry shapes from `into_core`:
/// elaboration checks plain fields positionally against the declared labels,
/// pairs `use` entries with the concept's `use`-marked positions, and rebuilds
/// the value entry-free. Empty means "all plain, no names written" — the
/// invariant for every internally-built and post-elaboration struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Struct {
    pub name: String,
    pub universes: Vec<Level>,
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub entries: Vec<StructEntry>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive
/// and differs only in its [`Cases`] payload.
///
/// An *elaborated* motive is closed at the eliminator's own arity: the
/// scrutinee's indices in declaration order, then the scrutinee. That is 1 for
/// every primitive carrier and for an unindexed inductive, and `n_indices + 1`
/// for an indexed one. Parameters are never abstracted — they are uniform
/// across constructors and fixed by the scrutinee's type, so the motive body
/// refers to them through the ambient scope like any other term.
///
/// Before elaboration the motive is instead the *written term*, carried in an
/// arity-0 scope — see [`Term::match_motive_written`].
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

/// One enumerated arm of a [`Cases::Induct`]: the arm body closed over its
/// payload binders, plus a plicity vector paralleling those binders one mark per
/// slot. `plicities.len()` equals `body.arity()`. Before elaboration the marks
/// are the written constructor-pattern plicities; after elaboration they are the
/// constructor's canonical payload plicities. Reduction and erasure open the body
/// positionally and never read the marks; conversion compares them alongside the
/// bodies. Kept beside the body (rather than in a second map) so the two can never
/// drift apart.
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
    pub(crate) fn arity(&self) -> usize {
        self.body.arity()
    }

    /// Open the arm body at its payload binders, positionally (plicity is not
    /// consulted by reduction or erasure).
    pub(crate) fn open(&self, args: &[&Term]) -> Term {
        self.body.open(args)
    }

    /// The arm body's free-variable reach, past its payload binders.
    pub(crate) fn reach(&self) -> usize {
        self.body.reach()
    }

    /// The arm's payload binder labels (hints), in order.
    pub(crate) fn label_iter(&self) -> impl Iterator<Item = Option<&str>> {
        self.body.label_iter()
    }

    /// Rebuild the arm with its whole body scope replaced, preserving the
    /// plicity vector (the traversal-side reconstruction helper).
    pub(crate) fn with_body(&self, body: Scope<Many>) -> Self {
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
    /// The primitive eliminator of a nominal inductive: one arm per constructor,
    /// each arm's arity equal to that constructor's payload arity.
    /// `default` is the optional catch-all arm (`| _ =>`, mirroring
    /// [`Cases::Switch`]'s): present iff the surface match ended in a bare `_`.
    /// It binds nothing and stands in for every constructor tag absent from
    /// `cases`; `None` means the arms structurally cover every constructor
    /// (a true elimination). The enumerated arms are checked at their own case
    /// target indices and the default at the scrutinee's actual ones, so a
    /// catch-all is legal on an indexed family too.
    Induct {
        cases: BTreeMap<Atom, InductArm>,
        default: Option<Term>,
    },
    /// Structural induction on a native free-monoid primitive (`Nat`/`Lst`/
    /// `Bin`): the `carrier` selects the primitive and carries both its parameters
    /// (`Lst`'s element type) and its two arms — an identity arm plus a cons arm
    /// binding the head generator (absent for `Nat`, whose unary generator carries
    /// no payload), the tail, and the induction hypothesis at the tail.
    FreeMonoid { carrier: Carrier },
}

/// The native free-monoid primitive a `Cases::FreeMonoid` eliminates, with its
/// type parameters and its two eliminator arms. `Nat` is the free monoid on one
/// (payload-less) generator; `Bin` carries none; `Lst` carries its element
/// type. Each variant pairs an identity arm (`empty_case`) with a cons arm whose
/// arity is fixed by the carrier — `Scope<Two>` for `Nat` (predecessor, ih),
/// `Scope<Three>` for `Bin`/`Lst` (head, tail, ih).
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

/// A straight-line block of `let` bindings: `bindings` in written order, then a
/// `tail` continuation in scope of all of them. Binding `i` is stored under the
/// `i` binders before it — its `type_` and `value` may reference bindings
/// `0..i` but never binding `i` itself; a `let` is non-recursive, self- and
/// mutual reference is [`Rec`]'s job. A whole run of source `let`s is one
/// `Let`, not a nest, so every walk over it (`traverse`/`reach`/`reduce`/
/// `erase`/`elaborate`) is a loop over `bindings` rather than one native stack
/// frame per binding — which is what keeps a long local `let` sequence from
/// overflowing the stack.
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
/// A local binding is monomorphic. Universe polymorphism is a property of
/// *declarations*, which are frozen into the prelude archive and re-instantiated
/// by later programs; a local binding has no such use sites, and cumulativity
/// already admits the uses a local scheme once served — for `let id : (@A :
/// Type, A) -> A` applied to both `Prop` and `Type 0`, a single `A : Type 1`
/// accepts both, and the level order is linear so a sup always exists.
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
    pub(crate) fn new(type_: Term, value: Term) -> Self {
        Self { type_, value }
    }

    pub fn type_(&self) -> &Term {
        &self.type_
    }

    pub fn value(&self) -> &Term {
        &self.value
    }

    pub(crate) fn into_parts(self) -> (Term, Term) {
        (self.type_, self.value)
    }
}

/// The shared knot of a mutually-recursive group. Every member type and body
/// is scoped over the full group. `Rc` sharing is an implementation detail;
/// equality and hashing remain structural through the scoped items.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecGroup {
    scheme: UniverseScheme<Rc<Vec<(Scope<Many>, Scope<Many>)>>>,
}

impl RecGroup {
    pub(crate) fn new(items: Vec<(Scope<Many>, Scope<Many>)>) -> Self {
        Self {
            scheme: UniverseScheme::monomorphic(Rc::new(items)),
        }
    }

    pub(crate) fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = &(Scope<Many>, Scope<Many>)> + Clone {
        self.scheme.value.iter()
    }

    fn item(&self, index: usize) -> &(Scope<Many>, Scope<Many>) {
        self.scheme
            .value
            .get(index)
            .expect("recursive member index in bounds")
    }

    pub fn universe_context(&self) -> &UniverseContext {
        &self.scheme.context
    }

    pub(crate) fn with_universe_context(mut self, universe_context: UniverseContext) -> Self {
        self.scheme.context = universe_context;
        self
    }

    pub(crate) fn len(&self) -> usize {
        self.iter().len()
    }

    pub(crate) fn members(&self) -> Vec<Term> {
        (0..self.len())
            .map(|index| Term::rec_member(self.clone(), index))
            .collect()
    }

    pub(crate) fn member_type(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).0.open(&refs)
    }

    pub(crate) fn member_body(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).1.open(&refs)
    }

    pub(crate) fn instantiate_universes(&self, arguments: &[Level]) -> Result<Self, UniverseError> {
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
                    .map(|(type_, body)| {
                        Ok((
                            type_.map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                            body.map_body(|body| {
                                instantiate_universe_levels_scoped(body, arguments)
                            })?,
                        ))
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
                .map(|(type_, body)| (visit.visit_scope(type_), visit.visit_scope(body)))
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
            .map(|(type_, body)| type_.reach().max(body.reach()))
            .max()
            .unwrap_or(0)
    }
}

/// A block of mutually recursive bindings with an arbitrary tail in scope of
/// the shared group. It is binding syntax; demanded member occurrences are
/// represented explicitly by [`RecMember`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Rec {
    pub group: RecGroup,
    pub tail: Scope<Many>,
}

/// The folded fixed point selecting one member of a [`RecGroup`]. This is a
/// structural term, not an allocation identity: separately allocated
/// alpha-equivalent groups compare equal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct RecMember {
    pub group: RecGroup,
    pub index: usize,
}

/// Provenance of an inserted implicit argument: the applied function (`func`)
/// had no `@`-argument for its implicit binder `binder` at some call site, so
/// the elaborator filled the slot with a fresh metavariable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct ImplicitOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of an inserted witness argument: the applied function (`func`)
/// had no `use`-argument for its witness binder `binder` at some call site, so
/// the elaborator filled the slot with a fresh metavariable and registered a
/// resolution goal for it. An occurrence still unsolved at zonk reports as a
/// missing witness (naming the goal type from the birth record) rather than an
/// uninferred implicit.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct WitnessOrigin {
    pub func: String,
    pub binder: String,
}

/// Provenance of a marked metavariable — which mechanism created it, deciding
/// how zonk reports it: an unsolved `Implicit`/`Witness` survivor names the
/// binder it filled, while a `Goal` is reported unconditionally.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum MetavarOrigin {
    Implicit(ImplicitOrigin),
    Witness(WitnessOrigin),
    /// A written goal `?` (`into_core` mints it via [`Term::goal`]): the user
    /// asked what elaboration determines here, so zonk errors with the goal's
    /// scope, type, and solution — solved or not — instead of splicing.
    Goal,
}

/// A metavariable's identity: a dense index into the `Context`'s `MetaStore`,
/// minted monotonically by an [`Entropy`](Entropy). A newtype so it can
/// never be confused with the other `usize`-shaped notions the kernel juggles
/// (de Bruijn indices, telescope arities, variant tags, `Nat` magnitudes).
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

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm,
/// born from a surface hole `?` and (possibly) solved by unification. The
/// solution, when one exists, lives in the `Context`'s `MetaStore`, keyed by
/// `id`, spelled with the *birth telescope's* free names.
///
/// `origin` rides with the node: `Some` iff the metavariable was marked at its
/// mint — an elaborator-inserted implicit/witness argument (zonk's
/// unsolved-hole report then names the binder instead of a bare id) or a
/// written goal `?` (zonk reports it unconditionally). Each id is minted
/// exactly once (`into_core` desugared holes with `None` and written goals
/// with `Some(Goal)`, core insertions above the floor `into_core` returns
/// with `Some`), so every occurrence of an id carries the same origin and the
/// derived equality never splits an id.
///
/// `spine` is the delayed substitution — one term per binder of the birth
/// telescope (`MetaEntry::telescope` order), recording what that binder
/// corresponds to at this occurrence. Identity (`Var::free(name)`) at birth.
/// The entries are ordinary term content: `traverse` walks them, so `close`
/// captures them and `open` substitutes them, and the mapping survives
/// re-closing under fresh names — which is what lets a solution mentioning a
/// sibling binder resolve correctly wherever the occurrence ends up. An empty
/// spine is a not-yet-birthed `into_core` hole and resolves as the identity.
///
/// The spine is `Rc`-shared: every meta born under the same Γ shares one
/// identity-spine allocation (see `Context::identity_snapshot`), which is what
/// keeps minting metavariables O(1) instead of O(|Γ|).
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

/// An internal, occurrence-specific instantiation of a universe-polymorphic
/// binding. The ordinary term binder structure remains entirely in `head`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseInst {
    pub head: Term,
    pub levels: Vec<Level>,
}

/// The actual node of the core term language — one variant per term former. [`Term`] wraps a `Subterm` in an `Rc` with cached hash/reach and an optional span, and `Deref`s here, so pattern matches are written against `Subterm` while construction goes through `Term`'s smart constructors. The final two variants (`Infix`, `NumLit`) are elaboration-transient: born in `into_core`, consumed by `elaborate`, never seen by reduce/convert/zonk/erase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Subterm {
    Type(Level),
    Prop,
    Prim(Prim),
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
    RecMember(RecMember),
    UniverseInst(UniverseInst),
    Var(Var),
    Metavar(Metavar),
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_numlit`.
    NumLit(NumLit),
}

impl Subterm {
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
            Subterm::Rec(Rec { group, .. }) | Subterm::RecMember(RecMember { group, .. }) => {
                context_matches(group.universe_context(), &mut level_matches)
            }
            _ => false,
        }
    }

    pub(crate) fn as_nat(&self) -> Option<Nat> {
        match self {
            Subterm::Prim(Prim::Nat(nat)) => Some(nat.clone()),
            _ => None,
        }
    }

    pub(crate) fn as_int(&self) -> Option<Int> {
        match self {
            Subterm::Prim(Prim::Int(value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub(crate) fn as_flt(&self) -> Option<Flt> {
        match self {
            Subterm::Prim(Prim::Flt(value)) => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn as_bln(&self) -> Option<bool> {
        match self {
            Subterm::Prim(Prim::Bool(value)) => Some(*value),
            _ => None,
        }
    }

    /// The free-variable labels occurring in this subterm — the inherent-method spelling of [`Bound::free_vars`], callable without importing the trait.
    pub fn free_vars(&self) -> BTreeSet<String> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect the head name of every inductive/struct *construction* and
    /// *type-former normal form* occurring in this subterm. These names are not
    /// `Var`s (they live in the registry, not the variable graph), so they do not
    /// appear in `free_vars`; the reachability prune (`order_flat_items`) needs
    /// them as edges so a definition that *builds* a `Struct`/`Variant` (e.g. the
    /// string-literal meta-emitter's `/syn/Str/Str`) keeps the backing type-former
    /// and field-type definitions alive even when no `Var` mentions them.
    pub fn construction_names(&self) -> BTreeSet<String> {
        let mut names = BTreeSet::new();
        self.collect_construction_names(&mut names);
        names
    }

    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<String>) {
        match self {
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => {}
            Subterm::UniverseInst(UniverseInst { head, .. }) => {
                head.collect_construction_names(names);
            }
            Subterm::NumLit(_) => {}
            Subterm::Infix(Infix { left, right, .. }) => {
                left.collect_construction_names(names);
                right.collect_construction_names(names);
            }
            Subterm::Metavar(Metavar { spine, .. }) => {
                spine
                    .iter()
                    .for_each(|t| t.collect_construction_names(names));
            }
            Subterm::Prim(prim) => prim.collect_construction_names(names),
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
                            .values()
                            .for_each(|s| s.body.body().collect_construction_names(names));
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
                for (type_, value) in group.iter() {
                    type_.body().collect_construction_names(names);
                    value.body().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::RecMember(RecMember { group, .. }) => {
                for (type_, value) in group.iter() {
                    type_.body().collect_construction_names(names);
                    value.body().collect_construction_names(names);
                }
            }
        }
    }

    /// Whether any metavariable occurring in this subterm satisfies `pred`,
    /// stopping at the first hit. The early-exit dual of `collect_metavars`
    /// (which is this with a collector that never stops): the reducer's memo
    /// gate uses it to reject caching a WHNF that still names an unsolved
    /// metavariable, without allocating the full id set.
    pub(crate) fn any_metavar<F: FnMut(MetaId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { id, spine, .. }) => {
                pred(*id) || spine.iter().any(|t| t.any_metavar(pred))
            }
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => false,
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.any_metavar(pred),
            Subterm::NumLit(_) => false,
            Subterm::Infix(Infix { left, right, .. }) => {
                left.any_metavar(pred) || right.any_metavar(pred)
            }
            Subterm::Prim(prim) => prim.any_metavar(pred),
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
                            cases.values().any(|s| s.body.body().any_metavar(pred))
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
                group.iter().any(|(type_, value)| {
                    type_.body().any_metavar(pred) || value.body().any_metavar(pred)
                }) || tail.body().any_metavar(pred)
            }
            Subterm::RecMember(RecMember { group, .. }) => group.iter().any(|(type_, value)| {
                type_.body().any_metavar(pred) || value.body().any_metavar(pred)
            }),
        }
    }

    /// Whether any direct child `Term` of this subterm satisfies `pred`,
    /// short-circuiting on the first hit — the shared structural walk under the
    /// cached [`has_local_free`](Self::has_local_free)/[`has_metavar`](Self::has_metavar)
    /// bits, which pass a child's own memoized accessor as `pred` so shared
    /// subterms are never re-walked. Scope bodies are visited closed: binder
    /// occurrences are bound indices there, so binder labels stay invisible to
    /// any free-variable predicate.
    fn any_child_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { spine, .. }) => spine.iter().any(&mut *pred),
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => false,
            Subterm::UniverseInst(UniverseInst { head, .. }) => pred(head),
            Subterm::NumLit(_) => false,
            Subterm::Infix(Infix { left, right, .. }) => pred(left) || pred(right),
            Subterm::Prim(prim) => prim.any_term(pred),
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
                            cases.values().any(|s| pred(s.body.body()))
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
                    .any(|(type_, value)| pred(type_.body()) || pred(value.body()))
                    || pred(tail.body())
            }
            Subterm::RecMember(RecMember { group, .. }) => group
                .iter()
                .any(|(type_, value)| pred(type_.body()) || pred(value.body())),
        }
    }

    /// Whether any free variable in this subterm carries an elaborator-minted
    /// (`#`-bearing) label — the uncached spelling of
    /// [`Term::has_local_free`], which supplies the per-node memoization.
    pub(crate) fn has_local_free(&self) -> bool {
        match self {
            Subterm::Var(var) => var.as_free().is_some_and(|label| label.contains('#')),
            _ => self.any_child_term(&mut |t| t.has_local_free()),
        }
    }

    /// Whether any `Metavar` node occurs in this subterm — the uncached
    /// spelling of [`Term::has_metavar`], which supplies the per-node
    /// memoization.
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
            Subterm::Rec(Rec { group, .. }) | Subterm::RecMember(RecMember { group, .. }) => {
                group.universe_context() != &UniverseContext::empty()
                    || self.any_child_term(&mut |term| term.has_universe_data())
            }
            _ => self.any_child_term(&mut |term| term.has_universe_data()),
        }
    }

    /// This subterm's free-variable set as its own free label (if it is a free
    /// `Var`) unioned with its children's already-memoized sets — the child-
    /// combining spelling that lets [`Term::get_or_init_free_vars`] fill a deep
    /// spine bottom-up in O(children) per node instead of re-walking the
    /// subtree. Equivalent to the whole-subtree `Bound::free_vars` walk, since a
    /// free name occurs free in exactly the nodes whose subtrees contain it.
    fn free_vars_from_children(&self) -> BTreeSet<String> {
        if let Subterm::Var(var) = self
            && let Some(label) = var.as_free()
        {
            return BTreeSet::from([label.to_string()]);
        }
        let mut vars = BTreeSet::new();
        self.any_child_term(&mut |child| {
            vars.extend(child.get_or_init_free_vars().iter().cloned());
            false
        });
        vars
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk (an `any_metavar` whose
    /// collector never short-circuits) enumerates them directly.
    fn collect_metavars(&self, ids: &mut BTreeSet<MetaId>) {
        self.any_metavar(&mut |id| {
            ids.insert(id);
            false
        });
    }
}

impl fmt::Display for Subterm {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_term(self.clone().into(), 0), formatter, 2)
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
            Subterm::Prim(prim) => Subterm::Prim(prim.traverse(visit)),
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
            Subterm::NumLit(num_lit) => Subterm::NumLit(num_lit.clone()),
            Subterm::Infix(Infix { op, left, right }) => Subterm::Infix(Infix {
                op: *op,
                left: visit.visit_subterm(left),
                right: visit.visit_subterm(right),
            }),
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
                        // The default binds nothing — it lives in the enclosing
                        // scope, like `head`.
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
                // Binding `i` sits under the `i` binders written before it, so
                // bracket the visit at that depth; the enter/leave don't stack
                // with `visit_scope(tail)`, which owns all the binders on its
                // own. A forward loop over `bindings` is what a flat block buys
                // over the old nested chain — no native frame per binding.
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
            Subterm::RecMember(RecMember { group, index }) => Subterm::RecMember(RecMember {
                group: group.traverse(visit),
                index: *index,
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
            // The spine is ordinary term content: visiting it is what keeps
            // the delayed substitution aligned through `close`/`open`. Spines
            // are wide (one entry per birth binder) and overwhelmingly
            // identity (bare variables a visit does not touch), so entries
            // are copy-on-write — an untouched `Var` is an `Rc` bump, never a
            // rebuild — and an entirely untouched spine reuses its shared
            // allocation. This is what keeps per-traversal cost flat for the
            // common meta instead of O(|Γ|) allocations.
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
            Subterm::NumLit(_) => 0,
            Subterm::Infix(Infix { left, right, .. }) => left.reach().max(right.reach()),
            Subterm::Metavar(Metavar { spine, .. }) => max_reach(spine.as_slice()),
            Subterm::UniverseInst(UniverseInst { head, .. }) => head.reach(),
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Prim(prim) => prim.reach(),
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
                    .values()
                    .map(|s| s.reach())
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
            // Binding `i` sits under `i` binders, so its reach past the block
            // boundary is `reach - i`; `Scope::reach` handles the tail's own
            // arity. A flat forward max — no inner-to-outer unwind — because
            // the block is flat, not a nest of arity-subtracting scopes.
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
            Subterm::RecMember(RecMember { group, .. }) => group.reach(),
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

/// Stamp one arm's payload binders with [`Plicity::Explicit`], the shape the
/// `_marked` inductive-match builders consume — the all-explicit builders'
/// per-arm adapter.
fn explicit_arm<L>(binders: Vec<L>) -> Vec<(Plicity, L)> {
    binders
        .into_iter()
        .map(|label| (Plicity::Explicit, label))
        .collect()
}
