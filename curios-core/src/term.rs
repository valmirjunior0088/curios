#[cfg(test)]
mod binder_tests;
#[cfg(test)]
mod sharing_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod traversal_tests;

// Deliberately not re-exported: the caches are [`Node`]'s private business.
mod scalars;
use scalars::*;

mod frees;
use frees::*;

mod shape;
pub use shape::*;

mod subterm;
pub use subterm::*;

use {
    super::{
        Atom, Bound, Enter, Free, Global, Intrinsic, Level, Many, Nat, Scope, SelfReference,
        Spelled, Spelling, Telescope, Three, Two, UniverseContext, UniverseError, UniverseMetaId,
        UniverseScheme, Var, Visit, instantiate_universe_levels_scoped, print_term,
    },
    curios_abi::ForeignFunction,
    curios_num::{Floating, Integer, Natural},
    curios_print::{run_printer, run_printer_within},
    curios_utilities::{Grain, InfixOp, Mint, Plicity, Sign, Span, recurse},
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

/// The head identity a scrutinee-refinement key gates on: a named free-variable head, or the tag standing in for a comparison intrinsic whose normal form has no named head. Produced only by [`Term::head_key`], which documents the mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeadTag<'a> {
    Name(&'a Free),
    Intrinsic(&'static str),
}

/// A core-calculus term: an `Rc`-shared `Node` — a [`Subterm`] plus its lazily-cached, span-independent derivations (a structural hash, `reach`, the free-variable set, and the `has_local_free`/`has_metavar` bits) — with an optional per-occurrence source span. Clones are pointer bumps that share the node's cache, so a subterm shared across occurrences memoizes each derivation once, not once per occurrence. Equality short-circuits first on pointer identity, then on the cached hashes, before falling back to structural comparison — which is what keeps conversion and the reduction memo affordable on heavily shared trees. The span is identity-irrelevant: hash and equality look only at the node, so re-spanning a term never splits a cache.
///
/// The caches are derivations, never identity: hash and equality answer the same before and after a fill, so a map keyed on `Term` is sound. The workspace `clippy.toml` states exactly that to `mutable_key_type`, which is why no `Term`-keyed map carries an `#[allow]` for it.
#[derive(Debug, Clone)]
#[curios_archive::archived(recursive)]
pub struct Term {
    span: Option<Span>,
    #[archived_omit_bounds]
    inner: Rc<Node>,
}

/// A [`Subterm`] together with its memoized, span-independent derivations. One per distinct node, behind the shared `Rc` every occurrence bumps, so each derivation fills at most once across the whole DAG. The caches are filled lazily by an iterative post-order walk over the node's descendants (`Term::warm_scalars`/`Term::get_or_init_free_vars`) rather than by native recursion, so a data-shaped spine of any depth memoizes on a bounded stack: filling one node reads its children's already-filled caches in O(children).
#[curios_archive::archived]
struct Node {
    /// The eager derivations — hash, `reach`, and the containment flags — packed behind one filled bit; see [`ScalarCache`].
    #[archived_with(curios_archive::Skip)]
    scalars: ScalarCache,
    /// The one derivation left lazy. A `BTreeSet<Free>` per node would dominate the archive it is stored in, and unlike the scalars it is wanted by a minority of nodes on a given compilation.
    #[archived_with(curios_archive::Skip)]
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

    /// This term's node identity, for a per-walk memo keyed on it. Two `Term`s answer the same identity exactly when they are one allocation — which is what makes it a memo key and never a comparison of values.
    pub(crate) fn identity(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
    }

    fn get_or_init_hash(&self) -> u64 {
        self.scalars().hash
    }

    /// The cached structural hash, for a consumer that needs a deterministic total preorder on terms rather than equality — `Nat::product` orders a monomial's factors by it. Deterministic because the hasher is seeded with fixed keys; two distinct terms hashing alike are not distinguished, which a consumer must treat as "no order", never as "equal".
    pub(crate) fn structural_hash(&self) -> u64 {
        self.get_or_init_hash()
    }

    /// Whether any *free* variable in this term is a binder some scope opened ([`Free::Local`]) rather than a top-level definition — the cached spelling of `Subterm::has_local_free`, which records why this is a discriminant test and not a spelling probe. Binder labels inside `Scope`s are closed occurrences, not free variables, and never count. Cached per node and computed from the children's cached scalars, so a shared subterm — a DAG-shaped lowered literal — pays O(degree) here, not O(size): the elaboration cache gates every `elaborate` call on this bit and must not re-walk shared chains.
    pub fn has_local_free(&self) -> bool {
        self.scalars().has_local_free
    }

    /// The logical units this term and everything under it occupy, read off the node's cache in O(1).
    ///
    /// What a retention charge is computed from: the specification asks for a conservative logical footprint of a value whose lifetime an insertion may extend, computed without allocating and without walking an adversarial shared graph. See `scalars`' `footprint_of` for what it counts and where it overcounts.
    pub fn footprint(&self) -> u64 {
        self.scalars().footprint
    }

    /// Whether any `Metavar` node occurs in this term. Cached per node like `has_local_free` and for the same reason: the elaboration cache's O(1)-per-call gate.
    pub(crate) fn has_metavar(&self) -> bool {
        self.scalars().has_metavar
    }

    /// Whether any elaboration-transient node occurs in this term. Cached per node like `has_metavar` and for the same consumer: the zonk-evidence projection's O(children)-per-node walk.
    pub(crate) fn has_transient(&self) -> bool {
        self.scalars().has_transient
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

    /// Whether this term mentions a member of `group` — anywhere, at any depth.
    ///
    /// A member reference materializes as a `Rec` node carrying the whole group with a projecting tail, so this is a structural question about what a term *says*, not a judgment about what it means. That is why both reducers may share it while writing their own rules around it: like [`reduce_intrinsic`](crate::reduce_intrinsic), it cannot admit anything on its own — the rule that consults it decides, and there are deliberately two of those.
    ///
    /// Iterative and pointer-deduplicated, like [`Term::any_universe_meta`]: the terms this runs on are reducts, which may be data-shaped.
    pub fn mentions_rec_member(&self, group: &RecGroup) -> bool {
        let mut seen: HashSet<*const Node> = HashSet::new();
        self.try_walk(
            &mut seen,
            |seen, term| {
                if !seen.insert(Rc::as_ptr(&term.inner)) {
                    return ControlFlow::Continue(Enter::Skip(()));
                }
                if term.as_rec_proj().is_some_and(|(found, _)| found == group) {
                    return ControlFlow::Break(());
                }
                ControlFlow::Continue(Enter::Descend)
            },
            |_, _, _| (),
        )
        .is_break()
    }

    /// Whether this term holds a `Rec` node at all — anywhere, at any depth. The cheap precheck before a display pass that would otherwise gather every recursive definition in scope to look for one.
    pub fn mentions_rec(&self) -> bool {
        let mut seen: HashSet<*const Node> = HashSet::new();
        self.try_walk(
            &mut seen,
            |seen, term| {
                if !seen.insert(Rc::as_ptr(&term.inner)) {
                    return ControlFlow::Continue(Enter::Skip(()));
                }
                if matches!(&**term, Subterm::Rec(_)) {
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
        term_metas: &mut BTreeSet<MetavarId>,
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
    /// Two occurrence shapes carry an instance. A nominal normal form holds it in its own universe vector; a not-yet-reduced reference to a type former is an ordinary variable, which holds it as the head of a wrapping [`Instance`] — the same node an external use site receives from scheme instantiation. A variable already heading an `Instance` has been instantiated and is returned untouched rather than wrapped twice.
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
            Subterm::Instance(instance)
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
                // The occurrence's span moves onto the wrapping instance: the head is a bare `Var` with no span of its own.
                let stamped = Term::instance(InstanceHead::Var(var.clone()), levels.to_vec());
                return Some(match self.span() {
                    Some(span) => stamped.with_span(span),
                    None => stamped,
                });
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
            Subterm::Instance(Instance { head, .. }) => head.head_name(),
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

    /// [`Term::list_match`] over an already-built motive scope.
    #[allow(clippy::too_many_arguments)]
    pub fn list_match_scoped<H, EL, EC, CC>(
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
                carrier: Carrier::List {
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
            Subterm::Instance(Instance { head, .. }) => head.head_name().map(HeadTag::Name),
            Subterm::Var(var) => var.as_free().map(HeadTag::Name),
            // A decidable comparison's normal form is an intrinsic node, not an application, so it has no named head. Scrutinee refinement keys on this tag and the reducer's probe gates on it, so an untagged key can be registered but never looked up — which is how an operator-spelled scrutinee used to lose its arm refinement while the equivalent `Nat/le(a, b)` kept it. The boolean connectives are tagged for the same reason: `match x && g(7)` resolves to a `BoolAnd` the way `a <= b` resolves to a `NatLe`, and a `Bool`-valued scrutinee is one a program matches on.
            Subterm::Intrinsic(intrinsic) => match intrinsic {
                Intrinsic::BoolAnd(..) => Some(HeadTag::Intrinsic("intrinsic:BoolAnd")),
                Intrinsic::BoolOr(..) => Some(HeadTag::Intrinsic("intrinsic:BoolOr")),
                Intrinsic::BoolXor(..) => Some(HeadTag::Intrinsic("intrinsic:BoolXor")),
                Intrinsic::BoolEql(..) => Some(HeadTag::Intrinsic("intrinsic:BoolEql")),
                Intrinsic::BoolNeq(..) => Some(HeadTag::Intrinsic("intrinsic:BoolNeq")),
                Intrinsic::NatEql(..) => Some(HeadTag::Intrinsic("intrinsic:NatEql")),
                Intrinsic::NatNeq(..) => Some(HeadTag::Intrinsic("intrinsic:NatNeq")),
                Intrinsic::NatLt(..) => Some(HeadTag::Intrinsic("intrinsic:NatLt")),
                Intrinsic::NatLe(..) => Some(HeadTag::Intrinsic("intrinsic:NatLe")),
                Intrinsic::ByteEql(..) => Some(HeadTag::Intrinsic("intrinsic:ByteEql")),
                Intrinsic::ByteLt(..) => Some(HeadTag::Intrinsic("intrinsic:ByteLt")),
                Intrinsic::ByteLe(..) => Some(HeadTag::Intrinsic("intrinsic:ByteLe")),
                Intrinsic::IntEql(..) => Some(HeadTag::Intrinsic("intrinsic:IntEql")),
                Intrinsic::IntNeq(..) => Some(HeadTag::Intrinsic("intrinsic:IntNeq")),
                Intrinsic::IntLt(..) => Some(HeadTag::Intrinsic("intrinsic:IntLt")),
                Intrinsic::IntLe(..) => Some(HeadTag::Intrinsic("intrinsic:IntLe")),
                Intrinsic::FltEql(..) => Some(HeadTag::Intrinsic("intrinsic:FltEql")),
                Intrinsic::FltNeq(..) => Some(HeadTag::Intrinsic("intrinsic:FltNeq")),
                Intrinsic::FltLt(..) => Some(HeadTag::Intrinsic("intrinsic:FltLt")),
                Intrinsic::FltLe(..) => Some(HeadTag::Intrinsic("intrinsic:FltLe")),
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
                let Subterm::Apply(apply) = &**telescope.terminal() else {
                    return None;
                };
                let head = &apply.head;
                let eta = apply.arguments.len() == arity
                    && apply.params().enumerate().all(|(index, param)| {
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

    /// An instance whose head references `name` — the shape elaboration mints for every occurrence of a universe-polymorphic binding.
    pub fn instance_of(name: &Free, levels: Vec<Level>) -> Self {
        Self::instance(InstanceHead::Var(Var::free(name.clone())), levels)
    }

    /// Instantiate a generalized binding at occurrence-specific levels. The result is span-less; a call site holding the occurrence lifts its span onto the wrapper, since the typed head carries none.
    pub fn instance(head: InstanceHead, levels: Vec<Level>) -> Self {
        if levels.is_empty() {
            head.to_term()
        } else {
            Self::from(Subterm::Instance(Instance { head, levels }))
        }
    }

    /// A bare silent hole, as `into_core` mints one for a desugared omission (an omitted annotation, motive, or lambda domain): empty spine (which resolves as the identity — see [`Metavar::spine`]) and [`MetavarOrigin::Hole`], so its solution is spliced silently at zonk.
    pub fn hole(id: impl Into<MetavarId>) -> Self {
        Self::metavar_birthed(id, MetavarOrigin::Hole, Vec::new())
    }

    /// A bare written goal `?`, as `into_core` mints one: the same empty spine as [`Term::hole`] under [`MetavarOrigin::Goal`], which makes zonk *report* what elaboration determined for it — scope, type, and solution — instead of splicing silently.
    pub fn goal(id: impl Into<MetavarId>) -> Self {
        Self::metavar_birthed(id, MetavarOrigin::Goal, Vec::new())
    }

    /// A metavariable carrying its provenance and birth spine: a hole or goal rebuilt at its birth point with the identity spine over its frozen telescope, or an elaborator insertion minted with its provenance (see [`Metavar::origin`] and [`Metavar::spine`]).
    pub fn metavar_birthed(
        id: impl Into<MetavarId>,
        origin: MetavarOrigin,
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
        Self::from(Subterm::Apply(Apply {
            head: head.into(),
            arguments: params
                .into_iter()
                .map(|(plicity, param)| Argument {
                    term: param.into(),
                    plicity,
                })
                .collect(),
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

    /// Build the intrinsic eliminator of a nominal inductive ([`Cases::Induct`]): one arm per constructor tag, each closed over its payload binders (all-explicit). `Term::induct_match_marked` carries per-binder plicity.
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

    /// `Term::induct_match_marked` over an already-built motive scope, with the optional `| _ =>` catch-all folded in — `into_core`'s single entry point for a nominal-inductive elimination.
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

    /// Build the structural `Nat` eliminator ([`Carrier::Nat`]): a zero arm plus a successor arm closed over `(pred, ih)` — `Nat`'s generator carries no payload, so the cons arm binds one fewer variable than `Bin`/`List`'s.
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

    /// Build the structural `List` eliminator ([`Carrier::List`]): the element type `elem`, an empty arm, and a cons arm closed over `(head, tail, ih)` — the induction hypothesis at the tail.
    #[allow(clippy::too_many_arguments)]
    pub fn list_match<H, M, EL, EC, CC>(
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
        Self::list_match_scoped(
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
    /// This is an ordinary [`Rec`] node and not a form of its own, which is what keeps one typing rule from having to be written twice — a member occurrence is checked by the rule that checks the group, because it *is* the group. Opening this tail over the group's members yields the same term back, so a projection is the fixed point of `rec` unfolding and therefore a normal form; `Term::as_rec_proj` is how a reducer recognizes one without running the substitution to find out.
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
/// The recursion this replaces was native, and a term deep enough overflowed the stack rather than answering — which a kernel must not do, and which the step budget cannot prevent, because depth is not steps. Every other derivation over a term already avoids native depth the same way (`Term::fill_post_order`, `traverse_rewrite_spine`); this closes the last one that decides acceptance.
///
/// Two shortcuts carry the common cases before any of that: pointer identity (hash-consing makes shared structure genuinely common) and the cached hashes. Only a pair that is distinct-but-hash-equal reaches the walk.
///
/// **A pair of shared nodes is compared once.** A reduct is a graph whose tree can be exponential in its depth — a web of definitions each naming the one before it twice reduces to one — and two such graphs that are equal but distinct reach the walk with every pair of shared nodes on as many paths as the tree has, each pair masked and compared again. The walk keeps the pairs it has already entered, exactly as `any_metavar` keeps its visited nodes, and for the same reason: a pair's answer is the pair's, whichever path reached it, and a `false` ends the walk outright, so a recorded pair is always one that is equal so far. Recorded only where both nodes are shared, so the set stays empty and unallocated over two trees.
impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        curios_profile::sample!("walk::term_eq", 1);
        // **Both verdicts the loop can reach in O(1), reached before it allocates anything.** They were inside the loop, which is correct and was quadratically wasteful: the setup below allocates a placeholder `Term`, a work vector and a pointer-pair set, and a comparison of a shared node against *itself* — the common case on a reduct, where one node stands in many positions — paid all three to then answer on the loop's first line. On a nine-definition web of definitions each naming the one before it twice, that was 592 million comparisons and the bulk of a 168 GB allocation churn.
        //
        // Neither is a new trust. One allocation is one value, so `ptr_eq` implies equality outright; and the hash is a function of the value, so a difference implies inequality. Both are the checks the loop already made, hoisted to where the answer is free.
        if Rc::ptr_eq(&self.inner, &other.inner) {
            return true;
        }
        if self.get_or_init_hash() != other.get_or_init_hash() {
            return false;
        }
        // Past both O(1) verdicts: this comparison does structural work.
        curios_profile::sample!("term_eq::structural", 1);
        // One visit for the whole comparison: the placeholder is allocated once, and each node's children are taken off it in turn.
        let mut visit = Visit::masking(|_, _| None, Term::from(Subterm::Prop));
        // Entering as a `Subterm` is what keeps the node itself unmasked — the hook fires per `Term`, and the node being compared is not one.
        let mut mask = |subterm: &Subterm| {
            let masked = subterm.traverse(&mut visit);
            (masked, visit.take_masked_children())
        };

        let mut work = vec![(self.clone(), other.clone())];
        let mut entered: HashSet<(*const Node, *const Node)> = HashSet::new();

        while let Some((this, that)) = work.pop() {
            if Rc::ptr_eq(&this.inner, &that.inner) {
                continue;
            }
            if this.get_or_init_hash() != that.get_or_init_hash() {
                return false;
            }
            if Rc::strong_count(&this.inner) > 1
                && Rc::strong_count(&that.inner) > 1
                && !entered.insert((Rc::as_ptr(&this.inner), Rc::as_ptr(&that.inner)))
            {
                continue;
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

        // The structural walk concluded equal — as opposed to the three `return false` exits above.
        curios_profile::sample!("term_eq::structural_true", 1);
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
            print_term(self.clone(), &Rc::new(Spelling::default())),
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
        let printer = print_term(self.value().clone(), self.spelling());
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
        // **Guarded per level, so a descent can chain stack segments.** Every child re-enters here, which makes this the one place a check per level lives — the intent [`recurse`] states. Without it a walk that starts inside a segment runs to that segment's end with no chance to map another: a `NatAdd` chain of a few thousand links, five debug frames per link, exhausted the 32 MiB `grown` reserve under the kernel's conversion history, which `capture`s a whole normal form to key a goal, and died as a bare `SIGBUS` with nothing on stderr. The iterative spine path below is no substitute — it is gated on the rewriting modes, and `capture` runs in `Plain`.
        recurse(|| {
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
        })
    }

    fn reach(&self) -> usize {
        self.scalars().reach
    }

    fn has_metavar(&self) -> bool {
        Term::has_metavar(self)
    }

    fn has_transient(&self) -> bool {
        Term::has_transient(self)
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
        self.rebuilt((**self).traverse(visit))
    }

    /// This term carrying `subterm` — **the node itself when `subterm` is what it already held.**
    ///
    /// The one place a traversal turns a rewritten payload back into a `Term`, so the sharing rule is stated once rather than at each reconstruction. Every child is compared by [`Term::eq`], whose first act is `Rc::ptr_eq`, so an untouched child settles in one pointer comparison and an untouched subtree of any size settles at its root: the check cascades, because unchanged leaves are what make a parent unchanged.
    ///
    /// What it replaces is a fresh `Rc` whose caches start empty, discarding every `hash`, `frees` and `scalars` fill the original had earned. That is affordable when a rewrite rewrites something and pure waste when it does not — and *does not* is the common case. `project_erased_universes` was measured returning an equal term on 1 491 163 of 1 491 163 calls on a nine-definition web of definitions each naming the one before it twice, spending 1.0 s rebuilding and a further 1.6 s re-hashing what it rebuilt, for 4.9 GB of allocation that answered the identity function.
    ///
    /// No caller can tell the difference, because three of them already receive the original node: [`Visit::universes_only`] and [`Visit::prune`] both short-circuit to `self.clone()`, and [`Mode::Sharing`] substitutes a canonical node outright. A span lives on this wrapper rather than on the node, so sharing one node across occurrences was always representable.
    fn rebuilt(&self, subterm: Subterm) -> Self {
        if subterm == **self {
            return self.clone();
        }

        // Preserve the span across traversal; a genuinely rebuilt node is a fresh structure, so its cache starts empty.
        Self {
            span: self.span.clone(),
            inner: Rc::new(Node::new(subterm)),
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
                        Subterm::Apply(apply) => {
                            let mut children = Vec::with_capacity(apply.arguments.len() + 1);
                            children.push(apply.head.clone());
                            children.extend(apply.params().cloned());
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
                        Subterm::Apply(apply) => {
                            let head = children
                                .next()
                                .expect("an application traversal preserves its head");
                            Subterm::Apply(Apply {
                                head,
                                arguments: children
                                    .zip(apply.plicities())
                                    .map(|(term, plicity)| Argument { term, plicity })
                                    .collect(),
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
                    let rebuilt = term.rebuilt(subterm);
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

    /// The cached free-variable set, borrowed.
    ///
    /// [`Bound::free_vars`] hands back an owned clone, which is what a caller that mutates or stores the set wants. A caller that only *reads* it — extending another set, testing membership — pays a deep copy of every [`Free`] for nothing, and a `Free::Global` drags a [`Qualifier`](curios_utilities::Qualifier) of owned segments behind it. The set is already behind an `Rc` precisely so it need not be copied; this is the accessor that keeps that promise.
    pub fn free_vars_shared(&self) -> &BTreeSet<Free> {
        self.get_or_init_free_vars()
    }

    fn get_or_init_free_vars(&self) -> &Rc<BTreeSet<Free>> {
        self.warm_frees();
        self.inner
            .frees
            .get()
            .expect("warm_frees fills the free-variable cache")
    }

    /// Whether `name` occurs free in this term, through the same memoized set [`Bound::free_vars`] fills — but as a membership probe instead of a set clone (`FreeCache::contains`): `define`'s selective reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk it avoids.
    pub fn mentions_free(&self, name: &Free) -> bool {
        self.warm_frees();
        self.inner.frees.contains(name)
    }

    /// The free-variable identities of this term. Inherent so a `term.free_vars()` call routes through the memoized, iteratively-filled set (this and the [`Bound`] impl agree) rather than deref-ing to the uncached, recursive [`Subterm::free_vars`] when the `Bound` trait is out of scope.
    pub fn free_vars(&self) -> BTreeSet<Free> {
        self.get_or_init_free_vars().as_ref().clone()
    }

    /// The ids of every metavariable in this term. Inherent, and gated on the memoized `has_metavar`: a ground term (every data spine) short-circuits without walking, so the enumeration only ever recurses through metavariable-bearing structure, whose depth is bounded by the written program.
    pub fn metavars(&self) -> BTreeSet<MetavarId> {
        let mut ids = BTreeSet::new();
        self.any_metavar(&mut |id| {
            ids.insert(id);
            false
        });
        ids
    }

    /// Whether any metavariable in this term satisfies `pred`, visiting each shared node once. The walk prunes on the cached `has_metavar` bit and dedupes revisits by node identity, because the two prunes fail in each other's gap: a reduction result is a DAG whose tree expansion can be exponential in its depth — one substitution landing a term in two positions doubles it — and a single metavariable at its base, solved or not, sets `has_metavar` on every ancestor, so without the visited set each occurrence of a shared subtree re-pays its whole expansion (measured as a ×2-per-depth elaboration runaway). Skipping a revisit is sound: `pred` is deterministic within one walk, a `true` ends the walk outright, so a recorded node is always one that answered `false`.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        self.any_metavar_walk(pred, &mut HashSet::new())
    }

    fn any_metavar_walk<F: FnMut(MetavarId) -> bool>(
        &self,
        pred: &mut F,
        visited: &mut HashSet<*const Node>,
    ) -> bool {
        if !self.has_metavar() {
            return false;
        }

        // A node reached twice in one walk has two owning handles, so a strong count of one proves this visit is the only one and skips the set — which therefore stays empty (and unallocated) on unshared terms.
        if Rc::strong_count(&self.inner) > 1 && !visited.insert(Rc::as_ptr(&self.inner)) {
            return false;
        }

        if let Subterm::Metavar(Metavar { id, .. }) = &self.inner.subterm
            && pred(*id)
        {
            return true;
        }

        self.inner
            .subterm
            .any_child_term(&mut |child| child.any_metavar_walk(pred, visited))
    }
}

/// Stamp one arm's payload binders with [`Plicity::Explicit`], the shape the plicity-marked inductive-match builders consume — the all-explicit builders' per-arm adapter.
fn explicit_arm<L>(binders: Vec<L>) -> Vec<(Plicity, L)> {
    binders
        .into_iter()
        .map(|label| (Plicity::Explicit, label))
        .collect()
}
