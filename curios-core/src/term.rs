#[cfg(test)]
mod tests;

use {
    super::{Atom, Bound, Many, Nat, Prim, Scope, Telescope, Three, Two, Var, Visit, print_term},
    curios_base::{Flt, Grain, Int, Mint, NumOp, Plicity, Span, printer::run_printer},
    num_bigint::BigUint,
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, hash_map::DefaultHasher},
        fmt,
        hash::{Hash, Hasher},
        ops::Deref,
        rc::Rc,
    },
};

#[cfg(feature = "archive")]
use curios_base::BigUintBytes;

/// A core-calculus term: an `Rc`-shared [`Subterm`] plus a lazily-cached structural hash, `reach`, and free-variable set, and an optional source span. Clones are pointer bumps, and equality short-circuits first on pointer identity, then on the cached hashes, before falling back to structural comparison — which is what keeps conversion and the reduction memo affordable on heavily shared trees. The span is identity-irrelevant: hash and equality look only at the inner node, so re-spanning a term never splits a cache.
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
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    hash: OnceCell<u64>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    reach: OnceCell<usize>,
    #[cfg_attr(feature = "archive", rkyv(with = rkyv::with::Skip))]
    free_vars: OnceCell<Rc<BTreeSet<String>>>,
    #[cfg_attr(feature = "archive", rkyv(omit_bounds))]
    inner: Rc<Subterm>,
}

impl Term {
    fn get_or_init_hash(&self) -> u64 {
        *self.hash.get_or_init(|| {
            let mut hasher = DefaultHasher::new();
            self.inner.hash(&mut hasher);

            hasher.finish()
        })
    }

    pub(crate) fn unwrap_or_clone(this: Self) -> Subterm {
        Rc::unwrap_or_clone(this.inner)
    }

    /// The free-variable label at the head of an application spine, descending
    /// through curried `Apply` heads: `classify(c)` and `f(a)(b)` report the
    /// label of `classify` / `f`. A bare free variable reports itself; anything
    /// else is `None`. Used to cheaply gate scrutinee-refinement
    /// canonicalization on the applied symbol before paying for argument
    /// reduction.
    pub(crate) fn head_label(&self) -> Option<&str> {
        match &*self.inner {
            Subterm::Apply(Apply { head, .. }) => head.head_label(),
            Subterm::Var(var) => var.as_free(),
            _ => None,
        }
    }

    /// Return the canonical target when this term is a straightforward
    /// transparent alias body: either a single free variable or its
    /// eta-expanded parameterized form `(xs) => Original(xs)`. The text-stage
    /// interface audit uses this after name resolution to preserve
    /// representation provenance; computed bodies are not classified as aliases.
    pub fn transparent_alias_target(&self) -> Option<String> {
        match &*self.inner {
            Subterm::Var(var) => var.as_free().map(str::to_string),
            Subterm::Func(Func { telescope }) => {
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
                Subterm::Type | Subterm::Prop => true,
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
                Subterm::Func(Func { telescope }) => direct_head(telescope.terminal()),
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

    /// The universe of types, `Type` (the trailing underscore dodges the Rust keyword).
    pub fn type_() -> Self {
        Self::from(Subterm::Type)
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
    pub fn metavar(id: impl Into<MetavarId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: None,
        }))
    }

    /// A written goal `?`, as `into_core` mints one: a bare metavariable (empty spine, like [`Term::metavar`]) whose [`MetavarOrigin::Goal`] origin makes zonk *report* what elaboration determined for it — scope, type, and solution — instead of splicing silently.
    pub fn goal(id: impl Into<MetavarId>) -> Self {
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
        id: impl Into<MetavarId>,
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

    /// Build a function literal from `(label, annotation)` parameters, closing the body over the labels via a [`Telescope`]. No plicity marks — see [`Func`].
    pub fn func<I, L, T, B>(params: I, body: B) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
    {
        Self::from(Subterm::Func(Func {
            telescope: Telescope::build(params, body.into()),
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

    /// Build an [`InductiveType`] normal form — the body of the generated type-constructor function. See the type's docs for the `params`/`indices` split.
    pub fn inductive_type<N, I, P, J, Q>(name: N, params: I, indices: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::InductiveType(InductiveType {
            name: name.into(),
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
        Self::from(Subterm::Variant(Variant {
            name: name.into(),
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
        Self::from(Subterm::StructType(StructType {
            name: name.into(),
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
        Self::from(Subterm::Struct(Struct {
            name: name.into(),
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
            params: params.into_iter().map(|p| p.into()).collect(),
            fields,
            entries,
        }))
    }

    /// Build the primitive eliminator of a nominal inductive ([`Cases::Inductive`]) without a type-pattern annotation: one arm per constructor tag, each closed over its payload binders. The annotated-motive form is [`Term::inductive_match_motive`].
    pub fn inductive_match<H, M, I, A, L, B>(
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
        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::Inductive {
                cases: Self::inductive_cases(cases),
                pattern: None,
                default: None,
            },
        }))
    }

    /// The primitive eliminator of a nominal inductive with an explicit `| _ =>`
    /// catch-all ([`Cases::Inductive`]'s `default`): the enumerated arms plus a
    /// binding-free default standing in for every other constructor tag. The
    /// dispatching analogue of [`Term::inductive_match`], mirroring how
    /// [`Term::switch`] relates to [`Term::nat_match`].
    pub fn inductive_match_default<H, M, I, A, L, B, D>(
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
        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::Inductive {
                cases: Self::inductive_cases(cases),
                pattern: None,
                default: Some(default.into()),
            },
        }))
    }

    /// An inductive match with the annotated type-pattern motive: the motive body
    /// is closed over the pattern's binder labels (slot order) then the
    /// scrutinee label. `binders` must list one label per
    /// [`MotiveSlot::Binder`] in `pattern.slots`, in order.
    pub fn inductive_match_motive<H, M, I, A, L, B>(
        head: H,
        binders: Vec<String>,
        scrutinee_label: &str,
        motive: M,
        pattern: MotivePattern,
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
        let labels = binders
            .iter()
            .map(String::as_str)
            .chain([scrutinee_label])
            .collect::<Vec<_>>();

        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Scope::close(Many(labels.len()), &labels, motive.into()),
            cases: Cases::Inductive {
                cases: Self::inductive_cases(cases),
                pattern: Some(pattern),
                default: None,
            },
        }))
    }

    fn inductive_cases<I, A, L, B>(cases: I) -> BTreeMap<Atom, Scope<Many>>
    where
        I: IntoIterator<Item = (A, Vec<L>, B)>,
        A: Into<Atom>,
        L: Into<String>,
        B: Into<Term>,
    {
        cases
            .into_iter()
            .map(|(atom, binders, body)| {
                let binders = binders.into_iter().map(Into::into).collect::<Vec<_>>();
                let binders = binders.iter().map(String::as_str).collect::<Vec<_>>();
                (
                    atom.into(),
                    Scope::close(Many(binders.len()), &binders, body.into()),
                )
            })
            .collect()
    }

    /// Build a match's arity-1 motive scope from an optional source label: a
    /// named scope when the label is present, a constant one when not. Shared by
    /// every match constructor whose motive binds just the scrutinee.
    fn motive_scope(motive_label: Option<&str>, motive: Term) -> Scope<Many> {
        match motive_label {
            Some(label) => Scope::close(Many(1), &[label], motive),
            None => Scope::constant(Many(1), motive),
        }
    }

    /// Build the dependent `Bln` eliminator ([`Cases::Bln`]): a false arm and a true arm, neither binding anything — the motive alone sees the scrutinee.
    pub fn bln_match<H, M, F, T>(
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
        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::Bln {
                false_case: false_case.into(),
                true_case: true_case.into(),
            },
        }))
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
        let pred_label = pred_label.into();
        let ih_label = ih_label.into();

        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::FreeMonoid {
                carrier: Carrier::Nat {
                    empty_case: zero_case.into(),
                    cons_case: Scope::close(
                        Two,
                        &[pred_label.as_str(), ih_label.as_str()],
                        succ_case.into(),
                    ),
                },
            },
        }))
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
        let head_label = head_label.into();
        let tail_label = tail_label.into();
        let ih_label = ih_label.into();

        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::FreeMonoid {
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
        }))
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
        let head_label = head_label.into();
        let tail_label = tail_label.into();
        let ih_label = ih_label.into();

        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::FreeMonoid {
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
        }))
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
        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: Self::motive_scope(motive_label, motive.into()),
            cases: Cases::Switch {
                cases: cases.into_iter().map(|(n, b)| (n, b.into())).collect(),
                default: default.into(),
            },
        }))
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
                merged.push((type_, body));

                for (binding_type, binding_value) in &bindings {
                    merged.push((
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
                bindings: vec![(type_, body)],
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

        *self.inner == *other.inner
    }
}

impl Eq for Term {}

impl AsRef<Subterm> for Term {
    fn as_ref(&self) -> &Subterm {
        &self.inner
    }
}

impl Deref for Term {
    type Target = Subterm;

    fn deref(&self) -> &Subterm {
        &self.inner
    }
}

impl From<Subterm> for Term {
    fn from(term: Subterm) -> Self {
        Self {
            span: None,
            hash: OnceCell::new(),
            reach: OnceCell::new(),
            free_vars: OnceCell::new(),
            inner: Rc::new(term),
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
        if visit.prune() && self.reach() <= visit.depth() {
            return self.clone();
        }

        // Preserve the span across traversal.
        Self {
            span: self.span.clone(),
            hash: OnceCell::new(),
            reach: OnceCell::new(),
            free_vars: OnceCell::new(),
            inner: Rc::new((**self).traverse(visit)),
        }
    }

    fn reach(&self) -> usize {
        *self.reach.get_or_init(|| self.inner.reach())
    }

    /// Cached alongside `hash`/`reach`: a closed subterm that `traverse`'s
    /// pruning short-circuit (above) hands back via `Rc::clone` keeps this
    /// same cell across every later traversal, so a term shared across many
    /// conversion goals — e.g. a `rec` group's own unchanging members,
    /// re-enqueued each round an unfolding cycle revisits them — pays this
    /// O(size) walk once rather than once per goal. Uniform in every term,
    /// not specific to recursive ones; see `Convert::history_key`.
    fn free_vars(&self) -> BTreeSet<String> {
        self.free_vars
            .get_or_init(|| {
                let mut vars = BTreeSet::new();
                self.traverse(&mut Visit::new(|_, var| {
                    if let Some(label) = var.as_free() {
                        vars.insert(label.to_string());
                    }
                    None
                }));
                Rc::new(vars)
            })
            .as_ref()
            .clone()
    }
}

/// An unresolved infix application `left <op> right`. Elaboration infers a
/// shared operand type for the two sides and rebuilds the node as a concept
/// method call (`a + b` ≙ `Add/add(a, b)`; `&&`/`||` alone are hardcoded on
/// `Bln` — see `elaborate_infix`); the node never survives elaboration.
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

/// A function literal: the parameter annotations and the body as one [`Telescope`] (each entry a parameter type, the `Done` payload the body). Unlike [`FuncType`]/[`Apply`], a lambda carries no plicity marks — its binders are matched against the expected function type's marks during elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Func {
    pub telescope: Telescope<Term>,
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
/// reduces to this. Two `InductiveType`s are convertible iff same `name` and
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
pub struct InductiveType {
    pub name: String,
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
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// A struct type as a primitive normal form (cf. [`InductiveType`], no indices).
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
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub entries: Vec<StructEntry>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive
/// and differs only in its [`Cases`] payload.
///
/// The motive's arity is 1 (the scrutinee binder) for every form except an
/// inductive match with an annotated type-pattern motive, where the pattern's
/// binder slots precede the scrutinee binder (in slot order, scrutinee last).
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

/// The written type-pattern of an annotated inductive-match motive,
/// `match v : (x : Vec(T, k)) => P`. Slots are positional over the inductive's
/// flat argument list (parameters then indices — told apart via the registry
/// during elaboration, which consumes and validates the pattern):
///
/// - a parameter slot may be a verbatim [`MotiveSlot::Term`] (checked
///   convertible with the scrutinee's actual parameter) or a binder (opened
///   with the actual parameter);
/// - an index slot must be a binder — that is the point of the form.
///
/// Binder labels live in the motive scope itself; `slots` records which
/// positions bind and carries the verbatim terms.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct MotivePattern {
    /// The (resolved) inductive name the annotation wrote — checked against the
    /// scrutinee's actual inductive.
    pub name: String,
    pub slots: Vec<MotiveSlot>,
}

/// One positional slot of a [`MotivePattern`] — see there for which positions may take which form.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum MotiveSlot {
    /// `_` or a bare identifier: occupies the next binder of the motive
    /// scope, in slot order before the scrutinee binder.
    Binder,
    /// Any other written term — parameters only.
    Term(Term),
}

/// The arm payload of a [`Match`] — the only part that differs between the elimination forms (the scrutinee and motive live on `Match` itself). Which variant a match carries decides both its reduction rule and how erasure lowers it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Cases {
    /// Dependent elimination of `Bln`: a false arm and a true arm.
    Bln { false_case: Term, true_case: Term },
    /// Sparse dispatch on specific `Nat` values with a default arm.
    Switch {
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
    /// The primitive eliminator of a nominal inductive: one arm per constructor,
    /// each arm's arity equal to that constructor's payload arity. `pattern`
    /// is `Some` iff the surface motive was the annotated type-pattern form.
    /// `default` is the optional catch-all arm (`| _ =>`, mirroring
    /// [`Cases::Switch`]'s): present iff the surface match ended in a bare `_`.
    /// It binds nothing and stands in for every constructor tag absent from
    /// `cases`; `None` means the arms structurally cover every constructor
    /// (a true elimination). A `Some(default)` may not co-occur with `pattern`
    /// — the annotated type-pattern motive is elimination-only.
    Inductive {
        cases: BTreeMap<Atom, Scope<Many>>,
        pattern: Option<MotivePattern>,
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
    pub bindings: Vec<(Term, Term)>,
    pub tail: Scope<Many>,
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
    items: Rc<Vec<(Scope<Many>, Scope<Many>)>>,
}

impl RecGroup {
    pub(crate) fn new(items: Vec<(Scope<Many>, Scope<Many>)>) -> Self {
        Self {
            items: Rc::new(items),
        }
    }

    pub(crate) fn items(&self) -> &[(Scope<Many>, Scope<Many>)] {
        &self.items
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }

    pub(crate) fn members(&self) -> Vec<Term> {
        (0..self.len())
            .map(|index| Term::rec_member(self.clone(), index))
            .collect()
    }

    pub(crate) fn member_type(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.items[index].0.open(&refs)
    }

    pub(crate) fn member_body(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.items[index].1.open(&refs)
    }

    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        Self::new(
            self.items
                .iter()
                .map(|(type_, body)| (visit.visit_scope(type_), visit.visit_scope(body)))
                .collect(),
        )
    }

    fn reach(&self) -> usize {
        self.items
            .iter()
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
pub struct MetavarId(pub usize);

impl From<usize> for MetavarId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for MetavarId {
    fn mint(entropy: usize) -> Self {
        Self(entropy)
    }
}

impl fmt::Display for MetavarId {
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
    pub id: MetavarId,
    pub spine: Rc<Vec<Term>>,
    pub origin: Option<MetavarOrigin>,
}

/// The actual node of the core term language — one variant per term former. [`Term`] wraps a `Subterm` in an `Rc` with cached hash/reach and an optional span, and `Deref`s here, so pattern matches are written against `Subterm` while construction goes through `Term`'s smart constructors. The final two variants (`Infix`, `NumLit`) are elaboration-transient: born in `into_core`, consumed by `elaborate`, never seen by reduce/convert/zonk/erase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Subterm {
    Type,
    Prop,
    Prim(Prim),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    InductiveType(InductiveType),
    Variant(Variant),
    Match(Match),
    StructType(StructType),
    Struct(Struct),
    Proj(Proj),
    Let(Let),
    Rec(Rec),
    RecMember(RecMember),
    Var(Var),
    Metavar(Metavar),
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_numlit`.
    NumLit(NumLit),
}

impl Subterm {
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
            Subterm::Prim(Prim::Bln(value)) => Some(*value),
            _ => None,
        }
    }

    /// The free-variable labels occurring in this subterm — the inherent-method spelling of [`Bound::free_vars`], callable without importing the trait.
    pub fn free_vars(&self) -> BTreeSet<String> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk enumerates them directly.
    pub(crate) fn metavars(&self) -> BTreeSet<MetavarId> {
        let mut ids = BTreeSet::new();
        self.collect_metavars(&mut ids);
        ids
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
            Subterm::Type | Subterm::Prop | Subterm::Var(_) => {}
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
            Subterm::Func(Func { telescope }) => telescope.collect_construction_names(names),
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
            Subterm::InductiveType(InductiveType {
                name,
                params,
                indices,
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
            Subterm::StructType(StructType { name, params }) => {
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
                    Cases::Bln {
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
                    Cases::Inductive {
                        cases,
                        pattern,
                        default,
                    } => {
                        cases
                            .values()
                            .for_each(|s| s.body().collect_construction_names(names));
                        pattern.iter().flat_map(|p| &p.slots).for_each(|slot| {
                            if let MotiveSlot::Term(t) = slot {
                                t.collect_construction_names(names);
                            }
                        });
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
            Subterm::Let(Let { bindings, tail }) => {
                for (type_, value) in bindings {
                    type_.collect_construction_names(names);
                    value.collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::Rec(Rec { group, tail }) => {
                for (type_, value) in group.items() {
                    type_.body().collect_construction_names(names);
                    value.body().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::RecMember(RecMember { group, .. }) => {
                for (type_, value) in group.items() {
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
    pub(crate) fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { id, spine, .. }) => {
                pred(*id) || spine.iter().any(|t| t.any_metavar(pred))
            }
            Subterm::Type | Subterm::Prop | Subterm::Var(_) => false,
            Subterm::NumLit(_) => false,
            Subterm::Infix(Infix { left, right, .. }) => {
                left.any_metavar(pred) || right.any_metavar(pred)
            }
            Subterm::Prim(prim) => prim.any_metavar(pred),
            Subterm::Func(Func { telescope }) => telescope.any_metavar(pred),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.any_metavar(pred),
            Subterm::Apply(Apply { head, params, .. }) => {
                head.any_metavar(pred) || params.iter().any(|p| p.any_metavar(pred))
            }
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.any_metavar(pred),
            Subterm::Tuple(Tuple { fields, .. }) => fields.iter().any(|f| f.any_metavar(pred)),
            Subterm::Proj(Proj { head, .. }) => head.any_metavar(pred),
            Subterm::InductiveType(InductiveType {
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
                        Cases::Bln {
                            false_case,
                            true_case,
                        } => false_case.any_metavar(pred) || true_case.any_metavar(pred),
                        Cases::Switch { cases, default } => {
                            cases.values().any(|b| b.any_metavar(pred)) || default.any_metavar(pred)
                        }
                        Cases::Inductive {
                            cases,
                            pattern,
                            default,
                        } => {
                            cases.values().any(|s| s.body().any_metavar(pred))
                                || pattern
                                    .iter()
                                    .flat_map(|p| &p.slots)
                                    .any(|slot| match slot {
                                        MotiveSlot::Term(t) => t.any_metavar(pred),
                                        _ => false,
                                    })
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
            Subterm::Let(Let { bindings, tail }) => {
                bindings
                    .iter()
                    .any(|(type_, value)| type_.any_metavar(pred) || value.any_metavar(pred))
                    || tail.body().any_metavar(pred)
            }
            Subterm::Rec(Rec { group, tail }) => {
                group.items().iter().any(|(type_, value)| {
                    type_.body().any_metavar(pred) || value.body().any_metavar(pred)
                }) || tail.body().any_metavar(pred)
            }
            Subterm::RecMember(RecMember { group, .. }) => {
                group.items().iter().any(|(type_, value)| {
                    type_.body().any_metavar(pred) || value.body().any_metavar(pred)
                })
            }
        }
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk (an `any_metavar` whose
    /// collector never short-circuits) enumerates them directly.
    fn collect_metavars(&self, ids: &mut BTreeSet<MetavarId>) {
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
            Subterm::Type => Subterm::Type,
            Subterm::Prop => Subterm::Prop,
            Subterm::Prim(prim) => Subterm::Prim(prim.traverse(visit)),
            Subterm::FuncType(FuncType {
                telescope,
                plicities,
            }) => Subterm::FuncType(FuncType {
                telescope: telescope.traverse(visit),
                plicities: plicities.clone(),
            }),
            Subterm::Func(Func { telescope }) => Subterm::Func(Func {
                telescope: telescope.traverse(visit),
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
            Subterm::InductiveType(InductiveType {
                name,
                params,
                indices,
            }) => Subterm::InductiveType(InductiveType {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                indices: indices.iter().map(|i| visit.visit_subterm(i)).collect(),
            }),
            Subterm::Variant(Variant {
                name,
                params,
                tag,
                payload,
            }) => Subterm::Variant(Variant {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                tag: tag.clone(),
                payload: payload.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::StructType(StructType { name, params }) => Subterm::StructType(StructType {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::Struct(Struct {
                name,
                params,
                fields,
                entries,
            }) => Subterm::Struct(Struct {
                name: name.clone(),
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
                    Cases::Bln {
                        false_case,
                        true_case,
                    } => Cases::Bln {
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
                    Cases::Inductive {
                        cases,
                        pattern,
                        default,
                    } => Cases::Inductive {
                        cases: cases
                            .iter()
                            .map(|(atom, scope)| (atom.clone(), visit.visit_scope(scope)))
                            .collect(),
                        // Verbatim slot terms live in the enclosing scope
                        // (outside the motive's binders), like `head`.
                        pattern: pattern.as_ref().map(|p| MotivePattern {
                            name: p.name.clone(),
                            slots: p
                                .slots
                                .iter()
                                .map(|slot| match slot {
                                    MotiveSlot::Binder => MotiveSlot::Binder,
                                    MotiveSlot::Term(t) => MotiveSlot::Term(visit.visit_subterm(t)),
                                })
                                .collect(),
                        }),
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
                    .map(|(i, (type_, value))| {
                        visit.enter_scope(i);
                        let out = (visit.visit_subterm(type_), visit.visit_subterm(value));
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
            Subterm::Type => 0,
            Subterm::Prop => 0,
            Subterm::NumLit(_) => 0,
            Subterm::Infix(Infix { left, right, .. }) => left.reach().max(right.reach()),
            Subterm::Metavar(Metavar { spine, .. }) => max_reach(spine.as_slice()),
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Prim(prim) => prim.reach(),
            Subterm::Func(Func { telescope }) => telescope.reach(),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.reach(),
            Subterm::Apply(Apply { head, params, .. }) => head.reach().max(max_reach(params)),
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.reach(),
            Subterm::Tuple(Tuple { fields, .. }) => max_reach(fields),
            Subterm::Proj(Proj { head, .. }) => head.reach(),
            Subterm::InductiveType(InductiveType {
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
                Cases::Bln {
                    false_case,
                    true_case,
                } => false_case.reach().max(true_case.reach()),
                Cases::Switch { cases, default } => max_reach(cases.values()).max(default.reach()),
                Cases::Inductive {
                    cases,
                    pattern,
                    default,
                } => {
                    cases
                        .values()
                        .map(|s| s.reach())
                        .max()
                        .unwrap_or(0)
                        .max(pattern.iter().flat_map(|p| &p.slots).fold(
                            0,
                            |acc, slot| match slot {
                                MotiveSlot::Binder => acc,
                                MotiveSlot::Term(t) => acc.max(t.reach()),
                            },
                        ))
                        .max(default.as_ref().map_or(0, |d| d.reach()))
                }
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
            Subterm::Let(Let { bindings, tail }) => {
                let mut reach = tail.reach();

                for (i, (type_, value)) in bindings.iter().enumerate() {
                    reach = reach
                        .max(type_.reach().saturating_sub(i))
                        .max(value.reach().saturating_sub(i));
                }

                reach
            }
            Subterm::Rec(Rec { group, tail }) => group.reach().max(tail.reach()),
            Subterm::RecMember(RecMember { group, .. }) => group.reach(),
        }
    }
}

fn max_reach<'a>(terms: impl IntoIterator<Item = &'a Term>) -> usize {
    terms
        .into_iter()
        .map(|term| term.reach())
        .max()
        .unwrap_or(0)
}
