use {
    super::{
        Atom, Bound, Flt, Int, Many, Nat, One, Prim, Scope, Telescope, Three, Two, Var, Visit,
    },
    crate::Span,
    num_bigint::BigUint,
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, hash_map::DefaultHasher},
        fmt::Debug,
        hash::{Hash, Hasher},
        ops::Deref,
        rc::Rc,
    },
};

#[derive(Debug, Clone)]
pub struct Term {
    span: Option<Span>,
    hash: OnceCell<u64>,
    reach: OnceCell<usize>,
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

    pub fn unwrap_or_clone(this: Self) -> Subterm {
        Rc::unwrap_or_clone(this.inner)
    }

    /// The free-variable label at the head of an application spine, descending
    /// through curried `Apply` heads: `classify(c)` and `f(a)(b)` report the
    /// label of `classify` / `f`. A bare free variable reports itself; anything
    /// else is `None`. Used to cheaply gate scrutinee-refinement
    /// canonicalization on the applied symbol before paying for argument
    /// reduction.
    pub fn head_label(&self) -> Option<&str> {
        match &*self.inner {
            Subterm::Apply(Apply { head, .. }) => head.head_label(),
            Subterm::Var(var) => var.as_free(),
            _ => None,
        }
    }

    /// Returns the span attached to this term, if any.
    pub fn span(&self) -> Option<Span> {
        self.span.clone()
    }

    /// Attaches a span to this term. If the term already carries a span (the innermost
    /// one), it is preserved — innermost wins, matching how `Error::at` keeps the first
    /// span it sees as errors propagate up.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }

    pub fn type_() -> Self {
        Self::from(Subterm::Type)
    }

    pub fn prop() -> Self {
        Self::from(Subterm::Prop)
    }

    pub fn prim<P: Into<Prim>>(prim: P) -> Self {
        Self::from(Subterm::Prim(prim.into()))
    }

    pub fn var(var: Var) -> Self {
        Self::from(Subterm::Var(var))
    }

    pub fn free_var<A: Into<String>>(label: A) -> Self {
        Self::var(Var::free(label))
    }

    pub fn infix(op: NumOp, left: Term, right: Term) -> Self {
        Self::from(Subterm::Infix(Infix { op, left, right }))
    }

    pub fn num_lit(magnitude: BigUint, signed: bool, negative: bool) -> Self {
        Self::from(Subterm::NumLit(NumLit {
            magnitude,
            signed,
            negative,
        }))
    }

    pub fn metavar(id: impl Into<MetavarId>) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: Rc::new(Vec::new()),
            origin: None,
        }))
    }

    /// A metavariable minted for an omitted implicit argument, carrying its
    /// insertion provenance (see [`Metavar::origin`]) and its birth spine.
    pub fn metavar_inserted(
        id: impl Into<MetavarId>,
        origin: ImplicitOrigin,
        spine: impl Into<Rc<Vec<Term>>>,
    ) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: spine.into(),
            origin: Some(origin),
        }))
    }

    /// A hole rebuilt at its birth point with the identity spine over its
    /// frozen telescope (see [`Metavar::spine`]).
    pub fn metavar_birthed(
        id: impl Into<MetavarId>,
        origin: Option<ImplicitOrigin>,
        spine: impl Into<Rc<Vec<Term>>>,
    ) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id: id.into(),
            spine: spine.into(),
            origin,
        }))
    }

    pub fn spanned<T: Into<Term>>(span: Span, inner: T) -> Self {
        inner.into().with_span(span)
    }

    pub fn func_type<I, L, T, O>(params: I, output: O) -> Self
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

    pub fn tuple_type<I, L, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
    {
        let telescope = Telescope::build(fields, ());

        Self::from(Subterm::TupleType(TupleType { telescope }))
    }

    pub fn tuple_unit() -> Self {
        Self::from(Subterm::Tuple(Tuple {
            fields: vec![],
            names: vec![],
        }))
    }

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

    pub fn proj<H: Into<Term>>(head: H, index: usize) -> Self {
        Self::from(Subterm::Proj(Proj {
            head: head.into(),
            field: Field::Index(index),
        }))
    }

    pub fn proj_label<H: Into<Term>, L: Into<String>>(head: H, label: L) -> Self {
        Self::from(Subterm::Proj(Proj {
            head: head.into(),
            field: Field::Label(label.into()),
        }))
    }

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
            names: vec![],
        }))
    }

    /// A struct literal carrying the written field names from `to_core`;
    /// elaboration validates them positionally and rebuilds name-free, exactly
    /// like `tuple_named`.
    pub fn struct_named<N, I, P, J, T>(name: N, params: I, fields: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        J: IntoIterator<Item = (Option<String>, T)>,
        T: Into<Term>,
    {
        let (mut names, fields): (Vec<_>, Vec<_>) = fields
            .into_iter()
            .map(|(name, term)| (name, term.into()))
            .unzip();

        if names.iter().all(Option::is_none) {
            names = vec![];
        }

        Self::from(Subterm::Struct(Struct {
            name: name.into(),
            params: params.into_iter().map(|p| p.into()).collect(),
            fields,
            names,
        }))
    }

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

    #[allow(clippy::too_many_arguments)]
    pub fn arr_match<H, M, EL, EC, HL, TL, IL, CC>(
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
                carrier: Carrier::Arr {
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

    #[allow(clippy::too_many_arguments)]
    pub fn bin_match<H, M, EC, HL, TL, IL, CC>(
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

    pub fn let_<L, T, B, U>(label: L, type_: T, body: B, tail: U) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let label = label.into();

        Self::from(Subterm::Let(Let {
            type_: type_.into(),
            body: body.into(),
            tail: Scope::close(One, &[label.as_str()], tail.into()),
        }))
    }

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

        Self::from(Subterm::Rec(Rec {
            items: items
                .into_iter()
                .map(|(_, type_, value)| {
                    (
                        Scope::close(Many(labels.len()), &labels, type_),
                        Scope::close(Many(labels.len()), &labels, value),
                    )
                })
                .collect(),
            tail: Scope::close(Many(labels.len()), &labels, tail.into()),
        }))
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
            inner: Rc::new(term),
        }
    }
}

/// Whether a binder/argument participates in implicit-argument insertion.
/// An elaboration directive only: conversion ignores plicity entirely, so
/// erasing the marks yields exactly the unmarked system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Plicity {
    Explicit,
    Implicit,
}

/// A fixed, overloaded infix operator. The surface parser maps an operator
/// symbol (with its precedence) onto one of these; elaboration resolves it to a
/// concrete scalar primitive once the operand type is known (`elaborate_infix`).
/// Both `NumOp` and the [`Infix`]/[`NumLit`] nodes are *elaboration-transient*:
/// born in `to_core`, consumed by `elaborate` (replaced with a `Prim` term), so
/// they never reach reduce/convert/zonk/erase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eql,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

impl NumOp {
    /// The operator's source spelling, for printing and error messages.
    pub fn symbol(self) -> &'static str {
        match self {
            NumOp::Add => "+",
            NumOp::Sub => "-",
            NumOp::Mul => "*",
            NumOp::Div => "/",
            NumOp::Rem => "%",
            NumOp::Eql => "==",
            NumOp::Neq => "!=",
            NumOp::Lt => "<",
            NumOp::Gt => ">",
            NumOp::Lte => "<=",
            NumOp::Gte => ">=",
            NumOp::And => "&&",
            NumOp::Or => "||",
        }
    }

    /// Comparison and equality operators yield `Bln` regardless of operand type;
    /// arithmetic operators yield the operand type.
    pub fn result_is_bln(self) -> bool {
        matches!(
            self,
            NumOp::Eql | NumOp::Neq | NumOp::Lt | NumOp::Gt | NumOp::Lte | NumOp::Gte
        )
    }

    /// Resolve the operator to the concrete primitive constructor for an operand
    /// type whose whnf head is `type_head`. `None` when no primitive realizes
    /// this operator at that type — `%` on `Flt`, `!=` on `Bln`, or any
    /// arithmetic/`&&`/`||` outside its supported scalar set — which the caller
    /// turns into an "operator not defined for type" error.
    pub fn prim_for(self, type_head: &Subterm) -> Option<fn(Term, Term) -> Prim> {
        let Subterm::Prim(prim) = type_head else {
            return None;
        };

        Some(match (self, prim) {
            (NumOp::Add, Prim::NatType) => Prim::NatAdd,
            (NumOp::Add, Prim::IntType) => Prim::IntAdd,
            (NumOp::Add, Prim::FltType) => Prim::FltAdd,
            (NumOp::Sub, Prim::NatType) => Prim::NatSub,
            (NumOp::Sub, Prim::IntType) => Prim::IntSub,
            (NumOp::Sub, Prim::FltType) => Prim::FltSub,
            (NumOp::Mul, Prim::NatType) => Prim::NatMul,
            (NumOp::Mul, Prim::IntType) => Prim::IntMul,
            (NumOp::Mul, Prim::FltType) => Prim::FltMul,
            (NumOp::Div, Prim::NatType) => Prim::NatDiv,
            (NumOp::Div, Prim::IntType) => Prim::IntDiv,
            (NumOp::Div, Prim::FltType) => Prim::FltDiv,
            (NumOp::Rem, Prim::NatType) => Prim::NatRem,
            (NumOp::Rem, Prim::IntType) => Prim::IntRem,
            (NumOp::Rem, Prim::FltType) => Prim::FltRem,
            (NumOp::Eql, Prim::NatType) => Prim::NatEql,
            (NumOp::Eql, Prim::IntType) => Prim::IntEql,
            (NumOp::Eql, Prim::FltType) => Prim::FltEql,
            (NumOp::Eql, Prim::BlnType) => Prim::BlnEql,
            (NumOp::Neq, Prim::NatType) => Prim::NatNeq,
            (NumOp::Neq, Prim::IntType) => Prim::IntNeq,
            (NumOp::Neq, Prim::FltType) => Prim::FltNeq,
            (NumOp::Neq, Prim::BlnType) => Prim::BlnNeq,
            (NumOp::Lt, Prim::NatType) => Prim::NatLt,
            (NumOp::Lt, Prim::IntType) => Prim::IntLt,
            (NumOp::Lt, Prim::FltType) => Prim::FltLt,
            (NumOp::Gt, Prim::NatType) => Prim::NatGt,
            (NumOp::Gt, Prim::IntType) => Prim::IntGt,
            (NumOp::Gt, Prim::FltType) => Prim::FltGt,
            (NumOp::Lte, Prim::NatType) => Prim::NatLte,
            (NumOp::Lte, Prim::IntType) => Prim::IntLte,
            (NumOp::Lte, Prim::FltType) => Prim::FltLte,
            (NumOp::Gte, Prim::NatType) => Prim::NatGte,
            (NumOp::Gte, Prim::IntType) => Prim::IntGte,
            (NumOp::Gte, Prim::FltType) => Prim::FltGte,
            (NumOp::And, Prim::BlnType) => Prim::BlnAnd,
            (NumOp::Or, Prim::BlnType) => Prim::BlnOr,
            _ => return None,
        })
    }
}

/// An unresolved infix application `left <op> right`. Elaboration infers a
/// shared operand type for the two sides and dispatches `op` to the matching
/// scalar primitive ([`NumOp::prim_for`]); the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct NumLit {
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
pub struct FuncType {
    pub telescope: Telescope<Term>,
    pub plicities: Vec<Plicity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
    pub telescope: Telescope<Term>,
}

/// `plicities` parallels `params`, one mark per argument — the call-site `@`
/// marks. Core must carry them (rather than `to_core` resolving them) because
/// `to_core` is type-blind: only the elaborator, holding the head's function
/// type, can decide which binder an `@`-argument fills.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
    pub plicities: Vec<Plicity>,
}

/// A dependent product (Σ-type). Erasure is sort-driven: a proof or type-valued
/// field is a *subset type* witness — dropped at erasure, leaving the relevant
/// fields (and collapsing to the bare field when only one remains).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

/// `names` carries the literal's written field names (`(status = 0, …)`) from
/// `to_core` to elaboration, which checks them against the expected tuple
/// type's labels and rebuilds the literal name-free. Empty means "no names
/// written" — the invariant for every internally-built and post-elaboration
/// tuple.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// A projection's field is positional in every post-elaboration term; the
/// `Label` form exists only between `to_core` and `elaborate`, which resolves
/// it against the head's tuple type and rebuilds it as `Index`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    Index(usize),
    Label(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct StructType {
    pub name: String,
    pub params: Vec<Term>,
}

/// A struct value as a primitive normal form (cf. [`Variant`], no tag).
/// `name`/`params` are recoverable from the inferred type but stored
/// redundantly so `convert` stays purely structural.
///
/// `names` carries the literal's written field names from `to_core`, exactly
/// as [`Tuple`] does: elaboration checks them positionally against the declared
/// labels and rebuilds the value name-free. Empty means "no names written" —
/// the invariant for every internally-built and post-elaboration struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub name: String,
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive
/// and differs only in its [`Cases`] payload.
///
/// The motive's arity is 1 (the scrutinee binder) for every form except a
/// inductive match with an annotated type-pattern motive, where the pattern's
/// binder slots precede the scrutinee binder (in slot order, scrutinee last).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct MotivePattern {
    /// The (resolved) inductive name the annotation wrote — checked against the
    /// scrutinee's actual inductive.
    pub name: String,
    pub slots: Vec<MotiveSlot>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MotiveSlot {
    /// `_` or a bare identifier: occupies the next binder of the motive
    /// scope, in slot order before the scrutinee binder.
    Binder,
    /// Any other written term — parameters only.
    Term(Term),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Inductive {
        cases: BTreeMap<Atom, Scope<Many>>,
        pattern: Option<MotivePattern>,
    },
    /// Structural induction on a native free-monoid primitive (`Nat`/`Arr`/
    /// `Bin`): the `carrier` selects the primitive and carries both its parameters
    /// (`Arr`'s element type) and its two arms — an identity arm plus a cons arm
    /// binding the head generator (absent for `Nat`, whose unary generator carries
    /// no payload), the tail, and the induction hypothesis at the tail.
    FreeMonoid { carrier: Carrier },
}

/// The native free-monoid primitive a `Cases::FreeMonoid` eliminates, with its
/// type parameters and its two eliminator arms. `Nat` is the free monoid on one
/// (payload-less) generator; `Bin` carries none; `Arr` carries its element
/// type. Each variant pairs an identity arm (`empty_case`) with a cons arm whose
/// arity is fixed by the carrier — `Scope<Two>` for `Nat` (predecessor, ih),
/// `Scope<Three>` for `Bin`/`Arr` (head, tail, ih).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Carrier {
    Nat {
        empty_case: Term,
        cons_case: Scope<Two>,
    },
    Bin {
        empty_case: Term,
        cons_case: Scope<Three>,
    },
    Arr {
        elem: Term,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub type_: Term,
    pub body: Term,
    pub tail: Scope<One>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rec {
    pub items: Vec<(Scope<Many>, Scope<Many>)>,
    pub tail: Scope<Many>,
}

/// Provenance of an inserted implicit argument: the applied function (`func`)
/// had no `@`-argument for its implicit binder `binder` at some call site, so
/// the elaborator filled the slot with a fresh metavariable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplicitOrigin {
    pub func: String,
    pub binder: String,
}

/// A metavariable's identity: a dense index into the `Context`'s `MetaStore`,
/// minted monotonically by an [`Entropy`](crate::Entropy). A newtype so it can
/// never be confused with the other `usize`-shaped notions the kernel juggles
/// (de Bruijn indices, telescope arities, variant tags, `Nat` magnitudes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MetavarId(pub usize);

impl From<usize> for MetavarId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl crate::Mint for MetavarId {
    fn mint(entropy: usize) -> Self {
        Self(entropy)
    }
}

impl std::fmt::Display for MetavarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm,
/// born from a surface hole `?` and (possibly) solved by unification. The
/// solution, when one exists, lives in the `Context`'s `MetaStore`, keyed by
/// `id`, spelled with the *birth telescope's* free names.
///
/// `origin` rides with the node: `Some` iff the elaborator minted this
/// metavariable for an omitted implicit argument, in which case zonk's
/// unsolved-hole report names the binder instead of a bare id. Each id is
/// minted exactly once (`to_core` holes with `None`, core insertions above
/// the floor `to_core` returns with `Some`), so every occurrence of an id
/// carries the same origin and the derived equality never splits an id.
///
/// `spine` is the delayed substitution — one term per binder of the birth
/// telescope (`MetaEntry::telescope` order), recording what that binder
/// corresponds to at this occurrence. Identity (`Var::free(name)`) at birth.
/// The entries are ordinary term content: `traverse` walks them, so `close`
/// captures them and `open` substitutes them, and the mapping survives
/// re-closing under fresh names — which is what lets a solution mentioning a
/// sibling binder resolve correctly wherever the occurrence ends up. An empty
/// spine is a not-yet-birthed `to_core` hole and resolves as the identity.
///
/// The spine is `Rc`-shared: every meta born under the same Γ shares one
/// identity-spine allocation (see `Context::identity_snapshot`), which is what
/// keeps minting metavariables O(1) instead of O(|Γ|).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Metavar {
    pub id: MetavarId,
    pub spine: Rc<Vec<Term>>,
    pub origin: Option<ImplicitOrigin>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Var(Var),
    Metavar(Metavar),
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_numlit`.
    NumLit(NumLit),
}

impl Subterm {
    pub fn as_nat(&self) -> Option<Nat> {
        match self {
            Subterm::Prim(Prim::Nat(nat)) => Some(nat.clone()),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<Int> {
        match self {
            Subterm::Prim(Prim::Int(value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn as_flt(&self) -> Option<Flt> {
        match self {
            Subterm::Prim(Prim::Flt(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn as_bln(&self) -> Option<bool> {
        match self {
            Subterm::Prim(Prim::Bln(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk enumerates them directly.
    pub fn metavars(&self) -> BTreeSet<MetavarId> {
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

    pub fn collect_construction_names(&self, names: &mut BTreeSet<String>) {
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
            Subterm::Func(Func { telescope }) => {
                telescope_term_construction_names(telescope, names)
            }
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope_term_construction_names(telescope, names)
            }
            Subterm::Apply(Apply { head, params, .. }) => {
                head.collect_construction_names(names);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::TupleType(TupleType { telescope, .. }) => {
                telescope_unit_construction_names(telescope, names)
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
                    Cases::Inductive { cases, pattern } => {
                        cases
                            .values()
                            .for_each(|s| s.body().collect_construction_names(names));
                        pattern.iter().flat_map(|p| &p.slots).for_each(|slot| {
                            if let MotiveSlot::Term(t) = slot {
                                t.collect_construction_names(names);
                            }
                        });
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
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::Arr {
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
            Subterm::Let(Let { type_, body, tail }) => {
                type_.collect_construction_names(names);
                body.collect_construction_names(names);
                tail.body().collect_construction_names(names);
            }
            Subterm::Rec(Rec { items, tail }) => {
                for (type_, value) in items {
                    type_.body().collect_construction_names(names);
                    value.body().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
        }
    }

    /// Whether any metavariable occurring in this subterm satisfies `pred`,
    /// stopping at the first hit. The early-exit dual of `collect_metavars`
    /// (which is this with a collector that never stops): the reducer's memo
    /// gate uses it to reject caching a WHNF that still names an unsolved
    /// metavariable, without allocating the full id set.
    pub fn any_metavar<F: FnMut(MetavarId) -> bool>(&self, pred: &mut F) -> bool {
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
                        Cases::Inductive { cases, pattern } => {
                            cases.values().any(|s| s.body().any_metavar(pred))
                                || pattern
                                    .iter()
                                    .flat_map(|p| &p.slots)
                                    .any(|slot| match slot {
                                        MotiveSlot::Term(t) => t.any_metavar(pred),
                                        _ => false,
                                    })
                        }
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => empty_case.any_metavar(pred) || cons_case.body().any_metavar(pred),
                            Carrier::Bin {
                                empty_case,
                                cons_case,
                            } => empty_case.any_metavar(pred) || cons_case.body().any_metavar(pred),
                            Carrier::Arr {
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
            Subterm::Let(Let { type_, body, tail }) => {
                type_.any_metavar(pred) || body.any_metavar(pred) || tail.body().any_metavar(pred)
            }
            Subterm::Rec(Rec { items, tail }) => {
                items.iter().any(|(type_, value)| {
                    type_.body().any_metavar(pred) || value.body().any_metavar(pred)
                }) || tail.body().any_metavar(pred)
            }
        }
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk (an `any_metavar` whose
    /// collector never short-circuits) enumerates them directly.
    pub fn collect_metavars(&self, ids: &mut BTreeSet<MetavarId>) {
        self.any_metavar(&mut |id| {
            ids.insert(id);
            false
        });
    }
}

// Walk a function/Π telescope (`Func`/`FuncType`): the parameter types and the
// trailing body/return type. Concrete in `Term` — no collector trait needed.
fn telescope_term_construction_names(telescope: &Telescope<Term>, names: &mut BTreeSet<String>) {
    match telescope {
        Telescope::Cons(ty, rest) => {
            ty.collect_construction_names(names);
            telescope_term_construction_names(rest.body(), names);
        }
        Telescope::Done(body) => body.collect_construction_names(names),
    }
}

// Walk a Σ telescope (`TupleType`): only the field types — its `Done` body is `()`.
fn telescope_unit_construction_names(telescope: &Telescope<()>, names: &mut BTreeSet<String>) {
    if let Telescope::Cons(ty, rest) = telescope {
        ty.collect_construction_names(names);
        telescope_unit_construction_names(rest.body(), names);
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
            inner: Rc::new((**self).traverse(visit)),
        }
    }

    fn reach(&self) -> usize {
        *self.reach.get_or_init(|| self.inner.reach())
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
                names,
            }) => Subterm::Struct(Struct {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                names: names.clone(),
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
                    Cases::Inductive { cases, pattern } => Cases::Inductive {
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
                                empty_case,
                                cons_case,
                            } => Carrier::Bin {
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                            Carrier::Arr {
                                elem,
                                empty_case,
                                cons_case,
                            } => Carrier::Arr {
                                elem: visit.visit_subterm(elem),
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                        },
                    },
                },
            }),
            Subterm::Let(Let { type_, body, tail }) => Subterm::Let(Let {
                type_: visit.visit_subterm(type_),
                body: visit.visit_subterm(body),
                tail: visit.visit_scope(tail),
            }),
            Subterm::Rec(Rec { items, tail }) => Subterm::Rec(Rec {
                items: items
                    .iter()
                    .map(|(type_, value)| (visit.visit_scope(type_), visit.visit_scope(value)))
                    .collect(),
                tail: visit.visit_scope(tail),
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
                Cases::Inductive { cases, pattern } => {
                    cases.values().map(|s| s.reach()).max().unwrap_or(0).max(
                        pattern
                            .iter()
                            .flat_map(|p| &p.slots)
                            .fold(0, |acc, slot| match slot {
                                MotiveSlot::Binder => acc,
                                MotiveSlot::Term(t) => acc.max(t.reach()),
                            }),
                    )
                }
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat {
                        empty_case,
                        cons_case,
                    } => empty_case.reach().max(cons_case.reach()),
                    Carrier::Bin {
                        empty_case,
                        cons_case,
                    } => empty_case.reach().max(cons_case.reach()),
                    Carrier::Arr {
                        elem,
                        empty_case,
                        cons_case,
                    } => elem.reach().max(empty_case.reach()).max(cons_case.reach()),
                },
            }),
            Subterm::Let(Let { type_, body, tail }) => {
                type_.reach().max(body.reach()).max(tail.reach())
            }
            Subterm::Rec(Rec { items, tail }) => items
                .iter()
                .map(|(type_, value)| type_.reach().max(value.reach()))
                .max()
                .unwrap_or(0)
                .max(tail.reach()),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn open_shares_closed_body_without_rebuild() {
        // body does not mention the bound variable -> open returns the stored Rc unchanged
        let scope = Scope::close(One, &["x"], Term::type_());
        let opened = scope.open(&[&Term::free_var("y")]);
        assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
    }

    #[test]
    fn open_shares_closed_subterm_inside_substituted_body() {
        let closed = Term::func([("a", Term::type_())], Term::free_var("a")); // λa.a, closed
        let scope = Scope::close(One, &["x"], Term::tuple([Term::free_var("x"), closed]));

        let stored_field = match &**scope.body() {
            Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
            _ => panic!("expected tuple body"),
        };

        let opened = scope.open(&[&Term::free_var("y")]);

        let opened_field = match &*opened {
            Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
            _ => panic!("expected tuple result"),
        };

        // the substituted field changed; the closed field is shared, not rebuilt
        assert_eq!(opened_field, stored_field);
        assert!(Rc::ptr_eq(&opened_field.inner, &stored_field.inner));
    }
}
