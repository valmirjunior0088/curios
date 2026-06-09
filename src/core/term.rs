use {
    super::{Atom, Flt, Int, Nat, Prim, scope::Visit},
    crate::Span,
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, hash_map::DefaultHasher},
        fmt::Debug,
        hash::{Hash, Hasher},
        ops::Deref,
        rc::Rc,
    },
};

pub use super::scope::{Arity, Bound, Many, One, Two, Var};

/// `core`-stage scope: a body binding `A` de Bruijn variables. Defaults its body
/// to [`Term`], so existing `Scope<One>` / `Scope<Many>` call sites are unchanged.
pub type Scope<A, B = Term> = super::scope::Scope<A, B>;

/// `core`-stage telescope of dependent binders ending in a body of type `B`.
pub type Telescope<B> = super::scope::Telescope<B>;

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

    pub fn prim<P: Into<Prim>>(prim: P) -> Self {
        Self::from(Subterm::Prim(prim.into()))
    }

    pub fn var(var: Var) -> Self {
        Self::from(Subterm::Var(var))
    }

    pub fn metavar(id: usize) -> Self {
        Self::from(Subterm::Metavar(Metavar { id, origin: None }))
    }

    /// A metavariable minted for an omitted implicit argument, carrying its
    /// insertion provenance (see [`Metavar::origin`]).
    pub fn metavar_inserted(id: usize, origin: ImplicitOrigin) -> Self {
        Self::from(Subterm::Metavar(Metavar {
            id,
            origin: Some(origin),
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
        Self::from(Subterm::TupleType(TupleType {
            telescope: Telescope::build(fields, ()),
        }))
    }

    pub fn tuple_unit() -> Self {
        Self::from(Subterm::Tuple(Tuple { fields: vec![] }))
    }

    pub fn tuple<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<Term>,
    {
        Self::from(Subterm::Tuple(Tuple {
            fields: fields.into_iter().map(|t| t.into()).collect(),
        }))
    }

    pub fn proj<H: Into<Term>>(head: H, index: usize) -> Self {
        Self::from(Subterm::Proj(Proj {
            head: head.into(),
            index,
        }))
    }

    pub fn union_type<N, I, P>(name: N, params: I) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::from(Subterm::UnionType(UnionType {
            name: name.into(),
            params: params.into_iter().map(|p| p.into()).collect(),
        }))
    }

    pub fn union_ctor<N, I, P, A, J, Q>(name: N, params: I, tag: A, payload: J) -> Self
    where
        N: Into<String>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
        A: Into<Atom>,
        J: IntoIterator<Item = Q>,
        Q: Into<Term>,
    {
        Self::from(Subterm::UnionCtor(UnionCtor {
            name: name.into(),
            params: params.into_iter().map(|p| p.into()).collect(),
            tag: tag.into(),
            payload: payload.into_iter().map(|p| p.into()).collect(),
        }))
    }

    pub fn union_match<H, M, I, A, L, B>(
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
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: Cases::Union(
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
                    .collect(),
            ),
        }))
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
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: Cases::Bln {
                false_case: false_case.into(),
                true_case: true_case.into(),
            },
        }))
    }

    pub fn nat_induction<H, M, ZC, PL, IL, SC>(
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
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: Cases::NatInduction {
                zero_case: zero_case.into(),
                succ_case: Scope::close(
                    Two,
                    &[pred_label.as_str(), ih_label.as_str()],
                    succ_case.into(),
                ),
            },
        }))
    }

    pub fn nat_dispatch<H, M, I, B, D>(
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
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: Cases::NatDispatch {
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

/// `plicities` parallels the telescope, one mark per binder; the builders
/// assert the lengths agree. `Telescope` itself is unchanged (`TupleType`,
/// `Func`, and the inductive registry carry no plicity).
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

/// An inductive type as a primitive normal form. Built inside the
/// automatically-generated type-constructor function's body. Users never write
/// one directly — they write `Result(A, E)` and the type-constructor function
/// reduces to this. Two `UnionType`s are convertible iff same `name` and
/// pointwise-convertible `params`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionType {
    pub name: String,
    pub params: Vec<Term>,
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
pub struct UnionCtor {
    pub name: String,
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// The unified eliminator: every match form shares a scrutinee and a motive
/// and differs only in its [`Cases`] payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub head: Term,
    pub motive: Scope<One>,
    pub cases: Cases,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cases {
    /// Dependent elimination of `Bln`: a false arm and a true arm.
    Bln { false_case: Term, true_case: Term },
    /// Structural induction on `Nat`: a zero arm plus a successor arm binding
    /// the predecessor and the induction hypothesis.
    NatInduction {
        zero_case: Term,
        succ_case: Scope<Two>,
    },
    /// Sparse dispatch on specific `Nat` values with a default arm.
    NatDispatch {
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
    /// The primitive eliminator of a nominal union: one arm per constructor,
    /// each arm's arity equal to that constructor's payload arity.
    Union(BTreeMap<Atom, Scope<Many>>),
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

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm,
/// born from a surface hole `?` and (possibly) solved by unification. It is a
/// global head carrying no de Bruijn index — like a free `Var`,
/// it is inert under the `Visit` machinery (it holds no `Var`). The solution,
/// when one exists, lives in the `Context`'s `MetaStore`, keyed by `id`; the
/// node itself is immutable.
///
/// `origin` rides with the node: `Some` iff the elaborator minted this
/// metavariable for an omitted implicit argument, in which case zonk's
/// unsolved-hole report names the binder instead of a bare id. Each id is
/// minted exactly once (`to_core` holes with `None`, core insertions above the
/// `Module::metavars` floor with `Some`), so every occurrence of an id carries
/// the same origin and the derived equality never splits an id.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Metavar {
    pub id: usize,
    pub origin: Option<ImplicitOrigin>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subterm {
    Type,
    Prim(Prim),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    Proj(Proj),
    UnionType(UnionType),
    UnionCtor(UnionCtor),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Var(Var),
    Metavar(Metavar),
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
            Subterm::Prim(Prim::Int(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn as_flt(&self) -> Option<Flt> {
        match self {
            Subterm::Prim(Prim::Flt(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect the ids of every metavariable occurring in this subterm. `Visit`
    /// only sees `Var`s and a `Metavar` holds none, so occurs/zonk analyses
    /// cannot piggyback on `free_vars` — this walk enumerates them directly.
    pub fn metavars(&self) -> BTreeSet<usize> {
        let mut ids = BTreeSet::new();
        self.collect_metavars(&mut ids);
        ids
    }

    fn collect_metavars(&self, ids: &mut BTreeSet<usize>) {
        match self {
            Subterm::Metavar(Metavar { id, .. }) => {
                ids.insert(*id);
            }
            Subterm::Type | Subterm::Var(_) => {}
            Subterm::Prim(prim) => prim_metavars(prim, ids),
            Subterm::Func(Func { telescope }) => telescope_metavars(telescope, ids),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope_metavars(telescope, ids),
            Subterm::Apply(Apply { head, params, .. }) => {
                head.collect_metavars(ids);
                params.iter().for_each(|p| p.collect_metavars(ids));
            }
            Subterm::TupleType(TupleType { telescope }) => telescope_metavars(telescope, ids),
            Subterm::Tuple(Tuple { fields }) => {
                fields.iter().for_each(|f| f.collect_metavars(ids));
            }
            Subterm::Proj(Proj { head, .. }) => head.collect_metavars(ids),
            Subterm::UnionType(UnionType { params, .. }) => {
                params.iter().for_each(|p| p.collect_metavars(ids));
            }
            Subterm::UnionCtor(UnionCtor {
                params, payload, ..
            }) => {
                params.iter().for_each(|p| p.collect_metavars(ids));
                payload.iter().for_each(|p| p.collect_metavars(ids));
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                head.collect_metavars(ids);
                motive.body().collect_metavars(ids);
                match cases {
                    Cases::Bln {
                        false_case,
                        true_case,
                    } => {
                        false_case.collect_metavars(ids);
                        true_case.collect_metavars(ids);
                    }
                    Cases::NatInduction {
                        zero_case,
                        succ_case,
                    } => {
                        zero_case.collect_metavars(ids);
                        succ_case.body().collect_metavars(ids);
                    }
                    Cases::NatDispatch { cases, default } => {
                        cases.values().for_each(|b| b.collect_metavars(ids));
                        default.collect_metavars(ids);
                    }
                    Cases::Union(cases) => {
                        cases.values().for_each(|s| s.body().collect_metavars(ids));
                    }
                }
            }
            Subterm::Let(Let { type_, body, tail }) => {
                type_.collect_metavars(ids);
                body.collect_metavars(ids);
                tail.body().collect_metavars(ids);
            }
            Subterm::Rec(Rec { items, tail }) => {
                for (type_, value) in items {
                    type_.body().collect_metavars(ids);
                    value.body().collect_metavars(ids);
                }
                tail.body().collect_metavars(ids);
            }
        }
    }
}

fn telescope_metavars<B>(telescope: &Telescope<B>, ids: &mut BTreeSet<usize>)
where
    B: Bound + CollectMetavars,
{
    match telescope {
        Telescope::Cons(ty, rest) => {
            ty.collect_metavars(ids);
            telescope_metavars(rest.body(), ids);
        }
        Telescope::Done(body) => body.collect_metavars(ids),
    }
}

/// Helper so `telescope_metavars` works uniformly over `Telescope<Term>` (a
/// `FuncType`'s body is a `Term`) and `Telescope<()>` (a `TupleType` has no
/// trailing body term).
trait CollectMetavars {
    fn collect_metavars(&self, ids: &mut BTreeSet<usize>);
}

impl CollectMetavars for Term {
    fn collect_metavars(&self, ids: &mut BTreeSet<usize>) {
        (**self).collect_metavars(ids);
    }
}

impl CollectMetavars for () {
    fn collect_metavars(&self, _: &mut BTreeSet<usize>) {}
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
            Subterm::Prim(prim) => Subterm::Prim(traverse_prim(prim, visit)),
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
            Subterm::Tuple(Tuple { fields }) => Subterm::Tuple(Tuple {
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
            }),
            Subterm::Proj(Proj { head, index }) => Subterm::Proj(Proj {
                head: visit.visit_subterm(head),
                index: *index,
            }),
            Subterm::UnionType(UnionType { name, params }) => Subterm::UnionType(UnionType {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::UnionCtor(UnionCtor {
                name,
                params,
                tag,
                payload,
            }) => Subterm::UnionCtor(UnionCtor {
                name: name.clone(),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                tag: tag.clone(),
                payload: payload.iter().map(|p| visit.visit_subterm(p)).collect(),
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
                    Cases::NatInduction {
                        zero_case,
                        succ_case,
                    } => Cases::NatInduction {
                        zero_case: visit.visit_subterm(zero_case),
                        succ_case: visit.visit_scope(succ_case),
                    },
                    Cases::NatDispatch { cases, default } => Cases::NatDispatch {
                        cases: cases
                            .iter()
                            .map(|(&n, body)| (n, visit.visit_subterm(body)))
                            .collect(),
                        default: visit.visit_subterm(default),
                    },
                    Cases::Union(cases) => Cases::Union(
                        cases
                            .iter()
                            .map(|(atom, scope)| (atom.clone(), visit.visit_scope(scope)))
                            .collect(),
                    ),
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
            // A metavariable holds no `Var`, so it is inert under every visit.
            Subterm::Metavar(m) => Subterm::Metavar(m.clone()),
        }
    }

    fn reach(&self) -> usize {
        match self {
            Subterm::Type | Subterm::Metavar(_) => 0,
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Prim(prim) => prim_reach(prim),
            Subterm::Func(Func { telescope }) => telescope.reach(),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.reach(),
            Subterm::Apply(Apply { head, params, .. }) => head.reach().max(max_reach(params)),
            Subterm::TupleType(TupleType { telescope }) => telescope.reach(),
            Subterm::Tuple(Tuple { fields }) => max_reach(fields),
            Subterm::Proj(Proj { head, .. }) => head.reach(),
            Subterm::UnionType(UnionType { params, .. }) => max_reach(params),
            Subterm::UnionCtor(UnionCtor {
                params, payload, ..
            }) => max_reach(params).max(max_reach(payload)),
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => head.reach().max(motive.reach()).max(match cases {
                Cases::Bln {
                    false_case,
                    true_case,
                } => false_case.reach().max(true_case.reach()),
                Cases::NatInduction {
                    zero_case,
                    succ_case,
                } => zero_case.reach().max(succ_case.reach()),
                Cases::NatDispatch { cases, default } => {
                    max_reach(cases.values()).max(default.reach())
                }
                Cases::Union(cases) => cases.values().map(|s| s.reach()).max().unwrap_or(0),
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

fn prim_reach(prim: &Prim) -> usize {
    match prim {
        Prim::BlnType
        | Prim::Bln(_)
        | Prim::NatType
        | Prim::Nat(Nat::Zero)
        | Prim::IntType
        | Prim::Int(_)
        | Prim::FltType
        | Prim::Flt(_)
        | Prim::BinType
        | Prim::Bin(_)
        | Prim::IoRead => 0,

        Prim::Nat(Nat::Succ(_, inner)) => inner.reach(),

        Prim::NatToStr(t)
        | Prim::IntToStr(t)
        | Prim::FltToStr(t)
        | Prim::NatToInt(t)
        | Prim::NatToFlt(t)
        | Prim::IntToNat(t)
        | Prim::IntToFlt(t)
        | Prim::FltToNat(t)
        | Prim::FltToInt(t)
        | Prim::FltNeg(t)
        | Prim::FltAbs(t)
        | Prim::FltSqrt(t)
        | Prim::FltFloor(t)
        | Prim::FltCeil(t)
        | Prim::FltTrunc(t)
        | Prim::FltNearest(t)
        | Prim::BinLen(t)
        | Prim::ArrType(t)
        | Prim::IoPrint(t) => t.reach(),

        Prim::NatEql(a, b)
        | Prim::NatNeq(a, b)
        | Prim::NatAdd(a, b)
        | Prim::NatSub(a, b)
        | Prim::NatMul(a, b)
        | Prim::NatLt(a, b)
        | Prim::NatDiv(a, b)
        | Prim::NatRem(a, b)
        | Prim::NatGt(a, b)
        | Prim::NatLte(a, b)
        | Prim::NatGte(a, b)
        | Prim::IntEql(a, b)
        | Prim::IntNeq(a, b)
        | Prim::IntAdd(a, b)
        | Prim::IntSub(a, b)
        | Prim::IntMul(a, b)
        | Prim::IntDiv(a, b)
        | Prim::IntRem(a, b)
        | Prim::IntLt(a, b)
        | Prim::IntGt(a, b)
        | Prim::IntLte(a, b)
        | Prim::IntGte(a, b)
        | Prim::FltAdd(a, b)
        | Prim::FltSub(a, b)
        | Prim::FltMul(a, b)
        | Prim::FltDiv(a, b)
        | Prim::FltEql(a, b)
        | Prim::FltNeq(a, b)
        | Prim::FltLt(a, b)
        | Prim::FltGt(a, b)
        | Prim::FltLte(a, b)
        | Prim::FltGte(a, b)
        | Prim::FltMin(a, b)
        | Prim::FltMax(a, b)
        | Prim::BinEql(a, b)
        | Prim::BinGet(a, b)
        | Prim::BinAppend(a, b)
        | Prim::ArrLen(a, b) => a.reach().max(b.reach()),

        Prim::BinSlice(a, b, c) | Prim::ArrGet(a, b, c) | Prim::ArrAppend(a, b, c) => {
            a.reach().max(b.reach()).max(c.reach())
        }

        Prim::ArrSlice(a, b, c, d) => a.reach().max(b.reach()).max(c.reach()).max(d.reach()),

        Prim::BinConcat(terms) | Prim::Arr(terms) => max_reach(terms),
        Prim::ArrConcat(ty, terms) => ty.reach().max(max_reach(terms)),
    }
}

fn prim_metavars(prim: &Prim, ids: &mut BTreeSet<usize>) {
    match prim {
        Prim::BlnType
        | Prim::Bln(_)
        | Prim::NatType
        | Prim::Nat(Nat::Zero)
        | Prim::IntType
        | Prim::Int(_)
        | Prim::FltType
        | Prim::Flt(_)
        | Prim::BinType
        | Prim::Bin(_)
        | Prim::IoRead => {}

        Prim::Nat(Nat::Succ(_, inner)) => inner.collect_metavars(ids),

        Prim::NatToStr(t)
        | Prim::IntToStr(t)
        | Prim::FltToStr(t)
        | Prim::NatToInt(t)
        | Prim::NatToFlt(t)
        | Prim::IntToNat(t)
        | Prim::IntToFlt(t)
        | Prim::FltToNat(t)
        | Prim::FltToInt(t)
        | Prim::FltNeg(t)
        | Prim::FltAbs(t)
        | Prim::FltSqrt(t)
        | Prim::FltFloor(t)
        | Prim::FltCeil(t)
        | Prim::FltTrunc(t)
        | Prim::FltNearest(t)
        | Prim::BinLen(t)
        | Prim::ArrType(t)
        | Prim::IoPrint(t) => t.collect_metavars(ids),

        Prim::NatEql(a, b)
        | Prim::NatNeq(a, b)
        | Prim::NatAdd(a, b)
        | Prim::NatSub(a, b)
        | Prim::NatMul(a, b)
        | Prim::NatLt(a, b)
        | Prim::NatDiv(a, b)
        | Prim::NatRem(a, b)
        | Prim::NatGt(a, b)
        | Prim::NatLte(a, b)
        | Prim::NatGte(a, b)
        | Prim::IntEql(a, b)
        | Prim::IntNeq(a, b)
        | Prim::IntAdd(a, b)
        | Prim::IntSub(a, b)
        | Prim::IntMul(a, b)
        | Prim::IntDiv(a, b)
        | Prim::IntRem(a, b)
        | Prim::IntLt(a, b)
        | Prim::IntGt(a, b)
        | Prim::IntLte(a, b)
        | Prim::IntGte(a, b)
        | Prim::FltAdd(a, b)
        | Prim::FltSub(a, b)
        | Prim::FltMul(a, b)
        | Prim::FltDiv(a, b)
        | Prim::FltEql(a, b)
        | Prim::FltNeq(a, b)
        | Prim::FltLt(a, b)
        | Prim::FltGt(a, b)
        | Prim::FltLte(a, b)
        | Prim::FltGte(a, b)
        | Prim::FltMin(a, b)
        | Prim::FltMax(a, b)
        | Prim::BinEql(a, b)
        | Prim::BinGet(a, b)
        | Prim::BinAppend(a, b)
        | Prim::ArrLen(a, b) => {
            a.collect_metavars(ids);
            b.collect_metavars(ids);
        }

        Prim::BinSlice(a, b, c) | Prim::ArrGet(a, b, c) | Prim::ArrAppend(a, b, c) => {
            a.collect_metavars(ids);
            b.collect_metavars(ids);
            c.collect_metavars(ids);
        }

        Prim::ArrSlice(a, b, c, d) => {
            a.collect_metavars(ids);
            b.collect_metavars(ids);
            c.collect_metavars(ids);
            d.collect_metavars(ids);
        }

        Prim::BinConcat(terms) | Prim::Arr(terms) => {
            terms.iter().for_each(|term| term.collect_metavars(ids))
        }
        Prim::ArrConcat(ty, terms) => {
            ty.collect_metavars(ids);
            terms.iter().for_each(|term| term.collect_metavars(ids));
        }
    }
}

fn traverse_prim<F>(prim: &Prim, visit: &mut Visit<F>) -> Prim
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    match prim {
        Prim::BlnType => Prim::BlnType,
        Prim::Bln(value) => Prim::Bln(*value),
        Prim::NatType => Prim::NatType,
        Prim::Nat(Nat::Zero) => Prim::Nat(Nat::Zero),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            Prim::Nat(Nat::Succ(spine.clone(), visit.visit_subterm(inner)))
        }
        Prim::NatEql(left, right) => {
            Prim::NatEql(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatNeq(left, right) => {
            Prim::NatNeq(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatAdd(left, right) => {
            Prim::NatAdd(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatSub(left, right) => {
            Prim::NatSub(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatMul(left, right) => {
            Prim::NatMul(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatLt(left, right) => {
            Prim::NatLt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatDiv(left, right) => {
            Prim::NatDiv(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatRem(left, right) => {
            Prim::NatRem(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatGt(left, right) => {
            Prim::NatGt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatLte(left, right) => {
            Prim::NatLte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::NatGte(left, right) => {
            Prim::NatGte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntType => Prim::IntType,
        Prim::Int(value) => Prim::Int(*value),
        Prim::IntEql(left, right) => {
            Prim::IntEql(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntNeq(left, right) => {
            Prim::IntNeq(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntAdd(left, right) => {
            Prim::IntAdd(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntSub(left, right) => {
            Prim::IntSub(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntMul(left, right) => {
            Prim::IntMul(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntDiv(left, right) => {
            Prim::IntDiv(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntRem(left, right) => {
            Prim::IntRem(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntLt(left, right) => {
            Prim::IntLt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntGt(left, right) => {
            Prim::IntGt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntLte(left, right) => {
            Prim::IntLte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::IntGte(left, right) => {
            Prim::IntGte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltType => Prim::FltType,
        Prim::Flt(flt) => Prim::Flt(*flt),
        Prim::FltAdd(left, right) => {
            Prim::FltAdd(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltSub(left, right) => {
            Prim::FltSub(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltMul(left, right) => {
            Prim::FltMul(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltDiv(left, right) => {
            Prim::FltDiv(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltEql(left, right) => {
            Prim::FltEql(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltNeq(left, right) => {
            Prim::FltNeq(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltLt(left, right) => {
            Prim::FltLt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltGt(left, right) => {
            Prim::FltGt(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltLte(left, right) => {
            Prim::FltLte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltGte(left, right) => {
            Prim::FltGte(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltMin(left, right) => {
            Prim::FltMin(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltMax(left, right) => {
            Prim::FltMax(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::FltNeg(inner) => Prim::FltNeg(visit.visit_subterm(inner)),
        Prim::FltAbs(inner) => Prim::FltAbs(visit.visit_subterm(inner)),
        Prim::FltSqrt(inner) => Prim::FltSqrt(visit.visit_subterm(inner)),
        Prim::FltFloor(inner) => Prim::FltFloor(visit.visit_subterm(inner)),
        Prim::FltCeil(inner) => Prim::FltCeil(visit.visit_subterm(inner)),
        Prim::FltTrunc(inner) => Prim::FltTrunc(visit.visit_subterm(inner)),
        Prim::FltNearest(inner) => Prim::FltNearest(visit.visit_subterm(inner)),
        Prim::NatToStr(inner) => Prim::NatToStr(visit.visit_subterm(inner)),
        Prim::IntToStr(inner) => Prim::IntToStr(visit.visit_subterm(inner)),
        Prim::FltToStr(inner) => Prim::FltToStr(visit.visit_subterm(inner)),
        Prim::NatToInt(inner) => Prim::NatToInt(visit.visit_subterm(inner)),
        Prim::NatToFlt(inner) => Prim::NatToFlt(visit.visit_subterm(inner)),
        Prim::IntToNat(inner) => Prim::IntToNat(visit.visit_subterm(inner)),
        Prim::IntToFlt(inner) => Prim::IntToFlt(visit.visit_subterm(inner)),
        Prim::FltToNat(inner) => Prim::FltToNat(visit.visit_subterm(inner)),
        Prim::FltToInt(inner) => Prim::FltToInt(visit.visit_subterm(inner)),
        Prim::BinType => Prim::BinType,
        Prim::Bin(bytes) => Prim::Bin(bytes.clone()),
        Prim::BinLen(bin) => Prim::BinLen(visit.visit_subterm(bin)),
        Prim::BinEql(left, right) => {
            Prim::BinEql(visit.visit_subterm(left), visit.visit_subterm(right))
        }
        Prim::BinGet(bin, index) => {
            Prim::BinGet(visit.visit_subterm(bin), visit.visit_subterm(index))
        }
        Prim::BinSlice(bin, start, end) => Prim::BinSlice(
            visit.visit_subterm(bin),
            visit.visit_subterm(start),
            visit.visit_subterm(end),
        ),
        Prim::BinAppend(bin, byte) => {
            Prim::BinAppend(visit.visit_subterm(bin), visit.visit_subterm(byte))
        }
        Prim::BinConcat(operands) => {
            Prim::BinConcat(operands.iter().map(|e| visit.visit_subterm(e)).collect())
        }
        Prim::ArrType(elem) => Prim::ArrType(visit.visit_subterm(elem)),
        Prim::Arr(elems) => Prim::Arr(elems.iter().map(|e| visit.visit_subterm(e)).collect()),
        Prim::ArrLen(ty, list) => Prim::ArrLen(visit.visit_subterm(ty), visit.visit_subterm(list)),
        Prim::ArrGet(ty, list, index) => Prim::ArrGet(
            visit.visit_subterm(ty),
            visit.visit_subterm(list),
            visit.visit_subterm(index),
        ),
        Prim::ArrSlice(ty, list, start, end) => Prim::ArrSlice(
            visit.visit_subterm(ty),
            visit.visit_subterm(list),
            visit.visit_subterm(start),
            visit.visit_subterm(end),
        ),
        Prim::ArrAppend(ty, list, elem) => Prim::ArrAppend(
            visit.visit_subterm(ty),
            visit.visit_subterm(list),
            visit.visit_subterm(elem),
        ),
        Prim::ArrConcat(ty, operands) => Prim::ArrConcat(
            visit.visit_subterm(ty),
            operands.iter().map(|e| visit.visit_subterm(e)).collect(),
        ),
        Prim::IoPrint(inner) => Prim::IoPrint(visit.visit_subterm(inner)),
        Prim::IoRead => Prim::IoRead,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metavar_is_a_closed_global_head() {
        let m = Term::metavar(7);
        assert_eq!(m.reach(), 0);
        assert!(m.closed());
        assert_eq!(format!("{m}"), "?7");
    }

    #[test]
    fn metavars_collects_ids_across_structure() {
        // (λx. ?1)(?2, Nat.add ?3 ?1)
        let term = Term::apply(
            Term::func([("x", Term::type_())], Term::metavar(1)),
            [
                Term::metavar(2),
                Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
            ],
        );
        assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3]));
    }

    #[test]
    fn metavar_is_inert_under_traversal() {
        // shifting/capture must not disturb a metavariable node
        let m = Term::metavar(4);
        assert_eq!(m.shift(3), m);
        let scope = Scope::close(One, &["x"], Term::metavar(4));
        assert_eq!(scope.open(&[&Term::var(Var::free("y"))]), Term::metavar(4));
    }

    #[test]
    fn union_ctor_collects_metavars_and_prints_as_function_call() {
        let ctor = Term::union_ctor(
            "Result",
            [Term::metavar(1)],
            "success",
            [Term::metavar(2)],
        );
        assert_eq!(ctor.metavars(), BTreeSet::from([1, 2]));
        assert_eq!(format!("{ctor}"), "Result/success(?2)");

        let type_ = Term::union_type("Result", [Term::prim(Prim::NatType), Term::metavar(3)]);
        assert_eq!(type_.metavars(), BTreeSet::from([3]));
        assert_eq!(format!("{type_}"), "Result(Nat, ?3)");
    }

    #[test]
    fn implicit_marks_print_and_default_to_explicit() {
        let ft = Term::func_type_marked(
            [
                (Plicity::Implicit, "T", Term::type_()),
                (Plicity::Explicit, "x", Term::var(Var::free("T"))),
            ],
            Term::var(Var::free("T")),
        );
        assert_eq!(format!("{ft}"), "(@T : Type, x : T) -> T");

        // The unmarked builders default every slot to `Explicit`.
        let plain = Term::func_type([("T", Term::type_())], Term::type_());
        match &*plain {
            Subterm::FuncType(FuncType { plicities, .. }) => {
                assert_eq!(plicities, &[Plicity::Explicit]);
            }
            _ => unreachable!(),
        }

        let call = Term::apply_marked(
            Term::var(Var::free("foo")),
            [
                (Plicity::Implicit, Term::var(Var::free("Nat"))),
                (Plicity::Explicit, Term::var(Var::free("x"))),
            ],
        );
        assert_eq!(format!("{call}"), "foo(@Nat, x)");
    }

    #[test]
    fn union_match_case_binders_are_captured() {
        // match r : #m => Type; | success(value) => value;
        let term = Term::union_match(
            Term::var(Var::free("r")),
            None,
            Term::type_(),
            [(
                "success",
                vec!["value"],
                Term::var(Var::free("value")),
            )],
        );

        let free = term.free_vars();
        assert!(free.contains("r"));
        assert!(!free.contains("value"));
    }

    #[test]
    fn union_variants_reach_spans_components() {
        assert_eq!(
            Term::union_type("Result", [Term::var(Var::bound(2))]).reach(),
            3
        );
        assert_eq!(
            Term::union_ctor(
                "Result",
                [Term::var(Var::bound(0))],
                "success",
                [Term::var(Var::bound(4))],
            )
            .reach(),
            5
        );
    }

    #[test]
    fn reach_basic_values() {
        assert_eq!(Term::type_().reach(), 0);
        assert_eq!(Term::var(Var::free("x")).reach(), 0);
        assert_eq!(Term::var(Var::bound(0)).reach(), 1);
        assert_eq!(Term::var(Var::bound(3)).reach(), 4);
        // closed identity function λx.x
        assert_eq!(
            Term::func([("x", Term::type_())], Term::var(Var::free("x"))).reach(),
            0
        );
    }

    #[test]
    fn reach_telescope_absorbs_arity() {
        // body references bound index 2 (reach 3); each telescope binder absorbs one.
        // `Scope::constant` places the body without capturing, so the bound index is
        // preserved exactly (unlike `Telescope::cons`, which captures by label).
        let f1 = Term::from(Subterm::Func(Func {
            telescope: Telescope::Cons(
                Term::type_(),
                Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
            ),
        }));
        assert_eq!(f1.reach(), 2); // one binder: (2 + 1) - 1

        let f2 = Term::from(Subterm::Func(Func {
            telescope: Telescope::Cons(
                Term::type_(),
                Scope::constant(
                    One,
                    Telescope::Cons(
                        Term::type_(),
                        Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
                    ),
                ),
            ),
        }));
        assert_eq!(f2.reach(), 1); // two binders: (2 + 1) - 2
    }

    #[test]
    fn open_shares_closed_body_without_rebuild() {
        // body does not mention the bound variable -> open returns the stored Rc unchanged
        let scope = Scope::close(One, &["x"], Term::type_());
        let opened = scope.open(&[&Term::var(Var::free("y"))]);
        assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
    }

    #[test]
    fn open_shares_closed_subterm_inside_substituted_body() {
        let closed = Term::func([("a", Term::type_())], Term::var(Var::free("a"))); // λa.a, closed
        let scope = Scope::close(
            One,
            &["x"],
            Term::tuple([Term::var(Var::free("x")), closed]),
        );

        let stored_field = match &**scope.body() {
            Subterm::Tuple(Tuple { fields }) => fields[1].clone(),
            _ => panic!("expected tuple body"),
        };

        let opened = scope.open(&[&Term::var(Var::free("y"))]);

        let opened_field = match &*opened {
            Subterm::Tuple(Tuple { fields }) => fields[1].clone(),
            _ => panic!("expected tuple result"),
        };

        // the substituted field changed; the closed field is shared, not rebuilt
        assert_eq!(opened_field, stored_field);
        assert!(Rc::ptr_eq(&opened_field.inner, &stored_field.inner));
    }
}
