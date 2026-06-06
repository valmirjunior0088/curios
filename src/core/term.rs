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

    pub fn atom<A: Into<Atom>>(atom: A) -> Self {
        Self::from(Subterm::Atom(atom.into()))
    }

    pub fn spanned<T: Into<Term>>(span: Span, inner: T) -> Self {
        inner.into().with_span(span)
    }

    pub fn atom_type<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Atom>,
    {
        Self::from(Subterm::AtomType(AtomType {
            atoms: atoms.into_iter().map(Into::into).collect(),
        }))
    }

    pub fn func_type<I, L, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
        O: Into<Term>,
    {
        Self::from(Subterm::FuncType(FuncType {
            telescope: Telescope::build(params, output.into()),
        }))
    }

    pub fn func<I, L, B>(labels: I, body: B) -> Self
    where
        I: IntoIterator<Item = L>,
        L: Into<String>,
        B: Into<Term>,
    {
        let labels = labels
            .into_iter()
            .map(|l| l.into())
            .collect::<Vec<String>>();

        let label_strs = labels.iter().map(|s| s.as_str()).collect::<Vec<_>>();

        Self::from(Subterm::Func(Func {
            body: Scope::close(Many(labels.len()), &label_strs, body.into()),
        }))
    }

    pub fn apply<H, I, P>(head: H, params: I) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self::from(Subterm::Apply(Apply {
            head: head.into(),
            params: params.into_iter().map(|p| p.into()).collect(),
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

    pub fn match_<H, M, I, A, B>(head: H, motive_label: Option<&str>, motive: M, cases: I) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        Self::from(Subterm::Match(Match {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom.into(), body.into()))
                .collect(),
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
        Self::from(Subterm::BlnMatch(BlnMatch {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            false_case: false_case.into(),
            true_case: true_case.into(),
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

        Self::from(Subterm::NatMatch(NatMatch::Induction {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            zero_case: zero_case.into(),
            succ_case: Scope::close(
                Two,
                &[pred_label.as_str(), ih_label.as_str()],
                succ_case.into(),
            ),
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
        Self::from(Subterm::NatMatch(NatMatch::Dispatch {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: cases.into_iter().map(|(n, b)| (n, b.into())).collect(),
            default: default.into(),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub telescope: Telescope<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
    pub body: Scope<Many>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NatMatch {
    Induction {
        head: Term,
        motive: Scope<One>,
        zero_case: Term,
        succ_case: Scope<Two>,
    },
    Dispatch {
        head: Term,
        motive: Scope<One>,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlnMatch {
    pub head: Term,
    pub motive: Scope<One>,
    pub false_case: Term,
    pub true_case: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub head: Term,
    pub motive: Scope<One>,
    pub cases: BTreeMap<Atom, Term>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subterm {
    Type,
    Prim(Prim),
    BlnMatch(BlnMatch),
    NatMatch(NatMatch),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    Proj(Proj),
    AtomType(AtomType),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Var(Var),
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
            Subterm::AtomType(at) => Subterm::AtomType(at.clone()),
            Subterm::Atom(atom) => Subterm::Atom(atom.clone()),
            Subterm::FuncType(FuncType { telescope }) => Subterm::FuncType(FuncType {
                telescope: telescope.traverse(visit),
            }),
            Subterm::Func(Func { body }) => Subterm::Func(Func {
                body: visit.visit_scope(body),
            }),
            Subterm::Apply(Apply { head, params }) => Subterm::Apply(Apply {
                head: visit.visit_subterm(head),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
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
            Subterm::BlnMatch(BlnMatch {
                head,
                motive,
                false_case,
                true_case,
            }) => Subterm::BlnMatch(BlnMatch {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                false_case: visit.visit_subterm(false_case),
                true_case: visit.visit_subterm(true_case),
            }),
            Subterm::NatMatch(NatMatch::Induction {
                head,
                motive,
                zero_case,
                succ_case,
            }) => Subterm::NatMatch(NatMatch::Induction {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                zero_case: visit.visit_subterm(zero_case),
                succ_case: visit.visit_scope(succ_case),
            }),
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            }) => Subterm::NatMatch(NatMatch::Dispatch {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                cases: cases
                    .iter()
                    .map(|(&n, body)| (n, visit.visit_subterm(body)))
                    .collect(),
                default: visit.visit_subterm(default),
            }),
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => Subterm::Match(Match {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                cases: cases
                    .iter()
                    .map(|(atom, body)| (atom.clone(), visit.visit_subterm(body)))
                    .collect(),
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
        }
    }

    fn reach(&self) -> usize {
        match self {
            Subterm::Type | Subterm::Atom(_) | Subterm::AtomType(_) => 0,
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Prim(prim) => prim_reach(prim),
            Subterm::Func(Func { body }) => body.reach(),
            Subterm::FuncType(FuncType { telescope }) => telescope.reach(),
            Subterm::Apply(Apply { head, params }) => head.reach().max(max_reach(params)),
            Subterm::TupleType(TupleType { telescope }) => telescope.reach(),
            Subterm::Tuple(Tuple { fields }) => max_reach(fields),
            Subterm::Proj(Proj { head, .. }) => head.reach(),
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => head
                .reach()
                .max(motive.reach())
                .max(max_reach(cases.values())),
            Subterm::BlnMatch(BlnMatch {
                head,
                motive,
                false_case,
                true_case,
            }) => head
                .reach()
                .max(motive.reach())
                .max(false_case.reach())
                .max(true_case.reach()),
            Subterm::NatMatch(NatMatch::Induction {
                head,
                motive,
                zero_case,
                succ_case,
            }) => head
                .reach()
                .max(motive.reach())
                .max(zero_case.reach())
                .max(succ_case.reach()),
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            }) => head
                .reach()
                .max(motive.reach())
                .max(max_reach(cases.values()))
                .max(default.reach()),
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
    fn reach_basic_values() {
        assert_eq!(Term::type_().reach(), 0);
        assert_eq!(Term::var(Var::free("x")).reach(), 0);
        assert_eq!(Term::var(Var::bound(0)).reach(), 1);
        assert_eq!(Term::var(Var::bound(3)).reach(), 4);
        // closed identity function λx.x
        assert_eq!(Term::func(["x"], Term::var(Var::free("x"))).reach(), 0);
    }

    #[test]
    fn reach_scope_absorbs_arity() {
        // body references bound index 2 (reach 3); a scope absorbs its arity
        let f1 = Term::from(Subterm::Func(Func {
            body: Scope::constant(Many(1), Term::var(Var::bound(2))),
        }));
        assert_eq!(f1.reach(), 2); // (2 + 1) - 1

        let f2 = Term::from(Subterm::Func(Func {
            body: Scope::constant(Many(2), Term::var(Var::bound(2))),
        }));
        assert_eq!(f2.reach(), 1); // (2 + 1) - 2
    }

    #[test]
    fn open_shares_closed_body_without_rebuild() {
        // body does not mention the bound variable -> open returns the stored Rc unchanged
        let scope = Scope::close(One, &["x"], Term::atom("k"));
        let opened = scope.open(&[&Term::var(Var::free("y"))]);
        assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
    }

    #[test]
    fn open_shares_closed_subterm_inside_substituted_body() {
        let closed = Term::func(["a"], Term::var(Var::free("a"))); // λa.a, closed
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
