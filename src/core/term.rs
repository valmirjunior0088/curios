use {
    super::{Arity, Atom, Many, Nat, One, Prim, Two},
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VarType {
    Free(String),
    Bound(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    type_: VarType,
}

impl Var {
    pub fn free<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: VarType::Free(label.into()),
        }
    }

    fn as_free(&self) -> Option<&str> {
        match &self.type_ {
            VarType::Free(label) => Some(label),
            VarType::Bound(_) => None,
        }
    }

    fn bound(index: usize) -> Self {
        Self {
            type_: VarType::Bound(index),
        }
    }

    fn as_bound(&self) -> Option<usize> {
        match &self.type_ {
            VarType::Free(_) => None,
            &VarType::Bound(index) => Some(index),
        }
    }

    pub fn unwrap(&self) -> &str {
        self.as_free().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Term {
    hash: OnceCell<u64>,
    reach: OnceCell<usize>,
    inner: Rc<Subterm>,
}

impl Term {
    pub fn new(term: Subterm) -> Self {
        Self {
            hash: OnceCell::new(),
            reach: OnceCell::new(),
            inner: Rc::new(term),
        }
    }

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

        if let (Some(a), Some(b)) = (self.hash.get(), other.hash.get())
            && a != b
        {
            return false;
        }

        *self.inner == *other.inner
    }
}

impl Eq for Term {}

impl Deref for Term {
    type Target = Subterm;
    fn deref(&self) -> &Subterm {
        &self.inner
    }
}

impl From<Subterm> for Term {
    fn from(term: Subterm) -> Self {
        Self::new(term)
    }
}

impl AsRef<Subterm> for Term {
    fn as_ref(&self) -> &Subterm {
        &self.inner
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope<A: Arity, B: Bound = Term> {
    arity: A,
    names: Option<Vec<String>>,
    body: Box<B>,
}

impl<A, B> Scope<A, B>
where
    A: Arity,
    B: Bound,
{
    pub fn close<'a>(arity: A, labels: A::Params<'a, str>, body: B) -> Self {
        assert!(
            arity.arity() == labels.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            labels.as_ref().len()
        );

        Self {
            arity,
            names: Some(labels.as_ref().iter().map(|s| s.to_string()).collect()),
            body: body.capture(labels.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    fn reach(&self) -> usize {
        self.body.reach().saturating_sub(self.arity())
    }

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> B {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn constant(arity: A, body: B) -> Self {
        Self {
            arity,
            names: None,
            body: body.into(),
        }
    }

    pub fn first_label(&self) -> Option<&str> {
        self.names.as_deref()?.first().map(String::as_str)
    }

    pub fn second_label(&self) -> Option<&str> {
        self.names.as_deref()?.get(1).map(String::as_str)
    }

    pub fn label_iter(&self) -> impl Iterator<Item = Option<&str>> {
        (0..self.arity()).map(move |i| {
            self.names
                .as_deref()
                .and_then(|ns| ns.get(i))
                .map(String::as_str)
        })
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        self.body.free_vars()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Telescope<B: Bound> {
    Done(Box<B>),
    Cons(Term, Scope<One, Telescope<B>>),
}

impl<B: Bound> Bound for Telescope<B> {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Telescope::Cons(ty, rest) => {
                Telescope::Cons(visit.visit_subterm(ty), visit.visit_scope(rest))
            }
            Telescope::Done(body) => Telescope::Done(body.traverse(visit).into()),
        }
    }

    fn reach(&self) -> usize {
        match self {
            Telescope::Cons(ty, rest) => ty.reach().max(rest.reach()),
            Telescope::Done(body) => body.reach(),
        }
    }
}

impl<B: Bound> Telescope<B> {
    pub fn done(body: B) -> Self {
        Telescope::Done(body.into())
    }

    pub fn cons<L, T>(label: L, ty: T, rest: Telescope<B>) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
    {
        let label = label.into();
        Telescope::Cons(ty.into(), Scope::close(One, &[label.as_str()], rest))
    }

    pub fn build<I, L, T>(entries: I, body: B) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
    {
        entries
            .into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(Telescope::done(body), |rest, (l, t)| {
                Telescope::cons(l, t, rest)
            })
    }

    pub fn len(&self) -> usize {
        let mut n = 0;
        let mut cur = self;
        while let Telescope::Cons(_, rest) = cur {
            n += 1;
            cur = &rest.body;
        }
        n
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Telescope::Done(_))
    }

    pub fn first_label(&self) -> Option<&str> {
        match self {
            Telescope::Cons(_, rest) => rest.first_label(),
            Telescope::Done(_) => None,
        }
    }

    pub fn open(&self, args: &[&Term]) -> B {
        assert!(
            self.len() == args.len(),
            "telescope arity mismatch in `open`: expected {}, got {}",
            self.len(),
            args.len()
        );

        let mut cur = self.clone();
        for arg in args {
            cur = match cur {
                Telescope::Cons(_, rest) => rest.open(&[arg]),
                Telescope::Done(_) => unreachable!(),
            };
        }
        match cur {
            Telescope::Done(body) => *body,
            Telescope::Cons(_, _) => unreachable!(),
        }
    }

    pub fn nth<F>(self, index: usize, mut sub: F) -> Option<Term>
    where
        F: FnMut(usize) -> Term,
    {
        fn go<B: Bound, F: FnMut(usize) -> Term>(
            tele: Telescope<B>,
            index: usize,
            j: usize,
            sub: &mut F,
        ) -> Option<Term> {
            match tele {
                Telescope::Done(_) => None,
                Telescope::Cons(ty, rest) => {
                    if j == index {
                        Some(ty)
                    } else {
                        go(rest.open(&[&sub(j)]), index, j + 1, sub)
                    }
                }
            }
        }

        go(self, index, 0, &mut sub)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub telescope: Telescope<Term>,
}

impl FuncType {
    pub fn new<I, L, T, O>(params: I, output: O) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
        O: Into<Term>,
    {
        Self {
            telescope: Telescope::build(params, output.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
    pub body: Scope<Many>,
}

impl Func {
    pub fn new<I, L, B>(labels: I, body: B) -> Self
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

        Self {
            body: Scope::close(Many(labels.len()), &label_strs, body.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
}

impl Apply {
    pub fn new<H, I, P>(head: H, params: I) -> Self
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        Self {
            head: head.into(),
            params: params.into_iter().map(|p| p.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

impl TupleType {
    pub fn unit() -> Self {
        Self {
            telescope: Telescope::done(()),
        }
    }

    pub fn new<I, L, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = (L, T)>,
        L: Into<String>,
        T: Into<Term>,
    {
        Self {
            telescope: Telescope::build(fields, ()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

impl Tuple {
    pub fn unit() -> Self {
        Self { fields: vec![] }
    }

    pub fn new<I, T>(fields: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<Term>,
    {
        Self {
            fields: fields.into_iter().map(|t| t.into()).collect::<Vec<_>>(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

impl Proj {
    pub fn new<H: Into<Term>>(head: H, index: usize) -> Self {
        Self {
            head: head.into(),
            index,
        }
    }
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

impl NatMatch {
    pub fn induction<H, M, ZC, PL, IL, SC>(
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

        Self::Induction {
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
        }
    }

    pub fn dispatch<H, M, I, B, D>(
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
        Self::Dispatch {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: cases.into_iter().map(|(n, b)| (n, b.into())).collect(),
            default: default.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlnMatch {
    pub head: Term,
    pub motive: Scope<One>,
    pub false_case: Term,
    pub true_case: Term,
}

impl BlnMatch {
    pub fn new<H, M, F, T>(
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
        Self {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            false_case: false_case.into(),
            true_case: true_case.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

impl AtomType {
    pub fn new<I, A>(atoms: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Atom>,
    {
        Self {
            atoms: atoms.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub head: Term,
    pub motive: Scope<One>,
    pub cases: BTreeMap<Atom, Term>,
}

impl Match {
    pub fn new<H, M, I, A, B>(head: H, motive_label: Option<&str>, motive: M, cases: I) -> Self
    where
        H: Into<Term>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        Self {
            head: head.into(),
            motive: match motive_label {
                Some(l) => Scope::close(One, &[l], motive.into()),
                None => Scope::constant(One, motive.into()),
            },
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom.into(), body.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub type_: Term,
    pub body: Term,
    pub tail: Scope<One>,
}

impl Let {
    pub fn new<L, T, B, U>(label: L, type_: T, body: B, tail: U) -> Self
    where
        L: Into<String>,
        T: Into<Term>,
        B: Into<Term>,
        U: Into<Term>,
    {
        let label = label.into();

        Self {
            type_: type_.into(),
            body: body.into(),
            tail: Scope::close(One, &[label.as_str()], tail.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rec {
    pub items: Vec<(Scope<Many>, Scope<Many>)>,
    pub tail: Scope<Many>,
}

impl Rec {
    pub fn new<I, L, T, U, V>(items: I, tail: V) -> Self
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

        Self {
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
        }
    }
}

#[derive(Debug, Clone)]
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
    Spanned(Span, Term),
}

impl PartialEq for Subterm {
    fn eq(&self, other: &Self) -> bool {
        let mut this = self;
        let mut that = other;

        loop {
            match (this, that) {
                (Subterm::Spanned(_, inner), _) => this = inner,
                (_, Subterm::Spanned(_, inner)) => that = inner,
                (Subterm::Type, Subterm::Type) => break true,
                (Subterm::Prim(a), Subterm::Prim(b)) => break a == b,
                (Subterm::BlnMatch(a), Subterm::BlnMatch(b)) => break a == b,
                (Subterm::NatMatch(a), Subterm::NatMatch(b)) => break a == b,
                (Subterm::FuncType(a), Subterm::FuncType(b)) => break a == b,
                (Subterm::Func(a), Subterm::Func(b)) => break a == b,
                (Subterm::Apply(a), Subterm::Apply(b)) => break a == b,
                (Subterm::TupleType(a), Subterm::TupleType(b)) => break a == b,
                (Subterm::Tuple(a), Subterm::Tuple(b)) => break a == b,
                (Subterm::Proj(a), Subterm::Proj(b)) => break a == b,
                (Subterm::AtomType(a), Subterm::AtomType(b)) => break a == b,
                (Subterm::Atom(a), Subterm::Atom(b)) => break a == b,
                (Subterm::Match(a), Subterm::Match(b)) => break a == b,
                (Subterm::Let(a), Subterm::Let(b)) => break a == b,
                (Subterm::Rec(a), Subterm::Rec(b)) => break a == b,
                (Subterm::Var(a), Subterm::Var(b)) => break a == b,
                _ => break false,
            }
        }
    }
}

impl Eq for Subterm {}

impl Hash for Subterm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut term = self;

        loop {
            match term {
                Subterm::Type => break,
                Subterm::Prim(x) => break x.hash(state),
                Subterm::BlnMatch(x) => break x.hash(state),
                Subterm::NatMatch(x) => break x.hash(state),
                Subterm::FuncType(x) => break x.hash(state),
                Subterm::Func(x) => break x.hash(state),
                Subterm::Apply(x) => break x.hash(state),
                Subterm::TupleType(x) => break x.hash(state),
                Subterm::Tuple(x) => break x.hash(state),
                Subterm::Proj(x) => break x.hash(state),
                Subterm::AtomType(x) => break x.hash(state),
                Subterm::Atom(x) => break x.hash(state),
                Subterm::Match(x) => break x.hash(state),
                Subterm::Let(x) => break x.hash(state),
                Subterm::Rec(x) => break x.hash(state),
                Subterm::Var(x) => break x.hash(state),
                Subterm::Spanned(_, inner) => term = inner,
            }
        }
    }
}

impl Subterm {
    pub fn as_nat(&self) -> Option<u32> {
        match self {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => Some(0),
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => match inner.as_ref() {
                Subterm::Prim(Prim::Nat(Nat::Zero)) => Some(*spine),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn free_vars(&self) -> BTreeSet<String> {
        Bound::free_vars(self)
    }
}

impl From<Type> for Subterm {
    fn from(Type: Type) -> Self {
        Self::Type
    }
}

impl From<Prim> for Subterm {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl From<FuncType> for Subterm {
    fn from(value: FuncType) -> Self {
        Self::FuncType(value)
    }
}

impl From<Func> for Subterm {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Apply> for Subterm {
    fn from(value: Apply) -> Self {
        Self::Apply(value)
    }
}

impl From<TupleType> for Subterm {
    fn from(value: TupleType) -> Self {
        Self::TupleType(value)
    }
}

impl From<Tuple> for Subterm {
    fn from(value: Tuple) -> Self {
        Self::Tuple(value)
    }
}

impl From<BlnMatch> for Subterm {
    fn from(value: BlnMatch) -> Self {
        Self::BlnMatch(value)
    }
}

impl From<NatMatch> for Subterm {
    fn from(value: NatMatch) -> Self {
        Self::NatMatch(value)
    }
}

impl From<Proj> for Subterm {
    fn from(value: Proj) -> Self {
        Self::Proj(value)
    }
}

impl From<AtomType> for Subterm {
    fn from(value: AtomType) -> Self {
        Self::AtomType(value)
    }
}

impl From<Atom> for Subterm {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

impl From<Match> for Subterm {
    fn from(value: Match) -> Self {
        Self::Match(value)
    }
}

impl From<Let> for Subterm {
    fn from(value: Let) -> Self {
        Self::Let(value)
    }
}

impl From<Rec> for Subterm {
    fn from(value: Rec) -> Self {
        Self::Rec(value)
    }
}

impl From<Var> for Subterm {
    fn from(value: Var) -> Self {
        Self::Var(value)
    }
}

impl From<Type> for Term {
    fn from(_: Type) -> Self {
        Self::new(Subterm::Type)
    }
}

impl From<Prim> for Term {
    fn from(value: Prim) -> Self {
        Self::new(Subterm::Prim(value))
    }
}

impl From<FuncType> for Term {
    fn from(value: FuncType) -> Self {
        Self::new(Subterm::FuncType(value))
    }
}

impl From<Func> for Term {
    fn from(value: Func) -> Self {
        Self::new(Subterm::Func(value))
    }
}

impl From<Apply> for Term {
    fn from(value: Apply) -> Self {
        Self::new(Subterm::Apply(value))
    }
}

impl From<TupleType> for Term {
    fn from(value: TupleType) -> Self {
        Self::new(Subterm::TupleType(value))
    }
}

impl From<Tuple> for Term {
    fn from(value: Tuple) -> Self {
        Self::new(Subterm::Tuple(value))
    }
}

impl From<BlnMatch> for Term {
    fn from(value: BlnMatch) -> Self {
        Self::new(Subterm::BlnMatch(value))
    }
}

impl From<NatMatch> for Term {
    fn from(value: NatMatch) -> Self {
        Self::new(Subterm::NatMatch(value))
    }
}

impl From<Proj> for Term {
    fn from(value: Proj) -> Self {
        Self::new(Subterm::Proj(value))
    }
}

impl From<AtomType> for Term {
    fn from(value: AtomType) -> Self {
        Self::new(Subterm::AtomType(value))
    }
}

impl From<Atom> for Term {
    fn from(value: Atom) -> Self {
        Self::new(Subterm::Atom(value))
    }
}

impl From<Match> for Term {
    fn from(value: Match) -> Self {
        Self::new(Subterm::Match(value))
    }
}

impl From<Let> for Term {
    fn from(value: Let) -> Self {
        Self::new(Subterm::Let(value))
    }
}

impl From<Rec> for Term {
    fn from(value: Rec) -> Self {
        Self::new(Subterm::Rec(value))
    }
}

impl From<Var> for Term {
    fn from(value: Var) -> Self {
        Self::new(Subterm::Var(value))
    }
}

pub trait Bound: Sized + Clone + Eq + Hash + Debug {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>;

    /// Number of outer de Bruijn binders this term depends on: `1 + max escaping bound
    /// index`, or `0` if none. A term with `reach <= depth` contains no bound index
    /// `>= depth`, so `shift`/`release` at that depth are the identity on it.
    fn reach(&self) -> usize;

    /// `true` iff the term has no loose de Bruijn indices — i.e. it's not floating
    /// inside some outer scope. Reducing a non-closed term doesn't make sense; this
    /// is also the gate for memoising reductions.
    fn closed(&self) -> bool {
        self.reach() == 0
    }

    fn shift(&self, amount: usize) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound()
                .filter(|&index| index >= depth)
                .map(|index| Var::bound(index + amount).into())
        }))
    }

    fn capture(&self, labels: &[&str]) -> Self {
        self.traverse(&mut Visit::new(|depth, var| {
            var.as_free()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate| label == candidate)
                        .map(|index| Var::bound(depth + index).into())
                })
                .or_else(|| {
                    var.as_bound()
                        .filter(|&index| index >= depth)
                        .map(|index| Var::bound(index + labels.len()).into())
                })
        }))
    }

    fn release(&self, terms: &[&Term]) -> Self {
        self.traverse(&mut Visit::pruning(|depth, var| {
            var.as_bound().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => (**terms[delta]).shift(depth),
                        false => Var::bound(index - terms.len()).into(),
                    })
            })
        }))
    }

    fn free_vars(&self) -> BTreeSet<String> {
        let mut vars = BTreeSet::new();
        self.traverse(&mut Visit::new(|_, var| {
            if let Some(label) = var.as_free() {
                vars.insert(label.to_string());
            }
            None
        }));
        vars
    }
}

impl Bound for () {
    fn traverse<F>(&self, _: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
    }

    fn reach(&self) -> usize {
        0
    }
}

impl Bound for Term {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        if visit.prune && self.reach() <= visit.depth {
            return self.clone();
        }

        Term::new((**self).traverse(visit))
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
            Subterm::Type => Type.into(),
            Subterm::Prim(prim) => visit.visit_prim(prim).into(),
            Subterm::BlnMatch(bm) => visit.visit_bln_match(bm).into(),
            Subterm::NatMatch(nm) => visit.visit_nat_match(nm).into(),
            Subterm::FuncType(ft) => visit.visit_func_type(ft).into(),
            Subterm::Func(func) => visit.visit_func(func).into(),
            Subterm::Apply(apply) => visit.visit_apply(apply).into(),
            Subterm::TupleType(tt) => visit.visit_tuple_type(tt).into(),
            Subterm::Tuple(t) => visit.visit_tuple(t).into(),
            Subterm::Proj(proj) => visit.visit_proj(proj).into(),
            Subterm::AtomType(at) => at.clone().into(),
            Subterm::Atom(atom) => atom.clone().into(),
            Subterm::Match(m) => visit.visit_match(m).into(),
            Subterm::Let(let_) => visit.visit_let(let_).into(),
            Subterm::Rec(rec) => visit.visit_rec(rec).into(),
            Subterm::Var(var) => {
                (visit.visit)(visit.depth, var).unwrap_or_else(|| var.clone().into())
            }
            Subterm::Spanned(span, inner) => Subterm::Spanned(*span, visit.visit_subterm(inner)),
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
            Subterm::Spanned(_, inner) => inner.reach(),
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
        | Prim::ArrLen(t)
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
        | Prim::ArrGet(a, b)
        | Prim::ArrAppend(a, b) => a.reach().max(b.reach()),

        Prim::BinSlice(a, b, c) | Prim::ArrSlice(a, b, c) => {
            a.reach().max(b.reach()).max(c.reach())
        }

        Prim::BinConcat(terms) | Prim::ArrConcat(terms) | Prim::Arr(terms) => max_reach(terms),
    }
}

#[derive(Debug)]
pub struct Visit<F> {
    depth: usize,
    prune: bool,
    visit: F,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Var) -> Option<Subterm>,
{
    fn new(visit: F) -> Self {
        Self {
            depth: 0,
            prune: false,
            visit,
        }
    }

    /// Like `new`, but lets `Term::traverse` skip (and structurally share) subtrees that
    /// the visit provably leaves unchanged. Only sound for index-monotonic visits whose
    /// effect on a subterm depends solely on bound indices `>= depth` — i.e. `shift` and
    /// `release`. Must NOT be used for `capture` (rewrites free names) or `free_vars`
    /// (must observe every node).
    fn pruning(visit: F) -> Self {
        Self {
            depth: 0,
            prune: true,
            visit,
        }
    }

    fn visit_subterm(&mut self, subterm: &Term) -> Term {
        subterm.traverse(self)
    }

    fn visit_prim(&mut self, prim: &Prim) -> Prim {
        match prim {
            Prim::BlnType => Prim::BlnType,
            Prim::Bln(value) => Prim::Bln(*value),
            Prim::NatType => Prim::NatType,
            Prim::Nat(Nat::Zero) => Prim::Nat(Nat::Zero),
            Prim::Nat(Nat::Succ(spine, inner)) => {
                Prim::Nat(Nat::Succ(*spine, self.visit_subterm(inner)))
            }
            Prim::NatEql(left, right) => {
                Prim::NatEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatNeq(left, right) => {
                Prim::NatNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatAdd(left, right) => {
                Prim::NatAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatSub(left, right) => {
                Prim::NatSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatMul(left, right) => {
                Prim::NatMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatLt(left, right) => {
                Prim::NatLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatDiv(left, right) => {
                Prim::NatDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatRem(left, right) => {
                Prim::NatRem(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatGt(left, right) => {
                Prim::NatGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatLte(left, right) => {
                Prim::NatLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::NatGte(left, right) => {
                Prim::NatGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntType => Prim::IntType,
            Prim::Int(value) => Prim::Int(*value),
            Prim::IntEql(left, right) => {
                Prim::IntEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntNeq(left, right) => {
                Prim::IntNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntAdd(left, right) => {
                Prim::IntAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntSub(left, right) => {
                Prim::IntSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntMul(left, right) => {
                Prim::IntMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntDiv(left, right) => {
                Prim::IntDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntRem(left, right) => {
                Prim::IntRem(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntLt(left, right) => {
                Prim::IntLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntGt(left, right) => {
                Prim::IntGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntLte(left, right) => {
                Prim::IntLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::IntGte(left, right) => {
                Prim::IntGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltType => Prim::FltType,
            Prim::Flt(flt) => Prim::Flt(*flt),
            Prim::FltAdd(left, right) => {
                Prim::FltAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltSub(left, right) => {
                Prim::FltSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMul(left, right) => {
                Prim::FltMul(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltDiv(left, right) => {
                Prim::FltDiv(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltEql(left, right) => {
                Prim::FltEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltNeq(left, right) => {
                Prim::FltNeq(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltLt(left, right) => {
                Prim::FltLt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltGt(left, right) => {
                Prim::FltGt(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltLte(left, right) => {
                Prim::FltLte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltGte(left, right) => {
                Prim::FltGte(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMin(left, right) => {
                Prim::FltMin(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMax(left, right) => {
                Prim::FltMax(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltNeg(inner) => Prim::FltNeg(self.visit_subterm(inner)),
            Prim::FltAbs(inner) => Prim::FltAbs(self.visit_subterm(inner)),
            Prim::FltSqrt(inner) => Prim::FltSqrt(self.visit_subterm(inner)),
            Prim::FltFloor(inner) => Prim::FltFloor(self.visit_subterm(inner)),
            Prim::FltCeil(inner) => Prim::FltCeil(self.visit_subterm(inner)),
            Prim::FltTrunc(inner) => Prim::FltTrunc(self.visit_subterm(inner)),
            Prim::FltNearest(inner) => Prim::FltNearest(self.visit_subterm(inner)),
            Prim::NatToStr(inner) => Prim::NatToStr(self.visit_subterm(inner)),
            Prim::IntToStr(inner) => Prim::IntToStr(self.visit_subterm(inner)),
            Prim::FltToStr(inner) => Prim::FltToStr(self.visit_subterm(inner)),
            Prim::NatToInt(inner) => Prim::NatToInt(self.visit_subterm(inner)),
            Prim::NatToFlt(inner) => Prim::NatToFlt(self.visit_subterm(inner)),
            Prim::IntToNat(inner) => Prim::IntToNat(self.visit_subterm(inner)),
            Prim::IntToFlt(inner) => Prim::IntToFlt(self.visit_subterm(inner)),
            Prim::FltToNat(inner) => Prim::FltToNat(self.visit_subterm(inner)),
            Prim::FltToInt(inner) => Prim::FltToInt(self.visit_subterm(inner)),
            Prim::BinType => Prim::BinType,
            Prim::Bin(bytes) => Prim::Bin(bytes.clone()),
            Prim::BinLen(bin) => Prim::BinLen(self.visit_subterm(bin)),
            Prim::BinEql(left, right) => {
                Prim::BinEql(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::BinGet(bin, index) => {
                Prim::BinGet(self.visit_subterm(bin), self.visit_subterm(index))
            }
            Prim::BinSlice(bin, start, end) => Prim::BinSlice(
                self.visit_subterm(bin),
                self.visit_subterm(start),
                self.visit_subterm(end),
            ),
            Prim::BinAppend(bin, byte) => {
                Prim::BinAppend(self.visit_subterm(bin), self.visit_subterm(byte))
            }
            Prim::BinConcat(operands) => {
                Prim::BinConcat(operands.iter().map(|e| self.visit_subterm(e)).collect())
            }
            Prim::ArrType(elem) => Prim::ArrType(self.visit_subterm(elem)),
            Prim::Arr(elems) => Prim::Arr(elems.iter().map(|e| self.visit_subterm(e)).collect()),
            Prim::ArrLen(list) => Prim::ArrLen(self.visit_subterm(list)),
            Prim::ArrGet(list, index) => {
                Prim::ArrGet(self.visit_subterm(list), self.visit_subterm(index))
            }
            Prim::ArrSlice(list, start, end) => Prim::ArrSlice(
                self.visit_subterm(list),
                self.visit_subterm(start),
                self.visit_subterm(end),
            ),
            Prim::ArrAppend(list, elem) => {
                Prim::ArrAppend(self.visit_subterm(list), self.visit_subterm(elem))
            }
            Prim::ArrConcat(operands) => {
                Prim::ArrConcat(operands.iter().map(|e| self.visit_subterm(e)).collect())
            }
            Prim::IoPrint(inner) => Prim::IoPrint(self.visit_subterm(inner)),
            Prim::IoRead => Prim::IoRead,
        }
    }

    fn visit_scope<A: Arity, B: Bound>(&mut self, scope: &Scope<A, B>) -> Scope<A, B> {
        self.depth += scope.arity.arity();
        let body = scope.body.traverse(self).into();
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            names: scope.names.clone(),
            body,
        }
    }

    fn visit_func_type(&mut self, ft: &FuncType) -> FuncType {
        FuncType {
            telescope: ft.telescope.traverse(self),
        }
    }

    fn visit_func(&mut self, func: &Func) -> Func {
        Func {
            body: self.visit_scope(&func.body),
        }
    }

    fn visit_apply(&mut self, apply: &Apply) -> Apply {
        Apply {
            head: self.visit_subterm(&apply.head),
            params: apply.params.iter().map(|p| self.visit_subterm(p)).collect(),
        }
    }

    fn visit_tuple_type(&mut self, tt: &TupleType) -> TupleType {
        TupleType {
            telescope: tt.telescope.traverse(self),
        }
    }

    fn visit_tuple(&mut self, t: &Tuple) -> Tuple {
        Tuple {
            fields: t
                .fields
                .iter()
                .map(|f| self.visit_subterm(f))
                .collect::<Vec<_>>(),
        }
    }

    fn visit_bln_match(&mut self, bm: &BlnMatch) -> BlnMatch {
        BlnMatch {
            head: self.visit_subterm(&bm.head),
            motive: self.visit_scope(&bm.motive),
            false_case: self.visit_subterm(&bm.false_case),
            true_case: self.visit_subterm(&bm.true_case),
        }
    }

    fn visit_nat_match(&mut self, nm: &NatMatch) -> NatMatch {
        match nm {
            NatMatch::Induction {
                head,
                motive,
                zero_case,
                succ_case,
            } => NatMatch::Induction {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                zero_case: self.visit_subterm(zero_case),
                succ_case: self.visit_scope(succ_case),
            },
            NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            } => NatMatch::Dispatch {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                cases: cases
                    .iter()
                    .map(|(&n, body)| (n, self.visit_subterm(body)))
                    .collect(),
                default: self.visit_subterm(default),
            },
        }
    }

    fn visit_proj(&mut self, proj: &Proj) -> Proj {
        Proj {
            head: self.visit_subterm(&proj.head),
            index: proj.index,
        }
    }

    fn visit_match(&mut self, m: &Match) -> Match {
        Match {
            head: self.visit_subterm(&m.head),
            motive: self.visit_scope(&m.motive),
            cases: m
                .cases
                .iter()
                .map(|(atom, body)| (atom.clone(), self.visit_subterm(body)))
                .collect(),
        }
    }

    fn visit_let(&mut self, let_: &Let) -> Let {
        Let {
            type_: self.visit_subterm(&let_.type_),
            body: self.visit_subterm(&let_.body),
            tail: self.visit_scope(&let_.tail),
        }
    }

    fn visit_rec(&mut self, rec: &Rec) -> Rec {
        Rec {
            items: rec
                .items
                .iter()
                .map(|(type_, value)| (self.visit_scope(type_), self.visit_scope(value)))
                .collect(),
            tail: self.visit_scope(&rec.tail),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_open_substitutes_label_name() {
        let term = Scope::close(One, &["x"], Subterm::from(Var::free("x")))
            .open(&[&Var::free("y").into()]);

        let Subterm::Var(var) = term else {
            panic!("unexpected `{term:?}`")
        };

        assert_eq!(var, Var::free("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term = Scope::close(One, &["x"], Subterm::from(Func::new(["y"], Var::free("x"))))
            .open(&[&Var::free("z").into()]);

        let Subterm::Func(body) = term else {
            panic!("unexpected `{term:?}`")
        };

        let Subterm::Var(var) = Term::unwrap_or_clone(body.body.open(&[&Var::free("w").into()]))
        else {
            panic!("unexpected term")
        };

        assert_eq!(var, Var::free("z"));
    }

    #[test]
    fn collect_ignores_index_names() {
        let term = Subterm::from(Func::new(
            ["x"],
            Tuple::new([
                Subterm::from(Var::free("x")),
                Rec::new(
                    vec![("y", Type, Var::free("z"))],
                    Tuple::new([Var::free("y"), Var::free("w")]),
                )
                .into(),
            ]),
        ));

        assert_eq!(
            term.free_vars(),
            BTreeSet::from([String::from("w"), String::from("z")])
        );
    }

    #[test]
    fn reach_basic_values() {
        assert_eq!(Term::from(Type).reach(), 0);
        assert_eq!(Term::from(Var::free("x")).reach(), 0);
        assert_eq!(Term::new(Subterm::Var(Var::bound(0))).reach(), 1);
        assert_eq!(Term::new(Subterm::Var(Var::bound(3))).reach(), 4);
        // closed identity function λx.x
        assert_eq!(Term::from(Func::new(["x"], Var::free("x"))).reach(), 0);
    }

    #[test]
    fn reach_scope_absorbs_arity() {
        // body references bound index 2 (reach 3); a scope absorbs its arity
        let f1 = Term::from(Func {
            body: Scope::constant(Many(1), Term::new(Subterm::Var(Var::bound(2)))),
        });
        assert_eq!(f1.reach(), 2); // (2 + 1) - 1

        let f2 = Term::from(Func {
            body: Scope::constant(Many(2), Term::new(Subterm::Var(Var::bound(2)))),
        });
        assert_eq!(f2.reach(), 1); // (2 + 1) - 2
    }

    #[test]
    fn open_shares_closed_body_without_rebuild() {
        // body does not mention the bound variable -> open returns the stored Rc unchanged
        let scope = Scope::close(One, &["x"], Term::from(Atom::from("k")));
        let opened = scope.open(&[&Var::free("y").into()]);
        assert!(Rc::ptr_eq(&opened.inner, &scope.body.inner));
    }

    #[test]
    fn open_shares_closed_subterm_inside_substituted_body() {
        let closed: Term = Func::new(["a"], Var::free("a")).into(); // λa.a, closed
        let scope = Scope::close(
            One,
            &["x"],
            Term::from(Tuple::new([Term::from(Var::free("x")), closed])),
        );

        let stored_field = match &**scope.body {
            Subterm::Tuple(Tuple { fields }) => fields[1].clone(),
            _ => panic!("expected tuple body"),
        };

        let opened = scope.open(&[&Var::free("y").into()]);

        let opened_field = match &*opened {
            Subterm::Tuple(Tuple { fields }) => fields[1].clone(),
            _ => panic!("expected tuple result"),
        };

        // the substituted field changed; the closed field is shared, not rebuilt
        assert_eq!(opened_field, stored_field);
        assert!(Rc::ptr_eq(&opened_field.inner, &stored_field.inner));
    }
}
