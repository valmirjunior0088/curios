use {
    super::{Arity, FltType, IntType, Many, One, Prim, Two},
    crate::macros::name,
    std::collections::{BTreeMap, BTreeSet},
};

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NameType {
    Label(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    type_: NameType,
}

impl Name {
    pub fn label<A>(label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            type_: NameType::Label(label.into()),
        }
    }

    fn as_label(&self) -> Option<&str> {
        match &self.type_ {
            NameType::Label(label) => Some(label),
            NameType::Index(_) => None,
        }
    }

    fn index(index: usize) -> Self {
        Self {
            type_: NameType::Index(index),
        }
    }

    fn as_index(&self) -> Option<usize> {
        match &self.type_ {
            NameType::Label(_) => None,
            &NameType::Index(index) => Some(index),
        }
    }

    pub fn unwrap(&self) -> &str {
        self.as_label().unwrap()
    }
}

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope<A: Arity> {
    arity: A,
    body: Subterm,
}

impl<A> Scope<A>
where
    A: Arity,
{
    pub fn close<'a, B>(arity: A, labels: A::Params<'a, str>, body: B) -> Self
    where
        B: Into<Term>,
    {
        assert!(
            arity.arity() == labels.as_ref().len(),
            "scope arity mismatch in `close`: expected {}, got {}",
            arity.arity(),
            labels.as_ref().len()
        );

        Self {
            arity,
            body: body.into().capture(labels.as_ref()).into(),
        }
    }

    pub fn arity(&self) -> usize {
        self.arity.arity()
    }

    pub fn open<'a>(&self, terms: A::Params<'a, Term>) -> Term {
        assert!(
            self.arity() == terms.as_ref().len(),
            "scope arity mismatch in `open`: expected {}, got {}",
            self.arity(),
            terms.as_ref().len()
        );

        self.body.release(terms.as_ref())
    }

    pub fn collect(&self) -> BTreeSet<String> {
        self.body.collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub input: Subterm,
    pub output: Scope<One>,
}

impl FuncType {
    pub fn new<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Func {
    pub body: Scope<One>,
}

impl Func {
    pub fn new<L, B>(label: L, body: B) -> Self
    where
        L: Into<String>,
        B: Into<Term>,
    {
        let label = label.into();

        Self {
            body: Scope::close(One, &[label.as_str()], body),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Apply {
    pub head: Subterm,
    pub param: Subterm,
}

impl Apply {
    pub fn new<H, P>(head: H, param: P) -> Self
    where
        H: Into<Term>,
        P: Into<Term>,
    {
        Self {
            head: head.into().into(),
            param: param.into().into(),
        }
    }

    pub fn many<H, I, P>(head: H, params: I) -> Term
    where
        H: Into<Term>,
        I: IntoIterator<Item = P>,
        P: Into<Term>,
    {
        params
            .into_iter()
            .fold(head.into(), |head, param| Self::new(head, param).into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PairType {
    pub input: Subterm,
    pub output: Scope<One>,
}

impl PairType {
    pub fn new<L, I, O>(label: L, input: I, output: O) -> Self
    where
        L: Into<String>,
        I: Into<Term>,
        O: Into<Term>,
    {
        let label = label.into();

        Self {
            input: input.into().into(),
            output: Scope::close(One, &[label.as_str()], output),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pair {
    pub fst: Subterm,
    pub snd: Subterm,
}

impl Pair {
    pub fn new<F, S>(fst: F, snd: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self {
            fst: fst.into().into(),
            snd: snd.into().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Split {
    pub head: Subterm,
    pub motive: Scope<One>,
    pub tail: Scope<Two>,
}

impl Split {
    pub fn new<H, ML, M, FL, SL, T>(
        head: H,
        motive_label: ML,
        motive: M,
        fst_label: FL,
        snd_label: SL,
        tail: T,
    ) -> Self
    where
        H: Into<Term>,
        ML: Into<String>,
        M: Into<Term>,
        FL: Into<String>,
        SL: Into<String>,
        T: Into<Term>,
    {
        let motive_label = motive_label.into();
        let fst_label = fst_label.into();
        let snd_label = snd_label.into();

        Self {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive),
            tail: Scope::close(Two, &[fst_label.as_str(), snd_label.as_str()], tail),
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
    pub head: Subterm,
    pub motive: Scope<One>,
    pub cases: BTreeMap<Atom, Subterm>,
}

impl Match {
    pub fn new<H, L, M, I, A, B>(head: H, motive_label: L, motive: M, cases: I) -> Self
    where
        H: Into<Term>,
        L: Into<String>,
        M: Into<Term>,
        I: IntoIterator<Item = (A, B)>,
        A: Into<Atom>,
        B: Into<Term>,
    {
        let motive_label = motive_label.into();

        Self {
            head: head.into().into(),
            motive: Scope::close(One, &[motive_label.as_str()], motive),
            cases: cases
                .into_iter()
                .map(|(atom, body)| (atom.into(), body.into().into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub type_: Subterm,
    pub body: Subterm,
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
            type_: type_.into().into(),
            body: body.into().into(),
            tail: Scope::close(One, &[label.as_str()], tail),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetRec {
    pub items: Vec<(Scope<Many>, Scope<Many>)>,
    pub tail: Scope<Many>,
}

impl LetRec {
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
            tail: Scope::close(Many(labels.len()), &labels, tail),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Type,
    Prim(Prim),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    PairType(PairType),
    Pair(Pair),
    Split(Split),
    AtomType(AtomType),
    Atom(Atom),
    Match(Match),
    Let(Let),
    LetRec(LetRec),
    Name(Name),
}

impl Term {
    pub fn collect(&self) -> BTreeSet<String> {
        let mut names = BTreeSet::new();

        Visit::new(|_, name| {
            if let Some(label) = name.as_label() {
                names.insert(label.to_string());
            }

            None
        })
        .visit_term(self);

        names
    }

    fn shift(&self, amount: usize) -> Self {
        Visit::new(|depth, name| {
            name.as_index()
                .filter(|&index| index >= depth)
                .map(|index| Name::index(index + amount).into())
        })
        .visit_term(self)
    }

    fn capture(&self, labels: &[&str]) -> Self {
        Visit::new(|depth, name| {
            name.as_label()
                .and_then(|label| {
                    labels
                        .iter()
                        .position(|&candidate| label == candidate)
                        .map(|index| Name::index(depth + index).into())
                })
                .or_else(|| {
                    name.as_index()
                        .filter(|&index| index >= depth)
                        .map(|index| Name::index(index + labels.len()).into())
                })
        })
        .visit_term(self)
    }

    fn release(&self, terms: &[&Term]) -> Self {
        Visit::new(|depth, name| {
            name.as_index().and_then(|index| {
                index
                    .checked_sub(depth)
                    .map(|delta| match delta < terms.len() {
                        true => terms[delta].shift(depth),
                        false => Name::index(index - terms.len()).into(),
                    })
            })
        })
        .visit_term(self)
    }
}

impl From<Type> for Term {
    fn from(Type: Type) -> Self {
        Self::Type
    }
}

impl From<Prim> for Term {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl From<IntType> for Term {
    fn from(IntType: IntType) -> Self {
        Self::Prim(Prim::IntType)
    }
}

impl From<i32> for Term {
    fn from(value: i32) -> Self {
        Self::Prim(value.into())
    }
}

impl From<FltType> for Term {
    fn from(FltType: FltType) -> Self {
        Self::Prim(Prim::FltType)
    }
}

impl From<f32> for Term {
    fn from(value: f32) -> Self {
        Self::Prim(value.into())
    }
}

impl From<FuncType> for Term {
    fn from(value: FuncType) -> Self {
        Self::FuncType(value)
    }
}

impl From<Func> for Term {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Apply> for Term {
    fn from(value: Apply) -> Self {
        Self::Apply(value)
    }
}

impl From<PairType> for Term {
    fn from(value: PairType) -> Self {
        Self::PairType(value)
    }
}

impl From<Pair> for Term {
    fn from(value: Pair) -> Self {
        Self::Pair(value)
    }
}

impl From<Split> for Term {
    fn from(value: Split) -> Self {
        Self::Split(value)
    }
}

impl From<AtomType> for Term {
    fn from(value: AtomType) -> Self {
        Self::AtomType(value)
    }
}

impl From<Atom> for Term {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

impl From<Match> for Term {
    fn from(value: Match) -> Self {
        Self::Match(value)
    }
}

impl From<Let> for Term {
    fn from(value: Let) -> Self {
        Self::Let(value)
    }
}

impl From<LetRec> for Term {
    fn from(value: LetRec) -> Self {
        Self::LetRec(value)
    }
}

impl From<Name> for Term {
    fn from(value: Name) -> Self {
        Self::Name(value)
    }
}

#[derive(Debug)]
struct Visit<F> {
    depth: usize,
    visit: F,
}

impl<F> Visit<F>
where
    F: FnMut(usize, &Name) -> Option<Term>,
{
    fn new(visit: F) -> Self {
        Self { depth: 0, visit }
    }

    fn visit_subterm(&mut self, subterm: &Subterm) -> Subterm {
        self.visit_term(subterm).into()
    }

    fn visit_prim(&mut self, prim: &Prim) -> Prim {
        match prim {
            Prim::IntType => Prim::IntType,
            Prim::Int(value) => Prim::Int(*value),
            Prim::IntEql(left, right) => {
                Prim::IntEql(self.visit_subterm(left), self.visit_subterm(right))
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
            Prim::FltType => Prim::FltType,
            Prim::Flt(bits) => Prim::Flt(*bits),
            Prim::FltAdd(left, right) => {
                Prim::FltAdd(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltSub(left, right) => {
                Prim::FltSub(self.visit_subterm(left), self.visit_subterm(right))
            }
            Prim::FltMul(left, right) => {
                Prim::FltMul(self.visit_subterm(left), self.visit_subterm(right))
            }
        }
    }

    fn visit_scope<A: Arity>(&mut self, scope: &Scope<A>) -> Scope<A> {
        self.depth += scope.arity.arity();
        let body = self.visit_subterm(&scope.body);
        self.depth -= scope.arity.arity();

        Scope {
            arity: scope.arity,
            body,
        }
    }

    fn visit_term(&mut self, term: &Term) -> Term {
        match term {
            Term::Type => Type.into(),
            Term::Prim(prim) => self.visit_prim(prim).into(),
            Term::FuncType(FuncType { input, output }) => FuncType {
                input: self.visit_subterm(input),
                output: self.visit_scope(output),
            }
            .into(),
            Term::Func(Func { body }) => Func {
                body: self.visit_scope(body),
            }
            .into(),
            Term::Apply(Apply { head, param }) => Apply {
                head: self.visit_subterm(head),
                param: self.visit_subterm(param),
            }
            .into(),
            Term::PairType(PairType { input, output }) => PairType {
                input: self.visit_subterm(input),
                output: self.visit_scope(output),
            }
            .into(),
            Term::Pair(Pair { fst, snd }) => Pair {
                fst: self.visit_subterm(fst),
                snd: self.visit_subterm(snd),
            }
            .into(),
            Term::Split(Split { head, motive, tail }) => Split {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                tail: self.visit_scope(tail),
            }
            .into(),
            Term::AtomType(AtomType { atoms }) => AtomType {
                atoms: atoms.clone(),
            }
            .into(),
            Term::Atom(atom) => atom.clone().into(),
            Term::Match(Match {
                head,
                motive,
                cases,
            }) => Match {
                head: self.visit_subterm(head),
                motive: self.visit_scope(motive),
                cases: cases
                    .iter()
                    .map(|(atom, body)| (atom.clone(), self.visit_subterm(body)))
                    .collect(),
            }
            .into(),
            Term::Let(Let { type_, body, tail }) => Let {
                type_: self.visit_subterm(type_),
                body: self.visit_subterm(body),
                tail: self.visit_scope(tail),
            }
            .into(),
            Term::LetRec(LetRec { items, tail }) => LetRec {
                items: items
                    .iter()
                    .map(|(type_, value)| (self.visit_scope(type_), self.visit_scope(value)))
                    .collect(),
                tail: self.visit_scope(tail),
            }
            .into(),
            Term::Name(name) => {
                (self.visit)(self.depth, name).unwrap_or_else(|| name.clone().into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_open_substitutes_label_name() {
        let term = Scope::close(One, &["x"], Name::label("x")).open(&[&Name::label("y").into()]);

        let name = match term {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::label("y"));
    }

    #[test]
    fn close_open_preserves_nested_bind() {
        let term = Scope::close(One, &["x"], Func::new("y", Name::label("x")))
            .open(&[&Name::label("z").into()]);

        let body = match term {
            Term::Func(body) => body.body,
            term => panic!("unexpected `{term:?}`"),
        };

        let name = match body.open(&[&Name::label("w").into()]) {
            Term::Name(name) => name,
            term => panic!("unexpected `{term:?}`"),
        };

        assert_eq!(name, Name::label("z"));
    }

    #[test]
    fn collect_ignores_index_names() {
        let term = Term::from(Func::new(
            "x",
            Pair::new(
                Name::label("x"),
                LetRec::new(
                    vec![("y", Type, Name::label("z"))],
                    Pair::new(Name::label("y"), Name::label("w")),
                ),
            ),
        ));

        assert_eq!(
            term.collect(),
            BTreeSet::from([String::from("w"), String::from("z")])
        );
    }
}
