use super::{Name, Prim};

pub type Subterm = Box<Term>;

#[derive(Debug)]
pub struct Elim {
    pub head: Subterm,
    pub zero_case: Subterm,
    pub pred: String,
    pub succ_case: Subterm,
}

#[derive(Debug)]
pub struct Func {
    pub captures: Vec<String>,
    pub param: String,
    pub body: Subterm,
}

#[derive(Debug)]
pub struct Apply {
    pub head: Subterm,
    pub param: Subterm,
}

#[derive(Debug)]
pub struct Pair {
    pub fst: Subterm,
    pub snd: Subterm,
}

#[derive(Debug)]
pub struct Split {
    pub head: Subterm,
    pub fst: String,
    pub snd: String,
    pub tail: Subterm,
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    pub index: usize,
}

#[derive(Debug)]
pub struct Case {
    pub head: Subterm,
    pub cases: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Let {
    pub name: String,
    pub body: Subterm,
    pub tail: Subterm,
}

#[derive(Debug)]
pub struct LetRec {
    pub names: Vec<String>,
    pub items: Vec<Subterm>,
    pub tail: Subterm,
}

#[derive(Debug)]
pub enum Term {
    Erased,
    Prim(Prim),
    Elim(Elim),
    Func(Func),
    Apply(Apply),
    Pair(Pair),
    Split(Split),
    Atom(Atom),
    Case(Case),
    Let(Let),
    LetRec(LetRec),
    Name(Name),
}

impl From<Prim> for Term {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl From<Elim> for Term {
    fn from(value: Elim) -> Self {
        Self::Elim(value)
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

impl From<Atom> for Term {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

impl From<Case> for Term {
    fn from(value: Case) -> Self {
        Self::Case(value)
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
