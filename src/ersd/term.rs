use super::{Name, Prim};

pub type Subterm = Box<Term>;

#[derive(Debug)]
pub struct NatFold {
    pub head: Subterm,
    pub zero_case: Subterm,
    pub pred: String,
    pub ih: String,
    pub succ_case: Subterm,
}

#[derive(Debug)]
pub struct NatMatch {
    pub head: Subterm,
    pub cases: Vec<(u32, Subterm)>,
    pub default: Subterm,
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
pub struct Tuple {
    pub fields: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Split {
    pub head: Subterm,
    pub fields: Vec<String>,
    pub tail: Subterm,
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    pub index: usize,
}

#[derive(Debug)]
pub struct Match {
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
pub struct Rec {
    pub names: Vec<String>,
    pub items: Vec<Subterm>,
    pub tail: Subterm,
}

#[derive(Debug)]
pub enum Term {
    Erased,
    Prim(Prim),
    NatFold(NatFold),
    NatMatch(NatMatch),
    Func(Func),
    Apply(Apply),
    Tuple(Tuple),
    Split(Split),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Name(Name),
}

impl From<Prim> for Term {
    fn from(value: Prim) -> Self {
        Self::Prim(value)
    }
}

impl From<NatFold> for Term {
    fn from(value: NatFold) -> Self {
        Self::NatFold(value)
    }
}

impl From<NatMatch> for Term {
    fn from(value: NatMatch) -> Self {
        Self::NatMatch(value)
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

impl From<Tuple> for Term {
    fn from(value: Tuple) -> Self {
        Self::Tuple(value)
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

impl From<Rec> for Term {
    fn from(value: Rec) -> Self {
        Self::Rec(value)
    }
}

impl From<Name> for Term {
    fn from(value: Name) -> Self {
        Self::Name(value)
    }
}
