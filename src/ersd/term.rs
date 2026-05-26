use super::{Name, Prim};

pub type Subterm = Box<Term>;

#[derive(Debug)]
pub enum NatMatch {
    Induction {
        head: Subterm,
        zero_case: Subterm,
        pred: String,
        ih: String,
        succ_case: Subterm,
    },
    Dispatch {
        head: Subterm,
        cases: Vec<(u32, Subterm)>,
        default: Subterm,
    },
}

#[derive(Debug)]
pub struct Func {
    pub captures: Vec<String>,
    pub params: Vec<String>,
    pub body: Subterm,
}

#[derive(Debug)]
pub struct Apply {
    pub head: Subterm,
    pub params: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Tuple {
    pub fields: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Proj {
    pub head: Subterm,
    pub index: usize,
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
    NatMatch(NatMatch),
    Func(Func),
    Apply(Apply),
    Tuple(Tuple),
    Proj(Proj),
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

impl From<Proj> for Term {
    fn from(value: Proj) -> Self {
        Self::Proj(value)
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
