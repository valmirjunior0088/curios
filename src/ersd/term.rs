use {
    super::{Name, Prim},
    std::{collections::BTreeMap, ops::Deref},
};

#[derive(Debug)]
pub struct Term {
    inner: Box<Subterm>,
}

impl Term {
    pub fn into_subterm(self) -> Subterm {
        *self.inner
    }

    pub fn as_subterm(&self) -> &Subterm {
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
    fn from(subterm: Subterm) -> Self {
        Self {
            inner: Box::new(subterm),
        }
    }
}

#[derive(Debug)]
pub enum NatMatch {
    Induction {
        head: Term,
        zero_case: Term,
        pred: String,
        ih: String,
        succ_case: Term,
    },
    Dispatch {
        head: Term,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

/// A captured or parameter binder, plus its specialization-*candidate* flag: true
/// when its (pre-erasure) type was a function, a `Type`, or unit. Computed during
/// type-directed erasure (the last point types are available) and carried to
/// `cont`, glued to the name so the two can never desync. Defaults to
/// non-candidate, so a binder built from a bare name (`"x".into()`) is not one.
#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub candidate: bool,
}

impl<S: Into<String>> From<S> for Argument {
    fn from(name: S) -> Self {
        Self {
            name: name.into(),
            candidate: false,
        }
    }
}

impl Argument {
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct Func {
    pub captures: Vec<Argument>,
    pub params: Vec<Argument>,
    pub body: Term,
}

#[derive(Debug)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
}

#[derive(Debug)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

#[derive(Debug)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    pub index: usize,
}

#[derive(Debug)]
pub struct Match {
    pub head: Term,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Let {
    pub name: String,
    pub body: Term,
    pub tail: Term,
}

#[derive(Debug)]
pub struct Rec {
    pub names: Vec<String>,
    pub items: Vec<Term>,
    pub tail: Term,
}

#[derive(Debug)]
pub enum Subterm {
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
