use super::{Atom, Name};

pub type Subterm = Box<Term>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<const ARITY: usize> {
    body: Subterm,
}

impl<const ARITY: usize> Scope<ARITY> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Type,

    FuncType(Subterm, Scope<1>),
    Func(Scope<1>),
    Apply(Subterm, Subterm),

    PairType(Subterm, Scope<1>),
    Pair(Subterm, Subterm),
    Split(Subterm, Scope<2>),

    AtomType(Vec<Atom>),
    Atom(Atom),
    Match(Subterm, Vec<(Atom, Term)>),

    Bind(Name, Subterm, Scope<1>),

    Name(Name),
}
