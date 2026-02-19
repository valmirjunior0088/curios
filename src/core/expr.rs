use {
    super::{Atom, Name},
    std::array,
};

pub type Node = Box<Term>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<const ARITY: usize> {
    body: Node,
}

impl<const ARITY: usize> Scope<ARITY> {
    pub fn bind<F>(names: [&str; ARITY], f: F) -> Self
    where
        F: FnOnce([Term; ARITY]) -> Term,
    {
        Self {
            body: Box::new(f(array::from_fn(|index| {
                Term::Name(Name::new(index, names[index]))
            }))),
        }
    }

    pub fn open(self, terms: [Term; ARITY]) -> Node {
        unimplemented!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Type,

    FuncType(Node, Scope<1>),
    Func(Scope<1>),
    Apply(Node, Node),

    PairType(Node, Scope<1>),
    Pair(Node, Node),
    Split(Node, Scope<2>),

    AtomType(Vec<Atom>),
    Atom(Atom),
    Match(Node, Vec<(Atom, Term)>),

    Bind(Name, Node, Scope<1>),

    Name(Name),
}
