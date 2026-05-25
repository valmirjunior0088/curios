use {
    super::{Atom, Name, Prim},
    crate::Span,
    std::collections::{BTreeMap, BTreeSet},
};

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<(Option<String>, Subterm)>,
    pub output: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub params: Vec<String>,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Apply {
    Call {
        head: Subterm,
        params: Vec<Subterm>,
    },
    Closure {
        head: Subterm,
        args: Vec<Option<Subterm>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub fields: Vec<(Option<String>, Subterm)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub fields: Vec<Subterm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Motive {
    pub label: Option<String>,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NatFold {
    pub head: Subterm,
    pub motive: Motive,
    pub zero_case: Subterm,
    pub pred_label: String,
    pub ih_label: String,
    pub succ_case: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NatMatch {
    pub head: Subterm,
    pub motive: Motive,
    pub cases: BTreeMap<u32, Subterm>,
    pub default: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlnMatch {
    pub head: Subterm,
    pub motive: Motive,
    pub false_case: Subterm,
    pub true_case: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proj {
    pub head: Subterm,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtomMatch {
    pub head: Subterm,
    pub motive: Motive,
    pub cases: BTreeMap<Atom, Subterm>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Bln(BlnMatch),
    NatFold(NatFold),
    Nat(NatMatch),
    Atom(AtomMatch),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefFrom {
    pub label: String,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefInto {
    pub label: String,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub label: String,
    pub type_: Subterm,
    pub body: Subterm,
    pub tail: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecItem {
    pub label: String,
    pub type_: Subterm,
    pub value: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rec {
    pub items: Vec<RecItem>,
    pub tail: Subterm,
}

#[derive(Debug, Clone)]
pub enum Term {
    Type,
    Prim(Prim),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    Proj(Proj),
    AtomType(AtomType),
    Atom(Atom),
    Match(Match),
    DefFrom(DefFrom),
    DefInto(DefInto),
    Let(Let),
    Rec(Rec),
    Name(Name),
    Spanned(Span, Subterm),
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        let mut this = self;
        let mut that = other;

        loop {
            match (this, that) {
                (Term::Spanned(_, inner), _) => this = inner,
                (_, Term::Spanned(_, inner)) => that = inner,
                (Term::Type, Term::Type) => break true,
                (Term::Prim(a), Term::Prim(b)) => break a == b,
                (Term::FuncType(a), Term::FuncType(b)) => break a == b,
                (Term::Func(a), Term::Func(b)) => break a == b,
                (Term::Apply(a), Term::Apply(b)) => break a == b,
                (Term::TupleType(a), Term::TupleType(b)) => break a == b,
                (Term::Tuple(a), Term::Tuple(b)) => break a == b,
                (Term::Proj(a), Term::Proj(b)) => break a == b,
                (Term::AtomType(a), Term::AtomType(b)) => break a == b,
                (Term::Atom(a), Term::Atom(b)) => break a == b,
                (Term::Match(a), Term::Match(b)) => break a == b,
                (Term::DefFrom(a), Term::DefFrom(b)) => break a == b,
                (Term::DefInto(a), Term::DefInto(b)) => break a == b,
                (Term::Let(a), Term::Let(b)) => break a == b,
                (Term::Rec(a), Term::Rec(b)) => break a == b,
                (Term::Name(a), Term::Name(b)) => break a == b,
                _ => break false,
            }
        }
    }
}
