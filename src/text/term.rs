use {
    super::{Atom, Name, Prim},
    std::collections::{BTreeMap, BTreeSet},
};

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub label: Option<String>,
    pub input: Subterm,
    pub output: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub label: String,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub head: Subterm,
    pub param: Subterm,
}

impl Apply {
    pub fn many(head: Term, params: Vec<Term>) -> Term {
        params.into_iter().fold(head, |head, param| {
            Term::Apply(Apply {
                head: head.into(),
                param: param.into(),
            })
        })
    }
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
pub struct NatFold {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub zero_case: Subterm,
    pub pred_label: String,
    pub ih_label: String,
    pub succ_case: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NatMatch {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub cases: BTreeMap<u32, Subterm>,
    pub default: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Split {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub field_labels: Vec<String>,
    pub tail: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub cases: BTreeMap<Atom, Subterm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct From {
    pub label: String,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Into {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Type,
    Prim(Prim),
    NatFold(NatFold),
    NatMatch(NatMatch),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    Split(Split),
    AtomType(AtomType),
    Atom(Atom),
    Match(Match),
    From(From),
    Into(Into),
    Let(Let),
    Rec(Rec),
    Name(Name),
}
