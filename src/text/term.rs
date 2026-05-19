use {
    super::{Atom, Prim},
    std::collections::{BTreeMap, BTreeSet},
};

pub type Subterm = Box<Term>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub label: Option<String>,
    pub input: Subterm,
    pub output: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub label: String,
    pub body: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub head: Subterm,
    pub param: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleType {
    pub fields: Vec<(Option<String>, Subterm)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub fields: Vec<Subterm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatFold {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub zero_case: Subterm,
    pub pred_label: String,
    pub ih_label: String,
    pub succ_case: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Split {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub field_labels: Vec<String>,
    pub tail: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub head: Subterm,
    pub motive_label: String,
    pub motive: Subterm,
    pub cases: BTreeMap<Atom, Subterm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub label: String,
    pub type_: Subterm,
    pub body: Subterm,
    pub tail: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecItem {
    pub label: String,
    pub type_: Subterm,
    pub value: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rec {
    pub items: Vec<RecItem>,
    pub tail: Subterm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Type,
    Prim(Prim),
    NatFold(NatFold),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    Split(Split),
    AtomType(AtomType),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Var(Var),
}
