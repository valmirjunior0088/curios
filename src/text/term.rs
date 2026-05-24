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
pub struct Match {
    pub head: Subterm,
    pub motive: Motive,
    pub cases: BTreeMap<Atom, Subterm>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Type,
    Prim(Prim),
    BlnMatch(BlnMatch),
    NatFold(NatFold),
    NatMatch(NatMatch),
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
}
