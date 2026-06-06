use {
    super::{Atom, Name, Prim},
    crate::Span,
    std::{
        collections::{BTreeMap, BTreeSet},
        ops::Deref,
    },
};

#[derive(Debug, Clone)]
pub struct Term {
    span: Option<Span>,
    inner: Box<Subterm>,
}

impl Term {
    /// Attaches a span to this term. If the term already carries a span (the
    /// innermost one), it is preserved — innermost wins, matching how
    /// `Error::at` keeps the first span it sees as errors propagate up.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }

        self
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

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

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl From<Subterm> for Term {
    fn from(subterm: Subterm) -> Self {
        Self {
            span: None,
            inner: Box::new(subterm),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<(Option<String>, Term)>,
    pub output: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub params: Vec<String>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub fields: Vec<(Option<String>, Term)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Motive {
    pub label: Option<String>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NatMatch {
    Induction {
        head: Term,
        motive: Motive,
        zero_case: Term,
        pred_label: String,
        ih_label: String,
        succ_case: Term,
    },
    Dispatch {
        head: Term,
        motive: Motive,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlnMatch {
    pub head: Term,
    pub motive: Motive,
    pub false_case: Term,
    pub true_case: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtomType {
    pub atoms: BTreeSet<Atom>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtomMatch {
    pub head: Term,
    pub motive: Motive,
    pub cases: BTreeMap<Atom, Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionCase {
    pub binders: Vec<String>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMatch {
    pub head: Term,
    pub motive: Motive,
    pub cases: BTreeMap<String, UnionCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Bln(BlnMatch),
    Nat(NatMatch),
    Atom(AtomMatch),
    Union(UnionMatch),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetSignature {
    Name {
        type_: Term,
        body: Term,
    },
    Func {
        params: Vec<(String, Term)>,
        output: Term,
        body: Term,
    },
}

impl LetSignature {
    pub fn type_(&self) -> Term {
        match self {
            LetSignature::Name { type_, .. } => type_.clone(),
            LetSignature::Func { params, output, .. } => Subterm::FuncType(FuncType {
                params: params
                    .iter()
                    .map(|(n, t)| (Some(n.clone()), t.clone()))
                    .collect(),
                output: output.clone(),
            })
            .into(),
        }
    }

    pub fn body(&self) -> Term {
        match self {
            LetSignature::Name { body, .. } => body.clone(),
            LetSignature::Func { params, body, .. } => Subterm::Func(Func {
                params: params.iter().map(|(n, _)| n.clone()).collect(),
                body: body.clone(),
            })
            .into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub label: String,
    pub signature: LetSignature,
    pub tail: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecItem {
    pub label: String,
    pub signature: LetSignature,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rec {
    pub items: Vec<RecItem>,
    pub tail: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Subterm {
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
    Let(Let),
    Rec(Rec),
    Name(Name),
    /// A surface hole `_`: a placeholder elaborated to a fresh metavariable.
    /// Carries no payload — its span rides on the wrapping [`Term`].
    Hole,
}
