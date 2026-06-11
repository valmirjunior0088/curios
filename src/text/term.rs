use {
    super::{Name, Plicity, Prim},
    crate::Span,
    std::{collections::BTreeMap, ops::Deref},
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
    pub params: Vec<(Plicity, Option<String>, Term)>,
    pub output: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    /// Each parameter carries an optional domain annotation. `None` is the
    /// surface `(x) => …` form, sugar for `(x : _) => …`; it lowers to a hole
    /// (`to_core::elaborate`), solved against the expected function type when the
    /// lambda is checked, or synthesized from the annotation when inferred.
    pub params: Vec<(String, Option<Term>)>,
    pub body: Term,
}

/// Each argument carries its call-site plicity mark: `@`-arguments fill
/// implicit binders, plain arguments fill explicit ones. The marks lower to
/// core untouched — `to_core` is type-blind and cannot match them to slots.
#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<(Plicity, Term)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub fields: Vec<(Option<String>, Term)>,
}

/// Tuple literal fields may carry a name annotation (`(status = 0, handle = h)`);
/// names are checked positionally against the expected tuple type's labels at
/// elaboration and never survive past it.
#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub fields: Vec<(Option<String>, Term)>,
}

/// A match's motive ladder — one grammar growing, the binder parenthesized
/// in every form (motives look exactly like the lambdas they morally are):
///
/// - `match v : P` — constant;
/// - `match v : (x) => P` — depends on the scrutinee;
/// - `match v : (x : Vec(T, k)) => P` — the annotated type-pattern form,
///   union scrutinees only: binds the indices where they naturally appear.
#[derive(Debug, Clone, PartialEq)]
pub enum Motive {
    Constant(Term),
    Scrutinee {
        label: String,
        body: Term,
    },
    Annotated {
        label: String,
        /// The union type the annotation names.
        name: Name,
        /// The written argument slots, positionally (parameters then
        /// indices); a bare unresolvable identifier is a binder, anything
        /// else verbatim — classified at lowering, validated positionally
        /// by core elaboration against the registry.
        slots: Vec<Term>,
        body: Term,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum NatMatch {
    Induction {
        head: Term,
        motive: Option<Motive>,
        zero_case: Term,
        pred_label: String,
        ih_label: String,
        succ_case: Term,
    },
    Dispatch {
        head: Term,
        motive: Option<Motive>,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlnMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub false_case: Term,
    pub true_case: Term,
}

/// A projection names its field either positionally (`p.0`) or by the tuple
/// type's label (`p.status`). Labels are resolved to positions during core
/// elaboration; they never survive past it.
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Index(usize),
    Label(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proj {
    pub head: Term,
    pub field: Field,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionCase {
    pub binders: Vec<String>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub cases: BTreeMap<String, UnionCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Bln(BlnMatch),
    Nat(NatMatch),
    Union(UnionMatch),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetSignature {
    Name {
        /// `None` only for a local `let x = body` (the parser forbids omitting
        /// the type for top-level `let` and for every `rec` binding). It lowers
        /// to a hole so the core elaborator infers the body's type.
        type_: Option<Term>,
        body: Term,
    },
    Func {
        params: Vec<(Plicity, String, Term)>,
        output: Term,
        body: Term,
    },
}

impl LetSignature {
    pub fn type_(&self) -> Term {
        match self {
            LetSignature::Name {
                type_: Some(type_), ..
            } => type_.clone(),
            // An omitted (local-only) annotation lowers to a hole, so the core
            // elaborator infers the body's type; identical to writing `: _`.
            LetSignature::Name { type_: None, .. } => Subterm::Hole.into(),
            LetSignature::Func { params, output, .. } => Subterm::FuncType(FuncType {
                params: params
                    .iter()
                    .map(|(plicity, n, t)| (*plicity, Some(n.clone()), t.clone()))
                    .collect(),
                output: output.clone(),
            })
            .into(),
        }
    }

    pub fn body(&self) -> Term {
        match self {
            LetSignature::Name { body, .. } => body.clone(),
            // The lambda binds every parameter, implicit or not — plicity is a
            // fact about the *type*, consulted only at application sites.
            LetSignature::Func { params, body, .. } => Subterm::Func(Func {
                params: params
                    .iter()
                    .map(|(_, n, t)| (n.clone(), Some(t.clone())))
                    .collect(),
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

/// A `with <bind>  <body>` block: monadic do-notation. `bind` is an atomic term
/// denoting a binary bind `(M A, A -> M B) -> M B` — typically a partial
/// application like `Parse/bind(?, ?)`. The desugarer re-elaborates `bind` at each
/// `!` site — minting fresh holes, so a region can mix action types — and applies
/// it to the action and continuation, keeping the bind's head in head position
/// (synthesizable without annotations). `body` sequences effects via the postfix
/// `!`. Both this and [`Subterm::Bang`] exist only between parsing and desugaring —
/// `to_core::elaborate` eliminates them before core elaboration.
#[derive(Debug, Clone, PartialEq)]
pub struct With {
    pub bind: Term,
    pub body: Term,
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
    Match(Match),
    Let(Let),
    Rec(Rec),
    With(With),
    /// A postfix bang `e!`: extracts the result of monadic action `e` inline.
    /// The operand is the action whose result is bound. Only meaningful inside a
    /// [`With`] body; a stray `Bang` is rejected during desugaring.
    Bang(Term),
    Name(Name),
    /// A surface hole `?`: a placeholder elaborated to a fresh metavariable.
    /// Carries no payload — its span rides on the wrapping [`Term`].
    Hole,
}
