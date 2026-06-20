use {
    super::{Name, Plicity, Quantity, Prim},
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

/// One Π-binder as written: its plicity (`@` on the name), its quantity (`@` on
/// the type — erased), an optional binder name, and the domain type.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncTypeParam {
    pub plicity: Plicity,
    pub quantity: Quantity,
    pub label: Option<String>,
    pub type_: Term,
}

/// One Σ-type / struct-declaration field as written: an optional label, a
/// quantity (`@` on the type — erased), and the field type. Shared by tuple
/// types and `struct` declarations (the `TopStruct` fields reuse this grammar).
#[derive(Debug, Clone, PartialEq)]
pub struct TupleTypeParam {
    pub label: Option<String>,
    pub quantity: Quantity,
    pub type_: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<FuncTypeParam>,
    pub output: Term,
}

/// A binding pattern in a binder position (`let`, lambda parameter, match-arm
/// payload). Stage 1 covers the irrefutable *product* fragment — plain binders
/// and positional tuple patterns; both lower to projections off a fresh temp in
/// `to_core`, never to a branch. The grammar is shared with the (future)
/// refutable constructor patterns the pattern-matrix compiler will consume.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// A plain binder `x` (or `_`). `_` lowers to a fresh internal name, so
    /// repeated wildcards within one pattern never collide.
    Bind(String),
    /// A positional tuple pattern `(p0, p1, …)`. Irrefutable — a tuple has a
    /// single shape, so destructuring is pure projection.
    Tuple(Vec<Pattern>),
    /// A nominal struct pattern `Foo { bar, baz }` (pun, binding each field by
    /// its label) or `Foo { bar = p, … }` (rename, binding the nested pattern
    /// `p`). The `head` names the struct type — checked nominally, like the
    /// `Foo { … }` struct literal it mirrors. Irrefutable, and *partial*: only
    /// the listed fields are projected, the rest are ignored.
    Struct {
        head: Name,
        fields: Vec<(String, Pattern)>,
    },
    /// A *refutable* nominal constructor pattern `tag(p, …)` — `nil()` is the
    /// nullary form. Mandatory parens distinguish it from a `Bind`. Appears only
    /// in match-arm rows (stage 2 — the pattern-matrix compiler consumes it);
    /// the irrefutable binder positions (`let`, lambda params) reject it.
    Variant { tag: String, args: Vec<Pattern> },
    /// A *refutable* scalar literal pattern — `0`/`'c'` (Nat) or `true`/`false`
    /// (Bln) — nested in the matrix. Compiles to a `switch` / `bln_match`
    /// dispatch with the catch-all rows as the default.
    Lit(PatternLit),
}

/// The scalar literal a [`Pattern::Lit`] tests. Only the kinds with a core
/// dispatch node are representable: `Nat` (→ `switch`, `u32`-ranged like the
/// surface `match`) and `Bln` (→ `bln_match`). `Int`/`Flt`/`Str` have no
/// elimination node, so they are not pattern-matchable.
#[derive(Debug, Clone, PartialEq)]
pub enum PatternLit {
    Nat(u32),
    Bln(bool),
}

impl Pattern {
    /// The name a Π-binder should carry when this pattern is a function
    /// parameter: a plain `Bind`'s name (so dependent domains/outputs can refer
    /// to it), or `None` for any compound/refutable pattern (no whole-value name
    /// to refer to).
    pub fn binder_name(&self) -> Option<String> {
        match self {
            Pattern::Bind(name) => Some(name.clone()),
            Pattern::Tuple(_)
            | Pattern::Struct { .. }
            | Pattern::Variant { .. }
            | Pattern::Lit(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    /// Each parameter binds a [`Pattern`] and carries an optional domain
    /// annotation. `None` is the surface `(x) => …` form, sugar for `(x : _) =>
    /// …`; it lowers to a hole (`to_core::elaborate`), solved against the
    /// expected function type when the lambda is checked, or synthesized from
    /// the annotation when inferred. A tuple-pattern parameter needs its own
    /// parentheses — `((a, b)) => …` is one pair-destructuring parameter, while
    /// `(a, b) => …` stays two parameters.
    pub params: Vec<(Pattern, Option<Term>)>,
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
    pub fields: Vec<TupleTypeParam>,
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

/// Structural induction on an `Arr`: an `| [] =>` identity arm and a
/// `| (head, tail), ih =>` cons arm. The surface analogue of `NatMatch::Induction`
/// for the native free-monoid primitives (the empty literal selects the carrier).
#[derive(Debug, Clone, PartialEq)]
pub struct ArrMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub empty_case: Term,
    pub head_label: String,
    pub tail_label: String,
    pub ih_label: String,
    pub cons_case: Term,
}

/// Structural induction on a `Bin`: a `| \\ =>` identity arm (the empty
/// bytestring literal) and a `| (head, tail), ih =>` cons arm whose `head` is the
/// leading byte (a `Nat`) and `tail` the rest. The `Bin` analogue of [`ArrMatch`];
/// `Bin` carries no element type, so there is no carrier parameter to read off.
#[derive(Debug, Clone, PartialEq)]
pub struct BinMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub empty_case: Term,
    pub head_label: String,
    pub tail_label: String,
    pub ih_label: String,
    pub cons_case: Term,
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

/// A struct literal: a head naming the struct type (`Pair`, possibly applied —
/// `Pair(Nat, Bin)` / `Pair(Nat, ?)` — to pin the parameters) followed by a
/// brace of fields. `params` is the optionally-applied head arguments (empty
/// for the bare-name head; holes appear as `?` terms). `fields` reuse the
/// tuple-literal field grammar (`fst = a` or positional), validated positionally
/// against the declared labels at core elaboration.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLit {
    pub head: Name,
    pub params: Vec<Term>,
    pub fields: Vec<(Option<String>, Term)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    /// The arm rows, in source order. Each row pairs a (refutable) pattern with
    /// its body. Unlike a per-constructor map, the rows are *ordered* and may
    /// *repeat* a head constructor (`cons(0, _) => … | cons(x, _) => …`) — the
    /// matrix the pattern compiler consumes. The legacy single-level shape is
    /// the special case of distinct-tag [`Pattern::Variant`] rows whose args are
    /// all irrefutable.
    pub rows: Vec<(Pattern, Term)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Bln(BlnMatch),
    Nat(NatMatch),
    Union(UnionMatch),
    Arr(ArrMatch),
    Bin(BinMatch),
}

/// One parameter of the function-definition sugar `let f(p : T, …) -> R = body`.
/// Unlike [`FuncTypeParam`], it carries a [`Pattern`] (so a parameter can
/// destructure) rather than a bare label, and feeds two lowerings: the binder
/// name flows into the Π-type, the whole pattern into the lambda.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncSugarParam {
    pub plicity: Plicity,
    pub quantity: Quantity,
    pub pattern: Pattern,
    pub type_: Term,
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
        params: Vec<FuncSugarParam>,
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
            // A plain-binder parameter names the Π-binder (so a later domain or
            // the output may depend on it); a tuple-pattern parameter has no
            // whole-value name, so its Π-binder is anonymous — the result type
            // cannot mention a destructured argument.
            LetSignature::Func { params, output, .. } => Subterm::FuncType(FuncType {
                params: params
                    .iter()
                    .map(|param| FuncTypeParam {
                        plicity: param.plicity,
                        quantity: param.quantity,
                        label: param.pattern.binder_name(),
                        type_: param.type_.clone(),
                    })
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
            // fact about the *type*, consulted only at application sites. Each
            // parameter keeps its pattern, so tuple parameters destructure in
            // the lambda body exactly as a bare lambda's would.
            LetSignature::Func { params, body, .. } => Subterm::Func(Func {
                params: params
                    .iter()
                    .map(|param| (param.pattern.clone(), Some(param.type_.clone())))
                    .collect(),
                body: body.clone(),
            })
            .into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    /// The bound pattern. A plain `Bind` is the ordinary `let x = …` (and the
    /// `let f(…) -> R = …` function-definition sugar, where `f` is the binder);
    /// a `Tuple` destructures — `let (a, b) = …`. The function-definition
    /// [`LetSignature::Func`] only ever pairs with a `Bind` binder (the parser
    /// never mints a tuple-pattern function definition); a tuple binder always
    /// carries the [`LetSignature::Name`] `(: T)? = value` form.
    pub binder: Pattern,
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

/// A `let ! = <bind>; <body>` block: monadic do-notation. `bind` is an atomic term
/// denoting a binary bind `(M A, A -> M B) -> M B` — typically a reference like
/// `Parse/bind`. The `to_core` pass re-elaborates `bind` at each `!` site — minting
/// fresh holes, so a region can mix action types — and applies it to the action and
/// continuation, keeping the bind's head in head position (synthesizable without
/// annotations). `body` sequences effects via the postfix `!` and runs to the end of
/// the enclosing term (no `end`), like a `let` tail. Both this and [`Subterm::Bang`]
/// exist only between parsing and the `to_core` pass, which eliminates them before
/// core elaboration.
#[derive(Debug, Clone, PartialEq)]
pub struct LetBang {
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
    StructLit(StructLit),
    Match(Match),
    Let(Let),
    Rec(Rec),
    LetBang(LetBang),
    /// A postfix bang `e!`: extracts the result of monadic action `e` inline.
    /// The operand is the action whose result is bound. Only meaningful inside a
    /// [`LetBang`] body; a stray `Bang` is rejected during desugaring.
    Bang(Term),
    Name(Name),
    /// A surface hole `?`: a placeholder elaborated to a fresh metavariable.
    /// Carries no payload — its span rides on the wrapping [`Term`].
    Hole,
}
