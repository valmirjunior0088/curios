use {
    super::{Name, Prim, Radix},
    curios_base::{NumOp, Plicity, Span},
    num_bigint::BigUint,
    std::{collections::BTreeMap, ops::Deref},
};

/// The unit of the surface syntax tree: a [`Subterm`] plus an optional source span. The span is deliberately excluded from `PartialEq` — tests build spanless expected trees and compare structure — and is readable only crate-internally; `Deref<Target = Subterm>` lets consumers match on the structure directly.
#[derive(Debug, Clone)]
pub struct Term {
    span: Option<Span>,
    inner: Box<Subterm>,
}

impl Term {
    /// Attaches a span to this term. If the term already carries a span (the
    /// innermost one), it is preserved — innermost wins, matching how
    /// `Error::at` keeps the first span it sees as errors propagate up.
    pub(crate) fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }

        self
    }

    pub(crate) fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub(crate) fn into_subterm(self) -> Subterm {
        *self.inner
    }

    pub(crate) fn as_subterm(&self) -> &Subterm {
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

/// One Π-binder as written: its plicity (`@` on the name), an optional binder
/// name, and the domain type.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncTypeParam {
    pub plicity: Plicity,
    pub label: Option<String>,
    pub type_: Term,
}

/// One Σ-type / struct-declaration field as written: an optional label and the
/// field type. Shared by tuple types and `struct` declarations (the `TopStruct`
/// fields reuse this grammar).
#[derive(Debug, Clone, PartialEq)]
pub struct TupleTypeParam {
    pub label: Option<String>,
    /// `Some` for the signature sugar `label(params) -> type_` — the written
    /// parameter list, kept verbatim so the printer round-trips it. `to_core`
    /// undoes the sugar, lowering the field as `label : (params) -> type_`
    /// (see `TupleTypeParam::desugared_type`). Always paired with a label.
    pub func_params: Option<Vec<FuncTypeParam>>,
    pub type_: Term,
}

impl TupleTypeParam {
    /// The field's type with the signature sugar undone: the written type when
    /// the field is plain, the Π-type `(params) -> type_` when it was written
    /// `label(params) -> type_`.
    pub(crate) fn desugared_type(&self) -> Term {
        match &self.func_params {
            Some(params) => Subterm::FuncType(FuncType {
                params: params.clone(),
                output: self.type_.clone(),
            })
            .into(),
            None => self.type_.clone(),
        }
    }
}

/// A Π-/function type `(x : A, @y : B) -> C(x)`: a dependent telescope of binders (see [`FuncTypeParam`]) and the output type, which may mention any named binder.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<FuncTypeParam>,
    pub output: Term,
}

/// A lambda `(x, (a, b) : P) => body`. Domain annotations are optional and parameters may be compound patterns — the field doc details how each lowers.
#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    /// Each parameter is a binder pattern with an optional domain annotation.
    /// `None` is the surface `(x) => …` form, sugar for `(x : _) => …`; it
    /// lowers to a hole, solved by `curios_core::elaborate` against the
    /// expected function type when the lambda is checked, or synthesized from
    /// the annotation when inferred. A compound (tuple/struct) pattern desugars
    /// at lowering into a fresh core binder plus a projection-`let` chain —
    /// see [`Pattern`].
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

/// A Σ-/tuple type `(fst : A, snd : B(fst))`: a dependent telescope of fields — a later field's type may mention an earlier field's label. Its field grammar ([`TupleTypeParam`]) is also what `struct`/`record` declarations reuse.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub fields: Vec<TupleTypeParam>,
}

/// One tuple-literal / struct-literal field as written: an optional name
/// annotation (`(status = 0, handle = h)`) and the field value. Names are
/// checked positionally against the expected tuple type's labels at
/// elaboration and never survive past it.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    pub label: Option<String>,
    /// `Some` for the definition sugar `label(params) = value` — the written
    /// parameter list, kept verbatim so the printer round-trips it. `to_core`
    /// undoes the sugar, lowering the field as `label = (params) => value`
    /// (see `TupleField::desugared_value`). Always paired with a label.
    pub func_params: Option<Vec<(String, Option<Term>)>>,
    pub value: Term,
}

impl TupleField {
    /// The field's value with the definition sugar undone: the written value
    /// when the field is plain, the lambda `(params) => value` when it was
    /// written `label(params) = value`.
    pub(crate) fn desugared_value(&self) -> Term {
        match &self.func_params {
            // This sugar's own parameter list stays plain-name-only (it is not
            // one of the pattern-accepting binder sites), so each name is
            // wrapped as a trivial `Pattern::Binder` to match `Func.params`'s
            // element type.
            Some(params) => Subterm::Func(Func {
                params: params
                    .iter()
                    .map(|(name, ty)| (Pattern::Binder(Some(name.clone())), ty.clone()))
                    .collect(),
                body: self.value.clone(),
            })
            .into(),
            None => self.value.clone(),
        }
    }
}

/// A tuple / anonymous-record literal `(a, snd = b)` — see [`TupleField`] for the per-field grammar. Struct literals do not reuse this node: a named head is a [`StructLit`], validated against its declared type.
#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub fields: Vec<TupleField>,
}

/// A binder pattern at `let`, lambda-parameter, or function-definition-sugar
/// parameter position: a plain name, or a tuple/struct destructuring that
/// desugars — at lowering, in `to_core` — into a fresh synthetic binder plus a
/// chain of ordinary projection `let`s, exactly what a person would hand-write
/// today. Always irrefutable: unlike a match-arm pattern, there is no
/// constructor-tag case, since these binder sites never dispatch on shape —
/// a tuple/struct value always has exactly one shape.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// `None` only for a function-sugar `use` parameter, which has no source
    /// binder position at all — genuinely anonymous, not a user-spelled `_`.
    /// Lowering mints a fresh internal name for it directly; `Some("_")` (a
    /// user actually typing the wildcard) goes through the same gensym path
    /// but is a distinct case, kept apart so an anonymous `use` binder lowers
    /// its Π-type binder as truly unlabeled (see `LetSignature::type_`)
    /// rather than as a Π-binder spelled `"_"`.
    Binder(Option<String>),
    Tuple(Vec<PatternField>),
    Struct {
        head: String,
        fields: Vec<PatternField>,
    },
}

/// One tuple-pattern / struct-pattern field: a labeled sub-pattern (`label =
/// pattern`) or a bare positional one. The literal mirror of [`TupleField`]
/// with `Term` replaced by `Pattern` in the value slot — construction and
/// destructuring read as inverses of each other.
#[derive(Debug, Clone, PartialEq)]
pub struct PatternField {
    pub label: Option<String>,
    pub value: Pattern,
}

/// A match's motive ladder — one grammar growing, the binder parenthesized
/// in every form (motives look exactly like the lambdas they morally are):
///
/// - `match v : P` — constant;
/// - `match v : (x) => P` — depends on the scrutinee;
/// - `match v : (x : Vec(T, k)) => P` — the annotated type-pattern form,
///   inductive scrutinees only: binds the indices where they naturally appear.
#[derive(Debug, Clone, PartialEq)]
pub enum Motive {
    Constant(Term),
    Scrutinee {
        label: String,
        body: Term,
    },
    Annotated {
        label: String,
        /// The inductive type the annotation names.
        name: Name,
        /// The written argument slots, positionally (parameters then
        /// indices); a bare unresolvable identifier is a binder, anything
        /// else verbatim — classified at lowering, validated positionally
        /// by core elaboration against the registry.
        slots: Vec<Term>,
        body: Term,
    },
}

/// A `Nat` match, in its two surface forms. `Induction` is the structural `| 0 => … | n + 1; ih => …` split, with the induction hypothesis bound in the successor arm (mandatory, unlike `Lst`/`Bin`'s optional `; ih`). `Dispatch` is literal dispatch — `u32` literal cases plus a mandatory default arm — for matching specific numerals without peeling one successor at a time.
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

/// A `Bln` case split: `match b | false => … | true => …`, both arms structurally mandatory (the hardcoded carriers have no core-side exhaustiveness mechanism to fall back on). `Bln` has no recursive structure, so unlike its `Nat`/`Lst`/`Bin` siblings there is no induction-hypothesis form at all.
#[derive(Debug, Clone, PartialEq)]
pub struct BlnMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub false_case: Term,
    pub true_case: Term,
}

/// Structural induction on an `Lst`: an `| [] =>` identity arm and a
/// `| [head, ..tail]; ih =>` cons arm, mirroring the `Lst` literal's own
/// bracket-and-comma shape. The surface analogue of `NatMatch::Induction` for
/// the native free-monoid primitives (the empty literal selects the carrier).
/// `ih_label` is `None` when `; ih` is omitted — a plain case-split with no
/// induction hypothesis at all, not a user-spelled placeholder name; lowering
/// mints a fresh internal name for it directly.
#[derive(Debug, Clone, PartialEq)]
pub struct LstMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub empty_case: Term,
    pub head_label: String,
    pub tail_label: String,
    pub ih_label: Option<String>,
    pub cons_case: Term,
}

/// Structural induction on a `Bin`: a `| \\ =>` identity arm (the empty
/// bytestring literal) and a `| \head\..tail; ih =>` cons arm, mirroring the
/// `Bin` literal's own backslash-delimited shape, whose `head` is the leading
/// byte (a `Nat`) and `tail` the rest. The `Bin` analogue of [`LstMatch`];
/// `Bin` carries no element type, so there is no carrier parameter to read off.
/// `ih_label` is `None` when `; ih` is omitted, exactly as in [`LstMatch`].
#[derive(Debug, Clone, PartialEq)]
pub struct BinMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    pub empty_case: Term,
    pub head_label: String,
    pub tail_label: String,
    pub ih_label: Option<String>,
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

/// A projection `head.field` out of a tuple/struct value — positional (`p.0`) or by label (`p.status`), see [`Field`].
#[derive(Debug, Clone, PartialEq)]
pub struct Proj {
    pub head: Term,
    pub field: Field,
}

/// One struct-literal entry: a plain field (the tuple-literal field grammar —
/// `fst = a` or positional), a `use <term>` fill for a concept's next
/// `use`-marked field position (mirroring call-site witness arguments; only
/// meaningful when the head is a concept, enforced at core elaboration), or a
/// `..base` spread copying every unwritten field from `base` (legal only
/// first and at most once, also enforced at core elaboration).
#[derive(Debug, Clone, PartialEq)]
pub enum StructLitEntry {
    Field(TupleField),
    Use(Term),
    Spread(Term),
}

/// A struct literal: a head naming the struct type (`Pair`, possibly applied —
/// `Pair(Nat, Bin)` / `Pair(Nat, ?)` — to pin the parameters) followed by a
/// brace of entries. `params` is the optionally-applied head arguments (empty
/// for the bare-name head; holes appear as `?` terms). Plain entries are
/// validated positionally against the declared non-`use` labels at core
/// elaboration; a concept's `use`-marked fields are filled by `use <term>`
/// entries or, when omitted, by witness resolution.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLit {
    pub head: Name,
    pub params: Vec<Term>,
    pub entries: Vec<StructLitEntry>,
}

/// The general pattern match: one scrutinee and arms of arbitrary (nested, across constructors, tuples, structs, and the four literal-carrier leaves) [`MatchPattern`]s, compiled by the pattern-matrix scheme in `to_core::lower` into the same single-level core match/projection forms a person would get from hand-nesting matches.
#[derive(Debug, Clone, PartialEq)]
pub struct MatrixMatch {
    pub head: Term,
    pub motive: Option<Motive>,
    /// The arms, in source order. Each pairs a (possibly nested, across
    /// constructors/tuples/structs — see [`MatchPattern`]) pattern with its
    /// body; zero arms is legal (a vacuous elimination, e.g. of `False`).
    /// The grammar enforces "full enumeration" (no wildcard/catch-all, no
    /// row priority — see `to_core::lower`'s doc comment); lowering rejects
    /// a repeated tag and an overlapping/duplicate row.
    pub arms: Vec<MatrixArm>,
}

/// One arm of a [`MatrixMatch`]: `| pattern => body`. Compiled by
/// `to_core::lower` into the single-level core match/projection forms —
/// exactly what a person would get from hand-nesting matches today (see its
/// doc comment). A flat, unnested arm (`tag(x, y) => body`, i.e. every
/// argument a plain [`MatchPattern::Binder`]) lowers exactly as before.
#[derive(Debug, Clone, PartialEq)]
pub struct MatrixArm {
    pub pattern: MatchPattern,
    pub body: Term,
}

/// A surface `match`, split by what the arms dispatch on: the four hardcoded carriers (`Bln`, `Nat`, `Lst`, `Bin`) each get a fixed-shape variant whose arms are exactly the carrier's own cases, while constructor-tag dispatch — including nested and tuple/struct patterns — is the general [`MatrixMatch`]. The parser classifies by arm shape, and each variant lowers through a different core elimination form.
#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Bln(BlnMatch),
    Nat(NatMatch),
    Matrix(MatrixMatch),
    Lst(LstMatch),
    Bin(BinMatch),
}

/// A match-arm pattern: genuinely refutable, unlike [`Pattern`] (which is
/// always irrefutable — see its own doc comment). Distinct from `Pattern` on
/// purpose, even though `Tuple`/`Struct` mirror its field grammar exactly:
/// a `let`/lambda/function-sugar binder site never dispatches on shape, but a
/// match arm's whole point is dispatching on shape, so the two must never be
/// conflated.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    /// A plain name (or `_`) — never splits a column by itself; legal only
    /// when every row shares this shape in that column (see the matrix
    /// compiler in `to_core::lower`).
    Binder(String),
    /// An inductive constructor tag applied to sub-patterns — positional
    /// (constructors have no field labels in this language).
    Ctor {
        tag: String,
        args: Vec<MatchPattern>,
    },
    /// A tuple pattern — field grammar mirrors [`PatternField`] exactly.
    Tuple(Vec<MatchPatternField>),
    /// A struct/record pattern — the same labeled/punned/positional grammar
    /// as struct literals and today's irrefutable [`Pattern::Struct`], not
    /// the positional constructor-call shape (structs have field labels;
    /// inductive constructors don't).
    Struct {
        head: String,
        fields: Vec<MatchPatternField>,
    },
    /// A `Bln` literal leaf: `true` or `false`.
    Bln(bool),
    /// A nested `Nat` literal leaf — mirrors [`NatMatch::Induction`]'s own
    /// `0`/`n+1; ih` arms.
    Nat(NatPattern),
    /// A nested `Lst` literal leaf — mirrors [`LstMatch`]'s own `[]`/
    /// `[head,..tail][; ih]` arms.
    Lst(LstPattern),
    /// A nested `Bin` literal leaf — mirrors [`BinMatch`]'s own `\\`/
    /// `\head\..tail[; ih]` arms.
    Bin(BinPattern),
}

/// The two shapes a nested `Nat` leaf can take — see [`MatchPattern::Nat`].
#[derive(Debug, Clone, PartialEq)]
pub enum NatPattern {
    /// The `0` leaf.
    Zero,
    /// The `pred + 1; ih` leaf. `pred_label`/`ih_label` are always plain
    /// binder names, never a further nested sub-pattern — deep peeling in
    /// one arm stays expressible only via hand-nested matches. `ih_label`
    /// is mandatory, mirroring `NatMatch::Induction`'s own asymmetry versus
    /// `Lst`/`Bin` below.
    Succ {
        pred_label: String,
        ih_label: String,
    },
}

/// The two shapes a nested `Lst` leaf can take — see [`MatchPattern::Lst`].
#[derive(Debug, Clone, PartialEq)]
pub enum LstPattern {
    /// The `[]` leaf.
    Nil,
    /// The `[head, ..tail][; ih]` leaf. `ih_label` is `None` when `; ih` is
    /// omitted (lowering mints a fresh internal name), mirroring
    /// `LstMatch`'s own optionality.
    Cons {
        head_label: String,
        tail_label: String,
        ih_label: Option<String>,
    },
}

/// The two shapes a nested `Bin` leaf can take — see [`MatchPattern::Bin`].
#[derive(Debug, Clone, PartialEq)]
pub enum BinPattern {
    /// The `\\` leaf.
    End,
    /// The `\head\..tail[; ih]` leaf, mirroring `BinMatch`'s own optional
    /// `ih_label`.
    Byte {
        head_label: String,
        tail_label: String,
        ih_label: Option<String>,
    },
}

/// One tuple-pattern / struct-pattern field in a [`MatchPattern`]: a labeled
/// sub-pattern (`label = pattern`) or a bare positional one. The literal
/// mirror of [`PatternField`] with `Pattern` replaced by `MatchPattern`.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchPatternField {
    pub label: Option<String>,
    pub value: MatchPattern,
}

/// One parameter of the function-definition sugar `let f(x : T, …) -> R = body`.
/// A plain-name (`Pattern::Binder`) label flows into both the Π-type binder
/// and the lambda parameter, exactly as before. A compound (tuple/struct)
/// pattern has no single name to give the Π-type binder, so it lowers to an
/// *anonymous* Π-binder (see `LetSignature::type_`) — its destructured
/// leaves are visible only in the function's value body, never in a later
/// parameter's type or the output type.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncSugarParam {
    pub plicity: Plicity,
    pub label: Pattern,
    pub type_: Term,
}

/// The right-hand side shared by every `let`-like binding site (local [`Let`], top-level [`TopLet`](crate::TopLet), [`RecItem`]): a plain annotated body, or the function-definition sugar `f(x : T, …) -> R = body`, kept verbatim so the printer round-trips it. Lowering undoes the sugar through the crate-internal `type_()`/`body()` accessors — the type becomes a Π-type, the body a lambda binding every parameter.
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
    pub(crate) fn type_(&self) -> Term {
        match self {
            LetSignature::Name {
                type_: Some(type_), ..
            } => type_.clone(),
            // An omitted (local-only) annotation lowers to a hole, so the core
            // elaborator infers the body's type; identical to writing `: _`.
            LetSignature::Name { type_: None, .. } => Subterm::Hole.into(),
            // A plain-name parameter names its Π-binder, so a later domain or
            // the output may depend on it. A compound pattern has no single
            // name to give the binder, so it lowers anonymously (`None`) —
            // already a fully legal Π-binder shape (e.g. today's `use`-binder
            // or an unlabeled `(T) -> R` parameter).
            LetSignature::Func { params, output, .. } => Subterm::FuncType(FuncType {
                params: params
                    .iter()
                    .map(|param| FuncTypeParam {
                        plicity: param.plicity,
                        label: match &param.label {
                            Pattern::Binder(name) => name.clone(),
                            Pattern::Tuple(_) | Pattern::Struct { .. } => None,
                        },
                        type_: param.type_.clone(),
                    })
                    .collect(),
                output: output.clone(),
            })
            .into(),
        }
    }

    pub(crate) fn body(&self) -> Term {
        match self {
            LetSignature::Name { body, .. } => body.clone(),
            // The lambda binds every parameter, implicit or not — plicity is a
            // fact about the *type*, consulted only at application sites.
            LetSignature::Func { params, body, .. } => Subterm::Func(Func {
                params: params
                    .iter()
                    .map(|param| (param.label.clone(), Some(param.type_.clone())))
                    .collect(),
                body: body.clone(),
            })
            .into(),
        }
    }
}

/// A local `let …; tail`: one binding in scope for `tail` only. The only `let` position where the type annotation may be omitted (`LetSignature::Name` with `type_: None`) — top-level `let` and every `rec` item always carry one.
#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    /// The bound pattern — `let x = …`, `let (x, y) = …`, or the `f` in the
    /// `let f(…) -> R = …` function-definition sugar. A compound pattern here
    /// is grammatically legal for the function-sugar form too, but pointless:
    /// a function's own binding is never itself a tuple/struct value, so
    /// destructuring it fails the same "not a tuple/struct" projection type
    /// error a hand-written misuse would (no special-casing needed).
    pub binder: Pattern,
    pub signature: LetSignature,
    pub tail: Term,
}

/// One definition of a [`Rec`] block: a plain label (no destructuring — the binding must be nameable in its siblings' bodies) and its signature, whose type annotation the parser makes mandatory here.
#[derive(Debug, Clone, PartialEq)]
pub struct RecItem {
    pub label: String,
    pub signature: LetSignature,
}

/// A local `rec` block: a group of mutually recursive definitions, each in scope of every other and of `tail`. Unlike `let` there is no pattern binder or omitted annotation — every item is a plain label with a mandatory type (a recursive reference's type cannot be inferred from a body that mentions it).
#[derive(Debug, Clone, PartialEq)]
pub struct Rec {
    pub items: Vec<RecItem>,
    pub tail: Term,
}

/// A surface infix application `left <op> right`, produced by the
/// precedence-climbing parser. Lowered verbatim to a `core::Infix` and resolved
/// to a concrete scalar primitive during elaboration (the operand types are not
/// yet known at lowering).
#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub op: NumOp,
    pub left: Term,
    pub right: Term,
}

/// A surface polymorphic numeric literal: an integer `magnitude` with an
/// optional written sign. Its concrete type (`Nat`/`Int`/`Flt`) is chosen during
/// elaboration. The `radix` is retained only so the printer round-trips the
/// written form (`0xC2` back to `0xC2`); lowering to core drops it. Decimal
/// literals are not `NumLit`; they parse to `Prim::Flt`.
#[derive(Debug, Clone, PartialEq)]
pub struct NumLit {
    pub magnitude: BigUint,
    pub radix: Radix,
    pub signed: bool,
    pub negative: bool,
}

/// The term grammar proper, one variant per surface form. Spanless by design — a location rides on the wrapping [`Term`] — so `PartialEq` compares structure alone; bare terms are built via `From<Subterm> for Term`. Most variants lower one-to-one onto a core counterpart in `to_core`; the ones that exist only in the surface language are documented on their variants below.
#[derive(Debug, Clone, PartialEq)]
pub enum Subterm {
    Type,
    Prop,
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
    /// A postfix bang `e!`: extracts the result of monadic action `e` inline.
    /// The operand is the action whose result is bound. The `to_core` pass hoists
    /// each bang to the top of its enclosing region (a value body, re-rooted at
    /// lambda bodies, match arms, and `rec` items) and sequences it through
    /// `/syn/Monad/bind`, whose `use` binder resolves the `Monad` witness from
    /// the action's type. Exists only between parsing and `to_core`, which
    /// eliminates it before core elaboration; a bang in a type is rejected.
    Bang(Term),
    Name(Name),
    /// A surface hole `?`: a placeholder elaborated to a fresh metavariable.
    /// Carries no payload — its span rides on the wrapping [`Term`].
    Hole,
    /// A literal whose value is synthesized from `/syn` rather than lowered to a
    /// core primitive (see [`Syn`]). The lowerer runs a meta-emitter on it
    /// instead of `prim()`.
    Syn(Syn),
    /// An infix operator application `left <op> right` (see [`Infix`]).
    Infix(Infix),
    /// A polymorphic numeric literal (see [`NumLit`]).
    NumLit(NumLit),
}

/// The literals the lowerer desugars to a `/syn` construction: a string becomes
/// a proof-carrying `/syn/Str` value. Held as a dedicated [`Subterm`] variant
/// (not a `Prim`) because the result is a core term, never a core primitive.
#[derive(Debug, Clone, PartialEq)]
pub enum Syn {
    Str(String),
}
