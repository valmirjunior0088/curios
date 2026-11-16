#[cfg(test)]
mod tests;

use {
    super::{
        Dialect, Intrinsic, Label, Literal, Name, NatLiteral, Program, Radix, parse_whole_term,
        print_match_pattern, print_term,
    },
    curios_common::{InfixOp, Plicity, Sign, Source, Span, recurse},
    curios_num::{Floating, Natural},
    curios_parse::ParserError,
    curios_print::run_printer,
    std::{fmt, mem, ops::Deref, rc::Rc, str::FromStr, sync::Arc},
};

/// The unit of the surface syntax tree: a [`Subterm`] plus an optional source span. The span is deliberately excluded from `PartialEq` — tests build spanless expected trees and compare structure — and is readable only crate-internally; `Deref<Target = Subterm>` lets consumers match on the structure directly.
///
/// `Rc`-backed like `curios-core`'s `Term`, and for the same reason: a clone must be a pointer bump, never a tree copy. The packrat cache stores a clone of every memoized success, so a `Box`-backed tree would be cloned once per cached offset — O(N²) over nested programs — and the root entry's clone of the whole program would recurse once per level, overflowing on deep operator chains however iterative the parser.
#[derive(Debug, Clone)]
pub struct Term<D: Dialect = Program> {
    span: Option<Span>,
    inner: Rc<Subterm<D>>,
}

impl<D: Dialect> Term<D> {
    /// Attaches a span to this term. If the term already carries a span (the innermost one), it is preserved — innermost wins, matching how `Error::at` keeps the first span it sees as errors propagate up.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }

        self
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub(crate) fn into_subterm(mut self) -> Subterm<D> {
        // A uniquely held node moves out, leaving a husk its drop passes over; a shared one (a packrat cache entry, a spread base) is copied one level — its own `Term` fields are `Rc` bumps, so the copy never recurses.
        match Rc::get_mut(&mut self.inner) {
            Some(subterm) => mem::replace(subterm, Subterm::Type),
            None => Subterm::clone(&self.inner),
        }
    }

    pub fn as_subterm(&self) -> &Subterm<D> {
        &self.inner
    }
}

/// Freed inside [`recurse`]: a tree nests as deep as the text it was read from, and the derived drop descends one native frame per level. A node this drop holds alone gives its subterm up for a husk holding no term, and the subterm is freed inside the bracket; a shared one only loses a reference.
impl<D: Dialect> Drop for Term<D> {
    fn drop(&mut self) {
        let Some(subterm) = Rc::get_mut(&mut self.inner) else {
            return;
        };
        // A form holding no term frees nothing deep, so it takes no bracket, which on a thread short of the reserve maps a segment; a husk is one.
        if matches!(
            subterm,
            Subterm::Type
                | Subterm::Prop
                | Subterm::Sort
                | Subterm::Name(_)
                | Subterm::Hole
                | Subterm::Held
                | Subterm::Goal
                | Subterm::Derive
                | Subterm::ProofLiteral(_)
                | Subterm::NumLit(_)
        ) {
            return;
        }

        let detached = mem::replace(subterm, Subterm::Type);
        recurse(|| drop(detached));
    }
}

impl<D: Dialect> Deref for Term<D> {
    type Target = Subterm<D>;

    fn deref(&self) -> &Subterm<D> {
        &self.inner
    }
}

impl<D: Dialect> PartialEq for Term<D> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<D: Dialect> From<Subterm<D>> for Term<D> {
    fn from(subterm: Subterm<D>) -> Self {
        Self {
            span: None,
            inner: Rc::new(subterm),
        }
    }
}

impl FromStr for Term {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parse_whole_term(&Source::inline(input))
    }
}

impl fmt::Display for Term {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_term(self.clone()), formatter, 4)
    }
}

impl fmt::Display for MatchPattern {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_match_pattern(self.clone()), formatter, 4)
    }
}

/// One Π-binder as written: its mark, an optional binder name, and the domain type. A `use` member is the type alone — the parser never writes a name beside the witness mark — and a plain or `@` member is named or not: `n: Nat`, `Nat`, `@n: Nat`, `@Holds(0 < n)`.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncTypeParam<D: Dialect = Program> {
    pub plicity: Plicity,
    /// What the decorator before the binder's word states of the label Core keeps for it: Core text's, and nothing in a program. Written only before a word, so a member with none states nothing here.
    pub core: D::Fields,
    pub label: Option<String>,
    pub type_: Term<D>,
}

/// One Σ-type / struct-declaration field as written: an optional label and the field type. Shared by tuple types and `struct` declarations (the `TopStruct` fields reuse this grammar).
#[derive(Debug, Clone, PartialEq)]
pub struct TupleTypeParam<D: Dialect = Program> {
    /// What the decorator before the label states of the label Core keeps for the field's binder: Core text's, and nothing in a program. Written only before a label.
    pub core: D::Fields,
    /// The written label, carrying the span of the word alone, or `None` for a positional field. Spanned because a `struct` declaration reuses this grammar and its duplicate-field refusal points at the label that arrived second, as every other duplicate refusal in this stage does.
    pub label: Option<Label>,
    /// `Some` for the signature sugar `label(params) -> type_` — the written parameter list, kept verbatim so the printer round-trips it. `into_core` undoes the sugar, lowering the field as `label : (params) -> type_` (see `TupleTypeParam::desugared_type`). Always paired with a label.
    pub func_params: Option<Vec<FuncTypeParam<D>>>,
    pub type_: Term<D>,
}

impl<D: Dialect> TupleTypeParam<D> {
    /// The field's type with the signature sugar undone: the written type when the field is plain, the Π-type `(params) -> type_` when it was written `label(params) -> type_`.
    pub fn desugared_type(&self) -> Term<D> {
        match &self.func_params {
            Some(params) => signature_type(self.label.as_ref(), params, &self.type_),
            None => self.type_.clone(),
        }
    }
}

/// The Π-type a signature written `label(params) -> output` stands for, placed from its label to its result type where both were written: a report about the type stands on the signature its reader wrote, as one about a plain field's type stands on that type.
pub fn signature_type<D: Dialect>(
    label: Option<&Label>,
    params: &[FuncTypeParam<D>],
    output: &Term<D>,
) -> Term<D> {
    let undone: Term<D> = Subterm::FuncType(FuncType {
        params: params.to_vec(),
        output: output.clone(),
    })
    .into();

    match (label.and_then(Label::span), output.span()) {
        (Some(label), Some(output)) => undone.with_span(Span::new(
            Arc::clone(&label.source),
            label.start,
            output.end,
        )),
        _ => undone,
    }
}

/// A Π-/function type `(x : A, @y : B) -> C(x)`: a dependent telescope of binders (see [`FuncTypeParam`]) and the output type, which may mention any named binder.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncType<D: Dialect = Program> {
    pub params: Vec<FuncTypeParam<D>>,
    pub output: Term<D>,
}

/// One lambda binder as written: its mark, the binder pattern, and an optional domain annotation. The mark applies to the outer function slot the parameter occupies, whatever the pattern shape — a `_`, a tuple, or a struct pattern claims a slot of that plicity just as a plain name does. A `use` member is written `use _` and nothing else: the wildcard holds its place and the expected function type states it. Only the definition sugar's own lambda (`func_sugar_params`) carries an annotation beside the witness mark, and no source spells that lambda.
#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam<D: Dialect = Program> {
    pub plicity: Plicity,
    pub pattern: Pattern<D>,
    /// `None` is the surface `(x) => …` form, sugar for `(x : _) => …`; it lowers to a hole, solved by `curios_elab::elaborate` against the expected function type when the lambda is checked, or synthesized from the annotation when inferred.
    pub annotation: Option<Term<D>>,
}

/// A lambda `(x, @A, use _, (a, b) : P) => body`. Each parameter retains its plicity mark (see [`FuncParam`]); domain annotations are optional and parameters may be compound patterns — the field doc details how each lowers. A compound (tuple/struct) pattern desugars at lowering into a fresh core binder plus a projection-`let` chain — see [`Pattern`].
#[derive(Debug, Clone, PartialEq)]
pub struct Func<D: Dialect = Program> {
    pub params: Vec<FuncParam<D>>,
    pub body: Term<D>,
}

/// One call-site argument: the term with its written mark — `use` fills a witness slot, `@` an implicit one, a plain term an explicit one. The same pairing `curios-core`'s `Argument` carries, spelled over surface terms; one vector of these rather than two parallel ones, so a mark can never drift out of correspondence with its term.
#[derive(Debug, Clone, PartialEq)]
pub struct Argument<D: Dialect = Program> {
    pub term: Term<D>,
    pub plicity: Plicity,
}

/// The marks lower to core untouched — `into_core` is type-blind and cannot match them to slots.
#[derive(Debug, Clone, PartialEq)]
pub struct Apply<D: Dialect = Program> {
    pub head: Term<D>,
    pub arguments: Vec<Argument<D>>,
}

/// A Σ-/tuple type `(fst : A, snd : B(fst))`: a dependent telescope of fields — a later field's type may mention an earlier field's label. Its field grammar ([`TupleTypeParam`]) is also what `struct` declarations reuse.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType<D: Dialect = Program> {
    pub fields: Vec<TupleTypeParam<D>>,
}

/// One tuple-literal / struct-literal field as written: an optional name annotation (`(status = 0, handle = h)`) and the field value. Names are checked positionally against the expected tuple type's labels at elaboration and never survive past it.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleField<D: Dialect = Program> {
    pub label: Option<String>,
    /// `Some` for the definition sugar `label(params) = value` — the written parameter list, each a lambda's parameter, kept verbatim so the printer round-trips it. `into_core` undoes the sugar, lowering the field as `label = (params) => value` (see `TupleField::desugared_value`). Always paired with a label.
    pub func_params: Option<Vec<FuncParam<D>>>,
    pub value: Term<D>,
}

impl<D: Dialect> TupleField<D> {
    /// The field's value with the definition sugar undone: the written value when the field is plain, the lambda `(params) => value` when it was written `label(params) = value`.
    pub fn desugared_value(&self) -> Term<D> {
        match &self.func_params {
            Some(params) => Subterm::Func(Func {
                params: params.clone(),
                body: self.value.clone(),
            })
            .into(),
            None => self.value.clone(),
        }
    }
}

/// A tuple / anonymous-record literal `(a, snd = b)` — see [`TupleField`] for the per-field grammar. Struct literals do not reuse this node: a named head is a [`StructLit`], validated against its declared type.
#[derive(Debug, Clone, PartialEq)]
pub struct Tuple<D: Dialect = Program> {
    pub fields: Vec<TupleField<D>>,
}

/// A binder pattern at `let`, lambda-parameter, or function-definition-sugar parameter position: a plain name, or a tuple/struct destructuring that desugars — at lowering, in `into_core` — into a fresh synthetic binder plus a chain of ordinary projection `let`s, exactly what a person would hand-write. Always irrefutable: unlike a match-arm pattern, there is no constructor-tag case, since these binder sites never dispatch on shape — a tuple/struct value always has exactly one shape.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<D: Dialect = Program> {
    /// A name, or `_`, which holds the position and binds nothing. A member with no binder at all — a `use` premise — is not a pattern: the site that admits one holds it apart ([`FuncSugarParam::Premise`]).
    Binder(Binder<D>),
    Tuple(TuplePattern<D>),
    /// A structure's fields under the name the structure is written by, kept as written for the lowering to resolve: the pattern is held to the structure its binder is.
    Struct {
        head: Name,
        fields: Vec<PatternField<D>>,
    },
}

/// A tuple pattern's fields, and where its parentheses are written: what a refusal of its shape points at, since `()` has no leaf to point at. A location, as a [`Term`]'s span is, and no part of what the pattern is.
#[derive(Debug, Clone)]
pub struct TuplePattern<D: Dialect = Program> {
    pub fields: Vec<PatternField<D>>,
    pub span: Option<Span>,
}

impl<D: Dialect> PartialEq for TuplePattern<D> {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields
    }
}

/// A binder's word as written, under what the decorator before it states of the label Core keeps for it — where the binder sits among its declaration's written binders and whether it binds a proof, `#{written = 2, proof = true} h`: Core text's, and nothing in a program, whose binder is its word alone. The word is the label's hint; the decorator states the rest of it, each field by the name Core's label gives it.
#[derive(Debug, Clone, PartialEq)]
pub struct Binder<D: Dialect = Program> {
    pub core: D::Fields,
    pub label: Label,
}

impl<D: Dialect> Deref for Binder<D> {
    type Target = Label;

    fn deref(&self) -> &Label {
        &self.label
    }
}

impl<D: Dialect> From<Label> for Binder<D> {
    /// The word alone, under no decorator.
    fn from(label: Label) -> Self {
        Self {
            core: D::Fields::default(),
            label,
        }
    }
}

impl<D: Dialect> From<&str> for Binder<D> {
    /// The word alone, under no decorator.
    fn from(word: &str) -> Self {
        Label::from(word).into()
    }
}

impl<D: Dialect> From<String> for Binder<D> {
    /// The word alone, under no decorator.
    fn from(word: String) -> Self {
        Label::from(word).into()
    }
}

/// One tuple-pattern / struct-pattern field: a labeled sub-pattern (`label = pattern`) or a bare positional one. The literal mirror of [`TupleField`] with `Term` replaced by `Pattern` in the value slot — construction and destructuring read as inverses of each other.
#[derive(Debug, Clone, PartialEq)]
pub struct PatternField<D: Dialect = Program> {
    pub label: Option<String>,
    pub value: Pattern<D>,
}

/// A projection names its field either positionally (`p.0`) or by the tuple type's label (`p.status`). Labels are resolved to positions during core elaboration; they never survive past it.
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Index(usize),
    Label(String),
}

/// A projection `head.field` out of a tuple/struct value — positional (`p.0`) or by label (`p.status`), see [`Field`].
#[derive(Debug, Clone, PartialEq)]
pub struct Proj<D: Dialect = Program> {
    pub head: Term<D>,
    pub field: Field,
}

/// One struct-literal entry: a plain field (the tuple-literal field grammar — `fst = a` or positional), an `@` field written under its mark in the same grammar (`@ok = proof`, `@proof`, or `@_` to hold its place), a `use <term>` fill for a concept's next `use`-marked field position (mirroring call-site witness arguments; only meaningful when the head is a concept, enforced at core elaboration), or a `..base` spread copying every unwritten plain field from `base` (legal only first and at most once, also enforced at core elaboration).
#[derive(Debug, Clone, PartialEq)]
pub enum StructLitEntry<D: Dialect = Program> {
    Field(TupleField<D>),
    Implicit(TupleField<D>),
    Use(Term<D>),
    Spread(Term<D>),
}

/// A struct literal: a head naming the struct type (`Pair`, possibly applied — `Pair(Nat, Bytes)` / `Pair(Nat, ?)` — to pin the parameters) followed by a brace of entries. `params` is the head's argument list where one was written — `None` for the bare-name head, and the empty list for `Box()`, the application with nothing written; holes appear as `?` terms. An applied head is an application of the type former, so its arguments take the marks a call's do: `Box(@Nat) { … }`. Plain entries are validated positionally against the declared non-`use` labels at core elaboration; a concept's `use`-marked fields are filled by `use <term>` entries or, when omitted, by witness resolution.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLit<D: Dialect = Program> {
    pub head: Name,
    pub params: Option<Vec<Argument<D>>>,
    pub entries: Vec<StructLitEntry<D>>,
}

/// A headed dependent elimination: one scrutinee and arms of arbitrary (nested, across constructors, tuples, structs, and the four literal-carrier leaves) [`MatchPattern`]s, handed to Core as written (`into_core::match_compile`) and compiled by the pattern-matrix scheme of `curios-elab`'s matrix compiler into the same single-level core match/projection forms a person would get from hand-nesting matches.
#[derive(Debug, Clone, PartialEq)]
pub struct Match<D: Dialect = Program> {
    pub head: Term<D>,
    /// The arms, in source order. Each pairs a (possibly nested, across constructors/tuples/structs — see [`MatchPattern`]) pattern with its body; zero arms is legal (a vacuous elimination, e.g. of `False`). Arms have no priority: two may match one value only where the first is a case of the other, and elaboration rejects an arm no value reaches and a value no arm matches (see `curios-elab`'s matrix compiler).
    pub arms: Vec<MatrixArm<D>>,
}

/// One arm of a [`Match`]: `| pattern | … , guard … => body`, its alternatives in the order they are written, the guards after them and the one body they share. Compiled by `curios-elab`'s matrix compiler into the single-level core match/projection forms — exactly what a person would get from hand-nesting matches (see its doc comment). A flat, unnested arm (`tag(x, y) => body`, i.e. every argument a plain [`MatchPattern::Binder`]) comes to one core arm binding the written names.
#[derive(Debug, Clone, PartialEq)]
pub struct MatrixArm<D: Dialect = Program> {
    /// At least one.
    pub alternatives: Vec<Alternative<D>>,
    /// The guards after the alternatives, each read under every alternative.
    pub guards: Vec<Guard<D>>,
    pub body: Term<D>,
}

impl<D: Dialect> MatrixArm<D> {
    /// The arm of one pattern, written nowhere: an arm the compiler built.
    pub fn of(pattern: MatchPattern<D>, body: Term<D>) -> Self {
        Self {
            alternatives: vec![Alternative {
                pattern,
                span: None,
            }],
            guards: Vec::new(),
            body,
        }
    }
}

/// A guard an arm writes after its patterns, or one of those an arm of a [`HeadlessMatch`] is: a `Bool`, which passes where it is `true`, or `pattern = value`, which passes where the value matches the pattern and binds its names for the guards after it and the arm's body.
#[derive(Debug, Clone, PartialEq)]
pub enum Guard<D: Dialect = Program> {
    Holds(Term<D>),
    Binds {
        pattern: Box<Alternative<D>>,
        value: Term<D>,
    },
}

/// A match with no head: guards alone, `match | guard, … => body | … | _ => default end`, the arms tried from the top and the default required. The match of no columns, lowered as a match over `()` whose arms write `_` (`into_core::match_compile`), so `curios-elab`'s matrix compiler reads its arms where a headed match's meet theirs.
#[derive(Debug, Clone, PartialEq)]
pub struct HeadlessMatch<D: Dialect = Program> {
    pub arms: Vec<HeadlessArm<D>>,
    pub default: Term<D>,
}

impl<D: Dialect> HeadlessMatch<D> {
    /// The match with no head a match over `()` with `arms` was lowered from: every arm one `_` that names nothing, every one guarded but the last, which has no guard. `None` where the arms are no such match's.
    pub fn of_arms(arms: &[MatrixArm<D>]) -> Option<Self> {
        let unnamed = |arm: &MatrixArm<D>| {
            matches!(
                arm.alternatives.as_slice(),
                [Alternative { pattern: MatchPattern::Binder(name), .. }] if name.as_str() == "_"
            )
        };
        let (default, guarded) = arms.split_last()?;
        if !unnamed(default)
            || !default.guards.is_empty()
            || !guarded
                .iter()
                .all(|arm| unnamed(arm) && !arm.guards.is_empty())
        {
            return None;
        }

        Some(Self {
            arms: guarded
                .iter()
                .map(|arm| HeadlessArm {
                    guards: arm.guards.clone(),
                    body: arm.body.clone(),
                })
                .collect(),
            default: default.body.clone(),
        })
    }
}

/// One arm of a [`HeadlessMatch`]: its guards, at least one, and its body.
#[derive(Debug, Clone, PartialEq)]
pub struct HeadlessArm<D: Dialect = Program> {
    pub guards: Vec<Guard<D>>,
    pub body: Term<D>,
}

/// One of the patterns an arm writes before its `=>`: the arm is taken by a value any of them matches, its names bound as that one binds them.
#[derive(Debug, Clone)]
pub struct Alternative<D: Dialect = Program> {
    pub pattern: MatchPattern<D>,
    /// Where the pattern is written, or `None` for one the compiler built. A location, as a [`Term`]'s span is, and no part of what the alternative is.
    pub span: Option<Span>,
}

impl<D: Dialect> PartialEq for Alternative<D> {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

/// A match-arm pattern: genuinely refutable, unlike [`Pattern`] (which is always irrefutable — see its own doc comment). Distinct from `Pattern` on purpose, even though `Tuple`/`Struct` mirror its field grammar exactly: a `let`/lambda/function-sugar binder site never dispatches on shape, but a match arm's whole point is dispatching on shape, so the two must never be conflated.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern<D: Dialect = Program> {
    /// A plain name (or `_`) — never splits a column by itself: where another row takes the column apart, a row that writes one is under every case of it (see `curios-elab`'s matrix compiler).
    Binder(Binder<D>),
    /// An inductive constructor tag applied to sub-patterns — positional (constructors have no field labels in this language). Each argument retains its plicity mark (`@`/`use`): a payload slot the constructor declared implicit must be matched with `@`. The marks lower to the Core arm untouched; core elaboration checks them against the constructor's canonical payload plicities.
    Variant {
        tag: String,
        args: Vec<(Plicity, MatchPattern<D>)>,
    },
    /// A tuple pattern — field grammar mirrors [`PatternField`] exactly.
    Tuple(Vec<MatchPatternField<D>>),
    /// A struct pattern — the same labeled/punned/positional grammar as struct literals and the irrefutable [`Pattern::Struct`], not the positional constructor-call shape (structs have field labels; inductive constructors don't).
    Struct {
        head: Name,
        fields: Vec<MatchPatternField<D>>,
    },
    /// A `Bool` literal leaf: `true` or `false`.
    Bool(bool),
    /// A character literal leaf — a dispatch case spelled by its scalar value, at a `Char`, a `Nat` or a `Byte`, printed back as written.
    Char(CharLit),
    /// A string literal leaf — a `Str` taken apart by its bytes — printed back in the spelling it was written in.
    Str(StrLit),
    /// A nested `Nat` literal leaf — the `0`/`n + 1` (case split) or bare literal (dispatch) shapes a headed `Nat` match takes.
    Nat(NatPattern<D>),
    /// A run over a list: `[]`, `[a, b]` or `[a, b, ..rest]`.
    List(Elements<D>),
    /// A run over a packed sequence: `x[]`, `x[a, b]` or `x[a, b, ..rest]`.
    Bytes(Elements<D>),
}

impl<D: Dialect> MatchPattern<D> {
    /// Whether this leaf is a genuine single-dispatch shape (`Variant`/`Bool`/`Char`/`Nat`/`List`/`Bytes`), as opposed to `Binder`/`Tuple`/`Struct`, which never produce a core `Match` node at all (a binder never splits, a tuple/struct explodes into projections). The matrix compiler (`into_core::match_compile`) reads a final `_` after arms that each dispatch as their default.
    pub fn is_dispatchable(&self) -> bool {
        !matches!(
            self,
            MatchPattern::Binder(_) | MatchPattern::Tuple(_) | MatchPattern::Struct { .. }
        )
    }
}

/// The two shapes a nested `Nat` leaf can take — see [`MatchPattern::Nat`].
#[derive(Debug, Clone, PartialEq)]
pub enum NatPattern<D: Dialect = Program> {
    /// The `0` leaf.
    Zero,
    /// The `pred + 1` leaf. `pred_label` is a binder, never a pattern: one there would spell again what a literal spells, `0 + 1` the `1`.
    Succ { pred_label: Binder<D> },
    /// A literal-dispatch leaf `k` — matched by value, peeling no successor. Always `k >= 1`: the numeral `0` is [`NatPattern::Zero`], never `Lit(0)`, so a `Nat` has one canonical leaf per value. A column of `Lit` (and possibly `Zero`) leaves with no `Succ` is value dispatch, compiled to a `Cases::Switch` with a mandatory default rather than the `Nat` eliminator (`curios-elab`'s matrix compiler).
    ///
    /// Carries the numeral whole, as every stage down to the lowering that builds a branch table does: a key is as unbounded as the value it is compared with, and one no table indexes is decided by equality there (`curios-anf`'s `lower_switch`). Narrowed here, a numeral past a machine word would backtrack past every leaf, so the arm would be reported as holding no pattern where its key is one.
    ///
    /// The radix it was written in rides along, as a term's numeral keeps it, so `| 0x10 =>` prints back as written; dispatch reads the value alone.
    Lit(NatLiteral),
}

/// A sequence pattern's elements as the literal that builds the value writes them, each a pattern taken from the front — see [`MatchPattern::List`] and [`MatchPattern::Bytes`]. An open run writes one element at least: one of none would be the binder its rest is, so the tree holds no `[..rest]`.
#[derive(Debug, Clone, PartialEq)]
pub enum Elements<D: Dialect = Program> {
    /// `[]` or `[a, b]`: a sequence of exactly these elements.
    Closed(Vec<MatchPattern<D>>),
    /// `[a, b, ..rest]`: a sequence these elements begin, and what follows them, which its rest binds.
    Open {
        first: Box<MatchPattern<D>>,
        then: Vec<MatchPattern<D>>,
        rest: Binder<D>,
    },
}

impl<D: Dialect> Elements<D> {
    /// Every element, in the order the run writes them.
    pub fn elements(&self) -> impl Iterator<Item = &MatchPattern<D>> {
        let (first, then) = match self {
            Elements::Closed(elements) => (None, elements.as_slice()),
            Elements::Open { first, then, .. } => (Some(&**first), then.as_slice()),
        };

        first.into_iter().chain(then)
    }

    /// `[head, ..tail]`, a binder for each: the run a case split's cons arm is written as.
    pub fn cons(head: Binder<D>, tail: Binder<D>) -> Self {
        Elements::Open {
            first: Box::new(MatchPattern::Binder(head)),
            then: Vec::new(),
            rest: tail,
        }
    }

    /// The binders of a run written as [`Elements::cons`] writes one, and nothing for any other.
    pub fn as_cons(&self) -> Option<(&Binder<D>, &Binder<D>)> {
        match self {
            Elements::Open { first, then, rest } if then.is_empty() => match &**first {
                MatchPattern::Binder(head) => Some((head, rest)),
                _ => None,
            },
            _ => None,
        }
    }
}

/// One tuple-pattern / struct-pattern field in a [`MatchPattern`]: a labeled sub-pattern (`label = pattern`) or a bare positional one. The literal mirror of [`PatternField`] with `Pattern` replaced by `MatchPattern`.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchPatternField<D: Dialect = Program> {
    pub label: Option<String>,
    pub value: MatchPattern<D>,
}

/// One parameter of the function-definition sugar `let f(x : T, …) -> R = body`. A plain-name (`Pattern::Binder`) binder flows into both the Π-type binder and the lambda parameter. A compound (tuple/struct) pattern has no single name to give the Π-type binder, so it lowers to an *anonymous* Π-binder (see `LetSignature::type_`) — its destructured leaves are visible only in the function's value body, never in a later parameter's type or the output type.
///
/// A member is one of two things, and the tree holds no third: a premise has no binder, and a plain or `@` member always has one, `_` where the body never names it — `let f(n: Nat, @_: Holds(0 < n))`.
#[derive(Debug, Clone, PartialEq)]
pub enum FuncSugarParam<D: Dialect = Program> {
    /// `use Concept(args)`: a witness the body reaches by resolution.
    Premise(Term<D>),
    /// `binder: type`, plain or under `@`.
    Bound {
        implicit: bool,
        binder: Pattern<D>,
        type_: Term<D>,
    },
}

impl<D: Dialect> FuncSugarParam<D> {
    /// The mark the member is written under.
    pub fn plicity(&self) -> Plicity {
        match self {
            FuncSugarParam::Premise(_) => Plicity::Witness,
            FuncSugarParam::Bound { implicit: true, .. } => Plicity::Implicit,
            FuncSugarParam::Bound {
                implicit: false, ..
            } => Plicity::Explicit,
        }
    }

    /// The type the member states.
    pub fn type_(&self) -> &Term<D> {
        match self {
            FuncSugarParam::Premise(type_) | FuncSugarParam::Bound { type_, .. } => type_,
        }
    }
}

/// The right-hand side shared by every `let`-like binding site (local [`Let`], top-level [`TopLet`](crate::TopLet)): a plain annotated body, or the function-definition sugar `f(x : T, …) -> R = body`, kept verbatim so the printer round-trips it. Lowering undoes the sugar — the type becomes a Π-type, the body a lambda binding every parameter — over one set of binders, so a parameter is the same binder in both; the crate-internal `type_()`/`body()` accessors spell the two halves as surface terms for a site that lowers them apart.
#[derive(Debug, Clone, PartialEq)]
pub enum LetSignature<D: Dialect = Program> {
    Name {
        /// `None` only for a local `let x = body` (the parser forbids omitting the type for top-level `let` and for every member after `and`). It lowers to a hole so the core elaborator infers the body's type.
        type_: Option<Term<D>>,
        body: Term<D>,
    },
    Func {
        params: Vec<FuncSugarParam<D>>,
        output: Term<D>,
        body: Term<D>,
    },
}

/// The function-definition sugar's telescope as the parameters of the Π-type it declares. A plain-name parameter names its Π-binder, so a later domain or the output may depend on it. A compound pattern has no single name to give the binder and a premise has none at all, so each lowers anonymously (`None`) — already a fully legal Π-binder shape (e.g. an unlabeled `(T) -> R` parameter). Shared by a `let`'s signature and a `test`'s, which is the same sugar with its output fixed.
pub fn func_sugar_type_params<D: Dialect>(params: &[FuncSugarParam<D>]) -> Vec<FuncTypeParam<D>> {
    params
        .iter()
        .map(|param| FuncTypeParam {
            plicity: param.plicity(),
            core: D::Fields::default(),
            label: match param {
                FuncSugarParam::Bound {
                    binder: Pattern::Binder(name),
                    ..
                } => Some(name.to_string()),
                FuncSugarParam::Bound {
                    binder: Pattern::Tuple(_) | Pattern::Struct { .. },
                    ..
                }
                | FuncSugarParam::Premise(_) => None,
            },
            type_: param.type_().clone(),
        })
        .collect()
}

/// The function-definition sugar's body as the lambda binding every parameter. Each binder copies its plicity: the generated function value carries the same marks its signature does, so a written `@`/`use` in the sugar is checked against the slot it claims rather than silently dropped.
pub fn func_sugar_lambda<D: Dialect>(params: &[FuncSugarParam<D>], body: &Term<D>) -> Term<D> {
    Subterm::Func(Func {
        params: func_sugar_params(params),
        body: body.clone(),
    })
    .into()
}

/// The function-definition sugar's telescope as the parameters of the lambda its body is, each annotated with its written type. A premise takes the wildcard, which holds its slot and binds nothing.
pub fn func_sugar_params<D: Dialect>(params: &[FuncSugarParam<D>]) -> Vec<FuncParam<D>> {
    params
        .iter()
        .map(|param| FuncParam {
            plicity: param.plicity(),
            pattern: match param {
                FuncSugarParam::Bound { binder, .. } => binder.clone(),
                FuncSugarParam::Premise(_) => Pattern::Binder("_".into()),
            },
            annotation: Some(param.type_().clone()),
        })
        .collect()
}

impl<D: Dialect> LetSignature<D> {
    pub fn type_(&self) -> Term<D> {
        match self {
            LetSignature::Name {
                type_: Some(type_), ..
            } => type_.clone(),
            // An omitted (local-only) annotation lowers to a hole, so the core elaborator infers the body's type; identical to writing `: _`.
            LetSignature::Name { type_: None, .. } => Subterm::Hole.into(),
            LetSignature::Func { params, output, .. } => Subterm::FuncType(FuncType {
                params: func_sugar_type_params(params),
                output: output.clone(),
            })
            .into(),
        }
    }

    pub fn body(&self) -> Term<D> {
        match self {
            LetSignature::Name { body, .. } => body.clone(),
            LetSignature::Func { params, body, .. } => func_sugar_lambda(params, body),
        }
    }
}

/// One binding of a [`Let`] block: a bound pattern and its signature.
#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding<D: Dialect = Program> {
    /// The bound pattern — `let x = …`, `let (x, y) = …`, or the `f` in the `let f(…) -> R = …` function-definition sugar. A compound pattern here is grammatically legal for the function-sugar form too, but pointless: a function's own binding is never itself a tuple/struct value, so destructuring it fails the same "not a tuple/struct" projection type error a hand-written misuse would (no special-casing needed).
    pub binder: Pattern<D>,
    pub signature: LetSignature<D>,
}

/// One local `let` statement: a binding, or a `let … and …;` group of them. A group's members are in scope of one another's types and bodies; a lone binding is in scope of its own, which is what lets it recurse. Every member after the first is a plain label with a mandatory type — the parser makes it so — and lowering holds the first member to the same shape once the group has more than one member or mentions itself, since a recursive reference's type cannot be inferred from a body that mentions it and a pattern binds no name its own value could use.
#[derive(Debug, Clone, PartialEq)]
pub struct LetGroup<D: Dialect = Program> {
    pub members: Vec<LetBinding<D>>,
}

/// A local `let` block: a straight-line run of `groups`, each in scope for the groups after it and for `tail`. `let` is the only position where a type annotation may be omitted (`LetSignature::Name` with `type_: None`) — top-level `let` always carries one, and so does every member of a group. A whole run is one node, not a nest, so cloning/lowering it is a loop over `groups` rather than one native stack frame per binding.
#[derive(Debug, Clone, PartialEq)]
pub struct Let<D: Dialect = Program> {
    pub groups: Vec<LetGroup<D>>,
    pub tail: Term<D>,
}

/// A surface infix application `left <op> right`, produced by the precedence-climbing parser. Lowered verbatim to a `core::Infix` and resolved during elaboration to a projection off the witness of its `/std/ops` concept (the operand types are not yet known at lowering).
#[derive(Debug, Clone, PartialEq)]
pub struct Infix<D: Dialect = Program> {
    pub op: InfixOp,
    pub left: Term<D>,
    pub right: Term<D>,
}

/// A surface polymorphic numeric literal: an integer `magnitude` with an optional written sign. Its concrete type (`Nat`/`Int`/`Flt`) is chosen during elaboration. The `radix` is retained only so the printer round-trips the written form (`0xC2` back to `0xC2`); lowering to core drops it. Decimal literals are not `NumLit`; they parse to `Literal::Flt`.
#[derive(Debug, Clone, PartialEq)]
pub struct NumLit {
    pub magnitude: Natural,
    pub radix: Radix,
    pub sign: Sign,
}

/// The term grammar proper, one variant per surface form. Spanless by design — a location rides on the wrapping [`Term`] — so `PartialEq` compares structure alone; bare terms are built via `From<Subterm> for Term`. Most variants lower one-to-one onto a core counterpart in `into_core`; the ones that exist only in the surface language are documented on their variants below.
#[derive(Debug, Clone, PartialEq)]
pub enum Subterm<D: Dialect = Program> {
    Type,
    Prop,
    /// A sort its declaration leaves to each caller, `Prop` at one use and `Type` at another: written in the signature of a top-level `let`, `induct` or `struct`, where a use states it.
    Sort,
    /// A literal as written (see [`Literal`]).
    Literal(Literal<D>),
    /// One of core's intrinsics over surface operands (see [`Intrinsic`]). Never parsed — the `/sys` roster plants it — and lowered by lowering its operands.
    Intrinsic(Intrinsic<D>),
    FuncType(FuncType<D>),
    Func(Func<D>),
    Apply(Apply<D>),
    TupleType(TupleType<D>),
    Tuple(Tuple<D>),
    Proj(Proj<D>),
    StructLit(StructLit<D>),
    Match(Match<D>),
    /// A match with no head (see [`HeadlessMatch`]).
    HeadlessMatch(HeadlessMatch<D>),
    Let(Let<D>),
    /// A postfix bang `e!`: extracts the result of monadic action `e` inline. The operand is the action whose result is bound. The `into_core` pass hoists each bang to the top of its enclosing region (a value body, re-rooted at lambda bodies, match arms, and recursive-group members) and sequences it through `/std/Monad/bind`, whose `use` binder resolves the `Monad` witness from the action's type. Exists only between parsing and `into_core`, which eliminates it before core elaboration; a bang in a type is rejected.
    Bang(Term<D>),
    Name(Name),
    /// A silent inference hole: a placeholder elaborated to a fresh metavariable whose solution zonk splices in without comment. Desugar-only — the parser mints [`Subterm::Goal`] for a written `?`; `Hole` stands in where a desugar omits a term (an unannotated local `let`'s type). Carries no payload — its span rides on the wrapping [`Term`].
    Hole,
    /// A held place, `_` after a mark: a hidden argument, `@_` or `use _`, or a literal's hidden entry, left to the elaborator as one left out is. Only the argument and literal-entry grammars produce it, so a `_` read anywhere else is no term. Carries no payload — its span rides on the wrapping [`Term`].
    Held,
    /// A written goal `?`: elaborated to a metavariable like [`Subterm::Hole`], but marked so zonk *reports* what elaboration determined for it (scope, type, and solution, if any) instead of splicing silently — writing `?` always fails compilation with that report. Minted only by the parser; carries no payload — its span rides on the wrapping [`Term`].
    Goal,
    /// The body of a derived witness, `satisfy C(T) { .. }`: lowered to the `Derive` elaboration transient, which the concept's derivation expands or refuses. Desugar-only — minted by the witness lowering, never parsed — so that the declaration's telescope wraps it exactly as it wraps a written body. Carries no payload; its span rides on the wrapping [`Term`].
    Derive,
    /// A literal whose value is synthesized from the registry rather than lowered to a core intrinsic (see [`ProofLiteral`]). The lowerer runs a meta-emitter on it instead of `intrinsic()`.
    ProofLiteral(ProofLiteral),
    /// An infix operator application `left <op> right` (see [`Infix`]).
    Infix(Infix<D>),
    /// A polymorphic numeric literal (see [`NumLit`]).
    NumLit(NumLit),
    /// A form one dialect writes and the other does not: Core text's [`CoreForm`](crate::CoreForm), and in a program none.
    Core(D::Form),
}

/// The literals the lowerer desugars to a proof-carrying construction: a character becomes a proof-carrying `/std/Char`, and a string becomes a proof-carrying `/std/Str`. Held as a dedicated [`Subterm`] variant (not an `Intrinsic`) because the result is a core term, never a core intrinsic.
#[derive(Debug, Clone, PartialEq)]
pub enum ProofLiteral {
    Char(CharLit),
    Str(StrLit),
}

/// The text a literal was written as, so the printer writes it back as it was written: `1.0e9` stays `1.0e9`, and an escape stays the escape its author chose for a character that is hard to see or to type. Empty for a literal the compiler built, which prints in its canonical spelling.
///
/// Presentational, as a span is: no part of what the literal is, so any two are equal and a literal compares by its value alone — which is what lets the formatter's verification read back a literal whose invisible character it spelled as an escape.
#[derive(Debug, Clone, Default)]
pub struct Written(Option<Rc<str>>);

impl Written {
    /// `text`, as its literal was written.
    pub fn new(text: &str) -> Self {
        Self(Some(text.into()))
    }

    /// The text written, where the literal was read from one.
    pub fn text(&self) -> Option<&str> {
        self.0.as_deref()
    }
}

impl PartialEq for Written {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

/// A character literal's value with the text written between its quotes.
#[derive(Debug, Clone, PartialEq)]
pub struct CharLit {
    pub value: char,
    pub written: Written,
}

impl From<char> for CharLit {
    fn from(value: char) -> Self {
        Self {
            value,
            written: Written::default(),
        }
    }
}

/// A `Flt` literal's value with the text it was written as, its sign and exponent among it.
#[derive(Debug, Clone, PartialEq)]
pub struct FltLit {
    pub value: Floating,
    pub written: Written,
}

impl From<Floating> for FltLit {
    fn from(value: Floating) -> Self {
        Self {
            value,
            written: Written::default(),
        }
    }
}

/// A string literal's value with the spelling it was written in, so the printer rounds a block back to a block and writes the text back as written. Purely presentational, as a numeral's [`Radix`] is: dropped at lowering, where both spellings are the same `/std/Str`.
#[derive(Debug, Clone, PartialEq)]
pub struct StrLit {
    pub value: String,
    /// Written as a block — `"""`, a newline, the lines, a newline and `"""` — rather than on one line.
    pub block: bool,
    /// The text between the quotes of a one-line literal, and a block's lines past the indentation they share with its closer, joined by newlines.
    pub written: Written,
}

impl StrLit {
    /// The one-line spelling of `value`.
    pub fn line(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            block: false,
            written: Written::default(),
        }
    }

    /// The block spelling of `value`.
    pub fn block(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            block: true,
            written: Written::default(),
        }
    }

    /// The literal, as the text it was written as.
    pub fn written(self, text: &str) -> Self {
        Self {
            written: Written::new(text),
            ..self
        }
    }
}
