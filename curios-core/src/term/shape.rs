//! The payload each [`Subterm`] variant carries: one named type per shape, so a variant's fields are read by name at every construction and match rather than by position.
//!
//! Nothing here judges or reduces. The types are plain records with the binder discipline spelled in their field types — a [`Scope`] where one binder is bound, a [`Telescope`] where several are — and the handful of impls are the operations that discipline forces: [`RecGroup`]'s member arithmetic, [`InductArm`]'s and [`LetBinding`]'s accessors, the types a [`Carrier`]'s cons arm binds, and the two hand-written [`TupleType`] instances that make an anonymous product compare by its fields.

use {
    crate::{
        Atom, Bound, CalleeId, Free, Global, Intrinsic, Label, Level, Many, Matrix, MetavarSpine,
        Quality, Scope, Subterm, Telescope, Term, Three, Two, Universes, Var, Visit,
        project_erased_universes,
    },
    curios_abi::ForeignFunction,
    curios_common::{ConceptField, InfixOp, Mint, Plicity, Sign},
    curios_num::Natural,
    std::{
        fmt,
        hash::{Hash, Hasher},
        rc::Rc,
        sync::Arc,
    },
};

/// An unresolved infix application `left <op> right`. Elaboration infers a shared operand type for the two sides and rebuilds the node as a concept method call (`a + b` ≙ `Add/add(a, b)`, `&&`/`||` included — see `elaborate_infix`); the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Infix {
    pub op: InfixOp,
    pub left: Term,
    pub right: Term,
}

/// A polymorphic literal, resolved to a concrete carrier by `elaborate_num_lit` once the expected type is known (or defaulted by shape). Decimal literals are *not* `NumLit` — they parse straight to `Intrinsic::Flt`.
///
/// The two spellings are one transient because they share the realization machinery, and an enum because they share nothing else: a numeral has a sign and no scalar constraint, a character has a scalar (Rust's `char` guarantees it) and no sign, and neither combination's absence is a convention — it is unrepresentable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum NumLit {
    /// A numeral: candidates `Nat`/`Bool`/`Byte`/`Int`/`Flt`, defaulting by [`Sign`] — `Int` when marked, else `Nat`.
    Number { magnitude: Natural, sign: Sign },
    /// A character-spelled scalar value: candidates `Char` (the default), `Nat`, `Byte`, `Int`.
    Character(char),
}

/// A postfix `!` sequencing site, already hoisted by lowering: `action` is the sequenced description, `continuation` the rest of its region as an ordinary one-parameter function (domain a lowering-minted hole). Consumed by `elaborate_bang`, which replaces it with the `/std/Monad/bind` application — built at elaboration rather than by the lowerer, so the sequencing survives to the stage that can make type-directed decisions about it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Bang {
    pub action: Term,
    pub continuation: Term,
}

/// A term stated at a type: `term` is to inhabit `type_`. Lowering writes one where the source states a type beside a term with no binder between them — a struct literal's applied head, `Box(@Nat) { value = 1 }`, whose head is an application of the type former like any other and takes the marks one does. Consumed by `elaborate_ascribed`, which elaborates the type, checks the term against it and leaves the term.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Ascribed {
    pub term: Term,
    pub type_: Term,
}

/// A concept's method named by its path, `Show/show`: field `field` of the concept `concept`, read off the witness resolution finds. Consumed by `elaborate`, which writes it as the method's η-form at an instance of the concept's levels, `(@A, use w: Show(A), x) => w.show(x)` ([`method_spelling`](crate::method_spelling)), so a call of it is the projection off the witness the call fills: a method is its concept's field, and no definition stands between.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Method {
    pub concept: Global,
    pub field: String,
}

/// The method a registry slot names.
impl From<ConceptField> for Method {
    fn from(field: ConceptField) -> Self {
        Self {
            concept: Global::Authored(field.concept.qualifier()),
            field: field.field.to_string(),
        }
    }
}

/// A lowering-born constructor consumed by elaboration: born in `into_core`, eliminated by `elaborate`, never legitimate in reduced, converted, zonked, or erased terms, and refused at the kernel boundary. Grouping the members under one `Subterm` variant lets every post-elaboration consumer dismiss the class wholesale — one refusal arm at the kernel, one `unreachable!` in each downstream stage — so a future transient extends lowering, elaboration, and display without touching them. `Metavar` is deliberately not a member: conversion parks on metavariables and zonk consumes them, so its lifecycle is elaboration-internal rather than pre-elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Transient {
    /// An unresolved infix operator application; consumed by `elaborate_infix`.
    Infix(Infix),
    /// A polymorphic numeric literal; consumed by `elaborate_num_lit`.
    NumLit(NumLit),
    /// A postfix `!` sequencing site; consumed by `elaborate_bang`.
    Bang(Bang),
    /// A term stated at a type; consumed by `elaborate_ascribed`.
    Ascribed(Ascribed),
    /// A witness body the compiler writes — the body position of a derived `satisfy C(T) { .. }`; consumed by `elaborate_derive`, which reads the concept application it is checked against and expands it or refuses. Carries nothing: the expected type is the whole of its input.
    Derive,
    /// A match as it is written; consumed by `elaborate_matrix`, which compiles its patterns into the single-level [`Match`] forms.
    Matrix(Matrix),
    /// A declaration named as it is written, before an instance of it is chosen; consumed by `elaborate`, which writes it as an [`Instance`] at the levels it chooses, none where the declaration is monomorphic.
    Name(Global),
    /// A concept's method named by its path; consumed by `elaborate` ([`Method`]).
    Method(Method),
}

impl Transient {
    /// The direct child terms, for the structural walks that must traverse a lowered (or display-folded) term. Transients are plain data over their children — none binds a variable of its own (`Bang`'s continuation is an ordinary `Func`, which carries the binder, and so is the body of a `Matrix`'s row) — so the walks need no scope handling here.
    pub fn subterms(&self) -> impl Iterator<Item = &Term> {
        let (children, matrix) = match self {
            Transient::Infix(Infix { left, right, .. }) => ([Some(left), Some(right)], None),
            Transient::NumLit(_)
            | Transient::Derive
            | Transient::Name(_)
            | Transient::Method(_) => ([None, None], None),
            Transient::Bang(Bang {
                action,
                continuation,
            }) => ([Some(action), Some(continuation)], None),
            Transient::Ascribed(Ascribed { term, type_ }) => ([Some(term), Some(type_)], None),
            Transient::Matrix(matrix) => ([None, None], Some(matrix)),
        };
        children
            .into_iter()
            .flatten()
            .chain(matrix.into_iter().flat_map(Matrix::subterms))
    }

    /// Rebuild this transient with every child term mapped, for the structural rewrites.
    pub fn map_subterms(&self, f: &mut impl FnMut(&Term) -> Term) -> Transient {
        match self {
            Transient::Infix(Infix { op, left, right }) => Transient::Infix(Infix {
                op: *op,
                left: f(left),
                right: f(right),
            }),
            Transient::NumLit(num_lit) => Transient::NumLit(num_lit.clone()),
            Transient::Derive => Transient::Derive,
            Transient::Name(name) => Transient::Name(*name),
            Transient::Method(method) => Transient::Method(method.clone()),
            Transient::Bang(Bang {
                action,
                continuation,
            }) => Transient::Bang(Bang {
                action: f(action),
                continuation: f(continuation),
            }),
            Transient::Ascribed(Ascribed { term, type_ }) => Transient::Ascribed(Ascribed {
                term: f(term),
                type_: f(type_),
            }),
            Transient::Matrix(matrix) => Transient::Matrix(matrix.map_subterms(f)),
        }
    }
}

/// A function type is its telescope: each parameter a type under its mark, the payload the result. The marks are the telescope's own ([`Telescope`]), so they are part of the type's identity with nothing beside it to keep in step. Erasure is sort-driven (a proof or a type erases), so a function type carries no runtime-multiplicity marks of its own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct FuncType {
    pub telescope: Telescope<Term>,
}

impl FuncType {
    /// A function type over `telescope` with its members under `plicities`, one per binder: for a caller that holds the marks apart from the entries it built.
    pub fn new(telescope: Telescope<Term>, plicities: Vec<Plicity>) -> Self {
        Self {
            telescope: telescope.with_marks(&plicities),
        }
    }

    /// The marks, one per parameter, as the telescope states them.
    pub fn plicities(&self) -> Vec<Plicity> {
        self.telescope.marks()
    }
}

/// A function literal: the parameter annotations and the body as one [`Telescope`] (each entry a parameter type under its mark, the `Done` payload the body). Plicity is part of a function's identity and calling convention: a lambda carries the marks its binders were written with (before elaboration) and the complete canonical marks of its checked type (after elaboration, once omitted hidden binders are inserted). The marks are the telescope's own, so `Eq`/`Hash` read them and two lambdas differing only in a written mark never share an elaboration-cache entry.
///
/// Erasure ignores the marks; its keep/drop decisions come from the checked function type and sort information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Func {
    pub telescope: Telescope<Term>,
}

impl Func {
    /// A function literal over `telescope` with its binders under `plicities`, one per binder: for a caller that holds the marks apart from the entries it built.
    pub fn new(telescope: Telescope<Term>, plicities: Vec<Plicity>) -> Self {
        Self {
            telescope: telescope.with_marks(&plicities),
        }
    }

    /// The marks, one per binder, as the telescope states them.
    pub fn plicities(&self) -> Vec<Plicity> {
        self.telescope.marks()
    }
}

/// One call-site argument: the term with its written `@`/`use` mark. One vector of these rather than two parallel ones, so a mark can never drift out of correspondence with its term — the pairing `Cases::Induct` arms state for their bodies, applied to the spine. Core must carry the marks (rather than `into_core` resolving them) because `into_core` is type-blind: only the elaborator, holding the head's function type, can decide which binder an `@`-argument fills.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Argument {
    pub term: Term,
    pub plicity: Plicity,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Apply {
    pub head: Term,
    pub arguments: Vec<Argument>,
}

impl Apply {
    /// The argument terms alone, in application order — the dominant read; the marks ride beside them.
    pub fn params(&self) -> impl ExactSizeIterator<Item = &Term> + Clone + '_ {
        self.arguments.iter().map(|argument| &argument.term)
    }

    /// The marks alone, paralleling [`Apply::params`] by construction.
    pub fn plicities(&self) -> impl ExactSizeIterator<Item = Plicity> + Clone + '_ {
        self.arguments.iter().map(|argument| argument.plicity)
    }
}

/// A dependent product (Σ-type). Erasure is sort-driven: a proof or type-valued field is a *subset type* witness — dropped at erasure, leaving the relevant fields (and collapsing to the bare field when only one remains).
///
/// Unlike binder hints elsewhere, field labels are the target of `.label` resolution during elaboration, so they are part of the type's identity: `Eq`/`Hash` reassert them on top of the label-blind [`Telescope`] identity. Otherwise the reduction memo could hand elaboration a twin type whose labels differ, and a well-typed projection would fail to resolve.
#[derive(Debug, Clone, Eq)]
#[curios_archive::archived]
pub struct TupleType {
    pub telescope: Telescope<()>,
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.telescope == other.telescope && self.telescope.labels() == other.telescope.labels()
    }
}

impl Hash for TupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.telescope.hash(state);
        self.telescope.labels().hash(state);
    }
}

/// `names` carries the literal's written field names (`(status = 0, …)`) from `into_core` to elaboration, which checks them against the expected tuple type's labels and rebuilds the literal name-free. Empty means "no names written" — the invariant for every internally-built and post-elaboration tuple.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Tuple {
    pub fields: Vec<Term>,
    pub names: Vec<Option<String>>,
}

/// A projection's field is a slot of the head's field telescope in every post-elaboration term, said with what the head is a value of: a tuple's slot stands alone, and a structure's names the structure, which the kernel holds to the head's type. A projection so says which field it reads to a reader of the term alone, who asks its head's type of nobody: a hidden field's label, a plain field's place among the plain ones.
///
/// The other two forms are what an author writes, and exist only between `into_core` and `elaborate`, which resolves each against the head's type and rebuilds it as a slot: a `Label` names its field, and a `Position` — a written `.N`, or a positional field of a pattern — counts the plain fields, a hidden one taking no position.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Field {
    /// A slot of a tuple's type.
    Tuple(usize),
    /// A slot of the fields the structure named declares.
    Struct(Global, usize),
    Label(String),
    Position(usize),
}

impl Field {
    /// The slot an elaborated projection reads, whatever it is a slot of. `None` for a written form, which names no slot yet.
    pub fn slot(&self) -> Option<usize> {
        match self {
            Field::Tuple(index) | Field::Struct(_, index) => Some(*index),
            Field::Label(_) | Field::Position(_) => None,
        }
    }

    /// The slot `index` of what this one is a slot of: how a field before this one is read off the same value.
    pub(crate) fn beside(&self, index: usize) -> Option<Field> {
        match self {
            Field::Tuple(_) => Some(Field::Tuple(index)),
            Field::Struct(structure, _) => Some(Field::Struct(*structure, index)),
            Field::Label(_) | Field::Position(_) => None,
        }
    }
}

/// A projection out of a tuple or a structure's value. See [`Field`] for why the field is a slot in every post-elaboration term.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Proj {
    pub head: Term,
    pub field: Field,
}

/// An inductive type as an intrinsic normal form. Built inside the automatically-generated type-constructor function's body. Users never write one directly — they write `Result(E, A)` and the type-constructor function reduces to this. Two `InductType`s are convertible iff same `name` and pointwise-convertible `params` and `indices`.
///
/// `params` are uniform across constructors; `indices` are the per-case constrained binders — each constructor's registry terminal states its own index expressions. The split lives here, in the registry, and in the type-constructor function, which takes the two in separate calls where a family has both: a use site writes `Vec(Bytes)(3)`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct InductType {
    pub name: Global,
    pub universes: Universes,
    pub params: Vec<Term>,
    pub indices: Vec<Term>,
}

/// A constructor application as an intrinsic normal form. Built inside the automatically-generated value-constructor function's body. Users never write one directly — they write `Result/success(value)` and the constructor function reduces to this.
///
/// `name` and `params` are recoverable from the term's inferred type; they are stored redundantly on purpose, so `convert` stays purely structural (no context lookups mid-comparison).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Variant {
    pub name: Global,
    pub universes: Universes,
    pub params: Vec<Term>,
    pub tag: Atom,
    pub payload: Vec<Term>,
}

/// A struct type as an intrinsic normal form (cf. [`InductType`], no indices). Built inside the generated type-former's body; users write `Pair(A, B)` and the former reduces to this. Convertible iff same `name` and pointwise-convertible `params`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct StructType {
    pub name: Global,
    pub universes: Universes,
    pub params: Vec<Term>,
}

/// One written struct-literal entry, parallel to [`Struct::fields`]: a plain positional field carrying its optional written label, an `@` field written under its mark with its optional label, an explicit `use <term>` fill that pairs with the concept's next `use`-marked field position, or a `..base` spread whose paired term is the base to copy the unwritten fields from (riding in `fields` keeps it visible to every term traversal). A `Spread`, if present, is `entries[0]` — enforced at elaboration, not by construction. Pre-elaboration metadata only, like written field names on [`Tuple`]; elaboration rebuilds the value entry-free.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum StructEntry {
    Field(Option<String>),
    Use,
    Spread,
    Implicit(Option<String>),
}

/// A struct value as an intrinsic normal form (cf. [`Variant`], no tag). `name`/`params` are recoverable from the inferred type but stored redundantly so `convert` stays purely structural.
///
/// `entries` carries the literal's written entry shapes from `into_core`, one per field: elaboration checks plain fields positionally against the declared labels, pairs `use` entries with the concept's `use`-marked positions, and rebuilds the value entry-free. A value with fields and no entries is the normal form — every slot of the field telescope in order, a hidden one included — which is what elaboration rebuilds and what the compiler builds whole; one with neither is the literal written with no entries. Elaborated, it is read as the literal written in full, each field under its slot's mark, so it is elaborated again unchanged.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Struct {
    pub name: Global,
    pub universes: Universes,
    pub params: Vec<Term>,
    pub fields: Vec<Term>,
    pub entries: Vec<StructEntry>,
}

/// The unified eliminator: every match form shares a scrutinee and a result and differs only in its [`Cases`] payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Match {
    pub head: Term,
    pub result: MatchResult,
    pub cases: Cases,
}

/// What an elimination's arms inhabit, in one of two forms.
///
/// A **family** is a motive: a scope closed at the eliminator's own arity — the scrutinee's indices in declaration order, then the scrutinee — which is 1 for every intrinsic carrier and for an unindexed inductive, and `n_indices + 1` for an indexed one. Parameters are never abstracted, being uniform across constructors and fixed by the scrutinee's type, so the body refers to them through the ambient scope like any other term. Before elaboration a written motive is carried in an arity-0 scope instead — see `Term::match_motive_written`. A family is the only form that states a result beyond the goal's own spelling, and the only one an induction hypothesis can be typed from, so it is what a written motive lowers to and what every free-monoid fold whose arm reads its hypothesis carries.
///
/// The **ambient** form is the expected type as it stood in the enclosing context, stated once there and inhabited by each arm under that case's specialization: the scrutinee standing for the case's value and each variable index for the case's target. It exists because a family must typecheck under fresh binders *outside* any arm — where a hypothesis whose type mentions the scrutinee no longer matches the position it occupies in the goal — while the ambient goal was typed where it was written and needs no such check. That is the whole reason for the convoy pattern, which this form makes unnecessary. Elaboration builds it wherever a motive is elided in a checked position, over any scrutinee. A variable is substituted for. Over an expression — written so, or reached by substitution, a `let` value into its tail or an argument into a body — the goal's syntactic occurrences of that expression stand for the case instead: the with-abstraction reading, a fixed operation on the spelling rather than a case equation the reducer may or may not meet, with an occurrence the spelling does not show left to the arm's case equation (see `documentation/design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum MatchResult {
    Family(Scope<Many>),
    Ambient(Term),
}

/// What one case substitutes in an ambient goal: each *variable* actual index for its target, and a variable scrutinee for the case's value. A repeated index variable takes its first target, exactly as an abstraction label binds it once; an expression index or scrutinee has nothing to substitute and is absent. It states the goal and nothing else: the locals an arm re-types are the ones its case's whole solution reaches, which both checkers compute with `curios_analysis::retyped` — this set is narrower, and holds even for a case whose index equations clash, where the solution is empty.
pub fn case_substitution<'a>(
    head: &'a Term,
    actual_indices: &'a [Term],
    case_indices: &'a [Term],
    case_value: &'a Term,
) -> Vec<(&'a Free, &'a Term)> {
    let mut substitution: Vec<(&Free, &Term)> = Vec::new();
    for (actual, target) in actual_indices.iter().zip(case_indices) {
        if let Subterm::Var(var) = &**actual
            && let Some(name) = var.as_free()
            && !substitution.iter().any(|(bound, _)| *bound == name)
        {
            substitution.push((name, target));
        }
    }
    if let Subterm::Var(var) = &**head
        && let Some(name) = var.as_free()
        && !substitution.iter().any(|(bound, _)| *bound == name)
    {
        substitution.push((name, case_value));
    }
    substitution
}

impl MatchResult {
    /// The family, when the result is one.
    pub fn family(&self) -> Option<&Scope<Many>> {
        match self {
            MatchResult::Family(motive) => Some(motive),
            MatchResult::Ambient(_) => None,
        }
    }

    /// The ambient goal, when the result is one.
    pub fn ambient(&self) -> Option<&Term> {
        match self {
            MatchResult::Family(_) => None,
            MatchResult::Ambient(goal) => Some(goal),
        }
    }

    /// The term under the result's binders, for a walk that reads names or tests a predicate and needs no instance.
    pub fn body(&self) -> &Term {
        match self {
            MatchResult::Family(motive) => motive.body(),
            MatchResult::Ambient(goal) => goal,
        }
    }

    /// The result at one case: the family opened at the case's index targets then its value, or the ambient goal with the scrutinee standing for the value and each variable actual index for its target. A variable scrutinee is substituted for; an expression scrutinee has its syntactic occurrences in the goal replaced by the value, which is what a substitution into the variable form leaves behind and what with-abstraction reads. A repeated index variable takes its first target, exactly as an abstraction label binds it once; a non-variable index has nothing to substitute and the goal keeps its spelling there.
    pub fn at(
        &self,
        head: &Term,
        actual_indices: &[Term],
        case_indices: &[Term],
        case_value: &Term,
    ) -> Term {
        match self {
            MatchResult::Family(motive) => {
                let refs = case_indices.iter().chain([case_value]).collect::<Vec<_>>();
                motive.open(&refs)
            }
            MatchResult::Ambient(goal) => {
                let substitution =
                    case_substitution(head, actual_indices, case_indices, case_value);
                let binders = substitution
                    .iter()
                    .map(|(name, _)| *name)
                    .collect::<Vec<_>>();
                let values = substitution
                    .iter()
                    .map(|(_, value)| *value)
                    .collect::<Vec<_>>();
                let goal = match &**head {
                    Subterm::Var(var) if var.as_free().is_some() => goal.clone(),
                    _ => goal.replace_term(head, case_value),
                };
                Scope::close(Many(binders.len()), &binders, goal).open(&values)
            }
        }
    }

    /// The result at the scrutinee itself — the elimination's own type: the family opened at the actual indices then the head, or the ambient goal as written.
    pub fn of(&self, head: &Term, actual_indices: &[Term]) -> Term {
        match self {
            MatchResult::Family(motive) => {
                let refs = actual_indices.iter().chain([head]).collect::<Vec<_>>();
                motive.open(&refs)
            }
            MatchResult::Ambient(goal) => goal.clone(),
        }
    }

    /// The same form with its one term rewritten in place — a family's body under its binders, or the ambient goal.
    pub fn try_map<E>(&self, f: impl FnOnce(&Term) -> Result<Term, E>) -> Result<Self, E> {
        Ok(match self {
            MatchResult::Family(motive) => MatchResult::Family(motive.try_map_body(f)?),
            MatchResult::Ambient(goal) => MatchResult::Ambient(f(goal)?),
        })
    }

    pub(crate) fn reach(&self) -> usize {
        match self {
            MatchResult::Family(motive) => motive.reach(),
            MatchResult::Ambient(goal) => goal.reach(),
        }
    }
}

/// One enumerated arm of a [`Cases::Induct`]: the arm body closed over its payload binders, plus a plicity vector paralleling those binders one mark per slot. `plicities.len()` equals `body.arity()`. Before elaboration the marks are the written constructor-pattern plicities; after elaboration they are the constructor's canonical payload plicities. Reduction, erasure and conversion open the body positionally and never read the marks; the kernel holds them to the payload's where it types the arm. Kept beside the body (rather than in a second map) and sealed at the crate boundary behind [`InductArm::new`], so the two can never drift apart.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct InductArm {
    body: Scope<Many>,
    plicities: Vec<Plicity>,
}

impl InductArm {
    /// The one construction door outside this crate: one mark per payload binder, asserted here rather than at every use.
    pub fn new(body: Scope<Many>, plicities: Vec<Plicity>) -> Self {
        assert_eq!(plicities.len(), body.arity());
        Self { body, plicities }
    }

    /// The arm body, closed over its payload binders.
    pub fn body(&self) -> &Scope<Many> {
        &self.body
    }

    /// The marks, one per payload binder by construction.
    pub fn plicities(&self) -> &[Plicity] {
        &self.plicities
    }

    /// The arm's payload arity — equal to `plicities.len()`.
    pub fn arity(&self) -> usize {
        self.body.arity()
    }

    /// Open the arm body at its payload binders, positionally (plicity is not consulted by reduction or erasure).
    pub fn open(&self, args: &[&Term]) -> Term {
        self.body.open(args)
    }

    /// The arm body's free-variable reach, past its payload binders.
    pub(crate) fn reach(&self) -> usize {
        self.body.reach()
    }

    /// The arm's payload binder hints, in order.
    pub fn hint_iter(&self) -> impl Iterator<Item = Option<&'static str>> {
        self.body.hint_iter()
    }

    /// What the arm's scope remembers of its payload binders, in order.
    pub fn label_iter(&self) -> impl Iterator<Item = Option<&Label>> {
        self.body.label_iter()
    }

    /// Rebuild the arm with its whole body scope replaced, preserving the plicity vector (the traversal-side reconstruction helper).
    pub fn with_body(&self, body: Scope<Many>) -> Self {
        InductArm {
            body,
            plicities: self.plicities.clone(),
        }
    }
}

/// The arm payload of a [`Match`] — the only part that differs between the elimination forms (the scrutinee and motive live on `Match` itself). Which variant a match carries decides both its reduction rule and how erasure lowers it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Cases {
    /// Dependent elimination of `Bool`: a false arm and a true arm.
    Bool { false_case: Term, true_case: Term },
    /// Sparse dispatch on specific `Nat` values with a default arm.
    ///
    /// **Keyed by [`Natural`], not by the erased carrier's `u32`.** `Nat` is unbounded here and a dispatch *key* narrows at the erase boundary, which *refuses* one no branch table indexes rather than wrapping it — see [Nat and Int are an i31 until they outgrow it](../../../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md). A `u32` here would write `curios-anf`'s width into the representation a proof is stated over, three stages above the boundary that owns it.
    ///
    /// A sequence rather than a `BTreeMap`, for the reason [`Cases::Induct`]'s arms are one: this enum is archived, and `Natural`'s archived form is its little-endian bytes, whose collation is not its numeric order. Rather than teach the archived form an ordering it does not have, the keys are held **strictly ascending** — the invariant every constructor establishes and every rebuild preserves, and the one that makes term identity independent of the order arms were written in.
    Switch {
        cases: Vec<(Natural, Term)>,
        default: Term,
    },
    /// The intrinsic eliminator of a nominal inductive: one arm per constructor, each arm's arity equal to that constructor's payload arity. `default` is the optional catch-all arm (`| _ =>`, mirroring [`Cases::Switch`]'s): present iff the surface match ended in a bare `_`. It binds nothing and stands in for every constructor tag absent from `cases`; `None` means the arms structurally cover every constructor (a true elimination). The enumerated arms are checked at their own case target indices and the default at the scrutinee's actual ones, so a catch-all is legal on an indexed family too.
    Induct {
        /// The enumerated arms, in the owning inductive's *declaration order* — the same order `InductDecl::constructor_order` reports, which is what makes this a canonical form: two matches whose arms are written in different source order elaborate to the same sequence, so arm order never enters term identity. Elaboration establishes that by building the arms from `constructor_order` rather than from the written order (`elaborate_induct_match`). A subsequence is legal — an arm may be absent under a `default`, or where index inversion prunes it.
        cases: Vec<(Atom, InductArm)>,
        default: Option<Term>,
        /// The instance of the family the scrutinee stands at, where the match states it: what a match with one arm over a proposition fires on ([`Cases::sole_arm`]), its payload read off this instance's indices and never off what a binder is assumed at. It is part of the term, so a substitution that moves the scrutinee's type moves the match with it, and what is remembered of the one is not read of the other. The elaborator states it where that rule can speak, a proposition whose one constructor's payload its index targets pin, and nowhere else; a match that states none is reduced by its scrutinee's value alone.
        stated: Option<Term>,
    },
    /// Structural induction on a native free-monoid intrinsic (`Nat`/`List`/ `Bytes`): the `carrier` selects the intrinsic and carries both its parameters (`List`'s element type) and its two arms — an identity arm plus a cons arm binding the head generator (absent for `Nat`, whose unary generator carries no payload), the tail, and the induction hypothesis at the tail.
    FreeMonoid { carrier: Carrier },
}

impl Cases {
    /// The one arm of a match that has no other and no catch-all beside it, with its constructor: the match a scrutinee's type decides, since no value of it is another case. `None` for every other match.
    pub fn sole_arm(&self) -> Option<(&Atom, &InductArm)> {
        let Cases::Induct {
            cases,
            default: None,
            ..
        } = self
        else {
            return None;
        };
        let [(tag, arm)] = cases.as_slice() else {
            return None;
        };

        Some((tag, arm))
    }

    /// The instance a match states that it eliminates ([`Cases::Induct`]'s `stated`), where it states one.
    pub fn stated(&self) -> Option<&Term> {
        match self {
            Cases::Induct { stated, .. } => stated.as_ref(),
            _ => None,
        }
    }

    /// The body of a match that reads nothing of its scrutinee: its [sole arm](Self::sole_arm), naming none of its payload. `None` for every other match.
    ///
    /// On a closed term such a match is its arm's body with the scrutinee unevaluated, which is how a cast along a proof runs none of the proof.
    pub fn unread(&self) -> Option<Term> {
        let (_, arm) = self.sole_arm()?;
        if (0..arm.arity()).any(|index| arm.body.uses(index)) {
            return None;
        }

        // No binder is named, so what the arm is opened at is read by nothing.
        let unread = Term::prop();
        Some(arm.open(&vec![&unread; arm.arity()]))
    }
}

/// The native free-monoid intrinsic a `Cases::FreeMonoid` eliminates, with its type parameters and its two eliminator arms. `Nat` is the free monoid on one (payload-less) generator; `Bytes` carries none; `List` carries its element type. Each variant pairs an identity arm (`empty_case`) with a cons arm whose arity is fixed by the carrier — `Scope<Two>` for `Nat` (predecessor, ih), `Scope<Three>` for `Bytes`/`List` (head, tail, ih).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Carrier {
    Nat {
        empty_case: Term,
        cons_case: Scope<Two>,
    },
    Bytes {
        empty_case: Term,
        cons_case: Scope<Three>,
    },
    List {
        elem: Term,
        empty_case: Term,
        cons_case: Scope<Three>,
    },
}

impl Carrier {
    /// The types a cons arm's binders before its induction hypothesis are opened at, in order: the predecessor for `Nat`, and the peeled generator and then the tail for a packed sequence and for a list. The tail is the last.
    pub fn cons_domains(&self) -> Vec<Term> {
        match self {
            Carrier::Nat { .. } => vec![Term::intrinsic(Intrinsic::NatType)],
            Carrier::Bytes { .. } => vec![
                Term::intrinsic(Intrinsic::ByteType),
                Term::intrinsic(Intrinsic::BytesType),
            ],
            Carrier::List { elem, .. } => vec![
                elem.clone(),
                Term::intrinsic(Intrinsic::ListType(elem.clone())),
            ],
        }
    }
}

/// A straight-line block of `let` bindings: `bindings` in written order, then a `tail` continuation in scope of all of them. Binding `i` is stored under the `i` binders before it — its `type_` and `value` may reference bindings `0..i` but never binding `i` itself; a `let` is non-recursive, self- and mutual reference is [`Rec`]'s job. A whole run of source `let`s is one `Let`, not a nest, so every walk over it (`traverse`/`reach`/`reduce`/ `erase`/`elaborate`) is a loop over `bindings` rather than one native stack frame per binding — which is what keeps a long local `let` sequence from overflowing the stack.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Let {
    pub bindings: Vec<LetBinding>,
    pub tail: Scope<Many>,
}

impl Let {
    /// Whether this block's tail binds one binder per binding, which is what opening it over their values requires.
    ///
    /// The count is a fact about a sibling field that nothing here relates it to, so it is asked rather than assumed: a reducer that opened the tail over the wrong number of values would reach `Scope::open`'s `assert!` and abort, where its rule is to stand stuck on every shape it cannot take (`curios-cert`'s README). A block answering `false` is refused where a module enters the kernel (`under_bindings`), so what a reducer meets answering `false` arrived another way.
    pub fn opens(&self) -> bool {
        self.tail.arity() == self.bindings.len()
    }
}

/// One non-recursive local binding: its declared type and its value.
///
/// A local binding is monomorphic. Universe polymorphism is a property of *declarations*, which are frozen into the prelude archive and re-instantiated by later programs; a local binding has no such use sites, and cumulativity admits the uses a local scheme would serve — for `let id : (@A : Type, A) -> A` applied to both `Prop` and `Type 0`, a single `A : Type 1` accepts both, and the level order is linear so a sup always exists.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct LetBinding {
    type_: Term,
    value: Term,
}

impl LetBinding {
    pub fn new(type_: Term, value: Term) -> Self {
        Self { type_, value }
    }

    pub fn type_(&self) -> &Term {
        &self.type_
    }

    pub fn value(&self) -> &Term {
        &self.value
    }

    pub(super) fn into_parts(self) -> (Term, Term) {
        (self.type_, self.value)
    }
}

/// One member of a recursive group as the knot stores it. Both scopes are closed over the whole group, so any member may reference any other.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct RecMemberScopes {
    pub type_: Scope<Many>,
    pub body: Scope<Many>,
}

/// The shared knot of a mutually-recursive group. Every member type and body is scoped over the full group. `Rc` sharing is an implementation detail; equality and hashing remain structural through the scoped items.
///
/// **A group binds no universe.** Its members' level and quality parameters are the enclosing declaration's, positionally, so an instance of that declaration substitutes them with every other level it binds and every reader of a member reads one thing. A group that bound its own would be a second reading of the same index — the one the kernel admitted Hurkens' paradox through, `xboard/src/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs` — since a scope walk shadows what a positional reader substitutes. `check_group` therefore checks a member's terms as written, under the constraints the declaration holding the group assumed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct RecGroup {
    members: Rc<Vec<RecMemberScopes>>,
}

impl RecGroup {
    /// A group of `members`, each a binder with its type and its body, every term closed over every binder in order.
    ///
    /// **The only door, and why there is one.** A member scope's arity is a fact about the members the group holds, and nothing in the representation relates the two. A constructor taking scopes already closed takes the two counts apart, and `member_type` and `member_body` then open a scope over one term per member through `Scope::open`'s `assert!` — the only reader of either count, and a reducer reaches it before `check_group` runs. Taking the binders here makes the arity *be* the member count rather than a second number to agree with, so the disagreement cannot be written. A restored archive reconstructs the fields directly and so passes no constructor at all, which is why `validate_stored_identities` reads the count at the seam a unit is stored.
    pub fn closing(members: Vec<(Free, Term, Term)>) -> Self {
        let binders = members
            .iter()
            .map(|(binder, _, _)| binder)
            .collect::<Vec<_>>();
        let arity = Many(binders.len());

        Self {
            members: Rc::new(
                members
                    .iter()
                    .map(|(_, type_, body)| RecMemberScopes {
                        type_: Scope::close(arity, &binders, type_.clone()),
                        body: Scope::close(arity, &binders, body.clone()),
                    })
                    .collect(),
            ),
        }
    }

    /// A group of scopes already closed, holding nothing between their arity and the member count — the door [`RecGroup::closing`] exists to remove.
    ///
    /// Reached by the traversals that rebuild a group in place, which preserve every arity because they reopen nothing, and by `curios-core`'s `test_support`, so a fixture can build the form a restored archive could carry and put it to the seam that reads it.
    pub(crate) fn from_scopes(items: Vec<RecMemberScopes>) -> Self {
        Self {
            members: Rc::new(items),
        }
    }

    /// [`RecGroup::map_members`] where the rewrite can fail. Every arity is preserved for the same reason: each scope is rebuilt in place and none is reopened.
    pub fn try_map_members<E>(
        &self,
        mut map: impl FnMut(&Term) -> Result<Term, E>,
    ) -> Result<Self, E> {
        Ok(Self {
            members: Rc::new(
                self.iter()
                    .map(|member| {
                        Ok(RecMemberScopes {
                            type_: member.type_.try_map_body(&mut map)?,
                            body: member.body.try_map_body(&mut map)?,
                        })
                    })
                    .collect::<Result<_, E>>()?,
            ),
        })
    }

    /// This group with every member's closed scopes rewritten in place.
    ///
    /// The bodies stay closed throughout. Terms under a scope carry loose de Bruijn indices, and two structurally identical such terms — indices included — denote the same thing at the same depth, so canonicalizing them together is sound without opening.
    pub fn map_members(&self, mut map: impl FnMut(&Term) -> Term) -> Self {
        Self::from_scopes(
            self.iter()
                .map(|member| RecMemberScopes {
                    type_: member.type_.map_body(&mut map),
                    body: member.body.map_body(&mut map),
                })
                .collect(),
        )
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RecMemberScopes> + Clone {
        self.members.iter()
    }

    fn item(&self, index: usize) -> &RecMemberScopes {
        self.members
            .get(index)
            .expect("recursive member index in bounds")
    }

    /// This group with universe data projected out of every member — the shape under which a group and the same group at an instance's levels are one group, which is what lets a diagnostic recognize an unfolded `rec` as the definition it came from.
    pub fn projected(&self) -> Self {
        self.map_members(project_erased_universes)
    }

    pub fn length(&self) -> usize {
        self.iter().len()
    }

    /// Whether every member scope binds one binder per member, which is what opening one over the group requires.
    ///
    /// True of every group [`RecGroup::closing`] builds and of every one a traversal rebuilds, so no construction in Rust can answer `false`. A restored archive reconstructs the fields directly and passes no constructor, which is the one way a group answering `false` enters a compilation — read at the seam a unit is stored (`validate_stored_identities`) and again where a walk meets one (`check_group`).
    pub fn counted(&self) -> bool {
        self.iter().all(|member| {
            member.type_.arity() == self.length() && member.body.arity() == self.length()
        })
    }

    /// One term per member, each denoting that member and needing nothing in scope to do it — what every scope in this group opens over.
    pub fn members(&self) -> Vec<Term> {
        (0..self.length())
            .map(|index| Term::rec_proj(self.clone(), index))
            .collect()
    }

    pub fn member_type(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).type_.open(&refs)
    }

    pub fn member_body(&self, index: usize) -> Term {
        let members = self.members();
        let refs = members.iter().collect::<Vec<_>>();
        self.item(index).body.open(&refs)
    }

    pub(super) fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        // No universe scope is entered: a member's level and quality parameters are the enclosing declaration's, so a walk that substitutes the declaration's parameters substitutes a member's with them. Entering one here would shadow exactly what every positional reader of a member reads.
        Self::from_scopes(
            self.iter()
                .map(|member| RecMemberScopes {
                    type_: visit.visit_scope(&member.type_),
                    body: visit.visit_scope(&member.body),
                })
                .collect(),
        )
    }

    pub(super) fn reach(&self) -> usize {
        self.iter()
            .map(|member| member.type_.reach().max(member.body.reach()))
            .max()
            .unwrap_or(0)
    }
}

/// A block of mutually recursive bindings with a tail in scope of the shared group.
///
/// This is the *only* recursion form. A demanded member occurrence is this same node with a tail that selects one member — see [`Term::rec_proj`] — rather than a form of its own, so the rule that checks a group is the rule that checks an occurrence of it. A self-describing occurrence node is rejected for what it costs: a node well-formed standing alone is one no scope gates, and a kernel could type one from the group it carried without ever checking that group.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Rec {
    pub group: RecGroup,
    pub tail: Scope<Many>,
}

impl Rec {
    /// Whether this block's tail binds one binder per member, which is what opening it over them requires — [`Let::opens`] of the other block form, and asked for the same reason.
    ///
    /// A tail that *selects* a member needs no opening at all, opening it being the identity ([`Rec::as_proj`]), so a projection is reduced by stepping to the member's body and this count never comes up. The member scopes it steps through carry a count of their own, which `check_group` reads at the boundary and this question does not cover: a reducer handed a projection out of a group whose member scopes disagree with its members opens one of them through `member_body` before any typing rule has read either count, which `documentation/roadmap/soundness/000-findings.md` holds as an entry.
    pub fn opens(&self) -> bool {
        self.tail.arity() == self.group.length()
    }

    /// The member index this block's tail selects, when it selects one rather than computing something of its own — see [`Term::rec_proj`].
    pub fn as_proj(&self) -> Option<usize> {
        let Subterm::Var(var) = &**self.tail.body() else {
            return None;
        };

        var.as_bound().filter(|index| *index < self.group.length())
    }
}

/// Provenance of an inserted implicit argument: the applied callee (`func`) had no `@`-argument for an implicit binder at some call site, so the elaborator filled the slot with a fresh metavariable. `binder` is the binder's name, `None` where it was written `_` or declared as its type alone.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct ImplicitOrigin {
    pub func: CalleeId,
    pub binder: Option<String>,
}

/// Provenance of an inserted witness argument: the applied function (`func`) had no `use`-argument for a witness slot at some call site, so the elaborator filled the slot with a fresh metavariable and registered a resolution goal for it. An occurrence still unsolved at zonk reports as a missing witness (naming the goal type from the birth record) rather than an uninferred implicit.
///
/// A `use` slot has no name, so `role` is what the slot is to its callee as a report says it — `its 2nd 'use' premise`, `its 'Eql' superclass` — and `None` where nothing states one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WitnessOrigin {
    pub func: CalleeId,
    pub role: Option<String>,
}

/// Provenance of a metavariable — which mechanism minted it, deciding both how zonk reports it unsolved and what an elaboration site may do with it. An unsolved `Implicit`/`Witness` survivor names the slot it filled — by its binder, or by its role to the callee — an unsolved `Domain` the lambda parameter whose type was never determined, and an unsolved `Hole` is a bare "cannot infer", while a `Goal` is reported unconditionally.
///
/// **A site may special-case a `Hole`; a `Goal` always takes the general path.** Inferring a binding over an elided annotation, synthesizing an elided motive, refusing a lambda whose domain nothing pins — each of those is a decision about a *silent* hole, and one matching any bare metavariable would discard a written `?` in those positions unelaborated and compile the program with a goal in it. [`Metavar::is_hole`] is the one predicate those sites ask, so the rule is stated in the type rather than re-derived per site.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum MetavarOrigin {
    /// A silent inference hole: an elided annotation, motive, lambda domain or element type (`into_core` mints it via `Term::hole`), or an elaborator placeholder with no provenance of its own — a parked problem's stand-in, a recursive group's slot, the type a goal in synthesis position stands over. Its solution is spliced without comment; unsolved, it is the bare "cannot infer".
    Hole,
    /// A settle-synthesized lambda's unannotated domain, named by its binder where it has a name: minted when a lambda whose expectation never gained structure is synthesized at a settle tier, replacing the silent hole (which is solved with it). Solved — by the body, or by whatever the settled type later unifies with — it splices silently like a hole; unsolved, zonk reports the parameter whose type was never determined, by name where it has one.
    Domain(Option<String>),
    Implicit(ImplicitOrigin),
    Witness(WitnessOrigin),
    /// A written goal `?` (`into_core` mints it via `Term::goal`): the user asked what elaboration determines here, so zonk errors with the goal's scope, type, and solution — solved or not — instead of splicing.
    Goal,
}

/// A metavariable's identity: a dense index into the elaborator's solution store (`Solutions`), minted monotonically by an `Entropy`. A newtype so it can never be confused with the other `usize`-shaped notions the kernel juggles (de Bruijn indices, telescope arities, variant tags, `Nat` magnitudes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct MetavarId(pub usize);

impl From<usize> for MetavarId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for MetavarId {
    fn mint(entropy: u64) -> Self {
        Self(usize::mint(entropy))
    }
}

impl fmt::Display for MetavarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A metavariable: a placeholder term standing for an as-yet-unknown subterm, born from a surface hole `?` and (possibly) solved by unification. The solution, when one exists, lives in the elaborator's solution store (`Solutions`), keyed by `id`, spelled with the *birth telescope's* free names.
///
/// `origin` rides with the node and says what minted it — a silent hole, an elaborator-inserted implicit/witness argument (zonk's unsolved report then names the slot instead of a bare id), or a written goal `?` (zonk reports it unconditionally). Each id is minted exactly once (`into_core` desugared holes as `Hole` and written goals as `Goal`, core insertions above the floor `into_core` returns with theirs), so every occurrence of an id carries the same origin and the derived equality never splits an id.
///
/// `spine` is the delayed substitution — one term per binder of the birth telescope (`MetaEntry::telescope` order), recording what that binder corresponds to at this occurrence. Identity (`Var::free(name)`) at birth. The entries are ordinary term content: `traverse` walks them, so `close` captures them and `open` substitutes them, and the mapping survives re-closing under fresh names — which is what lets a solution mentioning a sibling binder resolve correctly wherever the occurrence ends up. An empty spine is a not-yet-birthed `into_core` hole and resolves as the identity.
///
/// The spine is persistent and carries its own facts ([`MetavarSpine`]): every metavariable born under one scope shares the scope's identity spine, and a node holding one derives its hash and flags without reading it, which is what keeps minting a metavariable O(1) instead of O(|Γ|).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Metavar {
    pub id: MetavarId,
    #[archived_with(crate::AsSpineEntries)]
    pub spine: MetavarSpine,
    pub origin: MetavarOrigin,
}

impl Metavar {
    /// Whether this is a silent hole — the one origin an elaboration site may special-case (infer over, synthesize for, refuse as unpinned). Every other origin, the written goal above all, takes the site's general path; see [`MetavarOrigin`].
    pub fn is_hole(&self) -> bool {
        matches!(self.origin, MetavarOrigin::Hole)
    }
}

/// A sort: a universe of a quality, at its level. `Type u` and `Prop u` are the two a program writes, and both are typed `Type (u + 1)`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Sort {
    pub quality: Quality,
    pub level: Level,
}

impl Sort {
    /// `Type` at `level`.
    pub fn type_at(level: Level) -> Sort {
        Sort {
            quality: Quality::Type,
            level,
        }
    }

    /// `Prop` at `level`.
    pub fn prop_at(level: Level) -> Sort {
        Sort {
            quality: Quality::Prop,
            level,
        }
    }

    /// The quality of a record of fields at `this` and at `that`: a type's where either is one, a proposition's where both are, and an open one where it is the only one that is not a proposition's. Two open qualities apart have no join a sort can state, and are read as a type's, which is what a quality nothing decides closes as.
    pub fn join(this: Quality, that: Quality) -> Quality {
        match (this, that) {
            (Quality::Type, _) | (_, Quality::Type) => Quality::Type,
            (Quality::Prop, other) | (other, Quality::Prop) => other,
            (this, that) if this == that => this,
            _ => Quality::Type,
        }
    }

    /// Whether this is a sort of propositions. One whose quality is open is not: irrelevance is taken where a type is known a proposition, and a sort not decided yet, or left to a caller, is not known one.
    pub fn is_prop(&self) -> bool {
        self.quality.is_prop()
    }

    /// The universe term this sort denotes: what a type former reports as its own type.
    pub fn term(self) -> Term {
        Term::from(Subterm::Sort(self))
    }
}

/// A store-described host call: the row's [`WireSignature`](curios_abi::WireSignature) fixes the operand types checked at elaboration and the result shape (unit, bare value, or named record). Effectful, so reducing one at the type level is an error; it becomes a host call only at erasure.
///
/// A term former rather than a [`Intrinsic`] variant, because it is the one construct here whose meaning is *not* fixed by the enum that holds it: every intrinsic has a signature this crate spells, while a foreign call reads its own off the ABI row it carries. Nothing about it is closed, so it does not belong in a closed set.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Foreign {
    /// The row the call is made through.
    pub function: Arc<ForeignFunction>,
    /// The `foreign` declaration that states the row: what a report spells the call by, as it spells any name, where the row itself holds only the name it imports under.
    pub declared: Global,
    pub arguments: Vec<Term>,
}

/// A reference to a declaration at the levels, and the qualities, this occurrence chose: none where the declaration is monomorphic, so every reference to a declaration an elaborated term holds is one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Instance {
    /// The declaration this occurrence instantiates. A global, as a family's and a constructor's name is, so that a binder no scheme governs — a local, a bound variable, a `let` — has no spelling here; the nominal normal forms carry their instance in their own universe vectors instead (see `Term::stamp_declaration_node`).
    ///
    /// A projection out of a recursive group is no such name, and there is no other shape one can take. A group binds no universe ([`RecGroup`]), so a member has no scheme for an instance to satisfy and its levels are the enclosing declaration's, settled by the instance of *that* declaration; an instance that named a member would be a second binder over the same indices, which is the reading `xboard/src/formation/a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs` admitted Hurkens' paradox through.
    pub name: Global,
    pub universes: Universes,
}
