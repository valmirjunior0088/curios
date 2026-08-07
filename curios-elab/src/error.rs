use {
    super::Erased,
    curios_base::{Grain, Int, Plicity, Qualifier, Span},
    curios_core::{
        Atom, Level, Module, Polarity, ReduceError, Term, UniverseConstraintOrigin, UniverseError,
        build_rename, build_shorten, display_names, with_erased_universes, with_pretty_names,
        with_short_names,
    },
    num_bigint::BigUint,
    std::{collections::BTreeSet, fmt, rc::Rc},
};

/// One written goal's entry in an [`Error::Goals`] batch: its occurrence span, the local scope frozen at its birth, its expected type, and the solution unification committed (if any). Scope binders are free `Var` terms (not raw strings) for the same pretty-rename reason as [`Error::Goal`]; every term is display-ready — tolerantly materialized, so committed substitutions appear while goal-origin and unsolved metavariables stay visible.
#[derive(Debug)]
pub struct GoalReport {
    pub span: Option<Span>,
    pub scope: Vec<(Term, Term)>,
    pub goal: Term,
    pub solution: Option<Term>,
    /// Sandboxed candidate fits for an unsolved goal, display-ready and rendered as `? ≈` lines; empty for a solved goal. Observation-only: the compiler re-checks whatever the author pastes.
    pub candidates: Vec<Term>,
}

/// Source-location anchoring is the [`Error::Located`] wrapper's job — the elaborate/erase/zonk drivers attach the offending term's span as the error propagates. Variants therefore carry only what their message displays; a variant carries a `Term` only when the message prints it.
#[derive(Debug)]
pub enum Error {
    ReduceExhausted {
        term: Box<Term>,
    },
    ConvertExhausted {
        this: Box<Term>,
        that: Box<Term>,
    },
    BinGetOutOfBounds {
        len: usize,
        index: usize,
    },
    BinSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
    },
    LstGetOutOfBounds {
        len: usize,
        index: usize,
    },
    LstSliceOutOfRange {
        len: usize,
        start: usize,
        end: usize,
    },
    DivisionByZero {
        kind: &'static str,
    },
    UniverseInconsistency {
        lower: Level,
        upper: Level,
        path: Vec<UniverseConstraintOrigin>,
    },
    UniverseInvariant(String),
    ByteLiteralOutOfRange {
        value: String,
    },
    TypeMismatch {
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    NotAFunction {
        head_type: Box<Term>,
    },
    NotAFunctionType {
        expected: Box<Term>,
    },
    NotATuple {
        head_type: Box<Term>,
    },
    NotATupleType {
        expected: Box<Term>,
    },
    TupleArityMismatch {
        expected: usize,
        got: usize,
    },
    TupleIndexOutOfBounds {
        index: usize,
        arity: usize,
    },
    UnknownTupleLabel {
        label: String,
        available: Vec<String>,
    },
    DuplicateTupleLabel {
        label: String,
    },
    /// A second `induct` registered under a name a prior declaration already claims. The registry is global across every root elaborated into one `Context`, so this is rejected rather than silently overwritten — the same shape of collision `DuplicateWitness` guards for witnesses.
    DuplicateInduct {
        name: String,
    },
    /// A second `struct` registered under a name a prior declaration already claims — see `DuplicateInduct`.
    DuplicateStruct {
        name: String,
    },
    /// A second `concept` registered under a name a prior declaration already claims — see `DuplicateInduct`.
    DuplicateConcept {
        name: String,
    },
    TupleFieldNameMismatch {
        written: String,
        expected: String,
        position: usize,
    },
    NotNatType {
        head_type: Box<Term>,
    },
    NotBoolType {
        head_type: Box<Term>,
    },
    NotLstType {
        head_type: Box<Term>,
    },
    /// `grain` names the packed binary the operation wanted, so the message says `Bits` or `Bytes` — the spellings the surface has. There is no surface type named after the grain-parametric family.
    NotBinType {
        grain: Grain,
        head_type: Box<Term>,
    },
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    /// A written function binder claims a slot whose plicity it does not match. `position` is the binder's 1-based position among the written binders, `expected` the plicity of the expected slot it aligned with, and `written` the mark it carries. Under automatic hidden-binder insertion this fires when a marked (`@`/`use`) binder reaches an *explicit* expected slot — an explicit slot is never skipped and never marked.
    BinderPlicityMismatch {
        position: usize,
        expected: Plicity,
        written: Plicity,
    },
    UnknownMatchConstructor {
        type_name: String,
        tag: String,
    },
    MatchCaseMissing {
        term: Box<Term>,
        atom: Atom,
    },
    CtorArityMismatch {
        atom: Atom,
        expected: usize,
        got: usize,
    },
    NotAInductType {
        head_type: Box<Term>,
    },
    /// A strict proposition was eliminated into a relevant (data) result — a large elimination that would observe which inhabitant it was, breaking proof irrelevance. Permitted only for an empty or singleton proposition.
    LargeElimOfProp {
        name: String,
    },
    /// A struct was declared at sort `Prop` but a field is informative (its type is not itself a proposition). Proof irrelevance would then let projection observe which inhabitant was built — the singleton-elimination condition, applied at declaration time. `field` is the offending field; `field_type` its type.
    InformativePropStruct {
        name: String,
        field: String,
        field_type: Box<Term>,
    },
    /// An `induct` or `struct` declaration is not strictly positive: it reaches itself through a position that is not a plain payload. Such a declaration is not the initial algebra of a polynomial functor, so the eliminator it would hand back is not sound — the classic witness being `induct Bad | c(f : (Bad) -> False) end`, which inhabits `False`.
    ///
    /// `site` is the constructor payload or struct field the offending path starts from and `site_type` its type. The path itself may be longer than one step: `polarity` is where it ends up, not where the named site stands on its own.
    NotStrictlyPositive {
        name: String,
        site: String,
        site_type: Box<Term>,
        polarity: Polarity,
    },
    /// A position erasure deletes is not known to terminate.
    ///
    /// Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be total: a divergent type breaks type formation, and a divergent proof proves anything. General recursion is untouched everywhere erasure keeps it, so this rejects a *position*, never a definition.
    ///
    /// `offender` is the partial definition the position reaches, or `None` when the position is partial on its own account — an inline `rec` that does not descend, or an exit, neither of which has a name to blame.
    PartialInErasedPosition {
        erased: Erased,
        site: String,
        offender: Option<String>,
    },
    /// A struct literal's (or struct type's) head names a binding that is not a struct; `found` is that binding's type.
    NotAStructType {
        found: Box<Term>,
    },
    /// Wrong number of type arguments applied to a struct's type-former.
    StructArityMismatch {
        name: String,
        expected: usize,
        got: usize,
    },
    /// A struct literal supplies the wrong number of fields.
    WrongNumberOfFields {
        name: String,
        expected: usize,
        got: usize,
    },
    /// A nominal type whose declaration is absent from the registry.
    UnknownDeclaration {
        name: String,
    },
    /// A written field label does not match the declared label at its position (fields are given in declaration order — no reordering).
    UnknownStructField {
        name: String,
        label: String,
        available: Vec<String>,
    },
    /// A `use <term>` entry in a literal whose head is not a concept.
    UseEntryOutsideConcept {
        name: String,
    },
    /// More `use <term>` entries than the concept has `use`-marked fields.
    TooManyUseEntries {
        name: String,
        expected: usize,
        got: usize,
    },
    /// A `..` spread entry written anywhere but first in the literal.
    SpreadNotFirst {
        name: String,
    },
    /// More than one `..` spread entry in one literal.
    MultipleSpreads {
        name: String,
    },
    /// The base of a `..` spread is not a value of the literal's own struct.
    SpreadBaseTypeMismatch {
        name: String,
        found: Box<Term>,
    },
    /// An unlabeled override after a `..` spread — gaps make positions ambiguous, so every override must name its field.
    UnlabeledSpreadOverride {
        name: String,
    },
    /// Overrides after a `..` spread must be an order-preserving subsequence of the declared fields; `label` is repeated or out of place.
    SpreadOverrideOutOfOrder {
        name: String,
        label: String,
        order: Vec<String>,
    },
    /// Projecting a field of a struct whose representation is private, from outside the declaring module's subtree.
    PrivateField {
        name: String,
        field: String,
    },
    /// Constructing or eliminating a nominal type whose representation is private, from outside the declaring module's subtree.
    PrivateRepresentation {
        name: String,
    },
    UnboundVariable {
        term: Box<Term>,
    },
    CannotInfer,
    /// A parked checking problem that survived every retry: the expression's expected type never gained the structure the check was waiting on. Reported at the expression's own span by the item drain.
    PostponedCheck {
        expected: Box<Term>,
    },
    /// An overloaded infix operator applied at an operand type with no matching scalar primitive — `%` on `Flt`, `!=` on `Bool`, `+` on `Bool`, etc. The `symbol` is the operator's spelling; `type_` is the resolved operand type.
    OperatorUndefined {
        symbol: String,
        type_: Box<Term>,
    },
    /// An inserted implicit argument that unification never pinned. Carries the insertion provenance (the applied function and the binder it filled) so the report names the hole instead of a bare metavar id.
    UninferredImplicit {
        func: String,
        binder: String,
    },
    /// A call supplies more `@`-arguments than the function has implicit binders (the explicit-slot counterpart is `WrongNumberOfArguments`).
    TooManyImplicits {
        expected: usize,
        got: usize,
    },
    /// A call supplies more `use`-arguments than the function has witness binders (the `use` counterpart of `TooManyImplicits`).
    TooManyWitnessArgs {
        expected: usize,
        got: usize,
    },
    /// A witness goal that resolution could not discharge: no matching local `use` binder, no superclass projection, and no witness-table entry. `func`/`binder` are the insertion provenance (the applied function and the `use` binder the goal fills).
    NoWitness {
        goal: Box<Term>,
        func: String,
        binder: String,
    },
    /// A written goal `?` reaching zonk — reported unconditionally, solved or not: writing `?` asks what elaboration determined there, so the report *is* the outcome and the program never compiles. Carries the display frozen at the goal's birth: the local scope in binding order, the goal's type, and the solution unification committed (if any). Each scope binder is a free `Var` term (not a raw string) so it runs through the same pretty-rename map as the types and solution, and the report spells every name consistently.
    ///
    /// The compile path batches goals via [`Error::Goals`] before zonk runs, so this single-goal form survives as the safety net for direct zonk callers.
    Goal {
        scope: Vec<(Term, Term)>,
        goal: Box<Term>,
        solution: Option<Box<Term>>,
    },
    /// Every written goal `?` one elaboration reached, batched: collection replaces zonk's first-goal error on the compile path, so a program holding several goals reports them all in one run. Entries are deterministically ordered (items in declaration order, then the entrypoint tail) and each carries its own occurrence span rather than a shared `Located` wrapper — a goal's identity is its source location.
    Goals(Vec<GoalReport>),
    /// Two witnesses registered under the same `(concept, key)` — global coherence admits exactly one witness per key, program-wide.
    DuplicateWitness {
        concept: String,
        key: super::WitnessKey,
        /// The two declaring modules. Witnesses are anonymous, so the module is the coordinate that locates them for a reader — carried from each declaration's `island` rather than recovered by splitting the compiler-minted name.
        first: Qualifier,
        second: Qualifier,
    },
    /// A witness registered by a root that owns neither the concept nor any key head's declaring root — the orphan rule: a coherence-relevant registration must happen where the concept or a type it mentions is already declared, so two unrelated roots cannot independently `satisfy` the same concept+type and collide unfixably downstream.
    OrphanWitness {
        concept: String,
        key: super::WitnessKey,
        /// The declaring module — see [`Error::DuplicateWitness`].
        witness: Qualifier,
    },
    /// Two distinct superclass projections of local `use` binders match a goal at the same minimal depth — no principled tiebreak exists.
    AmbiguousWitness {
        goal: Box<Term>,
        first: Box<Term>,
        second: Box<Term>,
    },
    /// The superclass graph (concepts' `use`-marked fields) has a cycle.
    CyclicSuperclass {
        concept: String,
    },
    /// A concept's `use`-marked field names a superclass that isn't a registered concept at all (e.g. it resolves to a struct or inductive).
    UnknownSuperclass {
        concept: String,
        target: String,
    },
    /// A witness's concept parameter at `position` (0-based) does not reduce to a rigid nominal or primitive head — nothing to key the table entry on.
    InvalidWitnessHead {
        witness: String,
        position: usize,
        head: Box<Term>,
    },
    /// A witness's annotation does not elaborate to an application of a registered concept.
    NotAConcept {
        witness: String,
        found: Box<Term>,
    },
    /// A `use` premise of a witness applies its concept to something other than the witness's own parameters — resolution through it would not be structurally decreasing.
    NonRegularWitnessPremise {
        witness: String,
        premise: Box<Term>,
    },
    /// A witness telescope declares an explicit parameter; nothing could supply it at resolution time.
    ExplicitWitnessParam {
        witness: String,
    },
    NatOverflow {
        value: BigUint,
    },
    /// A program whose erased module fails the arena representation's verifier — today exactly the recursion classes the language rejects (a computed-only recursive cycle no initialization order satisfies). The verifier owns rejection; erasure only surfaces its diagnostic.
    ErasedModuleInvalid {
        detail: String,
    },
    /// An `Int` literal that survived to `erase` but does not fit `ersd`'s `i32` carrier — the type level is unbounded, so the representation narrowing lives at the erase boundary, like [`Error::NatOverflow`]'s u32. (The runtime's own i31 limit is enforced where it appears: `cont` → wasm lowering.)
    IntOverflow {
        value: Box<Int>,
    },
    /// A written motive binds the wrong number of names. An eliminator's motive abstracts the scrutinee's indices, in declaration order, and then the scrutinee — `expected` of them. `name` is the eliminated family when there is one to name (a primitive carrier has none).
    MotiveBinderCount {
        name: Option<String>,
        expected: usize,
        written: usize,
    },
    /// An arm of an indexed-inductive match was omitted, but inversion could not prove the case impossible at the scrutinee's indices.
    MissingArmNotImpossible {
        tag: Atom,
    },
    Located {
        span: Span,
        error: Box<Error>,
    },
    /// The declaration whose elaboration raised `error`.
    ///
    /// A sibling of [`Error::Located`] rather than a message prefix: the context is structured, so a consumer can still match on the underlying error, and every failure gains a name without each raising site formatting one.
    InDeclaration {
        name: String,
        error: Box<Error>,
    },
}

impl Error {
    /// Phrase a [`ReduceError`] as a user-facing diagnostic. The reducer reports what the term did; naming it is this crate's job, which is why the conversion lives on [`Error`] rather than on the core failure. The `exhausted` callback lets each caller decide what a spent budget reports — the term being reduced, or the pair being compared.
    pub(crate) fn from_reduce(error: ReduceError, exhausted: impl FnOnce() -> Error) -> Error {
        match error {
            ReduceError::Exhausted => exhausted(),
            ReduceError::BinGetOutOfBounds { len, index, span } => {
                Error::BinGetOutOfBounds { len, index }.at_opt(span)
            }
            ReduceError::BinSliceOutOfRange {
                len,
                start,
                end,
                span,
            } => Error::BinSliceOutOfRange { len, start, end }.at_opt(span),
            ReduceError::LstGetOutOfBounds { len, index, span } => {
                Error::LstGetOutOfBounds { len, index }.at_opt(span)
            }
            ReduceError::LstSliceOutOfRange {
                len,
                start,
                end,
                span,
            } => Error::LstSliceOutOfRange { len, start, end }.at_opt(span),
            ReduceError::DivisionByZero { kind, span } => {
                Error::DivisionByZero { kind }.at_opt(span)
            }
            ReduceError::Universe(error) => Error::from(error),
        }
    }

    pub(crate) fn reduce_exhausted<T: Into<Term>>(term: T) -> Self {
        Self::ReduceExhausted {
            term: Box::new(term.into()),
        }
    }

    pub(crate) fn convert_exhausted<T: Into<Term>, U: Into<Term>>(this: T, that: U) -> Self {
        Self::ConvertExhausted {
            this: Box::new(this.into()),
            that: Box::new(that.into()),
        }
    }

    pub(crate) fn type_mismatch<U: Into<Term>, V: Into<Term>>(inferred: U, expected: V) -> Self {
        Self::TypeMismatch {
            inferred: Box::new(inferred.into()),
            expected: Box::new(expected.into()),
        }
    }

    pub(crate) fn postponed_check<U: Into<Term>>(expected: U) -> Self {
        Self::PostponedCheck {
            expected: Box::new(expected.into()),
        }
    }

    pub(crate) fn not_a_function<U: Into<Term>>(head_type: U) -> Self {
        Self::NotAFunction {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn not_a_function_type<U: Into<Term>>(expected: U) -> Self {
        Self::NotAFunctionType {
            expected: Box::new(expected.into()),
        }
    }

    pub(crate) fn not_a_tuple<U: Into<Term>>(head_type: U) -> Self {
        Self::NotATuple {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn not_a_tuple_type<U: Into<Term>>(expected: U) -> Self {
        Self::NotATupleType {
            expected: Box::new(expected.into()),
        }
    }

    pub(crate) fn tuple_arity_mismatch(expected: usize, got: usize) -> Self {
        Self::TupleArityMismatch { expected, got }
    }

    pub(crate) fn tuple_index_out_of_bounds(index: usize, arity: usize) -> Self {
        Self::TupleIndexOutOfBounds { index, arity }
    }

    pub(crate) fn unknown_tuple_label(label: String, available: Vec<String>) -> Self {
        Self::UnknownTupleLabel { label, available }
    }

    pub(crate) fn duplicate_tuple_label(label: String) -> Self {
        Self::DuplicateTupleLabel { label }
    }

    pub(crate) fn duplicate_induct(name: String) -> Self {
        Self::DuplicateInduct { name }
    }

    pub(crate) fn duplicate_struct(name: String) -> Self {
        Self::DuplicateStruct { name }
    }

    pub(crate) fn duplicate_concept(name: String) -> Self {
        Self::DuplicateConcept { name }
    }

    pub(crate) fn tuple_field_name_mismatch(
        written: String,
        expected: String,
        position: usize,
    ) -> Self {
        Self::TupleFieldNameMismatch {
            written,
            expected,
            position,
        }
    }

    pub(crate) fn not_nat_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotNatType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn not_bool_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotBoolType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn not_lst_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotLstType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn not_bin_type<U: Into<Term>>(grain: Grain, head_type: U) -> Self {
        Self::NotBinType {
            grain,
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn wrong_number_of_arguments(expected: usize, got: usize) -> Self {
        Self::WrongNumberOfArguments { expected, got }
    }

    pub(crate) fn unknown_match_constructor(type_name: String, tag: String) -> Self {
        Self::UnknownMatchConstructor { type_name, tag }
    }

    pub(crate) fn match_case_missing<T: Into<Term>, A: Into<Atom>>(term: T, atom: A) -> Self {
        Self::MatchCaseMissing {
            term: Box::new(term.into()),
            atom: atom.into(),
        }
    }

    pub(crate) fn not_a_induct_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotAInductType {
            head_type: Box::new(head_type.into()),
        }
    }

    pub(crate) fn large_elim_of_prop<N: Into<String>>(name: N) -> Self {
        Self::LargeElimOfProp { name: name.into() }
    }

    pub(crate) fn informative_prop_struct<N: Into<String>, F: Into<String>, T: Into<Term>>(
        name: N,
        field: F,
        field_type: T,
    ) -> Self {
        Self::InformativePropStruct {
            name: name.into(),
            field: field.into(),
            field_type: Box::new(field_type.into()),
        }
    }

    pub(crate) fn not_strictly_positive<N: Into<String>, S: Into<String>, T: Into<Term>>(
        name: N,
        site: S,
        site_type: T,
        polarity: Polarity,
    ) -> Self {
        Self::NotStrictlyPositive {
            name: name.into(),
            site: site.into(),
            site_type: Box::new(site_type.into()),
            polarity,
        }
    }

    pub(crate) fn not_a_struct_type<U: Into<Term>>(found: U) -> Self {
        Self::NotAStructType {
            found: Box::new(found.into()),
        }
    }

    pub(crate) fn struct_arity_mismatch<N: Into<String>>(
        name: N,
        expected: usize,
        got: usize,
    ) -> Self {
        Self::StructArityMismatch {
            name: name.into(),
            expected,
            got,
        }
    }

    pub(crate) fn wrong_number_of_fields<N: Into<String>>(
        name: N,
        expected: usize,
        got: usize,
    ) -> Self {
        Self::WrongNumberOfFields {
            name: name.into(),
            expected,
            got,
        }
    }

    /// A nominal type whose declaration is missing from the registry — a well-typed term can only carry a declared nominal head, so this is an invariant violation surfaced as a diagnostic rather than a panic.
    pub(crate) fn unknown_declaration(name: String) -> Self {
        Self::UnknownDeclaration { name }
    }

    pub(crate) fn unknown_struct_field<N: Into<String>>(
        name: N,
        label: String,
        available: Vec<String>,
    ) -> Self {
        Self::UnknownStructField {
            name: name.into(),
            label,
            available,
        }
    }

    pub(crate) fn use_entry_outside_concept<N: Into<String>>(name: N) -> Self {
        Self::UseEntryOutsideConcept { name: name.into() }
    }

    pub(crate) fn too_many_use_entries<N: Into<String>>(
        name: N,
        expected: usize,
        got: usize,
    ) -> Self {
        Self::TooManyUseEntries {
            name: name.into(),
            expected,
            got,
        }
    }

    pub(crate) fn spread_not_first<N: Into<String>>(name: N) -> Self {
        Self::SpreadNotFirst { name: name.into() }
    }

    pub(crate) fn multiple_spreads<N: Into<String>>(name: N) -> Self {
        Self::MultipleSpreads { name: name.into() }
    }

    pub(crate) fn spread_base_type_mismatch<N: Into<String>, T: Into<Term>>(
        name: N,
        found: T,
    ) -> Self {
        Self::SpreadBaseTypeMismatch {
            name: name.into(),
            found: Box::new(found.into()),
        }
    }

    pub(crate) fn unlabeled_spread_override<N: Into<String>>(name: N) -> Self {
        Self::UnlabeledSpreadOverride { name: name.into() }
    }

    pub(crate) fn spread_override_out_of_order<N: Into<String>>(
        name: N,
        label: String,
        order: Vec<String>,
    ) -> Self {
        Self::SpreadOverrideOutOfOrder {
            name: name.into(),
            label,
            order,
        }
    }

    pub(crate) fn private_field<N: Into<String>, F: Into<String>>(name: N, field: F) -> Self {
        Self::PrivateField {
            name: name.into(),
            field: field.into(),
        }
    }

    pub(crate) fn private_representation<N: Into<String>>(name: N) -> Self {
        Self::PrivateRepresentation { name: name.into() }
    }

    pub(crate) fn ctor_arity_mismatch<A: Into<Atom>>(atom: A, expected: usize, got: usize) -> Self {
        Self::CtorArityMismatch {
            atom: atom.into(),
            expected,
            got,
        }
    }

    pub(crate) fn unbound_variable<T: Into<Term>>(var: T) -> Self {
        Self::UnboundVariable {
            term: Box::new(var.into()),
        }
    }

    pub(crate) fn operator_undefined<T: Into<Term>>(symbol: String, type_: T) -> Self {
        Self::OperatorUndefined {
            symbol,
            type_: Box::new(type_.into()),
        }
    }

    pub(crate) fn uninferred_implicit(func: String, binder: String) -> Self {
        Self::UninferredImplicit { func, binder }
    }

    pub(crate) fn too_many_implicits(expected: usize, got: usize) -> Self {
        Self::TooManyImplicits { expected, got }
    }

    pub(crate) fn too_many_witness_args(expected: usize, got: usize) -> Self {
        Self::TooManyWitnessArgs { expected, got }
    }

    pub(crate) fn no_witness<T: Into<Term>>(goal: T, func: String, binder: String) -> Self {
        Self::NoWitness {
            goal: Box::new(goal.into()),
            func,
            binder,
        }
    }

    pub(crate) fn goal(scope: Vec<(Term, Term)>, goal: Term, solution: Option<Term>) -> Self {
        Self::Goal {
            scope,
            goal: Box::new(goal),
            solution: solution.map(Box::new),
        }
    }

    pub(crate) fn goals(reports: Vec<GoalReport>) -> Self {
        Self::Goals(reports)
    }

    /// Whether this failure is a written-goal batch — incomplete development state rather than a hard error. The process boundary reports the two distinctly (the CLI exits 2 for incomplete, 1 for hard), so the distinction must survive formatting.
    pub fn is_incomplete(&self) -> bool {
        match self {
            Self::Goals(_) => true,
            Self::Located { error, .. } | Self::InDeclaration { error, .. } => {
                error.is_incomplete()
            }
            _ => false,
        }
    }

    pub(crate) fn duplicate_witness(
        concept: String,
        key: super::WitnessKey,
        first: Qualifier,
        second: Qualifier,
    ) -> Self {
        Self::DuplicateWitness {
            concept,
            key,
            first,
            second,
        }
    }

    pub(crate) fn orphan_witness(
        concept: String,
        key: super::WitnessKey,
        witness: Qualifier,
    ) -> Self {
        Self::OrphanWitness {
            concept,
            key,
            witness,
        }
    }

    pub(crate) fn ambiguous_witness<T: Into<Term>, U: Into<Term>, V: Into<Term>>(
        goal: T,
        first: U,
        second: V,
    ) -> Self {
        Self::AmbiguousWitness {
            goal: Box::new(goal.into()),
            first: Box::new(first.into()),
            second: Box::new(second.into()),
        }
    }

    pub(crate) fn cyclic_superclass<N: Into<String>>(concept: N) -> Self {
        Self::CyclicSuperclass {
            concept: concept.into(),
        }
    }

    pub(crate) fn unknown_superclass<N: Into<String>, T: Into<String>>(
        concept: N,
        target: T,
    ) -> Self {
        Self::UnknownSuperclass {
            concept: concept.into(),
            target: target.into(),
        }
    }

    pub(crate) fn invalid_witness_head<N: Into<String>, T: Into<Term>>(
        witness: N,
        position: usize,
        head: T,
    ) -> Self {
        Self::InvalidWitnessHead {
            witness: witness.into(),
            position,
            head: Box::new(head.into()),
        }
    }

    pub(crate) fn not_a_concept<N: Into<String>, T: Into<Term>>(witness: N, found: T) -> Self {
        Self::NotAConcept {
            witness: witness.into(),
            found: Box::new(found.into()),
        }
    }

    pub(crate) fn non_regular_witness_premise<N: Into<String>, T: Into<Term>>(
        witness: N,
        premise: T,
    ) -> Self {
        Self::NonRegularWitnessPremise {
            witness: witness.into(),
            premise: Box::new(premise.into()),
        }
    }

    pub(crate) fn explicit_witness_param<N: Into<String>>(witness: N) -> Self {
        Self::ExplicitWitnessParam {
            witness: witness.into(),
        }
    }

    pub(crate) fn nat_overflow(value: BigUint) -> Self {
        Self::NatOverflow { value }
    }

    pub(crate) fn erased_module_invalid(detail: String) -> Self {
        Self::ErasedModuleInvalid { detail }
    }

    pub(crate) fn int_overflow(value: Int) -> Self {
        Self::IntOverflow {
            value: Box::new(value),
        }
    }

    pub(crate) fn motive_binder_count(
        name: Option<String>,
        expected: usize,
        written: usize,
    ) -> Self {
        Self::MotiveBinderCount {
            name,
            expected,
            written,
        }
    }

    pub(crate) fn missing_arm_not_impossible(tag: Atom) -> Self {
        Self::MissingArmNotImpossible { tag }
    }

    /// Name the declaration this error arose in. Innermost wins, matching [`Error::at`]: a nested item keeps its own attribution.
    pub(crate) fn in_declaration(self, name: &str) -> Self {
        match self {
            Self::InDeclaration { .. } => self,
            error => Self::InDeclaration {
                name: name.to_string(),
                error: Box::new(error),
            },
        }
    }

    pub(crate) fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    pub(crate) fn at_opt(self, span: Option<Span>) -> Self {
        match span {
            Some(span) => self.at(span),
            None => self,
        }
    }

    pub(crate) fn format(&self) -> String {
        // Render with source-style names (axis (a)): collect the names appearing across every term this error displays, build one collision-aware rename map for them, and install it for the duration of the render so `inferred`/`expected` agree on what each name means.
        let mut terms = Vec::new();
        self.collect_terms(&mut terms);

        let mut names = BTreeSet::new();
        for term in terms {
            names.extend(display_names(term));
        }

        let rename = Rc::new(build_rename(&names));
        // Axis (c) wraps the whole render rather than any one variant: every error that prints a term prints it from the raw elaborated spelling, so the instance suffix is not a mismatch-specific wart. Suppressing at the printer keeps `Error` a plain data carrier — the alternative, rewriting each variant's terms through `project_erased_universes`, would have to mirror `collect_terms`'s roster and take `&mut self` through two public entry points.
        with_erased_universes(|| with_pretty_names(rename, || self.render()))
    }

    /// Like [`format`], additionally shortening global names against `module`'s symbol table (axis (b)) — the qualified-name universe an error's globals are spelled relative to. Used on the core error paths, where the lowered module is in scope.
    pub fn format_with(&self, module: &Module) -> String {
        with_short_names(Rc::new(build_shorten(&module.module_symbols())), || {
            self.format()
        })
    }

    /// One snippet per rendered error, for the innermost span attached to it.
    ///
    /// [`Error::at`] is first-wins *per wrapper*, so the innermost span is the first one stamped — but `in_declaration` may wrap a located error, after which a further `at` sees a non-`Located` head and stamps again, leaving the coarser span outermost. Rendering therefore searches for the innermost rather than reading the outermost, and the message body is assembled separately so a nested `Located` cannot swallow it: `Display` for the wrappers deliberately prints no snippet, and a body rendered through `to_string` would drop the inner span silently.
    fn render(&self) -> String {
        let body = self.render_body();
        match self.innermost_span() {
            Some(span) => format!("{body}\n\n{}", span.render_snippet()),
            None => body,
        }
    }

    /// The message without any snippet — wrappers are transparent, every other variant renders through its own `Display`.
    fn render_body(&self) -> String {
        match self {
            Self::Located { error, .. } => error.render_body(),
            Self::InDeclaration { name, error } => {
                format!("while elaborating {name}:\n{}", error.render_body())
            }
            error => error.to_string(),
        }
    }

    /// The innermost span stamped on this error, looking through both wrappers.
    fn innermost_span(&self) -> Option<&Span> {
        match self {
            Self::Located { span, error } => error.innermost_span().or(Some(span)),
            Self::InDeclaration { error, .. } => error.innermost_span(),
            _ => None,
        }
    }

    /// The terms this error embeds in its message, gathered so [`format`] can pretty-print their names consistently. Recurses through the `Located` wrapper; variants carrying no term contribute nothing.
    fn collect_terms<'a>(&'a self, out: &mut Vec<&'a Term>) {
        match self {
            Self::Located { error, .. } | Self::InDeclaration { error, .. } => {
                error.collect_terms(out)
            }
            Self::ReduceExhausted { term } => out.push(term),
            Self::ConvertExhausted { this, that } => {
                out.push(this);
                out.push(that);
            }
            Self::TypeMismatch { inferred, expected } => {
                out.push(inferred);
                out.push(expected);
            }
            Self::NotAFunction { head_type }
            | Self::NotATuple { head_type }
            | Self::NotNatType { head_type }
            | Self::NotBoolType { head_type }
            | Self::NotLstType { head_type }
            | Self::NotBinType { head_type, .. }
            | Self::NotAInductType { head_type } => out.push(head_type),
            Self::NotAFunctionType { expected } | Self::NotATupleType { expected } => {
                out.push(expected)
            }
            Self::NotAStructType { found } => out.push(found),
            Self::InformativePropStruct { field_type, .. } => out.push(field_type),
            Self::NotStrictlyPositive { site_type, .. } => out.push(site_type),
            Self::OperatorUndefined { type_, .. } => out.push(type_),
            Self::SpreadBaseTypeMismatch { found, .. } => out.push(found),
            Self::MatchCaseMissing { term, .. } => out.push(term),
            Self::UnboundVariable { term } => out.push(term),
            Self::PostponedCheck { expected } => out.push(expected),
            Self::NoWitness { goal, .. } => out.push(goal),
            Self::Goal {
                scope,
                goal,
                solution,
            } => {
                for (name, type_) in scope {
                    out.push(name);
                    out.push(type_);
                }
                out.push(goal);
                out.extend(solution.as_deref());
            }
            Self::Goals(reports) => {
                for report in reports {
                    for (name, type_) in &report.scope {
                        out.push(name);
                        out.push(type_);
                    }
                    out.push(&report.goal);
                    out.extend(report.solution.as_ref());
                    out.extend(&report.candidates);
                }
            }
            Self::AmbiguousWitness {
                goal,
                first,
                second,
            } => {
                out.push(goal);
                out.push(first);
                out.push(second);
            }
            Self::InvalidWitnessHead { head, .. } => out.push(head),
            Self::NotAConcept { found, .. } => out.push(found),
            Self::NonRegularWitnessPremise { premise, .. } => out.push(premise),
            _ => {}
        }
    }
}

impl From<UniverseError> for Error {
    fn from(error: UniverseError) -> Self {
        match error {
            UniverseError::Inconsistency { lower, upper, path } => {
                Self::UniverseInconsistency { lower, upper, path }
            }
            other => Self::UniverseInvariant(other.to_string()),
        }
    }
}

/// How a diagnostic names a declaring module. The root qualifier is the entry module — a legitimate value, not a missing one.
fn declaring_module(module: &Qualifier) -> String {
    match module.is_root() {
        true => "the entry module".to_string(),
        false => format!("module '{}'", module.join()),
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ReduceExhausted { term } => {
                write!(f, "reduction ran out of steps on: {term}")
            }
            Error::ConvertExhausted { this, that } => {
                write!(f, "conversion ran out of steps between {this} and {that}")
            }
            Error::TypeMismatch { inferred, expected } => {
                write!(
                    f,
                    "type mismatch\n  inferred: {inferred}\n  expected: {expected}"
                )
            }
            Error::UniverseInconsistency { lower, upper, path } => {
                write!(
                    f,
                    "this Type would need to be strictly below itself\n  required constraint: {lower} ≤ {upper}"
                )?;
                if path.len() > 1 {
                    write!(f, "\n  inconsistency path has {} steps", path.len())?;
                }
                Ok(())
            }
            Error::UniverseInvariant(message) => {
                write!(f, "invalid inferred universe state: {message}")
            }
            Error::NotAFunction { head_type } => {
                write!(f, "applied a non-function\n  head has type: {head_type}")
            }
            Error::NotAFunctionType { expected } => {
                write!(
                    f,
                    "introduced a lambda where the expected type is not a function type\n  expected: {expected}"
                )
            }
            Error::NotATuple { head_type } => {
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::NotATupleType { expected } => {
                write!(
                    f,
                    "introduced a tuple where the expected type is not a tuple type\n  expected: {expected}"
                )
            }
            Error::TupleArityMismatch { expected, got } => {
                write!(
                    f,
                    "tuple has {got} field(s) but expected type has {expected}"
                )
            }
            Error::TupleIndexOutOfBounds { index, arity } => {
                write!(f, "tuple index {index} out of bounds (arity {arity})")
            }
            Error::UnknownTupleLabel { label, available } => {
                if available.is_empty() {
                    write!(
                        f,
                        "no field named '{label}' (the tuple type has no labeled fields)"
                    )
                } else {
                    write!(
                        f,
                        "no field named '{label}' (available: {})",
                        available.join(", ")
                    )
                }
            }
            Error::DuplicateTupleLabel { label } => {
                write!(f, "duplicate field label '{label}' in tuple type")
            }
            Error::DuplicateInduct { name } => {
                write!(f, "duplicate inductive declaration '{name}'")
            }
            Error::DuplicateStruct { name } => {
                write!(f, "duplicate struct declaration '{name}'")
            }
            Error::DuplicateConcept { name } => {
                write!(f, "duplicate concept declaration '{name}'")
            }
            Error::TupleFieldNameMismatch {
                written,
                expected,
                position,
            } => {
                if expected.is_empty() {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type has no label there"
                    )
                } else {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type calls it '{expected}'"
                    )
                }
            }
            Error::NotNatType { head_type } => {
                write!(f, "expected Nat but got {head_type}")
            }
            Error::NotBoolType { head_type } => {
                write!(f, "expected Bool but got {head_type}")
            }
            Error::NotLstType { head_type } => {
                write!(f, "expected Lst but got {head_type}")
            }
            Error::NotBinType { grain, head_type } => {
                let expected = match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                };

                write!(f, "expected {expected} but got {head_type}")
            }
            Error::WrongNumberOfArguments { expected, got } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {got}"
                )
            }
            Error::BinderPlicityMismatch {
                position,
                expected,
                written,
            } => {
                let requirement = match expected {
                    Plicity::Explicit => "an explicit parameter (written with no mark)",
                    Plicity::Implicit => "an implicit parameter (written with `@`)",
                    Plicity::Witness => "a witness parameter (written with `use`)",
                };
                let written = match written {
                    Plicity::Explicit => "written with no mark",
                    Plicity::Implicit => "written with `@`",
                    Plicity::Witness => "written with `use`",
                };
                write!(
                    f,
                    "function parameter {position} is {requirement}, but was {written}"
                )
            }
            Error::UnknownMatchConstructor { type_name, tag } => {
                write!(f, "match arm '{tag}' is not a constructor of '{type_name}'")
            }
            Error::MatchCaseMissing { term, atom } => {
                write!(f, "missing match case for constructor '{atom}': {term}")
            }
            Error::NotAInductType { head_type } => {
                write!(
                    f,
                    "matched inductive constructors on a non-inductive type\n  head has type: {head_type}"
                )
            }
            Error::LargeElimOfProp { name } => {
                write!(
                    f,
                    "cannot eliminate the proposition '{name}' into a relevant result\n  a strict proposition admits large elimination only when empty or singleton"
                )
            }
            Error::InformativePropStruct {
                name,
                field,
                field_type,
            } => {
                write!(
                    f,
                    "struct '{name}' is declared at sort 'Prop' but field '{field} : {field_type}' is informative\n  a 'Prop' struct's fields must all be propositions (or forced by indices)"
                )
            }
            Error::NotStrictlyPositive {
                name,
                site,
                site_type,
                polarity,
            } => {
                write!(
                    f,
                    "'{name}' is not strictly positive: through '{site} : {site_type}' it occurs in itself {polarity}\n  a recursive occurrence must be a plain payload, never left of an arrow"
                )
            }
            Error::PartialInErasedPosition {
                erased,
                site,
                offender,
            } => {
                let advice = match erased {
                    Erased::Type => {
                        "everything a type reaches must terminate, or type formation may not"
                    }
                    Erased::Proof => {
                        "erasure deletes proofs, so a proof that may not terminate proves anything"
                    }
                };
                match offender {
                    Some(offender) => write!(
                        f,
                        "{site} is a {erased} position but reaches '{offender}', which is not known to terminate\n  {advice}"
                    ),
                    None => write!(
                        f,
                        "{site} is a {erased} position but does not terminate on every input\n  {advice}"
                    ),
                }
            }
            Error::NotAStructType { found } => {
                write!(f, "expected a struct type here\n  found: {found}")
            }
            Error::StructArityMismatch {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "struct '{name}' takes {expected} type argument(s) but got {got}"
                )
            }
            Error::WrongNumberOfFields {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "struct '{name}' has {expected} field(s) but the literal supplies {got}"
                )
            }
            Error::UnknownDeclaration { name } => {
                write!(f, "no declaration for '{name}'")
            }
            Error::UnknownStructField {
                name,
                label,
                available,
            } => {
                write!(
                    f,
                    "struct '{name}' has no field '{label}' at that position (fields in order: {})",
                    available.join(", ")
                )
            }
            Error::UseEntryOutsideConcept { name } => {
                write!(
                    f,
                    "'use' entries are only legal in concept literals — struct '{name}' is not a concept"
                )
            }
            Error::TooManyUseEntries {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "literal supplies {got} 'use' entr{} but concept '{name}' has only {expected} 'use' field(s)",
                    if *got == 1 { "y" } else { "ies" }
                )
            }
            Error::SpreadNotFirst { name } => {
                write!(
                    f,
                    "a '..' spread must be the first entry of the '{name}' literal"
                )
            }
            Error::MultipleSpreads { name } => {
                write!(
                    f,
                    "a struct literal takes at most one '..' spread, but the '{name}' literal has more"
                )
            }
            Error::SpreadBaseTypeMismatch { name, found } => {
                write!(
                    f,
                    "the '..' base of a '{name}' literal must itself be a '{name}'\n  found: {found}"
                )
            }
            Error::UnlabeledSpreadOverride { name } => {
                write!(
                    f,
                    "overrides after a '..' spread must be labeled ('field = value') in the '{name}' literal"
                )
            }
            Error::SpreadOverrideOutOfOrder { name, label, order } => {
                write!(
                    f,
                    "overrides after a '..' spread must follow '{name}''s declared field order ({}); '{label}' is repeated or out of place",
                    order.join(", ")
                )
            }
            Error::PrivateField { name, field } => {
                write!(
                    f,
                    "field '{field}' of struct '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::PrivateRepresentation { name } => {
                write!(
                    f,
                    "the representation of type '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::CtorArityMismatch {
                atom,
                expected,
                got,
            } => {
                write!(
                    f,
                    "constructor '{atom}' takes {expected} argument(s) but the match arm binds {got}"
                )
            }
            Error::UnboundVariable { term } => {
                write!(f, "unbound variable: {term}")
            }
            Error::CannotInfer => {
                write!(f, "cannot infer type of expression")
            }
            Error::PostponedCheck { expected } => {
                write!(
                    f,
                    "cannot check expression: its expected type never gained structure: {expected}"
                )
            }
            Error::OperatorUndefined { symbol, type_ } => {
                write!(f, "operator '{symbol}' is not defined for type {type_}")
            }
            Error::UninferredImplicit { func, binder } => {
                write!(
                    f,
                    "implicit argument '{binder}' of '{func}' was not inferred; supply it explicitly: {func}(@...)"
                )
            }
            Error::TooManyImplicits { expected, got } => {
                write!(
                    f,
                    "call supplies {got} '@' argument(s) but the function has only {expected} implicit parameter(s)"
                )
            }
            Error::TooManyWitnessArgs { expected, got } => {
                write!(
                    f,
                    "call supplies {got} 'use' argument(s) but the function has only {expected} 'use' parameter(s)"
                )
            }
            Error::NoWitness { goal, func, binder } => {
                write!(
                    f,
                    "no witness of {goal} found\n  needed by '{func}' for its 'use' binder '{binder}'"
                )
            }
            Error::Goal {
                scope,
                goal,
                solution,
            } => {
                // `?` itself is the turnstile: hypotheses as `name : type` lines, then `? : type` states the goal and `? = term` its solution — the declaration idiom `name : type = value` split into clauses about `?`. No `? =` line means nothing determined it.
                write!(f, "goal `?`")?;
                for (name, type_) in scope {
                    write!(f, "\n  {name} : {type_}")?;
                }
                write!(f, "\n  ? : {goal}")?;
                match solution {
                    Some(solution) => write!(f, "\n  ? = {solution}"),
                    None => Ok(()),
                }
            }
            Error::Goals(reports) => {
                // A report's terms render within a fixed width — the pipeline is pure and stays terminal-blind, so the target is a constant — and a broken term's continuation lines re-indent under the clause body rather than restarting at column zero.
                const WIDTH: usize = 100;
                let clause = |term: &Term| {
                    term.display_within(WIDTH)
                        .to_string()
                        .replace('\n', "\n    ")
                };

                // Each entry is the single-goal turnstile idiom followed by its own snippet — message first, then location, matching how `render` orders a `Located` diagnostic. Entries are separated by a blank line.
                for (index, report) in reports.iter().enumerate() {
                    if index > 0 {
                        write!(f, "\n\n")?;
                    }
                    write!(f, "goal `?`")?;
                    for (name, type_) in &report.scope {
                        write!(f, "\n  {name} : {}", clause(type_))?;
                    }
                    write!(f, "\n  ? : {}", clause(&report.goal))?;
                    if let Some(solution) = &report.solution {
                        write!(f, "\n  ? = {}", clause(solution))?;
                    }
                    for candidate in &report.candidates {
                        write!(f, "\n  ? \u{2248} {}", clause(candidate))?;
                    }
                    if let Some(span) = &report.span {
                        write!(f, "\n\n{}", span.render_snippet())?;
                    }
                }
                Ok(())
            }
            Error::DuplicateWitness {
                concept,
                key,
                first,
                second,
            } => {
                let noun = match key.0.len() {
                    1 => "head",
                    _ => "key",
                };
                write!(
                    f,
                    "duplicate witness of '{concept}' for {noun} '{key}'\n  one is declared in {}, another in {}\n  every concept-{noun} pair has at most one witness, program-wide",
                    declaring_module(first),
                    declaring_module(second)
                )
            }
            Error::OrphanWitness {
                concept,
                key,
                witness,
            } => {
                let noun = match key.0.len() {
                    1 => "head",
                    _ => "key",
                };
                write!(
                    f,
                    "orphan witness of '{concept}' for {noun} '{key}', declared in {}\n  a witness may only be declared where the concept or a type in its {noun} is already declared",
                    declaring_module(witness)
                )
            }
            Error::AmbiguousWitness {
                goal,
                first,
                second,
            } => {
                write!(
                    f,
                    "ambiguous witness of {goal}\n  both {first} and {second} match at the same superclass depth"
                )
            }
            Error::CyclicSuperclass { concept } => {
                write!(
                    f,
                    "concept '{concept}' participates in a superclass cycle ('use'-marked fields must form an acyclic graph)"
                )
            }
            Error::UnknownSuperclass { concept, target } => {
                write!(
                    f,
                    "concept '{concept}' names '{target}' as a superclass, but '{target}' is not a registered concept"
                )
            }
            Error::InvalidWitnessHead {
                witness,
                position,
                head,
            } => {
                write!(
                    f,
                    "witness '{witness}' cannot be keyed: its concept's parameter {n} reduces to {head}\n  every parameter's head must be an inductive, a struct, or a primitive type",
                    n = position + 1
                )
            }
            Error::NotAConcept { witness, found } => {
                write!(
                    f,
                    "witness '{witness}' does not witness a concept\n  its annotation elaborates to: {found}"
                )
            }
            Error::NonRegularWitnessPremise { witness, premise } => {
                write!(
                    f,
                    "witness '{witness}' has a non-regular premise: {premise}\n  every 'use' premise must apply its concept to variables bound by the witness's own parameters"
                )
            }
            Error::ExplicitWitnessParam { witness } => {
                write!(
                    f,
                    "witness '{witness}' declares an explicit parameter\n  witness parameters must be implicit ('@') or 'use' premises — nothing supplies explicit arguments during resolution"
                )
            }
            Error::NatOverflow { value } => {
                write!(f, "Nat literal {value} overflows u32 at the erase boundary")
            }
            Error::ErasedModuleInvalid { detail } => {
                write!(f, "unsupported recursion at the erase boundary: {detail}")
            }
            Error::IntOverflow { value } => {
                write!(
                    f,
                    "Int literal {value:+} overflows i32 at the erase boundary"
                )
            }
            Error::MotiveBinderCount {
                name,
                expected,
                written,
            } => {
                let subject = match name {
                    Some(name) => format!("eliminating '{name}'"),
                    None => "this eliminator".to_string(),
                };
                let shape = match expected {
                    1 => "just the scrutinee".to_string(),
                    2 => "one index, then the scrutinee".to_string(),
                    n => format!("{} indices, then the scrutinee", n - 1),
                };
                write!(
                    f,
                    "motive binds {written} name(s), but {subject} needs {expected} ({shape})"
                )
            }
            Error::MissingArmNotImpossible { tag } => {
                write!(
                    f,
                    "missing arm '{tag}': its index target is not provably impossible at the scrutinee's indices — write the arm"
                )
            }
            Error::BinGetOutOfBounds { len, index } => {
                write!(f, "Bin.get index {index} out of bounds (length {len})")
            }
            Error::BinSliceOutOfRange { len, start, end } => {
                write!(
                    f,
                    "Bin.slice range {start}..{end} out of range (length {len})"
                )
            }
            Error::LstGetOutOfBounds { len, index } => {
                write!(f, "Lst.get index {index} out of bounds (length {len})")
            }
            Error::LstSliceOutOfRange { len, start, end } => {
                write!(
                    f,
                    "Lst.slice range {start}..{end} out of range (length {len})"
                )
            }
            Error::DivisionByZero { kind } => {
                write!(f, "division by zero in {kind}")
            }
            Error::ByteLiteralOutOfRange { value } => {
                write!(f, "Byte literal {value} is out of range (expected 0..=255)")
            }
            Error::InDeclaration { name, error } => {
                write!(f, "while elaborating {name}:\n{error}")
            }
            Error::Located { error, .. } => {
                write!(f, "{error}")
            }
        }
    }
}
