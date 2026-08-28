mod display;
use display::*;

use {
    super::Erased,
    curios_core::{
        Atom, Free, Global, Imports, Level, Module, Polarity, ReduceError, Spelling, Subterm, Term,
        UniverseConstraintOrigin, UniverseError, build_rename, build_shorten, display_names,
    },
    curios_num::{Integer, Natural},
    curios_utilities::{Grain, Plicity, Qualifier, Report, Span},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        fmt,
        rc::Rc,
    },
};

/// One written goal's entry in an [`Error::Goals`] batch: its occurrence span, the local scope frozen at its birth, its expected type, and the solution unification committed (if any). Scope binders are free `Var` terms (not raw strings) for the same pretty-rename reason as [`Error::Goal`], an unnameable binder's line spelling `_` the way source does; every term is display-ready — tolerantly materialized, so committed substitutions appear while goal-origin and unsolved metavariables stay visible.
#[derive(Debug)]
pub struct GoalReport {
    pub span: Option<Span>,
    pub scope: Vec<(Term, Term)>,
    pub goal: Term,
    pub solution: Option<Term>,
    /// Conversions the item drain could not decide because this goal (perhaps with others) was all that held them up, each as the two sides that must become equal — rendered as `? such that` lines. What tells the reader of `Eq/cong(?, ih)` that the hole has to send `double(p)` to `double(p) + 2`.
    pub obligations: Vec<(Term, Term)>,
    /// Sandboxed candidate fits for an unsolved goal, display-ready and rendered as `? ≈` lines; empty for a solved goal. Observation-only: the compiler re-checks whatever the author pastes.
    pub candidates: Vec<Term>,
}

impl GoalReport {
    /// The axis-(a) rename map for this one report: built over the names *it* mentions, so a binder is suffixed only against a collision the reader can see from this goal. A batch-wide map — the one [`Error::rename_map`] builds for every other error — renamed the second of two functions' `n` to `n2`, a collision with a binder that belongs to a different goal's scope and appears nowhere in this one.
    fn rename_map(&self, shorten: &HashMap<Global, String>) -> Rc<HashMap<Free, String>> {
        let mut names = BTreeSet::new();
        for (name, type_) in &self.scope {
            names.extend(display_names(name));
            names.extend(display_names(type_));
        }
        names.extend(display_names(&self.goal));
        names.extend(self.solution.iter().flat_map(display_names));
        for (this, that) in &self.obligations {
            names.extend(display_names(this));
            names.extend(display_names(that));
        }
        names.extend(self.candidates.iter().flat_map(display_names));
        Rc::new(build_rename(&names, shorten))
    }
}

/// The embedding-specific half of a missing-witness report, computed when the unresolved goal is the registry's `Lift`: the two monads, whether the source is a monad at all, and any chain of declared edges connecting the pair — each fact the report needs to steer the fix (declare the edge, fix the action, or spell the composite) without the reader reconstructing the table.
#[derive(Debug)]
pub struct EmbeddingDiagnosis {
    pub source: Box<Term>,
    pub target: Box<Term>,
    /// Whether any `Monad` witness exists for the source's head: an edge out of a non-monad could never be declared, so suggesting one would be a trap.
    pub source_is_monad: bool,
    /// A chain of declared edges from source to target, each hop `(key display, declaring module)`. Non-empty means the embeddings exist but were never composed — embeddings never chain automatically. The module rides as its [`Qualifier`] so rendering stays with `declaring_module`, the one spelling of "declared in …".
    pub chain: Vec<(String, Qualifier)>,
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
        length: usize,
    },
    ListGetOutOfBounds {
        len: usize,
        index: usize,
    },
    ListSliceOutOfRange {
        len: usize,
        start: usize,
        length: usize,
    },
    DivisionByZero {
        kind: &'static str,
    },
    IntToNatNegative {
        value: Box<Integer>,
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
    BoolLiteralOutOfRange {
        value: String,
    },
    FltLiteralOutOfRange {
        value: String,
    },
    TypeMismatch {
        inferred: Box<Term>,
        expected: Box<Term>,
    },
    /// The `/syn/Monad/bind` application a postfix `!` desugars to, checked against a region that has nothing to sequence in: `bind` produces `M(B)` and the region's type stands over no result at all.
    ///
    /// `sequenced` is that `M(B)` with its unsolved holes blanked, so the message names the monad the region would have to be without naming the result a region that cannot sequence never reaches. It is `None` when `M` is itself one of those holes: nothing pinned the monad, and a spelling made only of placeholders would name nothing.
    StrandedSequencing {
        sequenced: Option<Box<Term>>,
        region: Box<Term>,
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
    NotListType {
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
    /// A parked conversion goal that survived every retry, distinguished from `TypeMismatch`: a rigid mismatch means the program is wrong, a postponed conversion means the program may be right and inference never gained the structure to decide. Reported at the goal's origin span by the item drain, naming the still-unsolved blockers it watched.
    PostponedConversion {
        this: Box<Term>,
        that: Box<Term>,
        /// The still-unsolved watched metavariables, pre-rendered with insertion provenance where an occurrence carries it.
        watching: Vec<String>,
        /// Whether the goal's frozen frame carried live match-arm refinements — a solution holding only under them is deliberately never committed, one way a goal stays undecided.
        under_refinements: bool,
        /// The watched witness goals whose registration never arrived in time, pre-rendered: their table entries were still missing when this item's drain ran, so the conversion could not unfold through them. Items order by the names they reference and a witness is anonymous, which is why the report suggests naming the operation.
        deferred_witnesses: Vec<String>,
    },
    /// A postfix `!` whose region's monad can never be determined: an inference-position region, or one whose expected type stayed an unsolved metavariable through every retry. Strict postponement reads the monad from the region's type and never infers it from the action, so a region that never names one cannot sequence.
    BangRegionUndetermined,
    /// An overloaded infix operator applied at an operand type with no matching scalar intrinsic — `%` on `Flt`, `!=` on `Bool`, `+` on `Bool`, etc. The `symbol` is the operator's spelling; `type_` is the resolved operand type.
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
        /// Present when the goal is the registry's `Lift`: the embedding-specific half of the report.
        embedding: Option<EmbeddingDiagnosis>,
    },
    /// Two witnesses that resolve each other. A witness may recurse through its *own* table entry — its declaration registers before its body elaborates for exactly that reason — but a cycle between two of them has no binding order: whichever is emitted first names one that does not exist yet, and the kernel refuses it as an unbound name. Caught here so the refusal is stated in the language's own terms, at a span, with the way out named.
    WitnessCycle {
        this: Box<Term>,
        that: Box<Term>,
    },
    /// A written goal `?` reaching zonk — reported unconditionally, solved or not: writing `?` asks what elaboration determined there, so the report *is* the outcome and the program never compiles. Carries the display frozen at the goal's birth: the local scope in binding order, the goal's type, and the solution unification committed (if any). Each scope binder is a free `Var` term (not a raw string) so it runs through the same pretty-rename map as the types and solution, and the report spells every name consistently — except an unnameable binder, whose line spells `_` the way source does.
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
    /// A witness's concept parameter at `position` (0-based) does not reduce to a rigid nominal or intrinsic head — nothing to key the table entry on.
    InvalidWitnessHead {
        witness: String,
        position: usize,
        head: Box<Term>,
    },
    /// A witness for a parameterless concept: with no parameter heads there is nothing to key the global table entry on, so such a concept is supplied through a local `use` binder instead.
    ParameterlessWitnessConcept {
        witness: String,
        concept: String,
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
        value: Natural,
    },
    /// A program whose erased module fails the erased representation's verifier — today exactly the recursion classes the language rejects (a computed-only recursive cycle no initialization order satisfies). The verifier owns rejection; erasure only surfaces its diagnostic.
    ErasedModuleInvalid {
        detail: String,
    },
    /// An `Int` literal that survived to `erase` but does not fit `ersd`'s `i32` carrier — the type level is unbounded, so the representation narrowing lives at the erase boundary, like [`Error::NatOverflow`]'s u32. (The runtime's own i31 limit is enforced where it appears: `cont` → wasm lowering.)
    IntOverflow {
        value: Box<Integer>,
    },
    /// A written motive binds the wrong number of names. An eliminator's motive abstracts the scrutinee's indices, in declaration order, and then the scrutinee — `expected` of them. `name` is the eliminated family when there is one to name (an intrinsic carrier has none).
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
            ReduceError::Exhausted { .. } => exhausted(),
            ReduceError::BinGetOutOfBounds { len, index, span } => {
                Error::BinGetOutOfBounds { len, index }.at_opt(span)
            }
            ReduceError::BinSliceOutOfRange {
                len,
                start,
                length,
                span,
            } => Error::BinSliceOutOfRange { len, start, length }.at_opt(span),
            ReduceError::ListGetOutOfBounds { len, index, span } => {
                Error::ListGetOutOfBounds { len, index }.at_opt(span)
            }
            ReduceError::ListSliceOutOfRange {
                len,
                start,
                length,
                span,
            } => Error::ListSliceOutOfRange { len, start, length }.at_opt(span),
            ReduceError::DivisionByZero { kind, span } => {
                Error::DivisionByZero { kind }.at_opt(span)
            }
            ReduceError::IntToNatNegative { value, span } => Error::IntToNatNegative {
                value: Box::new(value),
            }
            .at_opt(span),
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

    pub(crate) fn stranded_sequencing<V: Into<Term>>(sequenced: Option<Term>, region: V) -> Self {
        Self::StrandedSequencing {
            sequenced: sequenced.map(Box::new),
            region: Box::new(region.into()),
        }
    }

    pub(crate) fn postponed_check<U: Into<Term>>(expected: U) -> Self {
        Self::PostponedCheck {
            expected: Box::new(expected.into()),
        }
    }

    pub(crate) fn postponed_conversion<T: Into<Term>, U: Into<Term>>(
        this: T,
        that: U,
        watching: Vec<String>,
        under_refinements: bool,
        deferred_witnesses: Vec<String>,
    ) -> Self {
        Self::PostponedConversion {
            this: Box::new(this.into()),
            that: Box::new(that.into()),
            watching,
            under_refinements,
            deferred_witnesses,
        }
    }

    pub(crate) fn bang_region_undetermined() -> Self {
        Self::BangRegionUndetermined
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

    pub(crate) fn not_list_type<U: Into<Term>>(head_type: U) -> Self {
        Self::NotListType {
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

    pub(crate) fn no_witness<T: Into<Term>>(
        goal: T,
        func: String,
        binder: String,
        embedding: Option<EmbeddingDiagnosis>,
    ) -> Self {
        Self::NoWitness {
            goal: Box::new(goal.into()),
            func,
            binder,
            embedding,
        }
    }

    pub(crate) fn witness_cycle<U: Into<Term>, V: Into<Term>>(this: U, that: V) -> Self {
        Self::WitnessCycle {
            this: Box::new(this.into()),
            that: Box::new(that.into()),
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

    pub(crate) fn parameterless_witness_concept<N: Into<String>, C: Into<String>>(
        witness: N,
        concept: C,
    ) -> Self {
        Self::ParameterlessWitnessConcept {
            witness: witness.into(),
            concept: concept.into(),
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

    pub(crate) fn nat_overflow(value: Natural) -> Self {
        Self::NatOverflow { value }
    }

    pub(crate) fn erased_module_invalid(detail: String) -> Self {
        Self::ErasedModuleInvalid { detail }
    }

    pub(crate) fn int_overflow(value: Integer) -> Self {
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

    /// The collision-aware rename map axis (a) needs: one map over every name this error's terms mention, so `inferred` and `expected` agree on what each name means. The axis-(b) shorten map rides along so globals are reserved under the rendering they actually display.
    fn rename_map(&self, shorten: &HashMap<Global, String>) -> Rc<HashMap<Free, String>> {
        let mut terms = Vec::new();
        self.collect_terms(&mut terms);

        let mut names = BTreeSet::new();
        for term in terms {
            names.extend(display_names(term));
        }

        Rc::new(build_rename(&names, shorten))
    }

    /// Render this error with source-style names, shortening global names against `module`'s symbols together with `scope`'s (axis (b)) — the qualified-name universe an error's globals are spelled relative to. Every elaboration error reaching a reader comes through here, so all three axes are set in one place; axis (c) belongs to the whole render rather than any one variant, since every error that prints a term prints it from the raw elaborated spelling.
    pub fn format_with(&self, module: &Module, scope: &[&Module]) -> String {
        Report::render_all(&self.reports_with(module, scope))
    }

    /// [`Error::format_with`] as data: one [`Report`] per thing said, located. See [`Error::reports_with_hints`].
    pub fn reports_with(&self, module: &Module, scope: &[&Module]) -> Vec<Report> {
        self.reports_with_hints(module, scope, &BTreeMap::new(), &Imports::default())
    }

    /// [`Error::format_with`], with two tables of the text stage's. `unbound` is what each unresolved bare name could have meant — keyed by the binder the name lowered to, valued by the absolute paths of the public bindings in scope that carry it; an `unbound variable` report whose binder the table knows gets a line per candidate, and every other error ignores it. `imports` is what the unit's `use` declarations brought into scope, with the spelling each resolves under; a global the table knows displays under its shortest such spelling rather than its shortest unambiguous suffix, since the suffix is not always a name in scope (`/sys/Nat/add` shortens to `add`, which nothing imported) while the written path is by construction — which is what makes a suggested imported candidate pasteable. Both tables are the text stage's because only it sees re-exports — `/std/Bool` is a `pub use`, and Core holds the `/sys/Bool/Bool` it stands for — and they arrive here rather than on the error because the error records what was written and nothing about where.
    pub fn format_with_hints(
        &self,
        module: &Module,
        scope: &[&Module],
        unbound: &BTreeMap<Free, Vec<Qualifier>>,
        imports: &Imports,
    ) -> String {
        Report::render_all(&self.reports_with_hints(module, scope, unbound, imports))
    }

    /// [`Error::format_with_hints`] as data, and the primitive it renders: every error is one report at its innermost span, except a goal batch, which is one report *per goal* at that goal's own occurrence — a goal's identity is its source location, and a consumer placing each where it was written needs them apart. Rendering the list is exactly the text the compile path prints, so the located form and the printed form cannot drift.
    pub fn reports_with_hints(
        &self,
        module: &Module,
        scope: &[&Module],
        unbound: &BTreeMap<Free, Vec<Qualifier>>,
        imports: &Imports,
    ) -> Vec<Report> {
        // Everything a reader could see: `module`'s own declarations *and* whatever its environment put in scope. A module carries only its own, so both halves of the spelling have to be told the prelude exists — the shortening table to know `Vec` is an unambiguous suffix, and the plicity marks to know `Eq`'s first parameter is implicit.
        //
        // Taking the scope as a `Module` rather than as one of its projections is deliberate: this was first fixed by passing a name slice, which repaired the shortening and left the plicities reading a module that no longer holds the prelude. A second projection would have been a second thing to forget.
        let mut symbols = module.module_symbols();
        let mut plicities = module.nominal_plicities();
        for unit in scope {
            symbols.extend(unit.module_symbols());
            for (name, marks) in unit.nominal_plicities() {
                plicities.entry(name).or_insert(marks);
            }
        }

        let mut shorten = build_shorten(&symbols);
        for (global, spelling) in imports.spellings() {
            shorten.insert(global, spelling.to_string());
        }
        let shorten = Rc::new(shorten);
        let spelling = Rc::new(
            Spelling::default()
                .with_pretty_names(self.rename_map(&shorten))
                .with_short_names(shorten)
                .with_nominal_plicities(Rc::new(plicities))
                .with_erased_universes()
                .with_anonymous_metavars(),
        );
        let suggestion = self.unbound_suggestion(unbound, &spelling);
        self.reports(&spelling, suggestion)
    }

    /// The lines an `unbound variable` report adds from the text stage's table, or `None` for any other error or an unknown binder. A candidate nested below a root is offered both ways it can be reached — through its parent's name, `Eq/cong` once `Eq` is in scope, and by its own import; a root's direct child has no route shorter than the import or the absolute path.
    fn unbound_suggestion(
        &self,
        unbound: &BTreeMap<Free, Vec<Qualifier>>,
        spelling: &Rc<Spelling>,
    ) -> Option<String> {
        let term = match self {
            Self::Located { error, .. } | Self::InDeclaration { error, .. } => {
                return error.unbound_suggestion(unbound, spelling);
            }
            Self::UnboundVariable { term } => term,
            _ => return None,
        };
        let Subterm::Var(var) = &***term else {
            return None;
        };
        let candidates = unbound.get(var.unwrap())?;
        let written = term.spelled(spelling).to_string();

        let lines = candidates
            .iter()
            .map(|candidate| {
                let module = candidate.without_last();
                let import = format!("`use {}/{{{written}}};`", module.join());
                match module.segments().len() {
                    0 | 1 => format!(
                        "  `{written}` is `{}`: write it absolute, or {import}",
                        candidate.join()
                    ),
                    _ => format!(
                        "  `{written}` is `{}`: write `{parent}/{written}` if `{parent}` is imported, or {import}",
                        candidate.join(),
                        parent = module.last()
                    ),
                }
            })
            .collect::<Vec<_>>();
        (!lines.is_empty()).then(|| lines.join("\n"))
    }

    /// One report per rendered error, at the innermost span attached to it — or one per goal for a batch, each at its own occurrence, under whatever declaration prefix the wrappers add.
    ///
    /// [`Error::at`] is first-wins *per wrapper*, so the innermost span is the first one stamped — but `in_declaration` may wrap a located error, after which a further `at` sees a non-`Located` head and stamps again, leaving the coarser span outermost. Locating therefore searches for the innermost rather than reading the outermost, and the message body is assembled separately so a nested `Located` cannot swallow it: `Display` for the wrappers deliberately prints no snippet, and a body rendered through `to_string` would drop the inner span silently.
    fn reports(&self, spelling: &Rc<Spelling>, suggestion: Option<String>) -> Vec<Report> {
        if let Self::Goals(goals) = self.unwrapped() {
            let prefix = self.declaration_prefix();
            return goals
                .iter()
                .map(|goal| Report {
                    span: goal.span.clone(),
                    message: format!("{prefix}{}", goal_text(goal, spelling)),
                })
                .collect();
        }

        let mut body = self.render_body(spelling);
        if let Some(suggestion) = suggestion {
            body.push('\n');
            body.push_str(&suggestion);
        }
        vec![Report {
            span: self.innermost_span().cloned(),
            message: body,
        }]
    }

    /// The error under every wrapper.
    fn unwrapped(&self) -> &Self {
        match self {
            Self::Located { error, .. } | Self::InDeclaration { error, .. } => error.unwrapped(),
            error => error,
        }
    }

    /// What the wrappers prefix a body with — `render_body`'s own lines for them, without the body.
    fn declaration_prefix(&self) -> String {
        match self {
            Self::Located { error, .. } => error.declaration_prefix(),
            Self::InDeclaration { name, error } => {
                format!("while elaborating {name}:\n{}", error.declaration_prefix())
            }
            _ => String::new(),
        }
    }

    /// The message without any snippet — wrappers are transparent, every other variant renders through its own `Display`.
    fn render_body(&self, spelling: &Rc<Spelling>) -> String {
        match self {
            Self::Located { error, .. } => error.render_body(spelling),
            Self::InDeclaration { name, error } => {
                format!("while elaborating {name}:\n{}", error.render_body(spelling))
            }
            error => Displayed(error, Rc::clone(spelling)).to_string(),
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
            Self::StrandedSequencing { sequenced, region } => {
                out.extend(sequenced.as_deref());
                out.push(region);
            }
            Self::NotAFunction { head_type }
            | Self::NotATuple { head_type }
            | Self::NotNatType { head_type }
            | Self::NotBoolType { head_type }
            | Self::NotListType { head_type }
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
            Self::PostponedConversion { this, that, .. } => {
                out.push(this);
                out.push(that);
            }
            Self::NoWitness {
                goal, embedding, ..
            } => {
                out.push(goal);
                if let Some(diagnosis) = embedding {
                    out.push(&diagnosis.source);
                    out.push(&diagnosis.target);
                }
            }
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

/// The faithful rendering: core's own names, every universe shown. Diagnostics go through [`Error::format_with`], which supplies a [`Spelling`].
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Displayed(self, Rc::new(Spelling::default())).fmt(f)
    }
}
