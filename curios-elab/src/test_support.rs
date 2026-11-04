//! Scaffolding for tests that print a finished term as a report does, the discharged bound a fold's suite stands in, and the elaborator's errors by name and by where each was raised, which the soundness board's records and the suites that guard a rule name a refusal by.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests, which share it.

use {
    crate::*,
    curios_common::{SyntaxRegistry, variants},
    curios_core::{Free, Global, Term},
};

impl Error {
    /// The error beneath what located it, where it was raised: what a refusal is named by.
    pub fn raised(&self) -> &Error {
        let mut error = self;
        while let Error::Located { error: inner, .. }
        | Error::InDeclaration { error: inner, .. }
        | Error::InUnreachableArm { error: inner, .. }
        | Error::InScope { error: inner, .. } = error
        {
            error = inner;
        }

        error
    }

    /// The declaration the error was raised in, read beneath what located it: the innermost one a wrapper names, and none where it was raised at a program's closing term.
    pub fn declared_in(&self) -> Option<&Global> {
        match self {
            Error::InDeclaration { names, error } => error.declared_in().or(names.first()),
            Error::Located { error, .. }
            | Error::InUnreachableArm { error, .. }
            | Error::InScope { error, .. } => error.declared_in(),
            _ => None,
        }
    }
}

/// `term`, read over the units `established` holds, as a report spells it for a reader standing at the top of the declaration it belongs to: each witness resolution would put back there left as an open slot, each method projected off one called as a program calls it (`written`). What a report settles where it is made, a test that prints a finished declaration settles here.
pub fn written_for_reader(
    established: &Established<'_>,
    syntax: SyntaxRegistry,
    term: &Term,
) -> Term {
    let mut context = Context::with_default_budget(syntax);
    context.read_through(established);

    crate::written(&mut context, term)
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and the suites that take it are about the fold laws rather than the obligation.
pub fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

variants!(Error {
    ReduceExhausted,
    ConvertExhausted,
    BytesGetOutOfBounds,
    BytesSliceOutOfRange,
    ListGetOutOfBounds,
    ListSliceOutOfRange,
    DivisionByZero,
    IntToNatNegative,
    NatToByteAbove,
    UniverseInconsistency,
    UniverseInvariant,
    ByteLiteralOutOfRange,
    BoolLiteralOutOfRange,
    FltLiteralOutOfRange,
    TypeMismatch,
    RejectedCandidate,
    StrandedSequencing,
    UnembeddedAction,
    NotAFunction,
    NotAFunctionType,
    NotATuple,
    NotATupleType,
    TupleArityMismatch,
    TupleIndexOutOfBounds,
    UnknownTupleLabel,
    DuplicateTupleLabel,
    DuplicateInduct,
    DuplicateStruct,
    DuplicateConcept,
    TupleFieldNameMismatch,
    NotNatType,
    NotBoolType,
    NotListType,
    NotBytesType,
    WrongNumberOfArguments,
    SurplusFuncBinders,
    BinderPlicityMismatch,
    UnknownMatchConstructor,
    MatchCaseMissing,
    CtorArityMismatch,
    MatrixInconsistentShape,
    MatrixDuplicateRow,
    MatrixUncovered,
    MatrixArmsMeet,
    MatrixStructHead,
    MatrixMixedNatDispatch,
    NotAInductType,
    LargeElimOfProp,
    SortDecided,
    SortDecidedTwice,
    TooManySorts,
    RecordOverTwoSorts,
    InformativePropStruct,
    NotStrictlyPositive,
    PartialInErasedPosition,
    NotAStructType,
    StructArityMismatch,
    WrongNumberOfFields,
    UnknownDeclaration,
    UnknownStructField,
    UseEntryOutsideConcept,
    SpreadNotFirst,
    MultipleSpreads,
    SpreadBaseTypeMismatch,
    UnlabeledSpreadOverride,
    SpreadOverrideOutOfOrder,
    PrivateField,
    PrivateRepresentation,
    NoDerivation,
    Underivable,
    DeriveOutsideWitness,
    UnboundVariable,
    CannotInfer,
    PostponedCheck,
    PostponedConversion,
    AmbiguousSpines,
    BangRegionUndetermined,
    BangRegionNotAMonad,
    OperatorUndefined,
    UninferredImplicit,
    ImplicitAtOtherSort,
    DomainNeverDetermined,
    WitnessMemberNeverStated,
    HiddenMemberOutOfOrder,
    HiddenMemberWithoutSlot,
    NoWitness,
    DeclarationCycle,
    GroupApart,
    Goal,
    Goals,
    DuplicateWitness,
    OrphanWitness,
    AmbiguousWitness,
    CyclicSuperclass,
    UnknownSuperclass,
    InvalidWitnessHead,
    WitnessKeyNotSpelled,
    ParameterlessWitnessConcept,
    WitnessNotAConcept,
    UseParameterNotAConcept,
    ImplicitConceptMemberUnfillable,
    NonRegularWitnessPremise,
    ExplicitWitnessParam,
    MotiveBinderCount,
    MissingArmNotImpossible,
    Poisoned,
    Unfinished,
    Pending,
    Located,
    InDeclaration,
    InUnreachableArm,
    InScope,
    Batch,
});
