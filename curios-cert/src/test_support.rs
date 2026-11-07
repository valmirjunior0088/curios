//! What another crate's suite reads of the kernel and no judgment does: its errors by name, which the soundness board's records name a refusal by.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {crate::Error, curios_common::variants};

variants!(Error {
    Reduce,
    Unbound,
    NotClosed,
    Undeclared,
    UndeclaredCase,
    Redeclared,
    Unclassified,
    NotASort,
    OpenSort,
    AtSorts,
    TooManySorts,
    NotAMotive,
    AmbientFold,
    Mismatch,
    NotAFunction,
    NotATuple,
    NotASlotOf,
    Arity,
    Mark,
    LargeElimination,
    NotCore,
    IndexedByItsGroup,
    NeedsItsOwnTyping,
    RepeatedTag,
    MissingArm,
    NotDescending,
    NotPositive,
    VarianceDenied,
    Oversized,
    NotTotal,
    Informative,
    UntypedEntry,
    UnclosedUniverses,
    UnsatisfiableUniverses,
    UniverseInstance,
});
