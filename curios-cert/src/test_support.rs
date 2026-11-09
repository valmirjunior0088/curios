//! What another crate's suite reads of the kernel and no judgment does: its errors by name, which the soundness board's records name a refusal by, and a walk with its memos off.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {
    crate::{Error, Globals, Kernel, Verdict, recheck::verdicts_from},
    curios_common::{SyntaxRegistry, variants},
    curios_core::{Definitions, Zonked},
};

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

/// [`certify_definitions`](crate::certify_definitions)'s verdicts with the kernel's evaluation memos off.
///
/// Exists for one purpose: asserting that memoization changes no *semantic* verdict — the property that makes a memo an evaluation strategy rather than a store. A resource verdict may differ, since a term-keyed hit is free.
pub fn recheck_definitions_verdicts_uncached(
    definitions: &Zonked<Definitions>,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    verdicts_from(
        &mut Kernel::uncached(budget, syntax),
        definitions.as_definitions(),
        None,
        globals,
        None,
    )
    .verdicts
}
