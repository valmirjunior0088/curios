//! The Curios certifier: the trusted base as a crate.
//!
//! This crate holds the rules only *this* checker runs: the certifier deciding, from a finished term alone, whether a term is well-typed, the whole-module walk that applies it ([`certify_definitions`]), the erasure obligations, and the two decisions over a universe constraint set — the level entailment oracle (`entails`) and satisfiability ([`satisfiable`]) — whose one caller is the certifier, `Certifier::level_leq` for the first and the walk's universe verdict for the second, beside `curios-elab`'s tests, which hold its solver's decision against `satisfiable`; why the two sit here is `README.md`'s decision.
//!
//! What this checker shares with the elaborator is the layer both judge through. `curios-core` holds what a term *is* — the representation and its binder discipline — and three things beside it that decide: the literal folds of the intrinsic roster, which are arithmetic over `curios-num`'s scalar semantics; the linear views comparisons are read through; and the closed machine, a trusted evaluator both checkers enter, held to each one's own strategy by a differential. `curios-algebra` is the carriers' theory over abstract atoms, which conversion decides as part of its rule. `curios-analysis` holds the rules both run behind the `Env`/`Judge` seam — index inversion, what a case's solution re-types, strict positivity, size-change totality's grading of a call and closure of a group's calls, the erasure-shape queries and the chain conversion runs when two intrinsics meet. Which calls a group makes, this certifier records as it types them. Why `curios-analysis` is a crate of its own is its README's decision. None of the layer is re-exported to a consumer, who names the crate it wants a rule from. Inside this crate, a judgment imports each function of the layer from the crate that defines it, and the crate's suite lists every one a judgment calls, failing on one imported off the list or listed and imported nowhere.
//!
//! So the trusted base is this crate's dependency closure, which `cargo tree -p curios-cert` enumerates, and `documentation/design/soundness/` argues the rules it holds. Sharing the representation is not sharing a judgment. Why the base is a crate boundary, and why the judgments flatten onto the root, are `README.md`'s decisions; why the certifier depends on nothing of the elaborator is `documentation/design/soundness/an-independent-certifier-re-checks-what-the-elaborator-accepts.md`'s.
//!
//! The crate is a flat module space: its root names what another crate reads, so consumers use `curios_cert::Certifier` and `curios_cert::convert`.

mod entail;
use entail::*;

mod level_model;
use level_model::*;

mod satisfy;
pub use satisfy::satisfiable;
use satisfy::*;

mod obligation;
use obligation::*;

mod baseline;
use baseline::*;
pub use baseline::{Baseline, complete, part_digest};

mod recheck;
pub use recheck::{
    Rechecked, Verdict, certify_definitions, certify_definitions_over, certify_program,
};

mod certifier;
use certifier::*;
pub use certifier::{
    Certifier, Counted, EnteredArm, Error, Globals, Reader, carries_information, check, convert,
    infer,
};

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_cert::test_support` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

#[cfg(test)]
mod tests;
