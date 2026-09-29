//! The Curios certifier: the trusted base as a crate.
//!
//! This crate holds the rules only *this* checker runs: the kernel deciding, from a finished term alone, whether a term is well-typed, the whole-module walk that applies it ([`recheck_module`]), the erasure obligations, and the two decisions over a universe constraint set — the level entailment oracle (`entails`) and satisfiability ([`satisfiable`]) — which sit here rather than with the shared analyses because they take a constraint set rather than an `Env`, so nothing reaches them through the seam, and the kernel is their only caller: `Kernel::level_leq` for the first, the walk's universe verdict for the second. `curios-elab`'s tests call `satisfiable` too, to hold its solver's decision against it.
//!
//! The rules **both** checkers run are `curios-analysis`'s: the `Env`/`Judge` seam, index inversion, strict positivity, and size-change totality's grading of a call and closure of a group's calls — which calls a group makes, this kernel records as it types them. They are a separate crate because `curios-elab` needs them and does not need a kernel — so a kernel edit invalidates this crate and not elaboration, where before it re-elaborated the whole fixed prelude. They are not re-exported here; a consumer names the crate it wants a rule from.
//!
//! So the trusted base is two crates, and `cargo tree -p curios-cert` still enumerates it: `curios-analysis` is in the closure, one level out. `documentation/soundness/` grades the rules of both. What a term *is* — the representation, its binder discipline, the intrinsic roster and its folds — belongs to `curios-core`, which this crate builds on and which is the only thing it shares with the elaborator: sharing the representation is not sharing a judgment.
//!
//! The dependency direction is the whole point. `curios-elab` depends on `curios-core` and takes this crate only as a dev-dependency, for the tests that put one fixture to both checkers, and neither dependency ever reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration — independence is a property of the crate graph, and with the judgments in their own crate the trusted base is an enumerable boundary (`cargo tree -p curios-cert`) rather than a call-closure someone traces. The decision record is `documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`.
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_cert::Kernel` and `curios_cert::convert`. The crate name itself is what keeps the two checkers tellable apart — the judgments here name the same things the elaborator names its own, and `curios_cert::convert` versus the elaborator's bare `convert` reads exactly as the second opinion it is.

mod entail;
pub(crate) use entail::*;

mod level_model;
pub(crate) use level_model::*;

mod satisfy;
pub use satisfy::*;

mod obligation;
pub(crate) use obligation::*;

mod recheck;
pub use recheck::*;

mod kernel;
pub use kernel::*;

#[cfg(test)]
mod walk_tests;
