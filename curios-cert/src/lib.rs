//! The Curios certifier: the trusted base as a crate.
//!
//! This crate holds the rules only *this* checker runs: the kernel deciding, from a finished term alone, whether a term is well-typed, the whole-module walk that applies it ([`recheck_module`]), the erasure obligations, and the level entailment oracle (`entails`) — which sits here rather than with the shared analyses because it takes a constraint set rather than an `Env`, so nothing reaches it through the seam, and `Kernel::level_leq` is its only caller anywhere.
//!
//! The rules **both** checkers run are `curios-analysis`'s: the `Env`/`Judge` seam, index inversion, strict positivity, size-change totality, universe satisfiability. They are a separate crate because `curios-elab` needs them and does not need a kernel — so a kernel edit invalidates this crate and not elaboration, where before it re-elaborated the whole fixed prelude. They are not re-exported here; a consumer names the crate it wants a rule from.
//!
//! So the trusted base is two crates, and `cargo tree -p curios-cert` still enumerates it: `curios-analysis` is in the closure, one level out. `documentation/soundness.md` grades the rules of both. What a term *is* — the representation, its binder discipline, the intrinsic roster and its folds — belongs to `curios-core`, which this crate builds on and which is the only thing it shares with the elaborator: sharing the representation is not sharing a judgment.
//!
//! The dependency direction is the whole point. `curios-elab` depends on this crate and on `curios-core`, and neither dependency ever reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration — independence is a property of the crate graph, and with the judgments in their own crate the trusted base is an enumerable boundary (`cargo tree -p curios-cert`) rather than a call-closure someone traces. The decision record is `documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`.
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_cert::Kernel` and `curios_cert::convert`. The crate name itself is what keeps the two checkers tellable apart — the judgments here name the same things the elaborator names its own, and `curios_cert::convert` versus the elaborator's bare `convert` reads exactly as the second opinion it is.

// The `/syn` registry stand-in every kernel these tests build is handed. Shared with `curios-elab` and with `curios-analysis`'s own suite rather than copied into each, and gated there behind `test-support` so it reaches no build that ships.
#[cfg(test)]
pub(crate) use curios_analysis::fixture::SYNTAX;

mod entail;
pub(crate) use entail::*;

mod obligation;
pub(crate) use obligation::*;

mod recheck;
pub use recheck::*;

mod kernel;
pub use kernel::*;
