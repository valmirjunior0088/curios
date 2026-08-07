//! The Curios certifier: the trusted base as a crate.
//!
//! This crate holds every rule that can admit a program — the kernel deciding, from a finished term alone, whether it is well-typed, the whole-module walk that applies it ([`recheck_module`]), and the analyses both checkers share: index inversion and the singleton determination walk ([`invert_indices`], [`pinned_by_targets`]), strict positivity ([`positivity_vectors`]), size-change totality ([`group_totality`]). The level entailment oracle ([`entails`]) is *not* among them despite sitting beside them: it takes a constraint set rather than an [`Env`], so nothing reaches it through the seam, and `Kernel::level_leq` is its only caller anywhere — the elaborator decides its own level questions. What a term *is* — the representation, its binder discipline, the primitive roster and its folds — belongs to `curios-core`, which this crate builds on and which is the only thing it shares with the elaborator: sharing the representation is not sharing a judgment.
//!
//! The dependency direction is the whole point. `curios-elab` depends on this crate and on `curios-core`, and neither dependency ever reverses, so the kernel cannot consult a metavariable store, a refinement layer, or a cached elaboration — independence is a property of the crate graph, and with the judgments in their own crate the trusted base is an enumerable boundary (`cargo tree -p curios-cert`) rather than a call-closure someone traces. The decision record is `documentation/DESIGN.md`, "An independent kernel re-checks what the elaborator accepts".
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_cert::Kernel` and `curios_cert::convert`. The crate name itself is what keeps the two checkers tellable apart — the judgments here name the same things the elaborator names its own, and `curios_cert::convert` versus the elaborator's bare `convert` reads exactly as the second opinion it is.

mod satisfy;
pub use satisfy::*;

mod entail;
pub(crate) use entail::*;

mod obligation;
pub use obligation::*;

mod recheck;
pub use recheck::*;

mod judge;
pub use judge::*;

mod invert;
pub use invert::*;


mod positivity;
pub use positivity::*;

mod totality;
pub use totality::*;

mod kernel;
pub use kernel::*;
