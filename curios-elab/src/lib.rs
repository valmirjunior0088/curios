//! The core calculus of the Curios compiler: the dependently-typed kernel between `curios-text` (whose `into_core` lowers surface syntax into this crate's [`Term`]) and `curios-ersd` (which consumes the erased output of [`erase_module_with_prelude`]).
//!
//! The stage runs module-at-a-time: [`elaborate_and_zonk_module`] walks a lowered [`Module`] item by item, elaborating each definition (bidirectional infer/check with implicit-argument insertion, witness resolution, and infix/numeric-literal overload resolution) under a [`Context`] that accumulates metavariables, inductive/struct/concept declarations, and the program-wide witness table; `zonk`/[`zonk_module`] then substitute solved metavariables and report unsolved holes; `into_ersd` ([`erase_module_with_prelude`]) strips types, proofs, and other runtime-irrelevant structure for `curios-ersd`.
//!
//! `Type` is internally indexed by canonical algebraic [`Level`]s even though the surface term is nullary. Written levels and elaborator classifiers occupy distinct [`UniverseMetaId`]s with explicit roles; inequalities are solved transactionally with term metavariables, and declaration finalization minimizes flexible outputs before generalizing the remaining inputs into a closed [`UniverseContext`]. Top-level and local definitions instantiate those contexts freshly, while every member of a recursive group shares one monomorphic instance internally. [`validate_universes`] is the closure and instance-arity gate on zonked Core. The Core-to-Ersd boundary then projects through a validated universe-erased module, removing instances, contexts, and nominal vectors before any runtime representation is built.
//!
//! Everything else is that pipeline's machinery: `reduce` is budget-bounded type-level evaluation (`normalize` for full normal forms); `convert` decides definitional equality, solving metavariables and distinguishing hard mismatches from goals merely blocked on unsolved metas (`Outcome`) so `typing` can park and retry them; `resolve` implements witness (concept) resolution with global coherence checks. The term language and its printer belong to `curios-core`, and the shared inversion, positivity, and totality analyses to `curios-cert`; this crate drives those analyses and renders their refusals as spanned [`Error`]s.
//!
//! The crate is a flat module space: every module re-exports at the root, so downstream crates use `curios_elab::Context`, not paths into the modules.

#[cfg(test)]
mod fixture;
#[cfg(test)]
pub(crate) use fixture::*;

mod builders;
pub use builders::*;

mod universe_solver;
pub use universe_solver::*;

mod concept;
pub use concept::*;

mod positivity;
pub use positivity::*;

mod totality;
pub use totality::*;

mod reduce;
pub(crate) use reduce::*;

mod context;
pub use context::*;

mod convert;
pub(crate) use convert::*;

mod denoise;
pub(crate) use denoise::*;

mod error;
pub use error::*;

mod typing;
pub(crate) use typing::*;

mod resolve;
pub(crate) use resolve::*;

mod suggest;
pub(crate) use suggest::*;

mod elaborate;
pub use elaborate::*;

mod into_ersd;
pub use into_ersd::*;

mod zonk;
pub use zonk::*;
