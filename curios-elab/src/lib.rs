//! The core calculus of the Curios compiler: the dependently-typed kernel between `curios-text` (whose `into_core` lowers surface syntax into this crate's [`Term`]) and `curios-ersd` (which consumes the erased output of [`erase_module_with_prelude`]).
//!
//! The stage runs module-at-a-time: [`elaborate_and_zonk_module`] walks a lowered [`Module`] item by item, elaborating each definition (bidirectional infer/check with implicit-argument insertion, witness resolution, and infix/numeric-literal overload resolution) under a [`Context`] that accumulates metavariables, inductive/struct/concept declarations, and the program-wide witness table; `zonk`/[`zonk_module`] then substitute solved metavariables and report unsolved holes; `into_ersd` ([`erase_module_with_prelude`]) strips types, proofs, and other runtime-irrelevant structure for `curios-ersd`.
//!
//! `Type` is internally indexed by canonical algebraic [`Level`]s even though the surface term is nullary. Written levels and elaborator classifiers occupy distinct [`UniverseMetaId`]s with explicit roles; inequalities are solved transactionally with term metavariables, and declaration finalization minimizes flexible outputs before generalizing the remaining inputs into a closed [`UniverseContext`]. Top-level and local definitions instantiate those contexts freshly, while every member of a recursive group shares one monomorphic instance internally. [`validate_universes`] is the closure and instance-arity gate on zonked Core. The Core-to-Ersd boundary then projects through a validated universe-erased module, removing instances, contexts, and nominal vectors before any runtime representation is built.
//!
//! Everything else is that pipeline's machinery: `term`/`scope` define the term language and its locally-nameless binder discipline ([`Scope`], [`Telescope`], [`Bound`]); `reduce` is budget-bounded type-level evaluation (`normalize` for full normal forms); `convert` decides definitional equality, solving metavariables and distinguishing hard mismatches from goals merely blocked on unsolved metas (`Outcome`) so `typing` can park and retry them; `resolve` implements witness (concept) resolution with global coherence checks; `invert` proves omitted match arms impossible; `print`/`names` render terms for [`Error`] messages.
//!
//! The crate is a flat module space: every module re-exports at the root, so downstream crates use `curios_elab::Term`, not paths into the modules.

/// The core language this stage elaborates into: terms, binders, primitives,
/// universes, and the nominal registry. Re-exported flat so the elaborator's
/// modules keep naming them unqualified, as they did when they lived here.
pub use curios_core::*;

mod universe_solver;
pub use universe_solver::*;

mod module;
pub use module::*;

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

mod error;
pub use error::*;

mod typing;
pub(crate) use typing::*;

mod resolve;
pub(crate) use resolve::*;

mod invert;
pub(crate) use invert::*;

mod elaborate;
pub use elaborate::*;

mod into_ersd;
pub use into_ersd::*;

mod zonk;
pub use zonk::*;
