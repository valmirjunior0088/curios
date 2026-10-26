//! The elaborator of the Curios compiler, between `curios-text` (whose `into_core` lowers surface syntax into a `curios_core::Module`) and `curios-ersd` (which consumes the erased output of [`erase_unit`]). The kernel that independently re-checks what this crate accepts is `curios-cert`.
//!
//! The stage runs module-at-a-time: [`elaborate_and_zonk_module`] walks a lowered `Module` item by item, elaborating each definition (bidirectional infer/check with implicit-argument insertion, witness resolution, and infix/numeric-literal overload resolution) under a [`Context`] that accumulates metavariables, inductive/struct/concept declarations, and the program-wide witness table; each item is zonked as it is published, which substitutes solved metavariables and reports unsolved holes; `into_ersd` ([`erase_unit`]) strips types, proofs, and other runtime-irrelevant structure for `curios-ersd`.
//!
//! # What this stage starts from
//!
//! A compilation unit is elaborated *against* what is already in scope, and that scope is not part of the unit. [`Established`] carries it for elaboration, as a view a context reads through to where its own stores miss — a name's type and body, a registry entry, the witness at a key, each by reference into the unit that holds it — and [`Erasures`] carries it for erasure, pairing the Core of each unit in scope, read the same way out of its universes, with what its items erased to. `Established::nothing()` is a from-scratch elaboration, so one protocol serves both entry points instead of two implementations kept in agreement by reading.
//!
//! That they are types rather than a re-seeded context and two values passed apart is `documentation/design/compilation/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`'s decision; the lowered module carries only its own items.
//!
//! `Type` is internally indexed by canonical algebraic `Level`s even though the surface term is nullary. Written levels and elaborator classifiers occupy distinct `UniverseMetaId`s, each carrying where it came from — a `Type` written in an input position is chosen, a use's instance level an occurrence, any other inferred; inequalities are solved transactionally with term metavariables, and declaration finalization settles each level by where it stands — one the signature holds where conversion compares it kept as the declaration's parameter, an occurrence nothing holds at its least value, a chosen level bounded only by one other chosen level at that level — before generalizing what is left into a closed `UniverseContext`. A written type therefore sits where its reduct does, which the kernel relies on when it types one as written (`documentation/design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md`). Top-level and local definitions instantiate those contexts freshly, while every member of a recursive group shares one monomorphic instance internally. [`validate_universes`] is the closure and instance-arity gate on zonked Core. The Core-to-Ersd boundary then projects through a validated universe-erased module, removing instances, contexts, and nominal vectors before any runtime representation is built.
//!
//! Everything else is that pipeline's machinery: `reduce` is budget-bounded type-level evaluation (`normalize` for full normal forms); `convert` decides definitional equality, solving metavariables and distinguishing hard mismatches from goals merely blocked on unsolved metas (`Outcome`) so `typing` can park and retry them; `resolve` implements witness (concept) resolution with global coherence checks. The term language and its printer belong to `curios-core`, and the shared inversion, positivity, and totality analyses to `curios-analysis`; this crate drives those analyses and renders their refusals as [`Error`]s.
//!
//! The crate is a flat module space: every module re-exports at the root, so downstream crates use `curios_elab::Context`, not paths into the modules.

mod universe_solver;
use universe_solver::*;

mod concept;
use concept::*;

mod derive;
use derive::*;

mod positivity;
use positivity::*;

mod variance;
use variance::*;

mod totality;
use totality::*;

mod reduce;
use reduce::*;

mod context;
use context::*;
pub use context::{Context, DEFAULT_BUDGET};

mod convert;
use convert::*;

mod denoise;
use denoise::*;

mod error;
use error::*;
pub use error::{Error, Reading};

mod report;
use report::*;

mod typing;
use typing::*;

mod resolve;
use resolve::*;

mod suggest;
use suggest::*;

mod written;
use written::*;

mod entailment;
use entailment::*;

mod established;
pub use established::Established;

mod traces;
use traces::*;
pub use traces::{HeldTrace, Readers, TraceRecord, TraceTable, TracedEntry, Traces};

mod same;
use same::*;

mod candidate;
use candidate::*;
pub use candidate::{Candidating, declaration_input, lowered_digest};

mod elaborate;
use elaborate::*;
pub use elaborate::{
    FinalizedProgram, Recompile, Recompiled, Tail, elaborate_and_zonk_entry,
    elaborate_and_zonk_entry_over, elaborate_and_zonk_module, elaborate_and_zonk_program,
    elaborate_and_zonk_program_reporting, elaborate_and_zonk_unit, elaborate_and_zonk_unit_over,
};

#[cfg(test)]
mod walk_tests;

mod into_ersd;
use into_ersd::*;
pub use into_ersd::{
    ErasedBaseline, ErasedUnit, Erasures, Projection, erase_program, erase_unit, erase_unit_over,
};

mod zonk;
use zonk::*;
pub use zonk::{validate_entry_universes, validate_lowered_universe_seeds, validate_universes};

mod proof_binders;
use proof_binders::*;

mod test_tail;
pub use test_tail::ScheduledTest;
use test_tail::*;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_elab::test_support::declared_families` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(feature = "test-support")]
pub mod test_support;
