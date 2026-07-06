//! The core calculus of the Curios compiler: the dependently-typed kernel between `curios-text` (whose `to_core` lowers surface syntax into this crate's [`Term`]) and `curios-ersd` (which consumes [`erase_module`]'s erased output).
//!
//! The stage runs module-at-a-time: [`elaborate_module`] walks a lowered [`Module`] item by item, elaborating each definition (bidirectional infer/check with implicit-argument insertion, witness resolution, and infix/numeric-literal overload resolution) under a [`Context`] that accumulates metavariables, inductive/struct/concept declarations, and the program-wide witness table; `zonk`/[`zonk_module`] then substitute solved metavariables and report unsolved holes; [`erase_module`] strips types, proofs, and other runtime-irrelevant structure for `curios-ersd`.
//!
//! Everything else is that pipeline's machinery: `term`/`scope` define the term language and its locally-nameless binder discipline ([`Scope`], [`Telescope`], [`Bound`]); `reduce` is deadline-bounded type-level evaluation (`normalize` for full normal forms); `convert` decides definitional equality, solving metavariables and distinguishing hard mismatches from goals merely blocked on unsolved metas (`Outcome`) so `typing` can park and retry them; `resolve` implements witness (concept) resolution with global coherence checks; `invert` proves omitted match arms impossible; `print`/`names` render terms for [`Error`] messages.
//!
//! The crate is a flat module space: every module re-exports at the root, so downstream crates use `curios_core::Term`, not paths into the modules.

mod time;
use time::*;

mod scope;
pub use scope::*;

mod int;
pub use int::*;

mod flt;
pub use flt::*;

mod nat;
pub use nat::*;

mod prim;
pub use prim::*;

mod spine;
pub(crate) use spine::*;

mod free_monoid;
use free_monoid::*;

mod names;
pub use names::*;

mod qualifier;
pub use qualifier::*;

#[cfg(test)]
mod qualifier_tests;

mod term;
pub use term::*;

#[cfg(test)]
mod term_tests;

mod module;
pub use module::*;

mod inductive;
pub use inductive::*;

mod structure;
pub use structure::*;

mod concept;
pub use concept::*;

#[cfg(test)]
mod concept_tests;

mod syn_names;
pub(crate) use syn_names::*;

mod print;
use print::*;

mod reduce_prim;
use reduce_prim::*;

mod reduce;
pub(crate) use reduce::*;

#[cfg(test)]
mod reduce_tests;

mod context;
pub use context::*;

mod convert_prim;
use convert_prim::*;

mod convert;
pub(crate) use convert::*;

#[cfg(test)]
mod convert_tests;

mod error;
pub use error::*;

mod typing;
pub(crate) use typing::*;

mod resolve;
pub(crate) use resolve::*;

#[cfg(test)]
mod typing_tests;

mod invert;
pub(crate) use invert::*;

mod elaborate_prim;
use elaborate_prim::*;

mod elaborate;
pub use elaborate::*;

mod elaborate_match;
use elaborate_match::*;

mod elaborate_module;
pub use elaborate_module::*;

#[cfg(test)]
mod elaborate_tests;

mod erase_prim;
use erase_prim::*;

mod erase;
pub use erase::*;

#[cfg(test)]
mod erase_tests;

mod zonk;
pub use zonk::*;

#[cfg(test)]
mod zonk_tests;
