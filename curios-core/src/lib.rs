//! The core calculus of the Curios compiler: the dependently-typed kernel between `curios-text` (whose `into_core` lowers surface syntax into this crate's [`Term`]) and `curios-ersd` (which consumes [`erase_module`]'s erased output).
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

mod term;
pub use term::*;

mod module;
pub use module::*;

mod inductive;
pub use inductive::*;

mod structure;
pub use structure::*;

mod concept;
pub use concept::*;

mod print;
use print::*;

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

mod erase;
pub use erase::*;

mod zonk;
pub use zonk::*;
