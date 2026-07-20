//! The erased program as flat, explicit, first-order data — the replacement
//! representation specified by `documentation/01_ERSD_V2_SPEC.md`.
//!
//! Module-owned arenas addressed by typed `u32` identities hold blocks of
//! single-operation statements over atomic operands; ordered top-level items
//! plus an entry block form the module's top level. The alphabet is erased
//! Core's vocabulary with its semantic identities intact — distinct scalar
//! shapes, schema-carrying products and variants, Bool and Nat switches, and
//! first-class Nat/sequence folds — because every encoding decision (carriers,
//! tag layouts, dispatch, loop synthesis) belongs exclusively to the lowering
//! into Cont. Functions store no capture lists; free values, uses, the call
//! graph, and recursive components are derived on demand.
//!
//! This module tree coexists with the legacy recursive representation at the
//! crate root during construction; the temporary `Erased` name prefixes
//! disambiguate the clash and are removed, with the nesting, when the flip
//! deletes the legacy path.

mod id;
pub use id::*;

mod atom;
pub use atom::*;

mod operation;
pub use operation::*;

mod sequence;
pub use sequence::*;

mod schema;
pub use schema::*;

mod node;
pub use node::*;

mod module;
pub use module::*;

mod verify;
pub use verify::*;

mod build;
pub use build::*;

mod print;

mod walk;

mod analysis;
pub use analysis::*;

mod into_cont;
pub use into_cont::lower_to_cont;

mod semantics;
pub use semantics::*;

mod summary;
pub use summary::*;

mod optimize;
pub use optimize::optimize_ir;

#[cfg(test)]
mod module_tests;

#[cfg(test)]
mod build_tests;

#[cfg(test)]
mod verify_tests;

#[cfg(test)]
mod print_tests;

#[cfg(test)]
mod analysis_tests;

#[cfg(test)]
mod into_cont_tests;

#[cfg(test)]
mod semantics_tests;
