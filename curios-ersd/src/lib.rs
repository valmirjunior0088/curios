//! The erased, first-order IR — the pipeline stage between `curios-core`'s type-directed erasure and the continuation IR of `curios-cont`. Types, proofs, and erasable binders are gone by construction.
//!
//! The program is flat, explicit, first-order data: a [`Module`] of arena-allocated blocks of single-operation statements over atomic operands, addressed by typed `u32` identities, with ordered top-level items plus an entry block at its top level. The alphabet is erased Core's vocabulary with its semantic identities intact — distinct scalar shapes, schema-carrying products and variants, Bool and Nat switches, and first-class Nat/sequence folds — because every encoding decision (carriers, tag layouts, dispatch, loop synthesis) belongs exclusively to the lowering into Cont. Functions store no capture lists; free values, uses, the call graph, and recursive components are derived on demand ([`Analysis`]).
//!
//! `curios_core`'s `erase_ir` is the sole producer, constructing through the checked [`ErsdBuilder`]. [`optimize_ir`] is the Ersd-level optimizer (behavior-summary pruning, closed-term evaluation, literal-spine specialization, monoid worker/wrapper), and [`lower_to_cont`] is the one-way door where every encoding decision is made, lowering the module to a `curios_cont::CpsModule`.

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
