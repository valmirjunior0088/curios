//! The erased, first-order IR — the pipeline stage between `curios-core`'s type-directed erasure and the continuation IR of `curios-cont`. Types, proofs, and erasable binders are gone by construction: a program is an [`ErasedModule`] of arena-allocated blocks of single-operation statements over atomic operands, with ordered top-level items plus an entry block at its top level (see [`ir`]).
//!
//! `curios_core`'s `erase_ir` is the sole producer. [`optimize_ir`] is the Ersd-level optimizer (behavior-summary pruning, closed-term evaluation, literal-spine specialization, monoid worker/wrapper), and [`lower_to_cont`] is the one-way door where every encoding decision is made, lowering the module to a `curios_cont::CpsModule`.

pub mod ir;
pub use ir::*;
