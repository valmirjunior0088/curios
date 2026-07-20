//! Core erasure into the arena erased representation ([`curios_ersd::ir`]) —
//! the transcription defined by the Ersd v2 specification.
//!
//! This is the replacement counterpart to the legacy [`erase`](crate::erase):
//! it consumes the same meta-free Core [`Module`] and lowers it through the
//! checked [`curios_ersd::ErsdBuilder`] into a verified
//! [`curios_ersd::ErasedModule`], preserving the semantic identities the
//! legacy path desugared away — distinct `Bln`/`Byte` shapes, first-class
//! switches and folds, schema-carrying products and variants. Every encoding
//! decision (carriers, tag layouts, dispatch, loop synthesis) belongs to the
//! later lowering out of the representation, not to erasure.
//!
//! Erasure is a transcription under the **operand law**: every source
//! subexpression erases to exactly one operand ([`curios_ersd::ErasedAtom`]) —
//! an atomic value directly, a compound one bound by a statement in the
//! builder's innermost open block, in evaluation order — and every reuse
//! references the bound atom, never a re-erased copy. Divergence is explicit:
//! an expression that provably never yields a value (a process exit, a vacuous
//! elimination) reports the terminator that seals its block instead of an
//! atom, and dead code after it is never erased.
//!
//! Core classifies and traverses; the builder owns construction. The stage
//! coexists with the legacy path and has no production consumer yet — it is
//! exercised by tests until the lowering vertical and the flip land.

use {
    super::{
        Apply, Atom, Bound, Carrier, Cases, Context, Error, Field, Func, FuncType, Inductive,
        InductiveType, Item, Let, Many, Match, Module, MotivePattern, Nat, Prim, PrimHead, Proj,
        Scope, Struct, StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two, Variant,
        erasure_mask, expect_prim_head, infer, is_erasable, pattern_binder_slots, reduce_with,
        refine_head, wire_term,
    },
    curios_base::Qualifier,
    num_bigint::BigUint,
    num_traits::ToPrimitive,
    std::collections::{BTreeMap, BTreeSet},
};

mod environment;
use environment::*;

mod lower;
pub use lower::erase_module_to_ir;
use lower::{Lowering, Outcome};

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod prim;

#[cfg(test)]
mod tests;

/// Unwrap an [`Outcome`] to its emitted atom, propagating divergence to the
/// caller (the rest of the enclosing block is dead and is never erased).
macro_rules! emitted {
    ($outcome:expr) => {
        match $outcome {
            $crate::erase_ir::Outcome::Emitted(atom) => atom,
            diverged @ $crate::erase_ir::Outcome::Diverged(_) => return Ok(diverged),
        }
    };
}
use emitted;
