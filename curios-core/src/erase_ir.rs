//! Core erasure into the erased representation ([`curios_ersd::Module`]).
//!
//! It consumes the meta-free Core [`Module`] and lowers it through the
//! checked [`curios_ersd::ErsdBuilder`] into a verified
//! [`curios_ersd::Module`], preserving the language's semantic
//! identities — distinct `Bool`/`Byte` shapes, first-class switches and folds,
//! schema-carrying products and variants. Every encoding decision (carriers,
//! tag layouts, dispatch, loop synthesis) belongs to the later lowering out
//! of the representation, not to erasure.
//!
//! Erasure is a transcription under the **operand law**: every source
//! subexpression erases to exactly one operand ([`curios_ersd::Atom`]) —
//! an atomic value directly, a compound one bound by a statement in the
//! builder's innermost open block, in evaluation order — and every reuse
//! references the bound atom, never a re-erased copy. Divergence is explicit:
//! an expression that provably never yields a value (a process exit, a vacuous
//! elimination) reports the terminator that seals its block instead of an
//! atom, and dead code after it is never erased.
//!
//! Core classifies and traverses; the builder owns construction. Production
//! compilation erases the fixed prelude once at compiler build time
//! ([`erase_prelude_to_ir_prefix`], archived by `curios-prelude`) and replays
//! it under each program's user suffix ([`erase_module_with_prelude_to_ir`]).
//! Each entrypoint first validates the universe-closed Core module and projects
//! it through the private `UniverseErased<Module>` boundary. That projection
//! removes universe instances, declaration contexts, and nominal vectors once;
//! no universe data reaches Ersd and reduction never specializes runtime code
//! by universe instance.

use {
    super::{
        Apply, Atom, Bound, Carrier, Cases, Context, Error, Field, Func, FuncType, InductArm,
        InductDecl, InductType, Item, Let, Many, Match, Module, Nat, Prim, PrimHead, Proj, Rec,
        RecItem, RecMember, Scope, Struct, StructType, Subterm, Telescope, Term, Three, Tuple,
        TupleType, Two, Var, Variant, expect_prim_head, infer, reduce_with, refine_head, wire_term,
    },
    num_bigint::BigUint,
    num_traits::ToPrimitive,
    std::collections::{BTreeMap, BTreeSet},
};

mod classify;
use classify::*;

mod environment;
use environment::*;

mod lower;
pub use lower::{
    ErasedPrelude, erase_module_to_ir, erase_module_with_prelude_to_ir, erase_prelude_to_ir_prefix,
};
use lower::{Lowering, Outcome};

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod recursion;

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
