//! Core erasure into the erased representation ([`curios_ersd::Module`]).
//!
//! It consumes the meta-free Core [`Module`](curios_core::Module) and lowers it through the checked [`curios_ersd::ErsdBuilder`] into a verified [`curios_ersd::Module`], preserving the language's semantic identities — distinct `Bool`/`Byte` shapes, first-class switches and folds, schema-carrying products and variants. Every encoding decision (carriers, tag layouts, dispatch, loop synthesis) belongs to the later lowering out of the representation, not to erasure.
//!
//! Erasure is a transcription under the **operand law**: every source subexpression erases to exactly one operand ([`curios_ersd::Atom`]) — an atomic value directly, a compound one bound by a statement in the builder's innermost open block, in evaluation order — and every reuse references the bound atom, never a re-erased copy. Divergence is explicit: an expression that provably never yields a value (a process exit, a vacuous elimination) reports the terminator that seals its block instead of an atom, and dead code after it is never erased.
//!
//! Core classifies and traverses; the builder owns construction. Production compilation erases the fixed prelude once at compiler build time, archived by `curios-prelude-archive`, and resumes over its arena for every later unit ([`Resumed`]). [`erase_unit`] erases a unit, the prelude's included, and [`erase_program`] is the same walk with the entry sealed. Every entrypoint projects its Core module through the private `UniverseErased<Module>` boundary, which removes universe instances, declaration contexts, and nominal vectors once; no universe data reaches Ersd and reduction never specializes runtime code by universe instance.
//!
//! The boundary validates what it has not already seen validated, and projects what is not already projected — which for a unit erased over a scope is the unit's own items and the registry entries it adds. The prelude arrives immutable and checked from `curios-prelude-archive`'s restore, so validating and projecting it again would be a walk of the whole standard library, inside the erasure context's step budget, for an answer already in hand.

use {
    super::{Context, Error, expect_intrinsic_head, infer, reduce_with, refine_head},
    curios_core::{
        Apply, Atom, Bound, Carrier, Cases, Func, FuncType, InductArm, InductDecl, InductType,
        Intrinsic, IntrinsicHead, Let, Many, Match, Nat, Proj, Rec, RecItem, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two, Variant,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

mod classify;
use classify::*;

mod environment;
use environment::*;

mod lower;
pub use lower::*;

mod resumed;
pub use resumed::*;

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod recursion;

mod intrinsic;
use intrinsic::*;

#[cfg(test)]
mod tests;

/// Unwrap an [`Outcome`] to its emitted atom, propagating divergence to the caller (the rest of the enclosing block is dead and is never erased).
macro_rules! emitted {
    ($outcome:expr) => {
        match $outcome {
            $crate::into_ersd::Outcome::Emitted(atom) => atom,
            diverged @ $crate::into_ersd::Outcome::Diverged(_) => return Ok(diverged),
        }
    };
}
use emitted;
