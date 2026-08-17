//! Core erasure into the erased representation ([`curios_ersd::Module`]).
//!
//! It consumes the meta-free Core [`Module`] and lowers it through the checked [`curios_ersd::ErsdBuilder`] into a verified [`curios_ersd::Module`], preserving the language's semantic identities — distinct `Bool`/`Byte` shapes, first-class switches and folds, schema-carrying products and variants. Every encoding decision (carriers, tag layouts, dispatch, loop synthesis) belongs to the later lowering out of the representation, not to erasure.
//!
//! Erasure is a transcription under the **operand law**: every source subexpression erases to exactly one operand ([`curios_ersd::Atom`]) — an atomic value directly, a compound one bound by a statement in the builder's innermost open block, in evaluation order — and every reuse references the bound atom, never a re-erased copy. Divergence is explicit: an expression that provably never yields a value (a process exit, a vacuous elimination) reports the terminator that seals its block instead of an atom, and dead code after it is never erased.
//!
//! Core classifies and traverses; the builder owns construction. Production compilation erases the fixed prelude once at compiler build time, archived by `curios-prelude-archive`, and replays it under each program's user suffix. [`erase_unit`] is both: the two used to be separate functions, and what distinguished them was only whether the unit had an entrypoint to seal. Every entrypoint projects its Core module through the private `UniverseErased<Module>` boundary, which removes universe instances, declaration contexts, and nominal vectors once; no universe data reaches Ersd and reduction never specializes runtime code by universe instance.
//!
//! The boundary validates what it has not already seen validated, and projects what is not already projected — which for the replay entrypoint is the entry's own items and the registry entries it adds. The prelude arrives immutable and checked from `curios-prelude-archive`'s restore, so validating and projecting it again is a walk of the whole standard library for an answer already in hand. Doing both was measured at ~320 ms of a ~1000 ms release compilation of a one-line program, and it fell inside the erasure context's step budget, which a debug build then exceeded on `programs/hello_world.crs`.

use {
    super::{Context, Error, expect_intrinsic_head, infer, reduce_with, refine_head},
    curios_core::{
        Apply, Atom, Bound, Carrier, Cases, Field, Func, FuncType, InductArm, InductDecl,
        InductType, Intrinsic, IntrinsicHead, Let, Many, Match, Nat, Proj, Rec, RecItem, Scope,
        Struct, StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two, Variant,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

mod classify;
use classify::*;

mod environment;
use environment::*;

mod lower;
pub use lower::{ErasedArena, erase_module, erase_unit};

mod resumed;
use lower::{Lowering, Outcome};
pub use resumed::*;

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod recursion;

mod intrinsic;

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
