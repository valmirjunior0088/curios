//! Core erasure into the erased representation ([`curios_anf::Module`]).
//!
//! It consumes the meta-free Core [`Module`](curios_core::Module) and lowers it through the checked [`curios_anf::Builder`] into a verified [`curios_anf::Module`], preserving the language's semantic identities — distinct `Bool`/`Byte` shapes, first-class switches and folds, schema-carrying products and variants. Every encoding decision (carriers, tag layouts, dispatch, loop synthesis) belongs to the later lowering out of the representation, not to erasure.
//!
//! Erasure is a transcription under the **operand law**: every source subexpression erases to exactly one operand ([`curios_anf::Atom`]) — an atomic value directly, a compound one bound by a statement in the builder's innermost open block, in evaluation order — and every reuse references the bound atom, never a re-erased copy. Divergence is explicit: an expression that provably never yields a value (a process exit, a vacuous elimination) reports the terminator that seals its block instead of an atom, and dead code after it is never erased.
//!
//! Core classifies and traverses; the builder owns construction. An item erases into a fragment of its own, which names what another item binds as an import ([`ErasedItem`]), and a unit holds its items' fragments ([`ErasedUnit`]). Production compilation erases the fixed prelude once at compiler build time, imaged by `curios-prelude`, and every later unit reads how its names are bound ([`Erasures`]). [`erase_unit`] erases a unit, the prelude's included, and [`erase_program`] is the same walk with the entry erased and what it reaches linked into one module.
//!
//! **Erasure reads the module the kernel certified, through the kernel that certified it** ([`curios_cert::Reader`]): every type the walk is directed by is the kernel's reading of a term it accepted, at the sorts it judged. An item that leaves sorts to its caller erases at each assignment of them ([`ErasedForms`], [`Sorts`]), and a level carries nothing at run time, so no universe reaches ANF.

mod error;
pub use error::{ErasedName, Error};

mod sorts;
pub use sorts::{AtSorts, Sorts};

mod read;
use read::*;

mod classify;
use classify::*;

mod environment;
use environment::*;

mod lower;
use lower::*;
pub use lower::{ErasedProgram, erase_program, erase_unit, erase_unit_over};

mod erased;
use erased::*;
pub use erased::{ErasedForms, ErasedItem, ErasedUnit, Erasures};

mod asked;
use asked::*;
pub use asked::{
    Asked, AskedEntry, AskedItems, AskedRecord, AskedTable, HeldAsked, Made, NamedPart, Part, Read,
};

mod over;
pub use over::ErasedBaseline;
use over::*;

mod erased_candidate;
use erased_candidate::*;

mod link;
use link::*;

mod evaluated;
use evaluated::*;

#[cfg(test)]
mod cost_tests;
#[cfg(test)]
mod data_tests;
#[cfg(test)]
mod evaluation_tests;
#[cfg(test)]
mod stand_in_tests;
#[cfg(test)]
mod term_tests;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_erase::test_support::declared_families` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

/// Unwrap an [`Outcome`] to its emitted atom, propagating divergence to the caller (the rest of the enclosing block is dead and is never erased).
macro_rules! emitted {
    ($outcome:expr) => {
        match $outcome {
            $crate::Outcome::Emitted(atom) => atom,
            diverged @ $crate::Outcome::Diverged(_) => return Ok(diverged),
        }
    };
}
use emitted;
