//! The erased, first-order IR — the pipeline stage between `curios-elab`'s type-directed erasure and the continuation IR of `curios-cont`. Types, proofs, and erasable binders are gone by construction.
//!
//! The program is flat, explicit, first-order data: a [`Module`] of arena-allocated blocks of single-operation statements over atomic operands, addressed by typed `u32` identities, with ordered top-level items plus an entry block at its top level. The alphabet is erased Core's vocabulary with its semantic identities intact, as `README.md`'s "Shapes stay distinct" decides. Functions store no capture lists; free values, uses, the call graph, and recursive components are derived on demand ([`Analysis`]).
//!
//! `curios_elab`'s `into_ersd` is the sole producer, constructing through the checked [`ErsdBuilder`]. It erases each declaration into a module of its own, a *fragment*, which declares what another declaration binds as an import ([`Imports`]) and verifies with no entry ([`Module::verify_fragment`]); [`Module::link`] appends fragments into one program under fresh identities, and only a program, which has no import left, is optimized or lowered. [`optimize`](optimize()) is the Ersd-level optimizer (behavior-summary pruning, closed-term evaluation, literal-spine specialization, monoid worker/wrapper), and [`lower_to_cont`] is the one-way door where every encoding decision is made, lowering the module to a `curios_cont::Module`.

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
pub(crate) use print::*;

mod walk;
pub(crate) use walk::*;

mod analysis;
pub use analysis::*;

mod into_cont;
pub use into_cont::*;

// A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: the path says at its use site that the caller reached for scaffolding rather than product API. This crate's own tests read it too, which the feature does not reach.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

mod semantics;
pub use semantics::*;

mod summary;
pub use summary::*;

mod remap;
pub(crate) use remap::*;

mod link;
pub use link::*;

mod optimize;
pub use optimize::*;
