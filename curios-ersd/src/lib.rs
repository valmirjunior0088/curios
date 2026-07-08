//! The erased, first-order IR — the pipeline stage between `curios-core`'s type-directed erasure and the continuation IR of `curios-cont`. Types, proofs, and erasable binders are gone by construction: a program is a [`Module`] of flat top-level [`Item`]s whose bodies are [`Term`]s — closures with explicit precomputed captures ([`Func`]), tuples tagged by constructor [`Atom`]s, positional [`Match`] dispatch, the [`NatMatch`] induction/switch forms, and the [`Prim`] alphabet partitioned by purity into pure, host, and cell operations.
//!
//! `curios_core::erase_module` is the sole producer. [`optimize()`](optimize) is the Ersd-level optimizer (reachability prune, closed-term evaluation, spine specialization, worker/wrapper), and [`to_cont`] lowers the optimized module to a `curios_cont::Module`.

mod prim;
pub use prim::*;

mod names;
pub use names::*;

mod term;
pub use term::*;

mod module;
pub use module::*;

mod optm;
pub use optm::*;

mod print;
use print::*;

mod to_cont;
pub use to_cont::*;
