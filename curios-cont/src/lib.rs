//! The Curios continuation IR: the arena-backed, pre-closure CPS graph the erased stage lowers into, and the optimizer that rewrites it.
//!
//! `curios_ersd::lower_to_cont` constructs the public [`Module`], and [`optimize`] rewrites that high-CPS graph. Lowering it to WebAssembly is `curios-emit`'s, which depends on this crate rather than the reverse, so the erased stage that lowers into this IR never builds the emitter or `curios-wasm`. [`storage`] is the one decision this crate makes on the emitter's behalf: which values a machine register can hold, read off the same dataflow solver the passes share.
//!
//! Every CPS function owns a globally unique bodyless return continuation. Ordinary return is `ApplyCont(function.return_cont, [value])`; `Exit` is reserved for direct process termination.

mod cps;
pub use cps::*;

mod survey;
pub use survey::*;
