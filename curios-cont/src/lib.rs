//! The Curios continuation IR: the arena-backed, pre-closure CPS graph the erased stage lowers into, and the optimizer that rewrites it.
//!
//! `curios_ersd::lower_to_cont` constructs the public [`Module`], and [`optimize`](optimize()) rewrites that high-CPS graph. Lowering it to WebAssembly is `curios-emit`'s, which depends on this crate rather than the reverse, so the erased stage that lowers into this IR never builds the emitter or `curios-wasm`. [`storage`] is the one decision this crate makes on the emitter's behalf: which values a machine register can hold, read off the same dataflow solver the passes share.
//!
//! Every CPS function owns a globally unique bodyless return continuation. Ordinary return is `ApplyCont(function.return_cont, [value])`; `Halt` is reserved for a host call that terminates the process.

mod analysis;
pub(crate) use analysis::*;

mod dataflow;
pub(crate) use dataflow::*;

mod demand;
pub(crate) use demand::*;

mod intrinsic;
pub use intrinsic::*;

mod module;
pub use module::*;

mod node;
pub use node::*;

mod optimize;
pub use optimize::*;

mod origin;
pub(crate) use origin::*;

mod print;

mod represent;
pub use represent::*;

mod survey;
pub use survey::*;

mod verify;
pub use verify::*;

#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;
