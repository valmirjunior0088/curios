//! The Curios WebAssembly emission: [`into_wasm`](into_wasm()) lowers an optimized `curios_cont::Module` to a WebAssembly-GC module, performing delayed closure conversion, verifying a private closed machine CFG, structurizing reducible control into Wasm blocks and loops, and localizing dispatcher fallback to irreducible scopes.
//!
//! Machine lowering recognizes a function's bodyless return continuation in the current-function context, so an ordinary return, `ApplyCont(function.return_cont, [value])`, is emitted as `Return` without allocating a block.
//!
//! Every program value this crate emits lives in a GC reference — a struct, an array, or an `i31` — and never in linear memory. That is [WebAssembly-GC is the only target](../../documentation/design/compilation/webassembly-gc-is-the-only-target.md), and this crate is where it is decided: `curios-wasm` models the whole envelope's linear-memory surface and refuses nothing, so a module emitted here declares no memory at all rather than being kept out of one.
//!
//! The crate owns neither representation it works between, so every name from `curios_cont` and `curios_wasm` is written with its crate.

mod machine;
use machine::*;

mod emission;
pub(crate) use emission::*;

mod into_wasm;
pub use into_wasm::*;

mod symbols;
pub(crate) use symbols::*;

mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod code_emitter;
use code_emitter::*;

mod structure;
use structure::*;

mod expr_emitter;
use expr_emitter::*;

mod hoist;
use hoist::*;

mod immediate;
use immediate::*;

mod refusal;
use refusal::*;

mod module_emitter;
use module_emitter::*;

mod rope_emitter;
use rope_emitter::*;

mod big_emitter;
use big_emitter::*;

mod flt_emitter;
use flt_emitter::*;

mod shorthand;
use shorthand::*;

mod types;
pub use types::*;

#[cfg(test)]
mod aggregate_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod hoist_tests;
#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod module_tests;
#[cfg(test)]
mod rope_tests;
#[cfg(test)]
mod test_support;
