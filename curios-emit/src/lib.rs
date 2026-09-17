//! The Curios WebAssembly emission: [`into_wasm`](into_wasm()) lowers an optimized `curios_cont::Module` to a WebAssembly-GC module, performing delayed closure conversion, verifying a private closed machine CFG, structurizing reducible control into Wasm blocks and loops, and localizing dispatcher fallback to irreducible scopes.
//!
//! Machine lowering recognizes a function's bodyless return continuation in the current-function context, so an ordinary return, `ApplyCont(function.return_cont, [value])`, is emitted as `Return` without allocating a block.
//!
//! Every program value this crate emits lives in a GC reference — a struct, an array, or an `i31` — and never in linear memory. That is [WebAssembly-GC is the only target](../../documentation/design/toolchain/webassembly-gc-is-the-only-target.md), and this crate is where it is decided: `curios-wasm` models the whole envelope's linear-memory surface and refuses nothing, so a module emitted here declares no memory at all rather than being kept out of one.
//!
//! The crate owns neither representation it works between, so every name from `curios_cont` and `curios_wasm` is written with its crate.

mod machine;

mod into_wasm;
pub use into_wasm::*;
