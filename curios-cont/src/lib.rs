//! Arena-backed pre-closure CPS and its Wasm backend.
//!
//! `curios_ersd::lower_to_cont` constructs the public [`CpsModule`]. [`optimize`] rewrites that high-CPS graph before [`into_wasm`] performs delayed closure conversion, verifies a private closed machine CFG, structurizes reducible control into Wasm blocks and loops, and localizes dispatcher fallback to irreducible scopes.
//!
//! Every CPS function owns a globally unique bodyless return continuation. Ordinary return is `ApplyCont(function.return_cont, [value])`; machine lowering recognizes that ID in the current-function context and emits `Return` without allocating a block. `Exit` is reserved for direct process termination.
//!
//! Every program value this crate emits lives in a GC reference — a struct, an array, or an `i31` — and never in linear memory. That is [WebAssembly-GC is the only target](../../documentation/design/toolchain/webassembly-gc-is-the-only-target.md), and this crate is where it is decided: `curios-wasm` models the whole envelope's linear-memory surface and refuses nothing, so a module emitted here declares no memory at all rather than being kept out of one.

mod cps;
pub use cps::*;

mod machine;

mod into_wasm;
pub use into_wasm::*;
