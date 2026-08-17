//! Everything that needs a compiler behind it.
//!
//! **The gate is stated once, on the module, rather than on each item.** Both operations here exist only when wasmtime has a compiler in it — `Engine::precompile_module` and `Engine::precompile_compatibility_hash` are both declared behind `any(feature = "cranelift", feature = "winch")` — so a build without one has no functions to call, and the mistake is a named missing function at compile time instead of a launcher that quietly grew a backend. Inside, nothing repeats the condition.
//!
//! `validate` deliberately stays in `engine`: it is a wasmparser pass over bytes rather than a compilation, so a runtime-only build can decide it. The split here follows wasmtime's own.

use {
    super::shared_engine,
    std::hash::{Hash, Hasher},
};

/// AOT-compile `bytes` to the `.cwasm` payload [`run_bytes`](super::run_bytes) deserializes.
pub fn precompile(bytes: &[u8]) -> Result<Vec<u8>, String> {
    shared_engine()
        .precompile_module(bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Fold what decides whether a `.cwasm` produced here deserializes elsewhere into `hasher`: the target triple, the ISA flags Cranelift compiled for, this engine's tunables and wasm feature set, and wasmtime's own version.
///
/// **A fold rather than a digest, because the digest is the caller's vocabulary and wasmtime is this crate's.** A payload's store address has to carry this — it is the one input neither the compiler binary's digest nor any recorded source file covers, since Cranelift compiles for the host's ISA — and both halves of that sentence name a crate the other must not depend on. Handing over a [`Hasher`] to write into keeps `sha2` out of the runtime and `wasmtime` out of `curios`, which is where the address is assembled.
///
/// Wasmtime's own stamp inside the artifact stays the backstop. This decides where a payload is looked for; `Module::deserialize` still decides whether the bytes found there may be run, and refuses whatever this failed to separate.
pub fn engine_compatibility<H: Hasher>(hasher: &mut H) {
    shared_engine().precompile_compatibility_hash().hash(hasher);
}
