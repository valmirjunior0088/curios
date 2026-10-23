//! Everything that needs a compiler behind the engine.
//!
//! Every operation here runs on [`shared_engine`], the one configuration a module is later instantiated under, and hands back either bytes or a [`MachineCode`]. Why this is a crate beside `curios-native` rather than a feature of it, and what crosses between the two, are `README.md`'s decisions; why `validate` stays with the engine is `curios-native`'s.

use {
    curios_native::{MachineCode, shared_engine},
    std::hash::{Hash, Hasher},
    wasmtime::Module,
};

/// AOT-compile `bytes` to the `.cwasm` payload [`MachineCode::deserialize`] reads back: what a bundle carries and a store files.
pub fn precompile(bytes: &[u8]) -> Result<Vec<u8>, String> {
    shared_engine()
        .precompile_module(bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Compile `bytes`, a WebAssembly module, to machine code this process holds. Nothing is read back, so nothing is vouched for: the machine code is what the engine made of `bytes` just now. A refusal is the engine's own sentence, for the caller to say whose module it was.
pub fn compile(bytes: &[u8]) -> Result<MachineCode, String> {
    Module::new(shared_engine(), bytes)
        .map(MachineCode::of)
        .map_err(|error| error.to_string())
}

/// Write `code` out as the `.cwasm` payload [`MachineCode::deserialize`] reads back: [`precompile`]'s bytes, for a module that is already compiled.
pub fn serialize(code: &MachineCode) -> Result<Vec<u8>, String> {
    code.module().serialize().map_err(|error| error.to_string())
}

/// The names `bytes` exports, for a caller describing a module it is about to pin.
///
/// **Compiling is what reads them, and compiling is the stronger statement.** A module's leading magic says only what it claims to be; that this engine accepted and compiled it says the bytes are a module this toolchain can actually link. So the failure is as much of the answer as the list: a fetch that saved an error page reports what is wrong with it rather than an empty export list.
///
/// Here rather than in `curios-package`, which owns the row being written: reading a module needs a WebAssembly implementation, and the engine's two crates are the only ones in the workspace that have one.
pub fn module_exports(bytes: &[u8]) -> Result<Vec<String>, String> {
    let module = Module::new(shared_engine(), bytes).map_err(|error| format!("{error:#}"))?;

    Ok(module
        .exports()
        .map(|export| export.name().to_string())
        .collect())
}

/// Fold what decides whether a `.cwasm` produced here deserializes elsewhere into `hasher`: the target triple, the ISA flags Cranelift compiled for, this engine's tunables and wasm feature set, and wasmtime's own version.
///
/// **A fold rather than a digest, because the digest is the caller's vocabulary and wasmtime is this crate's.** A payload's store address has to carry this — it is the one input neither the compiler binary's digest nor any recorded source file covers, since Cranelift compiles for the host's ISA — and both halves of that sentence name a crate the other must not depend on. Handing over a [`Hasher`] to write into keeps `sha2` out of the engine's crates and `wasmtime` out of `curios`, which is where the address is assembled.
///
/// Wasmtime's own stamp inside the artifact stays the backstop. This decides where a payload is looked for; `Module::deserialize` still decides whether the bytes found there may be run, and refuses whatever this failed to separate.
pub fn engine_compatibility<H: Hasher>(hasher: &mut H) {
    shared_engine().precompile_compatibility_hash().hash(hasher);
}
