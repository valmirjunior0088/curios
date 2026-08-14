//! Optimize, precompile and run — the native back end, and the reason this crate exists.
//!
//! `curios` is the only crate that turns Cranelift on (through `curios-runtime`'s `cranelift` feature) and the only one that links Binaryen (via `curios-binaryen`); `curios-runtime` stays slim by default, and `curios-pipeline` stays clear of both. Everything here is downstream of a `curios_wasm::Module` and indifferent to where one came from — the compiling half, which used to sit in this file, is `curios_pipeline::compile_with_prelude` and its siblings, next to the fold it configures.
//!
//! Neither wasmtime operation below is spelled here. `curios_runtime::validate` and `curios_runtime::precompile` are, because they belong beside the engine whose configuration decides what a valid module is and what a `.cwasm` is compatible with; this crate names no wasmtime type at all, and its `curios-runtime` dependency row is a feature switch rather than an API dependency.

/// Reject a malformed module before Binaryen is handed it.
///
/// Binaryen answers a module it cannot parse with a C++ `assert`, which aborts the *process*. Under `cargo test` that takes down the whole run: no failing test is named and every other test's result is lost with it, which is how one representation bug in `into_wasm` cost a full corpus run to attribute. The validator is a linear pass over bytes that are about to be optimized and compiled anyway, and it names the offending function and byte offset.
///
/// A panic rather than an error: emitting a module that does not validate is the compiler's own broken contract rather than any program's fault, and no caller has a use for it. The check is wasmtime's rather than a second WasmGC validator on [`curios_wasm::Module`] — writing one would mean writing WasmGC subtyping, precisely the part a hand-rolled validator gets permissively wrong, and a validator that certifies wrongly is worse than none. The gap that leaves is `curios-js`, whose browser build links neither Binaryen nor wasmtime (its dependency on the latter is a dev-dependency, for the bridge tests) and so carries a malformed module all the way to `WebAssembly.compile`.
fn validate(bytes: &[u8]) {
    if let Err(error) = curios_runtime::validate(bytes) {
        panic!("`into_wasm` emitted a module that does not validate: {error}");
    }
}

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm` payload the runtime deserializes — the same payload a bundled executable carries. Uses `curios-runtime`'s shared engine so the precompiled artifact matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &curios_wasm::Module) -> Result<Vec<u8>, String> {
    let raw = curios_wasm::to_bytes(module);
    validate(&raw);
    // Keep the name section only when this build is a profiling one: it is what lets a sampling profiler name emitted wasm functions, and it is dead weight in a shipped binary. Same flag as the guest-side perf map in `curios-runtime` and the compiler spans in `curios-profile`, so one feature makes a whole compile-and-run legible.
    let bytes = curios_binaryen::optimize(raw, cfg!(feature = "profile"));

    curios_runtime::precompile(&bytes)
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and run it on the shared runtime engine — the identical path a bundled executable takes. `bindings` supplies the `ffi`-tier implementations for the module's own `foreign` declarations (pass [`curios_runtime::ForeignBindings::empty`] for a program that declares none). Returns the process exit code.
pub fn run_wasm<H: curios_runtime::HostOps + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    host: H,
    bindings: curios_runtime::ForeignBindings,
) -> Result<i32, String> {
    curios_runtime::run_bytes(&to_cwasm(module)?, host, bindings)
}
