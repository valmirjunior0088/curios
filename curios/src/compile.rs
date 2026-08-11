//! Optimize, precompile and run — the native back end, and the reason this crate exists.
//!
//! `curios` is the only crate that links Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-runtime` stays slim, and `curios-pipeline` stays clear of both. Everything here is downstream of a `curios_wasm::Module` and indifferent to where one came from — the compiling half, which used to sit in this file, is `curios_pipeline::compile_with_prelude` and its siblings, next to the fold it configures.

/// Reject a malformed module before Binaryen is handed it.
///
/// Binaryen answers a module it cannot parse with a C++ `assert`, which aborts the *process*. Under `cargo test` that takes down the whole run: no failing test is named and every other test's result is lost with it, which is how one representation bug in `into_wasm` cost a full corpus run to attribute. Wasmtime's validator is already linked here — it is the very one that Cranelift-compiles the module a few lines below — so this costs one linear pass over bytes that are about to be optimized and compiled anyway, and it names the offending function and byte offset.
///
/// A panic rather than an error: emitting a module that does not validate is the compiler's own broken contract rather than any program's fault, and no caller has a use for it. Validation lives here, in the one crate that links a validator, rather than on [`curios_wasm::Module`] — writing a second WasmGC validator would mean writing WasmGC subtyping, which is precisely the part a hand-rolled one gets permissively wrong, and a validator that certifies wrongly is worse than none. The gap that leaves is `curios-web`, which links neither Binaryen nor wasmtime and carries a malformed module all the way to `WebAssembly.compile`.
fn validate(bytes: &[u8]) {
    if let Err(error) = wasmtime::Module::validate(curios_runtime::shared_engine(), bytes) {
        panic!("`into_wasm` emitted a module that does not validate: {error}");
    }
}

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm` payload the runtime deserializes — the same payload a bundled executable carries. Uses `curios-runtime`'s shared engine so the precompiled artifact matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &curios_wasm::Module) -> Result<Vec<u8>, String> {
    let raw = curios_wasm::to_bytes(module);
    validate(&raw);
    // Keep the name section only when this build is a profiling one: it is what lets a sampling profiler name emitted wasm functions, and it is dead weight in a shipped binary. Same flag as the guest-side perf map in `curios-runtime` and the compiler spans in `curios-profile`, so one feature makes a whole compile-and-run legible.
    let bytes = curios_binaryen::optimize(raw, cfg!(feature = "profile"));

    curios_runtime::shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and run it on the shared runtime engine — the identical path a bundled executable takes. `bindings` supplies the `ffi`-tier implementations for the module's own `foreign` declarations (pass [`curios_runtime::ForeignBindings::empty`] for a program that declares none). Returns the process exit code.
pub fn run_wasm<H: curios_runtime::HostOps + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    host: H,
    bindings: curios_runtime::ForeignBindings,
) -> Result<i32, String> {
    curios_runtime::run_bytes(&to_cwasm(module)?, host, bindings)
}
