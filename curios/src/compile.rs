//! Optimize, precompile and run — the native back end, and the reason this crate exists.
//!
//! `curios` is the only crate that turns Cranelift on (through `curios-runtime`'s `cranelift` feature) and the only one that links Binaryen (via `curios-binaryen`); `curios-runtime` stays slim by default, and `curios-pipeline` stays clear of both. Everything here is downstream of a `curios_wasm::Module` and indifferent to where one came from — the compiling half is `curios_pipeline::compile_with_prelude` and its siblings, next to the fold it configures.
//!
//! No wasmtime operation below is spelled here. `curios_runtime::validate`, `curios_runtime::precompile` and `Compiled::compile` are, because they belong beside the engine whose configuration decides what a valid module is and what a `.cwasm` is compatible with; this crate names no wasmtime type at all, and its `curios-runtime` dependency row is a feature switch rather than an API dependency.

use {curios_runtime::Compiled, curios_utilities::Fingerprint, std::sync::LazyLock};

/// What decides whether a payload compiled on this machine runs on another, as a key part — computed once, since it is a property of the process, and handed to `curios-verdicts`'s payload family beside every program it files.
///
/// **The payload is the store's first machine-dependent artifact.** A unit is a judgment and travels wherever the compiler binary does; a payload is machine code Cranelift emitted for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. `curios-runtime` describes its engine, `curios-utilities`'s `Fingerprint` turns the description into a digest, and `curios-verdicts` files under it; this is the one crate that names all three, so the crate that names wasmtime and the crate that names `sha2` stay apart.
pub fn engine() -> &'static str {
    static ENGINE: LazyLock<String> = LazyLock::new(|| {
        let mut fingerprint = Fingerprint::new();
        curios_runtime::engine_compatibility(&mut fingerprint);

        fingerprint.hex()
    });

    &ENGINE
}

/// Reject a malformed module before Binaryen is handed it.
///
/// Binaryen answers a module it cannot parse with a C++ `assert`, which aborts the *process*. Under `cargo test` that takes down the whole run: no failing test is named and every other test's result is lost with it. The validator is a linear pass over bytes that are about to be optimized and compiled anyway, and it names the offending function and byte offset.
///
/// A panic rather than an error: emitting a module that does not validate is the compiler's own broken contract rather than any program's fault, and no caller has a use for it. The check is wasmtime's rather than a second WasmGC validator on [`curios_wasm::Module`] — writing one would mean writing WasmGC subtyping, precisely the part a hand-rolled validator gets permissively wrong, and a validator that certifies wrongly is worse than none. The gap that leaves is `curios-js`, whose browser build links neither Binaryen nor wasmtime (its dependency on the latter is a dev-dependency, for the bridge tests) and so carries a malformed module all the way to `WebAssembly.compile`.
fn validate(bytes: &[u8]) {
    if let Err(error) = curios_runtime::validate(bytes) {
        panic!("`into_wasm` emitted a module that does not validate: {error}");
    }
}

/// Optimize `module` with Binaryen and return its bytes, emitting `Stage::WasmOptm` through `observe` where the optimized module exists — exactly as the driver emits every other stage at its production site. This is the one stage the pure pipeline cannot emit, emitted here because this crate is where Binaryen runs.
///
/// The stage renders only when the observer formats it, so a compilation that watches nothing pays nothing for it. `names` keeps the name section: a caller that means to read the dump asks for it, or the text reads as bare indices.
pub fn optimize<O>(module: &curios_wasm::Module, names: bool, mut observe: O) -> Vec<u8>
where
    O: FnMut(curios_pipeline::Stage<'_>),
{
    let raw = curios_wasm::to_bytes(module);
    validate(&raw);

    curios_binaryen::optimize(raw, names, |optimized| {
        observe(curios_pipeline::Stage::WasmOptm(optimized));
    })
}

/// `module` optimized with Binaryen, as the bytes Cranelift is handed.
fn optimized(module: &curios_wasm::Module) -> Vec<u8> {
    // Keep the name section only when this build is a profiling one: it is what lets a sampling profiler name emitted wasm functions, and it is dead weight in a shipped binary. Same flag as the guest-side perf map in `curios-runtime` and the compiler spans in `curios-profile`, so one feature makes a whole compile-and-run legible.
    optimize(module, cfg!(feature = "profile"), |_| {})
}

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm` payload a bundled executable carries and a store files, which leaves this process and is read back by [`Compiled::deserialize`]. Uses `curios-runtime`'s shared engine so the precompiled artifact matches the configuration it is deserialized against.
pub fn to_cwasm(module: &curios_wasm::Module) -> Result<Vec<u8>, String> {
    let bytes = optimized(module);

    curios_profile::profile!("precompile" => curios_runtime::precompile(&bytes))
}

/// Optimize (Binaryen) and compile (Cranelift) a module to the program this process runs, on `curios-runtime`'s shared engine: [`to_cwasm`]'s machine code, held rather than written out, so running it reads nothing back.
pub fn to_compiled(module: &curios_wasm::Module) -> Result<Compiled, String> {
    let bytes = optimized(module);

    curios_profile::profile!("precompile" => Compiled::compile(&bytes))
}

/// Compile a module and run it in-process on the shared runtime engine. `bindings` supplies the `ffi`-tier implementations for the module's own `foreign` declarations (pass [`curios_runtime::ForeignBindings::empty`] for a program that declares none). Returns the process exit code.
pub fn run_wasm<H: curios_runtime::HostOps + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    host: H,
    bindings: curios_runtime::ForeignBindings,
) -> Result<u8, String> {
    to_compiled(module)?.run(host, bindings)
}
