//! Compile, precompile, and run-from-source helpers, reused by the `curios` binary and the integration suite. `curios` is the only crate that links Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-runtime` stays slim.

use std::{path::Path, slice::from_ref};

/// Compile `entrypoint` against the fixed prelude — the one unit every product path puts in scope.
///
/// **This is where the standard library is named, and the driver is where it no longer is.** `curios-pipeline` folds its stages over whatever scope it is handed and cannot tell which unit is `/std`; deciding that is a product's job, exactly as supplying the `/syn` registry already was. Everything the CLI, the embedder helpers and the integration suite compile comes through here, so there is one place that answers "what does a Curios program get for free".
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), curios_pipeline::CompileError>
where
    O: FnMut(curios_pipeline::Stage<'_>),
{
    compile_with_units(budget, &[], entrypoint, loader, None, observe)
}

/// Compile `units` in the order given, then `entrypoint` against all of them and the prelude.
///
/// The order *is* the dependency order — there is no manifest yet to derive one from, and none is invented here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what Phase C's declared dependencies replace.
pub fn compile_with_units<O>(
    budget: u64,
    units: &[curios_text::RootSource],
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    cache: Option<&dyn curios_pipeline::Cache>,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), curios_pipeline::CompileError>
where
    O: FnMut(curios_pipeline::Stage<'_>),
{
    curios_prelude::with_prelude(|prelude| {
        let sources = units
            .iter()
            .map(curios_text::UnitSource::mounted)
            .collect::<Vec<_>>();
        let produced = curios_pipeline::compile_units(
            budget,
            curios_unit::Prefix::over(from_ref(&prelude)),
            &curios_prelude::SYNTAX,
            &sources,
            cache,
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        curios_pipeline::compile_entrypoint(
            budget,
            curios_unit::Prefix::over(&scope),
            &curios_prelude::SYNTAX,
            entrypoint,
            loader,
            observe,
        )
    })
}

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

/// Lower and type-check `entrypoint` against the fixed prelude, reporting the erasure obligations rather than raising them. See [`curios_pipeline::typecheck_reporting`].
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
) -> Result<(curios_core::Module, Vec<String>), curios_pipeline::CompileError> {
    curios_prelude::with_prelude(|prelude| {
        curios_pipeline::typecheck_reporting(
            budget,
            curios_unit::Prefix::over(from_ref(&prelude)),
            &curios_prelude::SYNTAX,
            entrypoint,
            loader,
        )
    })
}

/// Put `module` to the independent kernel with the fixed prelude in scope. See [`curios_pipeline::recheck`].
pub fn recheck_with_prelude(
    module: &curios_core::Module,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    curios_prelude::with_prelude(|prelude| {
        curios_pipeline::recheck(
            module,
            budget,
            curios_unit::Prefix::over(from_ref(&prelude)),
        )
    })
}

/// Open a `.crs` entrypoint at `path`, paired with the [`curios_text::RootSource::entry`] its own stem directory anchors — a bare file is a header like any other, so `mod util` in `main.crs` reads `main/util.crs`.
pub fn load(path: &Path) -> Result<(curios_text::Entrypoint, curios_text::RootSource), String> {
    let entrypoint = curios_text::Entrypoint::from_path(path).map_err(|error| error.format())?;

    Ok((entrypoint, curios_text::RootSource::entry(path)))
}
