//! Compile, precompile, and run-from-source helpers, reused by the `curios`
//! binary and the integration suite. `curios` is the only crate that links
//! Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-rt`
//! stays slim.

use {
    curios_rt::{ForeignBindings, Host, run_bytes, shared_engine},
    std::path::Path,
};

#[cfg(test)]
use {curios_pipeline::compile_entrypoint, std::time::Duration};

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm`
/// payload the runtime deserializes — the same payload a bundled executable
/// carries. Uses `curios-rt`'s shared engine so the precompiled artifact
/// matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &curios_wasm::Module) -> Result<Vec<u8>, String> {
    let bytes = curios_binaryen::optimize(curios_wasm::to_bytes(module));

    shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and
/// run it on the shared runtime engine — the identical path a bundled executable
/// takes. `bindings` supplies the `ffi`-tier implementations for the module's own
/// `foreign` declarations (pass [`ForeignBindings::empty`] for a program that
/// declares none). Returns the process exit code.
pub fn run_wasm<H: Host + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    host: H,
    bindings: ForeignBindings,
) -> Result<i32, String> {
    run_bytes(&to_cwasm(module)?, host, bindings)
}

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' [`ForeignStore`](curios_abi::ForeignStore)
/// — this is the fused compile-and-run convenience path with no point to hand
/// it back to the caller; an embedder with `foreign` declarations to satisfy
/// calls [`compile_entrypoint`] directly instead, building [`ForeignBindings`]
/// from the returned store and calling [`run_wasm`] itself.
#[cfg(test)]
pub(crate) fn run_entrypoint<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) = compile_entrypoint(timeout, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, ForeignBindings::empty()).map(|_| ())
}

/// Parse `source` (no external modules) and run it.
#[cfg(test)]
pub(crate) fn run_text<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(timeout, &entrypoint, curios_text::RootSource::none(), host)
}

/// Open a `.crs` entrypoint at `path`, paired with a
/// [`curios_text::RootSource::file_system`] rooted at its parent directory —
/// the standard way to resolve a program's imports relative to the file it
/// lives in.
pub fn load(path: &Path) -> Result<(curios_text::Entrypoint, curios_text::RootSource), String> {
    let entrypoint = curios_text::Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader =
        curios_text::RootSource::file_system(path.parent().unwrap_or(Path::new(".")).to_path_buf());

    Ok((entrypoint, loader))
}
