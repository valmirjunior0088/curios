//! Compile, precompile, and run-from-source helpers, reused by the `curios`
//! binary and the integration suite. `curios` is the only crate that links
//! Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-runtime`
//! stays slim.

use {
    curios_binaryen::optimize,
    curios_runtime::{ForeignBindings, HostOps, run_bytes, shared_engine},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::{Module, to_bytes},
    std::path::Path,
};

#[cfg(test)]
use curios_pipeline::compile_entrypoint;

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm`
/// payload the runtime deserializes — the same payload a bundled executable
/// carries. Uses `curios-runtime`'s shared engine so the precompiled artifact
/// matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &Module) -> Result<Vec<u8>, String> {
    let bytes = optimize(to_bytes(module));

    shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and
/// run it on the shared runtime engine — the identical path a bundled executable
/// takes. `bindings` supplies the `ffi`-tier implementations for the module's own
/// `foreign` declarations (pass [`ForeignBindings::empty`] for a program that
/// declares none). Returns the process exit code.
pub fn run_wasm<H: HostOps + Send + Sync + 'static>(
    module: &Module,
    host: H,
    bindings: ForeignBindings,
) -> Result<i32, String> {
    run_bytes(&to_cwasm(module)?, host, bindings)
}

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' `ForeignStore`
/// — this is the fused compile-and-run convenience path with no point to hand
/// it back to the caller; an embedder with `foreign` declarations to satisfy
/// calls [`compile_entrypoint`] directly instead, building [`ForeignBindings`]
/// from the returned store and calling [`run_wasm`] itself.
#[cfg(test)]
pub(crate) fn run_entrypoint<H: HostOps + Send + Sync + 'static>(
    entrypoint: &Entrypoint,
    loader: RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) = compile_entrypoint(DEFAULT_STEP_BUDGET, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, ForeignBindings::empty()).map(|_| ())
}

/// The default reduction budget, re-exported beside the compile helpers that
/// take it.
pub use curios_pipeline::DEFAULT_STEP_BUDGET;

/// Parse `source` (no external modules) and run it.
#[cfg(test)]
pub(crate) fn run_text<H: HostOps + Send + Sync + 'static>(
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(&entrypoint, RootSource::none(), host)
}

/// Open a `.crs` entrypoint at `path`, paired with a
/// [`curios_text::RootSource::file_system`] rooted at its parent directory —
/// the standard way to resolve a program's imports relative to the file it
/// lives in.
pub fn load(path: &Path) -> Result<(Entrypoint, RootSource), String> {
    let entrypoint = Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader = RootSource::file_system(path.parent().unwrap_or(Path::new(".")).to_path_buf());

    Ok((entrypoint, loader))
}
