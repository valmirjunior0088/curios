//! curios-compiler library: compile, precompile, and run-from-source helpers,
//! reused by the `curios-compiler` binary and the integration suite. This is the
//! only crate that links Cranelift (via wasmtime) and Binaryen (via
//! `curios-binaryen`); `curios-runtime` stays slim.

use {
    curios::{compile_entrypoint, text, wasm},
    curios_runtime::{Host, run_bytes, shared_engine},
    std::{path::Path, time::Duration},
};

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm`
/// payload the runtime deserializes — the same payload a bundled executable
/// carries. Uses `curios-runtime`'s shared engine so the precompiled artifact
/// matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &wasm::Module) -> Result<Vec<u8>, String> {
    let bytes = curios_binaryen::optimize(wasm::to_bytes(module));

    shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and
/// run it on the shared runtime engine — the identical path a bundled executable
/// takes. Returns the process exit code.
pub fn run_wasm<H: Host + Send + Sync + 'static>(
    module: &wasm::Module,
    host: H,
) -> Result<i32, String> {
    run_bytes(&to_cwasm(module)?, host)
}

/// Compile an already-parsed entrypoint under `loader` and run it.
pub fn run_entrypoint<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    host: H,
) -> Result<(), String> {
    run_wasm(
        &compile_entrypoint(timeout, entrypoint, loader, |_| {})?,
        host,
    )
    .map(|_| ())
}

/// Parse `source` (no external modules) and run it.
pub fn run<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = source
        .parse::<text::Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(timeout, &entrypoint, &text::NullLoader, host)
}

/// Alias of [`run`] used by the integration suite.
pub fn run_text<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    run(timeout, source, host)
}

/// Load `path`'s entrypoint (file loader rooted at its parent) and run it.
pub fn run_file<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    path: &Path,
    host: H,
) -> Result<(), String> {
    let entrypoint = text::Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader = text::FileLoader::new(path.parent().unwrap_or(Path::new(".")));

    run_entrypoint(timeout, &entrypoint, &loader, host)
}

#[cfg(test)]
mod tests;
