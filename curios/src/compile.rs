//! Compile, precompile, and run-from-source helpers, reused by the `curios`
//! binary and the integration suite. `curios` is the only crate that links
//! Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-rt`
//! stays slim.

use {
    crate::{compile_entrypoint, text, wasm},
    curios_rt::{ForeignBindings, Host, run_bytes, shared_engine},
    std::{
        path::{Path, PathBuf},
        time::Duration,
    },
};

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm`
/// payload the runtime deserializes — the same payload a bundled executable
/// carries. Uses `curios-rt`'s shared engine so the precompiled artifact
/// matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &wasm::Module) -> Result<Vec<u8>, String> {
    let bytes = curios_binaryen::optimize(wasm::to_bytes(module));

    shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and
/// run it on the shared runtime engine — the identical path a bundled executable
/// takes. `bindings` supplies the `env`-tier implementations for the module's own
/// `foreign` declarations (pass [`ForeignBindings::empty`] for a program that
/// declares none). Returns the process exit code.
pub fn run_wasm<H: Host + Send + Sync + 'static>(
    module: &wasm::Module,
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
pub fn run_entrypoint<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: text::RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) = compile_entrypoint(timeout, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, ForeignBindings::empty()).map(|_| ())
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

    run_entrypoint(timeout, &entrypoint, text::RootSource::None, host)
}

/// Alias of [`run`] used by the integration suite.
pub fn run_text<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    run(timeout, source, host)
}

/// Open a `.crs` entrypoint at `path`, paired with a [`text::RootSource::FileSystem`]
/// rooted at its parent directory — the standard way to resolve a program's
/// imports relative to the file it lives in.
pub fn load(path: &Path) -> Result<(text::Entrypoint, text::RootSource), String> {
    let entrypoint = text::Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader =
        text::RootSource::FileSystem(path.parent().unwrap_or(Path::new(".")).to_path_buf());

    Ok((entrypoint, loader))
}

/// Load `path`'s entrypoint (file loader rooted at its parent) and run it.
pub fn run_file<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    path: &Path,
    host: H,
) -> Result<(), String> {
    let (entrypoint, loader) = load(path)?;

    run_entrypoint(timeout, &entrypoint, loader, host)
}

/// Like [`load`], but also resolves `dependencies` — each a name paired with
/// the path to its own root file (no fixed root-file convention; the caller
/// points at whatever file they want, exactly like the entrypoint itself) —
/// into named path compilation roots alongside the entrypoint's own loader.
pub fn load_with_dependencies(
    path: &Path,
    dependencies: Vec<(String, PathBuf)>,
) -> Result<(text::Entrypoint, text::RootSource), String> {
    let (entrypoint, base) = load(path)?;

    let deps = dependencies
        .into_iter()
        .map(|(name, dep_path)| {
            let (module, source) = text::RootSource::dependency_from_path(&dep_path)
                .map_err(|error| error.format())?;
            Ok((name, module, source))
        })
        .collect::<Result<Vec<_>, String>>()?;

    let loader = text::RootSource::dependencies(deps, base).map_err(|error| error.format())?;

    Ok((entrypoint, loader))
}
