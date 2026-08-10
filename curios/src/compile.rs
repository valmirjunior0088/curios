//! Compile, precompile, and run-from-source helpers, reused by the `curios` binary and the integration suite. `curios` is the only crate that links Cranelift (via wasmtime) and Binaryen (via `curios-binaryen`); `curios-runtime` stays slim.

use {
    curios_binaryen::optimize,
    curios_pipeline::{CompileError, Stage, compile_entrypoint},
    curios_prelude::{SYNTAX, with_prelude},
    curios_runtime::{ForeignBindings, HostOps, run_bytes, shared_engine},
    curios_text::{Entrypoint, RootSource},
    curios_unit::Scope,
    curios_wasm::{Module, to_bytes},
    std::{path::Path, slice::from_ref},
};

/// Compile `entrypoint` against the fixed prelude — the one unit every product path puts in scope.
///
/// **This is where the standard library is named, and the driver is where it no longer is.** `curios-pipeline` folds its stages over whatever scope it is handed and cannot tell which unit is `/std`; deciding that is a product's job, exactly as supplying the `/syn` registry already was. Everything the CLI, the embedder helpers and the integration suite compile comes through here, so there is one place that answers "what does a Curios program get for free".
pub fn compile_with_prelude<O>(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
    observe: O,
) -> Result<(Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    compile_with_units(budget, &[], entrypoint, loader, observe)
}

/// Compile `units` in the order given, then `entrypoint` against all of them and the prelude.
///
/// The order *is* the dependency order — there is no manifest yet to derive one from, and none is invented here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what Phase C's declared dependencies replace.
pub fn compile_with_units<O>(
    budget: u64,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: RootSource,
    observe: O,
) -> Result<(Module, curios_abi::ForeignStore), CompileError>
where
    O: FnMut(Stage<'_>),
{
    with_prelude(|prelude| {
        let sources = units
            .iter()
            .map(curios_text::UnitSource::mounted)
            .collect::<Vec<_>>();
        let produced = curios_pipeline::compile_units(
            budget,
            Scope::over(from_ref(&prelude)),
            &SYNTAX,
            &sources,
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        compile_entrypoint(
            budget,
            Scope::over(&scope),
            &SYNTAX,
            entrypoint,
            loader,
            observe,
        )
    })
}

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm` payload the runtime deserializes — the same payload a bundled executable carries. Uses `curios-runtime`'s shared engine so the precompiled artifact matches the configuration `run_bytes` deserializes against.
pub fn to_cwasm(module: &Module) -> Result<Vec<u8>, String> {
    let bytes = optimize(to_bytes(module));

    shared_engine()
        .precompile_module(&bytes)
        .map_err(|error| format!("failed to precompile module: {error}"))
}

/// Run a compiled module in-process: precompile to `.cwasm`, then deserialize and run it on the shared runtime engine — the identical path a bundled executable takes. `bindings` supplies the `ffi`-tier implementations for the module's own `foreign` declarations (pass [`ForeignBindings::empty`] for a program that declares none). Returns the process exit code.
pub fn run_wasm<H: HostOps + Send + Sync + 'static>(
    module: &Module,
    host: H,
    bindings: ForeignBindings,
) -> Result<i32, String> {
    run_bytes(&to_cwasm(module)?, host, bindings)
}

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' `ForeignStore` — this is the fused compile-and-run convenience path with no point to hand it back to the caller; an embedder with `foreign` declarations to satisfy calls [`compile_entrypoint`] directly instead, building [`ForeignBindings`] from the returned store and calling [`run_wasm`] itself.
#[cfg(test)]
pub(crate) fn run_entrypoint<H: HostOps + Send + Sync + 'static>(
    entrypoint: &Entrypoint,
    loader: RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) =
        compile_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, ForeignBindings::empty()).map(|_| ())
}

/// A unit mounted at `prefix`, declared in `path` and holding its modules in `path`'s stem directory.
///
/// Nothing is read here. A mounted unit used to arrive as an eagerly materialized map, because discovery of a unit already in scope was assumed to have no filesystem to reach — but a resolver is what it always needed, and `curios-web` keeps compiling because *supplying* the bodies is one base among several rather than the only shape a mounted unit has. So this hands back the two paths the layout rule relates, and `curios-text` reads each header exactly when discovery asks for it, with the same diagnostics the entry program's own modules get.
///
/// [`RootKind::Ordinary`](curios_base::RootKind::Ordinary) is passed here and nowhere else, which is what makes "no package can exempt itself from the orphan rule" structural: a manifest has no path to this argument.
pub fn load_unit(prefix: &str, path: &Path) -> curios_text::RootSource {
    let directory = path
        .parent()
        .unwrap_or(Path::new("."))
        .join(path.file_stem().unwrap_or_default());

    curios_text::RootSource::mounted(
        curios_base::Qualifier::from([prefix]),
        curios_base::RootKind::Ordinary,
        path,
        directory,
    )
}

/// Lower and type-check `entrypoint` against the fixed prelude, reporting the erasure obligations rather than raising them. See [`curios_pipeline::typecheck_reporting`].
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &Entrypoint,
    loader: RootSource,
) -> Result<(curios_core::Module, Vec<String>), CompileError> {
    with_prelude(|prelude| {
        curios_pipeline::typecheck_reporting(
            budget,
            Scope::over(from_ref(&prelude)),
            &SYNTAX,
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
    with_prelude(|prelude| {
        curios_pipeline::recheck(module, budget, Scope::over(from_ref(&prelude)))
    })
}

/// The default reduction budget, re-exported beside the compile helpers that take it.
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

/// Open a `.crs` entrypoint at `path`, paired with a [`curios_text::RootSource::file_system`] rooted at its parent directory — the standard way to resolve a program's imports relative to the file it lives in.
pub fn load(path: &Path) -> Result<(Entrypoint, RootSource), String> {
    let entrypoint = Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader = RootSource::file_system(path.parent().unwrap_or(Path::new(".")).to_path_buf());

    Ok((entrypoint, loader))
}
