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
    compile_with_units(budget, &[], entrypoint, loader, observe)
}

/// Compile `units` in the order given, then `entrypoint` against all of them and the prelude.
///
/// The order *is* the dependency order — there is no manifest yet to derive one from, and none is invented here. A unit naming a prefix mounted after it fails as an unbound name, which is what a positional order costs and what Phase C's declared dependencies replace.
pub fn compile_with_units<O>(
    budget: u64,
    units: &[curios_text::PreludeModules],
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    observe: O,
) -> Result<(curios_wasm::Module, curios_abi::ForeignStore), curios_pipeline::CompileError>
where
    O: FnMut(curios_pipeline::Stage<'_>),
{
    curios_prelude::with_prelude(|prelude| {
        let sources = units
            .iter()
            .map(curios_text::UnitSource::Mounted)
            .collect::<Vec<_>>();
        let produced = curios_pipeline::compile_units(
            budget,
            curios_unit::Scope::over(from_ref(&prelude)),
            &curios_prelude::SYNTAX,
            &sources,
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        curios_pipeline::compile_entrypoint(
            budget,
            curios_unit::Scope::over(&scope),
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
    let bytes = curios_binaryen::optimize(raw);

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

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' `ForeignStore` — this is the fused compile-and-run convenience path with no point to hand it back to the caller; an embedder with `foreign` declarations to satisfy calls [`curios_pipeline::compile_entrypoint`] directly instead, building [`curios_runtime::ForeignBindings`] from the returned store and calling [`run_wasm`] itself.
#[cfg(test)]
pub(crate) fn run_entrypoint<H: curios_runtime::HostOps + Send + Sync + 'static>(
    entrypoint: &curios_text::Entrypoint,
    loader: curios_text::RootSource,
    host: H,
) -> Result<(), String> {
    let (module, _foreigns) =
        compile_with_prelude(DEFAULT_STEP_BUDGET, entrypoint, loader, |_| {})?;

    run_wasm(&module, host, curios_runtime::ForeignBindings::empty()).map(|_| ())
}

/// Load a unit mounted at `prefix` from `path`, materializing its whole module tree.
///
/// A mounted unit's modules arrive as a map rather than through a loader, because discovery of a unit already in scope has no file system to reach — `curios-web` supplies every body inline and compiles with none at all. So the tree is walked eagerly here, at the one boundary that does have a file system: `mod foo;` inside `lib.crs` reads `lib/foo.crs`, exactly as the entry program's own file-backed modules resolve.
pub fn load_unit(prefix: &str, path: &Path) -> Result<curios_text::PreludeModules, String> {
    fn materialize(
        modules: &mut curios_text::PreludeModules,
        module: &curios_text::Module,
        at: &curios_base::Qualifier,
        base: &Path,
    ) -> Result<(), String> {
        for item in &module.items {
            let curios_text::TopItem::Mod(declaration) = item else {
                continue;
            };
            let here = at.with(&declaration.label);

            let child = match &declaration.module {
                // Written inline: discovery reads it out of the parent, so nothing is loaded.
                Some(_) => continue,
                None => {
                    let file = base.join(format!("{}.crs", declaration.label));
                    curios_text::Module::from_path(&file)
                        .map_err(|error| format!("{}: {error:?}", file.display()))?
                }
            };

            materialize(modules, &child, &here, &base.join(&declaration.label))?;
            modules.insert_module(here, child);
        }

        Ok(())
    }

    let root = curios_text::Module::from_path(path)
        .map_err(|error| format!("{}: {error:?}", path.display()))?;
    let directory = path
        .parent()
        .unwrap_or(Path::new("."))
        .join(path.file_stem().unwrap_or_default());

    let mut modules = curios_text::PreludeModules::new();
    materialize(
        &mut modules,
        &root,
        &curios_base::Qualifier::from([prefix]),
        &directory,
    )?;
    modules.insert_root(prefix, curios_base::RootKind::Ordinary, root);

    Ok(modules)
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
            curios_unit::Scope::over(from_ref(&prelude)),
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
        curios_pipeline::recheck(module, budget, curios_unit::Scope::over(from_ref(&prelude)))
    })
}

/// The default reduction budget, re-exported beside the compile helpers that take it.
pub use curios_pipeline::DEFAULT_STEP_BUDGET;

/// Parse `source` (no external modules) and run it.
#[cfg(test)]
pub(crate) fn run_text<H: curios_runtime::HostOps + Send + Sync + 'static>(
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(&entrypoint, curios_text::RootSource::none(), host)
}

/// Open a `.crs` entrypoint at `path`, paired with a [`curios_text::RootSource::file_system`] rooted at its parent directory — the standard way to resolve a program's imports relative to the file it lives in.
pub fn load(path: &Path) -> Result<(curios_text::Entrypoint, curios_text::RootSource), String> {
    let entrypoint = curios_text::Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader =
        curios_text::RootSource::file_system(path.parent().unwrap_or(Path::new(".")).to_path_buf());

    Ok((entrypoint, loader))
}
