//! Driving the compile pipeline for the CLI, with the `--print` stage dump wired in. `stage_printer` is the single owner of how each IR stage is selected and rendered.

use {
    curios::{Verdicts, compile_with_units, load},
    curios_package::Target,
    curios_pipeline::Cache,
    curios_pipeline::{CompileError, Stage},
    curios_wasm::Module,
    std::path::{Path, PathBuf},
};

/// Build the observer closure that prints each requested IR stage to stderr. `print` is the comma-separated stage list from `--print`; empty segments are dropped (the flag's absence arrives as `""`), and an unknown stage name is an error rather than a silently empty selection.
fn stage_printer(print: &str) -> Result<impl Fn(Stage<'_>) + '_, String> {
    let stages = print
        .split(',')
        .filter(|name| !name.is_empty())
        .collect::<Vec<_>>();

    if let Some(unknown) = stages.iter().find(|name| !Stage::NAMES.contains(name)) {
        return Err(format!(
            "unknown --print stage {unknown:?}; the stages are {}",
            Stage::NAMES.join(", ")
        ));
    }

    Ok(move |stage: Stage<'_>| {
        if stages.contains(&stage.name()) {
            eprintln!("\n=== {} ===\n{stage}", stage.name());
        }
    })
}

/// Compile `target` to a wasm module, against the `--unit` packages and whatever its own manifest declares.
///
/// A `--unit` package is the already-resolved form of a manifest entry, so it goes in front of the graph's own order: the order arguments arrive in *is* dependency order. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn compile_target(
    budget: u64,
    print: &str,
    units: &[PathBuf],
    target: Target,
) -> Result<Module, CompileError> {
    let mut scope = load_units(units)?;

    // A bare file has no project, so it has no store to consult: what it may reuse is a fact about the project it is in, and it is in none.
    let (entry, cache) = match target {
        Target::File(path) => (path, None),
        Target::Executable {
            entry, units, root, ..
        } => {
            scope.extend(units);

            (entry, Some(Verdicts::at(root)))
        }
    };

    compile_entry(
        budget,
        print,
        scope,
        &entry,
        cache.as_ref().map(|cache| cache as &dyn Cache),
    )
}

/// Compile `entry` against `units` in the order given, printing any requested IR stages along the way.
pub(crate) fn compile_entry(
    budget: u64,
    print: &str,
    units: Vec<curios_text::RootSource>,
    entry: &Path,
    cache: Option<&dyn Cache>,
) -> Result<Module, CompileError> {
    let printer = stage_printer(print).map_err(CompileError::Failure)?;
    let (entrypoint, loader) = load(entry).map_err(CompileError::Failure)?;

    // The CLI doesn't yet expose a way to supply `foreign` implementations, so its `ForeignStore` is dropped here.
    compile_with_units(budget, &units, &entrypoint, loader, cache, printer)
        .map(|(module, _foreigns)| module)
}

/// Read every `--unit DIR`'s manifest in the order written, which is the order they are compiled in.
pub(crate) fn load_units(units: &[PathBuf]) -> Result<Vec<curios_text::RootSource>, CompileError> {
    curios_package::mounted(units).map_err(CompileError::Failure)
}
