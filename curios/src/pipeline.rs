//! Driving the compile pipeline for the CLI, with the `--print` stage dump wired in. `stage_printer` is the single owner of how each IR stage is selected and rendered.

use {
    crate::cli::parse_unit,
    curios::{compile_with_units, load, load_unit},
    curios_pipeline::{CompileError, Stage},
    curios_wasm::Module,
    std::path::Path,
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

/// Compile `input_path` through the full pipeline to a wasm module, printing any requested IR stages along the way. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn compile_file(
    budget: u64,
    print: &str,
    units: &[String],
    input_path: &Path,
) -> Result<Module, CompileError> {
    let printer = stage_printer(print).map_err(CompileError::Failure)?;
    let (entrypoint, loader) = load(input_path).map_err(CompileError::Failure)?;
    let units = load_units(units)?;

    // The CLI doesn't yet expose a way to supply `foreign` implementations, so its `ForeignStore` is dropped here.
    compile_with_units(budget, &units, &entrypoint, loader, printer)
        .map(|(module, _foreigns)| module)
}

/// Load every `--unit PREFIX=PATH` in the order written, which is the order they are compiled in.
pub(crate) fn load_units(units: &[String]) -> Result<Vec<curios_text::RootSource>, CompileError> {
    units
        .iter()
        .map(|unit| {
            let (prefix, path) = parse_unit(unit).map_err(CompileError::Failure)?;
            Ok(load_unit(&prefix, &path))
        })
        .collect()
}
