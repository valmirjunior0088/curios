//! Driving the compile pipeline for the CLI, with the `--print` stage dump
//! wired in. `stage_printer` is the single owner of how each IR stage is
//! selected and rendered.

use {
    curios::load,
    curios_pipeline::{Stage, compile_entrypoint},
    std::path::Path,
};

/// Build the observer closure that prints each requested IR stage to stderr.
/// `print` is the comma-separated stage list from `--print`; an unlisted stage is
/// skipped.
fn stage_printer(print: &str) -> impl Fn(Stage<'_>) + '_ {
    let stages = print.split(',').collect::<Vec<_>>();

    move |stage| {
        if stages.contains(&stage.name()) {
            eprintln!("\n=== {} ===\n{stage}", stage.name());
        }
    }
}

/// Compile `input_path` through the full pipeline to a wasm module, printing any
/// requested IR stages along the way.
pub(crate) fn compile_file(
    budget: u64,
    print: &str,
    input_path: &Path,
) -> Result<curios_wasm::Module, String> {
    let (entrypoint, loader) = load(input_path)?;

    // The CLI doesn't yet expose a way to supply `foreign` implementations,
    // so its `ForeignStore` is dropped here.
    compile_entrypoint(budget, &entrypoint, loader, stage_printer(print))
        .map(|(module, _foreigns)| module)
}
