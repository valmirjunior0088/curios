//! Driving the compile / type-check pipeline for the CLI, with the `--print`
//! stage dump wired in. `stage_printer` is the single owner of how each IR stage
//! is selected and rendered, shared by both entrypoints below.

use {
    curios::{Stage, compile_entrypoint, load, typecheck_entrypoint, wasm},
    std::{path::Path, time::Duration},
};

/// Build the observer closure that prints each requested IR stage to stderr.
/// `print` is the comma-separated stage list from `--print`; an unlisted stage is
/// skipped. The closure handles every `Stage` variant, so it serves both the full
/// compile and the type-check path (which only ever emits `Text`/`Core`).
fn stage_printer(print: &str) -> impl Fn(Stage<'_>) + '_ {
    let stages = print.split(',').collect::<Vec<_>>();

    move |stage| match stage {
        Stage::Text(text) if stages.contains(&"text") => eprintln!("\n=== text ===\n{text}"),
        Stage::Core(core) if stages.contains(&"core") => eprintln!("\n=== core ===\n{core}"),
        Stage::Ersd(ersd) if stages.contains(&"ersd") => eprintln!("\n=== ersd ===\n{ersd}"),
        Stage::ErsdOptm(ersd) if stages.contains(&"ersd-optm") => {
            eprintln!("\n=== ersd-optm ===\n{ersd}")
        }
        Stage::Cont(cont) if stages.contains(&"cont") => eprintln!("\n=== cont ===\n{cont}"),
        Stage::ContOptm(cont) if stages.contains(&"cont-optm") => {
            eprintln!("\n=== cont-optm ===\n{cont}")
        }
        Stage::Wasm(wasm) if stages.contains(&"wasm") => eprintln!("\n=== wasm ===\n{wasm}"),
        _ => {}
    }
}

/// Compile `input_path` through the full pipeline to a wasm module, printing any
/// requested IR stages along the way.
pub(crate) fn compile_file(
    timeout: Duration,
    print: &str,
    input_path: &Path,
) -> Result<wasm::Module, String> {
    let (entrypoint, loader) = load(input_path)?;

    // The CLI doesn't yet expose a way to supply `foreign` implementations,
    // so its `ForeignStore` is dropped here.
    compile_entrypoint(timeout, &entrypoint, loader, stage_printer(print))
        .map(|(module, _foreigns)| module)
}

/// Type-check `input_path` only (the fast `check` path), printing any requested
/// pre-lowering stages (`text`/`core`).
pub(crate) fn typecheck_file(timeout: Duration, print: &str, input_path: &Path) -> Result<(), String> {
    let (entrypoint, loader) = load(input_path)?;

    typecheck_entrypoint(timeout, &entrypoint, loader, stage_printer(print))
}
