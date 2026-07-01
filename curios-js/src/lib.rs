//! wasm-bindgen exports of the pure compile pipeline, for a browser build
//! (`wasm-pack build curios-js --target web`). No host imports are satisfied
//! here — that is a separate concern (see `run`); this crate only turns
//! source text into a `wasm::Module`'s bytes, or a formatted error string.

use {
    curios_pipeline::{compile_entrypoint, typecheck_entrypoint},
    curios_text::{Entrypoint, NullLoader},
    std::time::Duration,
    wasm_bindgen::prelude::*,
};

/// Generous but bounded: a playground compile should never hang the tab.
const TIMEOUT: Duration = Duration::from_secs(10);

/// Compile `source` (no external module imports — see `NullLoader`) to wasm
/// bytes, or a formatted error string on parse/type/lowering failure.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    let module = compile_entrypoint(TIMEOUT, &entrypoint, &NullLoader, |_| {})?;

    Ok(curios_wasm::to_bytes(&module))
}

/// Type-check `source` and stop, skipping the erase/cont/wasm lowering —
/// the fast path for a playground's live-typecheck-as-you-type mode.
#[wasm_bindgen]
pub fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    typecheck_entrypoint(TIMEOUT, &entrypoint, &NullLoader, |_| {})
}
