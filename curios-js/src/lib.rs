//! The Curios ↔ JavaScript boundary, for a browser build (`cargo xtask js`: `cargo build` for wasm32, then `--target web` bindings — no `wasm-pack`): a wasm-bindgen export of the pure compile pipeline ([`compile`]) plus the browser run harness ([`run`]). The harness spells the wire names (`sys`/`ffi` namespaces, `sys.*` keys, the entry export) directly, like any embedder; the numeric status/stdio codes it answers with derive from `curios-abi`, the same source the compiler and runtime cite.

mod abi;
use abi::*;

mod bridge;
use bridge::*;

mod harness;
pub use harness::run;

#[cfg(test)]
mod tests;

use {
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::to_bytes,
    js_sys::{Object, Reflect, Uint8Array},
    wasm_bindgen::prelude::*,
};

/// The same budget the native compiler uses, so a program that compiles in the playground compiles at the command line and the reverse. A wall-clock bound could not promise that: the tab and the terminal are different machines.
const BUDGET: u64 = DEFAULT_STEP_BUDGET;

pub(crate) fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

/// Compile `source` (no external module imports — see `RootSource::none()`) to the wasm module bytes, or a formatted error string on parse/type/lowering failure. A program's own `foreign` declarations import under `ffi` by fully qualified name — the caller implements them via `run`'s `hooks.foreign`, keyed by exactly those names.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Uint8Array, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    let (module, _foreigns) =
        compile_with_prelude(BUDGET, &entrypoint, &RootSource::none(), |_| {})?;

    Ok(Uint8Array::from(to_bytes(&module).as_slice()))
}
