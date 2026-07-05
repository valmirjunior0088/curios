//! The curios ↔ JavaScript boundary, for a browser build (`wasm-pack build
//! curios-js --target web`): wasm-bindgen exports of the pure compile
//! pipeline ([`compile`], [`typecheck`]) plus the browser run harness —
//! [`run`] executes a compiled program against a JS host, and its building
//! blocks ([`bridge_bytes`], [`abi`]) are exported for playgrounds that wire
//! the harness themselves. Everything the harness knows about the host
//! boundary derives from `curios-abi` and `curios-cont`, the same sources the
//! compiler and runtime cite, so the browser host cannot drift from them.

mod abi;
pub use abi::*;

#[cfg(test)]
mod abi_tests;

mod bridge;
pub use bridge::*;

#[cfg(test)]
mod bridge_tests;

mod harness;
pub use harness::*;

use {
    curios_pipeline::{compile_entrypoint, typecheck_entrypoint},
    curios_text::{Entrypoint, RootSource},
    js_sys::{Array, Object, Reflect, Uint8Array},
    std::time::Duration,
    wasm_bindgen::prelude::*,
};

/// Generous but bounded: a playground compile should never hang the tab.
const TIMEOUT: Duration = Duration::from_secs(10);

/// Compile `source` (no external module imports — see `RootSource::None`) to
/// a `{ bytes, foreignNames }` object, or a formatted error string on
/// parse/type/lowering failure. `bytes` is the wasm module; `foreignNames` is
/// the `env`-tier import roster the program's own `foreign` declarations
/// require — a caller supplies them via `run`'s `hooks.foreign`.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Object, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    let (module, foreigns) = compile_entrypoint(TIMEOUT, &entrypoint, RootSource::None, |_| {})?;

    let object = Object::new();

    Reflect::set(
        &object,
        &JsValue::from_str("bytes"),
        &Uint8Array::from(curios_wasm::to_bytes(&module).as_slice()),
    )
    .expect("Reflect::set on a plain object");

    Reflect::set(
        &object,
        &JsValue::from_str("foreignNames"),
        &foreigns
            .iter()
            .map(|function| JsValue::from_str(&function.name))
            .collect::<Array>(),
    )
    .expect("Reflect::set on a plain object");

    Ok(object)
}

/// Type-check `source` and stop, skipping the erase/cont/wasm lowering —
/// the fast path for a playground's live-typecheck-as-you-type mode.
#[wasm_bindgen]
pub fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    typecheck_entrypoint(TIMEOUT, &entrypoint, RootSource::None, |_| {})
}
