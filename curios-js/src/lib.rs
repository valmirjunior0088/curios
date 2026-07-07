//! The Curios ↔ JavaScript boundary, for a browser build (`cargo build` +
//! `wasm-bindgen-cli --target web` — no `wasm-pack`, see AGENTS.md's
//! Gotchas): a wasm-bindgen export of the pure compile
//! pipeline ([`compile`]) plus the browser run harness —
//! [`run`] executes a compiled program against a JS host, and its building
//! blocks ([`bridge_bytes`], [`abi`]) are exported for playgrounds that wire
//! the harness themselves. The harness spells the wire names (`sys`/`ffi`
//! namespaces, `sys.io_*` keys, the entry export) directly, like any
//! embedder; the numeric status/stdio codes it answers with derive from
//! `curios-abi`, the same source the compiler and runtime cite.

mod abi;
pub use abi::*;

mod bridge;
pub use bridge::*;

mod harness;
pub use harness::*;

use {
    curios_pipeline::compile_entrypoint,
    curios_text::{Entrypoint, RootSource},
    js_sys::{Object, Reflect, Uint8Array},
    std::time::Duration,
    wasm_bindgen::prelude::*,
};

/// Generous but bounded: a playground compile should never hang the tab.
const TIMEOUT: Duration = Duration::from_secs(10);

pub(crate) fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

/// Compile `source` (no external module imports — see `RootSource::none()`) to
/// the wasm module bytes, or a formatted error string on parse/type/lowering
/// failure. A program's own `foreign` declarations import under `ffi` by
/// fully qualified name — the caller implements them via `run`'s
/// `hooks.foreign`, keyed by exactly those names.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Uint8Array, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    let (module, _foreigns) = compile_entrypoint(TIMEOUT, &entrypoint, RootSource::none(), |_| {})?;

    Ok(Uint8Array::from(curios_wasm::to_bytes(&module).as_slice()))
}
