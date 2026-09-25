//! The Curios ↔ JavaScript boundary, for a browser build (`cargo x js`: `cargo build` for wasm32, then `--target web` bindings — no `wasm-pack`): a wasm-bindgen export of the pure compile pipeline ([`compile`]) plus the browser run harness ([`run`]). The harness spells the wire names (`sys`/`ffi` namespaces, `sys.*` keys, the entry export) directly, like any embedder; the codes and tokens it answers with derive from `curios-abi`, the same source the compiler and runtime cite, and a program's own `foreign` rows arrive with the program, as `compile` described them.

mod abi;
use abi::*;

mod bridge;
use bridge::*;

mod foreigns;
use foreigns::*;

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

/// Compile `source` (no external module imports — see `RootSource::none()`) to `{ program, foreigns }`: the wasm module bytes, and the program's own `foreign` rows as plain data, one `{ name, params, results }` each — or a formatted error string on parse/type/lowering failure. `run` takes the object whole, and implements each row through `hooks.foreign`, keyed by the row's `name`.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Object, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    let (module, foreigns) =
        compile_with_prelude(BUDGET, &entrypoint, &RootSource::none(), |_| {})?;

    let compiled = Object::new();
    set(
        &compiled,
        "program",
        &Uint8Array::from(to_bytes(&module).as_slice()),
    );
    set(&compiled, "foreigns", &foreign_rows(&foreigns));

    Ok(compiled)
}
