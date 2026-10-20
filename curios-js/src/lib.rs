//! The Curios compiler in the browser, for a browser build (`cargo xtask bundle build`: `cargo build` for wasm32, then `--target web` bindings — no `wasm-pack`): a wasm-bindgen export of the pure compile pipeline, [`compile`]. Running what it compiles is `curios-browser`'s, whose `run` the same bundle exports, and the rows `compile` hands back are rendered by it, so the playground's two calls agree on one description of a program.

use {
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_wasm::to_bytes,
    js_sys::{JSON, Object, Reflect, Uint8Array},
    wasm_bindgen::prelude::*,
};

/// `run`, exported by the crate that owns running a program in the browser, and named here so the bundle links it.
pub use curios_browser::run;

/// The same budget the native compiler uses, so a program that compiles in the playground compiles at the command line and the reverse. A wall-clock bound could not promise that: the tab and the terminal are different machines.
const BUDGET: u64 = DEFAULT_STEP_BUDGET;

fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

/// Compile `source` (no external module imports — see `RootSource::none()`) to `{ program, foreigns }`: the wasm module bytes, and the program's own `foreign` rows as plain data, one `{ name, params, results }` each, `never: true` in place of `results` where a row does not return — or a formatted error string on parse/type/lowering failure. `run` takes the object whole, and implements each row through `hooks.foreign`, keyed by the row's `name`.
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
    set(
        &compiled,
        "foreigns",
        &JSON::parse(&curios_browser::rows(&foreigns)).expect("the rows render as JSON"),
    );

    Ok(compiled)
}
