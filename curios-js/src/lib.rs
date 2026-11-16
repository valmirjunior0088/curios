//! The Curios compiler in the browser, for a browser build (`cargo xtask bundle build`: `cargo build` for wasm32, then `--target web` bindings — no `wasm-pack`): a wasm-bindgen export of the pure compile pipeline, [`compile`]. Running what it compiles is `curios-browser`'s, whose `run` the same bundle exports, and the rows `compile` hands back are rendered by it, so the playground's two calls agree on one description of a program.

use {
    curios_common::{Kind, Style, runs},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
    curios_wasm::to_bytes,
    js_sys::{Array, JSON, Object, Reflect, Uint8Array},
    wasm_bindgen::prelude::*,
};

// `run` is exported by the crate that owns running a program in the browser; naming that crate is what links it into the bundle.
use curios_browser as _;

/// The same budget the native compiler uses, so a program that compiles in the playground compiles at the command line and the reverse. A wall-clock bound could not promise that: the tab and the terminal are different machines.
const BUDGET: u64 = DEFAULT_BUDGET;

fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

/// Compile `source` (no external module imports — see `RootSource::none()`) to `{ program, foreigns }`: the wasm module bytes, and the program's own `foreign` rows as plain data, one `{ name, params, results }` each, `never: true` in place of `results` where a row does not return — or a formatted error string on parse/type/lowering failure. `run` takes the object whole, and implements each row through `hooks.foreign`, keyed by the row's `name`.
///
/// A refusal is thrown as the runs of its report, `{ text, role }` each in order, `role` the name of the role the terminal paints the run in — `bold`, `dim`, `troubled` or `aside` — and absent where it is plain ([`curios_common::runs`]): a page paints them as the terminal does, and their texts joined are the report a reader who sees no colour reads.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Object, JsValue> {
    let painted = Style {
        here: None,
        painted: true,
    };
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| refused(&error.report().rendered(Kind::Error, &painted)))?;

    let (module, foreigns) = compile_with_prelude(BUDGET, &entrypoint, &RootSource::none(), |_| {})
        .map_err(|error| refused(&error.rendered(&painted)))?;

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

/// A report painted as the terminal paints it, as the runs a page paints: `{ text, role }` each, `role` absent where the run is plain.
fn refused(painted: &str) -> JsValue {
    let read = Array::new();
    for (role, text) in runs(painted) {
        let run = Object::new();
        set(&run, "text", &JsValue::from_str(&text));
        if let Some(role) = role {
            set(&run, "role", &JsValue::from_str(role.name()));
        }
        read.push(&run);
    }
    read.into()
}
