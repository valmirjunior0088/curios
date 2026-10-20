//! The `run` door over the JS harness. The heavy lifting — instantiating the program, satisfying the host imports, holding each `foreign` hook to its row, catching the exit signal — lives in `harness.mjs`, shipped as a wasm-bindgen snippet; this assembles its `config` from the [`configuration()`] every host of the browser is given, so a playground calling `run` never spells a wire code itself.

use {
    crate::{bridge_bytes, configuration},
    js_sys::{JSON, Object, Promise, Reflect, Uint8Array},
    wasm_bindgen::{JsCast, prelude::*},
};

#[wasm_bindgen(module = "/src/harness.mjs")]
extern "C" {
    #[wasm_bindgen(js_name = run)]
    fn harness_run(config: Object) -> Promise;
}

fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

/// Run a program `compile` compiled, handed back whole. `hooks` is an optional `{ onStdout?, onStderr?, foreign? }` object — `onStdout`/`onStderr` are per-write `Uint8Array` callbacks, and `foreign` implements the program's own `foreign` declarations, keyed by fully qualified name (e.g. `{ "/frobnicate": fn }`): each hook takes the row's operands in order, then an `AbortSignal` that fires where the call is cancelled, and answers its results — nothing, the one value, or an object keyed by the results' labels — in the JavaScript types `harness.mjs` states, at once or as a promise of them, which the call suspends on while the rest of the program runs; an answer outside the row stops the program. The promise resolves to `{ stdout, stderr, exitCode, trap }` — `trap` is `null` unless the program was stopped, and then the reason: `panicked: …` with the compiler's sentence for a refused computation, or the engine's or the harness's own text otherwise.
#[wasm_bindgen]
pub fn run(compiled: JsValue, hooks: JsValue) -> Promise {
    let config = JSON::parse(&configuration())
        .expect("the configuration renders as JSON")
        .unchecked_into::<Object>();
    set(&config, "compiled", &compiled);
    set(
        &config,
        "bridge",
        &Uint8Array::from(bridge_bytes().as_slice()),
    );
    set(&config, "hooks", &hooks);

    harness_run(config)
}
