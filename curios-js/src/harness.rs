//! The `run` façade over the JS harness. The heavy lifting — instantiating the program, satisfying the host imports, holding each `foreign` hook to its row, catching the exit signal — lives in `harness.js` (shipped as a wasm-bindgen snippet); this module assembles its `config` from the same `curios-abi`-derived code tables as the native runtime, so a playground calling `run` never spells a wire code itself.

use {
    crate::{abi, bridge_bytes, set},
    js_sys::{Object, Promise, Uint8Array},
    wasm_bindgen::prelude::*,
};

#[wasm_bindgen(module = "/src/harness.js")]
extern "C" {
    #[wasm_bindgen(js_name = run)]
    fn harness_run(config: Object) -> Promise;
}

/// Run a program `compile` compiled, handed back whole. `hooks` is an optional `{ onStdout?, onStderr?, foreign? }` object — `onStdout`/`onStderr` are per-write `Uint8Array` callbacks, and `foreign` implements the program's own `foreign` declarations, keyed by fully qualified name (e.g. `{ "/frobnicate": fn }`): each hook takes the row's operands in order and answers its results — nothing, the one value, or an object keyed by the results' labels — in the JavaScript types `harness.js` states, and an answer outside the row stops the program. The promise resolves to `{ stdout, stderr, exitCode, trap }` — `trap` is `null` unless the program was stopped, and then the reason: `panicked: …` with the compiler's sentence for a refused computation, or the engine's or the harness's own text otherwise.
#[wasm_bindgen]
pub fn run(compiled: JsValue, hooks: JsValue) -> Promise {
    let config = abi();
    set(&config, "compiled", &compiled);
    set(
        &config,
        "bridge",
        &Uint8Array::from(bridge_bytes().as_slice()),
    );
    set(&config, "hooks", &hooks);

    harness_run(config)
}
