//! The `run` façade over the JS harness. The heavy lifting — instantiating
//! the program, satisfying the host imports, catching the exit signal —
//! lives in `js/harness.js` (shipped as a wasm-bindgen snippet); this module
//! assembles its `config` from the same `curios-abi`-derived facts `abi()`
//! exposes, so a playground calling `run` never spells an ABI detail itself.

use {
    crate::{abi::abi_object, bridge::bridge_bytes},
    js_sys::{Array, Object, Promise, Reflect, Uint8Array},
    wasm_bindgen::prelude::*,
};

#[wasm_bindgen(module = "/js/harness.js")]
extern "C" {
    #[wasm_bindgen(js_name = run)]
    fn harness_run(config: Object) -> Promise;
}

/// Run a compiled program in the browser. `foreign_names` is `compile`'s
/// `foreignNames` roster (empty for a program with no `foreign`
/// declarations). `hooks` is an optional `{ onStdout?, onStderr?, foreign? }`
/// object — `onStdout`/`onStderr` are per-write `Uint8Array` callbacks,
/// `foreign` is a `{ name: fn, ... }` map implementing `foreign_names`. The
/// promise resolves to `{ stdout, stderr, exitCode, trap }`.
#[wasm_bindgen]
pub fn run(program: &[u8], foreign_names: Array, hooks: JsValue) -> Promise {
    let config = abi_object();

    Reflect::set(
        &config,
        &JsValue::from_str("program"),
        &Uint8Array::from(program),
    )
    .expect("Reflect::set on a plain object");

    Reflect::set(
        &config,
        &JsValue::from_str("bridge"),
        &Uint8Array::from(bridge_bytes().as_slice()),
    )
    .expect("Reflect::set on a plain object");

    Reflect::set(&config, &JsValue::from_str("foreignNames"), &foreign_names)
        .expect("Reflect::set on a plain object");

    Reflect::set(&config, &JsValue::from_str("hooks"), &hooks)
        .expect("Reflect::set on a plain object");

    harness_run(config)
}
