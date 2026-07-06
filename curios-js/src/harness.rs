//! The `run` façade over the JS harness. The heavy lifting — instantiating
//! the program, satisfying the host imports, catching the exit signal —
//! lives in `js/harness.js` (shipped as a wasm-bindgen snippet); this module
//! assembles its `config` from the same `curios-abi`-derived code tables
//! `abi()` exposes, so a playground calling `run` never spells a wire code
//! itself.

use {
    crate::{abi::abi, bridge::bridge_bytes, set},
    js_sys::{Object, Promise, Uint8Array},
    wasm_bindgen::prelude::*,
};

#[wasm_bindgen(module = "/js/harness.js")]
extern "C" {
    #[wasm_bindgen(js_name = run)]
    fn harness_run(config: Object) -> Promise;
}

/// Run a compiled program in the browser. `hooks` is an optional
/// `{ onStdout?, onStderr?, foreign? }` object — `onStdout`/`onStderr` are
/// per-write `Uint8Array` callbacks, `foreign` implements the program's own
/// `foreign` declarations, keyed by fully qualified name (e.g.
/// `{ "/frobnicate": fn }`). The promise resolves to
/// `{ stdout, stderr, exitCode, trap }`.
#[wasm_bindgen]
pub fn run(program: &[u8], hooks: JsValue) -> Promise {
    let config = abi();
    set(&config, "program", &Uint8Array::from(program));
    set(
        &config,
        "bridge",
        &Uint8Array::from(bridge_bytes().as_slice()),
    );
    set(&config, "hooks", &hooks);

    harness_run(config)
}
