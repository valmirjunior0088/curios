# curios-js

The Curios ↔ JavaScript boundary: `wasm-bindgen` exports of [`curios-pipeline`](../curios-pipeline) (`compile`) and the browser run harness (`run`), built for `wasm32-unknown-unknown` with `cargo build` + `wasm-bindgen-cli --target web` for the browser playground (see AGENTS.md's Gotchas for why not `wasm-pack`).

The harness JS ships as a wasm-bindgen snippet (`js/harness.js`); its wire codes derive from [`curios-abi`](../curios-abi), while its bridge module declares the same structural `array (mut i8)` `Bin` payload type the compiler emits.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-js --open`.
