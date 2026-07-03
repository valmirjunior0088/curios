# curios-js

The curios ↔ JavaScript boundary: `wasm-bindgen` exports of [`curios-pipeline`](../curios-pipeline) (`compile`, `typecheck`) plus the browser run harness (`run`, with `bridge_bytes` and `abi` as its exported building blocks), built with `wasm-pack --target web` for the browser playground.

The harness JS ships as a wasm-bindgen snippet (`js/harness.js`); everything it knows about the host boundary — import roster, wire codes, the `Bin` heap type — derives from [`curios-abi`](../curios-abi) and [`curios-cont`](../curios-cont), the same sources the compiler and runtime cite.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-js --open`.
