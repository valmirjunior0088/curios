# curios-js

The curios ↔ JavaScript boundary: `wasm-bindgen` exports (`compile`, `typecheck`) of [`curios-pipeline`](../curios-pipeline), built with `wasm-pack --target web` for the browser playground.

No host imports are satisfied here — this crate only turns source text into a `wasm::Module`'s bytes, or a formatted error string.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-js --open`.
