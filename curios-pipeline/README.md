# curios-pipeline

The pure pipeline driver: `compile_entrypoint`/`typecheck_entrypoint`/`Stage`, chaining `text` → `core` → `ersd` → `cont` → `wasm` with no runtime, Binaryen, or CLI dependencies.

Extracted from [`curios`](../curios) so a wasm32 build of the compiler (used by [`curios-js`](../curios-js)) doesn't have to drag those in.

See [AGENTS.md](../AGENTS.md#the-pipeline) for the full pipeline diagram. API docs: `cargo doc --package curios-pipeline --open`.
