# curios-binaryen

Wasm optimization for curios via the vendored Binaryen library (`curios-binaryen/binaryen/` — vendored C++, never edit directly; see `build.rs` for the re-vendoring procedure).

Deliberately the last stage of the pipeline: it consumes and produces serialized module bytes, after `wasm::to_bytes`, and knows nothing about any curios IR. Semantic optimization belongs upstream, in `optm/`.

See [AGENTS.md](../AGENTS.md#gotchas) before touching the vendored tree. API docs: `cargo doc --package curios-binaryen --open`.
