# curios-wasm

The curios wasm module model, parser, and binary writer/encoder.

A pristine leaf on top of [`curios-base`](../curios-base): it knows nothing about any curios IR, only about wasm itself. [`curios-cont`](../curios-cont) is the first stage to depend on it, emitting a `wasm::Module` as its final output (`cont/to_wasm`).

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-wasm --open`.
