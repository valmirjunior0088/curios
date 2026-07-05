# curios-binaryen

Wasm optimization for curios via a prebuilt Binaryen release (`build.rs` downloads and links `libbinaryen.a`; no vendored source).

Deliberately the last stage of the pipeline: it consumes and produces serialized module bytes, after `wasm::to_bytes`, and knows nothing about any curios IR. Semantic optimization belongs upstream, in `optm/`.

See [AGENTS.md](../AGENTS.md#gotchas) for the download/checksum/offline mechanism. API docs: `cargo doc --package curios-binaryen --open`.
