# curios-binaryen

Wasm optimization for Curios, built from a downloaded Binaryen source release (`build.rs` fetches the tagged source tarball and builds it via CMake; no vendored source in the repo).

Deliberately the last stage of the pipeline: it consumes and produces serialized module bytes, after `wasm::to_bytes`, and knows nothing about any Curios IR. Semantic optimization belongs upstream, in `optimize/`.

See [AGENTS.md](../AGENTS.md#gotchas) for the download/checksum/offline mechanism. API docs: `cargo doc --package curios-binaryen --open`.
