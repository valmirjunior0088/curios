# curios

The facade + driver + CLI: re-exports the five pipeline-stage crates under their historical module names (`text`, `core`, `ersd`, `cont`, `wasm`), plus [`curios-pipeline`](../curios-pipeline) and the compile/precompile/run-from-source helpers and the clap-based CLI (bin `curios`).

The only crate in the workspace that links Cranelift (via wasmtime's `cranelift` feature) and Binaryen. See the root [README.md](../README.md) for building and running the CLI, and [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios --open`.
