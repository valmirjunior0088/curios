# curios

The driver + CLI: the compile/precompile/run-from-source helpers, built on [`curios-pipeline`](../curios-pipeline)'s `compile_entrypoint`/`Stage`, plus the clap-based CLI (bin `curios`).

The only crate in the workspace that links Cranelift (via wasmtime's `cranelift` feature) and Binaryen. See the root [README.md](../README.md) for building and running the CLI, and [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios --open`.
