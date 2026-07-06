# curios-rt

Runtime-only Curios engine (lib) + the launcher stub (bin `curios-rt`). Deserializes a precompiled module and runs it on wasmtime, wiring the `sys.io_*` host imports (plus any `ffi.*` bindings an embedder supplies for a program's own `foreign` declarations); **never** links Cranelift or Binaryen.

Depends only on [`curios-abi`](../curios-abi) (for the wire constants), not on [`curios`](../curios) — that's what keeps it slim and lets `curios` depend back on it without a cycle. `cargo build --package curios-rt` in isolation is the slim launcher embedded into every compiled `curios` executable.

See [AGENTS.md](../AGENTS.md#crates-features-and-the-slim-launcher) for the full mechanism. API docs: `cargo doc --package curios-rt --open`.
