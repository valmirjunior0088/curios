# curios-abi

Host/guest ABI wire constants shared by the Curios compiler and runtime: the numeric codes for `/sys/Io`'s status, poll-event, and open-mode tags, plus the `ForeignStore` of `ForeignFunction` rows describing every host operation.

A pure leaf with no dependencies on the rest of the workspace — shared by the compiler's pipeline-stage crates and by [`curios-rt`](../curios-rt) (the runtime), which is what lets the runtime depend on the wire contract without depending on the compiler itself.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-abi --open`.
