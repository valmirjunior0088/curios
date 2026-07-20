# Bootstrap Phases 7 and 8 — generation stability and production integration

Working implementation specification for the final two bootstrap implementation phases. They follow the [authoritative Curios backend](07_BACKEND_SPEC.md) and consume the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the bootstrap completion criteria and the final verification gate.

## Phase 7 — generation stability

Build three compiler generations from one clean source snapshot and one pinned set of options:

```text
S0: the frozen Rust compiler
S1: S0 compiles the Curios compiler source
S2: S1 compiles the same Curios compiler source
S3: S2 compiles the same Curios compiler source
```

S1 need not match S0's implementation or raw output. S2 and S3 must be byte-identical raw compiler Wasm artifacts before Binaryen and Wasmtime precompilation. Their foreign manifests, compiler metadata, embedded source hashes, and diagnostics must also be identical.

If S2 and S3 differ, the bootstrap is not complete. A normalized or behavioral comparison may diagnose the difference but does not replace the fixed-point requirement. Binaryen output and `.cwasm` bytes are excluded because they are downstream host products and may carry backend- or platform-specific details.

Run the complete Curios language test corpus through S2 and S3, including accepted programs, rejected programs, deep-stack fixtures, foreign declarations, prelude compilation, deterministic repeated builds, and representative compiler-sized inputs. Both generations must agree on acceptance, diagnostics at the specified comparison level, raw program Wasm, and foreign manifests.

## Phase 8 — production integration and retirement of active dual maintenance

Make the self-hosted compiler artifact the default compiler used by the Rust CLI, browser compiler path where feasible, and release process. Keep artifact construction out of the slim runtime launcher and avoid linking S0 into binaries that only host an already-built compiler.

Document how a clean checkout builds S1, verifies S2/S3, refreshes any embedded compiler artifact, and recovers using S0. CI must exercise both the ordinary production build and the generation-stability job.

Mark the Rust pipeline as frozen bootstrap code or move it behind an explicit stage-zero build path. Remove production dependencies on its AST and IR APIs. Future compiler features land only in Curios unless they alter the permanent Rust host boundary.
