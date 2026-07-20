# Bootstrap Phase 3 — surface frontend in shadow mode

Working implementation specification for the third bootstrap implementation phase. It follows the [Curios base and Wasm leaf port](03_BASE_AND_WASM_LEAF_SPEC.md) and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the ownership model's shadow discipline and the differential testing strategy.

Port source storage, lexing, parsing, module discovery, interface resolution, the surface AST, printing needed by diagnostics, dependency ordering, and lowering to Core.

The Curios frontend runs in shadow mode over the complete embedded `sys`/`syn`/`std` sources and the integration corpus. Rust remains authoritative until Core elaboration and erasure are also ready, avoiding a live Core serialization boundary.

Surface tests compare parse success, item and term structure, exact spans where semantically relevant, module graphs, interface visibility, lowering results through a test normalization, and diagnostic categories. Parse-print-parse properties supplement direct comparison; they do not replace malformed-input tests.
