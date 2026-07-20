# Bootstrap Phase 2 — Curios base and Wasm leaf

Working implementation specification for the second bootstrap implementation phase. It follows the [feasibility gate](02_FEASIBILITY_SPEC.md) and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md).

Port the reusable equivalents of `curios-abi` and the necessary parts of `curios-base`: spans, names, entropy, packed binary values, numeric leaves, foreign signatures, printers, parser state, and compiler collections.

Port the Wasm model and binary encoder early as a leaf component. It is independently testable against the existing Rust writer and proves that the Curios compiler can construct large binary artifacts efficiently. The initial Wasm port need only encode the feature set emitted by Curios; parsing arbitrary external Wasm and WAT is not on the critical bootstrap path.

At the end of this phase, the hybrid compiler still delegates production compilation to `bootstrap_compile`.
