# Bootstrap Phase 5 — frontend ownership and the Ersd cutover

Working implementation specification for the fifth bootstrap implementation phase. It follows the [shadow-mode Core port](05_CORE_ELABORATION_SPEC.md) and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the Ersd interchange envelope, the Rust backend bridge, and the frontend-cutover done bar.

Implement the Ersd envelope encoder in Curios and decoder in Rust. Round-trip every Ersd constructor, validate corrupt data, pin canonical encoding, and compare Rust-decoded output with the Ersd produced directly by S0.

Switch the hybrid compiler's production path to:

```text
Curios frontend and erasure → bootstrap_backend → raw Wasm
```

The switch is atomic. The compiler no longer calls `bootstrap_compile`, and a Curios frontend error is returned directly rather than retried through Rust.

This is the principal architectural milestone. From this point onward, Curios owns the source language, ASTs, module system, type system, elaboration, diagnostics produced by those stages, and erasure. New AST-heavy features target Curios only.

The frozen Rust frontend remains in tests and S0. It does not receive post-cutover language extensions.
