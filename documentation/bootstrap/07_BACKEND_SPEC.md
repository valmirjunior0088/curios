# Bootstrap Phase 6 — Ersd and continuation backend

Working implementation specification for the sixth bootstrap implementation phase. It follows the [frontend cutover](06_FRONTEND_CUTOVER_SPEC.md) and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the backend-cutover done bar and the determinism requirements.

Port Ersd semantics, correctness-preserving lowering to continuation form, the continuation IR (the landed pre-closure CPS graph, its private machine CFG, and structured Wasm emission), and lowering to the Curios Wasm model. Establish an unoptimized correctness path before porting the optimizer suite.

Optimizers are restored in viability order rather than Rust source order. The first tier contains transformations required to keep compiler artifacts and self-compilation within practical memory, code-size, and execution bounds, including reachability pruning and the passes on which large recursive Curios programs rely. The second tier restores production performance and output quality. A pass may remain Rust-oracle-only temporarily if omitting it preserves semantics and does not prevent self-compilation.

As Curios takes ownership of each backend region, shadow tests compare normalized IR and end-to-end program behavior. The live path continues using `bootstrap_backend` until Curios can emit the complete raw Wasm module itself; no additional live Ersd-to-Cont or Cont-to-Wasm FFI seam is introduced.

When the Curios backend passes the raw-Wasm done bar, remove `bootstrap_backend` from the compiler source and verify that its Wasm import table contains neither temporary bootstrap service.
