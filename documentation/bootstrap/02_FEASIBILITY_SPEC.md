# Bootstrap Phase 1 — hybrid shell and feasibility probes

Working implementation specification for the first bootstrap implementation phase. It begins after the Phase 0 baseline is recorded and consumes the contracts in [01_CONTRACTS_SPEC.md](01_CONTRACTS_SPEC.md), in particular the whole-compiler fallback service and the compiler substrate requirements.

Build a minimal Curios compiler program with a `bootstrap_compile` foreign declaration. S0 compiles it, the Rust host binds the service, and the program delegates one successful and one failing compilation to Rust.

In parallel with the shell, implement narrow prototypes for the highest-risk substrate:

- A shared-term representation with minted identity, cached structural hash, and collision-safe structural comparison.
- Deterministic compiler maps, sets, queues, and byte builders.
- A byte-cursor lexer/parser that handles representative standard-library source.
- A representative reduction or conversion cache workload.
- Iterative traversal over a deliberately deep term or IR fixture.
- Raw Wasm byte construction for a small module.

This phase is a feasibility gate, not the bootstrap itself. It records time, allocation, peak memory, and artifact-size measurements for the representative workloads. No fixed speed ratio is required, but an asymptotic failure, uncontrolled allocation growth, stack dependence, or inability to retain deterministic output blocks the full port until the representation is revised.
