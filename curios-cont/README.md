# curios-cont

The Curios continuation-passing IR and its WebAssembly backend: `curios_ersd::into_cont` constructs the CPS graph, the optimizer rewrites it, and `into_wasm` performs delayed closure conversion and structurizes control into Wasm blocks and loops. The representation invariants and the backend pipeline belong to the crate rustdoc.

## Design

### Mutation hides behind instruction atomicity

**Decision.** The IR has no stateful operation sequence: observable mutation happens only inside a single emitted instruction. The sole stateful value is `Cell`, and each of its operations is one such instruction.

**Rationale.** CPS optimization reorders, duplicates, and deletes operations on dataflow grounds alone; an operation with internal state would turn each of those rewrites into a case analysis. Confining mutation to one instruction keeps effects atomic in the alphabet, so the optimizer's rewrites stay sound by construction rather than by side condition.
