# curios-cont

The Curios continuation-passing IR and its WebAssembly backend: `curios_ersd::into_cont` constructs the CPS graph, the optimizer rewrites it, and `into_wasm` performs delayed closure conversion and structurizes control into Wasm blocks and loops. The representation invariants and the backend pipeline belong to the crate rustdoc.

## Design

### Mutation hides behind instruction atomicity

**Decision.** The IR has no stateful operation sequence: observable mutation happens only inside a single emitted instruction. The sole stateful value is `Cell`, and each of its operations is one such instruction.

**Rationale.** CPS optimization reorders, duplicates, and deletes operations on dataflow grounds alone; an operation with internal state would turn each of those rewrites into a case analysis. Confining mutation to one instruction keeps effects atomic in the alphabet, so the optimizer's rewrites stay sound by construction rather than by side condition.

### Representation is decided for locals only

**Decision.** `cps/represent.rs` decides whether a value is held in a machine register or behind a reference for locals alone. Nothing it decides crosses a function boundary: a function parameter, a value free in some function's body, a call, host or cell result, and a recursive shell each keep the reference the emitter hands over, whatever their uses demand.

**Rationale.** Crossing a boundary means two parties agreeing on a representation, which puts layout into a *signature* and makes it a type rather than a decision one pass can take alone. Confining the analysis to locals is what keeps it a client of the shared solver instead of a simultaneous redesign of the closure type families, the struct field shapes, and the host ABI. The restriction is enforced rather than merely intended: the free-value withdrawal reads the same set lambda-lifting reads, because deciding a lifted value from the scope that binds it sent a reference into integer arithmetic and miscompiled — `cps::represent::tests::a_value_free_in_another_function_stays_boxed` is what holds that shut.
