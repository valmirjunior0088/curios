# curios-cont

The Curios continuation-passing IR and its optimizer: `curios_ersd::lower_to_cont` constructs the CPS graph, and the optimizer rewrites it before `curios-emit` lowers it to WebAssembly. The representation invariants belong to the crate rustdoc.

## Design

### Mutation hides behind instruction atomicity

**Decision.** The IR has no stateful operation sequence: observable mutation happens only inside a single emitted instruction. The sole stateful value is `Cell`, and each of its operations is one such instruction.

**Rationale.** CPS optimization reorders, duplicates, and deletes operations on dataflow grounds alone; an operation with internal state would turn each of those rewrites into a case analysis. Confining mutation to one instruction keeps effects atomic in the alphabet, so the optimizer's rewrites stay sound by construction rather than by side condition.

### Representation is decided for locals only

**Decision.** `cps/represent.rs` decides whether a value is held in a machine register or behind a reference for locals alone. Nothing it decides crosses a function boundary: a function parameter, a value free in some function's body, a call, host or cell result, and a recursive shell each keep the reference the emitter hands over, whatever their uses demand. Within locals, a machine word holds only what a word was handed: a `Nat` or `Int` is a reference whatever its size, so a continuation parameter is offered the word only when every argument reaching it is a small literal, a result its literal operands bound, or another such parameter — [Nat and Int are an i31 until they outgrow it](../documentation/design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md) states why.

**Rationale.** Crossing a boundary means two parties agreeing on a representation, which puts layout into a *signature* and makes it a type rather than a decision one pass can take alone. Confining the analysis to locals is what keeps it a client of the shared solver instead of a simultaneous redesign of the closure type families, the struct field shapes, and the host ABI. The restriction is enforced rather than merely intended: the free-value withdrawal reads the same set lambda-lifting reads, because deciding a lifted value from the scope that binds it sent a reference into integer arithmetic and miscompiled — `cps::represent::tests::a_value_free_in_another_function_stays_boxed` is what holds that shut.

### Printing is cross-cutting

`cps/print.rs` renders the module as nested CPS — every function and continuation at its binding site, the return sentinel spelled as the word rather than named — see [The continuation IR prints as nested CPS](../documentation/design/toolchain/the-continuation-ir-prints-as-nested-cps.md). The decision is filed there rather than here because it settles what this rung shares with the three above it and what it deliberately stops sharing, and because the role each continuation is named after is stamped by `curios-ersd`'s lowering rather than by this crate.
