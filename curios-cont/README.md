# curios-cont

The Curios continuation-passing IR and its optimizer: `curios_ersd::lower_to_cont` constructs the CPS graph, and the optimizer rewrites it before `curios-emit` lowers it to WebAssembly. How the module prints is [A printer states each fact once, where it is bound](../documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md); the representation invariants belong to the crate rustdoc.

## Design

### Mutation hides behind instruction atomicity

**Decision.** Each write-once cell and bounded channel operation ([Only a fiber waits](../documentation/design/effects/only-a-fiber-waits.md)) is a CPS node that completes before invoking its continuation: the emitter performs its state transition and chooses its outcome without calling the host or suspending, optimizers preserve these nodes and their order, and a readiness query observes state without reserving a later operation's outcome. Compiler-generated knots use the same operations: a result cell and a capacity-one initializer channel per computed member, every storage object and closure bound and every initializer enqueued before forcing starts. A force polls the result, otherwise takes and runs the initializer, then fills the result; an empty result beside an empty initializer channel is recursive re-entry and emits `Panic::Cycle`. The force captures only the two storage objects, so taking the initializer releases the knot's reference to its captures. The lowering and its structural tests live in `curios-ersd/src/into_cont/`.

**Rationale.** A push or take reports the outcome of its own attempt, and a transition inside one operation keeps a cooperative fiber from observing a partial update, within one guest instance. Initializers are pure and cannot suspend between the knot's operations, so the pair needs no atomic transaction.

**Rejected.** Holding the initializer and the eventual result in one channel, which needs tagged wrappers to tell them apart and an operation reading the result without consuming it.

### Representation is decided for locals only

**Decision.** `cps/represent.rs` decides whether a value is held in a machine register or behind a reference for locals alone. Nothing it decides crosses a function boundary: a function parameter, a value free in some function's body, a call, host or cell result, and a recursive shell keep the reference the emitter hands over. A `Nat` or `Int` is a reference whatever its size, so a continuation parameter is offered a machine word only when every argument reaching it is a small literal, a result its literal operands bound, or another such parameter ([Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).

**Rationale.** A representation crossing a boundary is two parties agreeing on a layout, which puts it into a signature and makes it a type rather than a decision one pass takes; confined to locals, the analysis stays a client of the shared solver. The free-value withdrawal reads the set lambda lifting reads, since deciding a lifted value from the scope binding it sends a reference into integer arithmetic; `cps::represent::tests::a_value_free_in_another_function_stays_boxed` holds it.
