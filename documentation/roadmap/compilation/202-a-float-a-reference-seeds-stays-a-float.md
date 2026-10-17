# A float a reference seeds stays a float

**Not refined yet.** This specification reserves a float carried as a float where a reference seeds it, which [Representation is decided for locals only](../../../curios-cont/README.md#representation-is-decided-for-locals-only) leaves boxed today. It is not an implementation plan.

## What is missing

**A float a reference seeds is carried behind one.** `float_params` (`curios-cont/src/represent.rs`) offers a continuation parameter a float only where every argument reaching it is a float literal, a filler, a definition producing a float, or another such parameter: a reference states no shape, and a parameter may hold a float on one edge and something else on the next ([Representation is decided for locals only](../../../curios-cont/README.md#representation-is-decided-for-locals-only)). So a loop contified into a function and seeded from that function's own parameter, or from a call's result, boxes its float on every round. Counted, at `3cd8ef597`, in the module `curios wonder stage wasm` prints for a function that seeds a loop of one multiplication and one addition from its float parameter and is called from two places, so that it is not inlined: one `struct.new $flt` stands inside the loop, and each round reads its float back out of the one the round before allocated. A function that recurs on itself by a tail call is not this case: it crosses its own signature every round and boxes there either way.

## Refinement

- **A float behind a reference.** Erasure records a shape for each function parameter and result, as it records `FieldShape` for a constructor's fields, the door carries it into Cont, and a reference of float shape feeds a register as a float-producing definition does. Check: the loop allocating nothing, with the three programs of `curios`'s `tests::codegen::structural::layout_tests` that join a float and a reference answering as they do.
