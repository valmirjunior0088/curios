# A float a reference seeds stays a float

Working specification for holding a float in a register past a boundary the compiler cannot trace: an escaping function's parameter, a closure call's result, or a read of a slot or a sequence, where only the value's type says it holds a float. A loop seeded from one of them boxes its float on every round; erasure states the carrier a binder's type names, and this has the representation read it where a reference meets a register.

## What this builds on

- **Representation is decided wherever every party is visible** ([`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible)). `represent.rs` decides a local, a captured value, a known function's parameters and a tail-call class's results by one fixpoint, and the emitter declares each at the carrier it settles; it withdraws an escaping function's parameters, the entry's, and every parameter of a continuation a closure call, a cell, a channel or a call-shaped intrinsic returns to. A position is offered a word or a float only where every argument reaching it is one already (`word_params`, `float_params`).
- **Erasure states a binder's register** where its producer names no carrier ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)): a function parameter, a call's result, a field or payload read and a sequence element whose type always rides a word or a float say so, the door carries it onto the continuation value, and nothing reads it yet.

## The gap

Counted at `28bcb9aba` in the module `curios wonder stage wasm` prints, the function kept alive by being stored in a list and the program fed by `Process/args`: **a loop in a function that escapes as a closure**, seeded from its parameter, keeps one `struct.new $flt` a round, and each round reads its float back out of the box the round before allocated. The parameter arrives through the closure's uniform `clsr/N` signature, and nothing says which reference is a float.

## Prior art

- **A type names the carrier past a boundary no flow reaches.** Lean 4's IR types every binder and parameter, and generates a `_boxed` twin for a declaration with a scalar parameter or result, the one a closure calls ([`Boxing.lean`](https://raw.githubusercontent.com/leanprover/lean4/v4.10.0/src/Lean/Compiler/IR/Boxing.lean)); OCaml 4.04 flags float-typed bindings from the typed tree, so a mutable float is unboxed though its first value arrives boxed ([LexiFi](https://www.lexifi.com/blog/ocaml/unboxed-floats-ocaml/)).

## Decisions

1. **A stated carrier feeds a register and decides nothing else.** `word_params` and `float_params` take a value stating their carrier as one that feeds them, unboxed once on the edge or the call; the value keeps its own offer.

## Stages

1. **Stated carriers feed registers.** Decision 1, in one commit. Check: a loop in an escaping function seeded by its parameter, and one seeded by a closure call's result, hold no `struct.new $flt`.

## Verification

- **Each loop's box, counted.** The stage's fixtures find their loop by an operation only it holds, assert first that the fixture keeps the shape it measures, assert the loop holds no `struct.new $flt` — an assertion that fails at `28bcb9aba` — and answer alike on the Binaryen path and the raw one (`raw_wasm_validates_and_executes_without_binaryen`).
- **What arrives where the analysis must not raise it.** The three programs of `curios`'s `tests::codegen::structural::layout_tests` that join a float and a reference — a payload two constructors share, a float a parser bound, a value of a computed type — answer as they do, and `curios-cont`'s `represent` tests hold each withdrawal that stays: an escaping function's parameter, the entry's, an unstated reference, a class a closure tail call fixes.
- `cargo xtask workspace lint` and `cargo xtask workspace test` on the commit.
- **Figures this may move**, named and not retaken: `curios`'s wasm-level measurement tests and the xbench readings, wherever a stated reference now feeds a register.

## Design decisions this overturns or corrects

- [`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible): a reference stating its carrier feeds a register.
