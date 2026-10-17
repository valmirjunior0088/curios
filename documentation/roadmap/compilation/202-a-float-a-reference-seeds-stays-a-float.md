# A float a reference seeds stays a float

Working specification for holding a float in a register past a boundary the compiler cannot trace: an escaping function's parameter, a closure call's result, or a read of a slot, a sequence or a cell, where only the value's type says it holds a float. A loop seeded from one of them boxes its float on every round; this has erasure state the carrier a binder's type names, and the representation read it where a reference meets a register.

## What this builds on

- **Representation is decided wherever every party is visible** ([`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible)). `represent.rs` decides a local, a captured value, a known function's parameters and a tail-call class's results by one fixpoint, and the emitter declares each at the carrier it settles; it withdraws an escaping function's parameters, the entry's, and every parameter of a continuation a closure call, a cell, a channel or a call-shaped intrinsic returns to. A position is offered a word or a float only where every argument reaching it is one already (`word_params`, `float_params`).
- **Erasure records a field's carrier** ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)), and nothing of a value's: an Ersd `ValueDef` holds its debug name alone.

## The gap

Counted at `28bcb9aba` in the module `curios wonder stage wasm` prints, the function kept alive by being stored in a list and the program fed by `Process/args`: **a loop in a function that escapes as a closure**, seeded from its parameter, keeps one `struct.new $flt` a round, and each round reads its float back out of the box the round before allocated. The parameter arrives through the closure's uniform `clsr/N` signature, and nothing says which reference is a float.

## Prior art

- **A type names the carrier past a boundary no flow reaches.** Lean 4's IR types every binder and parameter, and generates a `_boxed` twin for a declaration with a scalar parameter or result, the one a closure calls ([`Boxing.lean`](https://raw.githubusercontent.com/leanprover/lean4/v4.10.0/src/Lean/Compiler/IR/Boxing.lean)); OCaml 4.04 flags float-typed bindings from the typed tree, so a mutable float is unboxed though its first value arrives boxed ([LexiFi](https://www.lexifi.com/blog/ocaml/unboxed-floats-ocaml/)).

## Decisions

1. **A binder whose producer names no carrier states the register carrier its type names.** A function parameter, a call's result, a field or payload read, a sequence element, and a cell's or a channel's contents state `Immediate` as a word and `Flt` as a float where their type reduces to one, recorded by erasure from the type the binder is checked at, as it records a field's, and carried onto the continuation value. An operation's result, a construction and a join state none, since their producers name their carriers or their flows decide them, and the Ersd verifier refuses one there. A copy carries it, a binder a pass mints states none, and both printers spell it at the binder.
2. **A stated carrier feeds a register and decides nothing else.** `word_params` and `float_params` take a value stating their carrier as one that feeds them, unboxed once on the edge or the call; the value keeps its own offer.

## Stages

1. **Stated carriers.** Decision 1, with no reader yet. Check: erasure states a `Flt` parameter, a `Bool` parameter, a call's result and an `Option(Flt)` payload, and no value of a computed type, of a type variable, or an operation's result.
2. **Stated carriers feed registers.** Decision 2. Check: a loop in an escaping function seeded by its parameter, and one seeded by a closure call's result, hold no `struct.new $flt`.

## Verification

- **Each loop's box, counted.** Each stage's fixtures find their loop by an operation only it holds, assert first that the fixture keeps the shape it measures, assert the loop holds no `struct.new $flt` — an assertion that fails at `28bcb9aba` — and answer alike on the Binaryen path and the raw one (`raw_wasm_validates_and_executes_without_binaryen`).
- **What arrives where the analysis must not raise it.** The three programs of `curios`'s `tests::codegen::structural::layout_tests` that join a float and a reference — a payload two constructors share, a float a parser bound, a value of a computed type — answer as they do, and `curios-cont`'s `represent` tests hold each withdrawal that stays: an escaping function's parameter, the entry's, an unstated reference, a class a closure tail call fixes.
- `cargo xtask workspace lint` after each commit, and `cargo xtask workspace test` after each stage.
- **Figures this may move**, named and not retaken: the prelude image and every stored unit, and erasure's spending over `/std`.

## Design decisions this overturns or corrects

- [`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible): a reference stating its carrier feeds a register. Stage 2.
- [A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md): a binder's carrier is recorded as a field's is. Stage 1.
- [A printer states each fact once, where it is bound](../../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md): Ersd keeps a binder's carrier where its type names one. Stage 1.
