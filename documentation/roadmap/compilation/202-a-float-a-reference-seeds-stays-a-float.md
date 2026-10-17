# A float a reference seeds stays a float

Working specification for holding a float in a register across the boundaries it still crosses behind a reference: a function's captures, which the compiler sees whole, and a parameter, a call's result or a read the compiler cannot trace, where only the value's type says it holds a float. A loop seeded from any of them boxes its float on every round; this takes the captures by the fixpoint that already decides a continuation's parameters and a known call's, and the rest by a carrier erasure states where the type names one.

## What this builds on

- **Representation is decided wherever every party is visible** ([`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible)). `represent.rs` decides a local, a known function's parameters and a tail-call class's results by one fixpoint, and the emitter keys a function's type by the carriers it settles; it withdraws an escaping function's parameters, the entry's, every value free in another function's body, and every parameter of a continuation a closure call, a cell, a channel or a call-shaped intrinsic returns to. A position is offered a word or a float only where every argument reaching it is one already (`word_params`, `float_params`).
- **A captured value is lifted under its own name.** Every value free in a function is a leading parameter of its direct code, passed by every direct call, and an escaping function is reached through a retained-ABI closure wrapper that unpacks its captures from an environment and tail-calls that code (`curios-emit`'s `machine.rs`); a closure's environment is a final struct per closure.
- **Erasure records a field's carrier** ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)), and nothing of a value's: an Ersd `ValueDef` holds its debug name alone.

## The gap

Counted at `28bcb9aba` in the modules `curios wonder stage wasm` and `wasm-optm` print, each program's functions kept out of line by a body past `MULTI_SITE_INLINE_LIMIT` and calls fed by `Process/args`:

- **A loop in a function that escapes as a closure**, seeded from its parameter, keeps one `struct.new $flt` a round, and each round reads its float back out of the box the round before allocated.
- **A float free in another function is boxed everywhere**, the loop that defines it included, read off `represent.rs`'s withdrawal of `free_values`.

Binaryen's Heap2Local replaces an allocation only where nothing else is stored to the local it flows through, and a loop's seed and its back edge share one.

## Prior art

- **A type names the carrier past a boundary no flow reaches.** Lean 4's IR types every binder and parameter, and generates a `_boxed` twin for a declaration with a scalar parameter or result, the one a closure calls ([`Boxing.lean`](https://raw.githubusercontent.com/leanprover/lean4/v4.10.0/src/Lean/Compiler/IR/Boxing.lean)); OCaml 4.04 flags float-typed bindings from the typed tree, so a mutable float is unboxed though its first value arrives boxed ([LexiFi](https://www.lexifi.com/blog/ocaml/unboxed-floats-ocaml/)).
- **Chez Scheme and Guile leave a closure's values boxed.** Chez's 2020 unboxing of local floating-point arithmetic reaches a loop's arguments and leaves a closure's values and a call's arguments boxed; Guile's boxes every function argument and return value ([Wingo](https://wingolog.org/archives/2016/01/19/unboxing-in-guile)).

## Decisions

1. **A captured value crosses at its own carrier.** A value free in a function is lifted onto it, and stored in a closure's environment, at the carrier its definition holds it at: a lifted parameter and an environment field are declared at it, and the withdrawal goes. A closure's environment is its own struct, so nothing shared re-keys.
2. **A binder whose producer names no carrier states the register carrier its type names.** A function parameter, a call's result, a field or payload read, a sequence element, and a cell's or a channel's contents state `Immediate` as a word and `Flt` as a float where their type reduces to one, recorded by erasure from the type the binder is checked at, as it records a field's, and carried onto the continuation value. An operation's result, a construction and a join state none, since their producers name their carriers or their flows decide them, and the Ersd verifier refuses one there. A copy carries it, a binder a pass mints states none, and both printers spell it at the binder.
3. **A stated carrier feeds a register and decides nothing else.** `word_params` and `float_params` take a value stating their carrier as one that feeds them, unboxed once on the edge or the call; the value keeps its own offer.

## Stages

1. **Captures.** Decision 1. Check: a float defined in a function and captured by an escaping closure seeds a loop in each, and neither loop holds a `struct.new $flt`; a captured counter in integer arithmetic answers as before.
2. **Stated carriers.** Decision 2, with no reader yet. Check: erasure states a `Flt` parameter, a `Bool` parameter, a call's result and an `Option(Flt)` payload, and no value of a computed type, of a type variable, or an operation's result.
3. **Stated carriers feed registers.** Decision 3. Check: a loop in an escaping function seeded by its parameter, and one seeded by a closure call's result, hold no `struct.new $flt`.

## Verification

- **Each loop's box, counted.** Each stage's fixtures find their loop by an operation only it holds, assert first that the fixture keeps the shape it measures, assert the loop holds no `struct.new $flt` — an assertion that fails at `28bcb9aba` — and answer alike on the Binaryen path and the raw one (`raw_wasm_validates_and_executes_without_binaryen`).
- **What arrives where the analysis must not raise it.** The three programs of `curios`'s `tests::codegen::structural::layout_tests` that join a float and a reference — a payload two constructors share, a float a parser bound, a value of a computed type — answer as they do, and `curios-cont`'s `represent` tests hold each withdrawal that stays: an escaping function's parameter, the entry's, an unstated reference, a class a closure tail call fixes.
- `cargo xtask workspace lint` after each commit, and `cargo xtask workspace test` after each stage.
- **Figures this may move**, named and not retaken: `curios`'s wasm-level measurement tests, the prelude image and every stored unit, and erasure's spending over `/std`.

## Design decisions this overturns or corrects

- [`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-wherever-every-party-is-visible): the free-value withdrawal goes (stage 1), and a reference stating its carrier feeds a register (stage 3).
- [A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md): a binder's carrier is recorded as a field's is. Stage 2.
- [A printer states each fact once, where it is bound](../../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md): Ersd keeps a binder's carrier where its type names one. Stage 2.
