# A float a reference seeds stays a float

Working specification for holding a float in a register across the boundaries a float crosses behind a reference today: a call's arguments and results and a function's captures, where the compiler sees every party, and a parameter, a call's result or a read the compiler cannot trace, where only the value's type says it holds a float. A loop seeded from any of them boxes its float on every round; this takes each boundary in turn, the ones the continuation IR sees whole by the fixpoint that already decides a continuation's parameters, and the rest by a carrier erasure states where the type names one.

## What this builds on

- **Representation is decided for locals only** ([`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-for-locals-only)). `represent.rs` withdraws every function parameter, every value free in another function's body, and every parameter of a continuation a call, a cell, a channel or a call-shaped intrinsic returns to; a continuation parameter is offered a word or a float only where every argument reaching it is one already (`word_params`, `float_params`).
- **A known function's boundary is crossed already, for aggregates.** A function that does not escape takes its aggregate parameters as fields, with no wrapper, since every call site is one the rewrite reaches (`optimize/fields.rs`), and a tail-call class every caller takes apart hands its result back as several values (`optimize/protocol.rs`), decided per undirected tail-call component because `return_call` needs a callee's results to match its caller's.
- **The emitter keys a function type by its counts.** `func/N` and `func/N/M` are uniformly `(ref null any)` (`curios-emit/src/table.rs`), a call loads every argument as a reference (`context.rs`'s `call_direct_instrs`), and a resume block asserts that a result arrives as a reference save a host's `Flt`. An escaping function is reached through a retained-ABI closure wrapper that unpacks its captures and tail-calls its direct code (`machine.rs`), every value free in a function is lifted onto it as a leading parameter of the same name, and a closure's environment is a final struct per closure.
- **Erasure records a field's carrier** ([A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md)), and nothing of a value's: an Ersd `ValueDef` holds its debug name alone.
- **The shared solver takes keys that bind no value** (`dataflow.rs`: "such as function-return positions").

## The gap

Counted at `28bcb9aba` in the modules `curios wonder stage wasm` and `wasm-optm` print, each program's functions kept out of line by a body past `MULTI_SITE_INLINE_LIMIT` and calls fed by `Process/args`:

- **A loop seeded by a known function's float parameter** keeps one `struct.new $flt` a round, and each round reads its float back out of the box the round before allocated; Binaryen narrows the parameter to `(ref (exact $flt))` and removes the box from none.
- **A loop seeded by a call's result** keeps one a round.
- **A function taking two `{x: Flt, y: Flt}` structures and a float and answering one**, called once a round from a loop, takes the four fields the worker splits out as `anyref`, reads each through a cast and ends in two `struct.new $flt`: two allocations a round, though its body and the loop compute in registers.
- **A loop in a function that escapes as a closure**, seeded from its parameter, keeps one a round.
- **A float free in another function is boxed everywhere**, the loop that defines it included, read off `represent.rs`'s withdrawal of `free_values`.
- **A float literal read raw is read through its box.** A hoisted `Flt` constant is loaded at a raw demand by `global.get`, a cast and `struct.get` (`context.rs`'s `load_value_instrs`), and Binaryen leaves the load in the loop.

Binaryen's Heap2Local replaces an allocation only where nothing else is stored to the local it flows through, and a loop's seed and its back edge share one.

## Prior art

- **Leroy finds the calling protocol is what matters** ([The effectiveness of type-based unboxing, 1997](https://xavierleroy.org/publi/unboxing-tic97.pdf)): "a modest amount of type-directed unboxing (tag-based handling of unboxed float arrays and records of unboxed floats) combined with mostly-standard, untyped optimizations (local unboxing, special calling protocols for known functions) performs just as well as, and even slightly better than aggressive coercion-based type-directed unboxing". Curios has the records and the local unboxing; this adds the protocol.
- **wasm_of_ocaml took the same two steps on WebAssembly GC**: number unboxing within functions ([#2069](https://github.com/ocsigen/js_of_ocaml/pull/2069)), then across function calls ([#2101](https://github.com/ocsigen/js_of_ocaml/pull/2101)), passing a directly called function's integer parameters raw. dart2wasm unboxes method results its whole-program type flow concludes are `int` or `double` ([commit](https://dart.googlesource.com/sdk/+/785dd5723c9fc6b22adc1326845f64bd87fc88c0)).
- **Chez Scheme and Guile stop where Curios stops today.** Chez's 2020 unboxing of local floating-point arithmetic reaches a loop's arguments and leaves a closure's values and a call's arguments boxed; Guile's unboxes a loop variable whose every definition is unboxable and boxes every function argument and return value ([Wingo](https://wingolog.org/archives/2016/01/19/unboxing-in-guile)).
- **A type names the carrier past a boundary no flow reaches.** Lean 4's IR types every binder and parameter, and generates a `_boxed` twin for a declaration with a scalar parameter or result, the one a closure calls ([`Boxing.lean`](https://raw.githubusercontent.com/leanprover/lean4/v4.10.0/src/Lean/Compiler/IR/Boxing.lean)); OCaml 4.04 flags float-typed bindings from the typed tree, so a mutable float is unboxed though its first value arrives boxed ([LexiFi](https://www.lexifi.com/blog/ocaml/unboxed-floats-ocaml/)).

## Decisions

1. **A boundary the compiler sees whole decides its carriers.** A parameter of a function that does not escape and is not the entry is a join whose edges are its call sites. A result slot of a tail-call class is a join whose edges are its members' return edges and whose uses are the resume parameters of every call into the class; a class is decided only where no member escapes, is the entry, or ends in a tail call through a closure, a foreign, a cell, a channel or a call-shaped intrinsic, which fix its results. The lattice and `fed_only_by` that decide a continuation parameter decide both, the word beside the float. The emitter declares each position at its carrier, loads a call's arguments as an edge loads its own, returns at the result carriers, and coerces a result to a resume parameter held otherwise.
2. **A captured value crosses at its own carrier.** A value free in a function is lifted onto it, and stored in a closure's environment, at the carrier its definition holds it at: a lifted parameter and an environment field are declared at it, and the withdrawal goes. A closure's environment is its own struct, so nothing shared re-keys.
3. **The uniform positions are the ones no single party controls**: an escaping function's own parameters and results, which its wrapper reaches at the shared `clsr/N` type, and the entry's, which the host calls.
4. **A binder whose producer names no carrier states the register carrier its type names.** A function parameter, a call's result, a field or payload read, a sequence element, and a cell's or a channel's contents state `Immediate` as a word and `Flt` as a float where their type reduces to one, recorded by erasure from the type the binder is checked at, as it records a field's, and carried onto the continuation value. An operation's result states none, since its operation names its carrier, and the Ersd verifier refuses one there. A copy carries it, a binder a pass mints states none, and both printers spell it at the binder.
5. **A stated carrier feeds a register and decides nothing else.** `word_params` and `float_params` take a value stating their carrier as one that feeds them, unboxed once on the edge or the call; the value keeps its own offer.
6. **A float literal loaded raw is `f64.const`.** The hoisted global serves the boxed reads.

## Stages

1. **The literal.** Decision 6. Check: a loop reading float literals raw holds `f64.const` and no `global.get` of a constant.
2. **The tail-call classes, shared.** The return protocol's components and the tail positions that fix a class's results become one analysis the protocol and the representation read. No module changes.
3. **Known calls.** Decisions 1 and 3. Check: a loop seeded by a known function's float parameter, one seeded by a call's result, and the two-structure step loop hold no `struct.new $flt`.
4. **Captures.** Decision 2. Check: a float defined in a function and captured by an escaping closure seeds a loop in each, and neither loop holds a `struct.new $flt`; a captured counter in integer arithmetic answers as before.
5. **Stated carriers.** Decision 4, with no reader yet. Check: erasure states a `Flt` parameter, a `Bool` parameter, a call's result and an `Option(Flt)` payload, and no value of a computed type, of a type variable, or an operation's result.
6. **Stated carriers feed registers.** Decision 5. Check: a loop in an escaping function seeded by its parameter, and one seeded by a closure call's result, hold no `struct.new $flt`.

## Verification

- **Each loop's box, counted.** Each stage's fixtures find their loop by an operation only it holds, assert first that the fixture keeps the shape it measures, assert the loop holds no `struct.new $flt` — an assertion that fails at `28bcb9aba` — and answer alike on the Binaryen path and the raw one (`raw_wasm_validates_and_executes_without_binaryen`).
- **What arrives where the analysis must not raise it.** The three programs of `curios`'s `tests::codegen::structural::layout_tests` that join a float and a reference — a payload two constructors share, a float a parser bound, a value of a computed type — answer as they do, and `curios-cont`'s `represent` tests hold each withdrawal that stays: an escaping function's parameter, the entry's, an unstated reference, a class a closure tail call pins.
- `cargo xtask workspace lint` after each commit, and `cargo xtask workspace test` after each stage.
- **Figures this may move**, named and not retaken: the NaN check's cost in `curios-emit`'s README, `curios`'s wasm-level measurement tests, the xbench readings where a `Bool` or a tag now crosses a known call as a word, the prelude image and every stored unit, and erasure's spending over `/std`.

## Design decisions this overturns or corrects

- [`curios-cont`'s README](../../../curios-cont/README.md#representation-is-decided-for-locals-only), *Representation is decided for locals only*: decided wherever every party is visible. Stages 3, 4 and 6.
- [A field is declared at the carrier its shape names](../../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md): its rejection of typed function signatures narrows to escaping functions (stage 3), and a binder's carrier is recorded as a field's is (stage 5).
- [`curios-emit`'s README](../../../curios-emit/README.md#absence-is-null), *Absence is null*: a position held in a register takes its zero, as a register slot does. Stage 3.
- [A printer states each fact once, where it is bound](../../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md): Ersd keeps a binder's carrier where its type names one. Stage 5.
