# A construction beside a dying value of its shape is rebuilt in place

This specification describes a removable cost: a construction beside a value of its own shape that dies with it allocates, where a proof that the value is used once would let the construction overwrite it. Its next step is stage 1, a bounded investigation.

## What it costs

`programs/spines` threads its map once (`walk(k, i + 1, y, Map/insert(m, y, …))`), and each insert rebuilds `/std/Map/internal/Node`'s path to the key around the node it was handed, which dies with the call: `Node/insert`'s fork arms build `fork(tc, z2, o)` or `fork(tc, z, o2)` from their `t`, and a leaf whose key is found builds `leaf(k0, v)` from its own. That is a node a level, about seventeen at the map's plateau of 65,536 keys, and every collection of the native heap recopies the live map.

Timed at `3062f2fe1`, release, x86-64 Linux, seven interleaved rounds of the time at N less the time at N = 1, median and span, with each variant printing the same answer at every N:

| `spines` | as built | `Node`'s slots mutable | rebuilt in place | heap of 128 MiB |
| --- | --- | --- | --- | --- |
| native, N = 2,000,000 | 1.048 s [1.017–1.050] | 1.032 s [1.025–1.054] | 0.716 s [0.706–0.721] | 0.958 s [0.949–0.966] |
| native, N = 500,000 | 0.267 s [0.265–0.280] | 0.272 s [0.266–0.279] | 0.186 s [0.179–0.189] | 0.300 s [0.293–0.313] |
| V8, N = 2,000,000 | 0.887 s [0.876–0.892] | 0.884 s [0.877–0.895] | 0.680 s [0.672–0.685] | — |
| V8, N = 500,000 | 0.253 s [0.251–0.260] | 0.252 s [0.249–0.253] | 0.177 s [0.176–0.179] | — |

So rebuilding in place is about a third of `spines` natively and a quarter to a third under V8, whose generational collector charges the write barrier this pays; declaring the row's slots mutable costs nothing measurable on either; and the heap the native engine starts at is not what the rest is.

**How the figures are taken.** A scratch copy of the commit whose `curios/src/compile.rs` `optimized()` writes the optimized module to `$CURIOS_WASM_DUMP` and reads it from `$CURIOS_WASM_OVERRIDE` where each is set, and whose `curios-native` engine takes its initial heap in mebibytes from `$CURIOS_HEAP_MIB`. `spines`' module is dumped, printed and parsed back by `wasm-tools`; `Node`'s row is the struct of an `i8` tag and three reference slots, its three slots made `mut` for the second and third variants; for the third, the six `struct.new` of that row in `Node/insert`'s copy that rebuild a leaf with its new value or a fork around its new child become a `struct.set` of that slot on the node the function was handed, which stays in one local for the whole function. Native runs are `curios run` of `programs/spines/spines.crs` under the override; V8 runs are Node 26.4 importing the page `compile --target browser` writes, whose `index.wasm` is the same module, and calling its loader's `start` with each module's bytes, each run a process of its own.

## What it reverses

[A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md) states that the compiler carries no reuse machinery, leaving a construction beside a dying value of its own shape to the host collector, and rejects reference counts, a sticky sharing bit, regions and arenas, each a test of uniqueness at run time, and construction coalescing and tail-modulo-cons, which need the mutable field the representation lacks. A proof of single use made at compile time is among none of them, and the figures above are what the collector leaves. Stage 2 restates the decision; every rejection it holds stands.

## What is known

- **The shape.** `Node/insert` reads `t`'s fields, calls itself on one child and builds the fork after the call returns, `t` read nowhere past its fields: the node dies before its rebuild, and its memory crosses one call. `Map` threads an `Option` root through every operation, and `Map/map` rebuilds a `Node(K, V)` as a `Node(K, W)`, one row once erased, so a map used once could be mapped in place as well.
- **Where it pays in `/std`.** The library's recursive families with content at run time are `Map`'s `Node`, `List/sort_by`'s `Layered` and `Test`; a list and a byte run are ropes, which a builder already extends once each. The pattern is otherwise user code's: a search tree, a rose tree, a zipper, a heap, a syntax tree a pass rewrites.
- **The census misses it.** `death_birth_census` (`curios/src/tests/codegen/census.rs`) counts structural tuples beside a dying value of matching width, each paired across a frame with a value arriving as a parameter; it counts no nominal row, so `Node`'s rebuilds, the ones measured, are outside its population.
- **What single use asks.** That the root handed to `Map/insert` is not read again is not enough: path copying shares every untouched subtree of a map with the map it was made from, so a map whose predecessor is still read has nodes two maps hold. A node may be rebuilt in place only where everything that holds it dies with the rebuild. Perceus's rule makes the descent work: a unique value taken apart and dead leaves its fields unique, its slots being dead memory.
- **What looks fresh and is shared.** The emitter hoists every statically constant binding into one global, interned structurally, so equal constants share it (`curios-emit`'s `hoist.rs`): a `Node/leaf` of two literals is a global, and a rebuild of it would write every later use. A value `curios-anf`'s evaluator computes where its unit is compiled is such a constant. So is a value captured by a closure, an `Io` description among them, which may run twice; one stored in a cell or a channel; one handed to the host; and one an escaping function or the entry receives (`Module::escaping_functions`).
- **What `curios-cps` holds already.** Its shared solver runs interprocedural facts over values and function returns, forward and backward (`dataflow.rs`, beside `origin.rs`, `demand.rs` and `row_flow.rs`); its specializer copies a component for a call context within an allowance (`CopyBudget`), which is what a second version of a function is. It has no node that writes into a row, cells and channels being its only mutable state, and no liveness at a program point.
- **Peers.** Lean 4 inserts `reset`/`reuse` where a case rebuilds a constructor of the width it took apart and tests the cell's count at run time ([Ullrich and de Moura, IFL 2019](https://arxiv.org/abs/1908.05647)); Koka's Perceus does the same with precise counts and reuses within a frame's bound ([Reinking et al., PLDI 2021](https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse-2/); [Lorenzen and Leijen, ICFP 2022](https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse/)), and checks a `fip` function in place statically ([Lorenzen, Leijen and Swierstra, ICFP 2023](https://www.microsoft.com/en-us/research/publication/fp2-fully-in-place-functional-programming/)). Without counts, the proof is static, as Mercury's compile-time garbage collection derives it from a structure-sharing analysis and a liveness analysis and calls a reuse version of a procedure where the caller shows the argument dead and unshared ([Mazur et al., 2001](https://arxiv.org/abs/cs/0110037); [Mazur, 2004](https://mercurylang.org/documentation/papers/CW2004_03_mazur.pdf)), and as Shankar's analysis compiles PVS's aggregate updates destructively ([Shankar, LOPSTR 2001](https://researchr.org/publication/Shankar01/bibtex)); Clean and Futhark have the user write uniqueness in types ([Futhark](https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html)).
- **A caution.** Roc's whole-program uniqueness analysis, Morphic, was buggy, slowed its compiler and measured within a few percent of none, against reference counts whose test of one is cheap ([Roc's discussion](https://roc-lang.github.io/zulip-export/stream/395097-compiler-development/topic/remove.20morphic.3F.html)). Here the comparison is a copy against a rebuild, which the figures above state, and the compile time it costs is still to be held to a budget.
- **What the engines are told.** Binaryen reads an immutable field as a constant where it sees every `struct.new` ([its lowering tips](https://github.com/WebAssembly/binaryen/wiki/GC-Implementation---Lowering-Tips)), and V8's generational collector charges a barrier on a store; both are inside the figures above for `Node`, and neither is measured for another row.

## Settled

- **Proven, never tested.** A value is rebuilt in place only where the compiler has proved it unshared and dead; no count and no flag rides a value, as the decision's rejections require.
- **In `curios-cps`,** after its copying passes settle and before the representation is decided, as clients of the shared solver and one rewrite: what every construction of a row, every function and every call is visible to.
- **Three facts.** Whether a function consumes a parameter or only reads it, backward; whether a value is unique, forward over known calls and returns, and shared at every boundary the module does not see whole — each listed under what looks fresh, the constants read off the emitter's own hoisting predicate, stated once; and where a value is last used within its function.
- **A field of a unique value taken apart and dead is unique**, which is what makes a path copy qualify.
- **The rewrite.** A construction of a row beside a unique value of the same row, dead there, becomes a rebuild of that value: a value form of its own, which the emitter lowers as a `struct.set` of each slot that changes, and the verifier holds to the same row and to no later use of the value rebuilt. Uniqueness is a whole-program fact no local check can see, so tests hold it.
- **Two versions where callers differ.** A function some calls hand a unique dying argument and others a shared one is copied for the first, as a reuse version, through the specializer's planned copies and charged to an allowance.
- **Mutable only where rebuilt.** A row some rebuild writes is declared with mutable slots, its tag among them where a rebuild changes the constructor; every other row stays immutable.
- **No surface or library change.** No keyword and no uniqueness type: `/std/Map` and its proofs stay as they are, and the rebuild is below erasure.

## Open

- **Precision against compile time.** How precise the uniqueness fact must be to admit what the count finds, and what it costs the harness's compile, held to a budget the count sets.
- **Reading before consuming.** Whether real programs read a value and then hand it on, `Map/len(m)` before `Map/insert(m, …)`, often enough that ownership must tell a read from a consumption across calls.
- **A dead cell across a call.** The memory a rebuild waits on stays held through the calls before it, bounded by the depth of the descent; whether any program holds it longer.
- **What a mutable row costs elsewhere.** Under Binaryen and both engines, measured per row the rewrite reaches.
- **Order.** Against scalar replacement, which leaves nothing to rebuild where it splits a value, and against hoisting.

## Stages

1. **Count what a proof admits.** In a scratch copy, a prototype of the three facts over the optimized CPS, listing every construction of a row or a tuple beside a unique dying value of its shape — with versions and without — over `programs/`, `/std`'s tests, `../curios-papers`' harness and the TOML driver, and over a set of persistent structures written for it: a search tree's insert, a rose tree's map, a zipper's move, a heap's merge. `death_birth_census` is restated to count rows. It ends with the count and with `spines`' sites among it; a count beyond `spines` lifts stage 2, and one that is `spines` alone drops this specification, recording the proof as rejected in the decision for the reason the count gives.
2. **The decision restated:** the rule and its peers, and as rejected a test of uniqueness at run time, a uniqueness the user writes, and a whole-program analysis held to no budget.
3. **The facts,** as clients of the shared solver, each with its tests, beside a suite of what looks fresh and is shared: a map read after its insert, a map in a list, a map a closure, a cell or a description holds, a constant map, a map handed to the host, two maps sharing a subtree.
4. **The rebuild,** its verification and its emission, with every program of the corpus and every `/std` test run with rebuilding on and off and holding to one output.
5. **Reuse versions,** through the specializer's copies and their allowance.
6. **Measurement:** `spines` natively and under V8 by the protocol above, the corpus's sizes, the harness's compile and a benchmark reading.

## Retirement

The decision states the rule and its peers, `curios-cps`' README the analysis, and a codegen test holds that `/std/Map/insert` rebuilds in place in a loop that uses its map once. Then this specification is deleted.
