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

[A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md) states that the compiler carries no reuse machinery, leaving a construction beside a dying value of its own shape to the host collector, and rejects reference counts, a sticky sharing bit, regions and arenas, each a test of uniqueness at run time, and construction coalescing and tail-modulo-cons, which need the mutable field the representation lacks. A proof of single use made at compile time is among none of them, and the figures above are what the collector leaves. The decision is restated where stage 2 settles a design.

## What is known

- **The shape is across a frame.** `death_birth_census` (`curios/src/tests/codegen/census.rs`) finds every structural tuple construction beside a dying value of matching width paired with a value arriving as a parameter, none constructed in the same function; it counts tuples and no nominal row, so `Node`'s rebuilds, the ones measured, are outside its population.
- **What single use asks.** That the root handed to `Map/insert` is not read again is not enough: the nodes on its path must be used once too, and path copying shares every untouched subtree of a map with the map it was made from, so a map whose predecessor is still read has nodes two maps hold. A node may be rebuilt in place only where everything that holds it dies with the rebuild.
- **Peers.** Mercury's compile-time garbage collection derives this by analysis — a structure-sharing analysis and a liveness analysis — and compiles a reuse version of a procedure beside the original, called where the caller shows the argument dead and unshared ([Mazur et al., 2001](https://arxiv.org/abs/cs/0110037); [Mazur, 2004](https://mercurylang.org/documentation/papers/CW2004_03_mazur.pdf)). Fully in-place functional programming checks the same discipline statically through a linear calculus and runs it through Perceus's counts where it is not proved ([Lorenzen, Leijen and Swierstra, ICFP 2023](https://www.microsoft.com/en-us/research/publication/fp2-fully-in-place-functional-programming/)).
- **Where it would run.** The CPS optimizer is where every construction of a row, every function and every call is visible, and where the representation is decided ([`curios-cps`'s README](../../../curios-cps/README.md#representation-is-decided-wherever-every-party-is-visible)); a write into a row is a node it does not have.

## Stages

1. **Count what a proof admits.** In a scratch copy, a prototype of the analysis over the optimized CPS — a value is unshared where it reaches its consumer by one flow and nothing that holds it is read after — listing the row and tuple constructions beside a dying unshared value of their shape, over `programs/` and `/std`'s tests, `../curios-papers`' harness among them. It ends with that count and with `spines`' sites among it; a count beyond `spines` lifts stage 2, and one that is `spines` alone drops this specification, recording the proof as rejected in the decision for the reason the count gives.
2. **A design**, restating the decision: the sharing analysis and where it runs, the reuse version of a function and the calls that take it, a write into a row as a CPS node and its verification, which rows are declared mutable, and what Binaryen and the engines are told.
3. **Delivery**, by the design's own stages, each checked by `curios-cps`' suite, the corpus, `curios-prelude`'s tests, and `spines` timed by the protocol above.

## Retirement

The decision states the rule and its peers, `curios-cps`' README the analysis, and a measurement test of the corpus holds the reuse it makes. Then this specification is deleted.
