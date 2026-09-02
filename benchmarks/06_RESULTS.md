# Results — the other column halved, and one commit did it

One run of the harness in [README.md](README.md), captured 2026-08-17. The same day and three commits after [run 05](05_RESULTS.md), exactly one of which touches the compiler. That run moved the integer column by a third and left the allocation column untouched, and said plainly that nothing in its interval had been aimed at allocation. This one is the answer to that sentence: `trees` fell by half on both paths, `lcg` did not move, and the single commit between the two captures is the one that removes half the workload's heap objects.

It is also the cleanest interval this harness has ever measured. Run 05 could finally hold the wasm engine fixed and called that out as a first. Here **nothing moved but the compiler**: every toolchain version is identical to run 05's — Lean included, which was the one exception last time — and the Dockerfile, `entrypoint.sh`, `lcg.crs` and `trees.crs` are byte-identical. There is one compiler change in the interval and no other variable to assign the result to.

## How this was run

- **Source** — Curios compiler commit `74987fc975c2`, version 0.9.3 (run 05 was `580a89647db8`, also 0.9.3 — no release landed in the interval).
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 47.0.3, Cranelift. **Unchanged from runs 04 and 05.** The `wasmtime` crate embedded in Curios's native executable is 46.0.1, also unchanged.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.2, Lean 4.33.0, Grain 0.7.2, AssemblyScript 0.28.20. **Every one of these is run 05's version**, which is the first time that sentence has no exception attached.
- **The benchmark programs did not change** — unlike runs 04 and 05, which each had to report a migration or a reformatting, `lcg.crs` and `trees.crs` are byte-identical to the sources run 05 measured.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends — and this run beats run 05 at being that check, the two rows landing 0.1 ms apart on `lcg` and 0.3 ms apart on `trees`.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 223.5 ± 0.3 ms | 1.00× |
| Lean 4 | 225.9 ± 1.9 ms | 1.01× |
| Node (V8) | 235.0 ± 1.5 ms | 1.05× |
| **Curios** | **293.1 ± 1.1 ms** | **1.31×** |
| OCaml (flambda) | 358.9 ± 2.2 ms | 1.61× |

Unchanged from run 05 in position and in figure. The 1.1% between the two captures is smaller than the sitting's own drift, which the section below prices.

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 41.4 ± 0.3 ms | 1.00× |
| Rust | 82.2 ± 0.4 ms | 1.99× |
| OCaml (flambda) | 95.1 ± 0.4 ms | 2.30× |
| **Curios** | **121.1 ± 1.5 ms** | **2.93×** |
| Node (V8) | 213.1 ± 46.6 ms | 5.15× |

**Curios moves from last to fourth**, passing Node by a margin that survives Node's spread — its slowest timed run was 272.0 ms and its fastest 177.8 ms, and Curios's whole range is 119.3 to 122.9 ms. The distance to Lean 4 went from 5.85× to 2.93×, and to OCaml from 2.48× to 1.27×.

Node's `trees` row is noisy for the fourth run out of six; hyperfine flagged its first timed run as a 272.0 ms outlier despite the warmup. Its mean is not a number to order finely against, but the gap to Curios is now wide enough that the ordering does not depend on it.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 259.7 ± 2.3 ms | 1.00× |
| **Curios** | **293.2 ± 1.4 ms** | **1.13×** |
| AssemblyScript | 319.3 ± 1.9 ms | 1.23× |
| Grain | 29,576 ± 82 ms | 113.90× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 116.7 ± 4.6 ms | 1.00× |
| **Curios** | **121.4 ± 0.6 ms** | **1.04×** |
| AssemblyScript | 228.6 ± 4.2 ms | 1.96× |
| Grain | 1,757 ± 5 ms | 15.05× |

**Curios is second in both wasm tables for the first time**, and on `trees` it is 1.04× off Rust → wasm — 4.7 ms, against Rust → wasm's own 4.6 ms standard deviation. hyperfine puts the ratio at 1.04 ± 0.04, so the band reaches 1.00 and the two are close to indistinguishable here. Rust → wasm is still ahead; that is the honest reading, and it is a different claim from the 2.06× run 05 recorded five days of compiler work ago.

It also moved Curios past AssemblyScript on the allocation workload, where AssemblyScript had led by 1.15× in run 05 and now trails by 1.88×.

## One column halved, the other did not move

| Curios row | Run 00 | Run 01 | Run 02 | Run 03 | Run 04 | Run 05 | Run 06 | 06 vs 00 | 06 vs 05 |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | 437.3 ms | 435.1 ms | 296.3 ms | 293.1 ms | −34.3% | −1.1% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | 438.4 ms | 435.7 ms | 296.7 ms | 293.2 ms | −34.1% | −1.2% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | 256.4 ms | 241.9 ms | 245.9 ms | 121.1 ms | −61.5% | −50.8% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | 260.0 ms | 244.5 ms | 249.6 ms | 121.4 ms | −62.3% | −51.4% |

**What the sitting itself is worth.** Every other contestant is a control here, since none of their toolchains changed, and they did drift: on `lcg`, Rust −0.4%, Lean +0.0%, OCaml −0.3%, Node −0.8%, Rust → wasm −1.6%, AssemblyScript +0.3%; on `trees`, Lean −1.4%, Rust −5.2%, OCaml −4.1%, Rust → wasm −3.7%, AssemblyScript +5.6%. So this sitting reads a few percent fast on `trees` and a fraction of a percent fast on `lcg`. Curios's `lcg` move of −1.1% sits inside that band and **is not a result**; its `trees` move of −50.8% is an order of magnitude outside it and is not explicable by the sitting, in the way a 6% move once was.

**The mechanism, and why it lands exactly here.** The one compiler commit in the interval is [A variant collapses when nothing needs to distinguish it](../documentation/design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md). A variant family with exactly one immediate-unary constructor now rides that constructor as its bare payload, its matches opening with an `IsImmediate` kind test — one `ref.test (ref i31)` — instead of a tag read, and the tag is not read at all when a single boxed constructor remains. `trees.crs` declares `induct Tree | leaf(Nat) | node(Nat, Tree, Tree) end`, which is that shape precisely: `leaf(Nat)` is the immediate-unary constructor and `node` is the lone boxed sibling. At D = 21 the leaves are 2,097,152 of the tree's 4,194,303 objects — half of them, plus one — and they stop being allocated.

**The prediction was structural, and this run is the timing it was deferred to.** The decision recorded that the encoding "removes the leaf allocations of tree-shaped data outright — half the objects of the binary-trees workload", pinned that structurally in `curios`'s `trees_leaf_rides_its_payload` — which asserts the `ref.test (ref i31)` in `sum`, the absence of any surviving tag read, and exactly one `struct.new` in `build` — and said in as many words that "the trees timing lives with the benchmark harness that reproduces it." This file is that figure. Half the objects removed, half the wall clock gone.

That the two halves agree to within a percentage point is a closer correspondence than the mechanism strictly claims, and should not be read as a law. Nothing removed the traversal: `sum` still visits all 4,194,303 payloads, and the remaining 2,097,151 nodes still allocate. What the agreement says is that allocation and its share of collection dominated this workload so thoroughly that removing half the objects removed half the time. A workload with a heavier traversal per object would not scale the same way.

**`lcg` did not move, and could not have.** Its hot loop is `match k | 0 | kp + 1`, matching on `Nat` — an intrinsic riding the i31 carrier, not a variant family — so there is no tag, no tuple and no dispatch for this encoding to reach. The only variant in the program is the `Option` returned by `/std/read()` and `Nat/of_str`, which runs once at startup and is disqualified anyway: `Option` is polymorphic, and the decision classifies a parameter's field `Opaque` because it opens to a stuck head. The asymmetry between the two columns is decided by the shapes in the source, not by anything this run discovered.

Two runs in a row have now moved exactly one column and predicted in advance which one. Run 05's scalar-locals work was locals-only and could not cross into `trees`; run 06's encoding acts on variant families and finds nothing to do in `lcg`.

## What actually happened this interval

Three commits, of which one is the compiler change above, one captured run 05, and one filed roadmap specifications.

That last one is worth naming, because it defines what this result did *not* do. `documentation/roadmap/` gained an `optimizations/` kind — a cost the compiler could remove from code that is already correct — and two campaigns were specified under it. [A variant travels as the fields of its widest constructor](../documentation/design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md) is the successor to this run's win and the boundary of it: the collapse removes *objects and dispatch reads*, never field representations, so a boxed constructor's payload fields are still uniformly boxed `(ref null any)` and a variant in compiler-coordinated flow still pays a heap object and a re-read. The immediate encoding in fact *widened* one such decline, since an immediate family's return edges are variant-width — a bare payload on one edge and a tuple on another — which the split-return protocol declines by construction. [A pure program rebuilds what an impure one would mutate](../curios-runtime/README.md) — since retired into its sizing decision — is the other, and it was deliberately mechanism-blocking until two numbers existed that this harness did not then produce: no workload here exercised death-birth overlap against the Perceus-compiled Lean column, and no census located it in real code.

So the remaining `trees` distance is not unexplained. It is `node`'s three boxed fields, and it has a specification with a number attached to it and a name.

## Where Curios stands now

**Curios is ~1.31× off native Rust on tight integer work and 1.13× off Rust → wasm, and allocation is now 2.93× off Lean 4 and 1.04× off Rust → wasm — second in both wasm tables, and no longer last in any table in this harness.**

The framing that has held since run 00 — an integer column and an allocation column, each stuck for its own reason — is spent. Both moved in the last two captures, each by a mechanism whose scope named the column it would move before the run was taken, and the current distance to Rust → wasm is 1.13× on integers and 1.04× on allocation. Read the second of those with its uncertainty: it is a near-tie inside Rust → wasm's own spread, not a lead.

Worth one cross-table note, since the README sanctions exactly this reading: Rust pays 1.42× to go from native to wasm on `trees` (82.2 → 116.7 ms). Curios's 121.1 ms is 1.47× off native Rust. Almost all of what separates Curios from a native-compiled Rust on this workload is now what WebAssembly itself costs Rust, leaving a Curios-specific factor of about 1.04×. That is a statement about one allocation workload on one machine, not about the language — but it is the first time it has been available to make.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table — that last one is doing real work in this run, since `trees` is where GC strategy shows and Curios's improvement is an allocation-count improvement.

Three notes specific to this capture. **The interval is one commit and every other variable is pinned**, which makes this the strongest attribution the harness has produced and also the narrowest — it says what one encoding change is worth on one workload shaped to receive it, and nothing about a program whose variants are not that shape. **The `trees` sitting reads a few percent fast**, by the control drift priced above; a few points of the 50.8% belong to the sitting and the rest does not. And **the 1.04× on wasm `trees` is a near-tie, not a lead** — Rust → wasm's ±4.6 ms is about the size of the gap, and it is still the faster of the two.
