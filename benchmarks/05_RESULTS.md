# Results — the plateau broke, and only where it was aimed

One run of the harness in [README.md](README.md), captured 2026-08-17. Eight days and 303 commits since [run 04](04_RESULTS.md), across four releases (0.9.0 through 0.9.3). The integer number that had not moved in five runs came off by a third; the allocation number did not move at all. That asymmetry is the result, and it is the one the work predicted in writing before this run was taken.

This is also the first capture whose toolchains are, with one exception, byte-identical to its predecessor's. rustc, OCaml, Node, Grain, AssemblyScript, wasmtime and hyperfine are all the same versions run 04 used; only Lean moved, and Lean sits under no Curios row. Run 04 had to leave its `trees` improvement unattributed because the wasm engine bumped underneath it. That caveat is discharged here: **the engine is fixed across this interval**, so what moved is the compiler.

## How this was run

- **Source** — Curios compiler commit `580a89647db8`, version 0.9.3 (run 04 was 0.8.1).
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 47.0.3, Cranelift. **Unchanged from run 04.** The `wasmtime` crate embedded in Curios's native executable is 46.0.1, also unchanged across the interval.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.2, Lean 4.33.0, Grain 0.7.2, AssemblyScript 0.28.20. Every one of these is run 04's version except Lean, which went 4.32.2 → 4.33.0.
- **The benchmark programs changed, but only in spelling** — `lcg.crs` and `trees.crs` were swept by the canonical formatter that landed in this interval (`x: Nat` rather than `x : Nat`, comments unwrapped to one line per paragraph). No declaration, no operation, and no arithmetic was touched, and both still print the anchor values every other language prints.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends — and this run is the strongest that check has been, the two rows landing 0.4 ms apart on `lcg`.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 224.3 ± 0.3 ms | 1.00× |
| Lean 4 | 225.8 ± 1.3 ms | 1.01× |
| Node (V8) | 236.8 ± 1.6 ms | 1.06× |
| **Curios** | **296.3 ± 1.0 ms** | **1.32×** |
| OCaml (flambda) | 359.8 ± 0.5 ms | 1.60× |

**Curios is not last in this table for the first time.** It passed OCaml — which did not itself move, timing within 2 ms of run 04 — and closed from 1.95× off Rust to 1.32×.

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 42.0 ± 0.5 ms | 1.00× |
| Rust | 86.7 ± 1.0 ms | 2.06× |
| OCaml (flambda) | 99.2 ± 0.8 ms | 2.36× |
| Node (V8) | 244.2 ± 52.6 ms | 5.81× |
| **Curios** | **245.9 ± 1.1 ms** | **5.85×** |

Node's `trees` row went noisy again — a 52.6 ms standard deviation, against the 2.3 ms run 04 recorded and remarked on as unusually clean. Its mean and Curios's are 1.7 ms apart with that spread around them, so the two are not distinguishable here and the ordering between them should not be read as one.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 263.9 ± 3.1 ms | 1.00× |
| **Curios** | **296.7 ± 5.1 ms** | **1.12×** |
| AssemblyScript | 318.4 ± 3.8 ms | 1.21× |
| Grain | 29,575 ± 68 ms | 112.08× |

**Curios is second in a wasm table for the first time**, ahead of AssemblyScript, and 1.12× off Rust → wasm — near enough that what remains of the gap is a smaller thing than any previous run has had to explain.

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 121.2 ± 2.0 ms | 1.00× |
| AssemblyScript | 216.5 ± 5.2 ms | 1.79× |
| **Curios** | **249.6 ± 3.8 ms** | **2.06×** |
| Grain | 1,763 ± 5 ms | 14.55× |

## One column moved a third, the other did not move

| Curios row | Run 00 | Run 01 | Run 02 | Run 03 | Run 04 | Run 05 | 05 vs 00 | 05 vs 04 |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | 437.3 ms | 435.1 ms | 296.3 ms | −33.6% | −31.9% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | 438.4 ms | 435.7 ms | 296.7 ms | −33.3% | −31.9% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | 256.4 ms | 241.9 ms | 245.9 ms | −21.9% | +1.7% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | 260.0 ms | 244.5 ms | 249.6 ms | −22.5% | +2.1% |

`lcg` had sat inside a 20 ms band for five consecutive runs while the compiler underneath it was rebuilt repeatedly — run 03 was titled for that flatness and run 04 reported it again over an interval of 416 commits. It fell 31.9% here, identically on both paths.

**This one is attributable, which is rare enough to say plainly.** `curios-cont` gained a dataflow substrate this interval and, as its first payoff, holds scalars in machine registers across a function body. That work was measured in-repo against a control with the decision suppressed — the two arms differing in nothing else — and recorded **−31.8% on `lcg` and −0.3% on `trees`** before this capture was taken. This run reproduces it: −31.9% and +1.7%, on a different architecture, by a different statistic, against a different baseline. The in-repo figure was min-of-12 on x86-64; this is mean-of-5 on arm64. Two methods that share no machine, no statistic and no comparison arm agreeing to a tenth of a percent is a stronger claim than either could make alone, and with every relevant toolchain version held fixed there is nothing else in the interval it could be.

**`trees` did not move, and the same measurement predicted that too.** The scalar-locals scope is *locals only* — nothing crosses a function boundary — and `curios/src/tests/codegen/structural.rs`'s `trees_constructor_payloads_stay_boxed` pins the consequence structurally: a `Tree/node`'s payload fields stay uniformly boxed. So the pass that bought `lcg` has no reach into this workload by construction.

The interval's other large runtime effort, the value-lifetime campaign, is the one that might have been expected to reach it, and did not. Its mechanism is that **a value costs when it is kept, not when it is named** — it removes heap traffic whose identity is never observed. `trees` is built specifically to exclude that case: unique heap-numbered payloads make every node structurally distinct so nothing can be shared, and `sum` then reads every one of the 2^(D+1)−1 nodes. Every allocation in it is kept and observed, so there is nothing for the campaign to take away. Its recorded wins were on the string walks that *are* the named-but-not-kept case — the digit walk fifteen percent under its pre-campaign floor, the multi-byte walk twenty-three — and those programs are not in this harness. The +1.7% / +2.1% itself sits inside the band runs 02 → 03 already established as between-sitting noise on this workload (+2.0% and +5.6%, reported then as no movement).

The honest summary is that this interval bought a third of the integer column and nothing of the allocation column, and that both halves were known in advance from the scope of the mechanism rather than discovered here.

## What actually happened this interval

303 commits and four releases, and unlike the previous interval a large share of it was about speed — most of which this harness cannot see, because both workloads are first-order integer and allocation loops with no strings, no closures and no monadic structure.

What the harness does see is the dataflow substrate above. What it does not see, all landed and all measured against their own probes: the return protocol, where a class of functions decided over the undirected tail-call graph now hands back a returned construction's leading fields so callers stop rebuilding what they take apart; absorbing into a function the application its returned closure always receives, which took `state_monad` from 1.124s to 0.047s and `rng_state` from 0.825s to 0.029s once the multi-site inline budget rose with it; an idiomatic string walk costing about a sixth of what it did, none of the three changes that bought it being the mechanism proposed for it; and the value-lifetime campaign's continuation scalar replacement and window virtualization. A copier closed under lexical nesting landed underneath several of these, and cost 0.3% emitted size for no measurable time — a floor, not an optimization.

Beside the backend, the compiler grew a compilation-unit boundary (`curios-unit`), a package manifest, dependency resolver and store (`curios-package`), a certified-prelude split separating the image from the verdict, one owning crate per external dependency, an explicit-stack closed-term machine shared by both checkers, and a reduction budget that prices a step by what it builds. Several crates were renamed to a uniform scheme in the process, this harness among them — `benchmarks/` is now `curios-benchmarks/`.

## Where Curios stands now

**Curios is ~1.32× off native Rust on tight integer work and 1.12× off Rust → wasm — ahead of OCaml natively and ahead of AssemblyScript in wasm, in both cases for the first time — while allocation is unchanged at ~5.85× off Lean 4 and 2.06× off Rust → wasm.**

The integer column is the one that moved, by a third, from a mechanism whose scope predicted exactly that and predicted the allocation column staying put. Allocation is now the whole of the remaining distance, and nothing that landed this interval was aimed at it.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table. Two notes specific to this run. **Run 04's engine caveat is discharged** — wasmtime is 47.0.3 in both captures and the embedded crate is 46.0.1 in both, so this is the first interval in which a run-to-run Curios comparison holds workload, setup and engine all fixed. And **Node's `trees` row is too noisy to order against Curios's**; its 52.6 ms spread swallows the 1.7 ms between the two means.
