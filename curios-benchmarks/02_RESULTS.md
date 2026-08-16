# Results — the debt got paid back, with interest

One run of the harness in [README.md](README.md), captured 2026-07-20. Four days and 50 commits since [run 01](01_RESULTS.md), that run closed with a specific, named debt: allocation had gotten 42–47% slower while the language grew, and the suggested next move was to "profile or bisect the Curios `trees` path... and then earn the old number back."

In the interim, two structural rewrites landed: the arena-based Ersd v2 erased representation replaced the legacy erasure path end to end, and Continuation IR v2 progressed to the point its specification was retired as superseded by the landed design. Neither commit series names itself as a performance fix — this run does not get to claim a single culprit either, same as run 01 didn't. But the shape of the result lines up with where that work touches: the allocation-heavy workload swung hard, the arithmetic-heavy one barely moved.

## How this was run

- **Source** — Curios compiler commit `89554494fc38`. The benchmark harness's Docker image tag was renamed from `curios-bench` to `curios-benchmarks` in this run; no compiler source changed as part of that.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 46.0.1, Cranelift.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.1, Lean 4.32.0, Grain 0.7.2, AssemblyScript 0.28.19.

One thing remains worth repeating from run 00: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 225.2 ± 0.6 ms | 1.00× |
| Lean 4 | 226.4 ± 0.8 ms | 1.00× |
| Node (V8) | 236.1 ± 1.1 ms | 1.05× |
| OCaml (flambda) | 359.8 ± 0.3 ms | 1.60× |
| **Curios** | **438.5 ± 1.4 ms** | **1.95×** |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 41.5 ± 0.3 ms | 1.00× |
| Rust | 84.7 ± 0.3 ms | 2.04× |
| OCaml (flambda) | 99.8 ± 0.6 ms | 2.40× |
| Node (V8) | 207.6 ± 37.9 ms | 5.00× |
| **Curios** | **251.5 ± 11.1 ms** | **6.06×** |

Node's `trees` row again ran noisy (187.8 to 274.9 ms across the five timed runs, plus a slow first warmup); read it as orientation, not a fine-grained number, same caveat as run 01.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 265.5 ± 6.4 ms | 1.00× |
| AssemblyScript | 318.4 ± 2.5 ms | 1.20× |
| **Curios** | **438.2 ± 4.8 ms** | **1.65×** |
| Grain | 30,029 ± 216 ms | 113.10× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 123.6 ± 2.9 ms | 1.00× |
| AssemblyScript | 212.7 ± 3.6 ms | 1.72× |
| **Curios** | **246.1 ± 2.0 ms** | **1.99×** |
| Grain | 1,764 ± 4 ms | 14.26× |

## The debt, repaid

**Allocation didn't just recover, it overshot the original baseline.** Curios's `trees` row went from 462.2 ms (run 01) to 251.5 ms native, and from 456.2 ms to 246.1 ms in wasm — 45.6% and 46.1% faster than run 01, respectively. That's not merely undoing run 01's regression: it lands 20.1% below run 00's original 314.9 ms native figure and 23.6% below run 00's 322.2 ms wasm figure. Whatever run 01 measured as a language-expansion tax, this run measures as more than paid off.

| Curios row | Run 00 | Run 01 | Run 02 | Run 02 vs Run 00 | Run 02 vs Run 01 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | −1.7% | −3.5% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | −1.4% | −3.3% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | −20.1% | −45.6% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | −23.6% | −46.1% |

The two Curios paths still agree closely (438.5 vs 438.2 ms on `lcg`, 251.5 vs 246.1 ms on `trees`, within 2.2%), so this is one story told twice, not a wasm-specific artifact. In throughput terms: `trees` at D = 21 builds and reduces roughly 4.2 million nodes in about a quarter of a second, near 17 million nodes per second — nearly double run 01's ~9 million and well above the rate implied by run 00.

**The integer loop kept doing what it did in run 01: nothing dramatic.** 454.6 → 438.5 ms native (−3.5%), 453.0 → 438.2 ms wasm (−3.3%). Small enough to be adjacent noise, or a minor knock-on benefit from the same rewrites — either way, `lcg` has now held within a few percent of native Rust's 2× line across all three runs while the compiler under it changed twice.

## Where Curios stands now

The new number is: **Curios remains ~1.9–2.0× off native Rust on tight integer work, while allocation is down to ~6.1× off Lean 4 and ~2.0× off Rust → wasm** — the closest the allocation column has been to the leaders across all three runs.

Run 01 declined to name a culprit for the regression it measured; this run declines to claim full credit for the recovery, for the same reason. Ersd v2's arena erasure and the Continuation IR v2 work both landed in the intervening 50 commits, and both touch the representations and lowering steps that sit between a `trees` allocation and the wasm-GC call that performs it. That is circumstantial, not a profiled attribution — the profiling run 01 asked for still has not happened, and the honest way to name a cause remains bisecting the interval it was asked for.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table. The run-to-run Curios comparison stays the strongest signal because its workload and execution setup stayed fixed across all three runs; a profiled or bisected explanation for this swing remains future work, same as it was after run 01.
