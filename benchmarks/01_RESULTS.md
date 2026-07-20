# Results — this is what growing a language looks like

One run of the harness in [README.md](README.md), captured 2026-07-16. It has been sixteen days and 320 commits since [the first run](00_RESULTS.md). In that time Curios did not sit still long enough to protect a benchmark number: it grew concepts and witnesses, concept-dispatched operators, an extensible foreign interface, packed `Bits` and `Bytes`, certified `Char` and `Str`, arbitrary-precision naturals and integers, richer matching and spread syntax, new erased-stage specialization, and an archived fixed prelude.

So I expected this run to give some ground back. Not because adding features should make unrelated programs slower forever, and not because every item above reaches this hot path, but because this was a language-expansion sprint rather than a performance-conservation exercise. Representations, lowering, optimization, runtime carriers, and the standard library all moved at once. The interesting question was not whether there would be a bill. It was where the bill would land.

It landed almost entirely on allocation.

## How this was run

- **Source** — Curios compiler commit `395aa359c4a4`, with the benchmark Dockerfile fixed to include the new `curios-prelude` workspace crate.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 46.0.1, Cranelift.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.0, OCaml 5.2.0 (flambda), Node v22.23.1, Lean 4.32.0, Grain 0.7.2, AssemblyScript 0.28.19.

One thing remains worth repeating from run 00: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 226.6 ± 1.9 ms | 1.00× |
| Lean 4 | 227.6 ± 1.0 ms | 1.00× |
| Node (V8) | 236.0 ± 1.2 ms | 1.04× |
| OCaml (flambda) | 362.7 ± 1.7 ms | 1.60× |
| **Curios** | **454.6 ± 2.1 ms** | **2.01×** |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 42.1 ± 0.3 ms | 1.00× |
| Rust | 87.8 ± 0.6 ms | 2.09× |
| OCaml (flambda) | 102.9 ± 0.5 ms | 2.45× |
| Node (V8) | 232.5 ± 55.4 ms | 5.53× |
| **Curios** | **462.2 ± 16.6 ms** | **10.99×** |

Node's `trees` row wandered from 189.1 to 293.8 ms. It belongs in the orientation table, but its mean is too noisy for a fine-grained run-to-run story.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 263.8 ± 4.0 ms | 1.00× |
| AssemblyScript | 316.6 ± 3.2 ms | 1.20× |
| **Curios** | **453.0 ± 1.2 ms** | **1.72×** |
| Grain | 29,898 ± 136 ms | 113.33× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 125.8 ± 2.0 ms | 1.00× |
| AssemblyScript | 214.0 ± 4.9 ms | 1.70× |
| **Curios** | **456.2 ± 3.2 ms** | **3.63×** |
| Grain | 1,762 ± 3 ms | 14.00× |

## The part I did not expect to survive intact

**The integer loop barely moved.** Curios went from 446.1 to 454.6 ms in the native table and from 444.6 to 453.0 ms in the wasm table: 1.9% in both cases. That is noise-sized movement beside the amount of language built around it. Every infix operator in the source now travels through the language's concept system before specialization brings primitive arithmetic back into view, yet the hot loop remains almost exactly 2× native Rust and 1.7× Rust → wasm.

That is the best result in this run. Curios gained abstraction without imposing a blanket abstraction tax on its simplest compute workload. The new language is much larger; the loop is not meaningfully slower.

## The bill I expected

**Allocation moved from a weakness to the obvious debt.** Curios went from 314.9 to 462.2 ms in the native table and from 322.2 to 456.2 ms in the wasm table. That is 46.8% and 41.6% slower respectively.

| Curios row | Run 00 | Run 01 | Change |
| :--- | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | +1.9% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | +1.9% |
| `trees`, native targets | 314.9 ms | 462.2 ms | +46.8% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | +41.6% |

The shape is too clean to blame on the executable wrapper: Curios's two paths agree within 1.3%. It is also too large to hide behind a generally noisy machine. Excluding Node, every comparison language's `trees` mean stayed within about 6% of run 00; Curios moved by more than 40% in both sections.

And still: this is the kind of bill I expected after sixteen days spent widening the language at every level. The benchmark program is textually almost unchanged, but it now enters through a larger standard library and richer elaboration path, uses concept-dispatched arithmetic, and emerges from compiler stages that gained new representations and optimization responsibilities. Some of that work is compile-time-only and some is dead before execution, so this run does not get to name a culprit. It only says that allocation performance was not conserved while the system grew.

There is a useful scale hidden beneath the ratio. Curios still constructs and traverses roughly 4.2 million unique nodes in under half a second—about nine million nodes per second—through checked arithmetic and wasm-GC. That is not a bad floor for a young dependently typed language. It is simply no longer close enough to the leaders to pretend the allocation path is done.

## Where Curios stands now

The new number is: **Curios remains ~2× off native Rust on tight integer work, while allocation is ~5.3× off native Rust, ~11× off Lean, and ~3.6× off Rust → wasm.**

I am happier with that result than the allocation delta alone suggests. Run 00 measured a small language with promising code generation. Run 01 measures a substantially more capable language whose compute result survived the expansion and whose allocation result tells us exactly where optimization work accumulated.

Expected does not mean free, and it does not mean explained. The right next move is not to rerun the same table until it gives a kinder number. It is to profile or bisect the Curios `trees` path between these snapshots, turn the broad allocation bill into one or more named costs, and then earn the old number back without giving up the language that arrived in between.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table. This is still orientation, not a leaderboard. The run-to-run Curios comparison is the strongest signal because its workload and execution setup stayed fixed; the explanation for that signal remains future work.
