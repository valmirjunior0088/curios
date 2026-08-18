# Results — the most surprising night in a long while

One run of the harness in [README.md](README.md), captured 2026-06-30. Read this as orientation, not a leaderboard: it is a single-machine, single-sitting snapshot whose job is to tell Curios roughly where it stands. It delivered that — and then a few things happened that I did not see coming.

## How this was run

- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares that same virtualized guest, so the numbers are honest _relative to each other_.
- **Engine (wasm section)** — wasmtime 46.0.1, Cranelift.
- **Method** — hyperfine, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes). Both immune to constant-folding, i31-safe, bit-identical across all eight languages (verified: `lcg(8) = 9345`, `trees(10) = 96122`, `lcg(1e8) = 17662`, `trees(21) = 536864`).
- **Toolchains** — rustc 1.96.1, OCaml 5.2.0 (flambda), Node v22.23.1 (V8), Lean 4.31.0, Grain 0.7.2, AssemblyScript 0.28.19. Perf-correct installs; see the README.

One thing to keep straight while reading: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and runs the very same module as its wasm row — so those two Curios numbers agreeing (446 vs 445 ms) is a consistency check, not two implementations. There is no separate "Curios wasm tax."

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language        |               Mean |   vs best |
| :-------------- | -----------------: | --------: |
| Rust            |     226.0 ± 0.9 ms |     1.00× |
| Lean 4          |     227.5 ± 0.9 ms |     1.01× |
| Node (V8)       |     236.9 ± 1.2 ms |     1.05× |
| OCaml (flambda) |     360.7 ± 0.7 ms |     1.60× |
| **Curios**      | **446.1 ± 4.7 ms** | **1.97×** |

### `trees` — allocation + heap traversal (D = 21)

| Language        |               Mean |   vs best |
| :-------------- | -----------------: | --------: |
| Lean 4          |      41.8 ± 0.4 ms |     1.00× |
| Rust            |      87.0 ± 0.4 ms |     2.08× |
| OCaml (flambda) |     105.7 ± 7.6 ms |     2.53× |
| Node (V8)       |     190.5 ± 1.8 ms |     4.56× |
| **Curios**      | **314.9 ± 4.4 ms** | **7.54×** |

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language       |               Mean |   vs best |
| :------------- | -----------------: | --------: |
| Rust → wasm    |     268.3 ± 2.8 ms |     1.00× |
| AssemblyScript |     315.0 ± 4.5 ms |     1.17× |
| **Curios**     | **444.6 ± 1.1 ms** | **1.66×** |
| Grain          |    29,684 ± 167 ms |   110.64× |

### `trees` (D = 21)

| Language       |                Mean |   vs best |
| :------------- | ------------------: | --------: |
| Rust → wasm    |      123.8 ± 1.2 ms |     1.00× |
| AssemblyScript |      226.4 ± 4.6 ms |     1.83× |
| **Curios**     | **322.2 ± 20.0 ms** | **2.60×** |
| Grain          |   1,781.8 ± 31.6 ms |    14.39× |

## The surprising night

**Lean 4 beat Rust by 2× on `trees` — and lapped everything else.** The one language here in Curios's own family, the dependently-typed proof-oriented peer, is the outright allocation champion: 41.8 ms against Rust's 87.0 ms, 4.6× ahead of Node. Perceus reference counting with in-place reuse turns "allocate 4.2M nodes and fold them" into something that outruns `Box<T>`. I expected the systems language to own the allocation workload. It came second. The corollary matters more than the headline: Curios's worst column is not a curse of the paradigm — a language in the same family wins it.

**Grain melted down on the loop: 110× slower than Rust → wasm.** 29.7 seconds to do what Rust → wasm finishes in 268 ms. Grain's unified `Number` type pays a per-operation representation dispatch that, times 10⁸ iterations, is ruinous — yet on `trees`, which is allocation rather than arithmetic, the same language on the same engine is "only" 14× off. One toolchain, two completely different personalities depending on whether the hot path is numbers or nodes. It is the most dramatic swing in the dataset, and the reason it earned its place in the tables rather than a quiet footnote.

**V8 and Lean essentially tie native Rust on the loop.** Node at 1.05×, Lean at 1.01×. A JIT with a decade of tuning and an AOT-compiler-to-C both land within a whisker of `rustc -O` on a loop this hot and this simple. "Interpreters are slow" does not survive contact with the workload.

## Where Curios stands

The number I came for: **Curios is ~2× off Rust on tight integer work and ~3.6–7.5× off the leaders on allocation.**

- **Integer loop:** 1.97× native Rust, 1.66× Rust → wasm, faster than Grain by 67×, slower than AssemblyScript by 1.4×. For a young language whose `Nat`/`Int` are _checked_ i31 — every multiply carries an overflow-trap the others don't — being within 2× of Rust on an ALU loop is a better result than I feared.
- **Allocation:** 7.54× off Lean and 3.6× off Rust natively; 2.60× off Rust → wasm. This is the weak column, and the whole exercise points a finger straight at it: Curios's wasm-GC allocation and heap traversal are where the next wins live. Lean's result is the existence proof that there is a lot of room to take.

Net: creditable on compute, clearly behind on allocation, and — the happy surprise — behind _fixable_ leaders rather than fundamental ones.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number); the load-bearing ones: single machine under a macOS→Linux VM (relative numbers only), whole-process wall-clock, idiomatic machine integers (Curios's a checked i31), and wasm-GC vs linear-memory GC in the wasm section — so the `trees` rows compare _GC strategies_, not just codegen. Run it again on a quiet machine before quoting any single figure to three digits.
