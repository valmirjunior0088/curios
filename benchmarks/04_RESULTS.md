# Results — the run that had to fix the compiler before it could measure it

One run of the harness in [README.md](README.md), captured 2026-08-09. Nine days and 416 commits since [run 03](03_RESULTS.md), which measured a flat interval and said so. This one is the opposite in every respect except the headline: the interval was the largest between any two runs so far, three minor releases landed in it, and the numbers still barely moved.

It also could not be captured at first. The harness image failed to build twice, its `curios-prelude` build script killed by the OOM killer inside Docker's default 8 GB VM. That was not a harness problem: the build script needed 7.71 GB of resident memory, so capping its build parallelism could not help and did not. Diagnosing it is what the run turned into, and the fix is the reason there are numbers below.

## How this was run

- **Source** — Curios compiler commit `9d2e061032fe`, version 0.8.1 (run 03 was 0.5.0).
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 47.0.3, Cranelift. **Run 03 used 46.0.1**; see the caveat below.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.2, Lean 4.32.2, Grain 0.7.2, AssemblyScript 0.28.20.
- **The benchmark programs changed** — `lcg.crs` and `trees.crs` were migrated to the `Io` entrypoint contract that landed in this interval: a program's tail is now an `Io({})` description, and `/std/read()` is sequenced with postfix `!` rather than matched directly. The computation each program performs is untouched, and both still print the anchor values every other language prints.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 223.4 ± 0.3 ms | 1.00× |
| Lean 4 | 227.0 ± 5.2 ms | 1.02× |
| Node (V8) | 234.3 ± 1.2 ms | 1.05× |
| OCaml (flambda) | 357.8 ± 0.2 ms | 1.60× |
| **Curios** | **435.1 ± 1.8 ms** | **1.95×** |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 40.6 ± 0.6 ms | 1.00× |
| Rust | 83.4 ± 0.6 ms | 2.06× |
| OCaml (flambda) | 97.4 ± 0.8 ms | 2.40× |
| Node (V8) | 187.2 ± 2.3 ms | 4.61× |
| **Curios** | **241.9 ± 1.8 ms** | **5.96×** |

Node's `trees` row ran clean this time — 183.7 to 189.6 ms across the five timed runs, against swings of 40 ms and more in runs 01 through 03. Nothing about the harness changed to earn that, so read it as this machine being quiet rather than as V8 having improved.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 259.9 ± 2.5 ms | 1.00× |
| AssemblyScript | 319.2 ± 3.0 ms | 1.23× |
| **Curios** | **435.7 ± 2.9 ms** | **1.68×** |
| Grain | 29,352 ± 9 ms | 112.92× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 122.0 ± 4.1 ms | 1.00× |
| AssemblyScript | 215.4 ± 2.4 ms | 1.76× |
| **Curios** | **244.5 ± 2.3 ms** | **2.00×** |
| Grain | 1,744 ± 6 ms | 14.29× |

## Flat again, across the largest interval yet

| Curios row | Run 00 | Run 01 | Run 02 | Run 03 | Run 04 | 04 vs 00 | 04 vs 03 |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | 437.3 ms | 435.1 ms | −2.5% | −0.5% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | 438.4 ms | 435.7 ms | −2.0% | −0.6% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | 256.4 ms | 241.9 ms | −23.2% | −5.7% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | 260.0 ms | 244.5 ms | −24.1% | −6.0% |

`lcg` held within half a percent of run 03 on both paths, which now makes four consecutive runs sitting on the same integer number while the compiler underneath was rebuilt repeatedly. `trees` came down 5.7% native and 6.0% in wasm, landing below run 02's previous best and taking the allocation column to its lowest figure on record.

Whether the compiler earned that 6% is not something this run can establish. Four new passes landed in `curios-cont` during the interval — a join specializer for known-tag tuple jump arguments, common-subexpression elimination over dominated deterministic intrinsics, constant emission data hoisted into start-initialized globals, and one-literal identity and absorption folding — plus a raised fixpoint backstop that lets more optimization rounds actually run. The join specializer is the one whose shape matches `trees`: an inductive constructor *is* a known-tag tuple. But the wasm engine also moved from 46.0.1 to 47.0.3 between the two runs, and it sits under both Curios rows as well as the entire wasm section. A 6% allocation improvement is well within what an engine release can deliver on its own. Attributing it would need the two versions run against one compiler, which this harness did not do.

## What actually happened this interval

The interval carried 416 commits and three minor releases (0.5.0 → 0.8.1), and almost none of it was about speed: the `Io` monad and the typed-effect discipline replacing the purity analysis, a canonical formatter and `curios format`, goal suggestions, `Lift` embeddings and concept-dispatched operators, the surface printer rebuilt on width-aware groups, and the universe hierarchy that retired `Type : Type`. That the runtime numbers moved by single digits across all of it is the same result run 03 reported, over an interval an order of magnitude larger.

The compiler-side story is where this run spent itself. `curios-prelude`'s build script peaked at 7.71 GB resident and took 261 seconds, and a profile attributed 76% of that peak to a single span: `universe::substitute` retained 5,024 MB of a 6,580 MiB total. The universe solver's constraint store journals a pre-image for every rewrite so a speculative branch can be rolled back, but nothing dropped those pre-images when a branch *succeeded* — they accumulated until the declaration ended, and because a substitution widens the constraints it lands in, the journal kept every intermediate width of every constraint it ever touched. Scoping the journal to open speculation took the build to 1,665 MiB and 2.16 GB RSS, and the image then built in the same 8 GB VM that had killed it twice, with no Dockerfile or Docker setting changed.

Two things fell out of that which are worth recording. The profiler gained memory reporting — `retained` and `allocated` bytes per span, behind the existing `profile` feature — because a timing-only profile could not have found this. And `UniverseSolver::zonk` was converted from native recursion to an explicit worklist: it was the deepest walk in the build, leaving the elaborator within about a megabyte of the default 8 MB stack, close enough that adding a few bytes per frame anywhere else in the crate overflowed it. Both prelude archives are byte-identical to the ones the previous compiler emitted, so neither change altered what gets compiled.

## Where Curios stands now

**Curios remains ~1.9–2.0× off native Rust on tight integer work, while allocation is now ~6.0× off Lean 4 and 2.00× off Rust → wasm** — the closest the allocation column has come to the wasm leader across five runs, though the engine bump means the compiler cannot claim all of the distance.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table. New to this run: **the wasm engine version changed**, so the run-to-run Curios comparison — normally the strongest signal here, because workload and setup stayed fixed — is for the first time comparing across two engines as well as two compilers. Treat this run's `trees` improvement as unattributed until a run holds the engine fixed.
