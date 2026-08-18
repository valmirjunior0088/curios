# Results — nothing moved, and that was the point

One run of the harness in [README.md](README.md), captured 2026-07-31. Eleven days and 50 commits since [run 02](02_RESULTS.md), which closed with an allocation recovery it declined to fully explain. This run has no comparable performance story to tell, and that absence is itself the finding.

The interval was almost entirely one project: building out `curios-cert`, an independent trusted kernel that rechecks a whole compiled module against the same judgments the elaborator already made. It moved the finished-program representation into `curios-core`, decided erasure obligations and universe-context satisfiability inside the kernel rather than trusting the elaborator's word for them, and landed a long series of "refuse a malformed X" commits hardening what the kernel rejects — a redeclared item, a grafted body at the wrong index, a body of the wrong type entirely, a universe context that names what it doesn't declare. None of it touches `curios-ersd`'s optimizer, `curios-cont`'s CPS lowering, or `curios-wasm`'s codegen — the stages that actually decide what a compiled program does at runtime. The kernel is a second, independent judge of a program that has already been compiled; it doesn't change how that program compiles or what it does when it runs. Going in, the working hypothesis was flat `lcg`/`trees` numbers, and that's what came back.

## How this was run

- **Source** — Curios compiler commit `834793ad6517`.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 46.0.1, Cranelift.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included).
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21 (~4.2M nodes).
- **Correctness** — all eight implementations agreed at both cross-check inputs: `lcg(8) = 9345` and `trees(10) = 96122`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.1, Lean 4.32.2, Grain 0.7.2, AssemblyScript 0.28.19.
- **Harness itself changed, not just the compiler** — `curios-benchmarks/Dockerfile`'s crate `COPY` list was missing `curios-core`, `curios-cert`, and `curios-profile` (added to the workspace since run 02's harness was last exercised) and was repaired; a `make curios/benchmarks` target replaced the old `build.sh` helper; and `entrypoint.sh` switched to an explicit stdout allowlist — only `hyperfine`'s own report reaches stdout, everything else (build progress, the correctness cross-check) goes to stderr — which is how this run's numbers were captured cleanly. No compiler source changed as part of any of that.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 225.7 ± 1.7 ms | 1.00× |
| Lean 4 | 226.8 ± 2.2 ms | 1.00× |
| Node (V8) | 235.1 ± 1.6 ms | 1.04× |
| OCaml (flambda) | 360.0 ± 2.0 ms | 1.60× |
| **Curios** | **437.3 ± 1.9 ms** | **1.94×** |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 41.6 ± 0.5 ms | 1.00× |
| Rust | 90.0 ± 0.9 ms | 2.16× |
| OCaml (flambda) | 103.1 ± 0.2 ms | 2.48× |
| Node (V8) | 213.9 ± 43.8 ms | 5.14× |
| **Curios** | **256.4 ± 8.3 ms** | **6.16×** |

Node's `trees` row ran noisy again (192.5 to 292.2 ms across the five timed runs, plus a first-run outlier hyperfine flagged directly); same caveat as every prior run — read it as orientation, not a fine-grained number.

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 261.3 ± 1.3 ms | 1.00× |
| AssemblyScript | 316.3 ± 4.0 ms | 1.21× |
| **Curios** | **438.4 ± 1.7 ms** | **1.68×** |
| Grain | 29,898 ± 120 ms | 114.42× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 127.0 ± 1.5 ms | 1.00× |
| AssemblyScript | 232.1 ± 2.2 ms | 1.83× |
| **Curios** | **260.0 ± 9.2 ms** | **2.05×** |
| Grain | 1,787 ± 11 ms | 14.07× |

## Flat, as predicted

| Curios row | Run 00 | Run 01 | Run 02 | Run 03 | Run 03 vs Run 00 | Run 03 vs Run 02 |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | 437.3 ms | −2.0% | −0.3% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | 438.4 ms | −1.4% | +0.0% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | 256.4 ms | −18.6% | +1.9% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | 260.0 ms | −19.3% | +5.6% |

`lcg` held within half a percent of run 02 on both paths, well inside the noise this harness has shown between identical-source runs. `trees` moved a little more — +1.9% native, +5.6% wasm — but run 02 measured its own allocation swing at over 45%, and hyperfine flagged statistical outliers on this run's `trees` table the same way it has before; a low-single-digit wobble here reads as run-to-run variance, not a regression to chase. Both Curios paths still agree closely with each other (437.3 vs 438.4 ms on `lcg`, 256.4 vs 260.0 ms on `trees`), so whatever small movement there is tells one consistent story rather than a wasm-specific artifact.

## Where Curios stands now

The number is unchanged from run 02: **Curios remains ~1.9–2.0× off native Rust on tight integer work, while allocation sits at ~6.2× off Lean 4 and ~2.1× off Rust → wasm.** Fifty commits landed, a second independent typechecker went from nonexistent to sitting on the compile path, and the runtime numbers didn't move outside their own noise floor — which is exactly what should happen when an entire interval's work is a trust boundary around already-compiled programs, not a change to how they compile.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table. The run-to-run Curios comparison stays the strongest signal because its workload and execution setup stayed fixed across all four runs now on record.
