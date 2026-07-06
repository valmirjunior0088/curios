# bench — where is Curios, roughly, on performance?

A throwaway, run-once-every-never harness to place Curios against industry languages with a single number per workload. It is **not** a rigorous benchmark suite — its job is orientation: "Curios is ~Nx off Rust on integer loops, ~Mx on allocation." Everything runs in one kitchen-sink arm64 container so nothing has to be installed locally.

## Contestants

Two sections, with Curios as the subject compared against each:

| Section              | Languages                              | What it tells you                                                                                                                              |
| -------------------- | -------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| **Native targets**   | Rust, OCaml, JavaScript (Node), Lean 4 | distance from a native ceiling, a fair functional peer (OCaml), a world-class JIT (V8), and a dependently-typed peer with a real backend (Lean) |
| **wasm on wasmtime** | Rust→wasm, Grain, AssemblyScript       | how Curios's codegen compares to other wasm languages on the same engine                                                                        |

Rust appears in both sections (one source, compiled two ways) — Rust-native vs Rust→wasm is a clean read on the "wasm tax."

## The i31 constraint — why the constants are small

Curios's `Nat` and `Int` are unbounded in the type checker but are represented as **i31** (the unboxed wasm-GC 31-bit integer) at runtime, and arithmetic is *checked*: a multiply whose result exceeds i31 traps. (`Flt`/`f64` has full 64-bit range but heap-allocates per value — the wrong tool for a tight integer loop.) So both workloads are deliberately sized to keep **every intermediate, including products,** within i31, and every other language uses its native integer to compute the identical values. The upshot: the integer comparison is like-for-like on values, and it honestly folds Curios's per-op overflow check into the measured cost rather than hiding it.

## Workloads

Both are (a) expressible in a total, structurally-recursive language, (b) immune to constant-folding (the input arrives at runtime) and to closed-form optimization, (c) i31-safe, and (d) bit-identical in output across all eight languages — so a mismatch flags a mistranslation before any timing is trusted. The Curios sources are **verified** locally (they compile, run, and the loop is confirmed tail-call-optimized); the anchors below are their actual output.

- **`lcg`** — iterate `x = (75 · x) mod 65537` N times from `x = 1`. One multiply + one modulo per iteration; the max intermediate is 75·65536 ≈ 4.9M, far under i31. Measures integer ALU + loop/call overhead. Default `N = 100_000_000` (≈ 0.45s of Curios compute; below ~10⁷ it is startup-dominated). Verified anchor: `lcg(10⁸) = 17662`.
- **`trees`** (the classic binary-trees allocation stress) — build a perfect tree of depth D whose nodes carry unique heap-numbered payloads (root `1`, children `2v` / `2v+1`), then reduce to `sum mod 1000003`. The unique payloads defeat Curios's `evaluate_pure_calls` subtree-sharing, forcing 2^(D+1)−1 real allocations; the modulus keeps the checksum inside i31. Measures allocation + GC and heap traversal. Default `D = 21` (≈ 4.2M nodes, ≈ 0.25s; D=23 ≈ 1s). Verified anchor: `trees(21) = 536864`.

## Layout

```
bench/
  Dockerfile           kitchen-sink arm64 image with all 8 toolchains + curios
  build.sh             build the image (repo root as context) from anywhere
  entrypoint.sh        build all, cross-check outputs, then 4 hyperfine tables
  programs/
    lcg/               lcg.{crs,rs,ml,js,ts,gr}  Lcg.lean  lakefile.toml
    trees/             trees.{crs,rs,ml,js,ts,gr}  Trees.lean  lakefile.toml
```

Each program folder also carries a small `lakefile.toml`, because Lean's build system (Lake) is package-oriented — its source needs a package rather than the loose single file the other seven compile from.

## Run it

The image build needs the Curios sources, which live *above* `bench/`, so it must run with the **repo root as the build context**. The helper does that from anywhere:

```sh
bash bench/build.sh                        # builds the image (repo root as context)
docker run --rm --cpuset-cpus 0 curios-bench

# equivalent by hand, from the repo root:
docker build --platform linux/arm64 -f bench/Dockerfile -t curios-bench .

# tune the workloads:
docker run --rm --cpuset-cpus 0 -e N_LCG=200000000 -e D_TREES=23 -e RUNS=7 curios-bench
```

`entrypoint.sh` first prints the correctness cross-check (all eight outputs must be identical), then hyperfine's comparison — with relative "x times faster than" ratios — for each table, and writes `bin/*.md`. Read the ratio to Rust as the headline "where are we" number.

## Toolchains — installed the perf-correct way

- **OCaml** — opam with a flambda switch (`ocaml-variants.5.2.0+options` + `ocaml-option-flambda`), compiled `-O3`; not apt's flambda-less 4.14, which would quietly handicap OCaml.
- **Node** — NodeSource current LTS for a fresh V8; not apt's 18.x.
- **Grain** — publishes **no arm64 binary**, and its source build needs `esy`, which also has no arm64 release. So the `.gr` are compiled to wasm in an `amd64` build stage (the official x64 binary, emulated at build time only); the emitted wasm is arch-neutral and runs on the final image's native arm64 wasmtime. Consequence: editing a `.gr` needs an image rebuild, not just a rerun.
- **Lean** — a Lake package built with `lake build` (the supported path to an executable), not a hand-rolled `lean -c` + `leanc`.
- **Rust / wasmtime / hyperfine / AssemblyScript** — official image, install scripts, release binary, and npm.

## Caveats — read these before trusting a number

- **Idiomatic machine integers, not bignums.** Each language uses its natural integer (Curios i31, OCaml 63-bit `int`, Lean `UInt64`, …), never its default-but-slow arbitrary-precision type. We measure the language, not its bignum path — and for Curios that natural integer is a *checked* i31, whose cost the numbers fairly include.
- **wasm-GC vs linear-memory GC.** Curios delegates collection to wasmtime's GC; Grain, AssemblyScript, and Rust→wasm manage memory in linear memory. The binary-trees row therefore compares *GC strategies*, not just codegen.
- **"Same engine" is approximate.** Each wasm language brings its own host/ABI, so the wasm section is "the same Cranelift engine core in different runners," not one controlled invocation. Fine for a ballpark; don't oversell it.
- **Whole-process wall-clock.** Startup (Node's V8 warmup, Curios's wasmtime instantiate, etc.) is included, because that is what running a program costs. The workloads are sized so the inner loop dominates startup; hyperfine's `--warmup` also primes caches. Keep the workloads large.
- **macOS → Linux VM.** On Apple Silicon, Docker runs in a VM. That is fine here because *every* contestant — Curios included — shares the same virtualized guest, so the numbers are relative to each other. Keep everything `linux/arm64`; one stray amd64 image emulated under qemu makes that row meaningless.
- **binary-trees measures allocation + heap traversal, not steady-state collection.** It builds one big tree and exits. For churn/collection pressure, loop the build over distinct seeds — an easy extension left out to keep v1 simple.

## VERIFY checklist

The Curios sources are verified locally (they compile, run, and match). The other toolchains' **APIs are now verified against source**; what remains is compilation and toolchain wiring, which only a real `docker build` can shake out:

1. **Grain** — APIs verified against the `grain-v0.7.2` stdlib: `from "wasi/process"`'s `argv() -> Result<Array<String>, _>`, `Result.unwrap`, `Number.parseInt(s, 10)`, `arr[i]` indexing, and the `print` pervasive. The `.gr` are precompiled to wasm in the amd64 stage; left to confirm: the emulated x64 `grain compile` succeeds and emits a WASI module wasmtime runs.
2. **AssemblyScript** — verified end to end against the installed shim (asc 0.28, wasi-shim): top-level statements compile into the WASI `_start`, and the shim patches the built-in `process`/`console` so the `.ts` uses `process.argv` / `console.log` as globals (no imports). `asc` must run with the shim's install dir as cwd or its `lib: ./assembly` glob fails to resolve — `entrypoint.sh` does this. Both programs compile and print the correct anchors.
3. **Lean** — `lakefile.toml` schema verified (`name` / `defaultTargets` / `[[lean_exe]]` with `name` + `root`). Left to confirm: `lake build` in each program dir resolves the elan default toolchain.
4. **wasmtime invocations** for Grain/AssemblyScript may still need WASI flags depending on how each emits its module.
