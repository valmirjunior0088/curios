# curios-benchmarks — where is Curios, roughly, on performance?

A throwaway, run-once-every-never harness to place Curios against industry languages with a single number per workload. It is **not** a rigorous benchmark suite — its job is orientation: "Curios is ~Nx off Rust on integer loops, ~Mx on allocation." Everything runs in one kitchen-sink arm64 container so nothing has to be installed locally.

## Results

Every capture so far: Apple Silicon, Docker Desktop's Linux VM, one pinned arm64 core.

### Curios across peers

Each results file is a single-sitting snapshot with its own commentary:

| Run                                                              | Captured   |
| ---------------------------------------------------------------- | ---------- |
| [00 — The most surprising night in a long while](00_RESULTS.md)  | 2026-06-30 |
| [01 — This is what growing a language looks like](01_RESULTS.md) | 2026-07-16 |
| [02 — The debt got paid back, with interest](02_RESULTS.md)      | 2026-07-20 |
| [03 — Nothing moved, and that was the point](03_RESULTS.md)      | 2026-07-31 |
| [04 — The run that had to fix the compiler first](04_RESULTS.md) | 2026-08-09 |
| [05 — The plateau broke, and only where it was aimed](05_RESULTS.md) | 2026-08-17 |
| [06 — The other column halved, and one commit did it](06_RESULTS.md) | 2026-08-17 |

### Curios across runs

Mean ± std dev in milliseconds from each run's results file, one row appended per capture; `× 00` compares against run 00's mean and `× prev` against the previous run's mean, in the same column. A workload that enters later is based at its own first capture, since there is no run 00 figure to divide by; the workloads without a capture yet — `chain`, `churn`, `spines` — have no table here.

The workload and the execution setup are fixed across runs, but the *toolchains* are not: each capture installs current versions, so the wasm engine under both Curios columns has changed between some rows. A row-to-row move is therefore Curios plus its engine, not Curios alone — run 04's `trees` improvement landed alongside a wasmtime major bump and is explicitly left unattributed for that reason.

#### `lcg`

<table>
  <thead>
    <tr>
      <th rowspan="2">Run</th>
      <th colspan="3">Native</th>
      <th colspan="3">WebAssembly</th>
    </tr>
    <tr>
      <th>mean (ms)</th>
      <th>× 00</th>
      <th>× prev</th>
      <th>mean (ms)</th>
      <th>× 00</th>
      <th>× prev</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>00</td>
      <td align="right">446.1 ± 4.7</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
      <td align="right">444.6 ± 1.1</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
    </tr>
    <tr>
      <td>01</td>
      <td align="right">454.6 ± 2.1</td>
      <td align="right">1.02×</td>
      <td align="right">1.02×</td>
      <td align="right">453.0 ± 1.2</td>
      <td align="right">1.02×</td>
      <td align="right">1.02×</td>
    </tr>
    <tr>
      <td>02</td>
      <td align="right">438.5 ± 1.4</td>
      <td align="right">0.98×</td>
      <td align="right">0.96×</td>
      <td align="right">438.2 ± 4.8</td>
      <td align="right">0.99×</td>
      <td align="right">0.97×</td>
    </tr>
    <tr>
      <td>03</td>
      <td align="right">437.3 ± 1.9</td>
      <td align="right">0.98×</td>
      <td align="right">1.00×</td>
      <td align="right">438.4 ± 1.7</td>
      <td align="right">0.99×</td>
      <td align="right">1.00×</td>
    </tr>
    <tr>
      <td>04</td>
      <td align="right">435.1 ± 1.8</td>
      <td align="right">0.98×</td>
      <td align="right">1.00×</td>
      <td align="right">435.7 ± 2.9</td>
      <td align="right">0.98×</td>
      <td align="right">0.99×</td>
    </tr>
    <tr>
      <td>05</td>
      <td align="right">296.3 ± 1.0</td>
      <td align="right">0.66×</td>
      <td align="right">0.68×</td>
      <td align="right">296.7 ± 5.1</td>
      <td align="right">0.67×</td>
      <td align="right">0.68×</td>
    </tr>
    <tr>
      <td>06</td>
      <td align="right">293.1 ± 1.1</td>
      <td align="right">0.66×</td>
      <td align="right">0.99×</td>
      <td align="right">293.2 ± 1.4</td>
      <td align="right">0.66×</td>
      <td align="right">0.99×</td>
    </tr>
  </tbody>
</table>

#### `trees`

<table>
  <thead>
    <tr>
      <th rowspan="2">Run</th>
      <th colspan="3">Native</th>
      <th colspan="3">WebAssembly</th>
    </tr>
    <tr>
      <th>mean (ms)</th>
      <th>× 00</th>
      <th>× prev</th>
      <th>mean (ms)</th>
      <th>× 00</th>
      <th>× prev</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>00</td>
      <td align="right">314.9 ± 4.4</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
      <td align="right">322.2 ± 20.0</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
    </tr>
    <tr>
      <td>01</td>
      <td align="right">462.2 ± 16.6</td>
      <td align="right">1.47×</td>
      <td align="right">1.47×</td>
      <td align="right">456.2 ± 3.2</td>
      <td align="right">1.42×</td>
      <td align="right">1.42×</td>
    </tr>
    <tr>
      <td>02</td>
      <td align="right">251.5 ± 11.1</td>
      <td align="right">0.80×</td>
      <td align="right">0.54×</td>
      <td align="right">246.1 ± 2.0</td>
      <td align="right">0.76×</td>
      <td align="right">0.54×</td>
    </tr>
    <tr>
      <td>03</td>
      <td align="right">256.4 ± 8.3</td>
      <td align="right">0.81×</td>
      <td align="right">1.02×</td>
      <td align="right">260.0 ± 9.2</td>
      <td align="right">0.81×</td>
      <td align="right">1.06×</td>
    </tr>
    <tr>
      <td>04</td>
      <td align="right">241.9 ± 1.8</td>
      <td align="right">0.77×</td>
      <td align="right">0.94×</td>
      <td align="right">244.5 ± 2.3</td>
      <td align="right">0.76×</td>
      <td align="right">0.94×</td>
    </tr>
    <tr>
      <td>05</td>
      <td align="right">245.9 ± 1.1</td>
      <td align="right">0.78×</td>
      <td align="right">1.02×</td>
      <td align="right">249.6 ± 3.8</td>
      <td align="right">0.77×</td>
      <td align="right">1.02×</td>
    </tr>
    <tr>
      <td>06</td>
      <td align="right">121.1 ± 1.5</td>
      <td align="right">0.38×</td>
      <td align="right">0.49×</td>
      <td align="right">121.4 ± 0.6</td>
      <td align="right">0.38×</td>
      <td align="right">0.49×</td>
    </tr>
  </tbody>
</table>

## Contestants

Two sections, with Curios as the subject compared against each:

| Section                      | Languages                               | What it tells you                                                                                                                               |
| ---------------------------- | --------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| **Native**                   | Rust, OCaml, JavaScript (Node), Lean 4  | distance from a native ceiling, a fair functional peer (OCaml), a world-class JIT (V8), and a dependently-typed peer with a real backend (Lean) |
| **WebAssembly (`wasmtime`)** | Rust→WebAssembly, Grain, AssemblyScript | how Curios's codegen compares to other WebAssembly languages on the same engine                                                                 |

Curios only targets WebAssembly, so its presence in both sections is not two backends: the Native row is a self-contained executable that embeds wasmtime and runs the exact module its WebAssembly row runs. Those two numbers agreeing is a consistency check, not a contest, and there is no separate "Curios WebAssembly tax" to discover. That reading belongs to Rust, which genuinely is one source compiled two ways — Rust-native vs Rust→WebAssembly is a clean read on what WebAssembly itself costs.

## Workloads

All five are (a) expressible in a total, structurally-recursive language, (b) immune to constant-folding and closed-form shortcuts — the input arrives at runtime — and (c) bit-identical in output across every implementation, so a mismatch flags a mistranslation before any timing is trusted. `entrypoint.sh` enforces that cross-check at the start of every run.

- **`lcg`** — iterate `x = (75 · x) mod 65537` N times from `x = 1`. One multiply + one modulo per iteration; the max intermediate is 75·65536 ≈ 4.9M, far under i31. Measures integer ALU + loop/call overhead. Default `N = 100_000_000` (≈ 0.45s of Curios compute; below ~10⁷ it is startup-dominated). Anchor: `lcg(10⁸) = 17662`.
- **`trees`** (the classic binary-trees allocation stress) — build a perfect tree of depth D whose nodes carry unique heap-numbered payloads (root `1`, children `2v` / `2v+1`), then reduce to `sum mod 1000003`. The unique payloads make every node distinct, defeating any structural subtree-sharing and forcing 2^(D+1)−1 real allocations; the modulus keeps the checksum inside i31. Measures allocation + GC and heap traversal. Default `D = 21` (≈ 4.2M nodes, ≈ 0.25s; D=23 ≈ 1s). Anchor: `trees(21) = 536864`.
- **`chain`** — build a cons list of 10 000 cells once, then transform it K times, each round rebuilding every cell from a predecessor that dies with the operation. Measures *death-birth churn*: unlike `trees`, where every allocation survives to be traversed, nothing here outlives the step that replaces it. That is the pairing [Perceus](https://dl.acm.org/doi/10.1145/3453483.3454032) reference counting turns into an in-place write, so the Lean column prices dynamic reuse and the ratio to it is the number this workload exists for. The seed is derived from K so nothing is a closed term, each round reverses the order (which the sum ignores), and every walk is tail-recursive or a loop so no contestant's stack depends on the 10 000. Default `K = 1600` (≈ 16M cells reborn, ≈ 0.33s). Anchors: `chain(8) = 819185`, `chain(1600) = 457407`.

  Two asymmetries belong beside its ratio rather than inside it. A Curios cons cell is **three** slots to Lean's two, since a tagged constructor carries its discriminant — real, and not what the workload is about. And the chain's live set is small by design, so the collector's marking half is barely exercised: what is measured is the allocation rate and the young-collection frequency that churn drives, which is the half reuse removes.

- **`churn`** — thread a six-field record through N LCG-fed steps, two fields updated per step, the written pair rotating over three phases so every field keeps circulating; print one field at the end. The purest record-update signal: the imperative contestants mutate a struct in place and allocate nothing, OCaml's and Grain's functional updates allocate a fresh record per step, and Lean's structure update is the shape Perceus's reset-and-reuse rewrites in place. Curios spells the update as a spread — and its optimizer erases the record entirely: the threaded record travels as fields, the loop allocates nothing (pinned by `churn_threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs`), so its column prices dispatch and checked i31 arithmetic against the mutation floor rather than allocation. The record-update tax this workload was specified to price therefore lives only where a record *rests*, which is the census's and the coming `spines` workload's territory. Default `N = 75_000_000` (≈ 0.33s of Curios compute). Anchors: `churn(8) = 897441`, `churn(75000000) = 762495`.

- **`spines`** — N LCG-keyed inserts into a map, then fold the values; the keys revisit a 65 536-value orbit, so the live set plateaus while every insert keeps rebuilding a root-to-leaf spine that dies with the operation — the live-set-under-churn dimension `chain` deliberately lacks. Two confounds are part of the design and the reason it orients rather than proves: the table compares map algorithms as much as memory management (Curios's crit-bit trie against imperative hash maps and the functional contestants' balanced trees), and `/std/Map` deliberately has no `Key(Nat)` — the injectivity a `Key` witness owes is unprovable for a division-based encoding under unary elimination, as the module records — so Curios keys enter through `Bytes/of_nat`: minimal big-endian, a division and a table index per byte, a boundary cost of a few arithmetic operations no int-keyed hash map quite mirrors. Lean's `Std.TreeMap` is the reuse-on-spines column: a persistent tree whose dying path Perceus rewrites in place. Default `N = 75_000` (≈ 0.3s of Curios compute). Anchors: `spines(8) = 28`, `spines(75000) = 675283`.

### Why the constants are small

Curios's `Nat` and `Int` are unbounded in the type checker but ride an **i31** — the unboxed WebAssembly-GC 31-bit integer — at runtime, and arithmetic is _checked_: a result that leaves i31 traps. Why it traps rather than wrapping is [Numeric carriers narrow by refusing, never by changing a value](../documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md). (`Flt`/`f64` has the range but heap-allocates per value — the wrong tool for a tight integer loop.) So every workload is deliberately sized to keep **every intermediate, including products,** within i31, and every other language uses its native integer to compute the identical values. The upshot: the integer comparison is like-for-like on values, and it honestly folds Curios's per-op overflow check into the measured cost rather than hiding it.

## Run it

```
curios-benchmarks/
  Dockerfile                 kitchen-sink arm64 image with all 8 toolchains + curios
  Dockerfile.dockerignore    what the build context excludes (target/, .artifacts/, .git)
  entrypoint.sh              build all, cross-check outputs, then 10 hyperfine tables
  programs/
    lcg/                     lcg.{crs,rs,ml,js,ts,gr}  Lcg.lean  lakefile.toml
    trees/                   trees.{crs,rs,ml,js,ts,gr}  Trees.lean  lakefile.toml
    chain/                   chain.{crs,rs,ml,js,ts,gr}  Chain.lean  lakefile.toml
  .artifacts/                built contestants — inside the container only; a run leaves nothing on the host
```

The image build needs the Curios sources, which live _above_ `curios-benchmarks/`, so it must run with the **repo root as the build context**. The `curios/benchmarks` Makefile target does that, from the repo root:

```sh
make curios/benchmarks

# tune the workloads:
docker run --rm --cpuset-cpus 0 -e N_LCG=200000000 -e D_TREES=23 -e K_CHAIN=3200 -e N_CHURN=150000000 -e N_SPINES=150000 -e RUNS=7 curios-benchmarks
```

The run splits its two audiences across the two streams, which is how a `--rm` container hands back a document without a bind mount:

- **stdout** carries the ten markdown tables and nothing the harness adds to them, so `docker run … > run.md` is the whole capture, ready to paste into a results file. Through `make curios/benchmarks > run.md` the two echoed recipe lines land above the first table; delete them and the rest is the document.
- **stderr** carries the build log, the correctness cross-check (all eight outputs must be identical), and hyperfine's own comparison with its relative "x times faster than" ratios. Watch this stream while it runs; read the ratio to Rust as the headline "where are we" number.

A table hyperfine fails to produce is absent from stdout rather than empty in it, and its error is on stderr.

## Toolchains — installed the perf-correct way

- **OCaml** — opam with a flambda switch (`ocaml-variants.5.2.0+options` + `ocaml-option-flambda`), compiled `-O3`; not apt's flambda-less 4.14, which would quietly handicap OCaml.
- **Node** — NodeSource current LTS for a fresh V8; not apt's 18.x.
- **Grain** — publishes **no arm64 binary**, and its source build needs `esy`, which also has no arm64 release. So the `.gr` are compiled to WebAssembly in an `amd64` build stage (the official x64 binary, emulated at build time only); the emitted WebAssembly is arch-neutral and runs on the final image's native arm64 wasmtime. Consequence: editing a `.gr` needs an image rebuild, not just a rerun.
- **Lean** — a Lake package built with `lake build` (the supported path to an executable), not a hand-rolled `lean -c` + `leanc`. Each program folder carries a small `lakefile.toml` because Lake is package-oriented; the other seven languages compile from a loose single file.
- **AssemblyScript** — asc plus the wasi-shim, which patches the built-in `process`/`console` into globals so the `.ts` needs no imports. One trap: `asc` must run with the shim's install dir as cwd or its `lib: ./assembly` glob fails to resolve — `entrypoint.sh` does this.
- **Rust / wasmtime / hyperfine / AssemblyScript's npm packages** — official image, install script, release binary, and npm.

## Caveats — read these before trusting a number

- **Idiomatic machine integers, not bignums.** Each language uses its natural integer (Curios i31, OCaml 63-bit `int`, Lean `UInt64`, …), never its default-but-slow arbitrary-precision type. We measure the language, not its bignum path — and for Curios that natural integer is a _checked_ i31, whose cost the numbers fairly include.
- **WebAssembly-GC vs linear-memory GC.** Curios delegates collection to wasmtime's GC; Grain, AssemblyScript, and Rust→WebAssembly manage memory in linear memory. The binary-trees row therefore compares _GC strategies_, not just codegen.
- **"Same engine" is approximate.** Each WebAssembly language brings its own host/ABI, so the WebAssembly section is "the same Cranelift engine core in different runners," not one controlled invocation. Fine for a ballpark; don't oversell it.
- **Whole-process wall-clock.** Startup (Node's V8 warmup, Curios's wasmtime instantiate, etc.) is included, because that is what running a program costs. The workloads are sized so the inner loop dominates startup; hyperfine's `--warmup` also primes caches. Keep the workloads large.
- **macOS → Linux VM.** On Apple Silicon, Docker runs in a VM. That is fine here because _every_ contestant — Curios included — shares the same virtualized guest, so the numbers are relative to each other. Keep everything `linux/arm64`; one stray amd64 image emulated under qemu makes that row meaningless.
- **binary-trees measures allocation + heap traversal, not steady-state collection.** It builds one big tree and exits, so every allocation survives to be traversed. Churn and collection pressure are `chain`'s half of the harness, and the two tables answer different questions about the same collector.
