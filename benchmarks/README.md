# Benchmarks — where is Curios, roughly, on performance?

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
| [07 — Three columns arrived, and one of them is a wall](07_RESULTS.md) | 2026-08-18 |
| [08 — The wall fell by three quarters, and only the wall](08_RESULTS.md) | 2026-08-20 |
| [09 — Three columns fell together, and each was named before the run](09_RESULTS.md) | 2026-08-22 |

### Curios across runs

Mean ± std dev in milliseconds from each run's results file, one row appended per capture; `× 00` compares against run 00's mean and `× prev` against the previous run's mean, in the same column. A workload that enters later is based at its own first capture, since there is no run 00 figure to divide by — `chain`, `churn` and `spines` debuted in run 07 and are based there, so their `× 07` column is what `× 00` is for the other two.

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
    <tr>
      <td>07</td>
      <td align="right">297.8 ± 2.4</td>
      <td align="right">0.67×</td>
      <td align="right">1.02×</td>
      <td align="right">295.1 ± 3.5</td>
      <td align="right">0.66×</td>
      <td align="right">1.01×</td>
    </tr>
    <tr>
      <td>08</td>
      <td align="right">292.9 ± 0.3</td>
      <td align="right">0.66×</td>
      <td align="right">0.98×</td>
      <td align="right">295.7 ± 4.4</td>
      <td align="right">0.67×</td>
      <td align="right">1.00×</td>
    </tr>
    <tr>
      <td>09</td>
      <td align="right">293.6 ± 0.6</td>
      <td align="right">0.66×</td>
      <td align="right">1.00×</td>
      <td align="right">293.0 ± 0.5</td>
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
    <tr>
      <td>07</td>
      <td align="right">117.5 ± 2.3</td>
      <td align="right">0.37×</td>
      <td align="right">0.97×</td>
      <td align="right">115.5 ± 1.0</td>
      <td align="right">0.36×</td>
      <td align="right">0.95×</td>
    </tr>
    <tr>
      <td>08</td>
      <td align="right">115.5 ± 0.8</td>
      <td align="right">0.37×</td>
      <td align="right">0.98×</td>
      <td align="right">115.1 ± 0.7</td>
      <td align="right">0.36×</td>
      <td align="right">1.00×</td>
    </tr>
    <tr>
      <td>09</td>
      <td align="right">100.6 ± 0.6</td>
      <td align="right">0.32×</td>
      <td align="right">0.87×</td>
      <td align="right">100.9 ± 0.8</td>
      <td align="right">0.31×</td>
      <td align="right">0.88×</td>
    </tr>
  </tbody>
</table>

#### `chain`

<table>
  <thead>
    <tr>
      <th rowspan="2">Run</th>
      <th colspan="3">Native</th>
      <th colspan="3">WebAssembly</th>
    </tr>
    <tr>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>07</td>
      <td align="right">134.4 ± 1.1</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
      <td align="right">134.8 ± 1.3</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
    </tr>
    <tr>
      <td>08</td>
      <td align="right">133.4 ± 2.1</td>
      <td align="right">0.99×</td>
      <td align="right">0.99×</td>
      <td align="right">132.9 ± 1.0</td>
      <td align="right">0.99×</td>
      <td align="right">0.99×</td>
    </tr>
    <tr>
      <td>09</td>
      <td align="right">69.5 ± 0.5</td>
      <td align="right">0.52×</td>
      <td align="right">0.52×</td>
      <td align="right">69.2 ± 0.5</td>
      <td align="right">0.51×</td>
      <td align="right">0.52×</td>
    </tr>
  </tbody>
</table>

#### `churn`

<table>
  <thead>
    <tr>
      <th rowspan="2">Run</th>
      <th colspan="3">Native</th>
      <th colspan="3">WebAssembly</th>
    </tr>
    <tr>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>07</td>
      <td align="right">274.9 ± 2.1</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
      <td align="right">273.8 ± 0.4</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
    </tr>
    <tr>
      <td>08</td>
      <td align="right">273.4 ± 1.5</td>
      <td align="right">0.99×</td>
      <td align="right">0.99×</td>
      <td align="right">272.7 ± 0.6</td>
      <td align="right">1.00×</td>
      <td align="right">1.00×</td>
    </tr>
    <tr>
      <td>09</td>
      <td align="right">268.8 ± 0.8</td>
      <td align="right">0.98×</td>
      <td align="right">0.98×</td>
      <td align="right">268.4 ± 0.2</td>
      <td align="right">0.98×</td>
      <td align="right">0.98×</td>
    </tr>
  </tbody>
</table>

#### `spines`

<table>
  <thead>
    <tr>
      <th rowspan="2">Run</th>
      <th colspan="3">Native</th>
      <th colspan="3">WebAssembly</th>
    </tr>
    <tr>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
      <th>mean (ms)</th>
      <th>× 07</th>
      <th>× prev</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>07</td>
      <td align="right">206.7 ± 1.0</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
      <td align="right">207.0 ± 1.0</td>
      <td align="right">1.00×</td>
      <td align="right">—</td>
    </tr>
    <tr>
      <td>08</td>
      <td align="right">54.4 ± 0.3</td>
      <td align="right">0.26×</td>
      <td align="right">0.26×</td>
      <td align="right">53.9 ± 0.6</td>
      <td align="right">0.26×</td>
      <td align="right">0.26×</td>
    </tr>
    <tr>
      <td>09</td>
      <td align="right">24.4 ± 0.4</td>
      <td align="right">0.12×</td>
      <td align="right">0.45×</td>
      <td align="right">24.5 ± 0.5</td>
      <td align="right">0.12×</td>
      <td align="right">0.45×</td>
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

Five workloads, one directory each, carrying the same program in eight spellings. What each computes, its anchors and defaults, and why the constants are small are [the corpus's to state](../programs/README.md#the-cross-language-workloads) — they are properties of the programs, not of this harness.

What belongs here is the guarantee: `entrypoint.sh` cross-checks all eight outputs before timing anything, so a mistranslation surfaces as a failed run rather than a wrong number. Co-location never provided that, which is why the Curios spelling lives in the corpus with every other measured program.

## Run it

```
benchmarks/
  Dockerfile                 kitchen-sink arm64 image with all 8 toolchains + curios
  Dockerfile.dockerignore    what the build context excludes (target/, .artifacts/, .git)
  entrypoint.sh              build all, cross-check outputs, then 10 hyperfine tables
  NN_RESULTS.md              one single-sitting capture each, with its own commentary
  .artifacts/                built contestants — inside the container only; a run leaves nothing on the host

../programs/
  lcg/ trees/ chain/ churn/ spines/    the timed programs, one directory per workload
```

The image build needs the Curios sources *and* the corpus, both of which live _above_ `benchmarks/`, so it must run with the **repo root as the build context**. The `benchmarks` recipe does that, from the repo root:

```sh
cargo xtask benchmarks

# tune the workloads:
docker run --rm --cpuset-cpus 0 -e N_LCG=200000000 -e D_TREES=23 -e K_CHAIN=3200 -e N_CHURN=150000000 -e N_SPINES=150000 -e RUNS=7 curios-benchmarks
```

The run splits its two audiences across the two streams, which is how a `--rm` container hands back a document without a bind mount:

- **stdout** carries the ten markdown tables and nothing the harness adds to them, so `docker run … > run.md` is the whole capture, ready to paste into a results file. Through `cargo xtask benchmarks > run.md` the capture is the same: the recipe echoes its commands on stderr, not into the file.
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
