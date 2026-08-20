# Results — the wall fell by three quarters, and only the wall

One run of the harness in [README.md](README.md), captured 2026-08-20. Run 07 ended on a column it could not explain: `spines` at 206.7 ms, 81× off Rust, behind Grain, with the collector measured absent from the insert loop. Two days later the same workload reads **54.4 ms natively and 53.9 ms on wasm** — 3.8× faster in both tables, 22.6× off Rust instead of 81×, 2.9× off Lean instead of 10.6×, and ahead of Grain by 2.5× where it had been behind.

The other four workloads did not move. Eight Curios rows sit within 1.7% of their run 07 figures, inside the band the controls drifted by, and every toolchain is run 07's version to the patch. That is the cleanest attribution this harness can offer: one interval, one campaign aimed at one column, and exactly that column moved.

## How this was run

- **Source** — Curios compiler commit `7f166fa87148`, version 0.10.1 (run 07 was `8db87da9012a`, version 0.10.0). The harness — `Dockerfile`, `entrypoint.sh` — and all five workloads' sources in every language are byte-identical to what run 07 timed; nothing in the interval touches `benchmarks/` or `programs/`.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 47.0.3, Cranelift, unchanged from runs 04–07. The `wasmtime` crate embedded in Curios's native executable is also 47.0.3, as in run 07.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included). hyperfine flagged statistical outliers on two rows of the `lcg` wasm table (Curios at ±4.4 ms, AssemblyScript at ±4.9 ms), a slow first run on Rust → wasm `chain` (173.9 ms against a 170.4 mean), and its sub-5 ms calibration caveat on Rust `spines`; nothing else.
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21; `chain` at K = 1600; `churn` at N = 75,000,000; `spines` at N = 75,000. Every one is the harness default, which is [the corpus's documented size](../programs/README.md#the-cross-language-workloads) for each.
- **Correctness** — all eight implementations agreed at every cross-check input: `lcg(8) = 9345`, `trees(10) = 96122`, `chain(8) = 819185`, `churn(8) = 897441`, `spines(8) = 28`. Separately, Rust and Curios were checked against the corpus's five full-size anchors and reproduced all of them: `lcg(10⁸) = 17662`, `trees(21) = 536864`, `chain(1600) = 457407`, `churn(75000000) = 762495`, `spines(75000) = 675283`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.2, Lean 4.33.0, Grain 0.7.2, AssemblyScript 0.28.20. **Every one is run 07's version**, as run 07's were run 06's and run 06's were run 05's.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends — and across all five workloads they agree to within 2.8 ms, the largest gap once again being `lcg`'s; the other four are within 0.7 ms.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 223.7 ± 0.2 ms | 1.00× |
| Lean 4 | 225.2 ± 0.4 ms | 1.01× |
| Node (V8) | 236.2 ± 2.1 ms | 1.06× |
| **Curios** | **292.9 ± 0.3 ms** | **1.31×** |
| OCaml (flambda) | 358.0 ± 0.4 ms | 1.60× |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 41.3 ± 0.4 ms | 1.00× |
| Rust | 94.3 ± 0.4 ms | 2.28× |
| OCaml (flambda) | 105.7 ± 0.6 ms | 2.56× |
| **Curios** | **115.5 ± 0.8 ms** | **2.80×** |
| Node (V8) | 201.7 ± 2.4 ms | 4.89× |

### `chain` — death-birth churn over a cons list (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| OCaml (flambda) | 46.4 ± 0.4 ms | 1.00× |
| Node (V8) | 56.8 ± 0.7 ms | 1.23× |
| Lean 4 | 106.9 ± 0.3 ms | 2.30× |
| Rust | 113.5 ± 0.9 ms | 2.45× |
| **Curios** | **133.4 ± 2.1 ms** | **2.88×** |

### `churn` — record update against the mutation floor (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 240.5 ± 0.3 ms | 1.00× |
| **Curios** | **273.4 ± 1.5 ms** | **1.14×** |
| Node (V8) | 295.8 ± 1.7 ms | 1.23× |
| Lean 4 | 323.3 ± 0.7 ms | 1.34× |
| OCaml (flambda) | 356.2 ± 1.1 ms | 1.48× |

### `spines` — map inserts under a plateaued live set (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 2.4 ± 0.0 ms | 1.00× |
| Node (V8) | 16.5 ± 0.9 ms | 6.87× |
| Lean 4 | 18.7 ± 0.1 ms | 7.79× |
| OCaml (flambda) | 22.8 ± 0.2 ms | 9.50× |
| **Curios** | **54.4 ± 0.3 ms** | **22.62×** |

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 258.2 ± 0.1 ms | 1.00× |
| **Curios** | **295.7 ± 4.4 ms** | **1.15×** |
| AssemblyScript | 310.3 ± 4.9 ms | 1.20× |
| Grain | 29,578 ± 21 ms | 114.56× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **115.1 ± 0.7 ms** | **1.00×** |
| Rust → wasm | 121.7 ± 0.7 ms | 1.06× |
| AssemblyScript | 217.4 ± 0.4 ms | 1.89× |
| Grain | 1,773 ± 4 ms | 15.40× |

### `chain` (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **132.9 ± 1.0 ms** | **1.00×** |
| Rust → wasm | 170.4 ± 2.0 ms | 1.28× |
| AssemblyScript | 502.5 ± 0.9 ms | 3.78× |
| Grain | 4,635 ± 19 ms | 34.88× |

### `churn` (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 234.5 ± 0.6 ms | 1.00× |
| AssemblyScript | 252.6 ± 0.8 ms | 1.08× |
| **Curios** | **272.7 ± 0.6 ms** | **1.16×** |
| Grain | 58,765 ± 62 ms | 250.64× |

### `spines` (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| AssemblyScript | 11.4 ± 1.1 ms | 1.00× |
| Rust → wasm | 15.8 ± 0.4 ms | 1.38× |
| **Curios** | **53.9 ± 0.6 ms** | **4.71×** |
| Grain | 134.5 ± 0.4 ms | 11.76× |

## `spines` — 206.7 ms to 54.4 ms, and what did it

Run 07 ended its `spines` section by saying the collector was not the wall and that decomposing the remaining 2.2 collection-free microseconds per insert was work for a probe. That probe ran, and the campaign it drove is recorded in [The map wall falls by classes, not by symptom](../documentation/design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md), with every figure and its method in `map_wall_spines_slope` in `curios/src/tests/codegen/map_wall.rs`. The decomposition named three compiler classes — per-element read dispatch, scatter-then-gather construction, and boxed small packed values — and fixed them in that order: the leaf split and in-helper cache probe on the read protocol, flat-at-birth construction (`BinChunk`, and the census-admitted `ListSettle`/`ListFlat`), and small-canonical `Bytes` and `Bits` on the i31. A fourth step, reshaping `/std/Map` from a crit-bit trie to a qp-trie, was implemented in full, measured at 1802 ns/insert against the 1353 of the trie it would replace, and declined. The map is still a crit-bit trie; the library did not change shape.

The probe's own verdict is that the win was carried almost entirely by the third class. `spines` keys are `75·x mod 65537`, so every one is at most three bytes — inside the envelope where a `Bytes` value is now an i31 rather than a heap rope — and the bit test the trie descends by became a few register operations on an immediate instead of a dependent load through a node. Steps 1 and 2 each read inside the probe's noise on this workload, and the probe says why: the read protocol's *call* component was never the wall, and the ~60% read share the decomposition had measured was carried by the loads those calls sat beside. The register key deletes the loads.

What the harness adds is the whole-process figure, on arm64, against the other seven languages. Three figures are now on record for the same landing, and they are three methods, not three measurements of one quantity: the probe's insert *slope* between N = 25,000 and 75,000 fell 6.1× (8320 → 1353 ns); its in-process N = 75,000 total fell 4.8× (463 → 96 ms, on an x86-64 box); and this harness's whole-process total fell 3.8× (206.7 → 54.4 ms). They read in the order their fixed share predicts — whatever a method does not subtract, it dilutes — but this file does not decompose the 54 ms, and the harness cannot. At 75,000 inserts it averages about 0.73 µs per insert, startup included, against Lean's 0.25 and Rust's 0.03; run 07's figure was 2.8.

Two confounds the corpus published the workload with are still in force, and the run says something about each. `/std/Map` is still a crit-bit trie being compared against hash maps and balanced trees, and the reshape that would have changed that was declined on its own figure, so the algorithmic confound is now a measured choice rather than a suspicion. And every key still enters through `Bytes/of_nat` — the `Key(Nat)` witness is still unprovable — but the boundary's product is now an immediate, so the encoding is a division and a table index per byte *into a register*, and the 2.8 µs it was once offered as a partial explanation for is gone without it having changed.

## The other four columns did not move

| Curios row | Run 07 | Run 08 | 08 vs 07 |
| :--- | ---: | ---: | ---: |
| `lcg`, native targets | 297.8 ms | 292.9 ms | −1.6% |
| `lcg`, wasm on wasmtime | 295.1 ms | 295.7 ms | +0.2% |
| `trees`, native targets | 117.5 ms | 115.5 ms | −1.7% |
| `trees`, wasm on wasmtime | 115.5 ms | 115.1 ms | −0.3% |
| `chain`, native targets | 134.4 ms | 133.4 ms | −0.7% |
| `chain`, wasm on wasmtime | 134.8 ms | 132.9 ms | −1.4% |
| `churn`, native targets | 274.9 ms | 273.4 ms | −0.5% |
| `churn`, wasm on wasmtime | 273.8 ms | 272.7 ms | −0.4% |
| `spines`, native targets | 206.7 ms | 54.4 ms | −73.7% |
| `spines`, wasm on wasmtime | 207.0 ms | 53.9 ms | −74.0% |

The controls on those eight rows moved by the same amounts or more. On `lcg` every contestant is within 0.8% of run 07. On `chain` and `churn` the widest move is AssemblyScript's −2.1% on `churn` wasm; the rest sit within ±1.7%. On `trees` the whole native table read 1.9–3.6% faster than run 07 — Lean −1.9%, Rust −3.3%, OCaml −3.6%, Node −3.6% — and Curios's −1.7% is the smallest move in it. None of these eight Curios deltas is a result, and none was expected to be: the classes the campaign landed are shared by every consumer of a sequence, and these four workloads do not touch one past reading their input. That they read flat is the cheapest available evidence that small-canonical normalization and the census-gated flat builds cost nothing where they do not apply.

On `spines` the controls moved more than elsewhere — Rust −7.7%, Node −6.3%, Lean −4.1% natively, AssemblyScript −13.0% on wasm — which is what 2–20 ms figures do under whole-process timing, where shell calibration and startup are a visible fraction; Rust's row is below hyperfine's own 5 ms calibration floor. Curios's −74% is an order of magnitude beyond the widest of them.

Run 07 flagged `trees` as its one soft column: Rust and OCaml had read 15–19% slower than in run 06, for no reason the run could name. That shift did not revert. Across runs 06, 07 and 08, Rust `trees` reads 82.2, 97.5, 94.3 ms and OCaml 95.1, 109.7, 105.7 ms, with sources, compiler flags and toolchain versions identical throughout; Rust → wasm follows the same shape at 116.7, 124.8, 121.7 ms, while Lean, Node, Grain and AssemblyScript held level across all three. Whatever moved between runs 06 and 07 was in the environment, not the contestants, and it has now held for two sittings. The practical reading is that `trees` comparisons across the 06/07 boundary are untrustworthy for those three rows and trustworthy between 07 and 08; the cause is not something this harness can name.

That settles one sign run 07 left open. Curios leads Rust → wasm on `trees` by 1.06× here, 1.08× in run 07, with non-overlapping ranges both times; run 06 had the order reversed at 1.04×. Two consecutive sittings now agree, but the margin — 6.6 ms — is still of the order of the 3.1 ms Rust → wasm drifted between them, so it remains a near-tie whose sign has held for two sittings rather than a settled ordering.

## Where Curios stands now

**Curios is first in two of the ten tables, second in two more, and last in two.** The same two firsts and seconds as run 07 — `trees` and `chain` on wasm, `churn` native and `lcg` on wasm — and one fewer last: `spines` on wasm moved from fourth to third, ahead of Grain by 2.5×, and only `chain` native and `spines` native remain at the bottom of their tables.

Against **Perceus-compiled Lean 4**, the peer three of these workloads were specified against, the distances are now 0.85× on `churn` (ahead), 1.25× on `chain`, 1.30× on `lcg`, 2.80× on `trees`, and 2.91× on `spines`. Run 07's list was the same through the first four and ended at 10.6×. The order of magnitude that made `spines` the one column unlike the others is gone; what is left is a distance of the same kind the other four show — the largest of them, but the same kind.

Against Rust natively: 1.14× on `churn`, 1.18× on `chain`, 1.22× on `trees`, 1.31× on `lcg`, and 22.6× on `spines`. Against Rust → wasm: 0.78× on `chain`, 0.95× on `trees`, 1.15× on `lcg`, 1.16× on `churn`, and 3.41× on `spines`. `spines` is still the outlier in both lists, and the gap to Rust there is an imperative hash map against a persistent trie — the confound the corpus says it was published with, which the declined reshape priced and kept.

The thing to take from this run is that the question run 07 opened closed the way it was supposed to. A wall that had been filed under GC strategy was probed, found to be elsewhere, decomposed into classes every consumer shares, and closed by fixing those classes with the library's shape untouched — and the harness confirms the landing at the whole-process level with every other column held still. What this harness can next ask is the question that sits behind every remaining Lean ratio at once: three of the five are between 1.25× and 1.31×, and none of them is about allocation.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table.

Three notes specific to this capture. **`spines` now has a second figure**, so the three workloads that debuted in run 07 have each repeated across days: `chain` within 1.4%, `churn` within 0.5%, and `spines` after a deliberate 3.8× — the first evidence that all three are stable enough to read across runs. **`trees`'s controls carry a level shift from run 06** that has not reverted and is not explained; read that column across the 06/07 boundary with that in mind, and not at all for attribution. **`spines`'s two confounds are still structural** — a crit-bit trie against hash maps and balanced trees, and keys through `Bytes/of_nat` — but the second now produces an immediate, and the first is a measured decision rather than an open question.
