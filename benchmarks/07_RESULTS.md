# Results — three columns arrived, and one of them is a wall

One run of the harness in [README.md](README.md), captured 2026-08-18. The first capture to time all five workloads: `chain`, `churn` and `spines` were specified, landed and pinned by probes over the preceding weeks, but no run of this harness had ever placed them against the other seven languages. Ten tables instead of four.

The two columns every prior run argued about did not move. What the three new ones do is split the verdict three ways, and they do not agree with each other: `churn` puts Curios 1.13× off the imperative mutation floor and ahead of Perceus-compiled Lean, `chain` puts it first in its wasm table and 1.25× behind that same Perceus column natively, and `spines` puts it 81× off Rust and behind *Grain* — the worst relative position Curios has held in any table this harness has produced.

That spread is the run's actual result. Two captures ago the framing was "an integer column and an allocation column"; run 06 retired it by moving both. Three memory-shaped workloads now disagree by two orders of magnitude about how far off Curios is, which means "how does Curios do on allocation" was never one question.

## How this was run

- **Source** — Curios compiler commit `8db87da9012a`, version 0.10.0 (run 06 was `74987fc975c2`, version 0.9.3). The two harness fixes this run needed are committed alongside this file and touch no compiler crate; they are described below.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine (wasm section)** — wasmtime 47.0.3, Cranelift, unchanged from runs 04–06. **The `wasmtime` crate embedded in Curios's native executable moved 46.0.1 → 47.0.3**, so for the first time both Curios rows run the same engine version as each other and as the rest of the wasm section.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included). hyperfine flagged no statistical outliers in any of the forty rows.
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21; `chain` at K = 1600; `churn` at N = 75,000,000; `spines` at N = 75,000. Every one is the harness default, which is [the corpus's documented size](../programs/README.md#the-cross-language-workloads) for each.
- **Correctness** — all eight implementations agreed at every cross-check input: `lcg(8) = 9345`, `trees(10) = 96122`, `chain(8) = 819185`, `churn(8) = 897441`, `spines(8) = 28`. Separately, Rust and Curios were checked against the corpus's five full-size anchors and reproduced all of them: `lcg(10⁸) = 17662`, `trees(21) = 536864`, `chain(1600) = 457407`, `churn(75000000) = 762495`, `spines(75000) = 675283`.
- **Toolchains** — rustc 1.97.1, OCaml 5.2.0 (flambda), Node v22.23.2, Lean 4.33.0, Grain 0.7.2, AssemblyScript 0.28.20. **Every one is run 06's version**, as run 06's were run 05's.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends — and across all five workloads they now agree to within 2.7 ms, the largest gap being `lcg`'s.

### Two things had to be fixed before this run could be trusted

Both are in the harness and its spellings, not the compiler, and neither had ever been exercised before because no run had timed these workloads.

`programs/spines/Spines.lean` named `Std.TreeMap` with no import. Lean 4.33 auto-imports only `Init`, so `lake build` failed and no `spines` executable was produced. The file's own `VERIFY:` comment had flagged the `Std.TreeMap` surface as needing a check against the toolchain; `insert` and `foldl` turned out to be right and only the `import Std.Data.TreeMap` was missing.

`entrypoint.sh` then published a *partial* table rather than none. hyperfine exports the rows it finished before aborting, so the missing Lean binary produced a three-row `spines` native table on stdout — Lean and Curios both absent, with nothing in the document saying so. `table()` now requires hyperfine's exit status as well as a non-empty export, which is what makes README's "a table hyperfine fails to produce is absent from stdout" true rather than aspirational. A short table that looks finished is worse than a missing one.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 223.5 ± 0.1 ms | 1.00× |
| Lean 4 | 225.6 ± 0.8 ms | 1.01× |
| Node (V8) | 235.5 ± 1.1 ms | 1.05× |
| **Curios** | **297.8 ± 2.4 ms** | **1.33×** |
| OCaml (flambda) | 358.5 ± 0.6 ms | 1.60× |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 42.1 ± 0.3 ms | 1.00× |
| Rust | 97.5 ± 0.5 ms | 2.32× |
| OCaml (flambda) | 109.7 ± 0.5 ms | 2.61× |
| **Curios** | **117.5 ± 2.3 ms** | **2.79×** |
| Node (V8) | 209.2 ± 2.1 ms | 4.97× |

### `chain` — death-birth churn over a cons list (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| OCaml (flambda) | 46.0 ± 0.2 ms | 1.00× |
| Node (V8) | 57.8 ± 0.5 ms | 1.26× |
| Lean 4 | 107.4 ± 0.5 ms | 2.33× |
| Rust | 113.2 ± 0.5 ms | 2.46× |
| **Curios** | **134.4 ± 1.1 ms** | **2.92×** |

### `churn` — record update against the mutation floor (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 243.6 ± 1.3 ms | 1.00× |
| **Curios** | **274.9 ± 2.1 ms** | **1.13×** |
| Node (V8) | 298.6 ± 0.4 ms | 1.23× |
| Lean 4 | 325.4 ± 0.8 ms | 1.34× |
| OCaml (flambda) | 359.3 ± 1.6 ms | 1.48× |

### `spines` — map inserts under a plateaued live set (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 2.6 ± 0.1 ms | 1.00× |
| Node (V8) | 17.6 ± 1.0 ms | 6.90× |
| Lean 4 | 19.5 ± 0.2 ms | 7.66× |
| OCaml (flambda) | 23.2 ± 0.2 ms | 9.09× |
| **Curios** | **206.7 ± 1.0 ms** | **81.05×** |

## wasm on wasmtime

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 258.2 ± 0.4 ms | 1.00× |
| **Curios** | **295.1 ± 3.5 ms** | **1.14×** |
| AssemblyScript | 307.8 ± 0.1 ms | 1.19× |
| Grain | 29,461 ± 130 ms | 114.10× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **115.5 ± 1.0 ms** | **1.00×** |
| Rust → wasm | 124.8 ± 0.9 ms | 1.08× |
| AssemblyScript | 221.3 ± 1.1 ms | 1.92× |
| Grain | 1,772 ± 4 ms | 15.35× |

### `chain` (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **134.8 ± 1.3 ms** | **1.00×** |
| Rust → wasm | 168.3 ± 1.2 ms | 1.25× |
| AssemblyScript | 505.4 ± 1.8 ms | 3.75× |
| Grain | 4,646 ± 5 ms | 34.47× |

### `churn` (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 234.6 ± 0.9 ms | 1.00× |
| AssemblyScript | 258.0 ± 2.3 ms | 1.10× |
| **Curios** | **273.8 ± 0.4 ms** | **1.17×** |
| Grain | 59,008 ± 42 ms | 251.57× |

### `spines` (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| AssemblyScript | 13.1 ± 0.4 ms | 1.00× |
| Rust → wasm | 16.2 ± 0.5 ms | 1.24× |
| Grain | 135.8 ± 0.4 ms | 10.36× |
| **Curios** | **207.0 ± 1.0 ms** | **15.79×** |

## `churn` — 1.13× off the mutation floor, and ahead of Perceus

Curios is second in the native table, behind Rust and ahead of Node, Lean and OCaml. That ordering is the erasure the workload was specified to expose, and it is already pinned structurally: `threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs` asserts that a million spread-update steps reach the collector zero times, because continuation scalar replacement and the known-function field split erase the record the source spells. The threaded record travels as fields, so this column never allocates.

That is why the comparison lands where it does in both directions. Rust mutates a struct in place and allocates nothing, so its 243.6 ms is the mutation floor — and Curios's 1.13× over it is what checked i31 arithmetic and dispatch cost, with allocation contributing nothing on either side. OCaml and Grain allocate a fresh six-field record per step and pay for it. Lean's `{ r with … }` is the shape Perceus rewrites in place, so it should also be near the floor, and at 1.34× it is *behind Curios* — reference counting is not free even when it succeeds in reusing.

The record-update tax this workload was originally specified to price therefore did not appear here at all, which the specification anticipated: it lives where a record *rests*, not where one is threaded. `spines` is where it shows up.

## `chain` — first in its wasm table, 1.25× behind Perceus in the other

Two figures, and the workload's two halves put Curios on opposite sides of them.

**Against Rust → wasm, Curios is 1.25× faster** (134.8 vs 168.3 ms), with non-overlapping ranges. This is the first table in this harness where Curios leads Rust by a margin that is not a near-tie, and the mechanism is not a compiler win — it is the allocator comparison the README's caveat says the wasm table makes. Rust's spelling is `Option<Box<Node>>`, so it pays a malloc and a free per cell for 16 million cells that die immediately; Curios bump-allocates in a nursery the sizing decision now pre-grows and pays a young collection instead. On a workload where nothing survives, a collector beats a general-purpose allocator, and that also explains why Rust is not the ceiling of the native table either — OCaml's minor heap wins it at 46.0 ms and Node's generational GC takes second, both ahead of Rust.

**Against Lean 4, Curios is 1.25× slower** (134.4 vs 107.4 ms). This is the number the workload exists for: `chain` was specified to price dynamic reuse, and Lean is the column where a dying cell becomes an in-place write. Reuse is worth 1.25× here — real, and much less than the framing feared. Two asymmetries belong beside that ratio rather than inside it, both already recorded by the corpus: a Curios cons cell is three slots to Lean's two because a tagged constructor carries its discriminant, and the chain's live set is small by design, so what is measured is the allocation rate and young-collection frequency rather than the marking half.

Nothing in run 06's win reaches this workload. That encoding collapses a variant family with exactly one *immediate-unary* constructor; `chain.crs` declares `induct Chain | nil() | cons(Nat, Chain) end`, whose non-recursive constructor is nullary rather than unary, so there is no immediate payload to ride. The specified-but-unlanded successor, [A variant travels as the fields of its widest constructor](../documentation/design/toolchain/a-variant-travels-as-the-fields-of-its-widest-constructor.md), is the one aimed at `cons`'s boxed fields.

## `spines` — the wall, and it is not the collector

Curios is last in both tables, 81× off Rust and 10.6× off Lean natively, and in the wasm table it is beaten by Grain — a toolchain 114× off Rust on `lcg`. At 206.7 ms for 75,000 inserts that is about 2.8 µs per insert against Lean's 0.26 and Rust's 0.035.

The temptation is to file this under GC strategy, as `trees` legitimately is. **A probe already measured that it is not.** `spines_collection_decomposition` in `curios/src/tests/codegen/churn.rs`, retaken under the sixteen-mebibyte sizing decision on wasmtime 47.0.3, records 0.1 collections per 1000 inserts and no heap growth for the stock arrangement — collection is effectively absent from the insert loop — and pre-growing the heap with a 4M-cell ballast moves the per-insert cost only from 2.70 µs to 2.17 µs, a fifth at most, part of which the probe attributes to first-touch rather than collection. Whatever the remaining factor of eight against Lean is, the collector is not it. (The two measurements are read against each other only for that ratio; the probe's absolute per-insert figures come from a different in-process method and are not offered as corroboration of the harness's whole-process ones.)

What is left is the insert path itself, and the corpus names two confounds it was published with: `/std/Map` is a crit-bit trie being compared against imperative hash maps and balanced trees, and it deliberately has no `Key(Nat)` — the injectivity a `Key` witness owes is unprovable for a division-based encoding under unary elimination — so every key enters through `Bytes/of_nat`, a division and a table index per byte. Those were offered as reasons the table orients rather than proves. They were not offered as an explanation for an order of magnitude, and this run does not establish that they are one. Decomposing 2.2 collection-free microseconds per insert is work for the probe, not for this file.

The honest summary is that `spines` found something the other four workloads could not, which is why it was specified: a record at rest, reached through a boundary encoding, walking a pointer-scattered trie. Every other column in this harness measures a loop.

## The two old columns did not move

| Curios row | Run 00 | Run 01 | Run 02 | Run 03 | Run 04 | Run 05 | Run 06 | Run 07 | 07 vs 00 | 07 vs 06 |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lcg`, native targets | 446.1 ms | 454.6 ms | 438.5 ms | 437.3 ms | 435.1 ms | 296.3 ms | 293.1 ms | 297.8 ms | −33.2% | +1.6% |
| `lcg`, wasm on wasmtime | 444.6 ms | 453.0 ms | 438.2 ms | 438.4 ms | 435.7 ms | 296.7 ms | 293.2 ms | 295.1 ms | −33.6% | +0.6% |
| `trees`, native targets | 314.9 ms | 462.2 ms | 251.5 ms | 256.4 ms | 241.9 ms | 245.9 ms | 121.1 ms | 117.5 ms | −62.7% | −3.0% |
| `trees`, wasm on wasmtime | 322.2 ms | 456.2 ms | 246.1 ms | 260.0 ms | 244.5 ms | 249.6 ms | 121.4 ms | 115.5 ms | −64.2% | −4.9% |

On `lcg` the controls are the flattest this harness has recorded: Rust ±0.0%, Lean −0.1%, OCaml −0.1%, Node +0.2%, Rust → wasm −0.6%, with AssemblyScript's −3.6% the only exception. Curios's +1.6% and +0.6% sit at or just outside that band and are not a result; nothing in the interval was aimed at an integer loop.

On `trees` the controls did **not** hold still, and the pattern is odd enough to disqualify a cross-run reading: Rust +18.6% and OCaml +15.4% natively, Rust → wasm +6.9%, while Lean +1.7%, Node −1.8%, Grain +0.9% and AssemblyScript −3.2% barely moved. Curios's −3.0% and −4.9% are inside the spread those four define and well inside the drift the other three show. **`trees` did not move this run**; three of its seven contestants read materially slower than they did yesterday, and no attribution should be drawn from Curios's position relative to them across runs.

That matters for one nominal headline. Curios is first in the `trees` wasm table, 1.08× ahead of Rust → wasm with non-overlapping ranges — a real separation *within this sitting*. But Rust → wasm's +6.9% between-run drift is comparable to the 9.3 ms gap, and the ordering has flipped since run 06, which had Rust → wasm ahead by 1.04×. Treat `trees` on wasm as a near-tie whose sign is not yet stable, which is the same reading run 06 asked for from the other side of it.

## Where Curios stands now

**Curios is first in two of the ten tables, second in two more, and last in three.** The single-number summary the harness exists to produce no longer exists, because the five workloads disagree: 1.13× off Rust on record threading, 1.25× ahead of Rust → wasm on death-birth churn, 1.08× ahead of it on allocation-and-traversal, 1.14× behind it on integer loops, and 12.8× behind it on map inserts.

Read as distances to the peer that means the most, the picture is sharper than any prior run's. Against **Perceus-compiled Lean 4** — the dependently-typed peer with a real backend, and the reuse column three of these workloads were specified to price — Curios is 0.84× on `churn` (ahead), 1.25× on `chain`, 2.79× on `trees`, 1.32× on `lcg`, and 10.6× on `spines`. Reuse is worth about a quarter on a cons list and nothing at all on a threaded record; the order of magnitude is somewhere else entirely.

The thing to take from this run is which question is now open. Run 06 closed "why is allocation stuck" by removing half of `trees`'s objects. `chain` and `churn` say the collector and the record path are in reasonable shape — a young collection beats malloc/free on dying cells, and a threaded record costs nothing. `spines` says that a record reached through a key encoding and a pointer-scattered trie costs 2.8 µs per touch with the collector barely running, and that this is the only workload here that does not measure a loop. That is not a GC question and not a codegen-of-arithmetic question, and it is the first time this harness has produced one of those.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table.

Three notes specific to this capture. **Three workloads are debuting**, so `chain`, `churn` and `spines` have no prior figure to be compared against, and nothing yet establishes that any of them repeats across days — the next run is the first evidence of that. **`trees` is the one soft column**, for the reason the section above gives: three of its seven contestants read 7–19% slower than they did in run 06, so its cross-run comparison is not trustworthy at better than about 15% for those rows, and the 1.08× wasm lead is inside that. **`spines`' two confounds are structural, not incidental**: it compares a crit-bit trie against hash maps and balanced trees, and it routes every key through `Bytes/of_nat`, so it orients rather than proves — the 81× is a measured fact and its decomposition is not in this file.
