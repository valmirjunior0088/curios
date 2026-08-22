# Results — three columns fell together, and each was named before the run

One run of the harness in [README.md](README.md), captured 2026-08-22. Run 08 closed the `spines` wall and left the other four columns flat, ending on the observation that three of the five Lean ratios sat between 1.25× and 1.31× and none of them was about allocation. Two days later three columns move at once: **`chain` 133.4 → 69.5 ms (−47.9%), `spines` 54.4 → 24.4 ms (−55.1%), `trees` 115.5 → 100.6 ms (−12.9%)**, with `lcg` and `churn` inside ±1.7%.

The three that moved are exactly the three the compiler's own probes named in advance, and the two that did not are exactly the two those probes called flat. Curios is now **ahead of Lean on two workloads and ahead of Rust natively on one**, first in two of the ten tables, second in three, and last in one.

## How this was run

- **Source** — Curios compiler commit `51232992ce88`, version 0.10.1 (run 08 was `7f166fa87148`, also 0.10.1; 75 commits in between). The harness — `Dockerfile`, `entrypoint.sh` — and all five workloads' sources in every language are byte-identical to what run 08 timed; nothing in the interval touches `programs/`, and the only changes under `benchmarks/` are run 08's own results file and the README rows it appended.
- **Machine** — Apple Silicon (arm64), inside Docker Desktop's Linux VM, pinned to one core (`--cpuset-cpus 0`). Every contestant shares the same virtualized guest.
- **Engine** — and for the first time the two halves disagree. The `wasmtime` crate embedded in Curios's native executable is **47.0.3**, unchanged since run 04 and the engine under *both* Curios columns. The standalone `wasmtime` the wasm section runs its other three contestants under is **48.0.0**, up from 47.0.3 in runs 04–08, because the image installs the current release. See [the engine moved under half the wasm table](#the-engine-moved-under-half-the-wasm-table) — Curios's own rows are the engine-frozen ones this time.
- **Method** — hyperfine 1.20.0, 5 timed runs + 1 warmup per contestant, whole-process wall-clock (startup included). hyperfine flagged statistical outliers on Grain's `lcg` wasm row and its sub-5 ms calibration caveat on Rust `spines`; nothing else. One row it did not flag deserves a reader's caution anyway: Rust → wasm `chain` came in at 169.0 ± 18.8 ms over a 156.5–202.1 range, so quote that comparison by its range rather than its mean.
- **Workloads** — `lcg` at N = 100,000,000; `trees` at D = 21; `chain` at K = 1600; `churn` at N = 75,000,000; `spines` at N = 75,000. Every one is the harness default, which is [the corpus's documented size](../programs/README.md#the-cross-language-workloads) for each.
- **Correctness** — all eight implementations agreed at every cross-check input: `lcg(8) = 9345`, `trees(10) = 96122`, `chain(8) = 819185`, `churn(8) = 897441`, `spines(8) = 28`. Separately, Rust and Curios were checked against the corpus's five full-size anchors and reproduced all of them: `lcg(10⁸) = 17662`, `trees(21) = 536864`, `chain(1600) = 457407`, `churn(75000000) = 762495`, `spines(75000) = 675283`.
- **Toolchains** — rustc 1.98.0 (was 1.97.1), OCaml 5.2.0 (flambda, unchanged), Node v22.23.2 (unchanged), Lean 4.33.1 (was 4.33.0), Grain 0.7.2 (pinned, unchanged), AssemblyScript 0.28.20 (unchanged). This is the first capture since run 05 to carry any toolchain change at all — run 05's was Lean alone, and runs 06 through 08 carried none — and the first to carry three at once: the image's base layer moved, so every unpinned installer fetched its current release.

One thing remains worth repeating from every run so far: **Curios only targets wasm.** Its "native" row is a self-contained executable that embeds wasmtime and executes the same compiled module represented by the Curios wasm row. Those two numbers agreeing is a consistency check, not a contest between two Curios backends — and this is the closest they have ever agreed, within 0.6 ms on `lcg` and within 0.4 ms on the other four.

## Native targets

### `lcg` — integer ALU + counted loop (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 223.6 ± 0.2 ms | 1.00× |
| Lean 4 | 225.1 ± 0.4 ms | 1.01× |
| Node (V8) | 234.8 ± 0.5 ms | 1.05× |
| **Curios** | **293.6 ± 0.6 ms** | **1.31×** |
| OCaml (flambda) | 358.0 ± 0.2 ms | 1.60× |

### `trees` — allocation + heap traversal (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Lean 4 | 34.3 ± 0.4 ms | 1.00× |
| Rust | 93.6 ± 0.8 ms | 2.73× |
| **Curios** | **100.6 ± 0.6 ms** | **2.93×** |
| OCaml (flambda) | 104.4 ± 0.5 ms | 3.04× |
| Node (V8) | 197.9 ± 1.1 ms | 5.77× |

### `chain` — death-birth churn over a cons list (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| OCaml (flambda) | 46.0 ± 0.1 ms | 1.00× |
| Node (V8) | 57.4 ± 1.1 ms | 1.25× |
| **Curios** | **69.5 ± 0.5 ms** | **1.51×** |
| Lean 4 | 105.3 ± 1.1 ms | 2.29× |
| Rust | 113.1 ± 0.7 ms | 2.46× |

### `churn` — record update against the mutation floor (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 239.4 ± 0.4 ms | 1.00× |
| **Curios** | **268.8 ± 0.8 ms** | **1.12×** |
| Node (V8) | 292.6 ± 1.4 ms | 1.22× |
| Lean 4 | 321.9 ± 0.4 ms | 1.34× |
| OCaml (flambda) | 348.5 ± 0.8 ms | 1.46× |

### `spines` — map inserts under a plateaued live set (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust | 2.4 ± 0.0 ms | 1.00× |
| Node (V8) | 16.2 ± 0.5 ms | 6.69× |
| Lean 4 | 18.6 ± 0.5 ms | 7.70× |
| OCaml (flambda) | 22.6 ± 0.2 ms | 9.35× |
| **Curios** | **24.4 ± 0.4 ms** | **10.09×** |

## wasm on wasmtime

Curios's rows are its own executable at wasmtime 47.0.3; the other three run under the standalone wasmtime 48.0.0.

### `lcg` (N = 100,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 259.4 ± 1.4 ms | 1.00× |
| **Curios** | **293.0 ± 0.5 ms** | **1.13×** |
| AssemblyScript | 307.8 ± 0.6 ms | 1.19× |
| Grain | 29,700 ± 378 ms | 114.48× |

### `trees` (D = 21)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **100.9 ± 0.8 ms** | **1.00×** |
| Rust → wasm | 122.8 ± 1.0 ms | 1.22× |
| AssemblyScript | 213.1 ± 0.7 ms | 2.11× |
| Grain | 1,765 ± 6 ms | 17.49× |

### `chain` (K = 1600)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| **Curios** | **69.2 ± 0.5 ms** | **1.00×** |
| Rust → wasm | 169.0 ± 18.8 ms | 2.44× |
| AssemblyScript | 508.5 ± 2.5 ms | 7.35× |
| Grain | 4,660 ± 11 ms | 67.37× |

### `churn` (N = 75,000,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| Rust → wasm | 231.2 ± 1.9 ms | 1.00× |
| **Curios** | **268.4 ± 0.2 ms** | **1.16×** |
| AssemblyScript | 274.5 ± 1.1 ms | 1.19× |
| Grain | 58,283 ± 209 ms | 252.14× |

### `spines` (N = 75,000)

| Language | Mean | vs best |
| :--- | ---: | ---: |
| AssemblyScript | 13.3 ± 1.2 ms | 1.00× |
| Rust → wasm | 16.7 ± 1.9 ms | 1.26× |
| **Curios** | **24.5 ± 0.5 ms** | **1.85×** |
| Grain | 134.0 ± 0.6 ms | 10.10× |

## The three columns that moved, and what named them

Everything Curios's rows did in this interval is one campaign: the uniform-representation tax that `spines`' decomposition identified as all that was left of the map insert, taken up whole. It landed in four steps, each of which published which programs it expected to move *before* the harness ran, and the figures for each live beside the probe that retakes them in `curios/src/tests/codegen/`. A fifth thing happened in the interval and is deliberately absent from the table below, because its figure is not of the same kind: `/std/Map` learned to walk once per insert and to branch a two-way match, and the campaign that closed with those measured the insert *slope* at 1309 → 744 ns rather than any whole-process percentage. It applies to `spines` alone.

| Step | Decision | What its own probe measured, x86-64 in-process |
| :--- | :--- | :--- |
| Exact reads | [A tuple is read at its own final type](../documentation/design/toolchain/a-tuple-is-read-at-its-own-final-type.md) | `chain` −61.4%, `spines` −21.6%, against `lcg` +0.8%, `trees` +0.8%, `churn` +0.1% |
| Typed dispatch tables | [A closure carries its code as a table index](../documentation/design/toolchain/a-closure-carries-its-code-as-a-table-index.md) | `monad_io` −24.9%, `parse_digits` −15.4%; all five harness workloads inside ±1.7% |
| Family keying | [A field is declared at the carrier its shape names](../documentation/design/toolchain/a-field-is-declared-at-the-carrier-its-shape-names.md) | `chain` −9.3/−10.8%, `trees` −7.3/−7.6%, `spines` −7.1/−7.5%, against `lcg` and `churn` inside 1.5% |
| Typed slots | the same decision's second half | `spines` −7.7%, `trees` −7.2/−4.9%, `chain` −2.7/−2.8%, against `lcg`, `churn`, `state_monad` inside 1% |

Multiplying those three per-step figures — the middle two averaged over their two passes — gives a composite for each column, and the harness is the independent check on it: a different architecture, a different host, a virtualized guest, and whole-process timing against a different contestant field.

| Curios row | Composite of the tabled steps | Observed, native | Observed, wasm |
| :--- | ---: | ---: | ---: |
| `chain` | −66.2% | **−47.9%** | **−47.9%** |
| `spines` | −32.9% | **−55.1%** | **−54.5%** |
| `trees` | −12.4% | **−12.9%** | **−12.3%** |
| `churn` | −0.2% | −1.7% | −1.6% |
| `lcg` | +1.1% | +0.2% | −0.9% |

**Three columns land on the composite and two miss it in opposite directions, each for a reason stated before the arithmetic was done.** `trees` at −12.9% against −12.4%, and the two controls at −1.7% and +0.2% against −0.2% and +1.1%, are as close as two methods on two architectures have any right to be. `spines` beats its composite because that composite is a lower bound by construction — the map's own walk steps are the omitted fifth element above, and 1309 → 744 ns of insert slope is what fills the gap. `chain` delivers roughly two thirds of its composite, and nothing in the record explains the shortfall; what should be said instead is that nothing licensed the multiplication in the first place. The steps were measured on x86-64 Linux, where the `is_subtype` libcall this campaign deletes has its own cost, and percentage moves do not carry across a host, an architecture and a hypervisor.

So the composite is a direction with a magnitude attached, not a target. What the two methods agree on exactly is the *partition*: the four steps between them named `chain`, `spines` and `trees` as movers and `lcg` and `churn` as controls, and that is what the harness reproduces, in both tables, on hardware none of the steps was measured on.

The mechanism behind the partition is one sentence. A heap object's type is now keyed by the identity of what it holds — a variant family or a product schema — instead of by its arity, so a field read is one exact compare against a final type, and each slot is declared at the carrier its recorded shape names instead of uniformly `(ref null any)`, so a scalar payload is an `i32` where it was a boxed `i31` behind a cast. The columns that move are the columns whose hot loop reads a heap variant's typed payload: `chain`'s `link`, `Map/Node`'s `crit`, `trees`' node. `lcg` declares no variant at all, and `churn`'s record is erased into registers by the optimizer before any of this applies — which is why they were controls before the run rather than after it.

`spines` deserves one figure of its own, because it is the column this campaign was originally aimed at. Whole-process, startup included, it now averages **0.33 µs per insert** against Lean's 0.25 and Rust's 0.03. Run 07 read 2.8, run 08 read 0.73. The two confounds the corpus published the workload with are unchanged and still structural — a crit-bit trie against hash maps and balanced trees, and keys entering through `Bytes/of_nat` — and the second was priced in the interval at 14 ns against a 744 ns insert, under 2% (`map_wall_key_share`).

## The engine moved under half the wasm table

The image's base layer moved between captures, so every unpinned installer fetched its current release — which for the standalone wasmtime means **47.0.3 → 48.0.0**. Curios is unaffected: its executable embeds the `wasmtime` crate at the version `Cargo.lock` pins, still 47.0.3, and that same executable is what both Curios columns time. So this run inverts the usual reading — Curios's two rows are the engine-frozen ones and the other three wasm contestants are not.

Three rows moved by more than their native counterparts, all of them on wasm:

| Row | Run 08 | Run 09 | Change | The same contestant's native row |
| :--- | ---: | ---: | ---: | ---: |
| AssemblyScript, `churn` | 252.6 ms | 274.5 ms | **+8.7%** | — (no native AssemblyScript) |
| AssemblyScript, `spines` | 11.4 ms | 13.3 ms | **+16.7%** | — |
| Rust → wasm, `spines` | 15.8 ms | 16.7 ms | +5.7% | Rust native `spines`: 2.4 → 2.4 ms |

Only the first of those is beyond what this harness's noise explains. AssemblyScript's `churn` is a 275 ms figure with a ±1.1 ms spread in both captures, and the engine version is the only variable that changed under it. The two `spines` rows are 11–17 ms figures with ±1.2 and ±1.9 ms spreads, where whole-process timing spends a visible fraction on startup — the same instability run 08 recorded on that table. Everything else in the wasm section is within 2%.

The practical consequence: `churn` on wasm and `spines` on wasm should not be read across the 08/09 boundary for the AssemblyScript and Rust → wasm rows, and Curios's *position* in those two tables is partly a control's move rather than its own. Curios passing AssemblyScript on `churn` wasm is the clearest case — Curios improved 1.6% and AssemblyScript lost 8.7%, and the second number is the larger one.

## What the other controls did

The rest of the field is quiet, with one real exception. On `lcg` every contestant is within 0.6% of run 08. On `chain` and `churn` natively the widest move is OCaml's −2.2% on `churn`. On `spines` natively every non-Curios row is within 1.8%.

The exception is **Lean's `trees`, which fell 41.3 → 34.3 ms, −16.9%**, alongside its toolchain moving 4.33.0 → 4.33.1. No other Lean row moved by more than 1.5%, so this is a Lean improvement on the allocation workload rather than an environment shift, and it lands on the one table where Lean was already first by a wide margin. Its effect on this run's headline is to make Curios's ratio to Lean on `trees` *worse* — 2.80× in run 08, 2.93× here — while Curios's own figure improved 12.9%. That is the harness working: a ratio is two numbers.

Run 08's soft column is now three sittings old and unchanged. Across runs 06 through 09, Rust `trees` reads 82.2, 97.5, 94.3, 93.6 ms and OCaml 95.1, 109.7, 105.7, 104.4 ms, with sources and compiler flags identical throughout — and rustc itself moved 1.97.1 → 1.98.0 here without disturbing the level. Whatever shifted between runs 06 and 07 was in the environment, has held for three captures, and remains something this harness cannot name. `trees` comparisons across the 06/07 boundary stay untrustworthy for those rows; 07 through 09 are readable.

## Where Curios stands now

**Curios is first in two of the ten tables, second in three, and last in one.** The firsts are `trees` and `chain` on wasm, as in runs 07 and 08. The seconds are `lcg` on wasm, `churn` natively, and — new — `churn` on wasm. The single last is `spines` natively; `chain` natively left that position, moving from fifth to third.

Three orderings changed, and they are worth separating by how much of the change Curios owns:

- **`chain` natively, fifth to third.** Curios at 69.5 ms now sits ahead of Lean's 105.3 and Rust's 113.1, behind OCaml and Node. Curios moved −47.9% and both of the rows it passed moved less than 1.5%; this one is entirely Curios's.
- **`trees` natively, fourth to third.** Curios at 100.6 ms passes OCaml's 104.4 for the first time. Curios moved −12.9% against OCaml's −1.2%; also Curios's.
- **`churn` on wasm, third to second.** Curios at 268.4 ms passes AssemblyScript's 274.5. Curios moved −1.6% and AssemblyScript +8.7% under a changed engine; this one is mostly the control's, as the section above says.

Against **Perceus-compiled Lean 4**, the peer three of these workloads were specified against, the distances are now 0.66× on `chain` (ahead), 0.84× on `churn` (ahead), 1.30× on `lcg`, 1.31× on `spines`, and 2.93× on `trees`. In run 08 the same five read 1.25× on `chain`, 0.85× on `churn`, 1.30× on `lcg`, 2.91× on `spines` and 2.80× on `trees` — so `chain` crossed from behind to comfortably ahead, `spines` closed from 2.91× to 1.31×, `lcg` and `churn` held, and `trees` widened because Lean got faster. **The death-birth pair Perceus was the reference for is now the pair Curios leads**, which is the specific thing `chain` and `churn` were added to the harness to ask.

Against Rust natively: 0.61× on `chain` (ahead), 1.07× on `trees`, 1.12× on `churn`, 1.31× on `lcg`, and 10.17× on `spines`. Against Rust → wasm: 0.41× on `chain`, 0.82× on `trees`, 1.13× on `lcg`, 1.16× on `churn`, and 1.47× on `spines`. Two of those deserve a caveat rather than a cheer — the `chain` wasm figure divides by a row whose own range spans 156.5 to 202.1 ms, and `spines` on wasm divides by a 16.7 ± 1.9 ms row.

What this run leaves open is a shorter list than run 08's. `lcg` has not moved since run 05 and sits at 1.31× Rust natively, 1.13× Rust → wasm — it is the one column no representation work touches, because it allocates nothing and declares no variant, so whatever separates it from Rust is in the loop's own code. `trees` at 2.93× Lean is now the widest remaining ratio to that peer and is squarely about the collector: an all-live semi-space against Perceus, on the workload where everything survives. And `spines` at 10.17× Rust natively is still the crit-bit trie against a hash map, which the declined qp reshape priced and kept.

## Caveats

The full set is in [README.md](README.md#caveats--read-these-before-trusting-a-number). The load-bearing ones remain: one machine under a macOS → Linux VM, whole-process timing, idiomatic machine integers, and different memory-management strategies in the wasm table.

Three notes specific to this capture. **The wasm section's engine is no longer the engine Curios embeds** — 48.0.0 against 47.0.3 — so that half of the table compares across a version boundary this run did not choose; the section above names the three rows where it shows. **Three toolchains moved**, ending the run 06–08 stretch in which none did, and Lean's `trees` shows it. And **the composed prediction above is not a measurement** — it multiplies percentage moves taken on another architecture, and is recorded because the columns it named were right, not because its magnitudes were.
