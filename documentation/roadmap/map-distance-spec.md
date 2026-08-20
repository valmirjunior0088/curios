# The map's remaining distance falls by a walk, a cast and a branch

## Status

**Landed 2026-08-20, and re-scoped around what the figures found.** All three steps landed, plus a soundness fix the investigation tripped over; every prediction the specification made about *which* classes mattered held, and the one it was wrong about it had **under**stated by a factor of two and a half. What is open now is not any of them — it is the ~744 ns per insert that remains, which no session has decomposed, and the field-representation question the cast step's measurement turned from a hypothesis into a priced one.

The steps as they landed, with what each was worth. Figures are the insert slope between N = 25 000 and 75 000, seven readings per variant, x86-64 Linux, release, all taken in one sitting so nothing is compared across machine states; medians and minima are given together because the box was reading bimodally and agreement between the two is what makes a delta reportable. Every figure's method lives beside the probe that reproduces it, in `map_wall_spines_slope` and `a_tuple_is_read_at_its_own_final_type`.

| | median | min | step delta (min) |
| --- | --- | --- | --- |
| before | 1309 | 1267 | — |
| a walk — one descent per insert and remove | 951 | 924 | **−27%** |
| a branch — a two-way match is an `if` | 932 | 878 | **−5%** |
| a cast — a tuple is read at its own final type | 757 | 744 | **−15%** |

**Cumulative −41%**, and the campaign's objective is met: `spines` was the one harness column outside the 0.85×–2.80× band the other four occupy, at 2.91× off Lean, and the walk alone was enough to bring it inside.

## What the figures corrected

**The cast step was understated, not overstated.** The specification priced it at roughly 60 ns of libcalls against a ~470 ns insert — about 13% — and a read-only session then argued from *static* counts that it was worth less still and should be dropped as a stopgap: 10 `$tuple/1` tag-read casts against 338 `anyref` box/unbox sites in optimized `spines`. That reasoning was wrong, and the probe built to settle it is what caught the error. Whole-process, min of 5, every anchor checked on every run:

| workload | before | after | delta |
| --- | --- | --- | --- |
| `lcg` | 318.6 ms | 321.2 ms | +0.8% |
| `trees` | 351.3 ms | 354.1 ms | +0.8% |
| `churn` | 350.3 ms | 350.6 ms | +0.1% |
| `chain` | 339.6 ms | 131.1 ms | **−61.4%** |
| `spines` | 100.5 ms | 78.8 ms | **−21.6%** |

The static count answered the wrong question. It counted *sites*, and the cost is per *execution*: `chain` rebuilds and walks 16 million cons cells, each paying two non-exact prefix casts — the tag read and one payload read — and each of those was a host call. Ten sites in a loop outweigh three hundred outside one. **A static census cannot price a dynamic class, and this is the record of it failing to.**

The three flat columns are the evidence that this is a class rather than a coincidence, and they were flat for reasons stated *before* the measurement: `lcg` declares no variant, `trees`' leaf rides the i31 so its family never reads a tag and its one boxed constructor casts exactly, and `churn`'s hot loop is not a variant walk. Prediction and split agreeing is what makes the two moving columns credible.

**The `likely` hint was never needed.** The specification had `TupleGet` carry the widths its producer expects, so the emitter could order *and prune* the cascade — an IR change costing `CpsIntrinsic` its `Copy`, `Ord` and `Hash` across some fifty sites, and silently breaking CSE between two reads differing only in their hint. Exhausting the roster instead assumes nothing, and the roster is 2 to 5 across the whole corpus. The figures above are the exhaustive version's. See [a tuple is read at its own final type](../design/toolchain/a-tuple-is-read-at-its-own-final-type.md).

**A cross-language claim is not yet on record.** `chain` at −61% is measured on one box against itself. The harness's run 08 has Curios 1.25× behind Lean on that column; whether this moves it ahead needs a run 09, which this campaign did not capture.

## What is open

**The remaining ~744 ns per insert has never been decomposed.** The original specification accounted for about 85 ns of it — the libcalls and the jump tables — and disposed of the rest in one clause naming "the record's parked classes". That clause is still the whole of what is known. The candidates, unranked because nothing has measured them against each other:

- the `anyref` boundary — 338 box/unbox sites in optimized `spines`, untouched by any step here;
- per-insert key construction, `Bytes/of_nat`, which `programs/README.md` flags as a workload confound rather than a map cost;
- the rebuild's allocation and the collection it drives, measurable with the log-bridge machinery `curios/src/tests/codegen/churn.rs` already uses;
- the crit scan and the descent's remaining frames.

A decomposition session produces that ranking. **It should precede any further representation work**, for the reason this campaign just demonstrated at its own expense.

## The successor

[A monomorphic field carries its own type](typed-heap-fields-spec.md), and its census is now partly paid for. Two findings belong to it:

**Binaryen already runs closed-world, and refines none of these fields.** `curios-binaryen` sets closed world, which enables `TypeRefining` — and after `-O2` every `$tuple/N` field in `spines` is still `anyref`. The reason is structural: one type serves every constructor of that arity module-wide, so the join over its stores is the top type by construction. The width-keyed roster withholds from a pass already being paid for exactly the information it needs, which is an argument for re-keying that does not depend on any figure.

**The closure representation is the in-tree precedent.** `$envr/{arity}` is a shared supertype carrying an `i32` discriminant, with per-closure final subtypes underneath and typed fields — the shape a variant family wants, already implemented for closures and never applied to variants. It has the same defect this campaign just fixed for tuples: the dispatch casts to the non-final `$envr/{arity}`, 9 such sites in optimized `spines`.

Finality is direction-neutral for that successor — a nominal scheme wants final types too — and only the read cascade is keyed to the width-indexed roster, some thirty lines a re-keying would delete.

## Appendix — what this campaign did not touch

- **Mask forks and position encodings.** Measured and declined before the campaign began: the arithmetic per level is not the wall.
- **A three-way compare chain.** Two cases branch and three or more still table; whether a compare chain beats a small table is a probe question this file names and leaves open.
- **Carrier-aware immediate payloads.** `FieldShape::Immediate` could carry the payload's carrier so it rides raw through a loop. A schema change for a performance question, to be priced on its own probe.
