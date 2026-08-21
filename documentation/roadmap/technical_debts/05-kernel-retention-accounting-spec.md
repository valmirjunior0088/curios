# The retention allowance is a compilation's, and one declaration can spend it

## Status

Located and measured, unstarted. `curios`' `scrutinee_retention_measurements` is the probe, and `curios-core/src/retention.rs` already carries the ceiling's own measurement and the sentence this specification is the follow-through on: *"The open question is the source, not the ceiling."*

## Why it exists

`curios-core`'s `DEFAULT_RETENTION_QUOTA` is one billion logical units, measured for headroom against the whole fixed prelude — which uses eleven percent of it. A single thirteen-definition equality proof uses **thirty-one percent**, and three of them in one file use the lot:

```text
  the ladder — one proved web, at the default budget
  rules   kernel retained
  6              836355
  8             3962757
  10           22197227
  11           53329085
  12          128490445
  13          309948607

  control: how many proved webs one program contains, at 12 definitions each
  1 web(s)  retained      128490445
  2 web(s)  retained      256803853
  3 web(s)  retained      385117261
```

The allowance is cumulative and never refunded, so this is not a peak but a running total: what one declaration retains is denied to every declaration after it in the same compilation.

## What crossing it costs

Nothing semantic — the quota degrades the caches and never refuses a program, which `curios-cert`'s `an_exhausted_retention_quota_leaves_the_answer_alone` asserts. What it costs is that everything afterwards runs cold, and the two counters are coupled from one side: a memo that cannot be stored is re-derived against the *work* budget. A compilation that exhausts its allowance stops being linear in what it spends, and a later declaration can then be refused for exhaustion because an earlier one filled a cache it never read.

That was observed directly, before the refinement key moved to the written spelling, on a shape that saturated the quota at eighteen definitions:

| definitions | 17 | 18 | 19 | 40 |
| --- | --- | --- | --- | --- |
| retained | 999 919 608 | 999 999 999 | 1 000 000 000 | 999 999 986 |
| compile | 0.05 s | 2.66 s | 2.68 s | 2.70 s |

A step of fifty-fold on one more definition, then flat forever. It reproduced as budget-invariant across a sixty-four-fold range, invariant in the web's size past the threshold, and invariant in how many copies of the web the program contained — three fingerprints of a saturating compilation-scoped store, and none of a fan-out. `scrutinee_retention_measurements` records that table as what it printed before, and the door that reached it is closed; the proved door above still climbs toward the same ceiling.

## Where the units go

Unexplained, and that is the first work item rather than a caveat. Nothing in the observed programs is large: peak process memory over the whole eighteen-definition compile was 25 MiB, against an allowance that at eight logical bytes a unit nominally bounds retained storage at eight gigabytes.

`Retention` charges `Cost::units(term.footprint())` per insertion, and `footprint` is documented as counting a shared child **once per parent**: *"A DAG's footprint therefore reads as the tree it unfolds to, which overcounts."* Reduction of a web where each definition names the one before it twice produces exactly that DAG, so the natural hypothesis is that the accounting is exponential where the memory is linear. It is a hypothesis: no measurement here separates the footprint's overcount from the number of entries.

`curios-core/src/retention.rs` records the same open question about the elaborator's side, whose retention grows quadratically in a `Str` literal's length with about three-quarters of it unattributed.

## Deliberately not specified

Whether the repair is in the accounting (a footprint that counts a shared node once — which needs a per-walk visited set, and the module documentation rejects exactly that as "exact shared-ownership accounting" for a bound that must stay deterministic and destruction-order-independent), in the ceiling (a larger or per-declaration allowance, which makes what one declaration may spend depend on what ran before it — the property `Memos::begin_declaration` clears its tables to avoid), or in what is stored at all.

Also unspecified: whether saturation should be *observable*. Today a compilation that crosses the quota reports nothing, and the only way to see it is a fifty-fold step in a wall clock. `Kernel::retained` exists and nothing in the compiler reads it.

## How to take the figures

```sh
cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_retention_measurements
```

Taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
