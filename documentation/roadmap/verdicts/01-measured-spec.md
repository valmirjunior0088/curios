# Verdicts, part 1: the certifier measured, and Cranelift in parallel

Working specification for the measurements the rest of this campaign is decided and accepted against, with the one parallelism that needs nothing beside them. The campaign makes every verdict a function of the inputs it declares, with every read recorded: [part 2](02-no-history-spec.md) takes history out of a verdict, [part 3](03-no-minted-identity-spec.md) takes minted identities out of an artifact, [part 4](04-certifier-record-spec.md) has the certifier file its own verdicts and read its own call sites, [part 5](05-one-environment-spec.md) routes every read through one environment, [part 6](06-item-tasks-spec.md) schedules a compilation as a graph of item tasks, and [part 7](07-checked-evidence-spec.md) — not refined yet — has the certifier check evidence rather than trust complex code. Parts 2 to 5 each make the serial compiler better on their own; part 6 is where the threads arrive.

This part needs nothing. Where [the invariants campaign's instruments](../invariants/03-shared-term-costs-spec.md) have landed first, it extends them rather than adding a second set.

## What this builds on

- **The profile.** `curios-profile`'s spans, samples and notes, written as each row is made, and the prelude build's own stream at `curios-prelude-archive/.artifacts/profile.tsv`. A `declaration` span names its item in its `group` field, which is what a per-item distribution is read from.
- **The certifier's walk.** Typing, conversion, level entailment and the erased positions `kernel/positions.rs` records belong to the kernel ([`curios-cert`'s README](../../../curios-cert/README.md)).
- **The launcher boundary.** `curios-runtime`'s `cranelift` feature exists for `curios` and never enters `default`; `curios/src/bundle.rs` enforces it on the shipped launcher image.

## The gap

**The certifier's profile does not distinguish enough of its judgments** to justify decisions about which shared implementation should be replaced, or whether it needs an evaluator of its own ([part 7](07-checked-evidence-spec.md)). Profiling supplies evidence for those decisions; it does not select a new evaluator.

**Where the time is.** The prelude build's `profile.tsv` of 2026-09-23 — an instrumented build, so the shares are the figures and the durations are not — elaborates `/std`'s 2311 items in 49.7 s, parses its 137 files in 10.5 s, runs the whole-module finalization in 9.1 s and erases in 6.7 s. Item costs are spread thin: the heaviest, `/std/Tui/run`, is 2.3% of elaboration, the fifty heaviest 20.8%, and the median item 11.6 ms. A one-declaration program in `curios/.artifacts/profile.tsv` spends 1.3 s of 1.5 s elaborating and 0.1 s restoring the prelude; the back half costs tens of milliseconds. To retake a figure: build with the `profile` feature, and fold the file by pairing each `E` row with its `X` row per span id and summing per callsite name.

**Cranelift precompiles serially**, since `curios-runtime` enables no `parallel-compilation`.

## Stages

Each lands alone.

1. **Profile the certifier.** Spans for its judgments, costs reported by stage and by judgment, and a baseline for every later part, each item's cost in the kernel read beside its cost in the elaborator.
2. **Cranelift compiles in parallel.** `curios-runtime`'s `cranelift` feature enables Wasmtime's `parallel-compilation`; the launcher, which never enables `cranelift`, is unaffected. Accepted when the bundle guards in `curios/src/bundle.rs` pass and the launcher's graph is unchanged, with precompilation time reported before and after.

## Verification

- The certifier is measured before and after each later stage of the campaign, naming the judgment or the stage rather than attributing elaboration timings to certification.
- `cargo x runtime` rebuilds the launcher, and neither `cranelift-codegen` nor `curios-binaryen` enters its graph.

## Completion and retirement

The certifier's profile distinguishes its judgments and the baseline is recorded beside the measurement that retakes it; Cranelift compiles in parallel in `curios` alone. Record the baseline and its command in `curios-cert`'s README, replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
