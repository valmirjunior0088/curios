# The fixpoint costs more than everything it optimizes

## Status

Instrumented, and the cause located; no cure started. `curios_cont::optimize` now names and times every pass and records whether it fired, and `curios`' `fixpoint_pass_measurements` re-takes the headline program under those spans. The first revision of this document carried three hypotheses it could not separate; the measurement separates them, and what follows is the answer rather than the question.

## Why it exists

`curios_cont::optimize` is almost the whole of an ordinary `Toml/decode` compile — **97.6%**, 4 719 ms of 4 836 ms, when first measured on `aarch64-apple-darwin` over a program nothing named; **89%**, 2 858 ms of 3 196 ms, re-taken on `x86_64-unknown-linux-gnu` over the census's `TOML_DRIVER`, which is now the program the probe names. Every other stage is noise beside it: elaboration, the kernel, erasure and the lowering into Cont are each under 25 ms, wasm emission is 117 ms, and the whole erased-arena optimizer with its verifier is about 75 ms.

That share is not a symptom of anything else. A point-free spelling used to hand this pass four times the module — a grammar's reified copies grew quadratically until a replacement's residual group was bound at item level — and this pass is what converted that into fifteen minutes. That spelling is fixed, and the share above was always measured with none of it in play. Whatever a user writes, almost all of the compiler's time is here.

## Known for certain

- **The round count is the parameter-split count.** The driver converges in **57 rounds**, and `split_parameters` fired on **54** of them. It admits one split per call — "the first admissible split in deterministic order" — and its own comment defers the cleanup to "the next rounds": `forward_aggregate_projections` and `eliminate_dead_bindings`, which fired on 55 rounds each, are that cleanup. Nothing else fires often enough to be what keeps the fixpoint alive; the next candidates, `eliminate_dead_parameters` at 44 and `forward_continuations` at 28, are downstream of the same splits. Nine of the twenty-three passes share the one-candidate-per-call shape — `contify_calls`, `specialize_scc_calls`, `specialize_call_patterns`, `specialize_jump_patterns`, `split_parameters`, `split_workers`, `split_windows`, `uncurry_returns`, `dissolve_rec_init` — so on another program another of them may set the count, but the shape is the same.

- **Most of the time is paid by passes that did nothing.** The passes that fired on eight rounds or fewer — `inline_known_calls`, `split_workers`, `split_returns`, `flatten_indexed_lists`, `specialize_scc_calls`, `uncurry_returns`, `dedupe_intrinsics`, `fuse_append_chains`, `specialize_call_patterns` — sum to about 1.64 s, **57% of the fixpoint**, and to 9.7 M of its 15.5 M allocations. Each rebuilds its whole-module analysis on every round, and 57 rounds bought that analysis 57 times for a handful of rewrites. `inline_known_calls` is the largest single line: 645 ms, 210 MB and 4.3 M allocations, to inline on eight rounds. The cost is therefore super-linear *because* of the round count, not beside it: rounds scale with the number of splits, each round walks the whole module a few dozen times, and the product is the exponent.

- **No pass pair undoes the other's work.** The only passes firing in lockstep are the split and the forwarding and dead-binding removal that finish it, which is the designed sequence rather than churn. The 57 rounds are real, sequentially dependent work, done one candidate at a time.

- **The cost is super-quadratic in module size, and the series that shows it is the post-cure one.** Over `combinator_sharing_measurements`' as-written spelling the emitted-function count grows 108 → 262 while the compile grows 0.35 s → 1.90 s: a 2.4× module for 5.4× the time, an exponent near 1.9 on raw wall clocks and between 2.3 and 2.4 once the compile's fixed floor of about 0.3 s is removed. The first revision of this document derived `size^2.4` from a series running 118 → 566 functions and 0.54 s → 23.21 s; that series mixed the pre-cure point-free spelling's quadratically larger module into the size axis, and the 15.2 GB and 223.8 M allocations quoted beside it were that spelling's. Both are recorded in `unfolding.rs` as what the probe printed *before* let-insertion; neither module exists any more.

- **It allocates enormously and retains nothing.** The driver's fixpoint allocates 1 089 MB across 15.5 M allocations and retains 1.3 MB of it. That is the signature of analyses rebuilt and dropped per round, and the per-pass columns say which: the six heaviest passes account for 10.3 M of the allocations between them.

- **It converges.** 57 rounds against a `ROUND_LIMIT` of 1024, and the `debug_assert!` has never fired on any program measured here. The limit is not in play, and lowering it would truncate real work.

- **`into_wasm` is not negligible on this host and is not the problem.** 117 ms against the fixpoint's 2 858 ms — 3.7% of the compile — where the first measurement had it at 39 ms. Worth re-reading once the fixpoint is cheap; not worth anything before.

One cost stands apart from the fixpoint's shape and inside one pass: `forward_aggregate_projections` spent 142 ms of its 304 ms in the first round, forwarding the door's 1 459 projections by rescanning the module and rebuilding its construction map once per projection forwarded. `fold_intrinsic_identities` has the same find-one-and-rescan shape and, on this program, nothing to find.

## For comparison

`ROUND_LIMIT` is **1024**. GHC ships `-fmax-simplifier-iterations=4` and `-fsimplifier-phases=2`. The two designs are not the same shape — GHC's simplifier is one occurrence-analysis-guided pass where this is twenty-three independent rewrites driven to a joint fixpoint — so the numbers are not directly comparable, and the comparison is recorded because it is the obvious first question a reader will ask. What the measurement adds is that the round count here is not an iteration depth at all: it is a worklist being drained at one item per round, and GHC's four iterations are the depth a drained worklist would leave.

## The shape a cure might take

Now known, and in order.

**First, the nine one-candidate passes drain every admissible candidate per call.** `inline_known_calls` already shows the shape in this crate: build the analysis once, rewrite every candidate it exposes, and let a stale count only tighten a budget — its comment records that rebuilding per inline "is what made this quadratic on a large unoptimized module", which is the same lesson one pass learned and eight did not. A `split_parameters` that splits every admissible parameter its `origins` and `demands` admit, rather than the first, turns 54 rounds into the handful the splits' genuine dependencies need; the same holds for each of the others on the program where it sets the count. On the driver that bounds the fixpoint near six rounds of about 50 ms plus the first round's 142 ms — roughly half a second against 2.9 s — before any per-round cost is touched.

**Second, the two find-one-and-rescan passes become sweeps.** `forward_aggregate_projections` and `fold_intrinsic_identities` collect every candidate against one snapshot and rewrite them in one walk, as `eliminate_dead_bindings` already does. This is what the first round's 142 ms is.

**Not the analyses' incrementality, not the pass order, not the limit.** Making `analyze_calls` or `origins` incremental would attack the 57× multiplier from the expensive side; the multiplier is the thing to remove. The order converges in one sequence of dependent rewrites already, and the limit is never reached.

## Deliberately not specified

How each pass drains: a sweep over a snapshot that stays safe because every rewrite only narrows what the snapshot admits — `inline_known_calls`' argument — or a rebuild between splits, which is cheaper than a round but not free. Whether the drained passes keep their per-round budgets as per-call budgets. Whether `into_wasm`'s 117 ms stays at 3.7% once the fixpoint stops hiding it.

## How to take the figures

```sh
cargo test --release --package curios --lib --all-features -- --ignored --nocapture fixpoint_pass_measurements
```

for the per-pass time, the round count, and which passes fired on which rounds;

```sh
cargo test --release --package curios --lib -- --ignored --nocapture combinator_sharing_measurements
```

for the size-against-time series; and — writing the census's `TOML_DRIVER` to a file —

```sh
make curios/profile CURIOS_PROFILE_SOURCE=<file>
```

for the per-pass allocation columns, which the test binary cannot take because it installs no counting allocator. The first measurement was taken **2026-08-21**, **release**, `aarch64-apple-darwin`; the per-pass figures above were taken the same day, **release**, `x86_64-unknown-linux-gnu`, and live with the probe.
