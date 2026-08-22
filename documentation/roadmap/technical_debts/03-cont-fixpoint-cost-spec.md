# The fixpoint costs more than everything it optimizes

## Status

Instrumented, located, and two cures landed: `split_parameters`, `eliminate_dead_parameters` and `contify_calls` each drain every candidate one snapshot admits, `forward_aggregate_projections` forwards every projection in one sweep, and the headline program's fixpoint went from 57 rounds and 2 858 ms to **11 rounds and 634 ms**. `curios_cont::optimize` names and times every pass and records whether it fired, and `curios`' `fixpoint_pass_measurements` carries the before and the after. The first revision of this document carried three hypotheses it could not separate; the measurement separated them, the drains answered the one that held, and what remains is inside the inliner and the splits rather than between passes.

## Why it exists

`curios_cont::optimize` was almost the whole of an ordinary `Toml/decode` compile — **97.6%**, 4 719 ms of 4 836 ms, when first measured on `aarch64-apple-darwin` over a program nothing named; **89%**, 2 858 ms of 3 196 ms, re-taken on `x86_64-unknown-linux-gnu` over the census's `TOML_DRIVER`, which is now the program the probe names. After the drains and the forwarding sweep it is **67%**, 634 ms of 949 ms, on the same host and program. Every other stage is noise beside it: elaboration, the kernel, erasure and the lowering into Cont are each under 25 ms, wasm emission is 115 ms, and the whole erased-arena optimizer with its verifier is about 75 ms.

That share is not a symptom of anything else. A point-free spelling used to hand this pass four times the module — a grammar's reified copies grew quadratically until a replacement's residual group was bound at item level — and this pass is what converted that into fifteen minutes. That spelling is fixed, and the share above was always measured with none of it in play. Whatever a user writes, almost all of the compiler's time is here.

## Known for certain

- **The round count was the count of one-candidate-per-call rewrites, three passes deep.** The driver converged in **57 rounds**, and `split_parameters` fired on **54** of them: it admitted "the first admissible split in deterministic order" and its own comment deferred the cleanup to "the next rounds". Draining it took the count to 45 and exposed `eliminate_dead_parameters` behind it, one entity per call, at 44 of 45; draining that took it to 20 and exposed `contify_calls` at 17 of 20; draining that took it to **11**, where the fired column is flat and nothing sets the count. What remains is the depth of genuinely dependent rewrites — a split whose edge carries another split's rebuild, a helper whose site a contification has just moved. Seven passes still admit one candidate per call — `specialize_scc_calls`, `specialize_call_patterns`, `specialize_jump_patterns`, `split_workers`, `split_windows`, `uncurry_returns`, `dissolve_rec_init` — and none of them set the count here; the most frequent, `split_workers`, fired on 6 of the 11.

- **Most of the time was paid by passes that did nothing, and that cost went with the rounds.** Before the drains the passes that fired on eight rounds or fewer summed to about 1.64 s, **57% of the fixpoint**, and 9.7 M of its 15.5 M allocations — each rebuilding its whole-module analysis on every round, 57 times for a handful of rewrites. The cost was super-linear *because* of the round count, not beside it: rounds scaled with the number of rewrites, each round walked the whole module a few dozen times, and the product was the exponent. After the drains and the forwarding sweep the fixpoint allocates 240 MB across 3.9 M allocations.

- **No pass pair undoes the other's work.** The only passes firing in lockstep were the split and the forwarding and dead-binding removal that finish it, which is the designed sequence rather than churn. The rounds were real, sequentially dependent work, done one candidate at a time — which is why draining them was a cure and not a cap.

- **Each drain has a staleness argument, and they differ.** A sweep reuses one analysis for every candidate, so what each pass had to establish is that an earlier rewrite in the sweep leaves the later ones admissible. `eliminate_dead_parameters`' is trivial: removing a parameter only removes uses. `contify_calls`' is four conditions re-examined — sites, escape, reachability, capture — plus one mechanical repair, since a candidate's owner may have been contified ahead of it. `split_parameters`' is the one with a real hazard, and it resolves it by *declining*: a value the snapshot never saw can only be a sweep-mate's head rebuild, whose width is its own region's rather than this one's, so such a candidate waits a round rather than projecting at a width it cannot justify. Each argument lives in the pass's doc comment, and each has a test pinning the sweep and, for the split, the deferral.

- **The cost was super-quadratic in module size, and the series that shows it is the post-cure one.** Over `combinator_sharing_measurements`' as-written spelling the emitted-function count grows 108 → 262 while the compile grew 0.35 s → 1.90 s before the drains: a 2.4× module for 5.4× the time, an exponent near 1.9 on raw wall clocks and between 2.3 and 2.4 once the compile's fixed floor of about 0.3 s is removed. After the drains the same series ran 0.30 s → 1.27 s, and between its two largest sizes a 1.5× module cost 1.84× the time — near 2.1 with the floor removed. That program converges in 7 rounds, so what the exponent measures now is the per-candidate walks *inside* passes, which is where the next section points; its sixteen-rule fixpoint is 639 ms after the forwarding sweep, from 867. The first revision of this document derived `size^2.4` from a series running 118 → 566 functions and 0.54 s → 23.21 s; that series mixed the pre-cure point-free spelling's quadratically larger module into the size axis, and the 15.2 GB and 223.8 M allocations quoted beside it were that spelling's. Both are recorded in `unfolding.rs` as what the probe printed *before* let-insertion; neither module exists any more.

- **It allocated enormously and retained nothing.** Before the drains the driver's fixpoint allocated 1 089 MB across 15.5 M allocations and retained 1.3 MB of it — the signature of analyses rebuilt and dropped per round, which the per-pass columns confirmed: the six heaviest passes accounted for 10.3 M of the allocations between them.

- **It converges.** 57 rounds before, 11 after, against a `ROUND_LIMIT` of 1024; the `debug_assert!` has never fired on any program measured here. The limit is not in play, and lowering it would have truncated real work.

- **`into_wasm` is not negligible on this host and is not the problem.** 112 ms against the fixpoint's 634 ms — now 12% of the compile and the third-largest line of the whole compilation — where the first measurement had it at 39 ms. Worth a read once the fixpoint is out of the way.

What is left stands inside passes rather than between them. `forward_aggregate_projections` was the largest line after the drains — 304 ms with **225 ms in the first round**, forwarding the door's 1 459 projections and every split's by rescanning the module and rebuilding its construction map once per projection forwarded — and is 4 ms as one sweep. `inline_known_calls` is the largest now at 194 ms, walking three bodies per candidate call site per sweep; the three split passes follow at 81, 69 and 48 ms, each walking the module once per split to redirect the old parameter. A sweep's own per-candidate walks are the same shape one level down, and the split sweep already paid for one: on the sixteen-rule grammar its first round cost 285 ms and 4.0 M allocations while every split re-scanned the module for its carriers, and 167 ms and 0.3 M once those were indexed per sweep. `fold_intrinsic_identities` has the same find-one-and-rescan shape as the first and, on this program, nothing to find.

## For comparison

`ROUND_LIMIT` is **1024**. GHC ships `-fmax-simplifier-iterations=4` and `-fsimplifier-phases=2`. The two designs are not the same shape — GHC's simplifier is one occurrence-analysis-guided pass where this is twenty-three independent rewrites driven to a joint fixpoint — so the numbers are not directly comparable, and the comparison is recorded because it is the obvious first question a reader will ask. What the measurement adds is that the round count here is not an iteration depth at all: it is a worklist being drained at one item per round, and GHC's four iterations are the depth a drained worklist would leave.

## The shape the cure took, and what is left

**Done: the passes that set the round count drain every admissible candidate per call.** `inline_known_calls` already showed the shape in this crate — build the analysis once, rewrite every candidate it exposes, let a stale count only tighten a budget; its comment records that rebuilding per inline "is what made this quadratic on a large unoptimized module". Three passes learned it: the first revision predicted that draining `split_parameters` alone would bound the driver near half a second, and it did not, because two more one-per-call passes were standing behind it; draining all three gave 11 rounds and 933 ms. The seven that still admit one candidate per call did not set the count on this program, and each would take its own staleness argument; they are drained when a program shows one of them setting the count, not before.

**Done: the projection rescan is one sweep.** `forward_aggregate_projections` collects every forwardable projection against one construction snapshot, collapses the substitution chains the way `known_values` does, rewrites in one walk, and splices the dead nodes out in one pass — the shape `eliminate_dead_bindings` already had. The first round's 225 ms became 4 ms. `fold_intrinsic_identities` keeps the find-one-and-rescan shape deliberately: it fired on no round of either measured program and costs under a millisecond, so a sweep there would be a change with no measurement behind it.

**Next: `inline_known_calls`' per-candidate body walks**, three per call site per sweep and now the largest line; then the split passes' per-split `replace_atom` walk, which is the same shape one level down and is what their 80 ms each now is.

**Not the analyses' incrementality, not the pass order, not the limit.** Making `analyze_calls` or `origins` incremental would have attacked the 57× multiplier from the expensive side; removing the multiplier was the cure. The order converges in one sequence of dependent rewrites, and the limit is never reached.

## Deliberately not specified

Whether the seven remaining one-candidate passes are drained pre-emptively or on evidence. Whether the drained passes' per-round budgets become per-call budgets, which today they are not — a sweep spends a budget unit per rewrite exactly as a round did. Whether `into_wasm`'s 115 ms, now 9% of the compile, earns a look of its own.

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

for the per-pass allocation columns, which the test binary cannot take because it installs no counting allocator. The first measurement was taken **2026-08-21**, **release**, `aarch64-apple-darwin`; the per-pass figures above, before and after the drains, were taken the same day, **release**, `x86_64-unknown-linux-gnu`, and live with the probe.
