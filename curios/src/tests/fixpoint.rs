//! What the Cont fixpoint costs, pass by pass — the measurement behind the third of the compiler cliffs `documentation/roadmap.md` records, which is closed, and the one that says what is left of it.
//!
//! Behind the `profile` feature for the reason `churn` is: the per-pass spans and fired-samples `curios_cont::optimize` carries exist only there. It reports and does not assert, in the shape of `unfolding.rs`'s measurements, and it exists because the cliff's headline figure was first taken over "an ordinary `Toml/decode` compile" that nothing named — the census's driver is that program, and this is the probe that re-takes it.

use {
    super::codegen::TOML_DRIVER,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_profile::capture,
    curios_text::{Entrypoint, RootSource},
};

/// Where the fixpoint's time goes, and which passes keep it running.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture fixpoint_pass_measurements
/// ```
///
/// Release only, for the reason `combinator_sharing_measurements` gives: the wall clocks are the fixpoint's, and a debug build prices its walks differently. `--all-features` supplies the `profile` feature this module is gated on.
///
/// The memory columns are deliberately absent: the test binary installs no counting allocator, so they would read zero. For the allocation half write `TOML_DRIVER` to a file and take `make curios/profile CURIOS_PROFILE_SOURCE=<file>`, whose stage-level figures are the ones the specification quotes.
///
/// # What it prints
///
/// Every span `curios-cont` emits — the stage, the fixpoint, its analysis and each of its passes — with its total, its call count, and its extremes, then one row per pass saying on how many of the rounds it fired. A pass's `calls` is the round count; a pass whose `fired` tracks `rounds` is one that admits a single candidate per call and is being drained one round at a time.
///
/// # What it last printed
///
/// Taken **2026-08-21**, **release**, `x86_64-unknown-linux-gnu`, with `split_parameters`, `eliminate_dead_parameters` and `contify_calls` each draining every candidate one snapshot admits, and `forward_aggregate_projections` forwarding every projection in one sweep. `cont_optimize` was **634 ms of a 949 ms compile**, over **11 rounds**. The allocation columns are from the `make curios/profile` run on the same program, same day, same host: 240 MB across 3.87 M allocations for the fixpoint.
///
/// | pass | total | fired / 11 | allocated | allocs |
/// | --- | --- | --- | --- | --- |
/// | `inline_known_calls` | 194 ms | 5 | 74 MB | 1.42 M |
/// | `split_parameters` | 81 ms | 4 | 25 MB | 0.29 M |
/// | `split_workers` | 69 ms | 6 | 26 MB | 0.28 M |
/// | `split_returns` | 48 ms | 3 | 20 MB | 0.29 M |
/// | `flatten_indexed_lists` | 37 ms | 1 | 16 MB | 0.23 M |
/// | `inline_single_use_continuations` | 27 ms | 9 | 7 MB | 0.14 M |
/// | `eliminate_dead_parameters` | 25 ms | 8 | 11 MB | 0.13 M |
/// | `contify_calls` | 22 ms | 3 | 7 MB | 0.14 M |
/// | `uncurry_returns` | 19 ms | 2 | 7 MB | 0.14 M |
/// | `prune_unreachable` | 17 ms | 6 | 2 MB | 0.06 M |
/// | `specialize_scc_calls` | 17 ms | 4 | 6 MB | 0.10 M |
/// | `eliminate_dead_bindings` | 14 ms | 10 | 5 MB | 0.10 M |
/// | `known_values` | 13 ms | — | 5 MB | 0.11 M |
/// | `dedupe_intrinsics` | 9 ms | 5 | 3 MB | 0.04 M |
/// | `specialize_jump_patterns` | 7 ms | 8 | 3 MB | 0.06 M |
/// | `fuse_append_chains` | 5 ms | 1 | 2 MB | 0.04 M |
/// | `forward_aggregate_projections` | 4 ms | 9 | 0.4 MB | 0.002 M |
/// | `forward_continuations`, `specialize_call_patterns` | ≤ 3 ms each | 9, 2 | ≤ 3 MB | ≤ 0.05 M |
/// | `rewrite_atoms`, `simplify_nodes`, `fold_intrinsic_identities`, `dissolve_rec_init` | ≤ 1 ms each | 5, 7, 0, 0 | ≈ 0 | ≈ 0 |
/// | `split_windows` | 1 ms, one call | 0 | 0.4 MB | 0.01 M |
///
/// **No pass sets the round count any more.** The fired column is flat — the most frequent, `eliminate_dead_bindings` at 10 of 11, is cleanup — so what remains of the count is the depth of genuinely dependent rewrites: a split whose edge carries another split's rebuild, a helper whose call site a contification has just moved. That is the bound the previous table predicted.
///
/// **What is left is inside passes, not between them.** `forward_aggregate_projections` was the largest line after the drains — 304 ms, 225 ms of it in the first round, forwarding the door's 1 459 projections and every split's by rescanning the module and rebuilding its construction map once per projection — and is 4 ms as one sweep. `inline_known_calls` is the largest now and still walks three bodies per candidate site; the three splits behind it each walk the module once per split to redirect the old parameter. The 57-round multiplier they used to hide behind is gone, and so is the rescan.
///
/// **A sweep's own per-candidate walks are a cost of the same shape, one level down.** The sixteen-rule grammar of `combinator_sharing_measurements` converges in 7 rounds, and its first `split_parameters` round cost 285 ms and 4.0 M allocations when every split re-scanned the module for the nodes carrying edges into its continuation — indexing those once per sweep took the pass to 167 ms and 0.3 M there, and from 94 ms to 81 ms on this program. What a split still walks per candidate is the `replace_atom` that redirects its parameter to the head rebuild.
///
/// # What it printed before the drains
///
/// Same day, same host, the day the spans were added, with one candidate per call in every structural pass. `cont_optimize` was 2 858 ms of a 3 196 ms compile, over **57 rounds**; the fixpoint allocated 1 089 MB across 15.5 M allocations and retained 1.3 MB.
///
/// | pass | total | fired / 57 | allocated | allocs |
/// | --- | --- | --- | --- | --- |
/// | `inline_known_calls` | 645 ms | 8 | 210 MB | 4.30 M |
/// | `split_workers` | 323 ms | 6 | 121 MB | 1.35 M |
/// | `split_parameters` | 312 ms | **54** | 112 MB | 1.26 M |
/// | `forward_aggregate_projections` | 304 ms, 142 ms of it in one round | 55 | 171 MB | 0.83 M |
/// | `split_returns` | 225 ms | 4 | 95 MB | 1.32 M |
/// | `flatten_indexed_lists` | 199 ms | 1 | 83 MB | 1.20 M |
/// | `eliminate_dead_parameters` | 117 ms | 44 | 55 MB | 0.58 M |
/// | `contify_calls` | 103 ms | 17 | 31 MB | 0.65 M |
/// | `specialize_scc_calls` | 95 ms | 3 | 28 MB | 0.48 M |
/// | `prune_unreachable` | 87 ms | 28 | 11 MB | 0.28 M |
/// | `uncurry_returns` | 78 ms | 2 | 28 MB | 0.54 M |
/// | `inline_single_use_continuations` | 69 ms | 19 | 20 MB | 0.39 M |
/// | `eliminate_dead_bindings` | 68 ms | 55 | 20 MB | 0.46 M |
/// | `known_values` | 65 ms | — | 26 MB | 0.55 M |
/// | `specialize_jump_patterns` | 45 ms | 18 | 17 MB | 0.31 M |
/// | `dedupe_intrinsics` | 35 ms | 4 | 17 MB | 0.22 M |
/// | `fuse_append_chains` | 29 ms | 1 | 10 MB | 0.23 M |
/// | `forward_continuations` | 12 ms | 28 | 15 MB | 0.26 M |
/// | `specialize_call_patterns` | 12 ms | 3 | 5 MB | 0.05 M |
/// | `rewrite_atoms`, `simplify_nodes`, `fold_intrinsic_identities`, `dissolve_rec_init` | ≤ 6 ms each | 6, 14, 1, 0 | ≈ 0 | ≈ 0 |
/// | `split_windows` | 1 ms, one call | 0 | 0.4 MB | 0.01 M |
///
/// Three things it said that the two stage-level spans could not, and that the drains were the answer to.
///
/// **The round count was the parameter-split count.** `split_parameters` admitted one split per call and fired on 54 of the 57 rounds; `forward_aggregate_projections` and `eliminate_dead_bindings`, firing on 55 each, were the cleanup its comment promised "on the next rounds". Draining it alone took the count to 45 — and exposed `eliminate_dead_parameters` behind it at 44 of 45, one entity per call; draining that took it to 20 and exposed `contify_calls` at 17 of 20; draining that took it to 11, where nothing sets it.
///
/// **Most of the time was paid by passes that did nothing.** The passes that fired on eight rounds or fewer summed to about 1.64 s, 57% of the fixpoint, and 9.7 M of its 15.5 M allocations: each rebuilt its whole-module analysis on every round, and 57 rounds bought that analysis 57 times for a handful of rewrites.
///
/// **No pass pair undid the other's work.** The only passes firing in lockstep were the split and the forwarding and dead-binding removal that finish it — the designed sequence, one candidate at a time.
#[test]
#[ignore = "measurement: reports what each pass of the fixpoint costs rather than asserting"]
fn fixpoint_pass_measurements() {
    let entrypoint = TOML_DRIVER.parse::<Entrypoint>().expect("driver parses");
    let (outcome, report) = capture(|| {
        compile_with_prelude(
            DEFAULT_STEP_BUDGET,
            &entrypoint,
            &RootSource::none(),
            |_| {},
        )
    });
    outcome.expect("driver compiles");

    println!(
        "{:>10} {:>6} {:>9} {:>9}  name",
        "total_ms", "calls", "min_ms", "max_ms"
    );
    for summary in report.summaries.iter().filter(|summary| {
        summary.name == "compile_entrypoint" || summary.target.starts_with("curios_cont")
    }) {
        println!(
            "{:>10.3} {:>6} {:>9.3} {:>9.3}  {}",
            summary.total.as_secs_f64() * 1_000.0,
            summary.calls,
            summary.min.as_secs_f64() * 1_000.0,
            summary.max.as_secs_f64() * 1_000.0,
            summary.name,
        );
    }

    println!();
    println!("{:>6} {:>6}  name", "fired", "rounds");
    for sample in report
        .samples
        .iter()
        .filter(|sample| sample.name.starts_with("cont"))
    {
        println!("{:>6} {:>6}  {}", sample.total, sample.count, sample.name);
    }
}
