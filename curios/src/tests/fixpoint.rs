//! What the Cont fixpoint costs, pass by pass — the measurement `documentation/roadmap/technical_debts/03-cont-fixpoint-cost-spec.md` leans on.
//!
//! Behind the `profile` feature for the reason `churn` is: the per-pass spans and fired-samples `curios_cont::optimize` carries exist only there. It reports and does not assert, in the shape of `unfolding.rs`'s measurements, and it exists because that specification's headline figure was taken over "an ordinary `Toml/decode` compile" that nothing named — the census's driver is that program, and this is the probe that re-takes it.

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
/// Taken **2026-08-21**, **release**, `x86_64-unknown-linux-gnu`, the day the spans were added. `cont_optimize` was 2 858 ms of a 3 196 ms compile, over **57 rounds**. The allocation columns are from the `make curios/profile` run on the same program, same day, same host, whose totals agree with these to within four percent.
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
/// Three things it says that the two stage-level spans could not.
///
/// **The round count is the parameter-split count.** `split_parameters` admits one split per call and fired on 54 of the 57 rounds; the two passes that fire beside it every round, `forward_aggregate_projections` and `eliminate_dead_bindings`, are the cleanup its own comment promises "on the next rounds". Nothing else fires often enough to be what keeps the fixpoint alive — the next candidates, `eliminate_dead_parameters` at 44 and `forward_continuations` at 28, are downstream of the same splits.
///
/// **Most of the time is paid by passes that did nothing.** The passes that fired on eight rounds or fewer — `inline_known_calls`, `split_workers`, `split_returns`, `flatten_indexed_lists`, `specialize_scc_calls`, `uncurry_returns`, `dedupe_intrinsics`, `fuse_append_chains`, `specialize_call_patterns` — sum to about 1.64 s, 57% of the fixpoint, and to 9.7 M of its 15.5 M allocations: each rebuilds its whole-module analysis on every round, and 57 rounds bought the analysis 57 times for a handful of rewrites. `inline_known_calls` is the largest single line and inlined on eight rounds.
///
/// **No pass pair undoes the other's work.** The only passes firing in lockstep are the split and the forwarding and dead-binding removal that finish it, which is the designed sequence rather than churn; the 57 rounds are real, sequentially dependent work, done one candidate at a time.
///
/// The 142 ms maximum on `forward_aggregate_projections` is the first round, which forwards the door's 1 459 projections by rescanning the module and rebuilding its construction map once per projection forwarded — a cost inside one pass rather than of the fixpoint, and the largest such.
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
