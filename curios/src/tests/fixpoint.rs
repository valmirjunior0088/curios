//! What the Cont fixpoint costs, pass by pass.
//!
//! Behind the `profile` feature for the reason `churn` is: the per-pass spans and fired-samples `curios_cont::optimize` carries exist only there. It reports and does not assert, in the shape of `unfolding.rs`'s measurements, and its subject is the census's `TOML_DRIVER`, so the program a figure is taken over is named.

use {
    super::codegen::TOML_DRIVER,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_profile::{Destination, fold, trace},
    curios_text::{Entrypoint, RootSource},
};

/// The stream, kept in memory so the fold reads it back without a file.
#[derive(Clone, Default)]
struct Rows(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl Rows {
    fn rows(&self) -> Vec<u8> {
        self.0.lock().expect("rows lock").clone()
    }
}

impl std::io::Write for Rows {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.0.lock().expect("rows lock").extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

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
/// The memory columns are deliberately absent: the test binary installs no counting allocator, so they would read zero. For the allocation half write `TOML_DRIVER` to a file and take `cargo xtask profile --profile release <file>`, whose stage-level figures carry them.
///
/// # What it prints
///
/// Every span `curios-cont` emits — the stage, the fixpoint, its analysis and each of its passes — with its total, its call count, and its extremes, then one row per pass saying on how many of the rounds it fired. A pass's `calls` is the round count; a pass whose `fired` tracks `rounds` is one that admits a single candidate per call and is being drained one round at a time.
///
/// # What it last printed
///
/// **Release**, `x86_64-unknown-linux-gnu`. `cont_optimize` was **634 ms of a 949 ms compile**, over **11 rounds**. The allocation columns are from the `cargo xtask profile --profile release` run on the same program and host: 240 MB across 3.87 M allocations for the fixpoint.
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
/// | `rewrite_atoms`, `simplify_nodes`, `fold_intrinsic_identities` | ≤ 1 ms each | 5, 7, 0 | ≈ 0 | ≈ 0 |
/// | `split_windows` | 1 ms, one call | 0 | 0.4 MB | 0.01 M |
///
/// **No pass sets the round count.** The fired column is flat — the most frequent, `eliminate_dead_bindings` at 10 of 11, is cleanup — so what the count measures is the depth of genuinely dependent rewrites: a split whose edge carries another split's rebuild, a helper whose call site a contification has just moved.
///
/// **What is left is inside passes, not between them.** `inline_known_calls` is the largest line and walks three bodies per candidate site; the three splits behind it each walk the module once per split to redirect the old parameter.
///
/// **A sweep's own per-candidate walks are a cost of the same shape, one level down.** A split re-scanning the module for the nodes carrying edges into its continuation, rather than reading the index its sweep builds once, would pay that scan once per candidate. What a split still walks per candidate is the `replace_atom` that redirects its parameter to the head rebuild.
#[test]
#[ignore = "measurement: reports what each pass of the fixpoint costs rather than asserting"]
fn fixpoint_pass_measurements() {
    let entrypoint = TOML_DRIVER.parse::<Entrypoint>().expect("driver parses");
    // The stream goes to a buffer and is folded back here: this test wants the aggregate, which is one consumer of the rows.
    let rows = Rows::default();
    let outcome = trace(Destination::Stream(Box::new(rows.clone())), || {
        compile_with_prelude(
            DEFAULT_STEP_BUDGET,
            &entrypoint,
            &RootSource::none(),
            |_| {},
        )
    })
    .expect("a stream destination opens");
    outcome.expect("driver compiles");
    let report = fold(rows.rows().as_slice()).expect("the rows fold");

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
