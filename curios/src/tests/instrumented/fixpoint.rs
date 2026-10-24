//! What the Cont fixpoint costs, pass by pass.
//!
//! Instrumented because the per-pass spans and fired-samples `curios_cont::optimize` carries exist only under the `profile` feature. It reports costs without asserting thresholds, in the shape of `unfolding.rs`'s measurements, and its subject is the census's `TOML_DRIVER`, so the program a figure is taken over is named.

use {
    crate::tests::TOML_DRIVER,
    curios_pipeline::{DEFAULT_BUDGET, compile_with_prelude},
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
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture fixpoint_pass_measurements --test-threads=1
/// ```
///
/// This prints one profiled compilation of `TOML_DRIVER`: each span's total, call count, extremes, allocated bytes and allocation calls, followed by its magnitude samples. A pass's boolean sample total counts rounds in which it changes the graph; its sample count counts invocations. A sweep can rewrite many candidates in one invocation. Its fired count neither counts rewrites nor establishes one-candidate draining or independence: the direct pass tests hold the snapshot and deferral contracts. Samples ending in `::nodes`, `::conts`, `::cap` and `::values` are graph sizes, not fired counts. Every span must close.
///
/// For a reading, finish all builds and checks first, then run the command once as a warmup and seven more times sequentially, each invocation in a fresh test process. Keep the checkout, toolchain and feature set fixed, and keep other machine work idle. Cargo's test executable can be invoked directly with the same test filter and flags after the build. Retain each invocation's stdout. Discard the warmup; report the median and observed minimum–maximum of each span's aggregate time over the seven measured compilations. Record whether counted samples and allocations agree across those compilations; summarize varying allocation calls with their medians and observed spans.
///
/// Parsing precedes the trace. The trace wraps `compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})`, with no package cache or stage rendering. The `compile_entrypoint` span starts after prelude restoration and covers the pipeline through Wasm emission; it excludes Binaryen, Cranelift and guest execution. The stream stays in memory and is folded after compilation; printing is outside the trace.
///
/// The `profile` feature installs the process-wide counting allocator as well as tracing. Timings and allocation work include this observer, including its in-memory stream, and do not price the default-feature compiler. Allocation columns are cumulative work, including nested spans, rather than peak resident memory. Per-pass times and allocations are inclusive as well; nested spans cannot be added to their parent.
///
/// # Latest reading
///
/// Mixed: timed span totals; counted optimizer rounds, pass firings and allocation work. Taken on top of `9a18c2491` on 2026-10-23. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc3.1.fc44.x86_64`; Rust `1.99.1` (`58cf8f9c3`). Cargo release profile, all features, one test thread, no CPU pinning, CPU governor `performance`, boost enabled. One warmup and seven measured compilations of this checkout, with one-minute load averages 1.42–1.58 at invocation boundaries. Times are medians with observed spans in milliseconds. Allocated MiB are medians rounded to 0.1 MiB; allocated-byte totals vary by less than 0.016 MiB in every span. Allocation calls are a single count when identical across all seven compilations, otherwise a median with its observed span.
///
/// | Span | ms [span] | Allocated MiB | Allocation calls |
/// | --- | ---: | ---: | ---: |
/// | `compile_entrypoint` | 2455.121 [2407.826–2458.732] | 1,622.4 | 21,782,607 [21,782,519–21,782,653] |
/// | `cont_optimize` | 1909.334 [1874.810–1911.951] | 1,250.5 | 16,722,058 [16,722,048–16,722,060] |
///
/// The optimizer settles in 11 rounds. All fired counts and graph-size sample totals and counts agree across the seven measured compilations.
///
/// | Pass | ms [span] | Fired / calls | Allocated MiB | Allocation calls |
/// | --- | ---: | ---: | ---: | ---: |
/// | `split_parameters` | 233.562 [230.088–236.762] | 5 / 11 | 105.8 | 1,318,154 |
/// | `shrink` | 213.210 [209.077–214.218] | 9 / 11 | 111.9 | 1,650,707 [1,650,706–1,650,707] |
/// | `split_workers` | 192.100 [189.031–193.395] | 2 / 11 | 162.6 | 1,986,048 |
/// | `inline_known_calls` | 189.549 [186.523–191.377] | 4 / 11 | 149.5 | 1,889,774 |
/// | `split_returns` | 128.824 [127.356–129.791] | 6 / 11 | 95.9 | 1,253,116 |
/// | `flatten_indexed_lists` | 124.423 [122.796–124.724] | 1 / 11 | 57.2 | 1,029,201 |
/// | `answer_row_tests` | 119.003 [116.901–119.647] | 0 / 11 | 104.7 | 1,077,875 |
/// | `known_values` | 98.306 [96.256–99.328] | — / 11 | 79.4 | 1,094,948 [1,094,947–1,094,948] |
/// | `uncurry_returns` | 97.345 [95.947–98.859] | 4 / 11 | 72.8 | 999,936 |
/// | `specialize_scc_calls` | 74.940 [74.680–75.839] | 2 / 11 | 62.1 | 803,059 |
/// | `contify_calls` | 73.759 [72.162–74.700] | 5 / 11 | 57.8 | 768,589 |
/// | `separate_loop_entries` | 69.666 [67.971–70.493] | 5 / 11 | 58.7 | 788,046 |
/// | `prune_unreachable` | 35.947 [35.662–36.453] | 6 / 11 | 5.3 | 131,309 [131,309–131,310] |
/// | `uncurry_nests` | 32.392 [31.774–32.580] | 1 / 11 | 15.6 | 333,542 |
/// | `eliminate_dead_parameters` | 31.869 [31.384–32.337] | 5 / 11 | 23.4 | 288,372 |
/// | `eliminate_dead_calls` | 24.980 [24.583–25.465] | 1 / 11 | 20.2 | 197,927 |
/// | `split_windows` | 23.957 [23.560–24.238] | 1 / 2 | 9.7 | 174,054 |
/// | `answer_window_guards` | 20.360 [19.966–20.768] | 1 / 11 | 7.0 | 111,153 |
/// | `dedupe_intrinsics` | 18.603 [18.242–19.072] | 5 / 11 | 6.7 | 94,781 |
/// | `specialize_jump_patterns` | 18.524 [18.306–18.730] | 3 / 11 | 9.9 | 178,576 |
/// | `fuse_append_chains` | 11.866 [11.732–12.057] | 1 / 11 | 4.2 | 95,686 |
/// | `specialize_call_patterns` | 6.346 [6.254–6.616] | 3 / 11 | 2.3 | 23,702 |
/// | `forward_continuations` | 5.142 [5.056–5.253] | 6 / 11 | 6.0 | 109,781 [109,780–109,781] |
/// | `rewrite_atoms` | 3.231 [3.179–3.297] | 3 / 11 | 0.0 | 53 [52–53] |
/// | `forward_calls` | 2.612 [2.560–2.698] | 2 / 11 | 0.5 | 3,361 [3,360–3,361] |
///
/// `known_values` records no boolean fired sample. `split_windows` runs only on otherwise idle rounds. All eight compilations pass the compilation and closed-span assertions.
#[test]
#[ignore = "measurement, mixed: reports pass costs and counts"]
fn fixpoint_pass_measurements() {
    let entrypoint = TOML_DRIVER.parse::<Entrypoint>().expect("driver parses");
    // The stream goes to a buffer and is folded back here: this test wants the aggregate, which is one consumer of the rows.
    let rows = Rows::default();
    let outcome = trace(Destination::Stream(Box::new(rows.clone())), || {
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
    })
    .expect("a stream destination opens");
    outcome.expect("driver compiles");
    let report = fold(rows.rows().as_slice()).expect("the rows fold");
    assert!(report.open.is_empty(), "every span closed");

    println!(
        "{:>10} {:>6} {:>9} {:>9} {:>15} {:>12}  name",
        "total_ms", "calls", "min_ms", "max_ms", "allocated_bytes", "allocations"
    );
    for summary in report.summaries.iter().filter(|summary| {
        summary.name == "compile_entrypoint" || summary.target.starts_with("curios_cont")
    }) {
        println!(
            "{:>10.3} {:>6} {:>9.3} {:>9.3} {:>15} {:>12}  {}",
            summary.total.as_secs_f64() * 1_000.0,
            summary.calls,
            summary.min.as_secs_f64() * 1_000.0,
            summary.max.as_secs_f64() * 1_000.0,
            summary.allocated,
            summary.allocations,
            summary.name,
        );
    }

    println!();
    println!("{:>10} {:>7}  name", "total", "samples");
    for sample in report
        .samples
        .iter()
        .filter(|sample| sample.name.starts_with("cont"))
    {
        println!("{:>10} {:>7}  {}", sample.total, sample.count, sample.name);
    }
}
