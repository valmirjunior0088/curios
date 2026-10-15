//! Repeatable measurements of guest coordination, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::test_support::*,
    crate::tests::{LISTING_PROGRAM, compile},
    curios_profile::{Destination, fold, trace},
    curios_runtime::{ForeignBindings, MockHost},
    std::{
        fs::{File, create_dir_all, write},
        io::{BufReader, BufWriter},
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// Measure the compilation and execution of guest coordination.
///
/// Guest coordination is write-once cells, bounded channels and level waiting. This is a local native measurement, separate from the containerized cross-language series.
///
/// ## Workloads and reproduction
///
/// ```text
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture coordination_measurements --test-threads=1
/// ```
///
/// The instrument runs one warmup (sample 0) and five measured samples per workload, in order: `monad_suspend`, then `tui`. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the instrument starts and are excluded.
///
/// - **`monad_suspend`:** the existing `programs/monad_suspend.crs` corpus program, with the argument `10000`, requiring stdout `30000\n` and no stderr. This measures a suspension's path through the driver every combinator is written over, one a step, not contention among branches.
/// - **`tui`:** the existing bordered listing fixture in `curios/src/tests/tui.rs`, driven by 100 down/up pairs followed by `q`. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.
///
/// `curios_profile::trace` listens during compilation and execution, writing one stream per sample into a timestamped directory under `curios/.artifacts/coordination/`. Each stream has a folded `.tsv` beside it. The `coordination_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `coordination_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The test binary has no counting allocator; memory columns in the folded reports are zero and are not allocation evidence.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions. The Tui workload has synthetic time and no physical terminal latency.
///
/// ## What it last printed
///
/// Timed, taken on top of `7ded6ac34`. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc2.1.fc44.x86_64`; Rust `1.95.0`. Cargo release profile, all features, one test thread, default runtime engine configuration. Times are milliseconds; sample 0 is the warmup and is excluded from the medians.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_async` | 0 (warmup) | 1395.798 | 1.679 | 6 |
/// | `monad_async` | 1 | 1355.283 | 1.745 | 6 |
/// | `monad_async` | 2 | 1353.717 | 1.784 | 6 |
/// | `monad_async` | 3 | 1359.175 | 1.727 | 6 |
/// | `monad_async` | 4 | 1366.402 | 1.496 | 6 |
/// | `monad_async` | 5 | 1355.213 | 1.623 | 6 |
/// | `tui` | 0 (warmup) | 4061.824 | 40.514 | 11368 |
/// | `tui` | 1 | 3949.655 | 40.300 | 11368 |
/// | `tui` | 2 | 3959.136 | 40.569 | 11368 |
/// | `tui` | 3 | 3901.249 | 40.719 | 11368 |
/// | `tui` | 4 | 3894.582 | 38.928 | 11368 |
/// | `tui` | 5 | 3899.996 | 44.502 | 11368 |
///
/// Compilation medians are **1355.283 ms** for `monad_async` and **3901.249 ms** for `tui`; execution medians are **1.727 ms** and **40.569 ms**. Tui's final execution sample is the slowest at 44.502 ms, and no cause is assigned from timings alone.
///
/// All twelve captures passed the behavior and closed-span assertions, terminal restoration and all three screen clears included.
#[test]
#[ignore = "measurement, timed: captures compilation and execution costs"]
fn coordination_measurements() {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(".artifacts/coordination")
        .join(stamp.to_string());
    create_dir_all(&directory).expect("capture directory");
    println!("captures: {}", directory.display());
    println!("workload\tsample\tcompile_ms\trun_ms\toutput_bytes");

    for (workload, source) in [
        (
            "monad_suspend",
            include_str!("../../../../programs/monad_suspend.crs"),
        ),
        ("tui", LISTING_PROGRAM),
    ] {
        for sample in 0..=5 {
            let (host, io) = if workload == "tui" {
                listing_host()
            } else {
                MockHost::builder().args(["monad_suspend", "10000"]).build()
            };
            let path = directory.join(format!("{workload}-{sample}.trace"));
            let sink = BufWriter::new(File::create(&path).expect("capture file"));
            trace(Destination::Stream(Box::new(sink)), || {
                let compiled = curios_profile::profile!("coordination_compile" => compile(source))
                    .expect("workload compiles");
                let status = curios_profile::profile!("coordination_run" => compiled.run(host, ForeignBindings::empty()))
                    .expect("workload runs");
                assert_eq!(status, 0);
            })
            .expect("capture opens");
            if workload == "tui" {
                assert_listing(&io);
            } else {
                assert_eq!(io.output(), b"30000\n");
                assert!(io.errors().is_empty());
            }

            let report = fold(BufReader::new(File::open(&path).unwrap())).expect("capture folds");
            assert!(report.open.is_empty(), "every span closed");
            write(path.with_extension("tsv"), report.render()).expect("folded capture");
            let millis = |name| {
                let summary = report
                    .summaries
                    .iter()
                    .find(|row| row.name == name)
                    .unwrap();
                assert_eq!(summary.calls, 1);
                summary.total.as_secs_f64() * 1_000.0
            };
            println!(
                "{workload}\t{sample}\t{:.3}\t{:.3}\t{}",
                millis("coordination_compile"),
                millis("coordination_run"),
                io.output().len(),
            );
        }
    }
}
