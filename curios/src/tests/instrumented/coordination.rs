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
/// - **`tui`:** the existing bordered listing fixture in `curios/src/tests/tui.rs`, driven by 100 down/up pairs followed by `q`, the keys pausing twice between two whole keys so the session's size ticker runs once at each pause. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.
///
/// `curios_profile::trace` listens during compilation and execution, writing one stream per sample into a timestamped directory under `curios/.artifacts/coordination/`. Each stream has a folded `.tsv` beside it. The `coordination_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `coordination_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The `profile` feature installs `curios-profile`'s counting allocator, so each boundary row of a stream carries what the whole test process held and had taken there, and not what its span allocated alone.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions. The Tui workload has synthetic time and no physical terminal latency.
///
/// ## What it last printed
///
/// Timed, taken on top of `a439dba6a`. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc3.1.fc44.x86_64`; Rust `1.95.0`. Cargo release profile, all features, one test thread, default runtime engine configuration. Times are milliseconds; sample 0 is the warmup and is excluded from the medians.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_suspend` | 0 (warmup) | 823.813 | 21.616 | 6 |
/// | `monad_suspend` | 1 | 791.173 | 19.914 | 6 |
/// | `monad_suspend` | 2 | 792.721 | 19.737 | 6 |
/// | `monad_suspend` | 3 | 777.809 | 20.596 | 6 |
/// | `monad_suspend` | 4 | 786.896 | 20.457 | 6 |
/// | `monad_suspend` | 5 | 770.452 | 19.935 | 6 |
/// | `tui` | 0 (warmup) | 5033.656 | 63.509 | 11368 |
/// | `tui` | 1 | 5018.438 | 52.253 | 11368 |
/// | `tui` | 2 | 4984.113 | 48.168 | 11368 |
/// | `tui` | 3 | 5063.733 | 48.786 | 11368 |
/// | `tui` | 4 | 5032.786 | 46.899 | 11368 |
/// | `tui` | 5 | 4995.298 | 45.700 | 11368 |
///
/// Compilation medians are **786.896 ms** for `monad_suspend` and **5018.438 ms** for `tui`; execution medians are **19.935 ms** and **48.168 ms**.
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
