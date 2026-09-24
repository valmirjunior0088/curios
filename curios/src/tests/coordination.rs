//! Repeatable before-and-after measurements for guest coordination, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::{compile, tui::LISTING_PROGRAM},
    curios_profile::{Destination, fold, trace},
    curios_runtime::{MockHost, MockIo},
    std::{
        fs::{File, create_dir_all, write},
        io::{BufReader, BufWriter},
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

fn listing_host() -> (MockHost, MockIo) {
    let mut keys = vec![b"\x1b[".to_vec(), b"B".to_vec(), b"\x1b[A".to_vec()];
    for _ in 1..100 {
        keys.extend([b"\x1b[B".to_vec(), b"\x1b[A".to_vec()]);
    }
    keys.push(b"q".to_vec());

    MockHost::builder()
        .tty_sizes([(12, 5), (16, 6), (12, 5)])
        .stdin_chunks(keys)
        .mono((0..100_000).map(|millis| (millis / 1_000, millis % 1_000 * 1_000_000)))
        .build()
}

fn assert_listing(io: &MockIo) {
    let output = String::from_utf8(io.output()).expect("terminal output is UTF-8");
    assert!(output.starts_with("\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[>1u"));
    assert!(output.ends_with("\x1b[<u\x1b[?2004l\x1b[?25h\x1b[?1049lone"));
    assert_eq!(
        output.matches("\x1b[2J").count(),
        3,
        "initial frame and two resizes"
    );
    assert!(output.contains("┌pick"));
    assert_eq!(io.raw_modes(), [true, false]);
    assert!(io.errors().is_empty());
}

/// Measure compilation and execution around the guest coordination migration.
///
/// This capture compares compilation and execution around the migration from rewritable cells and notification-based coordination to write-once cells, bounded channels and level waiting. The baseline, isolated checking change and completed coordination comparison are recorded below. It is a local native measurement, separate from the containerized cross-language series.
///
/// ## Workloads and reproduction
///
/// ```text
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture coordination_measurements --test-threads=1
/// ```
///
/// The instrument runs one warmup (sample 0) and five measured samples per workload, in order: `monad_async`, then `tui`. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the instrument starts and are excluded.
///
/// - **`monad_async`:** the existing `programs/monad_async.crs` corpus program, with stdin `10000\n`, requiring stdout `30000\n` and no stderr. This measures the Async carrier's recursive bind path, not contention among fibers.
/// - **`tui`:** the existing bordered listing fixture in `curios/src/tests/tui.rs`, driven by 100 down/up pairs followed by `q`. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.
///
/// `curios_profile::trace` listens during compilation and execution, writing one unrotated stream per sample into a timestamped directory under `curios/.artifacts/coordination/`. Each stream has a folded `.tsv` beside it. The `coordination_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `coordination_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The test binary has no counting allocator; memory columns in the folded reports are zero and are not allocation evidence.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions. The Tui workload has synthetic time and no physical terminal latency.
///
/// ## Baseline
///
/// Captured on 2026-09-24, on `main` at `3a80b39ae1f657ca7a0871349897fa1d1d6ee036` plus the measurement harness in this checkpoint: `/sys/Option` and the shared string registry entry are present; cells, channels, Async and Tui still use the previous coordination implementation.
///
/// Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.4-ogc3.1.fc44.x86_64`; Rust `1.95.0 (59807616e 2026-04-14)`. Cargo release profile, all features, one test thread, default runtime engine configuration.
///
/// All times below are milliseconds. Sample 0 is the warmup and is excluded from the medians.
///
/// | Workload | Sample | Compile | Run | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_async` | 0 (warmup) | 1717.323 | 2.354 | 6 |
/// | `monad_async` | 1 | 1572.409 | 2.071 | 6 |
/// | `monad_async` | 2 | 1567.169 | 1.996 | 6 |
/// | `monad_async` | 3 | 1572.799 | 2.028 | 6 |
/// | `monad_async` | 4 | 1563.540 | 2.056 | 6 |
/// | `monad_async` | 5 | 1556.929 | 2.040 | 6 |
/// | `tui` | 0 (warmup) | 4751.845 | 53.916 | 11368 |
/// | `tui` | 1 | 4743.140 | 43.359 | 11368 |
/// | `tui` | 2 | 4721.612 | 41.798 | 11368 |
/// | `tui` | 3 | 4747.226 | 45.056 | 11368 |
/// | `tui` | 4 | 4731.631 | 42.499 | 11368 |
/// | `tui` | 5 | 4759.048 | 60.506 | 11368 |
///
/// Measured medians: `monad_async` compile **1567.169 ms**, run **2.040 ms**; `tui` compile **4743.140 ms**, run **43.359 ms**. Tui execution ranges from 41.798 to 60.506 ms, so a later comparison must account for that spread. All twelve samples passed their behavior and closed-span assertions; each workload produced the same output length in every sample.
///
/// Local raw and folded captures: `curios/.artifacts/coordination/1790267061024372648/`, with files named `<workload>-<sample>.trace` and `<workload>-<sample>.tsv`. These generated artifacts are not committed; the protocol and individual timings above are the durable baseline.
///
/// ## Isolating uniform intrinsic result checking
///
/// Captured on the same host and date, at `dff716722503e08341571ce6ef2b34a627189d85` plus the change that sends every `Produced::Fixed` result through the existing type-formation judgment in both checkers. No cell, channel, scheduler or session implementation has changed at this stage. The command, workloads and profiling configuration are the baseline's.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_async` | 0 (warmup) | 1701.989 | 2.183 | 6 |
/// | `monad_async` | 1 | 1564.255 | 2.010 | 6 |
/// | `monad_async` | 2 | 1556.703 | 1.989 | 6 |
/// | `monad_async` | 3 | 1549.208 | 1.963 | 6 |
/// | `monad_async` | 4 | 1562.326 | 2.012 | 6 |
/// | `monad_async` | 5 | 1555.506 | 2.160 | 6 |
/// | `tui` | 0 (warmup) | 4744.740 | 53.718 | 11368 |
/// | `tui` | 1 | 4729.084 | 43.130 | 11368 |
/// | `tui` | 2 | 4716.665 | 42.099 | 11368 |
/// | `tui` | 3 | 4772.841 | 43.263 | 11368 |
/// | `tui` | 4 | 4756.062 | 43.748 | 11368 |
/// | `tui` | 5 | 4774.254 | 59.223 | 11368 |
///
/// Measured compilation medians are **1556.703 ms** for `monad_async` and **4756.062 ms** for `tui`, respectively about −0.7% and +0.3% against the baseline. Execution medians are **2.010 ms** and **43.263 ms**. These samples show no substantial added compilation cost; they do not establish that the additional judgment is free for every program. All twelve captures passed their assertions. Raw and folded files are under `curios/.artifacts/coordination/1790268023036184798/`. This isolates the checking change; the completed coordination comparison follows.
///
/// ## Completed guest coordination
///
/// Captured on the same host and date, at `dff716722503e08341571ce6ef2b34a627189d85` plus the combined coordination checkpoint: uniform result checking, write-once cells, bounded channels, cell-and-channel knots, level waiting and threaded sessions. The command, workloads and profiling configuration are unchanged. The Rust release build took 2m 31s before the instrument started; it is excluded from every figure below.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_async` | 0 (warmup) | 1613.081 | 2.342 | 6 |
/// | `monad_async` | 1 | 1459.401 | 2.070 | 6 |
/// | `monad_async` | 2 | 1449.361 | 2.069 | 6 |
/// | `monad_async` | 3 | 1448.959 | 1.967 | 6 |
/// | `monad_async` | 4 | 1449.392 | 2.036 | 6 |
/// | `monad_async` | 5 | 1444.012 | 2.131 | 6 |
/// | `tui` | 0 (warmup) | 3977.918 | 54.264 | 11368 |
/// | `tui` | 1 | 3929.009 | 44.272 | 11368 |
/// | `tui` | 2 | 3951.643 | 43.544 | 11368 |
/// | `tui` | 3 | 3950.362 | 43.637 | 11368 |
/// | `tui` | 4 | 3909.094 | 43.176 | 11368 |
/// | `tui` | 5 | 3917.634 | 64.601 | 11368 |
///
/// Compilation medians are **1449.361 ms** for `monad_async` and **3929.009 ms** for `tui`, about **7.5%** and **17.2%** below baseline. Execution medians are **2.069 ms** and **43.637 ms**, about **1.4%** and **0.6%** above baseline and within its observed ranges. These samples show a compilation improvement and no clear execution regression; they do not establish a general performance guarantee. Tui's final execution sample remains an outlier at 64.601 ms, compared with the baseline's high sample of 60.506 ms. No cause is assigned from timings alone.
///
/// All twelve captures passed the behavior and closed-span assertions. Output lengths match the baseline for both workloads, including terminal restoration and all three screen clears. Raw and folded files are under `curios/.artifacts/coordination/1790272977105900109/`.
#[test]
#[ignore = "measurement: captures compilation and execution costs"]
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
            "monad_async",
            include_str!("../../../programs/monad_async.crs"),
        ),
        ("tui", LISTING_PROGRAM),
    ] {
        for sample in 0..=5 {
            let (host, io) = if workload == "tui" {
                listing_host()
            } else {
                MockHost::builder().stdin_lines(["10000"]).build()
            };
            let path = directory.join(format!("{workload}-{sample}.trace"));
            let sink = BufWriter::new(File::create(&path).expect("capture file"));
            trace(Destination::Stream(Box::new(sink)), || {
                let compiled = curios_profile::profile!("coordination_compile" => compile(source))
                    .expect("workload compiles");
                let status = curios_profile::profile!("coordination_run" => compiled.run(host))
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
