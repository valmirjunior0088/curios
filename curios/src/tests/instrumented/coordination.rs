//! Repeatable measurements of guest coordination, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_native::{ForeignBindings, test_support::*},
    curios_profile::{Destination, fold, trace},
    std::{
        fs::{File, create_dir_all},
        io::{BufReader, BufWriter},
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
    xbench::{Measure, read, read_elapsed, turn},
};

/// Measure the compilation and execution of guest coordination.
///
/// Guest coordination is write-once cells, bounded channels and level waiting. This is a local native measurement, separate from the containerized cross-language series.
///
/// ## Workloads and reproduction
///
/// `cargo xbench measure coordination_measurements` takes it, a group an invocation: an untimed round of each workload, then five rounds, the workloads turned round-robin. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the measurement starts and are excluded. Its figures are `<workload>/compile` and `<workload>/run`, each its span's time, and `<workload>/output`, the bytes the workload wrote.
///
/// - **`monad_suspend`:** the existing `programs/monad_suspend.crs` corpus program, with the argument `10000`, requiring stdout `30000\n` and no stderr. This measures a suspension's path through the driver every combinator is written over, one a step, not contention among branches.
/// - **`tui`:** the existing bordered listing fixture in `curios/src/tests/tui.rs`, driven by 100 down/up pairs followed by `q`, the keys pausing twice between two whole keys so the session's size ticker runs once at each pause. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.
///
/// `curios_profile::trace` listens during compilation and execution, writing one stream per sample into a timestamped directory under `curios/.artifacts/coordination/`. Each stream is read with `curios profile`. The `coordination_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `coordination_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The allocator counts while the trace listens, so each boundary of a stream carries what its thread had taken there and what the whole test process held, since counting began: a span is charged its own thread's allocations, and none of Wasmtime's, Binaryen's or Cranelift's threads'.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The Tui workload has synthetic time and no physical terminal latency.
///
/// ## What it last printed
///
/// Timed, taken on top of `9a18c2491` on 2026-10-23. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc3.1.fc44.x86_64`; Rust `1.99.1` (`58cf8f9c3`). Cargo release profile, all features, one test thread, no CPU pinning, CPU governor `performance`, boost enabled. Default runtime engine configuration. The one-minute load average is 1.00 before the instrument and 2.60 after it; every Rust build finishes before timing.
///
/// All times are milliseconds. Sample 0 is the warmup and is excluded from the medians and observed spans.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_suspend` | 0 (warmup) | 702.711 | 17.636 | 6 |
/// | `monad_suspend` | 1 | 663.383 | 17.558 | 6 |
/// | `monad_suspend` | 2 | 663.427 | 17.540 | 6 |
/// | `monad_suspend` | 3 | 657.640 | 17.181 | 6 |
/// | `monad_suspend` | 4 | 649.868 | 17.288 | 6 |
/// | `monad_suspend` | 5 | 652.808 | 17.589 | 6 |
/// | `tui` | 0 (warmup) | 7337.520 | 59.785 | 8747 |
/// | `tui` | 1 | 7546.438 | 63.552 | 8747 |
/// | `tui` | 2 | 7543.518 | 60.639 | 8747 |
/// | `tui` | 3 | 7531.747 | 60.206 | 8747 |
/// | `tui` | 4 | 7455.739 | 61.362 | 8747 |
/// | `tui` | 5 | 7541.695 | 60.992 | 8747 |
///
/// | Workload | Compile median [span], ms | Run median [span], ms |
/// | --- | ---: | ---: |
/// | `monad_suspend` | 657.640 [649.868–663.427] | 17.540 [17.181–17.589] |
/// | `tui` | 7541.695 [7455.739–7546.438] | 60.992 [60.206–63.552] |
///
/// All 12 captures pass the behavior and closed-span assertions; each workload produces the same output length in every sample. The Tui assertions include terminal restoration and all three screen clears.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
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

    let (mut monads, mut tuis) = (0, 0);
    let mut monad = || {
        sampled(
            &directory,
            "monad_suspend",
            include_str!("../../../../programs/monad_suspend.crs"),
            monads,
        );
        monads += 1;
    };
    let mut tui = || {
        sampled(&directory, "tui", LISTING_PROGRAM, tuis);
        tuis += 1;
    };
    turn(5, &mut [&mut monad, &mut tui]);
}

/// One sample of `workload`, the `sample`th: compiled and run under the recorder, its stream filed in `directory`, and its compile, its run and its output read as its figures.
fn sampled(directory: &Path, workload: &str, source: &str, sample: usize) {
    let (host, io) = if workload == "tui" {
        listing_host()
    } else {
        MockHost::builder().args(["monad_suspend", "10000"]).build()
    };
    let path = directory.join(format!("{workload}-{sample}.pftrace"));
    let sink = BufWriter::new(File::create(&path).expect("capture file"));
    trace(Destination::Stream(Box::new(sink)), TALLIES, || {
        let compiled = curios_common::profile!("coordination_compile" => compile(source))
            .expect("workload compiles");
        let status = curios_common::profile!("coordination_run" => compiled.run(host, ForeignBindings::empty()))
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
    for (span, figure) in [
        ("coordination_compile", "compile"),
        ("coordination_run", "run"),
    ] {
        let summary = report
            .summaries
            .iter()
            .find(|row| row.name == span)
            .unwrap();
        assert_eq!(summary.calls, 1);
        read_elapsed(&format!("{workload}/{figure}"), None, summary.total);
    }
    read(
        &format!("{workload}/output"),
        Measure::Count,
        None,
        io.output().len() as u64,
    );
}
