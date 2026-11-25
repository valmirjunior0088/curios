//! Repeatable measurements of guest coordination, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::*,
    crate::{Measure, phase, read, turn},
    curios::{TALLIES, test_support::*},
    curios_native::{ForeignBindings, test_support::*},
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
/// - **`tui`:** the bordered listing `curios`'s test support holds (`curios/src/test_support/listing.rs`), which the terminal suites drive too, driven by 100 down/up pairs followed by `q`, the keys pausing twice between two whole keys so the session's size ticker runs once at each pause. The first down escape sequence arrives in two chunks; each remaining key has its own chunk. Terminal-size queries answer 12×5, 16×6, then 12×5 forever. Monotonic-clock queries advance by 1 ms, starting at zero, with 100,000 scripted readings. The mock host releases the next input chunk when polled and does not sleep in real time. Assertions require terminal entry and restoration, raw mode on then off, selection `one`, a titled border, exactly three screen clears (initial frame and two resizes), no stderr, and a successful exit.
///
/// A sample's compile and its run are two phases under the recorder: the compile includes parsing, all compiler stages, Binaryen and Cranelift precompilation, and the run deserialization, instantiation and execution. Host setup and output assertions are outside them. A phase is charged its own thread's allocations, and none of Wasmtime's, Binaryen's or Cranelift's threads'.
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
/// | Workload | Compile median \[span\], ms | Run median \[span\], ms |
/// | --- | ---: | ---: |
/// | `monad_suspend` | 657.640 [649.868–663.427] | 17.540 [17.181–17.589] |
/// | `tui` | 7541.695 [7455.739–7546.438] | 60.992 [60.206–63.552] |
///
/// All 12 captures pass the behavior and closed-span assertions; each workload produces the same output length in every sample. The Tui assertions include terminal restoration and all three screen clears.
pub fn coordination_measurements() {
    let mut monad = || {
        sampled(
            "monad_suspend",
            include_str!("../../../programs/monad_suspend.crs"),
        )
    };
    let mut tui = || sampled("tui", LISTING_PROGRAM);
    turn(5, &mut [&mut monad, &mut tui]);
}

/// One sample of `workload`: compiled and run, each a phase, and its compile, its run and its output read as its figures.
fn sampled(workload: &str, source: &str) {
    let (host, io) = if workload == "tui" {
        listing_host()
    } else {
        MockHost::builder().args(["monad_suspend", "10000"]).build()
    };
    let compiled = phase(TALLIES, || compile(source).expect("workload compiles"));
    compiled.read(&format!("{workload}/compile"), None);
    let ran = phase(TALLIES, || {
        compiled
            .answer
            .run(host, ForeignBindings::empty())
            .expect("workload runs")
    });
    assert_eq!(ran.answer, 0);
    ran.read(&format!("{workload}/run"), None);
    if workload == "tui" {
        assert_listing(&io);
    } else {
        assert_eq!(io.output(), b"30000\n");
        assert!(io.errors().is_empty());
    }

    read(
        &format!("{workload}/output"),
        Measure::Count,
        None,
        io.output().len() as u64,
    );
}
