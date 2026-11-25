//! Repeatable measurements of the host and guest boundary, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::*,
    crate::{Measure, phase, read, turn},
    curios::{TALLIES, test_support::*},
    curios_native::{ForeignBindings, test_support::*},
};

/// Standard input copied to standard output a chunk at a time, through the `Io` read and write the standard streams serve.
const STREAM_COPY: &str = r#"
    use /std/{Io, Try, Proc};
    use /std/Io/{Chunk};

    let copy() -> Io({}) =
        let c = Try/run(Io/read(Proc/stdin, 4096))!;
        match c
        | success(bytes(bytes, @_)) =>
            let _ = Try/run(Io/write(Proc/stdout, bytes))!;
            copy()
        | _ => Io/pure(())
        end;

    copy()
"#;

/// The copy's input: 2,000 chunks of 256 bytes, each filled with one letter.
fn stream_chunks() -> Vec<Vec<u8>> {
    (0..2000u32)
        .map(|index| vec![b'a' + (index % 26) as u8; 256])
        .collect()
}

fn stream_host() -> (MockHost, MockIo) {
    MockHost::builder().stdin_chunks(stream_chunks()).build()
}

fn assert_stream(io: &MockIo) {
    assert_eq!(io.output(), stream_chunks().concat());
    assert!(io.errors().is_empty());
}

/// Measure compilation and execution around the host and guest boundary: the adapter, the rows' requirements and the library's readings of their replies.
///
/// This capture times compilation and execution around the host and guest boundary — [a host operation is a foreign declaration whose types are its whole contract](../../../../documentation/design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md): the host's adapter, the rows' requirements and the readings `/std` makes of wire-shaped replies. It is a local native measurement, separate from the containerized cross-language series.
///
/// ## Workloads and reproduction
///
/// `cargo xbench measure host_boundary_measurements` takes it, a group an invocation: an untimed round of each workload, then five rounds, the workloads turned round-robin. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the measurement starts and are excluded. Its figures are `<workload>/compile` and `<workload>/run`, each its span's time, and `<workload>/output`, the bytes the workload wrote.
///
/// - **`monad_io`:** the existing `programs/monad_io.crs` corpus program, with the argument `10000`, requiring stdout `30000\n` and no stderr. It measures the `Io` carrier's per-`bind` cost, which every reading of a reply adds to.
/// - **`stream_copy`:** 2,000 chunks of 256 bytes on standard input, copied to standard output through `Io/read` and `Io/write`, requiring the output to equal the input and no stderr. The scripted host answers `would_block` between chunks, so every chunk costs a read that blocks, a poll, a read that succeeds and a write.
/// - **`tui`:** the bordered listing fixture `coordination.rs` measures beside this file, with the host and assertions the two share: reads, writes, polls and terminal-size queries through a whole session.
///
/// A sample's compile and its run are two phases under the recorder: the compile includes parsing, all compiler stages, Binaryen and Cranelift precompilation, and the run deserialization, instantiation and execution. Host setup and output assertions are outside them. A phase is charged its own thread's allocations, and none of Wasmtime's, Binaryen's or Cranelift's threads'.
///
/// These are wall-clock timings under built-in tracing, with its overhead included.
///
/// ## Last reading
///
/// Timed, taken on top of `9a18c2491` on 2026-10-23. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc3.1.fc44.x86_64`; Rust `1.99.1` (`58cf8f9c3`). Cargo release profile, all features, one test thread, no CPU pinning, CPU governor `performance`, boost enabled. Default runtime engine configuration. The one-minute load average is 1.66 before the instrument and 1.58 after it; every Rust build finishes before timing.
///
/// All times are milliseconds. Sample 0 is the warmup and is excluded from the medians and observed spans.
///
/// | Workload | Sample | Compile (ms) | Run (ms) | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_io` | 0 (warmup) | 457.769 | 0.724 | 6 |
/// | `monad_io` | 1 | 405.472 | 0.626 | 6 |
/// | `monad_io` | 2 | 408.198 | 0.635 | 6 |
/// | `monad_io` | 3 | 410.798 | 0.613 | 6 |
/// | `monad_io` | 4 | 409.711 | 0.552 | 6 |
/// | `monad_io` | 5 | 407.037 | 0.631 | 6 |
/// | `stream_copy` | 0 (warmup) | 132.299 | 16.838 | 512000 |
/// | `stream_copy` | 1 | 123.333 | 16.203 | 512000 |
/// | `stream_copy` | 2 | 122.618 | 16.829 | 512000 |
/// | `stream_copy` | 3 | 124.316 | 16.858 | 512000 |
/// | `stream_copy` | 4 | 127.507 | 16.310 | 512000 |
/// | `stream_copy` | 5 | 125.570 | 16.541 | 512000 |
/// | `tui` | 0 (warmup) | 7338.942 | 60.602 | 8747 |
/// | `tui` | 1 | 7497.237 | 60.432 | 8747 |
/// | `tui` | 2 | 7585.840 | 60.534 | 8747 |
/// | `tui` | 3 | 7520.488 | 60.042 | 8747 |
/// | `tui` | 4 | 7515.068 | 61.248 | 8747 |
/// | `tui` | 5 | 7512.041 | 62.259 | 8747 |
///
/// | Workload | Compile median \[span\], ms | Run median \[span\], ms |
/// | --- | ---: | ---: |
/// | `monad_io` | 408.198 [405.472–410.798] | 0.626 [0.552–0.635] |
/// | `stream_copy` | 124.316 [122.618–127.507] | 16.541 [16.203–16.858] |
/// | `tui` | 7515.068 [7497.237–7585.840] | 60.534 [60.042–62.259] |
///
/// All 18 captures pass the behavior and closed-span assertions; each workload produces the same output length in every sample. The Tui assertions include terminal restoration and all three screen clears.
pub fn host_boundary_measurements() {
    let mut monad = || sampled("monad_io", include_str!("../../../programs/monad_io.crs"));
    let mut stream = || sampled("stream_copy", STREAM_COPY);
    let mut tui = || sampled("tui", LISTING_PROGRAM);
    turn(5, &mut [&mut monad, &mut stream, &mut tui]);
}

/// One sample of `workload`: compiled and run, each a phase, and its compile, its run and its output read as its figures.
fn sampled(workload: &str, source: &str) {
    let (host, io) = match workload {
        "monad_io" => MockHost::builder().args(["program", "10000"]).build(),
        "stream_copy" => stream_host(),
        _ => listing_host(),
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
    match workload {
        "monad_io" => {
            assert_eq!(io.output(), b"30000\n");
            assert!(io.errors().is_empty());
        }
        "stream_copy" => assert_stream(&io),
        _ => assert_listing(&io),
    }

    read(
        &format!("{workload}/output"),
        Measure::Count,
        None,
        io.output().len() as u64,
    );
}
