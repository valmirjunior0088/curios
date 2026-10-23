//! Repeatable measurements of the host and guest boundary, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::test_support::*,
    crate::tests::{LISTING_PROGRAM, compile},
    curios_native::{ForeignBindings, MockHost, MockIo},
    curios_profile::{Destination, fold, trace},
    std::{
        fs::{File, create_dir_all, write},
        io::{BufReader, BufWriter},
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// Standard input copied to standard output a chunk at a time, through the `Io` read and write the standard streams serve.
const STREAM_COPY: &str = r#"
    use /std/{Io, Try, Process};
    use /std/Io/{Chunk};

    let copy() -> Io({}) =
        let c = Try/run(Io/read(Process/stdin, 4096))!;
        match c
        | success(bytes(bytes, @_)) =>
            let _ = Try/run(Io/write(Process/stdout, bytes))!;
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
/// ```text
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture host_boundary_measurements --test-threads=1
/// ```
///
/// The instrument runs one warmup (sample 0) and five measured samples per workload, in order: `monad_io`, then `stream_copy`, then `tui`. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the instrument starts and are excluded.
///
/// - **`monad_io`:** the existing `programs/monad_io.crs` corpus program, with the argument `10000`, requiring stdout `30000\n` and no stderr. It measures the `Io` carrier's per-`bind` cost, which every reading of a reply adds to.
/// - **`stream_copy`:** 2,000 chunks of 256 bytes on standard input, copied to standard output through `Io/read` and `Io/write`, requiring the output to equal the input and no stderr. The scripted host answers `would_block` between chunks, so every chunk costs a read that blocks, a poll, a read that succeeds and a write.
/// - **`tui`:** the bordered listing fixture `coordination.rs` measures beside this file, with the host and assertions the two share: reads, writes, polls and terminal-size queries through a whole session.
///
/// `curios_profile::trace` listens during compilation and execution, writing one stream per sample into a timestamped directory under `curios/.artifacts/host_boundary/`. Each stream has a folded `.tsv` beside it. The `host_boundary_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `host_boundary_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The `profile` feature installs `curios-profile`'s counting allocator, so each boundary row of a stream carries what the whole test process held and had taken there, and not what its span allocated alone.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned; its CPU governor and boost settings are recorded with the reading. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions.
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
/// | Workload | Compile median [span], ms | Run median [span], ms |
/// | --- | ---: | ---: |
/// | `monad_io` | 408.198 [405.472–410.798] | 0.626 [0.552–0.635] |
/// | `stream_copy` | 124.316 [122.618–127.507] | 16.541 [16.203–16.858] |
/// | `tui` | 7515.068 [7497.237–7585.840] | 60.534 [60.042–62.259] |
///
/// All 18 captures pass the behavior and closed-span assertions; each workload produces the same output length in every sample. The Tui assertions include terminal restoration and all three screen clears.
#[test]
#[ignore = "measurement, timed: captures compilation and execution costs"]
fn host_boundary_measurements() {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(".artifacts/host_boundary")
        .join(stamp.to_string());
    create_dir_all(&directory).expect("capture directory");
    println!("captures: {}", directory.display());
    println!("workload\tsample\tcompile_ms\trun_ms\toutput_bytes");

    for (workload, source) in [
        (
            "monad_io",
            include_str!("../../../../programs/monad_io.crs"),
        ),
        ("stream_copy", STREAM_COPY),
        ("tui", LISTING_PROGRAM),
    ] {
        for sample in 0..=5 {
            let (host, io) = match workload {
                "monad_io" => MockHost::builder().args(["program", "10000"]).build(),
                "stream_copy" => stream_host(),
                _ => listing_host(),
            };
            let path = directory.join(format!("{workload}-{sample}.trace"));
            let sink = BufWriter::new(File::create(&path).expect("capture file"));
            trace(Destination::Stream(Box::new(sink)), || {
                let compiled = curios_profile::profile!("host_boundary_compile" => compile(source))
                    .expect("workload compiles");
                let status = curios_profile::profile!("host_boundary_run" => compiled.run(host, ForeignBindings::empty()))
                    .expect("workload runs");
                assert_eq!(status, 0);
            })
            .expect("capture opens");
            match workload {
                "monad_io" => {
                    assert_eq!(io.output(), b"30000\n");
                    assert!(io.errors().is_empty());
                }
                "stream_copy" => assert_stream(&io),
                _ => assert_listing(&io),
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
                millis("host_boundary_compile"),
                millis("host_boundary_run"),
                io.output().len(),
            );
        }
    }
}
