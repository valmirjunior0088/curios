//! Repeatable measurements of the host and guest boundary, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::test_support::*,
    crate::tests::{LISTING_PROGRAM, compile},
    curios_profile::{Destination, fold, trace},
    curios_runtime::{ForeignBindings, MockHost, MockIo},
    std::{
        fs::{File, create_dir_all, write},
        io::{BufReader, BufWriter},
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// Standard input copied to standard output a chunk at a time, through the `Io` read and write the standard streams serve.
const STREAM_COPY: &str = r#"
    use /std/{Io};
    use /std/Io/{Chunk};

    let copy() -> Io({}) =
        let c = Io/read(Io/stdin, 4096)!;
        match c
        | chunk(bytes, @_) =>
            let _ = Io/Write/write(Io/stdout, bytes)!;
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
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions.
///
/// ## Last reading
///
/// Timed, taken on top of `a439dba6a`. Host: AMD Ryzen 7 5800X3D (8 cores, 16 logical CPUs), `x86_64-unknown-linux-gnu`, Linux `7.2.9-ogc3.1.fc44.x86_64`; Rust `1.95.0`. Cargo release profile, all features, one test thread, default runtime engine configuration.
///
/// All times below are milliseconds. Sample 0 is the warmup and is excluded from the medians.
///
/// | Workload | Sample | Compile | Run | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_io` | 0 (warmup) | 521.382 | 0.682 | 6 |
/// | `monad_io` | 1 | 488.423 | 0.599 | 6 |
/// | `monad_io` | 2 | 487.174 | 0.594 | 6 |
/// | `monad_io` | 3 | 489.343 | 0.596 | 6 |
/// | `monad_io` | 4 | 489.763 | 0.592 | 6 |
/// | `monad_io` | 5 | 485.862 | 0.596 | 6 |
/// | `stream_copy` | 0 (warmup) | 168.671 | 16.175 | 512000 |
/// | `stream_copy` | 1 | 127.396 | 16.418 | 512000 |
/// | `stream_copy` | 2 | 126.147 | 16.053 | 512000 |
/// | `stream_copy` | 3 | 128.208 | 16.682 | 512000 |
/// | `stream_copy` | 4 | 128.350 | 17.707 | 512000 |
/// | `stream_copy` | 5 | 131.102 | 16.229 | 512000 |
/// | `tui` | 0 (warmup) | 5071.385 | 46.844 | 11368 |
/// | `tui` | 1 | 4976.005 | 50.437 | 11368 |
/// | `tui` | 2 | 4995.716 | 45.913 | 11368 |
/// | `tui` | 3 | 5000.773 | 56.292 | 11368 |
/// | `tui` | 4 | 4993.267 | 53.516 | 11368 |
/// | `tui` | 5 | 5020.888 | 46.392 | 11368 |
///
/// Measured medians: `monad_io` compile **488.423 ms**, run **0.596 ms**; `stream_copy` compile **128.208 ms**, run **16.418 ms**; `tui` compile **4995.716 ms**, run **50.437 ms**. All eighteen samples passed their behavior and closed-span assertions; each workload produced the same output length in every sample.
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
