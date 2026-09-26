//! Repeatable before-and-after measurements for the host and guest boundary, with compilation and execution captured separately. The measurement below owns its workload settings and individual results.

use {
    super::{
        compile,
        coordination::{assert_listing, listing_host},
        tui::LISTING_PROGRAM,
    },
    curios_profile::{Destination, fold, trace},
    curios_runtime::{MockHost, MockIo},
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
        | chunk(bytes) =>
            let _ = Io/write(Io/stdout, bytes)!;
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

/// Measure compilation and execution around the host and guest boundary's checked adapters and outcomes.
///
/// This capture compares compilation and execution around the checked host and guest boundary — [a host operation has one contract, checked at both ends](../../../documentation/design/toolchain/a-host-operation-has-one-contract-checked-at-both-ends.md): the checked host adapter, the operation contracts, guest reply validation and the `/sys` outcomes built over wire-shaped calls. It is a local native measurement, separate from the containerized cross-language series.
///
/// ## Workloads and reproduction
///
/// ```text
/// cargo test --release --package curios --lib --all-features -- --ignored --nocapture host_boundary_measurements --test-threads=1
/// ```
///
/// The instrument runs one warmup (sample 0) and five measured samples per workload, in order: `monad_io`, then `stream_copy`, then `tui`. Each sample compiles the source afresh against the embedded prelude and executes it once with a fresh scripted host. There is no package-cache lookup. Rust compilation and prelude construction finish before the instrument starts and are excluded.
///
/// - **`monad_io`:** the existing `programs/monad_io.crs` corpus program, with stdin `10000\n`, requiring stdout `30000\n` and no stderr. It measures the `Io` carrier's per-`bind` cost, which every generated `/sys` adapter adds to, and reads its line a byte at a time.
/// - **`stream_copy`:** 2,000 chunks of 256 bytes on standard input, copied to standard output through `Io/read` and `Io/write`, requiring the output to equal the input and no stderr. The scripted host answers `would_block` between chunks, so every chunk costs a read that blocks, a poll, a read that succeeds and a write.
/// - **`tui`:** the bordered listing fixture `curios/src/tests/coordination.rs` measures, with its host and assertions: reads, writes, polls and terminal-size queries through a whole session.
///
/// `curios_profile::trace` listens during compilation and execution, writing one unrotated stream per sample into a timestamped directory under `curios/.artifacts/host_boundary/`. Each stream has a folded `.tsv` beside it. The `host_boundary_compile` span includes parsing, all compiler stages, Binaryen and Cranelift precompilation. The `host_boundary_run` span includes deserialization, instantiation and execution. Host setup, output assertions and folding are outside those spans. All span boundaries must close. The test binary has no counting allocator; memory columns in the folded reports are zero and are not allocation evidence.
///
/// These are wall-clock timings under built-in tracing, with its overhead included. The process is not CPU-pinned and frequency scaling remains enabled. One warmup does not eliminate machine noise; retain the individual samples, use the same workload and profiling configuration for the comparison, and investigate changes before drawing conclusions.
///
/// ## Baseline
///
/// Captured on 2026-09-25, on `main` at `fdc478223dea6716a3e17231cfc8a3a5546a2032` plus the measurement harness in this checkpoint: `Byte` crosses the wire, builtins are named by their roster identity, exit is a diverging row and `/sys/Result` is present; host replies are still unchecked on both sides and `/sys` still hands `/std` raw status records.
///
/// Host: Apple M4 Pro (12 cores), `aarch64-apple-darwin`, Darwin `25.6.0`; Rust `1.95.0 (59807616e 2026-04-14)`. Cargo release profile, all features, one test thread, default runtime engine configuration.
///
/// All times below are milliseconds. Sample 0 is the warmup and is excluded from the medians.
///
/// | Workload | Sample | Compile | Run | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_io` | 0 (warmup) | 793.883 | 1.043 | 6 |
/// | `monad_io` | 1 | 708.643 | 0.590 | 6 |
/// | `monad_io` | 2 | 707.303 | 0.605 | 6 |
/// | `monad_io` | 3 | 705.960 | 0.589 | 6 |
/// | `monad_io` | 4 | 709.292 | 0.596 | 6 |
/// | `monad_io` | 5 | 707.670 | 0.623 | 6 |
/// | `stream_copy` | 0 (warmup) | 440.585 | 29.091 | 512000 |
/// | `stream_copy` | 1 | 445.554 | 29.642 | 512000 |
/// | `stream_copy` | 2 | 437.811 | 29.673 | 512000 |
/// | `stream_copy` | 3 | 434.638 | 29.518 | 512000 |
/// | `stream_copy` | 4 | 439.462 | 31.043 | 512000 |
/// | `stream_copy` | 5 | 438.405 | 28.847 | 512000 |
/// | `tui` | 0 (warmup) | 2780.869 | 20.820 | 11368 |
/// | `tui` | 1 | 2786.750 | 21.602 | 11368 |
/// | `tui` | 2 | 2765.944 | 19.957 | 11368 |
/// | `tui` | 3 | 2757.135 | 20.872 | 11368 |
/// | `tui` | 4 | 2769.737 | 20.624 | 11368 |
/// | `tui` | 5 | 2771.243 | 27.216 | 11368 |
///
/// Measured medians: `monad_io` compile **707.670 ms**, run **0.596 ms**; `stream_copy` compile **438.405 ms**, run **29.642 ms**; `tui` compile **2769.737 ms**, run **20.872 ms**. Tui execution ranges from 19.957 to 27.216 ms, so a later comparison must account for that spread. All eighteen samples passed their behavior and closed-span assertions; each workload produced the same output length in every sample.
///
/// Local raw and folded captures: `curios/.artifacts/host_boundary/1790363426131510000/`, with files named `<workload>-<sample>.trace` and `<workload>-<sample>.tsv`. These generated artifacts are not committed; the protocol and individual timings above are the durable baseline.
///
/// ## After
///
/// Captured on 2026-09-25, on `main` at `068c4e182267f4175d12db8f8c5c7b895e142f23`: every host reply checked by the native adapter and by the guest, the operation contracts in force on every host, and `/sys` reading each fallible row as a `Result` or `Option` that `/std` names. Same host, toolchain and protocol as the baseline.
///
/// | Workload | Sample | Compile | Run | Output bytes |
/// | --- | ---: | ---: | ---: | ---: |
/// | `monad_io` | 0 (warmup) | 825.605 | 1.093 | 6 |
/// | `monad_io` | 1 | 739.081 | 0.631 | 6 |
/// | `monad_io` | 2 | 737.592 | 0.638 | 6 |
/// | `monad_io` | 3 | 737.749 | 0.657 | 6 |
/// | `monad_io` | 4 | 736.749 | 0.635 | 6 |
/// | `monad_io` | 5 | 735.665 | 0.651 | 6 |
/// | `stream_copy` | 0 (warmup) | 405.444 | 27.936 | 512000 |
/// | `stream_copy` | 1 | 400.891 | 28.058 | 512000 |
/// | `stream_copy` | 2 | 401.952 | 27.990 | 512000 |
/// | `stream_copy` | 3 | 401.136 | 27.904 | 512000 |
/// | `stream_copy` | 4 | 398.554 | 28.260 | 512000 |
/// | `stream_copy` | 5 | 400.747 | 28.003 | 512000 |
/// | `tui` | 0 (warmup) | 2948.593 | 23.655 | 11368 |
/// | `tui` | 1 | 2984.907 | 21.664 | 11368 |
/// | `tui` | 2 | 2980.737 | 22.910 | 11368 |
/// | `tui` | 3 | 2965.527 | 21.523 | 11368 |
/// | `tui` | 4 | 2950.161 | 22.683 | 11368 |
/// | `tui` | 5 | 2942.890 | 20.914 | 11368 |
///
/// Measured medians, with the change from the baseline: `monad_io` compile **737.592 ms** (+4.2%), run **0.638 ms** (+0.042 ms); `stream_copy` compile **400.891 ms** (−8.6%), run **28.003 ms** (−5.5%); `tui` compile **2965.527 ms** (+7.1%), run **21.664 ms**, inside the baseline's 19.957–27.216 ms spread. All eighteen samples passed their behavior and closed-span assertions, with the baseline's output lengths.
///
/// The compile increases are reviewed and kept. Comparing the folded spans of `tui` sample 3 against the baseline's, the whole growth is in `cont_optimize` — 1757.1 to 1957.4 ms — spread across its passes in proportion to their size (`inline_known_calls` 251.1 to 299.8, `split_workers` 271.4 to 300.1, `split_parameters` 186.7 to 209.7), while parsing, elaboration, erasure, pruning and emission stay flat: the continuation module is larger by the code each fallible call now carries — `/sys`'s bind, continuation and status match — and `tui` reaches more rows than the other workloads. `stream_copy` compiles and runs faster because its read loop matches the row's `Result` directly where it built and took apart `Io/Chunk/of`'s `Option`. `monad_io`'s run pays for its byte-at-a-time reads, each now read through `/sys` and held to its row by the guest.
///
/// Local raw and folded captures: `curios/.artifacts/host_boundary/1790386533842688000/`, named as the baseline's are.
#[test]
#[ignore = "measurement: captures compilation and execution costs"]
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
        ("monad_io", include_str!("../../../programs/monad_io.crs")),
        ("stream_copy", STREAM_COPY),
        ("tui", LISTING_PROGRAM),
    ] {
        for sample in 0..=5 {
            let (host, io) = match workload {
                "monad_io" => MockHost::builder().stdin_lines(["10000"]).build(),
                "stream_copy" => stream_host(),
                _ => listing_host(),
            };
            let path = directory.join(format!("{workload}-{sample}.trace"));
            let sink = BufWriter::new(File::create(&path).expect("capture file"));
            trace(Destination::Stream(Box::new(sink)), || {
                let compiled = curios_profile::profile!("host_boundary_compile" => compile(source))
                    .expect("workload compiles");
                let status = curios_profile::profile!("host_boundary_run" => compiled.run(host))
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
