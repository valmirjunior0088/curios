//! Repeatable before-and-after measurements for guest coordination, with compilation and execution captured separately. Workload settings and individual results belong to `benchmarks/guest-coordination.md`.

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

/// Run with `cargo test --release --package curios --lib --all-features -- --ignored --nocapture coordination_measurements --test-threads=1`. Each workload gets one warmup and five measured samples, recompiling each time. The test binary has no counting allocator, so only timing columns are meaningful.
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
