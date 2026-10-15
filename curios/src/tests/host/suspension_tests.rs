//! A program that suspends: the host's turn loop advancing it — through a timer, through a cell already filled, through the binds a suspension passes out of, through a cancel that suspends in turn, through a FIFO a branch waits on — and what it reports of one suspended on nothing it can wait for.

use {
    crate::run_wasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, HostOps, MockHost, OsHost},
    curios_text::{Entrypoint, RootSource},
};

/// The status a program ended with under `host`.
fn status<H: HostOps + Send + Sync + 'static>(source: &str, host: H) -> u8 {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    let (module, foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    run_wasm(&module, foreigns, host, ForeignBindings::empty()).expect("execution succeeded")
}

/// What a program came to under a scripted host: the status it ended with, everything it wrote, and the standard error half of that.
fn ended(source: &str) -> (u8, Vec<u8>, Vec<u8>) {
    let (system, io) = MockHost::builder().build();
    let code = status(source, system);

    (code, io.output(), io.errors())
}

#[test]
fn a_program_suspended_on_a_timer_wakes_into_its_rest() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Nat};
        use /std/time/{Duration};
        let woke = Io/bind(Io/sleep(Duration/of_millis(50)), (_) => Io/pure(7))!;
        /std/print(Nat/to_str(woke))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"7".as_slice()));
}

#[test]
fn a_suspension_passed_out_through_binds_resumes_into_their_continuations_in_order() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Nat};
        use /std/time/{Instant};
        let due = Instant/timer(Instant/now()!)!;
        let suspended = Io/bind(Io/wait([due]), (_) => Io/pure(1));
        let x = Io/bind(Io/bind(suspended, (a) => Io/pure(a + 1)), (b) => Io/pure(b * 10))!;
        /std/print(Nat/to_str(x))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"20".as_slice()));
}

// The host reads no cell, so the entry goes on itself into the rest of a suspension whose cell is already filled.
#[test]
fn a_program_suspended_on_a_filled_cell_goes_on_into_its_rest() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Cell};
        let told(ended: Io/Ended) -> Io({}) =
            /std/print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let cell = Cell/new()!;
        let _ = Cell/fill(cell, ())!;
        let _ = Io/using(told, Io/bind(Io/wait_filled([cell]), (_) => /std/print("resumed|")))!;
        /std/print("after|")
        "#,
    );

    assert_eq!(
        (code, output.as_slice()),
        (0, b"resumed|finished|after|".as_slice())
    );
}

// Only the program fills a cell, and nothing of it runs while the host waits, so a cell still empty at the top is no more a wait than no handle is.
#[test]
fn a_program_suspended_on_an_empty_cell_alone_ends_reporting_the_deadlock() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Cell};
        let told(ended: Io/Ended) -> Io({}) =
            /std/print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let cell = Cell/new()!;
        let _ = Io/using(told, Io/bind(Io/wait_filled([cell]), (_) => /std/print("resumed|")))!;
        /std/print("after|")
        "#,
    );

    assert_eq!(code, 1);
    assert_eq!(
        output,
        [b"cancelled|".as_slice(), curios_abi::DEADLOCK.as_bytes()].concat()
    );
}

// The host advances a cancel as it does a rest, so what the cancel answers is surfaced the same way: a release that suspends inside a bind keeps the bind's continuation.
#[test]
fn a_cancel_that_suspends_part_way_runs_to_its_end() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io};
        use /std/time/{Instant};
        let due = Instant/timer(Instant/now()!)!;
        let release(_: Io/Ended) -> Io({}) = Io/bind(Io/wait([due]), (_) => /std/print("abandoned|"));
        let _ = Io/using(release, Io/bind(Io/wait([]), (_) => /std/print("resumed|")))!;
        /std/print("after|")
        "#,
    );

    assert_eq!(code, 1);
    assert_eq!(
        output,
        [b"abandoned|".as_slice(), curios_abi::DEADLOCK.as_bytes()].concat()
    );
}

#[test]
fn a_program_suspended_on_no_handle_runs_its_cancel_and_ends_reporting_the_deadlock() {
    let (code, output, errors) = ended(
        r#"
        use /std/{Io};
        let _ = Io/using((_) => /std/print("abandoned|"), Io/bind(Io/wait([]), (_) => /std/print("resumed|")))!;
        /std/print("after|")
        "#,
    );

    assert_eq!(code, 1);
    assert_eq!(
        output,
        [b"abandoned|".as_slice(), curios_abi::DEADLOCK.as_bytes()].concat()
    );
    assert_eq!(errors, curios_abi::DEADLOCK.as_bytes());
}

// The real host sleeps in the kernel until the timer's instant; the program reads the clock on both sides of the wait and reports through a file, since the real host's standard output cannot be captured.
#[test]
fn the_real_host_sleeps_a_suspension_on_a_timer_until_its_instant() {
    let root = curios_utilities::test_support::Temporary::new("host", "suspension-timer");
    std::fs::create_dir_all(&root).expect("a directory of its own");
    let report = root.join("report");
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = format!(
        r#"
        use /std/{{Io, Str, Path, File, Try}};
        use /std/time/{{Instant, Duration}};
        let main: Try(Io, Io/Error, {{}}) =
            let start = Instant/now()!;
            let _ = Io/sleep(Duration/of_millis(30))!;
            let slept = Duration/to_millis(Instant/diff(Instant/now()!, start));
            File/write_all(Path/of_str("{path}"), Str/to_bytes(match slept >= 30 | true => "slept" | false => "early" end));
        main
        "#
    );

    // A write that fails closes the program with status 1, reporting why on standard error.
    assert_eq!(status(&source, OsHost::with_args(vec![])), 0);

    let written = std::fs::read(&report).expect("the program wrote its report");
    assert_eq!(written, b"slept");
}

// A FIFO opened by path waits on no writer: one branch reads it while the other writes a marker file, which the writer outside the program waits for before it opens the FIFO, so the read can end only where the other branch ran while it waited.
#[test]
fn the_real_host_runs_a_branch_while_another_waits_on_a_fifo() {
    let root = curios_utilities::test_support::Temporary::new("host", "suspension-fifo");
    std::fs::create_dir_all(&root).expect("a directory of its own");
    let (fifo, marker, report) = (root.join("fifo"), root.join("marker"), root.join("report"));
    let made = std::process::Command::new("mkfifo")
        .arg(&fifo)
        .status()
        .expect("mkfifo runs");
    assert!(made.success(), "mkfifo made the FIFO");

    let writer = {
        let (fifo, marker) = (fifo.clone(), marker.clone());
        std::thread::spawn(move || {
            // Bounded, so a read that holds the whole program fails the test rather than holding it.
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while !marker.exists() && std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(5));
            }
            let marked = marker.exists();
            let mut end = std::fs::OpenOptions::new()
                .write(true)
                .open(&fifo)
                .expect("the reader holds the FIFO open");
            std::io::Write::write_all(&mut end, b"data").expect("the FIFO takes four bytes");

            marked
        })
    };

    let spelled =
        |path: &std::path::Path| path.to_str().expect("a UTF-8 temporary path").to_string();
    let (fifo, marker, report) = (spelled(&fifo), spelled(&marker), spelled(&report));
    let source = format!(
        r#"
        use /std/{{Io, Str, Path, File, Try}};
        let main: Try(Io, Io/Error, {{}}) =
            let (read, marked) = Io/both(Try/run(File/read_all(Path/of_str("{fifo}"))), Try/run(File/write_all(Path/of_str("{marker}"), Str/to_bytes("ran"))))!;
            let _ = marked!;
            let bytes = read!;
            File/write_all(Path/of_str("{report}"), bytes);
        main
        "#
    );

    assert_eq!(status(&source, OsHost::with_args(vec![])), 0);
    assert!(
        writer.join().expect("the writer ends"),
        "the marker was written while the read waited"
    );
    assert_eq!(
        std::fs::read(&report).expect("the program wrote its report"),
        b"data"
    );
}

// Every turn allocates beside what one turn holds for the next, so collections run while the loop holds the rest and the abandon as owned roots: what crosses a turn must come back as it went.
#[test]
fn what_a_suspension_holds_survives_the_collections_between_its_turns() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Handle, Nat, List};
        use /std/time/{Instant};
        let turn(due: Handle, n: Nat, held: List(Nat)) -> Io(Nat) =
            match n: (_) => Io(Nat)
            | 0 => Io/pure(List/len(held))
            | m + 1 =>
                let garbage = List/range(0, 2000);
                let _ = Io/bind(Io/wait([due]), (_) => Io/pure(List/len(garbage)))!;
                turn(due, m, [n, ..held])
            end;
        let due = Instant/timer(Instant/now()!)!;
        let counted = turn(due, 3000, [])!;
        /std/print(Nat/to_str(counted))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"3000".as_slice()));
}
