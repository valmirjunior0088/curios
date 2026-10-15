//! A program that suspends: the host's turn loop advancing it — through a timer, through a cell already filled, through the binds a suspension passes out of, through a cancel that suspends in turn, through a FIFO a branch waits on, through an embedder's row answered later — and what it reports of one suspended on nothing it can wait for.

use {
    crate::run_wasm,
    curios_abi::ForeignStore,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, HostOps, MockHost, OsHost, blocking},
    curios_text::{Entrypoint, RootSource},
};

/// A program compiled, ready to run.
fn compiled(source: &str) -> (curios_wasm::Module, ForeignStore) {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded")
}

/// The status a compiled program ended with under `host`, `bindings` answering its own rows.
fn ran<H: HostOps + Send + Sync + 'static>(
    (module, foreigns): (curios_wasm::Module, ForeignStore),
    host: H,
    bindings: ForeignBindings,
) -> u8 {
    run_wasm(&module, foreigns, host, bindings).expect("execution succeeded")
}

/// The status a program ended with under `host`.
fn status<H: HostOps + Send + Sync + 'static>(source: &str, host: H) -> u8 {
    ran(compiled(source), host, ForeignBindings::empty())
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

    let spelled =
        |path: &std::path::Path| path.to_str().expect("a UTF-8 temporary path").to_string();
    let source = format!(
        r#"
        use /std/{{Io, Str, Path, File, Try}};
        let main: Try(Io, Io/Error, {{}}) =
            let (read, marked) = Io/both(Try/run(File/read_all(Path/of_str("{}"))), Try/run(File/write_all(Path/of_str("{}"), Str/to_bytes("ran"))))!;
            let _ = marked!;
            let bytes = read!;
            File/write_all(Path/of_str("{}"), bytes);
        main
        "#,
        spelled(&fifo),
        spelled(&marker),
        spelled(&report),
    );
    // Compiled before the writer starts its clock, so the deadline bounds the run alone.
    let program = compiled(&source);

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

    assert_eq!(
        ran(program, OsHost::with_args(vec![]), ForeignBindings::empty()),
        0
    );
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

/// How long the work behind a row answered later waits for what the test holds it to, before it answers what says it never came.
const PATIENCE: std::time::Duration = std::time::Duration::from_secs(10);

/// Wait until `came` holds, for at most [`PATIENCE`], and say whether it did.
fn came(came: impl Fn() -> bool) -> bool {
    let deadline = std::time::Instant::now() + PATIENCE;
    while !came() && std::time::Instant::now() < deadline {
        std::thread::sleep(std::time::Duration::from_millis(5));
    }

    came()
}

// An embedder's row answered later suspends its call alone: the work behind it answers only once the other branch's tick is in the output, so a run that waited on the row with the rest of the program held answers `0`.
#[test]
fn a_branch_ticks_while_an_embedder_s_row_is_answered_later() {
    let (system, io) = MockHost::builder().build();
    let io = std::sync::Arc::new(io);
    let mut bindings = ForeignBindings::empty();
    bindings.define_async("/fetch", {
        let io = std::sync::Arc::clone(&io);

        move |n: u64| {
            let io = std::sync::Arc::clone(&io);

            blocking(move || match came(|| io.output().starts_with(b"tick ")) {
                true => n * 2,
                false => 0,
            })
        }
    });

    let program = compiled(
        r#"
        use /std/{Io, Nat};
        use /std/time/{Duration};
        foreign fetch(Nat) -> Nat;
        let tick = Io/bind(Io/sleep(Duration/of_millis(5)), (_) => /std/print("tick "));
        let (n, _) = Io/both(fetch(3), tick)!;
        /std/print(Nat/to_str(n))
        "#,
    );

    assert_eq!(ran(program, system, bindings), 0);
    assert_eq!(io.output(), b"tick 6");
}

// The real host files a row answered later as it files a timer: one branch waits on an embedder's row while the other writes a marker file, which the work behind the row waits for, so the row can answer only where the other branch ran while it waited.
#[test]
fn the_real_host_runs_a_branch_while_another_waits_on_a_row_answered_later() {
    let root = curios_utilities::test_support::Temporary::new("host", "suspension-later");
    std::fs::create_dir_all(&root).expect("a directory of its own");
    let (marker, report) = (root.join("marker"), root.join("report"));

    let spelled =
        |path: &std::path::Path| path.to_str().expect("a UTF-8 temporary path").to_string();
    let program = compiled(&format!(
        r#"
        use /std/{{Io, Str, Path, File, Try, Nat}};
        foreign fetch(Nat) -> Nat;
        let main: Try(Io, Io/Error, {{}}) =
            let (n, marked) = Io/both(fetch(3), Try/run(File/write_all(Path/of_str("{}"), Str/to_bytes("ran"))))!;
            let _ = marked!;
            File/write_all(Path/of_str("{}"), Str/to_bytes(Nat/to_str(n)));
        main
        "#,
        spelled(&marker),
        spelled(&report),
    ));

    let mut bindings = ForeignBindings::empty();
    bindings.define_async("/fetch", move |n: u64| {
        let marker = marker.clone();

        blocking(move || match came(|| marker.exists()) {
            true => n * 2,
            false => 0,
        })
    });

    assert_eq!(ran(program, OsHost::with_args(vec![]), bindings), 0);
    assert_eq!(
        std::fs::read(&report).expect("the program wrote its report"),
        b"6"
    );
}

// A call cancelled while its answer is pending drops the future behind it: the timeout's cancel drops the call's token through its row, and the host lets go of what the call waited on.
#[test]
fn a_call_cancelled_while_its_answer_is_pending_drops_the_future() {
    struct Held(std::sync::Arc<std::sync::atomic::AtomicBool>);

    impl Drop for Held {
        fn drop(&mut self) {
            self.0.store(true, std::sync::atomic::Ordering::Release);
        }
    }

    let dropped = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let mut bindings = ForeignBindings::empty();
    bindings.define_async("/hang", {
        let dropped = std::sync::Arc::clone(&dropped);

        move |_: u64| {
            let held = Held(std::sync::Arc::clone(&dropped));

            async move {
                let _held = held;

                std::future::pending::<u64>().await
            }
        }
    });

    let (system, io) = MockHost::builder().build();
    let program = compiled(
        r#"
        use /std/{Io};
        use /std/time/{Duration};
        foreign hang(Nat) -> Nat;
        let came = Io/timeout(Duration/of_millis(10), hang(1))!;
        /std/print(match came | some(_) => "came" | none() => "timed out" end)
        "#,
    );

    assert_eq!(ran(program, system, bindings), 0);
    assert_eq!(io.output(), b"timed out");
    assert!(dropped.load(std::sync::atomic::Ordering::Acquire));
}
