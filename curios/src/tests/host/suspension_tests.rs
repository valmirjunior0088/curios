//! A program that suspends: the host's turn loop advancing it — through a timer, through the binds a suspension passes out of, through `Io/advance` — and what it reports of one suspended on nothing.

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
        use /std/{Io, Handle, Nat};
        use /std/time/{Instant, Duration};
        let start = Instant/now()!;
        let timer = Instant/timer(Instant/add(start, Duration/of_millis(50)))!;
        let woke = Io/suspend([timer], Handle/close(timer), Io/pure(7))!;
        /std/print(Nat/to_str(woke))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"7".as_slice()));
}

#[test]
fn a_suspension_passed_out_through_binds_resumes_into_their_continuations_in_order() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Handle, Nat};
        let suspended = Io/suspend([Handle/ready], Io/pure(()), Io/pure(1));
        let x = Io/bind(Io/bind(suspended, (a) => Io/pure(a + 1)), (b) => Io/pure(b * 10))!;
        /std/print(Nat/to_str(x))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"20".as_slice()));
}

#[test]
fn advance_hands_a_suspension_to_its_handler_and_a_value_to_its_continuation() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Handle, Nat, Str};
        let suspended = Io/suspend([Handle/ready], Io/pure(()), Io/pure(7));
        let caught = Io/advance(suspended, (n) => Io/pure(n + 1), (_, _, rest) => Io/bind(rest, (n) => Io/pure(n + 100)))!;
        let passed = Io/advance(Io/pure(3), (n) => Io/pure(n + 1), (_, _, rest) => rest)!;
        /std/print(Str/flatten([Nat/to_str(caught), " ", Nat/to_str(passed)]))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"107 4".as_slice()));
}

#[test]
fn a_program_suspended_on_no_handle_runs_its_abandon_and_ends_reporting_the_deadlock() {
    let (code, output, errors) = ended(
        r#"
        use /std/{Io};
        let _ = Io/suspend([], /std/print("abandoned|"), /std/print("resumed|"))!;
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
        use /std/{{Io, Handle, Str, Path, File, Try}};
        use /std/time/{{Instant, Duration}};
        let main: Try(Io, Io/Error, {{}}) =
            let start = Instant/now()!;
            let timer = Instant/timer(Instant/add(start, Duration/of_millis(30)))!;
            let _ = Io/suspend([timer], Handle/close(timer), Handle/close(timer))!;
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

// Every turn allocates beside what one turn holds for the next, so collections run while the loop holds the rest and the abandon as owned roots: what crosses a turn must come back as it went.
#[test]
fn what_a_suspension_holds_survives_the_collections_between_its_turns() {
    let (code, output, _) = ended(
        r#"
        use /std/{Io, Handle, Nat, List};
        let turn(n: Nat, held: List(Nat)) -> Io(Nat) =
            match n: (_) => Io(Nat)
            | 0 => Io/pure(List/len(held))
            | m + 1 =>
                let garbage = List/range(0, 2000);
                let _ = Io/suspend([Handle/ready], Io/pure(()), Io/pure(List/len(garbage)))!;
                turn(m, [n, ..held])
            end;
        let counted = turn(3000, [])!;
        /std/print(Nat/to_str(counted))
        "#,
    );

    assert_eq!((code, output.as_slice()), (0, b"3000".as_slice()));
}
