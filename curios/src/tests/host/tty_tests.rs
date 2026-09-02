//! Raw mode and the window size through `/std/tty`, against a scripted terminal and against none.

use {crate::tests::run_text, curios_runtime::MockHost};

// The size is the scripted pair on a host with a terminal, and `other(ENOTTY)` on one without: the errno lane is how a program learns it has no terminal, and the mock reports it as the native host does.
#[test]
fn size_reads_the_scripted_dimensions_and_reports_no_terminal_otherwise() {
    let source = r#"
        use /std/{Str, Nat, Show, Try, Io, tty};
        match Try/run(tty/size)!
        | success(s) => /std/print(Show/show(s))
        | failure(e) => match e | other(n) => /std/print(Str/concat("other ", Nat/to_str(n))) | _ => /std/print("error") end
        end
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"80x24");

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"other 25");
}

// `with_raw` switches raw mode on, runs the body, and switches it back, so the mock's record is exactly one switch each way around the body's result.
#[test]
fn with_raw_brackets_the_body_and_restores_the_mode() {
    let source = r#"
        use /std/{Nat, Show, Try, Io, tty};
        match Try/run(tty/with_raw(Try/pure(7)))!
        | success(n) => /std/print(Nat/to_str(n))
        | failure(e) => /std/print(Show/show(e))
        end
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A body that fails still restores the mode, and its failure is the bracket's answer; with no terminal the bracket fails at the switch on, records nothing, and never runs the body.
#[test]
fn with_raw_restores_after_a_failing_body_and_refuses_without_a_terminal() {
    let source = r#"
        use /std/{Nat, Show, Try, Io, tty};
        let body: Try(Io, Io/Error, Nat) =
            Try/raise(Io/Error/refused());
        match Try/run(tty/with_raw(body))!
        | success(n) => /std/print(Nat/to_str(n))
        | failure(e) => /std/print(Show/show(e))
        end
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"refused");
    assert_eq!(io.raw_modes(), vec![true, false]);

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"other(25)");
    assert_eq!(io.raw_modes(), Vec::<bool>::new());
}

// `with_raw_async` is the same bracket inside a fiber: the body parks and resumes under raw mode, and the scheduler's guard restores it once around the result.
#[test]
fn with_raw_async_brackets_a_parking_body_inside_a_fiber() {
    let source = r#"
        use /std/{Nat, Show, Try, Async, Io, tty};
        let body: Try(Async, Io/Error, Nat) =
            let _ = Async/yield_now!;
            Try/pure(7);
        let fiber: Async({}) =
            let r = Try/run(tty/with_raw_async(body))!;
            match r
            | success(n) => /std/print(Nat/to_str(n))
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
    assert_eq!(io.raw_modes(), vec![true, false]);
}
