//! The controlling terminal through `/std/Tty`: opened, taken and measured against a scripted terminal, and refused where the host has none.

use {crate::tests::run_text, curios_runtime::MockHost};

// The size is the scripted pair on a host with a terminal, and `other(ENXIO)` on one without, from the open: the errno lane is how a program learns it has no controlling terminal, and the mock reports it as the native host does.
#[test]
fn size_reads_the_scripted_dimensions_and_reports_no_terminal_otherwise() {
    let source = r#"
        use /std/{Str, Nat, Show, Try, Io, Tty};
        let entry() -> /std/Io({}) =
            match Try/run(Try/scope((s) => Try/bind(Tty/open(s), Tty/size)))!
            | success(s) => /std/print(Show/show(s))
            | failure(e) => match e | other(n) => /std/print(Str/concat("other ", Nat/to_str(n))) | _ => /std/print("error") end
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"80x24");

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"other 6");
}

// A host result crosses as the raw `i64` its bits fill and the program boxes it, so a `Nat` past the i31 and the 32-bit word — and past `2⁶³`, where the bits read signed would be negative — arrives as the number the host answered rather than being refused on the way back.
#[test]
fn a_size_past_the_i31_crosses_back_whole() {
    let source = r#"
        use /std/{Show, Try, Io, Tty};
        let entry() -> /std/Io({}) =
            match Try/run(Try/scope((s) => Try/bind(Tty/open(s), Tty/size)))!
            | success(s) => /std/print(Show/show(s))
            | failure(e) => /std/print(Show/show(e))
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder()
        .tty_size(10_000_000_000_000_000_000, 1 << 40)
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"10000000000000000000x1099511627776");
}

// `raw` takes the terminal for the rest of the scope, which gives it back as it ends, so the mock's record is exactly one switch each way around the body's result, and the leave bytes reach the terminal once, as it is given back.
#[test]
fn raw_mode_lasts_the_scope_and_is_restored_as_it_ends() {
    let source = r#"
        use /std/{Nat, Show, Try, Io, Tty};
        let taken(s: Io/Scope) -> Try(Io, Io/Error, Nat) =
            let t = Tty/open(s)!;
            let _ = Tty/raw(s, t, x['<', 'l', '>'])!;
            Try/pure(7);
        let entry() -> /std/Io({}) =
            match Try/run(Try/scope(taken))!
            | success(n) => /std/print(Nat/to_str(n))
            | failure(e) => /std/print(Show/show(e))
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"<l>7");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A body that fails still has the terminal given back, and its failure is the scope's answer; with no terminal the open fails, nothing is taken, and the body never runs.
#[test]
fn raw_mode_is_restored_after_a_failing_body_and_refused_without_a_terminal() {
    let source = r#"
        use /std/{Nat, Show, Try, Io, Tty};
        let body: Try(Io, Io/Error, Nat) =
            Try/raise(Io/Error/connection_refused());
        let taken(s: Io/Scope) -> Try(Io, Io/Error, Nat) =
            let t = Tty/open(s)!;
            let _ = Tty/raw(s, t, x[])!;
            body;
        let entry() -> /std/Io({}) =
            match Try/run(Try/scope(taken))!
            | success(n) => /std/print(Nat/to_str(n))
            | failure(e) => /std/print(Show/show(e))
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"connection_refused");
    assert_eq!(io.raw_modes(), vec![true, false]);

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"other(6)");
    assert_eq!(io.raw_modes(), Vec::<bool>::new());
}

// The same scope over a body that suspends: the body yields and resumes with the terminal taken, and it is given back once around the result.
#[test]
fn raw_mode_lasts_across_a_body_that_suspends() {
    let source = r#"
        use /std/{Nat, Show, Try, Io, Tty};
        let taken(s: Io/Scope) -> Try(Io, Io/Error, Nat) =
            let t = Tty/open(s)!;
            let _ = Tty/raw(s, t, x[])!;
            let _ = Io/yield()!;
            Try/pure(7);
        let program() -> Io({}) =
            let r = Try/run(Try/scope(taken))!;
            match r
            | success(n) => /std/print(Nat/to_str(n))
            | failure(e) => /std/print(Show/show(e))
            end;
        program()
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// The terminal is a stream: what a program writes to it reaches the terminal, and what it reads from it is the keys the terminal sends.
#[test]
fn the_terminal_is_read_and_written_as_a_stream() {
    let source = r#"
        use /std/{Show, Try, Io, Tty};
        let echoed(s: Io/Scope) -> Try(Io, Io/Error, {}) =
            let t = Tty/open(s)!;
            let chunk = Io/read(t, 8)!;
            match chunk
            | bytes(b) => Io/write(t, x['>', ..b])
            | ended() => Io/write(t, x['.'])
            end;
        let entry() -> /std/Io({}) =
            match Try/run(Try/scope(echoed))!
            | success(_) => Io/pure(())
            | failure(e) => /std/print(Show/show(e))
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder()
        .tty_size(80, 24)
        .stdin_chunks(vec![b"hi".to_vec()])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b">hi");
}
