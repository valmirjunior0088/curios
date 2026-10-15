//! `/std/Tui`, for the claims a corpus unit cannot make.
//!
//! Everything the library *computes* is a `test` declaration in `curios-std/src/tests/tui/`, where one compile serves the whole unit. What stays here is what needs the compiler's own answer rather than a program's — a spelling that must be refused, which a corpus unit could only express by failing to compile and taking its every other test with it — and what needs a terminal: the session bracket and the loop run against the scripted host, whose standard input arrives in chunks a fiber parks between, exactly as a terminal delivers keystrokes.

use {
    super::test_support::Lying,
    crate::tests::{run_text, typecheck},
    curios_abi::{Failure, HostOp, TtySize},
    curios_runtime::MockHost,
};

// The palette bound is a decided proposition, so an index past the sixteen is refused where it is written rather than clamped or wrapped at runtime.
#[test]
fn a_palette_index_past_the_sixteen_is_refused() {
    let source = r#"
        use /std/{Tui};
        use /std/Tui/{Color};
        let picked: Color = Color/ansi(16);
        /std/print("unreachable")
        "#;

    let report = typecheck(source).expect_err("16 is not one of the sixteen");
    assert!(
        report.contains("nothing discharged /std/Bool/Holds(16 < 16)"),
        "the refusal should name the bound it could not discharge, got: {report}"
    );
}

// The bracket, both ways round: raw mode on, then the four modes in order, and on the way out the four inverses in reverse, then raw mode off — whichever way the body ended.
#[test]
fn a_session_switches_the_terminal_and_restores_it_in_reverse() {
    let (system, io) = MockHost::builder().tty_size(20, 5).build();
    run_text(
        r#"
        use /std/{Str, Try, Io, Show, Tui};
        use /std/Tui/{Session};
        let program: Io({}) =
            let outcome = Try/run(Session/with((s) => Try/pure("inside")))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    let enter = "\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[>1u";
    let leave = "\x1b[<u\x1b[?2004l\x1b[?25h\x1b[?1049l";
    assert_eq!(output, format!("{enter}{leave}inside"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// Without a terminal the bracket fails where it starts, as the `Try` it is, and switches nothing.
#[test]
fn a_session_without_a_terminal_is_refused_before_anything_is_written() {
    let (system, io) = MockHost::builder().build();
    run_text(
        r#"
        use /std/{Str, Try, Io, Show, Tui};
        use /std/Tui/{Session};
        let program: Io({}) =
            let outcome = Try/run(Session/with((s) => Try/pure("inside")))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(
        !output.contains("inside") && !output.contains("\x1b["),
        "{output:?}"
    );
    assert!(io.raw_modes().is_empty());
}

// The whole path as one program: a listing in a border, driven by scripted keystrokes that arrive one chunk per burst — so the reader waits between them. `down` moves the selection, `q` quits, and the model `run` answers is printed after the session ends. The first frame is written whole; the second is the diff that moved the highlight.
#[test]
fn a_program_runs_against_scripted_keystrokes_and_answers_its_model() {
    let (system, io) = MockHost::builder()
        .tty_size(12, 5)
        .stdin_chunks(vec![b"\x1b[B".to_vec(), b"q".to_vec()])
        .build();
    run_text(LISTING_PROGRAM, system).expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.starts_with("\x1b[?1049h"), "{output:?}");
    assert!(output.ends_with("\x1b[?1049ltwo"), "{output:?}");
    // The first frame is the whole screen: a clear, the border's corner, the title. The second is a diff: no clear, and the two rows whose highlight moved.
    assert_eq!(output.matches("\x1b[2J").count(), 1, "{output:?}");
    assert_eq!(output.matches("\x1b[?2026h").count(), 2, "{output:?}");
    assert!(output.contains("┌pick"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

#[test]
fn a_session_carries_incomplete_input_into_its_next_read() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .stdin_chunks(vec![b"\x1b".to_vec(), b"[".to_vec(), b"A".to_vec()])
        .build();
    run_text(r#"
        use /std/{Nat, List, Option, Try, Io, print};
        use /std/Tui/{Session};
        let program: Io({}) =
            let outcome = Try/run(Session/with((s) =>
                let first = Session/read(@Nat, s)!;
                let _ = print(match first | some(events) => Nat/to_str(List/len(events)) | none() => "eof" end)!;
                let second = Session/read(@Nat, s)!;
                let _ = print(match second
                    | some(events) => match events
                        | [event, .._] => match event
                            | key(k) => match k.code | up() => " up " | _ => " wrong-key " end
                            | _ => " wrong-event "
                            end
                        | [] => " missing "
                        end
                    | none() => " early-eof "
                    end)!;
                let third = Session/read(@Nat, s)!;
                let _ = print(match third | none() => "eof" | some(_) => "more" end)!;
                Try/pure(())))!;
            Io/pure(());
        program
    "# , host).expect("held input");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.contains("0 up eof"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

#[test]
fn drawing_history_and_size_travel_with_the_returned_session() {
    let (host, io) = MockHost::builder().tty_sizes([(4, 2), (6, 3)]).build();
    run_text(
        r#"
        use /std/{Nat, Option, Try, Io, print};
        use /std/Tui/{Session, Frame};
        let program: Io({}) =
            let outcome = Try/run(Session/with((s) =>
                let drawn = Session/draw(s, Frame/blank(4, 2), Option/none())!;
                let again = Session/draw(drawn, Frame/blank(4, 2), Option/none())!;
                let resized = Session/size(again)!;
                let _ = Session/draw(resized.0, Frame/blank(6, 3), Option/none())!;
                let original = Session/last_size(s)!;
                let updated = Session/last_size(resized.0)!;
                let _ = print(Nat/to_str(original.cols))!;
                let _ = print("/")!;
                let _ = print(Nat/to_str(updated.cols))!;
                Try/pure(())))!;
            Io/pure(());
        program
    "#,
        host,
    )
    .expect("threaded frames and dimensions");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert_eq!(output.matches("\x1b[2J").count(), 2, "{output:?}");
    assert!(output.contains("4/6"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

#[test]
fn resize_events_update_the_drawing_session_before_rendering() {
    let (host, io) = MockHost::builder()
        .tty_sizes([(4, 2), (6, 3), (8, 4)])
        .mono((0..40).map(|second| (second, 0)))
        .build();
    run_text(
        r#"
        use /std/{Nat, Option, Try, Tui, Io, print};
        use /std/Tui/{Frame, Directive};
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = match event
                | resize(w, h) => match w == 8
                    | true => (model + 1, Directive/quit())
                    | false => (model + 1, Directive/none())
                    end
                | _ => (model, Directive/none())
                end,
            view(model, w, h) = Frame/blank(w, h),
            cursor(model, w, h) = Option/none(),
        };
        let program: Io({}) =
            let result = Try/run(Tui/run(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program
    "#,
        host,
    )
    .expect("resize event loop");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert_eq!(output.matches("\x1b[2J").count(), 2, "{output:?}");
    assert!(output.ends_with("\x1b[?1049l2"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A program that never quits by itself, so what ends its run is its terminal: a failure is printed by its name, and a run that ended any other way prints that it did.
const IDLE_PROGRAM: &str = r#"
        use /std/{Nat, Option, Try, Show, Tui, Io, print};
        use /std/Tui/{Frame, Directive};
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = (model, Directive/none()),
            view(model, w, h) = Frame/blank(w, h),
            cursor(model, w, h) = Option/none(),
        };
        let program: Io({}) =
            let result = Try/run(Tui/run(app))!;
            print(match result | success(_) => "quit" | failure(e) => Show/show(e) end);
        program
    "#;

// A read of the terminal that fails ends the run with the read's error, the bracket undone on the way out: a program whose terminal has failed does not run on without input and without a reason.
#[test]
fn a_failed_read_ends_the_run_with_its_error() {
    let (host, io) = MockHost::builder().tty_size(4, 2).build();
    let failing = Lying::new(
        host,
        HostOp::HandleRead,
        Err::<Option<Vec<u8>>, Failure>(Failure::PermissionDenied),
    );
    run_text(IDLE_PROGRAM, failing).expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(
        output.ends_with("\x1b[?1049lpermission_denied"),
        "{output:?}"
    );
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// The size is asked once where the session opens and again at each tick, and the tick's query failing ends the run as a failed read does. Standard input has ended by then, which ends the reading alone.
#[test]
fn a_failed_size_query_ends_the_run_with_its_error() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .mono((0..40).map(|second| (second, 0)))
        .build();
    let failing = Lying::after(
        host,
        HostOp::TtySize,
        1,
        Err::<TtySize, Failure>(Failure::PermissionDenied),
    );
    run_text(IDLE_PROGRAM, failing).expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(
        output.ends_with("\x1b[?1049lpermission_denied"),
        "{output:?}"
    );
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// The end of standard input is no failure: the reading stops and the program runs on, here until the answer of its own directive, which arrives after the input has ended, has it quit.
#[test]
fn a_program_outlives_the_end_of_its_input() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .mono((0..40).map(|second| (second, 0)))
        .build();
    run_text(
        r#"
        use /std/{Nat, Option, Try, Tui, Io, print};
        use /std/time/{Duration};
        use /std/Tui/{Frame, Directive};
        let later: Io(Nat) =
            let _ = Io/sleep(Duration/of_millis(300))!;
            Io/pure(7);
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/perform(later)),
            update(model, event) = match event
                | custom(n) => (n, Directive/quit())
                | _ => (model, Directive/none())
                end,
            view(model, w, h) = Frame/blank(w, h),
            cursor(model, w, h) = Option/none(),
        };
        let program: Io({}) =
            let result = Try/run(Tui/run(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program
    "#,
        host,
    )
    .expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.ends_with("\x1b[?1049l7"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// Shared with the coordination measurement so both exercise the same application.
pub(super) const LISTING_PROGRAM: &str = r#"
        use /std/{Nat, Str, Bool, List, Option, Try, Io, Show, Tui};
        use /std/Tui/{Style, Frame, Key, Event, Directive, Listing};
        let app: Tui(Listing, Nat) =
            Tui {
                init = (Listing/new(["one", "two", "three"]), Directive/none()),
                update(model, event) =
                    match event
                    | key(k) =>
                        match k.code
                        | char(c) => match c == 'q' | true => (model, Directive/quit()) | false => (model, Directive/none()) end
                        | _ => (Listing/move(model, k.code), Directive/none())
                        end
                    | _ => (model, Directive/none())
                    end,
                view(model, w, h) =
                    let boxed(iw: Nat, ih: Nat) -> Frame(iw + 2, ih + 2) =
                        Tui/border(Style/plain, "pick", Listing/draw(model, Style/plain, Style { ..Style/plain, reverse = true }, iw, ih));
                    match w: (a) => Frame(a, h)
                    | 0 => Frame/blank(0, h)
                    | w1 + 1 =>
                        match w1: (b) => Frame(b + 1, h)
                        | 0 => Frame/blank(1, h)
                        | iw + 1 =>
                            match h: (c) => Frame(iw + 1 + 1, c)
                            | 0 => Frame/blank(iw + 1 + 1, 0)
                            | h1 + 1 =>
                                match h1: (d) => Frame(iw + 1 + 1, d + 1)
                                | 0 => Frame/blank(iw + 1 + 1, 1)
                                | ih + 1 => boxed(iw, ih)
                                end
                            end
                        end
                    end,
                cursor(model, w, h) = Option/none(),
            };
        let program: Io({}) =
            let outcome = Try/run(Tui/run(app))!;
            match outcome
            | success(model) => /std/print(Option/unwrap_or(Listing/selected(model), "none"))
            | failure(e) => /std/print(Show/show(e))
            end;
        program
        "#;
