//! `/std/Tui`, for the claims a corpus unit cannot make.
//!
//! Everything the library *computes* is a `test` declaration in `curios/src/tests/corpus/tui/`, where one compile serves the whole unit. What stays here is what needs the compiler's own answer rather than a program's — a spelling that must be refused, which a corpus unit could only express by failing to compile and taking its every other test with it — and what needs a terminal: the session bracket and the loop run against the scripted host, whose standard input arrives in chunks a fiber parks between, exactly as a terminal delivers keystrokes.

use {
    crate::tests::{run_text, typecheck},
    curios_runtime::MockHost,
};

// The palette bound is a decided proposition, so an index past the sixteen is refused where it is written rather than clamped or wrapped at runtime.
#[test]
fn a_palette_index_past_the_sixteen_is_refused() {
    let source = r#"
        use /std/{Handle, Tui};
        use /std/Tui/{Color};
        let picked: Color = Color/ansi(16);
        /std/print("unreachable")
        "#;

    let report = typecheck(source).expect_err("16 is not one of the sixteen");
    assert!(
        report.contains("Lt"),
        "the refusal should name the bound it could not discharge, got: {report}"
    );
}

// The bracket, both ways round: raw mode on, then the four modes in order, and on the way out the four inverses in reverse, then raw mode off — whichever way the body ended.
#[test]
fn a_session_switches_the_terminal_and_restores_it_in_reverse() {
    let (system, io) = MockHost::builder().tty_size(20, 5).build();
    run_text(
        r#"
        use /std/{Str, Try, Async, Io, Show, Tui};
        use /std/Tui/{Session};
        let fiber: Async({}) =
            let outcome = Try/run(Session/with((s) => Try/pure("inside")))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
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
        use /std/{Str, Try, Async, Io, Show, Tui};
        use /std/Tui/{Session};
        let fiber: Async({}) =
            let outcome = Try/run(Session/with((s) => Try/pure("inside")))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
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

// The whole path as one program: a listing in a border, driven by scripted keystrokes that arrive one chunk per burst — so the reader fiber parks between them, which is what `7234cc3f` scripted standard input for. `down` moves the selection, `q` quits, and the model `run` answers is printed after the session ends. The first frame is written whole; the second is the diff that moved the highlight.
#[test]
fn a_program_runs_against_scripted_keystrokes_and_answers_its_model() {
    let (system, io) = MockHost::builder()
        .tty_size(12, 5)
        .stdin_chunks(vec![b"\x1b[B".to_vec(), b"q".to_vec()])
        .build();
    run_text(
        r#"
        use /std/{Nat, Str, Bool, List, Option, Try, Async, Io, Show, Tui};
        use /std/Tui/{Style, Frame, Key, Event, Cmd, Listing, Border};
        let app: Tui(Listing, Nat) =
            Tui {
                init = (Listing/new(["one", "two", "three"]), Cmd/none()),
                update(model, event) =
                    match event
                    | key(k) =>
                        match k.code
                        | char(c) => match c == 'q' | true => (model, Cmd/quit()) | false => (model, Cmd/none()) end
                        | _ => (Listing/move(model, k.code), Cmd/none())
                        end
                    | _ => (model, Cmd/none())
                    end,
                view(model, w, h) =
                    let boxed(iw: Nat, ih: Nat) -> Frame(iw + 2, ih + 2) =
                        Border/around(Style/plain, "pick", Listing/draw(model, Style/plain, Style { ..Style/plain, reverse = true }, iw, ih));
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
        let fiber: Async({}) =
            let outcome = Try/run(Tui/run(app))!;
            match outcome
            | success(model) => /std/print(Option/unwrap_or(Listing/selected(model), "none"))
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#,
        system,
    )
    .expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.starts_with("\x1b[?1049h"), "{output:?}");
    assert!(output.ends_with("\x1b[?1049ltwo"), "{output:?}");
    // The first frame is the whole screen: a clear, the border's corner, the title. The second is a diff: no clear, and the two rows whose highlight moved.
    assert_eq!(output.matches("\x1b[2J").count(), 1, "{output:?}");
    assert_eq!(output.matches("\x1b[?2026h").count(), 2, "{output:?}");
    assert!(output.contains("┌pick"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A `Cmd/none` beside a `Cmd/perform` through a join the optimizer split into fields, with nothing the compile-time evaluator can fold. The two constructors share a row whose closure slot `none` leaves padded; the continuation split carried that padding as a filler, and the join's head rebuilt the row from its field parameters through the slot's cast — which an `i31` zero fails and a null passes. The events depend on the process arguments so the fold runs at run time, which is the only place the trap was.
#[test]
fn a_padded_variant_survives_a_split_join_at_run_time() {
    let (system, io) = MockHost::builder().args([b"program".as_slice()]).build();
    run_text(
        r#"
        use /std/{Nat, Bool, Str, Char, List, Option, Async, Handle, Tui, proc};
        use /std/Tui/{Style, Frame, Key, Event, Cmd};
        use /std/Tui/Key/{Code};
        let counter: Tui(Nat, Nat) =
            Tui {
                init = (0, Cmd/none()),
                update(model, event) =
                    match event
                    | key(k) =>
                        match k.code
                        | char(c) =>
                            choose
                            | c == '+' => (model + 1, Cmd/none())
                            | c == 'q' => (model, Cmd/quit())
                            | c == '!' => (model, Cmd/perform(Async/pure(model * 2)))
                            | _ => (model, Cmd/none())
                            end
                        | _ => (model, Cmd/none())
                        end
                    | custom(n) => (n, Cmd/none())
                    | _ => (model, Cmd/none())
                    end,
                view(model, w, h) = Frame/of_lines(Style/plain, [Nat/to_str(model)], w, h),
                cursor(model, w, h) = Option/none(),
            };
        let n = List/len(proc/args!);
        let press(c: Char) -> Event(Nat) = Event/key(Key/plain(Code/char(c)));
        let d = Tui/drive(counter, 3, 1, [Event/custom(n + 6), press('+'), press('!')]);
        /std/print(Str/flatten([Nat/to_str(d.model), ":", Nat/to_str(List/len(d.pending))]))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"8:1");
}
