//! `/std/Tui`, for the claims a corpus unit cannot make.
//!
//! Everything the library *computes* is a `test` declaration in `curios-std/src/tests/tui/`, where one compile serves the whole unit. What stays here is what needs the compiler's own answer rather than a program's — a spelling that must be refused, which a corpus unit could only express by failing to compile and taking its every other test with it — and what needs a terminal: the session bracket and the loop run against the scripted host, whose standard input arrives in chunks a reader suspends between, exactly as a terminal delivers keystrokes.

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

// What a session asks the terminal where it opens: the keyboard protocol's flags, the three modes it uses where the terminal supports them, and the device attributes last.
const ASKED: &str = "\x1b[?u\x1b[?2026$p\x1b[?2027$p\x1b[?2048$p\x1b[c";

// Every mode a session may set, undone in reverse, which the host writes as it gives the terminal back.
const LEAVE: &str = "\x1b[<u\x1b[?2048l\x1b[?2027l\x1b[?1004l\x1b[?2004l\x1b[?25h\x1b[?1049l";

// The answer of a terminal that supports synchronized output and nothing else the session asks after: a chunk scripted first, so the session opens on it and leaves the keys after it to the program.
const SYNCHRONIZED: &[u8] = b"\x1b[?2026;2$y\x1b[?62c";

// The session's scope, both ways round: raw mode on, the questions, then the four modes a terminal that answered none of them is set up with, in order; and as the scope ends every mode the session may set undone in reverse, then raw mode off — whichever way the body ended.
#[test]
fn a_session_switches_the_terminal_and_restores_it_in_reverse() {
    let (system, io) = MockHost::builder().tty_size(20, 5).build();
    run_text(
        r#"
        use /std/{Str, Try, Io, Show, Tui};
        use /std/Tui/{Session};
        let program() -> Io({}) =
            let outcome = Try/run(Try/scope((scope) => Try/bind(Session/open(scope), (s) => Try/pure("inside"))))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        program()
        "#,
        system,
    )
    .expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    let enter = "\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[?1004h";
    assert_eq!(output, format!("{ASKED}{enter}{LEAVE}inside"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A terminal that answers with the keyboard protocol's flags and every mode supported is set up with all of them: the protocol pushed, grapheme clusters and in-band resize set.
#[test]
fn a_session_sets_up_what_the_terminal_confirmed() {
    let (system, io) = MockHost::builder()
        .tty_size(20, 5)
        .stdin_chunks(vec![
            b"\x1b[?1u\x1b[?2026;2$y\x1b[?2027;2$y\x1b[?2048;2$y\x1b[?62c".to_vec(),
        ])
        .build();
    run_text(
        r#"
        use /std/{Str, Try, Io, Show, Tui};
        use /std/Tui/{Session};
        let program() -> Io({}) =
            let outcome = Try/run(Try/scope((scope) => Try/bind(Session/open(scope), (s) => Try/pure("inside"))))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        program()
        "#,
        system,
    )
    .expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    let enter = "\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[?1004h\x1b[>1u\x1b[?2027h\x1b[?2048h";
    assert_eq!(output, format!("{ASKED}{enter}{LEAVE}inside"), "{output:?}");
}

// Without a terminal entering the session fails where it starts, as the `Try` it is, and switches nothing.
#[test]
fn a_session_without_a_terminal_is_refused_before_anything_is_written() {
    let (system, io) = MockHost::builder().build();
    run_text(
        r#"
        use /std/{Str, Try, Io, Show, Tui};
        use /std/Tui/{Session};
        let program() -> Io({}) =
            let outcome = Try/run(Try/scope((scope) => Try/bind(Session/open(scope), (s) => Try/pure("inside"))))!;
            match outcome
            | success(s) => /std/print(s)
            | failure(e) => /std/print(Show/show(e))
            end;
        program()
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
        .stdin_chunks(vec![
            SYNCHRONIZED.to_vec(),
            b"\x1b[B".to_vec(),
            b"q".to_vec(),
        ])
        .build();
    run_text(LISTING_PROGRAM, system).expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.starts_with(ASKED), "{output:?}");
    assert!(output.ends_with("\x1b[?1049ltwo"), "{output:?}");
    // The first frame is the whole screen: a clear, the border's corner, the title. The second is a diff: no clear, and the two rows whose highlight moved.
    assert_eq!(output.matches("\x1b[2J").count(), 1, "{output:?}");
    assert_eq!(output.matches("\x1b[?2026h").count(), 2, "{output:?}");
    assert!(output.contains("┌\x1b[1;2Hpick"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A pause in the keys lets the loop's deadline for the size pass: it asks the terminal's size there and finds it new, so the frame after it is drawn whole, a second clear, where keys that never pause leave it unasked. A key follows the pause before `q`, since a burst that quits draws nothing.
#[test]
fn a_pause_in_the_keys_lets_the_loop_ask_the_size_and_read_a_resize() {
    let (system, io) = MockHost::builder()
        .tty_sizes([(12, 5), (16, 6)])
        .stdin_chunks(vec![SYNCHRONIZED.to_vec(), b"\x1b[B".to_vec()])
        .stdin_pause()
        .stdin_chunks(vec![b"\x1b[A".to_vec(), b"q".to_vec()])
        .mono((0..1_000).map(|millis| (millis / 1_000, millis % 1_000 * 1_000_000)))
        .build();
    run_text(LISTING_PROGRAM, system).expect("expected result");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.ends_with("\x1b[?1049lone"), "{output:?}");
    assert_eq!(output.matches("\x1b[2J").count(), 2, "{output:?}");
}

#[test]
fn a_session_carries_incomplete_input_into_its_next_read() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .stdin_chunks(vec![
            SYNCHRONIZED.to_vec(),
            b"\x1b".to_vec(),
            b"[".to_vec(),
            b"A".to_vec(),
        ])
        .build();
    run_text(r#"
        use /std/{Nat, List, Option, Try, Io, print};
        use /std/Tui/{Session};
        let program() -> Io({}) =
            let outcome = Try/run(Try/scope((scope) => Try/bind(Session/open(scope), (s) =>
                let first = Session/read(s)!;
                let _ = print(match first | some(events) => Nat/to_str(List/len(events)) | none() => "eof" end)!;
                let second = Session/read(s)!;
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
                let third = Session/read(s)!;
                let _ = print(match third | none() => "eof" | some(_) => "more" end)!;
                Try/pure(()))))!;
            Io/pure(());
        program()
    "# , host).expect("held input");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.contains("0 up eof"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A session is a handle: a draw diffs against the frame the last one wrote, and a size asked is the size it knows from then on, so a frame of another size is written whole.
#[test]
fn a_session_remembers_what_it_drew_and_the_size_it_was_told() {
    let (host, io) = MockHost::builder().tty_sizes([(4, 2), (6, 3)]).build();
    run_text(
        r#"
        use /std/{Nat, Try, Io, print};
        use /std/Tui/{Session, View};
        let program() -> Io({}) =
            let outcome = Try/run(Try/scope((scope) => Try/bind(Session/open(scope), (s) =>
                let _ = Session/draw(s, View/blank(4, 2))!;
                let _ = Session/draw(s, View/blank(4, 2))!;
                let original = Session/size(s)!;
                let now = Session/measure(s)!;
                let _ = Session/draw(s, View/blank(6, 3))!;
                let known = Session/size(s)!;
                let _ = print(Nat/to_str(original.cols))!;
                let _ = print("/")!;
                let _ = print(Nat/to_str(now.cols))!;
                let _ = print("/")!;
                let _ = print(Nat/to_str(known.cols))!;
                Try/pure(()))))!;
            Io/pure(());
        program()
    "#,
        host,
    )
    .expect("threaded frames and dimensions");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert_eq!(output.matches("\x1b[2J").count(), 2, "{output:?}");
    assert!(output.contains("4/6/6"), "{output:?}");
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
        use /std/{Nat, Try, Tui, Io, print};
        use /std/Tui/{View, Directive};
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = match event
                | input(resize(size)) => match size.cols == 8
                    | true => (model + 1, Directive/quit())
                    | false => (model + 1, Directive/none())
                    end
                | _ => (model, Directive/none())
                end,
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program()
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
        use /std/{Nat, Try, Show, Tui, Io, print};
        use /std/Tui/{View, Directive};
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = (model, Directive/none()),
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(_) => "quit" | failure(e) => Show/show(e) end);
        program()
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
        use /std/{Nat, Try, Tui, Io, print};
        use /std/time/{Duration};
        use /std/Tui/{View, Directive};
        let later() -> Io(Nat) =
            let _ = Io/sleep(Duration/of_millis(300))!;
            Io/pure(7);
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/perform(later())),
            update(model, event) = match event
                | custom(n) => (n, Directive/quit())
                | _ => (model, Directive/none())
                end,
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program()
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
        use /std/Tui/{Style, Pane, Key, Event, Directive, Listing};
        let app: Tui(Listing(Str), Nat) =
            Tui {
                init = (Listing/new(["one", "two", "three"]), Directive/none()),
                update(model, event) =
                    match event
                    | input(key(k)) =>
                        match k.code
                        | char(c) => match c == 'q' | true => (model, Directive/quit()) | false => (model, Directive/none()) end
                        | _ => (Listing/handle(model, k, 3), Directive/none())
                        end
                    | _ => (model, Directive/none())
                    end,
                view(model) = Pane/border(Style/plain, "pick", Listing/view(model, (s: Str) => s, Style/plain, Style { ..Style/plain, reverse = true })),
            };
        let program() -> Io({}) =
            let outcome = Try/run(Tui/main(app))!;
            match outcome
            | success(model) => /std/print(Option/unwrap_or(Listing/selected(model), "none"))
            | failure(e) => /std/print(Show/show(e))
            end;
        program()
        "#;

// Saving and quitting in one update: the quit waits for the save its update asked for, which runs to its end before the program does.
#[test]
fn a_quit_waits_for_the_work_its_update_asked_for() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .stdin_chunks(vec![SYNCHRONIZED.to_vec(), b"q".to_vec()])
        .build();
    run_text(
        r#"
        use /std/{Nat, Try, Tui, Io, print};
        use /std/Tui/{View, Directive};
        let save() -> Io(Nat) =
            let _ = print("saved")!;
            Io/pure(1);
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = match event
                | input(key(_)) => (model, Directive/batch([Directive/perform(save()), Directive/quit()]))
                | _ => (model, Directive/none())
                end,
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(_) => "/quit" | failure(_) => "/failed" end);
        program()
    "#,
        host,
    )
    .expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.contains("saved"), "{output:?}");
    assert!(output.ends_with("\x1b[?1049l/quit"), "{output:?}");
}

// A draw whose write fails ends the run with the write's error, the terminal given back on the way out.
#[test]
fn a_failed_draw_ends_the_run_with_its_error() {
    let (host, io) = MockHost::builder().tty_size(4, 2).build();
    // The session's questions and its set-up are the first two writes; the first frame is the third.
    let failing = Lying::after(
        host,
        HostOp::HandleWrite,
        2,
        Err::<u64, Failure>(Failure::PermissionDenied),
    );
    run_text(IDLE_PROGRAM, failing).expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(
        output.ends_with("\x1b[?1049lpermission_denied"),
        "{output:?}"
    );
    assert_eq!(io.raw_modes(), vec![true, false]);
}

// A release gives the terminal back around its action — the modes undone and raw mode off while the action runs — and takes it again after, set up as before; its answer reaches `update`.
#[test]
fn a_release_gives_the_terminal_back_around_its_action() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .stdin_chunks(vec![SYNCHRONIZED.to_vec(), b"r".to_vec(), b"q".to_vec()])
        .build();
    run_text(
        r#"
        use /std/{Nat, Try, Tui, Io, print};
        use /std/Tui/{View, Directive};
        let outside() -> Io(Nat) =
            let _ = print("outside")!;
            Io/pure(5);
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = match event
                | input(key(k)) => match k.code
                    | char(c) => match c == 'r'
                        | true => (model, Directive/release(outside()))
                        | false => (model, Directive/quit())
                        end
                    | _ => (model, Directive/none())
                    end
                | custom(n) => (model + n, Directive/none())
                | _ => (model, Directive/none())
                end,
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program()
    "#,
        host,
    )
    .expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    let outside = output.find("outside").expect("the action ran");
    let given = output[..outside]
        .rfind("\x1b[?1049l")
        .expect("the terminal was given back first");
    let taken = output[outside..]
        .find("\x1b[?1049h")
        .expect("and taken again after");
    assert!(given < outside && taken > 0, "{output:?}");
    assert!(output.ends_with("\x1b[?1049l5"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false, true, false]);
}

// A terminal that reports its size in band is never asked it: the report is an input, and the run ends on it with no clock to tick.
#[test]
fn a_size_the_terminal_reports_reaches_update_as_an_input() {
    let (host, io) = MockHost::builder()
        .tty_size(4, 2)
        .stdin_chunks(vec![
            b"\x1b[?2048;2$y\x1b[?62c".to_vec(),
            b"\x1b[48;3;8;0;0t".to_vec(),
        ])
        .build();
    run_text(
        r#"
        use /std/{Nat, Try, Tui, Io, print};
        use /std/Tui/{View, Directive};
        let app: Tui(Nat, Nat) = Tui {
            init = (0, Directive/none()),
            update(model, event) = match event
                | input(resize(size)) => (size.cols, Directive/quit())
                | _ => (model, Directive/none())
                end,
            view(model) = (w, h) => View/blank(w, h),
        };
        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(n) => Nat/to_str(n) | failure(_) => "failed" end);
        program()
    "#,
        host,
    )
    .expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(output.contains("\x1b[?2048h"), "{output:?}");
    assert!(output.ends_with("\x1b[?1049l8"), "{output:?}");
}

// A whole program of the widgets: a field that filters a listing, each in a bordered pane under `Pane/rows`, the keys going to whichever the focus holds. It types a letter, sits through a resize, moves the focus to the listing and down it, and saves the selection and quits in one update.
const PICKER_PROGRAM: &str = r#"
        use /std/{Nat, Str, List, Option, Try, Tui, Io, print};
        use /std/ops/{Eql};
        use /std/Tui/{Style, View, Pane, Key, Input, Event, Directive, Listing, Field, Focus};

        induct Part: Type
        | filter()
        | items()
        end

        satisfy Eql(Part) {
            ..
        }

        struct Model: Type {
            field: Field,
            listing: Listing(Str),
            focus: Focus(Part),
        }

        let fruit: List(Str) = ["apple", "banana", "blueberry", "cherry"];

        let matching(f: Field) -> Listing(Str) =
            Listing/new(List/filter(fruit, (s: Str) => Str/starts_with(s, f.text)));

        let save(chosen: Option(Str)) -> Io(Nat) =
            let _ = print(Str/concat("saved ", Option/unwrap_or(chosen, "nothing")))!;
            Io/pure(0);

        let reacted(m: Model, k: Key) -> {Model, Directive(Nat)} =
            choose
            | k.code == Key/Code/tab() => (Model { ..m, focus = Focus/handle(m.focus, k) }, Directive/none())
            | k.ctrl && k.code == Key/Code/char('s') =>
                (m, Directive/batch([Directive/perform(save(Listing/selected(m.listing))), Directive/quit()]))
            | Focus/holds(m.focus, Part/filter()) =>
                let field = Field/handle(m.field, k);
                (Model { ..m, field = field, listing = matching(field) }, Directive/none())
            | _ => (Model { ..m, listing = Listing/handle(m.listing, k, 3) }, Directive/none())
            end;

        let marked: Style = Style { ..Style/plain, reverse = true };

        let app: Tui(Model, Nat) =
            Tui {
                init = (
                    Model { field = Field/empty, listing = matching(Field/empty), focus = Focus/new(Part/filter(), [Part/items()]) },
                    Directive/none()),
                update(model, event) =
                    match event
                    | input(key(k)) => reacted(model, k)
                    | _ => (model, Directive/none())
                    end,
                view(model) =
                    Pane/rows([
                        (Pane/Constraint/length(3), Pane/border(Style/plain, "find", Field/view(model.field, Style/plain))),
                        (Pane/Constraint/fill(1), Pane/border(Style/plain, "fruit", Listing/view(model.listing, (s: Str) => s, Style/plain, marked)))]),
            };

        let program() -> Io({}) =
            let result = Try/run(Tui/main(app))!;
            print(match result | success(m) => Str/concat(" with ", m.field.text) | failure(_) => " failed" end);
        program()
    "#;

// The program's frames, its cursor and its answer: the first frame whole and the resize's whole again, the terminal's cursor in the field past the letter typed, the save run with the selection the keys moved to, and the field's text answered.
#[test]
fn a_whole_program_of_widgets_runs_against_the_scripted_terminal() {
    let (host, io) = MockHost::builder()
        .tty_sizes([(20, 10), (24, 10)])
        .stdin_chunks(vec![SYNCHRONIZED.to_vec(), b"b".to_vec()])
        .stdin_pause()
        .stdin_chunks(vec![b"\t".to_vec(), b"\x1b[B".to_vec(), b"\x13".to_vec()])
        .mono((0..1_000).map(|millis| (millis / 1_000, millis % 1_000 * 1_000_000)))
        .build();
    run_text(PICKER_PROGRAM, host).expect("the run ends");
    let output = String::from_utf8_lossy(&io.output()).into_owned();
    assert_eq!(output.matches("\x1b[2J").count(), 2, "{output:?}");
    assert!(output.contains("┌\x1b[1;2Hfind"), "{output:?}");
    assert!(output.contains("\x1b[2;3H\x1b[?25h"), "{output:?}");
    assert!(output.contains("saved blueberry"), "{output:?}");
    assert!(output.ends_with("\x1b[?1049l with b"), "{output:?}");
    assert_eq!(io.raw_modes(), vec![true, false]);
}
