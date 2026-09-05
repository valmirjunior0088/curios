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
