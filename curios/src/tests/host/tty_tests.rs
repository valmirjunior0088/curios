//! Raw mode and the window size through `/std/tty`, against a scripted terminal and against none.

use {crate::tests::run_text, curios_runtime::MockHost};

// The size is the scripted pair on a host with a terminal, and `other(ENOTTY)` on one without: the errno lane is how a program learns it has no terminal, and the mock reports it as the native host does.
#[test]
fn size_reads_the_scripted_dimensions_and_reports_no_terminal_otherwise() {
    let source = r#"
        use /std/{Handle, Str, Nat, Show, Async, Io, tty};
        match Async/block_on(tty/size(Handle/stdin))!
        | failure(_) => /std/print("deadlock")
        | success(r) =>
            match r
            | success(s) => /std/print(Show/show(s))
            | failure(e) => match e | other(n) => /std/print(Str/concat("other ", Nat/to_str(n))) | _ => /std/print("error") end
            end
        end
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"80x24");

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"other 25");
}

// `with_raw` switches raw mode on, runs the body, and switches it back through `Async/using`, so the mock's record is exactly one switch each way around the body's result.
#[test]
fn with_raw_brackets_the_body_and_restores_the_mode() {
    let source = r#"
        use /std/{Handle, Str, Nat, Async, Io, tty};
        match Async/block_on(tty/with_raw(Handle/stdin, Async/pure(7)))!
        | failure(_) => /std/print("deadlock")
        | success(r) =>
            match r | success(n) => /std/print(Nat/to_str(n)) | failure(_) => /std/print("error") end
        end
        "#;

    let (system, io) = MockHost::builder().tty_size(80, 24).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"7");
    assert_eq!(io.raw_modes(), vec![true, false]);
}
