//! Opening, reading and writing a file, and the bracket its handle lives inside — synchronous, and across a suspension through the stream witnesses.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Path, Try, Io};
        let entry() -> /std/Io({}) =
            match Try/run(File/read_all(Path/of_str("data.txt")))!
            | success(contents) => Io/bind(Try/run(Io/write(Io/stdout, contents)), (_) => Io/pure(()))
            | failure(_) => /std/print("error")
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder()
        .files([("data.txt", "file contents")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"file contents");
}

#[test]
fn read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Path, Try, Io};
        let entry() -> /std/Io({}) =
            match Try/run(File/read_all(Path/of_str("nope.txt")))!
            | success(_) => /std/print("contents")
            | failure(e) =>
                match e : (_) => /std/Io({})
                | not_found() => /std/print("not found")
                | permission_denied() => /std/print("denied")
                | already_exists() => /std/print("already exists")
                | connection_refused() => /std/print("connection refused")
                | tls() => /std/print("tls")
                | not_empty() => /std/print("not empty")
                | is_directory() => /std/print("is a directory")
                | not_directory() => /std/print("not a directory")
                | connection_reset() => /std/print("connection reset")
                | broken_pipe() => /std/print("broken pipe")
                | too_long() => /std/print("too long")
                | not_text(_) => /std/print("not text")
                | other(_) => /std/print("other")
                end
            end;
        entry()
        "#;

    assert_eq!(run(source), b"not found");
}

/// A file read as text is its text, and one whose bytes are not text is refused with the length of their longest prefix that is.
#[test]
fn read_all_text_answers_text_and_refuses_bytes_that_are_none() {
    for (contents, said) in [
        (&b"caf\xc3\xa9"[..], "caf\u{e9}"),
        (&b"ab\xffcd"[..], "not_text(2)"),
    ] {
        let source = r#"
            use /std/{File, Path, Show, Try, Io, print};
            let entry() -> /std/Io({}) =
                match Try/run(File/read_all_text(Path/of_str("data.txt")))!
                | success(text) => print(text)
                | failure(why) => print(Show/show(why))
                end;
            entry()
            "#;

        let (system, io) = MockHost::builder().files([("data.txt", contents)]).build();
        run_text(source, system).expect("expected result");
        assert_eq!(String::from_utf8_lossy(&io.output()), said);
    }
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Path, Try, Io};
        let entry() -> /std/Io({}) =
            match Try/run(File/with(Path/of_str("out.txt"), File/Mode/write(), (f) => File/write(f, /std/Str/to_bytes("written"))))!
            | success(_) => /std/print("ok")
            | failure(_) => /std/print("error")
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// A read inside the bracket is synchronous, a `Try` region over `Io` sequencing an `Io` action through the identity edge, and the bracket closes the file after the body's answer.
#[test]
fn read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Path, Str, Bytes, Try, Io};
        let entry() -> /std/Io({}) =
            let bytes = Try/run(File/with(Path/of_str("lines.txt"), File/Mode/read(), (f) =>
                let c = Io/read(f, 1024)!;
                Try/pure(match c | bytes(b, @_) => b | ended() => x[] end)))!;
            match bytes
            | success(b) => Io/bind(Try/run(Io/write(Io/stdout, b)), (_) => Io/pure(()))
            | failure(_) => /std/print("error")
            end;
        entry()
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}

// The same bracket over a body that suspends: the file reads through its stream witness after the body yields, and the close rides the suspension, so it is owed to whoever advances the body rather than to the body reaching its end.
#[test]
fn a_bracketed_file_reads_through_its_stream_witness_across_a_suspension() {
    let source = r#"
        use /std/{File, Path, Str, Bytes, Try, Io};
        let program: Try(Io, Io/Error, Bytes) =
            File/with(Path/of_str("lines.txt"), File/Mode/read(), (f) =>
                let _ = Io/yield()!;
                let c = Io/read(f, 1024)!;
                Try/pure(match c | bytes(b, @_) => b | _ => x[] end));
        let main() -> Io({}) =
            let r = Try/run(program)!;
            match r
            | success(b) => Io/bind(Try/run(Io/write(Io/stdout, b)), (_) => Io/pure(()))
            | failure(_) => /std/print("error")
            end;
        main()
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}
