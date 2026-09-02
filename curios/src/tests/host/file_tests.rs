//! Opening, reading and writing a file, and the bracket its handle lives inside — synchronous in `Io`, and inside a fiber through the stream witnesses.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Path, Try, Io};
        match Try/run(File/read_all(Path/of_str("data.txt")))!
        | success(contents) => Io/write(Io/stdout, contents)
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("error"))
        end
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
        match Try/run(File/read_all(Path/of_str("nope.txt")))!
        | success(_) => /std/print("contents")
        | failure(e) =>
            match e : (_) => /std/Io({})
            | not_found() => /std/print("not found")
            | permission_denied() => /std/print("denied")
            | exists() => /std/print("exists")
            | refused() => /std/print("refused")
            | tls() => /std/print("tls")
            | would_block() => /std/print("would block")
            | not_empty() => /std/print("not empty")
            | is_directory() => /std/print("is a directory")
            | not_directory() => /std/print("not a directory")
            | other(_) => /std/print("other")
            end
        end
        "#;

    assert_eq!(run(source), b"not found");
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Path, Try, Io};
        match Try/run(File/with(Path/of_str("out.txt"), File/Mode/write(), (f) => File/write(f, /std/Str/to_bytes("written"))))!
        | success(_) => /std/print("ok")
        | failure(_) => /std/print("error")
        end
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
        let bytes = Try/run(File/with(Path/of_str("lines.txt"), File/Mode/read(), (f) =>
            let c = File/read(f, 1024)!;
            Try/pure(match c | chunk(b) => b | eof() => x[] | error(_) => x[] end)))!;
        match bytes
        | success(b) => Io/write(Io/stdout, b)
        | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}

// The same bracket inside a fiber: the file reads through its stream witness, the body may yield, and the bracket is a scheduler guard, so the close is owed to the scheduler rather than to the body reaching its end.
#[test]
fn with_async_reads_through_the_stream_witness_inside_a_fiber() {
    let source = r#"
        use /std/{File, Path, Str, Bytes, Try, Async, Io, stream};
        let program: Try(Async, Io/Error, Bytes) =
            File/with_async(Path/of_str("lines.txt"), File/Mode/read(), (f) =>
                let _ = Async/yield_now!;
                let c = stream/Read/read(f, 1024)!;
                Try/pure(match c | chunk(b) => b | _ => x[] end));
        let fiber: Async({}) =
            let r = Try/run(program)!;
            match r
            | success(b) => Io/write(Io/stdout, b)
            | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
            end;
        Async/run(fiber)
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}
