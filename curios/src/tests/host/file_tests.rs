//! Opening, reading and writing a file, and the bracket its handle lives inside.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Handle, Async};
        let _ = (match Async/block_on(File/read_all("data.txt"))!
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(contents) => Handle/write(Handle/stdout, contents)
            | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("error"))
            end
        end)!;
        /std/Io/pure(())
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
        use /std/{File, Handle, Async};
        match Async/block_on(File/read_all("nope.txt"))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("contents")
            | failure(e) =>
                match e : (_) => /std/Io({})
                | not_found() => /std/print("not found")
                | permission_denied() => /std/print("denied")
                | exists() => /std/print("exists")
                | refused() => /std/print("refused")
                | tls() => /std/print("tls")
                | would_block() => /std/print("would block")
                | other(_) => /std/print("other")
                end
            end
        end
        "#;

    assert_eq!(run(source), b"not found");
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Handle, Async};
        match Async/block_on(File/with("out.txt", File/Mode/write(), (f) => File/write(f, /std/Str/to_bytes("written"))))!
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("ok")
            | failure(_) => /std/print("error")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// Matching on an effectful scrutinee must evaluate it exactly once — the erased inductive match binds the scrutinee in a `let` and projects from it. Append mode makes a second evaluation visible: it would append twice.
#[test]
fn read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Handle, Str, Bytes, Async};
        let _ = (match Async/block_on(File/with("lines.txt", File/Mode/read(), (f) =>
            Async/bind(File/read(f, 1024), (r) =>
                match r : (_) => Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x[])
                | error(_) => Async/pure(x[])
                end)))!
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Handle/write(Handle/stdout, bytes)
            | failure(_) => Handle/write(Handle/stdout, Str/to_bytes("error"))
            end
        end)!;
        /std/Io/pure(())
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}
