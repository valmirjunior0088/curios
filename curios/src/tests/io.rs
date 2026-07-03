use {curios_rt::MockHost, std::time::Duration};

#[test]
fn io_write() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bin("hello"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello");
}

#[test]
fn io_write_stderr() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stderr, /std/Str/to_bin("oops"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"oops");
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        match std/Io/read(std/Io/stdin, 1024) : {}
        | chunk(b) => let w = std/Io/write(std/Io/stdout, b); ()
        | eof() => ()
        | error(_) => ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io};
        let show(r : Io/Read) -> {} =
            match r : {}
            | chunk(b) => let _ = Io/write(Io/stdout, b); ()
            | eof() => Io/print("1")
            | error(_) => Io/print("e")
            end;
        let _ = show(Io/read(Io/stdin, 2));
        let _ = show(Io/read(Io/stdin, 2));
        show(Io/read(Io/stdin, 2))
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

#[test]
fn file_read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("data.txt"))
        | success(contents) => Io/write(Io/stdout, contents)
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("data.txt", "file contents")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"file contents");
}

#[test]
fn file_read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/read_all("nope.txt"))
        | success(_) => Io/print("contents")
        | failure(e) =>
            match e : {}
            | not_found() => Io/print("not found")
            | permission_denied() => Io/print("denied")
            | exists() => Io/print("exists")
            | refused() => Io/print("refused")
            | tls() => Io/print("tls")
            | would_block() => Io/print("would block")
            | other(_) => Io/print("other")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"not found");
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Io, Task};
        match Task/block_on(File/with("out.txt", Io/Mode/write(), (f) => File/write(f, /std/Str/to_bin("written"))))
        | success(_) => Io/print("ok")
        | failure(_) => Io/print("error")
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// Matching on an effectful scrutinee must evaluate it exactly once — the
// erased inductive match binds the scrutinee in a `let` and projects from it.
// Append mode makes a second evaluation visible: it would append twice.
#[test]
fn file_read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Io, Str, Bin, Task};
        match Task/block_on(File/with("lines.txt", Io/Mode/read(), (f) =>
            Task/bind(File/read(f, 1024), (r) =>
                match r : Task(Bin)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(\\)
                | error(_) => Task/pure(\\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bin("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}

#[test]
fn std_io_read_line_sequences_lines() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Str};
        let program : Reader({}) =
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match first : Reader({})
            | some(a) =>
                match second : Reader({})
                | some(b) =>
                    match Str/of_bin(Bin/concat(a, b)) : Reader({})
                    | some(s) => Reader/pure(Io/print(s))
                    | none() => Reader/pure(Io/print("invalid utf-8"))
                    end
                | none() => Reader/pure(Io/print("missing"))
                end
            | none() => Reader/pure(Io/print("missing"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["alpha", "beta"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"alpha\nbeta\n");
}

#[test]
fn std_io_read_line_signals_eof_with_none() {
    let source = r#"
        use /std/{Io, Reader, Option};
        let program : Reader({}) =
            let first = Reader/read_line!;
            let second = Reader/read_line!;
            match second : Reader({})
            | some(_) => Reader/pure(Io/print("line"))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["only"]).build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"eof");
}

// A line longer than `read_line`'s 1024-byte refill chunk forces the buffer
// to absorb one full chunk, miss the newline, and refill before slicing.
#[test]
fn std_io_read_line_spans_refills() {
    let source = r#"
        use /std/{Io, Reader, Option, Bin, Nat};
        let program : Reader({}) =
            let line = Reader/read_line!;
            match line : Reader({})
            | some(bytes) => Reader/pure(Io/print(Nat/to_str(Bin/len(bytes))))
            | none() => Reader/pure(Io/print("eof"))
            end;
        Reader/run(program, Io/stdin)
        "#;

    let long_line = "a".repeat(1500);
    let (system, io) = MockHost::builder()
        .stdin_lines([long_line.as_str()])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"1501");
}

#[test]
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `Lst(Bin)`; indexing it round-trips one entry.
    let (system, io) = MockHost::builder().args(["prog", "hello", "world"]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stdout, /std/Option/unwrap_or(/std/Lst/get(/std/Proc/args(), 1), \\))"#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"hello");
}

#[test]
fn proc_env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        match /std/Proc/env("HOME") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"/root");
}

#[test]
fn proc_env_absent_is_none() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        match /std/Proc/env("NOPE") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bin("missing")); ()
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"missing");
}

#[test]
fn proc_exit_halts_with_code() {
    // exit traps: it surfaces its code *and* the trailing write never runs.
    let entrypoint = r#"
        let _ : std/False = /std/Proc/exit(7);
        std/Io/write(std/Io/stdout, /std/Str/to_bin("unreachable"))
        "#
    .parse::<crate::text::Entrypoint>()
    .expect("failed to parse source");

    let module = crate::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        &crate::text::NullLoader,
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system).expect("execution succeeded");

    assert_eq!(code, 7);
    assert!(io.output().is_empty());
}
