use {
    curios_runtime::{ForeignBindings, MockHost},
    std::time::Duration,
};

#[test]
fn io_write() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stdout, /std/Str/to_bytes("hello"))"#,
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
        r#"std/Io/write(std/Io/stderr, /std/Str/to_bytes("oops"))"#,
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

// `Io/read(h, n)` is the typed blocking read: each call yields a `chunk` of
// 1..n available bytes (here one injected line per refill, served in `n`-byte
// slices), and the third read past the data yields `eof`.
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
        | failure(_) => Io/write(Io/stdout, /std/Str/to_bytes("error"))
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
        match Task/block_on(File/with("out.txt", Io/Mode/write(), (f) => File/write(f, /std/Str/to_bytes("written"))))
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
        use /std/{File, Io, Str, Bytes, Task};
        match Task/block_on(File/with("lines.txt", Io/Mode/read(), (f) =>
            Task/bind(File/read(f, 1024), (r) =>
                match r : Task(Bytes)
                | chunk(b) => Task/pure(b)
                | eof() => Task/pure(x\)
                | error(_) => Task/pure(x\)
                end)))
        | success(bytes) => Io/write(Io/stdout, bytes)
        | failure(_) => Io/write(Io/stdout, Str/to_bytes("error"))
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}

#[test]
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `Lst(Bytes)`; indexing it round-trips one entry.
    let (system, io) = MockHost::builder().args(["prog", "hello", "world"]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"std/Io/write(std/Io/stdout, /std/Option/unwrap_or(/std/Lst/get(/std/proc/args(), 1), x\))"#,
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
        match /std/proc/env("HOME") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("missing")); ()
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
        match /std/proc/env("NOPE") : {}
        | some(v) => let _ = std/Io/write(std/Io/stdout, v); ()
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("missing")); ()
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
        let _ : std/False = /std/proc/exit(7);
        std/Io/write(std/Io/stdout, /std/Str/to_bytes("unreachable"))
        "#
    .parse::<curios_text::Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        curios_text::RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code =
        crate::run_wasm(&module, system, ForeignBindings::empty()).expect("execution succeeded");

    assert_eq!(code, 7);
    assert!(io.output().is_empty());
}

#[test]
fn proc_exit_in_local_binding_halts() {
    // A local binding evaluates under call-by-value even when its result type
    // is a proposition: the never-returning body runs. Regression test:
    // erasure used to collapse proof-typed local bindings to the unit constant
    // wholesale, silently dropping the exit.
    let entrypoint = r#"
        use /std/{Nat, Io, Str};
        let go(n : std/Nat) -> std/Nat =
            let dead = /std/proc/exit(3);
            n;
        std/Io/write(std/Io/stdout, /std/Str/to_bytes(std/Nat/to_str(go(1))))
        "#
    .parse::<curios_text::Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = curios_pipeline::compile_entrypoint(
        Duration::from_secs(10),
        &entrypoint,
        curios_text::RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code =
        crate::run_wasm(&module, system, ForeignBindings::empty()).expect("execution succeeded");

    assert_eq!(code, 3);
    assert!(io.output().is_empty());
}
