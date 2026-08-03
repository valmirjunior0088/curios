use {
    curios_pipeline::compile_entrypoint,
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn io_write() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        r#"std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("hello"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello");
}

#[test]
fn io_write_stderr() {
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        r#"std/Handle/write(std/Handle/stderr, /std/Str/to_bytes("oops"))"#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"oops");
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        r#"
        match std/Handle/read(std/Handle/stdin, 1024) : (_) => {}
        | chunk(b) => let w = std/Handle/write(std/Handle/stdout, b); ()
        | eof() => ()
        | error(_) => ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

// `Handle/read(h, n)` is the typed blocking read: each call yields a `chunk` of 1..n available bytes (here one injected line per refill, served in `n`-byte slices), and the third read past the data yields `eof`.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Handle};
        let show(r : Handle/Read) -> {} =
            match r : (_) => {}
            | chunk(b) => let _ = Handle/write(Handle/stdout, b); ()
            | eof() => /std/print("1")
            | error(_) => /std/print("e")
            end;
        let _ = show(Handle/read(Handle/stdin, 2));
        let _ = show(Handle/read(Handle/stdin, 2));
        show(Handle/read(Handle/stdin, 2))
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

#[test]
fn file_read_all_reads_a_seeded_file() {
    let source = r#"
        use /std/{File, Handle, Async};
        match Async/block_on(File/read_all("data.txt"))
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(contents) => Handle/write(Handle/stdout, contents)
            | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("error"))
            end
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("data.txt", "file contents")])
        .build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"file contents");
}

#[test]
fn file_read_all_of_a_missing_path_is_not_found() {
    let source = r#"
        use /std/{File, Handle, Async};
        match Async/block_on(File/read_all("nope.txt"))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("contents")
            | failure(e) =>
                match e : (_) => {}
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"not found");
}

#[test]
fn file_with_write_mode_persists_through_close() {
    let source = r#"
        use /std/{File, Handle, Async};
        match Async/block_on(File/with("out.txt", File/Mode/write(), (f) => File/write(f, /std/Str/to_bytes("written"))))
        | failure(_) => /std/print("deadlock")
        | success(outcome) =>
            match outcome
            | success(_) => /std/print("ok")
            | failure(_) => /std/print("error")
            end
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
    assert_eq!(io.file(b"out.txt"), Some(b"written".to_vec()));
}

// Matching on an effectful scrutinee must evaluate it exactly once — the erased inductive match binds the scrutinee in a `let` and projects from it. Append mode makes a second evaluation visible: it would append twice.
#[test]
fn file_read_pulls_bytes_inside_the_bracket() {
    let source = r#"
        use /std/{File, Handle, Str, Bytes, Async};
        match Async/block_on(File/with("lines.txt", File/Mode/read(), (f) =>
            Async/bind(File/read(f, 1024), (r) =>
                match r : (_) => Async(Bytes)
                | chunk(b) => Async/pure(b)
                | eof() => Async/pure(x\)
                | error(_) => Async/pure(x\)
                end)))
        | failure(_) => Handle/write(Handle/stdout, /std/Str/to_bytes("deadlock"))
        | success(outcome) =>
            match outcome
            | success(bytes) => Handle/write(Handle/stdout, bytes)
            | failure(_) => Handle/write(Handle/stdout, Str/to_bytes("error"))
            end
        end
        "#;

    let (system, io) = MockHost::builder()
        .files([("lines.txt", "first\nsecond\n")])
        .build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"first\nsecond\n");
}

#[test]
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `Lst(Bytes)`; indexing it round-trips one entry.
    let (system, io) = MockHost::builder().args(["prog", "hello", "world"]).build();
    crate::run_text(r#"std/Handle/write(std/Handle/stdout, /std/Option/unwrap_or(/std/Lst/get(/std/proc/args(), 1), x\))"#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"hello");
}

#[test]
fn proc_env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    crate::run_text(
        r#"
        match /std/proc/env("HOME") : (_) => {}
        | some(v) => let _ = std/Handle/write(std/Handle/stdout, v); ()
        | none() => let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("missing")); ()
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
        r#"
        match /std/proc/env("NOPE") : (_) => {}
        | some(v) => let _ = std/Handle/write(std/Handle/stdout, v); ()
        | none() => let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("missing")); ()
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
        let _ : {} = /std/proc/exit(7);
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("unreachable"))
        "#
    .parse::<Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
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
    // A local binding evaluates under call-by-value even when nothing reads it: the never-returning body runs. Regression test: erasure used to collapse such bindings to the unit constant wholesale, silently dropping the exit.
    let entrypoint = r#"
        use /std/{Nat, Handle, Str};
        let go(n : std/Nat) -> std/Nat =
            let dead = /std/proc/exit(3);
            n;
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(go(1))))
        "#
    .parse::<Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = MockHost::builder().build();
    let code =
        crate::run_wasm(&module, system, ForeignBindings::empty()).expect("execution succeeded");

    assert_eq!(code, 3);
    assert!(io.output().is_empty());
}

// `drain` treats `eof` as the stream's only orderly terminator. The load-bearing script is chunk-then-error: the accumulated prefix must not be passed off as complete content, so the verdict is a failure and the prefix's length leaks nowhere. Chunk-then-eof is the control that accumulation itself still works.
#[test]
fn async_drain_surfaces_a_read_error_instead_of_a_partial_prefix() {
    let source = r#"
        use /std/{Nat, Bytes, Handle, Result, Async, Cell, Str, print};
        let show(r : Result(Result(Bytes, Handle/Error), Async/Deadlock)) -> Str =
            match r
            | failure(_) => "deadlock"
            | success(inner) =>
                match inner
                | success(bytes) => Str/concat("ok:", Nat/to_str(Bytes/len(bytes)))
                | failure(_) => "error"
                end
            end;
        let error_first(n : Nat) -> Async(Handle/Read) =
            Async/pure(Handle/Read/error(Handle/error_of(255)));
        let chunk_then_error() -> (Nat) -> Async(Handle/Read) =
            let calls = Cell/new(0);
            (n) =>
                let k = Cell/get(calls);
                let _ = Cell/set(calls, k + 1);
                match k
                | 0 => Async/pure(Handle/Read/chunk(x\41\42))
                | _ => Async/pure(Handle/Read/error(Handle/error_of(255)))
                end;
        let chunk_then_eof() -> (Nat) -> Async(Handle/Read) =
            let calls = Cell/new(0);
            (n) =>
                let k = Cell/get(calls);
                let _ = Cell/set(calls, k + 1);
                match k
                | 0 => Async/pure(Handle/Read/chunk(x\41\42\43))
                | _ => Async/pure(Handle/Read/eof())
                end;
        let _ = print(show(Async/block_on(Async/drain(error_first))));
        let _ = print(" / ");
        let _ = print(show(Async/block_on(Async/drain(chunk_then_error()))));
        let _ = print(" / ");
        print(show(Async/block_on(Async/drain(chunk_then_eof()))))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"error / error / ok:3");
}
