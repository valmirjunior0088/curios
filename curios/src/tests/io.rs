use {
    super::{run, run_text},
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn io_write() {
    assert_eq!(
        run(r#"
let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("hello"))!;
/std/Io/pure(())
"#),
        b"hello"
    );
}

#[test]
fn io_write_stderr() {
    assert_eq!(
        run(r#"
let _ = std/Handle/write(std/Handle/stderr, /std/Str/to_bytes("oops"))!;
/std/Io/pure(())
"#),
        b"oops"
    );
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        match std/Handle/read(std/Handle/stdin, 1024)! : (_) => /std/Io({})
        | chunk(b) => let w = std/Handle/write(std/Handle/stdout, b)!; /std/Io/pure(())
        | eof() => /std/Io/pure(())
        | error(_) => /std/Io/pure(())
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
        use /std/{Handle, Io};
        let show(r : Handle/Read) -> Io({}) =
            match r : (_) => Io({})
            | chunk(b) => let _ = Handle/write(Handle/stdout, b)!; /std/Io/pure(())
            | eof() => /std/print("1")
            | error(_) => /std/print("e")
            end;
        let _ = show(Handle/read(Handle/stdin, 2)!)!;
        let _ = show(Handle/read(Handle/stdin, 2)!)!;
        show(Handle/read(Handle/stdin, 2)!)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

#[test]
fn file_read_all_reads_a_seeded_file() {
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
fn file_read_all_of_a_missing_path_is_not_found() {
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
fn file_read_pulls_bytes_inside_the_bracket() {
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

#[test]
fn proc_args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `List(Bytes)`; indexing it round-trips one entry.
    let (system, io) = MockHost::builder().args(["prog", "hello", "world"]).build();
    run_text(r#"
let _ = std/Handle/write(std/Handle/stdout, /std/Option/unwrap_or(/std/List/get(/std/proc/args!, 1), x[]))!;
/std/Io/pure(())
"#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"hello");
}

#[test]
fn proc_env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    run_text(
        r#"
        use /std/{Io};
        match /std/proc/env("HOME")! : (_) => Io({})
        | some(v) => let _ = std/Handle/write(std/Handle/stdout, v)!; /std/Io/pure(())
        | none() => let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("missing"))!; /std/Io/pure(())
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"/root");
}

#[test]
fn proc_env_absent_is_none() {
    assert_eq!(
        run(r#"
        use /std/{Io};
        match /std/proc/env("NOPE")! : (_) => Io({})
        | some(v) => let _ = std/Handle/write(std/Handle/stdout, v)!; /std/Io/pure(())
        | none() => let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("missing"))!; /std/Io/pure(())
        end
        "#),
        b"missing"
    );
}

#[test]
fn proc_exit_halts_with_code() {
    // exit traps: it surfaces its code *and* the trailing write never runs.
    let entrypoint = r#"
        let _ = /std/proc/exit(7)!;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#
    .parse::<Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
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
    // A forced description bound to a name nothing reads still performs: `dead` is never mentioned again, and the program still exits 3 without reaching the write. Regression test: erasure used to collapse such bindings to the unit constant wholesale, silently dropping the exit. Post-retype `go` must return an `Io` for the force to have a region at all — an unforced `proc/exit(3)` would be an inert description, which is the whole point of the carrier.
    let entrypoint = r#"
        use /std/{Nat, Handle, Str, Io};
        let go(n : std/Nat) -> Io(std/Nat) =
            let dead = /std/proc/exit(3)!;
            Io/pure(n);
        let v = go(1)!;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(v)))!;
        /std/Io/pure(())
        "#
    .parse::<Entrypoint>()
    .expect("failed to parse source");

    let (module, _foreigns) = compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
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
        use /std/{Nat, Bytes, Handle, Result, Async, Cell, Str, Io, print};
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
        let chunk_then_error : Io((Nat) -> Async(Handle/Read)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(Handle/Read/chunk(x[0x41, 0x42]))
                | _ => Async/pure(Handle/Read/error(Handle/error_of(255)))
                end);
        let chunk_then_eof : Io((Nat) -> Async(Handle/Read)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(Handle/Read/chunk(x[0x41, 0x42, 0x43]))
                | _ => Async/pure(Handle/Read/eof())
                end);
        let _ = print(show(Async/block_on(Async/drain(error_first))!))!;
        let _ = print(" / ")!;
        let _ = print(show(Async/block_on(Async/drain(chunk_then_error!))!))!;
        let _ = print(" / ")!;
        print(show(Async/block_on(Async/drain(chunk_then_eof!))!))
        "#;

    assert_eq!(run(source), b"error / error / ok:3");
}
