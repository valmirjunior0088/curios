//! The process surface: argv, environment and exit through `/std/proc`, and children started through `/std/Command` and reaped through `/std/Command/Child`.

use {
    crate::tests::{run, run_text},
    curios_pipeline::compile_with_prelude,
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn args_indexes_the_argv_snapshot() {
    // argv crosses as a host-built `List(Bytes)`; indexing it round-trips one entry.
    let (system, io) = MockHost::builder().args(["prog", "hello", "world"]).build();
    run_text(r#"
let _ = std/Io/write(std/Io/stdout, /std/Option/unwrap_or(/std/List/try_get(/std/proc/args!, 1), x[]))!;
/std/Io/pure(())
"#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"hello");
}

#[test]
fn env_found_unwraps_to_some() {
    let (system, io) = MockHost::builder().env([("HOME", "/root")]).build();
    run_text(
        r#"
        use /std/{Io};
        match /std/proc/env("HOME")! : (_) => Io({})
        | some(v) => let _ = std/Io/write(std/Io/stdout, v)!; /std/Io/pure(())
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("missing"))!; /std/Io/pure(())
        end
        "#,
        system,
    )
    .expect("expected result");

    assert_eq!(io.output(), b"/root");
}

#[test]
fn env_absent_is_none() {
    assert_eq!(
        run(r#"
        use /std/{Io};
        match /std/proc/env("NOPE")! : (_) => Io({})
        | some(v) => let _ = std/Io/write(std/Io/stdout, v)!; /std/Io/pure(())
        | none() => let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("missing"))!; /std/Io/pure(())
        end
        "#),
        b"missing"
    );
}

#[test]
fn exit_halts_with_code() {
    // exit traps: it surfaces its code *and* the trailing write never runs.
    let entrypoint = r#"
        let _ = /std/proc/exit(7)!;
        let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("unreachable"))!;
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
fn exit_in_local_binding_halts() {
    // A forced description bound to a name nothing reads still performs: `dead` is never mentioned again, and the program still exits 3 without reaching the write. Regression test: erasure used to collapse such bindings to the unit constant wholesale, silently dropping the exit. Post-retype `go` must return an `Io` for the force to have a region at all — an unforced `proc/exit(3)` would be an inert description, which is the whole point of the carrier.
    let entrypoint = r#"
        use /std/{Nat, Str, Io};
        let go(n : std/Nat) -> Io(std/Nat) =
            let dead = /std/proc/exit(3)!;
            Io/pure(n);
        let v = go(1)!;
        let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes(std/Nat/to_str(v)))!;
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

#[test]
fn an_exit_alone_in_the_tail_carries_its_code() {
    // A program needing no nominal rows used to emit an empty recursion group, which Binaryen's reader refuses; the roster group is omitted when there is nothing to declare.
    let entrypoint = r#"
        /std/proc/exit(3)
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

/// A program computing one `Str` inside a `Try` region over `Async`, with the process names in scope; a failure that escapes the region prints as its name.
fn child_program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Nat, Option, Result, Show, Try, Async, Io, Path, Command}};
        use /std/Command/{{Child}};
        let text(b: Bytes) -> Str = Option/unwrap_or(Str/of_bytes(b), "?");
        let program: Try(Async, Io/Error, Str) =
            {body};
        let fiber: Async({{}}) =
            let r = Try/run(program)!;
            match r
            | failure(e) => /std/print(Show/show(e))
            | success(s) => /std/print(s)
            end;
        Async/run(fiber)
        "#
    )
}

// `run` captures both outputs and the exit: each pipe is drained in a task of its own and both are joined before the child is waited for, so neither output can stall the other.
#[test]
fn run_captures_both_outputs_and_the_exit() {
    let source = child_program(
        r#"
            let out = Command/run(Command/new("greet", ["world"]))!;
            Try/pure(Str/flatten([text(out.stdout), "|", text(out.stderr), "|", Show/show(out.exit)]))
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("greet", "hello\n", "warn\n", 0, 0)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"hello\n|warn\n|exited(0)");
    assert!(
        io.kills().is_empty(),
        "a child that was waited for is not killed"
    );
}

// `status` wires every stream as the command says and reports how the child ended; a signal shows as `signaled`, and a program the host cannot find is `not_found` at its own `run`, as an unknown path is to `open`.
#[test]
fn status_reports_a_signal_and_an_unknown_program_is_not_found() {
    let source = child_program(
        r#"
            let crashed = Command/status(Command/new("crash", []))!;
            let missing = Try/run(Command/status(Command { ..Command/new("missing", []), cwd = Option/some(Path/of_str("/tmp")) }))!;
            let shown = match missing | success(e) => Show/show(e) | failure(e) => Show/show(e) end;
            Try/pure(Str/join(" ", [Show/show(crashed), shown]))
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("crash", "", "", 0, 9)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"signaled(9) not_found");
}

// A child spawned inside `Child/with` is killed when the task around it is cancelled: the mock records the program name when `kill` reaches it.
#[test]
fn a_cancelled_task_kills_the_child_it_spawned() {
    let source = child_program(
        r#"
            let body: Try(Async, Io/Error, {}) =
                let child = Command/spawn(Command/new("sleepy", []))!;
                Child/with(child, Async/sleep(/std/time/Duration/of(60, 0)));
            let task = Async/spawn(Try/run(body))!;
            let _ = Async/yield_now!;
            let _ = Async/cancel(task)!;
            let _ = Async/yield_now!;
            Try/pure("cancelled")
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("sleepy", "", "", 0, 0)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"cancelled");
    assert_eq!(io.kills(), vec![b"sleepy".to_vec()]);
}

// A child's pipes are streams: `spawn` with piped output hands back a `Child/Pipe` that `stream/read_all` drains through the `Read` witness, and `wait` reaps the child afterwards.
#[test]
fn a_piped_output_is_read_through_the_stream_witness() {
    let source = child_program(
        r#"
            let child = Command/spawn(Command { ..Command/new("greet", []), stdout = Command/Stdio/piped() })!;
            let read =
                match Child/stdout(child)
                | some(p) => /std/stream/read_all(p)
                | none() => Async/pure(Result/success(x[]))
                end;
            let out = read!;
            let bytes = out!;
            let exit = Child/wait(child)!;
            Try/pure(Str/flatten([text(bytes), "|", Show/show(exit)]))
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("greet", "hello\n", "", 0, 0)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"hello\n|exited(0)");
}
