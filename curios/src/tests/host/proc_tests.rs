//! The process surface: argv, environment, exit, and children run through `/std/proc`'s `Command`.

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
let _ = std/Handle/write(std/Handle/stdout, /std/Option/unwrap_or(/std/List/try_get(/std/proc/args!, 1), x[]))!;
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
fn env_absent_is_none() {
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
fn exit_halts_with_code() {
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
fn exit_in_local_binding_halts() {
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

/// A program computing one `Str` under `Async/block_on`, with the `proc` names in scope.
fn child_program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Str, Bytes, Nat, Option, Result, Show, Async, Io, Handle, proc}};
        let text(b: Bytes) -> Str = Option/unwrap_or(Str/of_bytes(b), "?");
        let program: Async(Str) =
            {body};
        match Async/block_on(program)!
        | failure(_) => /std/print("deadlock")
        | success(s) => /std/print(s)
        end
        "#
    )
}

// `run` captures both outputs and the exit: each pipe is drained in a task of its own and both are joined before the child is waited for, so neither output can stall the other.
#[test]
fn run_captures_both_outputs_and_the_exit() {
    let source = child_program(
        r#"
            let r = proc/run(proc/Command/new("greet", ["world"]))!;
            Async/pure(
                match r
                | success(out) => Str/flatten([text(out.stdout), "|", text(out.stderr), "|", Show/show(out.exit)])
                | failure(e) => Show/show(e)
                end)
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

// `status` inherits every stream and reports how the child ended; a signal shows as `signaled`, and a program the host cannot find is `not_found`, as an unknown path is to `open`.
#[test]
fn status_reports_a_signal_and_an_unknown_program_is_not_found() {
    let source = child_program(
        r#"
            let crashed = proc/status(proc/Command/new("crash", []))!;
            let missing = proc/status(proc/Command { ..proc/Command/new("missing", []), cwd = Option/some("/tmp") })!;
            let show(r: Result(Io/Error, proc/Exit)) -> Str =
                match r | success(e) => Show/show(e) | failure(e) => Show/show(e) end;
            Async/pure(Str/join(" ", [show(crashed), show(missing)]))
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("crash", "", "", 0, 9)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"signaled(9) not_found");
}

// The child is acquired with `kill` as its finalizer, so a task cancelled while its child runs kills it: the mock records the program name when `kill` reaches it.
#[test]
fn a_cancelled_task_kills_the_child_it_spawned() {
    let source = child_program(
        r#"
            let body: Async({}) =
                let started = proc/spawn(proc/Command/new("sleepy", []))!;
                let _ = Async/sleep(/std/time/Duration/of(60, 0))!;
                Async/pure(());
            let task = Async/spawn(body)!;
            let _ = Async/yield_now!;
            let _ = Async/lift(Async/cancel(task))!;
            let _ = Async/yield_now!;
            Async/pure("cancelled")
        "#,
    );

    let (system, io) = MockHost::builder()
        .children([("sleepy", "", "", 0, 0)])
        .build();
    run_text(&source, system).expect("expected result");
    assert_eq!(io.output(), b"cancelled");
    assert_eq!(io.kills(), vec![b"sleepy".to_vec()]);
}
