//! The process surface: argv, environment, and exit.

use {
    super::super::{run, run_text},
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
