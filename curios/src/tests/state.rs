//! `/std/State`: pure, deterministic state threading.
//!
//! A `State(S, A)` region sequences with `!` like any monad — the parametric `Monad((A) => State(S, A))` witness resolves through partial-application keying, and the bind's monad pins by right-biased imitation from the region's type. What a `State` region can never do is perform an effect: no `Lift(Io, State(S))` edge exists, and its absence is the guarantee.

use {
    super::run,
    curios_text::{Entrypoint, RootSource},
};

/// Compile-only, for the programs whose point is that they are refused.
fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    curios_pipeline::compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// The full vocabulary in one region: `get`, `put`, `modify`, and the `state` constructor, sequenced with `!` and run purely at the boundary.
#[test]
fn state_threads_through_a_bang_region() {
    let source = r#"
        use /std/{Nat, State, print};
        use /std/State/{state, get, put, modify};
        pub let fresh: State(Nat, Nat) =
            let n = get()!;
            let _ = put(n + 1)!;
            State/pure(n);
        pub let steps: State(Nat, {Nat, Nat}) =
            let a = fresh!;
            let _ = modify((n) => n * 2)!;
            let b = state((s) => (s, s + 1))!;
            State/pure((a, b));
        let out = State/run(steps, 3);
        let _ = print(Nat/to_str(out.0.0))!;
        let _ = print(Nat/to_str(out.0.1))!;
        print(Nat/to_str(out.1))
        "#;

    // init 3: fresh yields 3 (state 4), modify doubles to 8, state yields 8 (state 9): (3, 8) with final state 9.
    assert_eq!(run(source), b"389");
}

/// Purity is enforced by an absent edge, not a convention: no `Lift(Io, State(S))` witness exists, so an `Io` action cannot sequence in a `State` region.
#[test]
fn a_state_region_cannot_perform_io() {
    let source = r#"
        use /std/{Nat, State, print};
        pub let leak: State(Nat, {}) =
            let _ = print("effect")!;
            State/pure(());
        let out = State/run(leak, 0);
        print("unreachable")
        "#;

    let error = typecheck(source).expect_err("a State region must refuse Io");
    assert!(
        error.contains("no witness of Lift(Io, /std/State/State")
            || error.contains("no witness of Lift(Io, State"),
        "expected the missing Io edge, got: {error}"
    );
}
