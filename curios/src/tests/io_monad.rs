//! The `Io` vocabulary: constructing, sequencing, and forcing descriptions.
//!
//! `Io(T)` is an inert description of a computation yielding a `T`, and the only thing that ever forces one is the emitted entrypoint boundary. These tests pin that reading rather than merely tolerating it — most of all [`a_description_bound_once_and_forced_twice_performs_twice`], which is what distinguishes a description from a value that happened to be computed early.

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

#[test]
fn pure_and_bind_sequence_a_description() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = Io/pure(print("a"));
        let b : Io({}) = Io/pure(print("b"));
        Io/bind(a, (_) => b)
        "#;

    assert_eq!(run(source), b"ab");
}

/// Postfix `!` is `/syn/Monad/bind`, and the `Monad(Io)` witness is what makes it reach `Io`. The explicit chain above is the control.
#[test]
fn bang_sequences_a_description_like_an_explicit_bind() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = Io/pure(print("a"));
        let b : Io({}) = Io/pure(print("b"));
        let _ = a!;
        b
        "#;

    assert_eq!(run(source), b"ab");
}

/// The result flows through the bind, so a description is not merely a sequencing token.
#[test]
fn bind_passes_the_action_result_to_its_continuation() {
    let source = r#"
        use /std/{Io, Nat, print};
        let n : Io(Nat) = Io/pure(7);
        Io/bind(n, (k) => Io/pure(print(Nat/to_str(k + 1))))
        "#;

    assert_eq!(run(source), b"8");
}

/// The whole of what makes an `Io` a *description*: it erases to a thunk, so binding one to a name and forcing it twice performs it twice.
///
/// The effect must sit where the thunk can hold it, which is inside a `bind`'s continuation rather than under a `pure`. The language is eager, so `Io/pure(e)` has evaluated `e` at the call site before any node exists: sequencing is what delays, not `pure`. Post-retype that costs nothing — an operand of non-`Io` type is pure — but it is the shape a description's semantics must be stated against.
#[test]
fn a_description_bound_once_and_forced_twice_performs_twice() {
    let source = r#"
        use /std/{Io, print};
        let step(_ : {}) -> Io({}) = Io/pure(print("x"));
        let a : Io({}) = Io/bind(Io/pure(()), step);
        Io/bind(a, (_) => a)
        "#;

    assert_eq!(run(source), b"xx");
}

/// A pure tail is untouched by the entrypoint boundary — the force is type-directed, and nothing but an `Io` tail gets one.
#[test]
fn a_pure_tail_still_runs_as_it_always_did() {
    assert_eq!(run(r#"/std/print("plain")"#), b"plain");
}

/// `Io` is opaque: no constructors, no `PrimHead` entry, no projection, and above all no eliminator from `Io(T)` to `T`. A scrutinee of description type therefore has nothing to eliminate — a bare `_` would be an irrefutable binder match rather than an elimination, so the arms here are concrete.
#[test]
fn an_io_scrutinee_is_refused() {
    let error = typecheck(
        r#"
        use /std/{Io, print};
        let a : Io({}) = Io/pure(());
        match a
        | true => print("t")
        | false => print("f")
        end
        "#,
    )
    .expect_err("matching a description must be refused");

    assert!(
        !error.is_empty(),
        "the refusal must carry a diagnostic: {error}"
    );
}

/// `Monad(Io)` is occupied by `/std/Io` and cannot be occupied twice. A user program is refused on either of two independent grounds — the orphan rule, since it owns neither `/syn`'s concept nor `/sys`'s type head, and one-witness-per-key — and the operative fact is the same: the program's `!` always means the prelude's witness.
#[test]
fn a_program_cannot_register_a_second_monad_witness_for_io() {
    let error = typecheck(
        r#"
        use /std/{Io, Monad};

        satisfy Monad(Io) {
            pure(x) = /sys/Io/pure(x),
            bind(m, f) = /sys/Io/bind(m, f),
        }

        Io/pure(())
        "#,
    )
    .expect_err("a duplicate Monad(Io) witness must be refused");

    assert!(
        !error.is_empty(),
        "the refusal must carry a diagnostic: {error}"
    );
}
