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
        let a : Io({}) = print("a");
        let b : Io({}) = print("b");
        Io/bind(a, (_) => b)
        "#;

    assert_eq!(run(source), b"ab");
}

/// Postfix `!` is `/syn/Monad/bind`, and the `Monad(Io)` witness is what makes it reach `Io`. The explicit chain above is the control.
#[test]
fn bang_sequences_a_description_like_an_explicit_bind() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = print("a");
        let b : Io({}) = print("b");
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
        Io/bind(n, (k) => print(Nat/to_str(k + 1)))
        "#;

    assert_eq!(run(source), b"8");
}

/// The whole of what makes an `Io` a *description*: it erases to a thunk, so binding one to a name and forcing it twice performs it twice.
///
/// `let a = …` names a description without performing it, so the two forces below are what produce the two writes. Calling `print` is not one of them: post-retype it builds an `Io({})` and performs nothing, which is why the name can be reused at all.
#[test]
fn a_description_bound_once_and_forced_twice_performs_twice() {
    let source = r#"
        use /std/{Io, print};
        let step(_ : {}) -> Io({}) = print("x");
        let a : Io({}) = Io/bind(Io/pure(()), step);
        Io/bind(a, (_) => a)
        "#;

    assert_eq!(run(source), b"xx");
}

/// The program tail is where the retype is load-bearing: a tail of non-`Io` type describes nothing to perform, so the pipeline refuses it rather than emitting a program that runs and does nothing.
#[test]
fn a_non_io_tail_is_refused() {
    let error = typecheck(
        r#"
        use /std/{Nat};
        let n : Nat = 7;
        n
        "#,
    )
    .expect_err("a pure tail must be refused");

    assert!(
        error.contains("Nat"),
        "the refusal must name the tail's actual type: {error}"
    );
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
