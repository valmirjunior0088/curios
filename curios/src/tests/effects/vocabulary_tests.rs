//! Each vocabulary in a region: an `Io` description sequenced and performed, `State` threaded, and `Result`'s early return.

use {
    crate::tests::{run, typecheck},
    curios_utilities::fill,
};

// === The `Io` vocabulary: constructing, sequencing, and forcing descriptions. ====

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

/// Postfix `!` is `/std/Monad/bind`, and the `Monad(Io)` witness is what makes it reach `Io`. The explicit chain above is the control.
#[test]
fn bang_sequences_a_description_like_an_explicit_bind() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = print("a");
        let b : Io({}) = print("b");
        let entry() -> /std/Io({}) =
            let _ = a!;
            b;
        entry()
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
/// `let a = …` names a description without performing it, so the two forces below are what produce the two writes. Calling `print` is not one of them: it builds an `Io({})` and performs nothing, which is why the name can be reused at all.
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

/// The program tail is where `Io` being a description is load-bearing: a tail of non-`Io` type describes nothing to perform, so the pipeline refuses it rather than emitting a program that runs and does nothing.
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

/// `Io` is opaque: no constructors, no `IntrinsicHead` entry, no projection, and above all no eliminator from `Io(T)` to `T`. A scrutinee of description type therefore has nothing to eliminate — a bare `_` would be an irrefutable binder match rather than an elimination, so the arms here are concrete.
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

/// `Monad(Io)` is occupied by `/std/Io` and cannot be occupied twice. A user program is refused on either of two independent grounds — the orphan rule, since it owns neither the concept nor `/sys`'s type head, and one-witness-per-key — and the operative fact is the same: the program's `!` always means the prelude's witness.
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

// === `/std/State`: pure, deterministic state threading. ==========================

/// The full vocabulary in one region: `get`, `set`, `modify`, and the `state` constructor, sequenced with `!` and run purely at the boundary.
#[test]
fn state_threads_through_a_bang_region() {
    let source = r#"
        use /std/{Nat, State, print};
        use /std/State/{state, get, set, modify};
        pub let fresh() -> State(Nat, Nat) =
            let n = get()!;
            let _ = set(n + 1)!;
            State/pure(n);
        pub let steps() -> State(Nat, {Nat, Nat}) =
            let a = fresh()!;
            let _ = modify((n) => n * 2)!;
            let b = state((s) => (s, s + 1))!;
            State/pure((a, b));
        let entry() -> /std/Io({}) =
            let out = State/run(steps(), 3);
            let _ = print(Nat/to_str(out.0.0))!;
            let _ = print(Nat/to_str(out.0.1))!;
            print(Nat/to_str(out.1));
        entry()
        "#;

    // init 3: fresh yields 3 (state 4), modify doubles to 8, state yields 8 (state 9): (3, 8) with final state 9.
    assert_eq!(run(source), b"389");
}

/// Purity is enforced by an absent edge, not a convention: no `Lift(Io, State(S))` witness exists, so an `Io` action cannot sequence in a `State` region.
#[test]
fn a_state_region_cannot_perform_io() {
    let source = r#"
        use /std/{Nat, State, print};
        pub let leak() -> State(Nat, {}) =
            let _ = print("effect")!;
            State/pure(());
        let out = State/run(leak(), 0);
        print("unreachable")
        "#;

    let error = typecheck(source).expect_err("a State region must refuse Io");
    assert!(
        error.contains("no witness of /std/Monad/Lift(/std/Io, (x) => State(Nat, x))"),
        "expected the missing Io edge, got: {error}"
    );
}

// === `/std/Result` as a monad: `raise` and `!` as checked early return. ====

/// The success path computes through; the failure path short-circuits at the `raise`, skipping the rest of the region. A `Result` is its own monad, error first, so no wrapper stands between the data type and the region.
#[test]
fn a_raise_short_circuits_the_region() {
    let source = r#"
        use /std/{Nat, Str, Result, print};
        use /std/Result/{raise, rescue};
        pub let checked_div(a: Nat, b: Nat) -> Result(Str, Nat) =
            match 0 < b
            | false => raise("division by zero")
            | true => Result/pure(a / b)
            end;
        pub let compute(a: Nat, b: Nat) -> Result(Str, Nat) =
            let q = checked_div(a, b)!;
            Result/pure(q + 1);
        let entry() -> /std/Io({}) =
            let ok =
                match compute(10, 2)
                | success(n) => Nat/to_str(n)
                | failure(e) => e
                end;
            let caught =
                match rescue(compute(1, 0), (_) => Result/pure(0))
                | success(n) => Nat/to_str(n)
                | failure(e) => e
                end;
            let _ = print(ok)!;
            print(caught);
        entry()
        "#;

    // 10/2 + 1 = 6; the rescued division by zero yields the handler's 0.
    assert_eq!(run(source), b"60");
}

/// A plain `Result` value sequences in a `Result` region with nothing to convert: the constructors are the monad's own.
#[test]
fn a_result_value_sequences_in_a_result_region() {
    let source = r#"
        use /std/{Nat, Str, Result, print};
        pub let add_pair(a: Result(Str, Nat), b: Result(Str, Nat)) -> Result(Str, Nat) =
            let x = a!;
            let y = b!;
            Result/success(x + y);
        let shown =
            match add_pair(Result/success(40), Result/success(2))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        print(shown)
        "#;

    assert_eq!(run(source), b"42");
}

/// A result computed from a settled argument is read for any declaration, not only a format: `true` is a literal, so `Shape(true)` reduces to `Io({})` and the tail lifts. A numeral is a literal no type has realized while the action is still unelaborated, so a result computed from one is not read, and the explicit lift is how it is embedded.
#[test]
fn a_result_computed_from_a_settled_argument_is_read() {
    let source = |argument: &str, tail: &str| {
        fill!(
            r#"
            use /std/{Str, Nat, Bool, Try, Io, Monad, print};
            let Shape(b: ${argument}) -> Type =
                match b | ${first} => Io({}) | ${rest} => Nat end;
            let say(b: ${argument}) -> Shape(b) =
                match b | ${first} => print("said\n") | ${rest} => 0 end;
            let execute() -> Try(Io, Str, {}) =
                ${tail};
            execute()
            "#,
            argument = argument,
            first = if argument == "Bool" { "true" } else { "0" },
            rest = if argument == "Bool" { "false" } else { "_ + 1" },
            tail = tail,
        )
    };

    assert_eq!(run(&source("Bool", "say(true)")), b"said\n");
    assert_eq!(run(&source("Nat", "Monad/lift(say(0))")), b"said\n");
    let error = typecheck(&source("Nat", "say(0)")).expect_err("a numeral is not read");
    assert!(
        error.contains("an action of one monad where another is expected"),
        "expected the lift report, got: {error}"
    );
}
