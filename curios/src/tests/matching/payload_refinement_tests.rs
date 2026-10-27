//! What an arm learns from its scrutinee's payload: the value it carried, kept through arithmetic and a refinement key, and the equations it may not assume.

use {
    crate::tests::{error, run, run_text},
    curios_native::MockHost,
};

// A refinement key is stored at the arm and probed wherever the scrutinee is mentioned again, so the two spellings have to compare equal. A scrutinee carrying an *inferred* metavariable is the hard case: `Pred/test(t, b)` elaborates to `(?w).0(t, b)`, and a second occurrence mints its own `?w'`, so two terms solved to the same witness would key differently and the arm would silently refine nothing. Solved metavariables are materialized into the key, which is what makes the two spellings one.
//
// Three constraints shape the scrutinee, and they pull against each other. It must carry a metavariable, or there is nothing to materialize. It must not reduce away, or the store is never reached — hence a method whose body eliminates the *symbolic* `b`. And its head must be one the kernel's refinement store reads. A concept dispatch is all three at once, and it is what `/std/Str/fold` and `/std/Str/Valid`'s decoder refine on in production — their `rem == 1` guard — rather than a shape invented here.
//
// The materialized key is what meets the second occurrence for one lookup. With the metavariables left standing in it the program still holds, the occurrence being met at the stuck form reduction leaves it as, against the guard's reduced spelling, which materializes them too.
#[test]
fn an_inferred_implicit_does_not_break_a_refinement_key() {
    let source = r#"
        use /std/{Eq, Bool, Nat, Str};

        concept Pred(A : Type) : pub Type {
            test(A, Bool) -> Bool,
        }

        satisfy Pred(Nat) {
            test(n, b) = match b | true => true | false => false end,
        }

        let refined(t : Nat, b : Bool) -> Str =
            match Pred/test(t, b)
            | true =>
                let p : Eq()(true, Pred/test(t, b)) = Eq/refl();
                "refined"
            | false => "unrefined"
            end;

        /std/print(refined(7, true))
        "#;

    assert_eq!(run(source), b"refined");
}

// An arm is opened at the forced constructor's own payload, so a matched payload reduces to the value that constructor carried. Opening it at projections of the scrutinee instead reduces to the same value but leaves a residual Core cannot type — `Proj` has no rule for an inductive — and the difference is invisible until such a residual reaches conversion as a metavariable solution candidate, where re-validation refuses it as `NotATuple`: a hard verdict that fails the goal outright rather than parking it. A parameterized family is what keeps the projection from reducing away first, and a phantom parameter suffices, so the shape reaches `/std/Option/map` and every container whose operation returns what a match arm bound.
#[test]
fn a_matched_payload_converts_against_the_value_it_carried() {
    let source = r#"
        use /std/{Nat, Option, Eq};

        induct L(V : Type) : Type
        | nil()
        | cons(V, L(V))
        end

        let head_opt(@V : Type, l : L(V)) -> Option(V) =
            match l | nil() => Option/none() | cons(h, t) => Option/some(h) end;

        let carried : Eq()(head_opt(L/cons(7, L/nil())), Option/some(7)) = Eq/refl();

        /std/print("carried")
        "#;

    assert_eq!(run(source), b"carried");
}

// The companion to the arm binding above: opening an arm at the constructor's payload must not make conversion accept an equation that is merely false.
#[test]
fn a_matched_payload_still_refuses_a_false_equation() {
    let source = r#"
        use /std/{Nat, Option, Eq};

        induct L(V : Type) : Type
        | nil()
        | cons(V, L(V))
        end

        let head_opt(@V : Type, l : L(V)) -> Option(V) =
            match l | nil() => Option/none() | cons(h, t) => Option/some(h) end;

        let carried : Eq()(head_opt(L/cons(7, L/nil())), Option/some(8)) = Eq/refl();

        /std/print("carried")
        "#;

    let error = error(source);
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

/// An immediate-encoded arm binds its payload through a read of its own rather than aliasing the scrutinee.
///
/// `stop(Nat)` beside `cons(Nat, L)` takes the `Immediate` family encoding, so `stop`'s payload rides bare. Were its arm's binder the scrutinee itself, `total`'s `acc + z` would demand a raw carrier of the scrutinee; the representation analysis would admit it, since a continuation parameter has no producer to contradict it, and carry the demand back along the loop edge to `build`'s accumulator, where the emitter's cast of a freshly built cell to `(ref i31)` traps for every input above zero — zero alone answers correctly, because the list is then just the bare `stop`.
///
/// The depth is host-tainted deliberately: a closed program folds at compile time and never reaches the emitter, so a closed fixture would pass whatever the emitter did.
#[test]
fn an_immediate_arm_payload_survives_arithmetic_in_a_loop() {
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Byte, Bytes, Nat, Option, Str, Io, Try, Process};

        induct L : Type
        | stop(Nat)
        | cons(Nat, L)
        end

        let build(n : Nat, acc : L) -> L =
            match n : (_) => L
            | 0 => acc
            | m + 1; ih => build(m, L/cons(m, acc))
            end;

        let total(c : L, acc : Nat) -> Nat =
            match c : (_) => Nat
            | stop(z) => acc + z
            | cons(v, tail) => total(tail, acc + v)
            end;

        let entry() -> /std/Io({}) =
            let bytes = match Try/run(Io/read(Process/stdin, 16))! : (_) => Bytes
                | success(bytes(b, @_)) => b
                | success(ended()) => x[]
                | failure(_) => x[]
                end;
            let n = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)), 60);
            /std/print(Nat/to_str(total(build(n, L/stop(n)), 0)));
        entry()
        "#,
        system,
    )
    .expect("expected result");

    // `A` is 65, so the depth is 5: the cons cells carry 0..4 and `stop` carries 5.
    assert_eq!(io.output(), b"15");
}

// An error in an arm its scrutinee's indices rule out says the arm is never taken, and that it may be left out. `zero()` is a `Tag(0)` and the scrutinee a `Tag(1)`, so the arm is checked with no index solved and the bound its body needs has nothing behind it: the report keeps what failed and adds the two types that clash. So does the report on an arm that one variable of the indices forced two ways rules out, `Two(n, n)` against `(0, 1)`, whose goal the case would have refined.
#[test]
fn an_error_in_an_arm_the_indices_rule_out_says_the_arm_may_be_left_out() {
    let literal = error(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        induct Tag: (Nat) -> Type
        | zero(): (0)
        | other(n: Nat): (n + 1)
        end
        let needs_zero(n: Nat, @ok: Holds(n == 0)) -> Nat = 7;
        let w(t: Tag(1)) -> Nat = match t | zero() => needs_zero(1) | other(_) => 0 end;
        /std/print("unreachable")
        "#,
    );
    assert!(
        literal.contains("implicit argument 'ok' of 'needs_zero' was not inferred")
            && literal.contains(
                "this arm is never taken: its case has type Tag(0) and its scrutinee has type Tag(1)"
            )
            && literal.contains("the arm may be left out"),
        "{literal}"
    );

    let variable = error(
        r#"
        use /std/{Nat, Eq};
        induct Two: (a: Nat, b: Nat) -> Type
        | mk(): (0, 1)
        end
        let f(n: Nat, h: Eq()(n, 7), x: Two(n, n)) -> Eq()(n, 7) = match x | mk() => h end;
        /std/print("unreachable")
        "#,
    );
    assert!(
        variable.contains(
            "this arm is never taken: its case has type Two(0, 1) and its scrutinee has type Two(n, n)"
        ) && variable.contains("the arm may be left out"),
        "{variable}"
    );
}
