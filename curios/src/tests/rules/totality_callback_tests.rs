//! The size-change obligation through a callback: a call in a lambda handed to a definition is graded where the definition applies the parameter the lambda stands for, as the summary `curios_analysis::group_applications` files states, and a parameter the definition uses any other way publishes nothing.

use super::test_support::*;

// A summary holds every application of its parameter however the body reaches it, or holds nothing: `h` applies `f` below `n` and nowhere else, so `spin`'s recursion through the lambda it hands `h` descends, and each refusal has `h`'s `0` arm apply `f` once more, at `n + 1`, by one route the walk summarizing `h` must follow or count as a use it does not see — directly, through a `let` alias, a closure a `let` binds, a local function, a local recursive definition, a guard, a tuple's projection, a constructor's payload, and a witness's method. Read as unseen, any one of them leaves the summary holding `f(j)` alone, below `n`, and `spin(1)`, which is `h(1, …)`, which is `h(0, …)` applying the lambda at `1`, is `spin(1)` again.
#[test]
fn a_callback_applied_by_any_route_at_a_value_not_below_is_not_graded_below() {
    let control = control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        induct Box : Type
        | hold(g : (Nat) -> True)
        end

        concept Runs(A : Type) : Type {
            run(A, (Nat) -> True, Nat) -> True,
        }

        satisfy Runs(Nat) {
            run(_, g, m) = g(m),
        }

        let h(n : Nat, f : (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, f) | j + 1 => f(j) end
            end;

        let spin(n : Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    );
    for route in [
        "| 0 => f(n + 1)",
        "| 0 => let g = f; g(n + 1)",
        "| 0 => let g = (x : Nat) => f(x); g(n + 1)",
        "| 0 => let g(x : Nat) -> True = f(x); g(n + 1)",
        "| 0 => let go(x : Nat, d : Nat) -> True = match d | 0 => f(x) | e + 1 => go(x, e) end; go(n + 1, 1)",
        "| 0, (match f(n + 1) | qed() => true end) => True/qed() | 0 => True/qed()",
        "| 0 => (f, n + 1).0((f, n + 1).1)",
        "| 0 => match Box/hold(f) | hold(g) => g(n + 1) end",
        "| 0 => Runs/run(n, f, n + 1)",
    ] {
        control
            .refused(&[("| 0 => True/qed()", route)])
            .by_elaborator_in("/spin", |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in("/spin", |error| {
                matches!(error, curios_cert::Error::NotDescending { .. })
            });
    }
}

// The bind a `!` lowers to is a route the walk follows: `h` sequences `f(j)` with a `!`, which `/std/Monad/bind` receives as its action, so `h` applies `f` below `n`, and `Spun`, a type reaching `spin`'s recursion through the lambda it hands `h`, is formed. The refusal sequences `f(n + 1)` in `h`'s `0` arm: read as unseen, it would leave `spin(1)`, which sequences `spin(1)` again, total.
#[test]
fn a_callback_sequenced_with_a_bang_is_graded_where_the_bind_applies_it() {
    control(
        r#"
        use /std/{Nat, Option, Bool};
        use /std/Bool/{Holds};

        let h(n: Nat, f: (Nat) -> Option(Bool)) -> Option(Bool) =
            match n
            | 0 => Option/some(true)
            | k + 1 => match k | 0 => h(k, f) | j + 1 => let b = f(j)!; Option/some(b) end
            end;

        let spin(n: Nat) -> Option(Bool) = h(n, (c) => spin(c));

        let Spun(n: Nat) -> Prop = Holds(match spin(n) | some(_) => true | none() => false end);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| 0 => Option/some(true)",
        "| 0 => let b = f(n + 1)!; Option/some(b)",
    )])
    .by_elaborator_in("/Spun", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spun", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback eta-expanded into another definition's callback is graded through both summaries: `at` applies `g` at `m` or below it, and `h` hands `at` the lambda `(x) => f(x)` with `j`, below `n`, so `f` is applied below `n` and `spin` descends. The refusal hands `at` `n` itself, where `f` is applied at `n`, and `spin(2)` is `spin(2)` again.
#[test]
fn a_callback_eta_expanded_into_another_definitions_callback_is_graded_through_both_summaries() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let at(m: Nat, g: (Nat) -> True) -> True =
            match m | 0 => g(m) | i + 1 => match i | 0 => at(i, g) | _ + 1 => g(m) end end;

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, f) | j + 1 => at(j, (x) => f(x)) end
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("at(j, (x) => f(x))", "at(n, (x) => f(x))")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// An implicit callback is a parameter as an explicit one is: `h` takes `f` under `@`, applies it below `n` and hands it on at its own place, so `spin` descends. The refusal hands `f` to `at` in `h`'s `0` arm, which applies it at `n + 1`: handed to another definition, `f` publishes nothing, where read as unseen `spin(1)` would be a total `spin(1)` again.
#[test]
fn an_implicit_callback_handed_to_another_definition_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let at(@g: (Nat) -> True, m: Nat) -> True = g(m);

        let h(n: Nat, @f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, @f) | j + 1 => f(j) end
            end;

        let spin(n: Nat) -> True = h(n, @(c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => True/qed()", "| 0 => at(@f, n + 1)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A callback handed to a member at another place than its own publishes nothing at either: `h` applies `f` below `n` and `g` at `n + 1`, and hands both on in place, so `spin`'s recursion through the lambda it hands `h` as `f` descends. The refusal hands them on exchanged, so the lambda `spin` hands as `f` is applied as `g` one call down, at what is `n` again: read as handed on at their own places, `f` would still be summarized below `n`, and `spin(1)` would be a total `spin(1)` again.
#[test]
fn a_callback_handed_to_a_member_at_another_place_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True, g: (Nat) -> True) -> True =
            match n
            | 0 => g(n + 1)
            | k + 1 => let _ = f(k); h(k, f, g)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c), (c) => True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("h(k, f, g)", "h(k, g, f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A callback a member answers publishes nothing: `give` hands its `g` on to `h` at the same place, inside the function it answers, so `h`'s `f` is applied below `n` and `spin` descends. The refusal has `give` answer `g` itself at `0`, which `h` applies at `n + 1`, and `spin(1)` is `spin(1)` again.
#[test]
fn a_callback_a_member_answers_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => give(n, f)(n + 1)
            | k + 1 => match k | 0 => h(k, f) | j + 1 => f(j) end
            end
        and give(m: Nat, g: (Nat) -> True) -> (Nat) -> True =
            match m
            | 0 => (_) => True/qed()
            | i + 1 => (_) => h(i, g)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => (_) => True/qed()", "| 0 => g")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A lambda handed to a member in its callback's own place is another function, whose applications of the callback are counted where the lambda is written: `h` applies `f` below `n` and hands it on, so `spin` descends. The refusal hands `h` the closure `(x) => f(x + 2)` in `f`'s place, which one call down applies `f` at what is `n` again: counted as `f` handed on, it would leave `f` summarized by `f(k)` alone, and `spin(2)` would be a total `spin(2)` again.
#[test]
fn a_closure_handed_in_its_callbacks_own_place_is_counted_where_it_is_written() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => let _ = f(k); h(k, f)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("h(k, f)", "h(k, (x) => f(x + 2))")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}
