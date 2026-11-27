//! What a binder standing for another establishes of it: a lambda's binder applied on the spot stands for its argument, so what an arm rules of the binder it rules of the argument where the binder stands for it bare, nowhere else, and only while the arm stands.

use super::test_support::*;

// A lambda applied on the spot to a `let`-bound local stands for what the `let` stands for, as every graded call reads the local: `u` is `n`, so the arm on `x` takes `n` apart, and `f(k)`, and `g(x - 1)` past a default, descend. Each refusal hands back `x` itself, which is `n` again.
#[test]
fn a_lambda_applied_to_a_let_bound_local_stands_for_what_the_let_stands_for() {
    control(
        r#"
        use /std/{Nat, Bool};

        let f(n: Nat) -> Bool =>
            let u = n;
            ((x: Nat) => match x | 0 => false | k + 1 => f(k) end)(u);

        let F(n: Nat) -> /std/Bool/True =>
            match f(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        let g(n: Nat) -> Bool =>
            let u = n;
            ((x: Nat) => match x | 0 => false | _ => g(x - 1) end)(u);

        let G(n: Nat) -> /std/Bool/True =>
            match g(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| k + 1 => f(k) end", "| k + 1 => f(x) end")])
    .by_elaborator_in("/F", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/F", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Zero ruled out of a lambda's binder is ruled out of what it stands for only where it stands for it bare. Applied on the spot to `n + 1`, the binder is a successor, which a dispatch's default rules zero out of whatever `n` is, so `n - 1` past it is at most `n`, and at `0` the call hands `f` its own argument. The control applies the lambda to `n`, which the default then rules zero out of, so the call descends.
#[test]
fn zero_ruled_out_of_a_binder_standing_for_a_successor_is_ruled_out_of_no_parameter() {
    control(
        r#"
        use /std/{Nat, Bool};

        let f(n : Nat) -> Bool =>
            ((x : Nat) => match x | 0 => false | _ => f(n - 1) end)(n);

        let F(n : Nat) -> /std/Bool/True =>
            match f(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("end)(n);", "end)(n + 1);")])
    .by_elaborator_in("/F", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/F", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What an arm establishes of a lambda's binder, and so of the parameter the binder stands for, leaves with the arm: past a `let` whose value split the binder, the parameter is neither the nonzero a default ruled nor the successor a `k + 1` arm read, so `n - 1` there is at most `n`, and at `0` the call hands `g` its own argument. Each control makes the call inside the arm, where it descends.
#[test]
fn what_an_arm_establishes_of_an_applied_lambdas_binder_leaves_with_the_arm() {
    control(
        r#"
        use /std/{Nat, Bool};

        let g(n : Nat) -> Bool =>
            ((x : Nat) =>
                let b : Bool = match x | 0 => false | _ => g(n - 1) end;
                match b | true => false | false => false end)(n);

        let G(n : Nat) -> /std/Bool/True =>
            match g(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("| _ => g(n - 1) end;", "| _ => true end;"),
        ("| false => false end)(n);", "| false => g(n - 1) end)(n);"),
    ])
    .by_elaborator_in("/G", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/G", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
    control(
        r#"
        use /std/{Nat, Bool};

        let h(n : Nat) -> Bool =>
            ((x : Nat) =>
                let b : Bool = match x | 0 => false | k + 1 => h(n - 1) end;
                match b | true => false | false => false end)(n);

        let H(n : Nat) -> /std/Bool/True =>
            match h(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("| k + 1 => h(n - 1) end;", "| k + 1 => true end;"),
        ("| false => false end)(n);", "| false => h(n - 1) end)(n);"),
    ])
    .by_elaborator_in("/H", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/H", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// An arm's case stands for the scrutinee as a graded call reads it, a variable an index inversion solved standing as written: under `q`'s arm, whose inversion solved `t` to `k`, `match t` takes `t` apart, so `direct(j, …)` descends, as matching `t` before `q` does. The refusal hands `t` back.
#[test]
fn an_arm_after_an_inversion_takes_apart_the_variable_a_call_names() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Q: (Nat) -> Type
        | c(k: Nat): (k)
        end

        let direct(t: Nat, q: Q(t)) -> Bool =>
            match q
            | c(k) =>
                match t
                | 0 => false
                | j + 1 => direct(j, Q/c(j))
                end
            end;

        let Direct(t: Nat, q: Q(t)) -> /std/Bool/True =>
            match direct(t, q) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| j + 1 => direct(j, Q/c(j))",
        "| j + 1 => direct(t, Q/c(t))",
    )])
    .by_elaborator_in("/Direct", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Direct", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A guard reads its comparison as a graded call reads it, as an arm's case does: under `q`'s arm, whose inversion solved `t` to `k`, `match t == 0` rules zero out of `t`, so `guarded(t - 1, …)` on its `false` arm descends. The refusal hands `t` back.
#[test]
fn a_guard_after_an_inversion_rules_zero_out_of_the_variable_a_call_names() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Q: (Nat) -> Type
        | c(k: Nat): (k)
        end

        let guarded(t: Nat, q: Q(t)) -> Bool =>
            match q
            | c(k) =>
                match t == 0
                | true => false
                | false => guarded(t - 1, Q/c(t - 1))
                end
            end;

        let Guarded(t: Nat, q: Q(t)) -> /std/Bool/True =>
            match guarded(t, q) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| false => guarded(t - 1, Q/c(t - 1))",
        "| false => guarded(t, Q/c(t))",
    )])
    .by_elaborator_in("/Guarded", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Guarded", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
