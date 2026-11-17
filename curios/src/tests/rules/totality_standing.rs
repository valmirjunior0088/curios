//! What a binder standing for another establishes of it: a lambda's binder applied on the spot stands for its argument, so what an arm rules of the binder it rules of the argument where the binder stands for it bare, nowhere else, and only while the arm stands.

use super::test_support::*;

// A lambda applied on the spot to a `let`-bound local stands for what the `let` stands for, as every graded call reads the local: `u` is `n`, so the arm on `x` takes `n` apart, and `f(k)`, and `g(x - 1)` past a default, descend. Each refusal hands back `x` itself, which is `n` again.
#[test]
fn a_lambda_applied_to_a_let_bound_local_stands_for_what_the_let_stands_for() {
    control(
        r#"
        use /std/{Nat, Bool};

        let f(n: Nat) -> Bool =
            let u = n;
            ((x: Nat) => match x | 0 => false | k + 1 => f(k) end)(u);

        let F(n: Nat) -> Prop = Bool/Holds(f(n));

        let g(n: Nat) -> Bool =
            let u = n;
            ((x: Nat) => match x | 0 => false | _ => g(x - 1) end)(u);

        let G(n: Nat) -> Prop = Bool/Holds(g(n));

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
