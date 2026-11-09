//! A binder's mark: part of a function type's identity, and no part of what a call, an arm or a projection reduces to.
//!
//! The rule is `documentation/design/theory/plicity-is-part-of-function-identity.md`. The kernel's halves are `curios_cert`'s `plicity_distinguishes_two_function_types`, `a_function_type_is_below_none_of_other_marks` and `a_lambda_of_other_marks_does_not_check_against_a_function_type`.

use {
    super::test_support::*, crate::tests::test_support::*, curios_common::Plicity,
    curios_elab::Error,
};

// A family computing a function type of one mark or the other, and a value of the first handed where the second is wanted. Wrapped in a call, the reference keeps its type and nothing is inserted, so what refuses it is conversion holding the two function types apart by their marks. The control hands the same value where its own type is wanted and calls it.
#[test]
fn a_function_type_is_not_aliased_across_a_mark() {
    control(
        r#"
        use /std/{Nat, Bool};

        let F(b: Bool) -> Type =
            match b
            | true => (@x: Nat) -> Nat
            | false => (x: Nat) -> Nat
            end;
        let pass(@T: Type, t: T) -> T = t;
        let f: F(true) = (@x) => x;
        let g: F(true) = pass(@F(true), f);
        /std/print(Nat/to_str(g(@3)))
        "#,
        b"3",
    )
    .refused(&[
        ("let g: F(true)", "let g: F(false)"),
        ("g(@3)", "g(3)"),
    ])
    .by_elaborator_in(
        "/g",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "(@x: Nat) -> Nat"),
    );
}

// An equation between two functions apart in a mark cannot be stated, since `Eq`'s two sides share one type and no function type bears both marks: `(n) => n` against `(@n) => ((x) => x)(n)`, which a mark-blind η contraction would let `ground` hold. Under one mark, either one, the two are one function: η contracts the second only where its argument stands under its binder's mark, so under `@` it stays a function at its own type and its body reduces to the first's. The refusal is derived from the equation under the explicit mark.
#[test]
fn an_equation_between_two_functions_apart_in_a_mark_is_not_stated() {
    let hidden = r#"
        use /std/{Nat, Eq};

        let stated: Eq()((@n: Nat) => n, (@n: Nat) => ((x: Nat) => x)(n)) = Eq/refl();
        /std/print("ok")
        "#;
    assert_eq!(run(hidden), b"ok");

    control(
        r#"
        use /std/{Nat, Eq};

        let stated: Eq()((n: Nat) => n, (n: Nat) => ((x: Nat) => x)(n)) = Eq/refl();
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "(n: Nat) => ((x: Nat) => x)(n)",
        "(@n: Nat) => ((x: Nat) => x)(n)",
    )])
    .by_elaborator_in("/stated", |error| {
        matches!(
            error,
            Error::BinderPlicityMismatch {
                expected: Plicity::Explicit,
                written: Plicity::Implicit,
                ..
            }
        )
    });
}

// A hidden member stands at its own position in what a call, an arm and a projection reduce to: `f(1, @2, 3)` is `3`, `tag(x, z)` binds `z` to the last payload and `.1` over `Sized` reads `items`, so the sum is `9`. Read by mark or counted among the written positions, each reads the hidden `2` instead and the sum is `6`, which `Holds` would then make `True`. The control states the sum it reduces to.
#[test]
fn a_hidden_member_reduces_at_its_own_position() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        induct Tagged: Type
        | tag(a: Nat, @size: Nat, k: Nat)
        end
        struct Sized: Type { a: Nat, @len: Nat, items: Nat }
        let f: (a: Nat, @b: Nat, c: Nat) -> Nat = (x, z) => z;
        let read(t: Tagged) -> Nat = match t | tag(x, z) => z end;
        let s: Sized = Sized { a = 1, @len = 2, items = 3 };
        let fact: Holds(f(1, @2, 3) + read(Tagged/tag(1, @2, 3)) + s.1 == 9) = True/qed();
        /std/print(Nat/to_str(f(1, @2, 3) + read(Tagged/tag(1, @2, 3)) + s.1))
        "#,
        b"9",
    )
    .refused(&[
        ("let fact: Holds(", "let lie: Holds("),
        ("s.1 == 9)", "s.1 == 6)"),
    ])
    .by_elaborator_in(
        "/lie",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/False"),
    );
}
