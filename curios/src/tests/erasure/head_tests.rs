//! A head is read at the type the checkers accepted it at. Erasure reads a callee's type off the finished term and needs its head, and a type's head may show only where reduction puts a question to conversion: a stuck fold's atoms classed, an arm's equation met at another spelling. Both checkers ask it, so the reader erasure reads through asks it too.
//!
//! Each program types its callee `g` at a `Pick` of a decision that is `true` to conversion and stuck to the fold alone. Mutation-checked: with the reader's reduction plain (`Asking::set_plain` in `Retyping::new`) the prelude erases to the bytes it had and each of these panics at `erase: applied a non-function`.

use crate::tests::run;

// The fold of `==` pairs its atoms by spelling, and `f(same(n))` is no spelling of `f(n)`: the two are one call to conversion, which unfolds `same`.
#[test]
fn a_callee_is_typed_through_a_comparison_of_two_spellings_of_one_call() {
    let source = r#"
        use /std/{Nat, Bool};

        let Pick(c: Bool) -> Type =
            match c: (_) => Type
            | true => (Nat) -> Nat
            | false => Nat
            end;

        let same(n: Nat) -> Nat = n;

        let applied(f: (Nat) -> Nat, n: Nat, g: Pick(f(same(n)) == f(n))) -> Nat = g(3);

        /std/print(Nat/to_str(applied((m) => m + 1, 2, (m) => m + 40)))
        "#;

    assert_eq!(run(source), b"43");
}

// The arm's equation is recorded under `f(n)` and the callee's type names `f(same(n))`: a stuck form met at the equation's reduced spelling, which is conversion's to say is the scrutinee.
#[test]
fn a_callee_is_typed_through_an_arms_equation_met_at_another_spelling() {
    let source = r#"
        use /std/{Nat, Bool};

        let Pick(c: Bool) -> Type =
            match c: (_) => Type
            | true => (Nat) -> Nat
            | false => Nat
            end;

        let same(n: Nat) -> Nat = n;

        let guarded(f: (Nat) -> Bool, n: Nat, g: Pick(f(same(n)))) -> Nat =
            match f(n)
            | true => g(3)
            | false => 0
            end;

        /std/print(Nat/to_str(guarded((m) => m == 2, 2, (m) => m + 40)))
        "#;

    assert_eq!(run(source), b"43");
}

// Two calls differing in a proof of one bound are one term to conversion and two atoms to the fold.
#[test]
fn a_callee_is_typed_through_a_comparison_of_calls_differing_in_a_proof() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let Pick(c: Bool) -> Type =
            match c: (_) => Type
            | true => (Nat) -> Nat
            | false => Nat
            end;

        let weighed(
            n: Nat,
            p: Holds(0 < n),
            q: Holds(0 < n),
            k: (Holds(0 < n)) -> Nat,
            g: Pick(k(p) == k(q)),
        ) -> Nat = g(3);

        /std/print(Nat/to_str(weighed(1, True/qed(), True/qed(), (_) => 5, (m) => m + 40)))
        "#;

    assert_eq!(run(source), b"43");
}

// A function and its expansion over one binder are one term to conversion and two atoms to the fold.
#[test]
fn a_callee_is_typed_through_a_comparison_of_a_function_and_its_expansion() {
    let source = r#"
        use /std/{Nat, Bool};

        let Pick(c: Bool) -> Type =
            match c: (_) => Type
            | true => (Nat) -> Nat
            | false => Nat
            end;

        let through(
            h: ((Nat) -> Nat) -> Nat,
            f: (Nat) -> Nat,
            g: Pick(h((x) => f(x)) == h(f)),
        ) -> Nat = g(3);

        /std/print(Nat/to_str(through((f) => f(1), (m) => m + 1, (m) => m + 40)))
        "#;

    assert_eq!(run(source), b"43");
}
