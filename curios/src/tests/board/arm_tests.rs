//! What an arm is checked at: its own case, and a hypothesis at the instance it was given.

use super::test_support::*;

#[test]
fn an_induction_hypothesis_is_typed_at_the_predecessor() {
    rejected_by(INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE, "type mismatch");
}

#[test]
fn a_dispatch_default_is_checked_at_its_scrutinee() {
    rejected_by(DISPATCH_DEFAULT_AT_A_CASE, "type mismatch");
}

#[test]
fn a_boolean_arm_is_checked_at_its_own_case() {
    rejected_by(BOOL_ARM_AT_THE_WRONG_CASE, "type mismatch");
}

#[test]
fn a_dispatch_literal_arm_is_checked_at_that_literal() {
    rejected_by(DISPATCH_LITERAL_AT_THE_WRONG_VALUE, "type mismatch");
}

// A guard's arm learns its own comparison and nothing stronger: `x <= y` at `true` is no `x < y`, which fails at `x = y`.
#[test]
fn a_guard_at_most_does_not_answer_the_strict_comparison() {
    rejected_by(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let strict(x: Nat, y: Nat, h: Holds(x <= y)) -> Holds(x < y) =
            match x <= y | true => True/qed() | false => match h end end;

        /std/print(Nat/to_str(0))
        "#,
        "type mismatch",
    );
}

// What a classification learned under an arm's equation lives as long as the arm: inside the `0` arm `f(h(x)) - f(0)` is zero, and after the match nothing says `h(x)` is.
#[test]
fn a_classing_made_under_an_arms_equation_answers_nothing_after_the_arm() {
    rejected_by(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, Holds};

        let leak(f: (Nat) -> Nat, h: (Nat) -> Nat, x: Nat) -> Holds(f(h(x)) - f(0) == 0) =
            let warm: Nat = match h(x) | 0 => (let seen: Eq()(f(h(x)) - f(0), 0) = Eq/refl(); 0) | k + 1 => 0 end;
            True/qed();

        /std/print(Nat/to_str(0))
        "#,
        "type mismatch",
    );
}

// What a guard's spelling settled under a split's re-typing answers is the split's: at `c = true` the operands are units and the guard is `k(0)`, inside a lambda there as anywhere in the arm, and after the split nothing says so. `curios-cert`'s `scope_tests` hold the kernel to the same terms.
#[test]
fn a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split() {
    rejected_by(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                let split: Nat = match c | false => 0 | true => ((settled: Holds(k(0))) => 0)(True/qed()) end;
                True/qed()
            end;

        /std/print(Nat/to_str(0))
        "#,
        "type mismatch",
    );
}

// The same guard settled in the arm of a split the operands are units in, and asked in the arm where they are numbers.
#[test]
fn a_guard_settled_in_one_arm_of_a_split_answers_nothing_in_the_other() {
    rejected_by(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | false => {} | true => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                match c
                | false => (let settled: Holds(k(0)) = True/qed(); True/qed())
                | true => True/qed()
                end
            end;

        /std/print(Nat/to_str(0))
        "#,
        "type mismatch",
    );
}
