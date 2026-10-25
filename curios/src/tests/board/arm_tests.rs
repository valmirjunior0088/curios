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
