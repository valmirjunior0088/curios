//! Which guards an arm records an equation under: only a spelling that mentions a local, the same rule in both checkers, so a dead arm under a closed guard is refused where it is written rather than accepted and then refused by the kernel.

use crate::tests::{error, run};

/// The refusal a proof resting on a guard that records nothing gets: the proof's own mismatch, and the note that the arm is never taken.
fn assert_refused_in_a_dead_arm(source: &str) {
    let error = error(source);
    assert!(
        error.contains("type mismatch")
            && error.contains("this arm is never taken: its guard")
            && error.contains("is always false")
            && !error.contains("the kernel refused"),
        "expected the elaborator to refuse the dead arm, got: {error}"
    );
}

/// A dispatch whose method ignores its locals resolves to a spelling that mentions none, `Nat/lt(3, 2)`. The written guard `x == y` is recorded; the resolved one is not, in either checker, so a proof about it is refused in the arm. Mutation-checked with the other three: removing the rule from `refine_head` accepts all four in the elaborator and leaves the kernel to refuse them.
#[test]
fn a_dispatch_that_resolves_to_a_closed_comparison_records_nothing() {
    assert_refused_in_a_dead_arm(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};
        use /std/ops/{Eql};

        struct U: pub Type { Nat }

        satisfy Eql(U) {
            eql(a, b) = Nat/lt(3, 2),
            neq(a, b) = true,
        }

        let dead(x: U, y: U) -> Nat =
            match x == y
            | true =>
                let _p: Holds(Nat/lt(3, 2)) = True/qed();
                0
            | false => 1
            end;

        print(Nat/to_str(dead(U { 0 }, U { 1 })))
        "#,
    );
}

#[test]
fn a_guard_over_a_top_level_name_records_nothing() {
    assert_refused_in_a_dead_arm(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        let flag: Bool = false;

        let dead(n: Nat) -> Nat =
            match flag
            | true =>
                let _p: Holds(flag) = True/qed();
                n
            | false => 0
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

#[test]
fn a_guard_over_a_global_projection_records_nothing() {
    assert_refused_in_a_dead_arm(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        let pair: {Bool, Nat} = (false, 0);

        let dead(n: Nat) -> Nat =
            match pair.0
            | true =>
                let _p: Holds(pair.0) = True/qed();
                n
            | false => 0
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

/// The kernel substitutes a local definition before it checks what follows, so the guard it meets is the closed term the definition names; the elaborator judges the guard in that spelling too.
#[test]
fn a_guard_over_a_local_definition_of_a_closed_term_records_nothing() {
    assert_refused_in_a_dead_arm(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let dead(n: Nat) -> Nat =
            let c = Nat/lt(3, 2);
            match c
            | true =>
                let _p: Holds(c) = True/qed();
                n
            | false => 0
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

/// The control: the same proof under a guard over a parameter is the arm's own equation, recorded in both checkers.
#[test]
fn a_guard_over_a_parameter_records_its_equation() {
    let output = run(r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        let live(b: Bool, n: Nat) -> Nat =
            match b
            | true =>
                let _p: Holds(b) = True/qed();
                n
            | false => 0
            end;

        print(Nat/to_str(live(true, 7)))
        "#);

    assert_eq!(output, b"7");
}

/// The refusal a proof resting on a guard gets in an arm, inside that guard's, whose case contradicts it: the proof's own mismatch, and the note that the arm is never taken.
fn assert_refused_under_a_contradicted_guard(source: &str) {
    let error = error(source);
    assert!(
        error.contains("type mismatch")
            && error.contains("this arm is never taken: in it the guard")
            && !error.contains("the kernel refused"),
        "expected the elaborator to refuse the dead arm, got: {error}"
    );
}

/// A second match on a variable the guard names, at a value the guard excludes. The kernel checks that arm with the variable substituted, so the guard is a closed comparison there and no equation is recorded for it; the elaborator, which keeps the variable spelled, withholds its own in that arm, and the proof is refused where it is written. Mutation-checked with the two below: with `Context::refine` withholding nothing the elaborator accepts all three and the kernel refuses them.
#[test]
fn a_guard_an_inner_case_contradicts_answers_nothing_in_that_arm() {
    assert_refused_under_a_contradicted_guard(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let dead(n: Nat) -> Nat =
            match n < 3
            | true =>
                match n
                | 5 =>
                    let _p: Holds(n < 3) = True/qed();
                    0
                | _ => 1
                end
            | false => 2
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

#[test]
fn a_guard_an_inner_boolean_case_contradicts_answers_nothing_in_that_arm() {
    assert_refused_under_a_contradicted_guard(
        r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{True, Holds};

        let dead(flag: Bool) -> Nat =
            match Bool/not(flag)
            | true =>
                match flag
                | true =>
                    let _p: Holds(Bool/not(flag)) = True/qed();
                    0
                | false => 1
                end
            | false => 2
            end;

        print(Nat/to_str(dead(false)))
        "#,
    );
}

/// The guard is over a call, and it is the call's value at the inner case that contradicts it.
#[test]
fn a_guard_over_a_call_an_inner_case_contradicts_answers_nothing_in_that_arm() {
    assert_refused_under_a_contradicted_guard(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let bound(n: Nat) -> Nat = match n | 0 => 7 | _ => 0 end;

        let dead(n: Nat) -> Nat =
            match bound(n) < 5
            | true =>
                match n
                | 0 =>
                    let _p: Holds(bound(n) < 5) = True/qed();
                    0
                | _ => 1
                end
            | false => 2
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

/// The two matches nested the other way: the guard is written inside the arm that fixes its variable, so it is a closed comparison where it is written, records nothing, and is the arm's own dead guard.
#[test]
fn a_guard_under_the_case_that_contradicts_it_records_nothing() {
    assert_refused_in_a_dead_arm(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let dead(n: Nat) -> Nat =
            match n
            | 5 =>
                match n < 3
                | true =>
                    let _p: Holds(n < 3) = True/qed();
                    0
                | false => 1
                end
            | _ => 2
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
}

/// A second match on a variable an arm around it already fixed: the kernel matches on the value the variable was substituted by, a literal, so the inner arm at another literal refines nothing and is dead. The elaborator judges the scrutinee in that spelling too, and leaves the variable at the value the outer arm gave it. Mutation-checked: with `refine_head` judging the scrutinee with its refined variables still spelled, the elaborator refines the variable again, accepts the proof, and the kernel refuses it.
#[test]
fn a_second_match_on_a_variable_an_arm_fixed_refines_nothing() {
    let error = error(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let dead(n: Nat) -> Nat =
            match n
            | 5 =>
                match n
                | 7 =>
                    let _p: Holds(n == 7) = True/qed();
                    0
                | _ => 1
                end
            | _ => 2
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
    assert!(
        error.contains("type mismatch")
            && error.contains("this arm is never taken: its guard n is always 5")
            && !error.contains("the kernel refused"),
        "expected the elaborator to refuse the dead arm, got: {error}"
    );
}

/// The bound procedure reads no fact from a guard the arm contradicts, so it reports the bound as one that does not follow, never a proof of its own that did not check.
#[test]
fn a_contradiction_claimed_in_a_dead_arm_is_refused_as_one_that_does_not_follow() {
    let error = error(
        r#"
        use /std/{Nat, print};
        use /std/Bool/{False};

        let dead(n: Nat) -> Nat =
            match n < 3
            | true =>
                match n
                | 5 => match False/refuted() end
                | _ => 1
                end
            | false => 2
            end;

        print(Nat/to_str(dead(1)))
        "#,
    );
    assert!(
        !error.contains("which is the compiler's fault") && !error.contains("the kernel refused"),
        "expected the bound to be refused as not following, got: {error}"
    );
}

/// The control: where the inner case agrees with the guard, the guard's fact holds there by reduction, the closed comparison computing to the case its arm assumed.
#[test]
fn a_guard_an_inner_case_agrees_with_holds_there_by_reduction() {
    let output = run(r#"
        use /std/{Nat, print};
        use /std/Bool/{True, Holds};

        let bound(n: Nat) -> Nat = match n | 0 => 1 | _ => 9 end;

        let live(n: Nat) -> Nat =
            match bound(n) < 5
            | true =>
                match n
                | 0 =>
                    let _p: Holds(bound(n) < 5) = True/qed();
                    7
                | _ => 1
                end
            | false => 2
            end;

        print(Nat/to_str(live(0)))
        "#);

    assert_eq!(output, b"7");
}
