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
