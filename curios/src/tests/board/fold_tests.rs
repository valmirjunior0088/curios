//! A fold's arms learn nothing of its scrutinee, and its motive reaches the scrutinee only through the binder it declares.

use {super::test_support::*, crate::tests::run};

// The induction hypothesis of a fold is typed at the motive opened at the tail, inside an arm where the scrutinee is refined to the cons value. A motive that names the scrutinee instead of binding it has that occurrence refined too, so the hypothesis is assumed at the arm's own goal, and `ih` alone proves `Eq()(k + 1, 0)` from `Eq()(k, 0)`. Without the refusal, every program below compiles, runs, and prints, certifying a closed inhabitant of `Eq()(1, 0)`.
#[test]
fn a_fold_motive_may_not_capture_its_scrutinee() {
    rejected_by(
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE,
        "through the binder it declares",
    );
}

// The same rule over the `List` carrier, whose cons arm binds the element as well: the carriers share one arm rule, and the refusal sits in front of it.
#[test]
fn a_list_fold_motive_may_not_capture_its_scrutinee() {
    rejected_by(
        A_LIST_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE,
        "through the binder it declares",
    );
}

// An expression scrutinee reaches the arm through a case equation rather than a substitution, and the equation refines every occurrence of its spelling — including the one the motive captured. The occurrence test is syntactic, which is exactly the reach of the equation.
#[test]
fn a_fold_motive_may_not_capture_its_scrutinee_expression() {
    rejected_by(
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_EXPRESSION,
        "through the binder it declares",
    );
}

// A `let` alias of the scrutinee is a definition the reducer reads through, so the elaborator reads the motive through it as well; the kernel sees the alias substituted away and refuses on the spelling itself.
#[test]
fn a_fold_motive_may_not_capture_its_scrutinee_through_an_alias() {
    rejected_by(
        A_FOLD_MOTIVE_MAY_NOT_CAPTURE_ITS_SCRUTINEE_THROUGH_AN_ALIAS,
        "through the binder it declares",
    );
}

// The control: a motive that binds the scrutinee types the hypothesis at the tail, and the ordinary induction goes through.
#[test]
fn a_fold_motive_that_binds_its_scrutinee_still_folds() {
    assert_eq!(
        run(A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS),
        b"3"
    );
}

// A fold takes its identity arm at the end of every value, where the scrutinee is whatever it was. Typed with `n` at `0`, the arm reads `h` as a proof of `Holds(0 == 2)`, and the fold at `2` is a proof of `False`. The arm learns nothing of `n`, so `h` stands at the type it was given.
#[test]
fn the_identity_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    rejected_by(
        THE_IDENTITY_ARM_OF_A_FOLD_READS_ITS_SCRUTINEE_AT_NO_CASE,
        "inferred: Holds(n == 2)",
    );
}

// The cons arm is taken at every tail, and `n` is `k + 1` at the outermost alone: `h` beside the hypothesis at the tail would refute each other at every other.
#[test]
fn the_cons_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    rejected_by(
        THE_CONS_ARM_OF_A_FOLD_READS_ITS_SCRUTINEE_AT_NO_CASE,
        "inferred: Holds(n == 2)",
    );
}

// An expression scrutinee is refined through a recorded equation where a variable is substituted, and an arm of a fold records none.
#[test]
fn an_arm_of_a_fold_over_a_call_records_no_equation() {
    rejected_by(
        AN_ARM_OF_A_FOLD_OVER_A_CALL_RECORDS_NO_EQUATION,
        "inferred: Holds(g(x) == 2)",
    );
}

// The carriers share one arm rule: a list's fold reads its scrutinee at no case either.
#[test]
fn an_arm_of_a_list_fold_reads_its_scrutinee_at_no_case() {
    rejected_by(
        AN_ARM_OF_A_LIST_FOLD_READS_ITS_SCRUTINEE_AT_NO_CASE,
        "inferred: Holds(List/len(@Nat, xs) == 2)",
    );
}

// The control: a hypothesis about the scrutinee rides in the motive, which restates it at each tail, and the arm hands the hypothesis at the tail the proof it asks for.
#[test]
fn a_hypothesis_about_a_folds_scrutinee_rides_in_its_motive() {
    assert_eq!(
        run(A_HYPOTHESIS_ABOUT_A_FOLDS_SCRUTINEE_RIDES_IN_ITS_MOTIVE),
        b"3"
    );
}
