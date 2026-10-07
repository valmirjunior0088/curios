//! A fold's arms learn nothing of its scrutinee: what an arm reads of it, and what a motive that names it states.

use {super::test_support::*, crate::tests::run};

// The induction hypothesis of a fold is typed at the motive opened at the tail, and no arm of a fold refines the scrutinee. A motive that names the scrutinee instead of binding it therefore states one proposition at every tail, `Eq()(n, 0)` here, and the identity arm's `refl` does not inhabit it. Were the arms typed with the scrutinee at their case, every program below would compile, run, and print, certifying a closed inhabitant of `Eq()(1, 0)`.
#[test]
fn a_fold_motive_that_names_its_scrutinee_states_it_at_every_tail() {
    rejected_by(
        A_FOLD_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_STATES_IT_AT_EVERY_TAIL,
        "expected: Eq(@Nat)(n, 0)",
    );
}

// The same over the `List` carrier, whose cons arm binds the element as well: the carriers share one arm rule.
#[test]
fn a_list_fold_motive_that_names_its_scrutinee_states_it_at_every_tail() {
    rejected_by(
        A_LIST_FOLD_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_STATES_IT_AT_EVERY_TAIL,
        "expected: Eq(@Nat)(List/len(@Nat, l), 0)",
    );
}

// An expression scrutinee reaches an arm taken once through a case equation rather than a substitution, and an arm of a fold records none: the occurrence the motive names stays the expression.
#[test]
fn a_fold_motive_that_names_its_scrutinee_expression_states_it_at_every_tail() {
    rejected_by(
        A_FOLD_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_EXPRESSION_STATES_IT_AT_EVERY_TAIL,
        "expected: Eq(@Nat)(2 * n, 0)",
    );
}

// A `let` alias of the scrutinee is a definition the reducer reads through, and the motive that names the alias states the scrutinee as one that names it does.
#[test]
fn a_fold_motive_that_names_its_scrutinee_through_an_alias_states_it_at_every_tail() {
    rejected_by(
        A_FOLD_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_THROUGH_AN_ALIAS_STATES_IT_AT_EVERY_TAIL,
        "expected: Eq(@Nat)(n, 0)",
    );
}

// A motive that names its scrutinee is no mistake where the arms inhabit what it states: the scrutinee is then a parameter of the fold, fixed at every tail.
#[test]
fn a_fold_whose_arms_inhabit_a_motive_that_names_its_scrutinee_is_accepted() {
    assert_eq!(
        run(A_FOLD_WHOSE_ARMS_INHABIT_A_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_IS_ACCEPTED),
        b"held"
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
