//! A fold's motive reaches its scrutinee only through the binder it declares.

use crate::tests::run;

use super::test_support::*;

// The induction hypothesis of a fold is typed at the motive opened at the tail, inside an arm where the scrutinee is refined to the cons value. A motive that names the scrutinee instead of binding it has that occurrence refined too, so the hypothesis is assumed at the arm's own goal, and `ih` alone proves `Eq(k + 1, 0)` from `Eq(k, 0)`. Verified while the hole was open: every program below compiled, ran, and printed, certifying a closed inhabitant of `Eq(1, 0)`.
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
