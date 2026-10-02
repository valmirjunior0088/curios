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
