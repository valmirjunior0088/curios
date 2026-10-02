//! A partial definition at an erased position, however it is reached, and what does not count as descent.

use super::test_support::*;

/// A partial definition behind a `Type`-sorted carrier, reached four ways, and a diverging proof at a position the judgment infers rather than checks: each is a proof resting on something that may not terminate, however it is reached.
#[test]
fn a_proof_resting_on_a_partial_definition_is_refused_however_it_is_reached() {
    for source in [
        PARTIAL_DIRECT,
        PARTIAL_THROUGH_WITNESS,
        PARTIAL_HIGHER_ORDER,
        PARTIAL_IN_FIELD,
        INFERRED_PROOF_POSITION,
    ] {
        rejected_by(source, "not known to terminate");
    }
}

#[test]
fn a_recursion_written_inline_under_a_carrier_is_still_partial() {
    rejected_by(INLINE_REC_UNDER_CARRIER, "not known to terminate");
}

#[test]
fn a_type_reaching_a_partial_definition_is_refused() {
    rejected_by(TYPE_REACHING_PARTIAL, "not known to terminate");
}

#[test]
fn saturating_subtraction_is_not_descent() {
    rejected_by(
        SATURATING_SUBTRACTION_IS_NOT_DESCENT,
        "not known to terminate",
    );
}

#[test]
fn permuting_arguments_is_not_descent() {
    rejected_by(PERMUTING_ARGUMENTS_IS_NOT_DESCENT, "not known to terminate");
}
