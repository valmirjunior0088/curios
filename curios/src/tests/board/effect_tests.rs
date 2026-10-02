//! An effect is not a value: what may be a scrutinee, an argument, or inhabit a pure arrow.

use crate::tests::run;

use super::test_support::*;

// The arm rule at its one arm with no case value of its own. A `| _ =>` catch-all binds nothing and refines no index, so the only instance it can be checked at is the scrutinee's — which is the instance the elimination then hands its caller.
//
// This is a disagreement between the two checkers rather than a route to `False`: the elaborator checks the catch-all at the actual scrutinee, and a kernel opening the motive's scrutinee binder at the family *type* `/Three` would refuse this program with `/Three` standing in both term positions of the `Eq`. That direction fails closed — nothing well-typed inhabits an expectation with a type substituted for a value — but the certifier's own judgment is the one that matters, and it would establish nothing about the scrutinee for any catch-all it accepted. No prelude catch-all has a scrutinee-dependent motive, so this fixture is where the rule is asked.
//
// The instance is pinned where the rule lives, in `curios_cert::kernel::infer::eliminate::tests`: `a_catch_all_sees_its_own_scrutinee` with its control `a_catch_all_at_another_value_of_the_family_is_refused`, which asserts the expectation itself so that not checking the catch-all cannot pass for closing the hole.
#[test]
fn a_catch_all_is_checked_at_its_scrutinee() {
    assert_eq!(run(A_CATCH_ALL_IS_CHECKED_AT_ITS_SCRUTINEE), b"1");
}

// The purity premise every refinement rests on: an arm is reached only when its scrutinee *equals* the case's value, which a term of non-`Io` type does because it denotes one value. An effectful read does not — two occurrences of `Cell/fill(c, true)` around a `Cell/fill(c, false)` would denote two values, refining both would re-read `p : Eq()(Cell/fill(c, true), true)` at `Eq()(false, true)` in the inner arm, and `/std/Bool/false_neq_true` would turn that into `/std/Bool/False` in an arm the run reaches. Its type is `Io(Bool)`, opaque and with no cases, so nothing lets the program treat it as a `Bool`, and the refusal names `Io`.
#[test]
fn an_effectful_scrutinee_is_not_a_value() {
    rejected_by(AN_EFFECTFUL_SCRUTINEE_IS_NOT_A_VALUE, "Io");
}

#[test]
fn a_match_on_a_forced_cell_read_still_compiles() {
    assert_eq!(run(A_MATCH_ON_A_FORCED_CELL_READ_STILL_COMPILES), b"t");
}

/// Asserted on the *argument*: the fixture uses `Cell/fill(c, true) : Io(Bool)`, which `f : (Bool) -> Bool` does not take, so the derivation is refused at the first occurrence and never reaches a refinement at all.
#[test]
fn an_effect_behind_a_stuck_head_is_not_an_argument() {
    rejected_by(AN_EFFECT_BEHIND_A_STUCK_HEAD_IS_NOT_AN_ARGUMENT, "Io");
}

#[test]
fn a_stuck_application_scrutinee_still_refines() {
    assert_eq!(run(A_STUCK_APPLICATION_SCRUTINEE_STILL_REFINES), b"t");
}

/// Asserted on the offending *argument*: the refusal is that `(b) => Cell/fill(c, true)` cannot be passed where a `(Bool) -> Bool` is wanted, so the description type has to appear in the diagnostic. A fixture refused anywhere else — at the cell, at `Eq/refl`, at an arm — would not produce that, and this file's rule is that a board test asserts its own diagnostic.
#[test]
fn an_effect_cannot_inhabit_a_pure_arrow() {
    rejected_by(AN_EFFECT_CANNOT_INHABIT_A_PURE_ARROW, "Io");
}

#[test]
fn a_parameter_headed_scrutinee_refines_again() {
    assert_eq!(run(A_PARAMETER_HEADED_SCRUTINEE_REFINES_AGAIN), b"t");
}
