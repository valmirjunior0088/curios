//! Where irrelevance takes over from comparison: a function into a proposition and a record's proof component, and the relevant parts still compared.

use {super::test_support::*, crate::tests::run};

// The composition the row named as unattacked — "eta at a function type whose codomain is a proposition, where the expansion's body lands at a `Prop`-sorted goal and irrelevance discharges it without comparing anything" — and at Π there is nothing to attack, because the shape cannot arise. `turn` tries irrelevance *before* it dispatches on the goal type's shape, and `func_sort` makes a Π into a proposition a proposition whatever it quantifies over, so the goal is discharged whole at the top and eta never opens a binder at all. Any two such functions are equal, which is this accepting rung.
//
// The relevant-codomain pair beside it is what says the discharge is the proposition's doing rather than conversion giving up on function types: the same two binders at `(Nat) -> Nat` are not identified.
#[test]
fn a_function_into_a_proposition_is_discharged_before_eta() {
    assert_eq!(
        run(A_FUNCTION_INTO_A_PROPOSITION_IS_DISCHARGED_BEFORE_ETA),
        b"1"
    );
}

#[test]
fn a_function_into_a_type_is_not_discharged_uncompared() {
    rejected_by(
        A_FUNCTION_INTO_A_TYPE_IS_NOT_DISCHARGED_UNCOMPARED,
        "type mismatch",
    );
}

// The same composition where it *is* reachable, which is Σ rather than Π. `tuple_sort` makes a record a proposition only when every component is one, so `{Nat, Eq()(0, 0)}` stays relevant, irrelevance does not preempt eta, and eta is what hands the second component to a `Prop`-sorted goal. Both rules fire in one equation here — eta at Π opens the binder, eta at Σ splits the record — and only the relevant component is ever compared.
//
// The refusal beside it pins that last clause: replace the relevant component with a literal and the equation dies although the proof component still matches, so the acceptance above is not eta declining to look.
#[test]
fn hands_a_records_proof_component_to_irrelevance() {
    assert_eq!(
        run(ETA_HANDS_A_RECORDS_PROOF_COMPONENT_TO_IRRELEVANCE),
        b"1"
    );
}

#[test]
fn still_compares_a_records_relevant_component() {
    rejected_by(
        ETA_STILL_COMPARES_A_RECORDS_RELEVANT_COMPONENT,
        "type mismatch",
    );
}

// **Where the forfeiture ends.** A struct literal's fields and a constructor's payload have a typed context the opaque head's arguments lack: the declaration's own telescope, which `struct_eta` already reads on the neutral side. Both checkers compare fields and payloads at the telescope; compared at `Type`, every proof-carrying value built from a different proof of the same fact would be refused, and `Str/concat` would not associate. The four equations below are those shapes: a field, a payload, a standard-library constructor, and the string law.
#[test]
fn a_proof_field_does_not_distinguish_two_literals() {
    assert_eq!(run(A_PROOF_FIELD_DOES_NOT_DISTINGUISH_TWO_LITERALS), b"1");
}
