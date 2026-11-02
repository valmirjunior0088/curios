mod a_member_reached_through_an_instance_is_a_call_the_kernel_does_not_record;
mod a_payload_that_quantifies_over_prop_is_applied_at_its_own_family_and_the_application_is_graded_below_its_constructor;
mod a_proof_index_refined_by_irrelevance_is_graded_as_the_constructor_it_was_unified_with;
mod a_proofs_constructor_inside_an_arms_solution_is_graded_as_structure;

use crate::Part;

/// Does this recursion end, and is what is erased total? The erasure obligations and the descent they rest on.
///
/// One ticket of this part was retired rather than fixed: `a_group_a_universe_instance_names_is_no_group_to_the_classification_of_a_unit_mounted_without_a_covering_record` spelled a member at a universe instance, and a group binds no universe, so no such spelling exists to classify ([`curios_core::InstanceHead`]). What it showed of `Totals::seeded` — that a classification reads a group whichever way one is spelled — stands in `curios-cert`'s `recheck::totality_tests`.
pub(super) const PART: Part = Part {
    name: "totality",
    tickets: &[
        a_member_reached_through_an_instance_is_a_call_the_kernel_does_not_record::TICKET, a_payload_that_quantifies_over_prop_is_applied_at_its_own_family_and_the_application_is_graded_below_its_constructor::TICKET, a_proof_index_refined_by_irrelevance_is_graded_as_the_constructor_it_was_unified_with::TICKET, a_proofs_constructor_inside_an_arms_solution_is_graded_as_structure::TICKET],
};
