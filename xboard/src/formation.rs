mod a_groups_instance_is_held_to_the_groups_constraints_and_its_bodies_were_checked_under_the_enclosing_declarations;
mod a_parameter_named_through_an_instance_is_an_occurrence_positivity_does_not_see;

use crate::Part;

/// Is this a type, and at which sort? For every former — Π, Σ, a declared family or record — with its levels, subsumption, `Prop` and positivity, and for what a declaration, an intrinsic or a `foreign` row may state.
pub(super) const PART: Part = Part {
    name: "formation",
    tickets: &[
        a_parameter_named_through_an_instance_is_an_occurrence_positivity_does_not_see::TICKET,
        a_groups_instance_is_held_to_the_groups_constraints_and_its_bodies_were_checked_under_the_enclosing_declarations::TICKET,
    ],
};
