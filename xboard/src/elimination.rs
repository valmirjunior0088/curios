mod a_guards_reduced_spelling_is_settled_under_one_case_of_a_split_inside_its_arm_and_answers_outside_that_case;
mod an_arm_of_a_fold_is_typed_with_its_scrutinee_at_the_arms_case_and_is_taken_at_every_tail;

use crate::Part;

/// May this value be taken apart this way, and what does an arm learn? A match, its motive, coverage, inversion and case equations, where a proposition may be eliminated, and that `Io` cannot be.
pub(super) const PART: Part = Part {
    name: "elimination",
    tickets: &[an_arm_of_a_fold_is_typed_with_its_scrutinee_at_the_arms_case_and_is_taken_at_every_tail::TICKET, a_guards_reduced_spelling_is_settled_under_one_case_of_a_split_inside_its_arm_and_answers_outside_that_case::TICKET],
};
