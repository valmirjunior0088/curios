mod a_name_declared_twice_means_two_things_to_the_kernel;

use crate::Part;

/// What is believed without being judged again? A unit already in scope, a cached verdict, a reused payload, a module taken as finished, an exhausted budget.
pub(super) const PART: Part = Part {
    name: "admission",
    tickets: &[a_name_declared_twice_means_two_things_to_the_kernel::TICKET],
};
