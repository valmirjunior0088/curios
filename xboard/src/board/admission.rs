use xboard::Part;

/// What is believed without being judged again? A unit already in scope, a cached verdict, a reused payload, a module taken as finished, an exhausted budget.
pub(super) const ADMISSION: Part = Part {
    name: "admission",
    tickets: &[],
};
