use xboard::Part;

/// Does this recursion end, and is what is erased total? The erasure obligations and the descent they rest on.
pub(super) const TOTALITY: Part = Part {
    name: "totality",
    tickets: &[],
};
