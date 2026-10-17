//! What another crate's tests read of the database: the conformance cases a segmentation of text is held to.

/// Every case of `GraphemeBreakTest.txt`: a run of scalars as the clusters it segments into, each cluster its scalars.
pub fn grapheme_break_cases() -> &'static [&'static [&'static [u32]]] {
    crate::GRAPHEME_BREAK_CASES
}
