//! What another crate's tests read of the database: the conformance cases a segmentation of text is held to, what each mapping maps a scalar to, as the database's rows state it rather than as the records `/sys` carries, and the properties no compiler code reads.

use crate::{
    CASE_IGNORABLE, CASED, FOLD_ROWS, LOWER_ROWS, LOWERCASE, UPPER_ROWS, UPPERCASE, property::held,
};

/// Every case of `GraphemeBreakTest.txt`: a run of scalars as the clusters it segments into, each cluster its scalars.
pub fn grapheme_break_cases() -> &'static [&'static [&'static [u32]]] {
    crate::GRAPHEME_BREAK_CASES
}

/// The scalars `scalar` becomes in upper case, its full `Uppercase_Mapping`: `scalar` itself where it has none.
pub fn uppercase(scalar: u32) -> Vec<u32> {
    mapped(UPPER_ROWS, scalar)
}

/// The scalars `scalar` becomes in lower case, its full `Lowercase_Mapping`, which reads no context: `scalar` itself where it has none.
pub fn lowercase(scalar: u32) -> Vec<u32> {
    mapped(LOWER_ROWS, scalar)
}

/// The scalars `scalar` folds to, its full `Case_Folding`, common and full rows alike: `scalar` itself where it has none.
pub fn casefold(scalar: u32) -> Vec<u32> {
    mapped(FOLD_ROWS, scalar)
}

/// Whether `scalar` is `Cased`.
pub fn is_cased(scalar: char) -> bool {
    held(CASED, scalar)
}

/// Whether `scalar` is `Lowercase`.
pub fn is_lowercase(scalar: char) -> bool {
    held(LOWERCASE, scalar)
}

/// Whether `scalar` is `Uppercase`.
pub fn is_uppercase(scalar: char) -> bool {
    held(UPPERCASE, scalar)
}

/// Whether `scalar` is `Case_Ignorable`.
pub fn is_case_ignorable(scalar: char) -> bool {
    held(CASE_IGNORABLE, scalar)
}

/// What the rows of a mapping map `scalar` to, or `scalar` itself where no row names it.
fn mapped(rows: &[(u32, &[u32])], scalar: u32) -> Vec<u32> {
    rows.binary_search_by_key(&scalar, |&(from, _)| from)
        .map_or_else(|_| vec![scalar], |row| rows[row].1.to_vec())
}
