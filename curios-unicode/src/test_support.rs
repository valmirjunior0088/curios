//! What another crate's tests read of the database: the conformance cases a segmentation of text is held to, and what each mapping maps a scalar to, as the database's rows state it rather than as the records `/sys` carries.

/// Every case of `GraphemeBreakTest.txt`: a run of scalars as the clusters it segments into, each cluster its scalars.
pub fn grapheme_break_cases() -> &'static [&'static [&'static [u32]]] {
    crate::GRAPHEME_BREAK_CASES
}

/// The scalars `scalar` becomes in upper case, its full `Uppercase_Mapping`: `scalar` itself where it has none.
pub fn uppercase(scalar: u32) -> Vec<u32> {
    mapped(crate::UPPER_ROWS, scalar)
}

/// What the rows of a mapping map `scalar` to, or `scalar` itself where no row names it.
fn mapped(rows: &[(u32, &[u32])], scalar: u32) -> Vec<u32> {
    rows.binary_search_by_key(&scalar, |&(from, _)| from)
        .map_or_else(|_| vec![scalar], |row| rows[row].1.to_vec())
}
