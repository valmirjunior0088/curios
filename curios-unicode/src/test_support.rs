//! What tests read of the database: the conformance cases a segmentation and a normalization of text are held to, what each mapping maps a scalar to, as the database's rows state it rather than as the records `/sys` carries, and the properties no compiler code reads.
//!
//! **The conformance files are a test's alone.** Only a test reads `GraphemeBreakTest.txt` and `NormalizationTest.txt`, so no build reads them and no table holds their cases: they are kept with the database, which `cargo xtask` fetches whole at the digest `xtask/tasks.toml` pins, and read here where the build script says it is kept.

use {
    crate::{
        CASE_IGNORABLE, CASED, FOLD_ROWS, LOWER_ROWS, LOWERCASE, UPPER_ROWS, UPPERCASE,
        property::held,
    },
    std::{fs, path::Path},
};

/// The text of the conformance file at `path` under the version's `ucd/` directory, read where the database is kept.
fn conformance(path: &str) -> String {
    let kept = Path::new(env!("CURIOS_UNICODE_DATABASE")).join(path);
    let bytes = fs::read(&kept).unwrap_or_else(|error| {
        panic!(
            "the Unicode Character Database is not kept at {}: {error}",
            kept.display()
        )
    });

    String::from_utf8(bytes).unwrap_or_else(|error| panic!("{path} is not UTF-8: {error}"))
}

/// The scalars a field writes, each in hexadecimal.
fn scalars(field: &str) -> Vec<u32> {
    field
        .split_whitespace()
        .map(|hex| u32::from_str_radix(hex, 16).expect("a scalar in hexadecimal"))
        .collect()
}

/// Every case of `GraphemeBreakTest.txt`: a run of scalars as the clusters it segments into, each cluster its scalars.
pub fn grapheme_break_cases() -> Vec<Vec<Vec<u32>>> {
    conformance("auxiliary/GraphemeBreakTest.txt")
        .lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|content| !content.is_empty())
        .map(|content| {
            let mut clusters: Vec<Vec<u32>> = Vec::new();
            for token in content.split_whitespace() {
                match token {
                    "÷" => clusters.push(Vec::new()),
                    "×" => {}
                    hex => clusters
                        .last_mut()
                        .expect("a case opens with a boundary")
                        .extend(scalars(hex)),
                }
            }
            clusters.retain(|cluster| !cluster.is_empty());
            clusters
        })
        .collect()
}

/// Every case of `NormalizationTest.txt`: its five columns, each its scalars, the source and its NFC, NFD, NFKC and NFKD forms.
pub fn normalization_cases() -> Vec<[Vec<u32>; 5]> {
    conformance("NormalizationTest.txt")
        .lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|content| !content.is_empty() && !content.starts_with('@'))
        .map(|content| {
            let columns: Vec<Vec<u32>> = content.split(';').take(5).map(scalars).collect();
            columns.try_into().expect("a case has five columns")
        })
        .collect()
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
