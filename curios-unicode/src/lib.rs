//! The Unicode Character Database the toolchain pins, read once.
//!
//! The build script downloads the files of one version of the database, checks each against its SHA-256 and writes the runs of scalars each property gives a class: [`grapheme_break`], [`indic_conjunct_break`], [`east_asian_width`], [`is_extended_pictographic`] and [`is_emoji_presentation`] answer from them, and [`graphemes()`] segments text by them into extended grapheme clusters (UAX #29). The same tables are handed on packed, as [`packed_tables`] and [`class_codes`] spell them, for `curios-text` to bake into `/sys`, where `/std` answers the same properties in Curios, so what the compiler reads of a character and what a program reads of it are one table.
//!
//! The pieces are neutral: a property is the database's and segmentation is UAX #29's, and a choice made over them — how many columns a cluster takes on a terminal — is its consumer's.

mod property;
pub use property::*;

mod graphemes;
pub use graphemes::*;

mod packed;
pub use packed::*;

#[cfg(feature = "test-support")]
pub mod test_support;

#[cfg(test)]
mod tests;

include!(concat!(env!("OUT_DIR"), "/tables.rs"));
