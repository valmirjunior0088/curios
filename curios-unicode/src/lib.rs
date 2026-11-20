//! The Unicode Character Database the toolchain pins, read once.
//!
//! The build script downloads the files of one version of the database, checks each against its SHA-256 and writes the runs of scalars each property gives a class: a lookup named for each property answers from them, and [`graphemes()`] segments text by them into extended grapheme clusters (UAX #29). The same tables are handed on packed, as [`packed_tables`], [`packed_mappings`] and [`class_codes`] spell them, for `curios-module` to bake into `/sys`, where `/std` answers the same properties and maps case in Curios, so what the compiler reads of a character and what a program reads of it are one table.
//!
//! The pieces are neutral — a property is the database's and segmentation is UAX #29's — as `README.md` decides, and a choice made over them is its consumer's.

mod property;
pub use property::{
    EastAsianWidth, GraphemeBreak, IndicConjunctBreak, east_asian_width, grapheme_break,
    indic_conjunct_break, is_alphabetic, is_control, is_default_ignorable, is_emoji_presentation,
    is_extended_pictographic, is_numeric, is_white_space,
};

mod graphemes;
pub use graphemes::{Graphemes, graphemes};

mod packed;
pub use packed::{
    ClassCodes, PACKED_RECORD, PACKED_RUN, PackedTable, class_codes, packed_mappings, packed_tables,
};

#[cfg(feature = "test-support")]
pub mod test_support;

#[cfg(test)]
mod tests;

include!(concat!(env!("OUT_DIR"), "/tables.rs"));
