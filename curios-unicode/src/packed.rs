//! The tables as `/sys` carries them: each a run of seven bytes a range, and each class set the codes its classes are read by.

use crate::{
    EAST_ASIAN_WIDTH, EAST_ASIAN_WIDTH_MISSING, EMOJI_PRESENTATION, EXTENDED_PICTOGRAPHIC,
    EastAsianWidth, GRAPHEME_BREAK, GRAPHEME_BREAK_MISSING, GraphemeBreak, INDIC_CONJUNCT_BREAK,
    INDIC_CONJUNCT_BREAK_MISSING, IndicConjunctBreak,
};

/// How many bytes a run takes packed: its first scalar and its last, three bytes each, least significant first, then its class's code.
pub const PACKED_RUN: usize = 7;

/// One property's table packed for `/sys/unicode`.
pub struct PackedTable {
    /// The name `/sys/unicode` gives the table.
    pub name: &'static str,
    /// Each run of scalars of one class but the class a scalar no run names, in the order of their scalars, as [`PACKED_RUN`] bytes. A binary property's runs are those of the scalars that have it, each of code `1`.
    pub bytes: Vec<u8>,
}

/// One property's classes as `/sys` names their codes.
pub struct ClassCodes {
    /// The `/sys` module that names them, which is the name of the property's table too.
    pub property: &'static str,
    /// Each class's name and code, in the order of the codes.
    pub codes: Vec<(&'static str, u8)>,
    /// The code of the class a scalar no run names.
    pub missing: u8,
}

fn pack(runs: impl Iterator<Item = (u32, u32, u8)>) -> Vec<u8> {
    runs.flat_map(|(first, last, code)| {
        let [first_0, first_1, first_2, _] = first.to_le_bytes();
        let [last_0, last_1, last_2, _] = last.to_le_bytes();
        [first_0, first_1, first_2, last_0, last_1, last_2, code]
    })
    .collect()
}

fn classes<C: Copy>(table: &[(u32, u32, C)], code: impl Fn(C) -> u8) -> Vec<u8> {
    pack(
        table
            .iter()
            .map(|&(first, last, class)| (first, last, code(class))),
    )
}

fn binary(table: &[(u32, u32)]) -> Vec<u8> {
    pack(table.iter().map(|&(first, last)| (first, last, 1)))
}

/// Every table, packed for `/sys/unicode`.
pub fn packed_tables() -> Vec<PackedTable> {
    vec![
        PackedTable {
            name: "grapheme_break",
            bytes: classes(GRAPHEME_BREAK, GraphemeBreak::code),
        },
        PackedTable {
            name: "indic_conjunct_break",
            bytes: classes(INDIC_CONJUNCT_BREAK, IndicConjunctBreak::code),
        },
        PackedTable {
            name: "east_asian_width",
            bytes: classes(EAST_ASIAN_WIDTH, EastAsianWidth::code),
        },
        PackedTable {
            name: "extended_pictographic",
            bytes: binary(EXTENDED_PICTOGRAPHIC),
        },
        PackedTable {
            name: "emoji_presentation",
            bytes: binary(EMOJI_PRESENTATION),
        },
    ]
}

fn codes<C: Copy>(
    all: &[C],
    sys_name: impl Fn(C) -> &'static str,
    code: impl Fn(C) -> u8,
) -> Vec<(&'static str, u8)> {
    all.iter()
        .map(|&class| (sys_name(class), code(class)))
        .collect()
}

/// Every class set, as `/sys` names its codes.
pub fn class_codes() -> Vec<ClassCodes> {
    vec![
        ClassCodes {
            property: "grapheme_break",
            codes: codes(
                GraphemeBreak::ALL,
                GraphemeBreak::sys_name,
                GraphemeBreak::code,
            ),
            missing: GRAPHEME_BREAK_MISSING.code(),
        },
        ClassCodes {
            property: "indic_conjunct_break",
            codes: codes(
                IndicConjunctBreak::ALL,
                IndicConjunctBreak::sys_name,
                IndicConjunctBreak::code,
            ),
            missing: INDIC_CONJUNCT_BREAK_MISSING.code(),
        },
        ClassCodes {
            property: "east_asian_width",
            codes: codes(
                EastAsianWidth::ALL,
                EastAsianWidth::sys_name,
                EastAsianWidth::code,
            ),
            missing: EAST_ASIAN_WIDTH_MISSING.code(),
        },
    ]
}
