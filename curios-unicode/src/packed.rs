//! The tables as `/sys` carries them: a property's a run of seven bytes a range, a mapping's a record of seventeen, a decomposition's an entry of seven naming a run of a table of scalars, three bytes each, a composite nine, and each class set the codes its classes are read by.

use crate::{
    ALPHABETIC, CANONICAL_DECOMPOSITION, CASE_IGNORABLE, CASED, COMBINING_CLASS,
    COMPATIBILITY_DECOMPOSITION, COMPOSITION, EAST_ASIAN_WIDTH, EAST_ASIAN_WIDTH_MISSING,
    EMOJI_PRESENTATION, EXTENDED_PICTOGRAPHIC, EastAsianWidth, FOLD, GRAPHEME_BREAK,
    GRAPHEME_BREAK_MISSING, GraphemeBreak, INDIC_CONJUNCT_BREAK, INDIC_CONJUNCT_BREAK_MISSING,
    IndicConjunctBreak, LOWER, LOWERCASE, UPPER, UPPERCASE, WHITE_SPACE,
};

/// How many bytes a run takes packed: its first scalar and its last, three bytes each, least significant first, then its class's code.
pub const PACKED_RUN: usize = 7;

/// How many bytes a mapping's record takes packed: its first scalar and its last, three bytes each, least significant first, the step between the scalars it covers, how many scalars each becomes, then three targets of three bytes each, a target the record does not have written as U+0000 so that every target is a scalar value.
pub const PACKED_RECORD: usize = 17;

/// How many bytes an entry of a decomposition takes packed: its scalar in three bytes, least significant first, the place the scalars it becomes start among the decomposition's scalars in three, then how many there are.
pub const PACKED_ENTRY: usize = 7;

/// How many bytes a composite takes packed: the first scalar of the pair it composes from in three bytes, least significant first, the second in three, then itself in three.
pub const PACKED_COMPOSITE: usize = 9;

/// One table packed for `/sys/unicode`.
pub struct PackedTable {
    /// The name `/sys/unicode` gives the table.
    pub name: &'static str,
    /// A property's runs of scalars of one class but the class a scalar no run names, in the order of their scalars, as [`PACKED_RUN`] bytes, a binary property's runs those of the scalars that have it, each of code `1`; or a mapping's records, in the order of their scalars, as [`PACKED_RECORD`] bytes.
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

/// A scalar, or a place among a table's scalars, in the three bytes a table spells it in, least significant first.
fn three(number: u32) -> [u8; 3] {
    let [low, middle, high, _] = number.to_le_bytes();
    [low, middle, high]
}

fn records(table: &[(u32, u32, u8, &[u32])]) -> Vec<u8> {
    table
        .iter()
        .flat_map(|&(first, last, step, targets)| {
            let count = u8::try_from(targets.len()).expect("a scalar becomes at most three");
            let slots = (0..3).flat_map(|slot| three(targets.get(slot).copied().unwrap_or(0)));
            three(first)
                .into_iter()
                .chain(three(last))
                .chain([step, count])
                .chain(slots)
        })
        .collect()
}

/// Every property's table, packed for `/sys/unicode`.
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
        PackedTable {
            name: "cased",
            bytes: binary(CASED),
        },
        PackedTable {
            name: "case_ignorable",
            bytes: binary(CASE_IGNORABLE),
        },
        PackedTable {
            name: "lowercase",
            bytes: binary(LOWERCASE),
        },
        PackedTable {
            name: "uppercase",
            bytes: binary(UPPERCASE),
        },
        PackedTable {
            name: "alphabetic",
            bytes: binary(ALPHABETIC),
        },
        PackedTable {
            name: "white_space",
            bytes: binary(WHITE_SPACE),
        },
    ]
}

/// A decomposition as its two tables: its entries, in the order of their scalars, each naming the place its scalars start among the second table's and how many there are, and those scalars, three bytes each.
fn sequences(table: &[(u32, &[u32])]) -> (Vec<u8>, Vec<u8>) {
    let mut entries = Vec::new();
    let mut scalars = Vec::new();
    for &(scalar, targets) in table {
        let start =
            u32::try_from(scalars.len() / 3).expect("a decomposition's scalars fit three bytes");
        let count = u8::try_from(targets.len()).expect("a scalar decomposes to at most 255");
        entries.extend(three(scalar).into_iter().chain(three(start)).chain([count]));
        scalars.extend(targets.iter().flat_map(|&target| three(target)));
    }
    (entries, scalars)
}

/// Every table normalization reads, packed for `/sys/unicode`: the canonical combining classes as a property's runs, each decomposition as its entries and the scalars they name, and the primary composites in the order of their pairs.
pub fn packed_normalization() -> Vec<PackedTable> {
    let (canonical, canonical_scalars) = sequences(CANONICAL_DECOMPOSITION);
    let (compatibility, compatibility_scalars) = sequences(COMPATIBILITY_DECOMPOSITION);
    vec![
        PackedTable {
            name: "combining_class",
            bytes: pack(COMBINING_CLASS.iter().copied()),
        },
        PackedTable {
            name: "canonical_decomposition",
            bytes: canonical,
        },
        PackedTable {
            name: "canonical_decomposition_scalars",
            bytes: canonical_scalars,
        },
        PackedTable {
            name: "compatibility_decomposition",
            bytes: compatibility,
        },
        PackedTable {
            name: "compatibility_decomposition_scalars",
            bytes: compatibility_scalars,
        },
        PackedTable {
            name: "composition",
            bytes: COMPOSITION
                .iter()
                .flat_map(|&(first, second, composite)| {
                    three(first)
                        .into_iter()
                        .chain(three(second))
                        .chain(three(composite))
                })
                .collect(),
        },
    ]
}

/// Every mapping, packed for `/sys/unicode`.
pub fn packed_mappings() -> Vec<PackedTable> {
    vec![
        PackedTable {
            name: "upper",
            bytes: records(UPPER),
        },
        PackedTable {
            name: "lower",
            bytes: records(LOWER),
        },
        PackedTable {
            name: "fold",
            bytes: records(FOLD),
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
