use crate::*;

/// The scalars of a conformance case as the text they spell.
fn spelled(scalars: &[u32]) -> String {
    scalars
        .iter()
        .map(|&scalar| char::from_u32(scalar).expect("a conformance case holds scalars"))
        .collect()
}

#[test]
fn every_conformance_case_segments_into_the_clusters_it_states() {
    let mut failed = Vec::new();

    for case in GRAPHEME_BREAK_CASES {
        let text = spelled(&case.concat());
        let found: Vec<String> = graphemes(&text).map(str::to_string).collect();
        let stated: Vec<String> = case.iter().map(|cluster| spelled(cluster)).collect();
        if found != stated {
            failed.push(format!("{text:?}: found {found:?}, stated {stated:?}"));
        }
    }

    assert!(
        failed.is_empty(),
        "{} of {} cases segment otherwise:\n{}",
        failed.len(),
        GRAPHEME_BREAK_CASES.len(),
        failed.join("\n")
    );
}

/// Whether `runs` ascend and never meet, which is what a search by halves over them assumes.
fn ascend_apart(runs: impl Iterator<Item = (u32, u32)>) -> bool {
    let runs: Vec<(u32, u32)> = runs.collect();

    runs.iter().all(|(first, last)| first <= last)
        && runs.windows(2).all(|pair| pair[0].1 < pair[1].0)
}

#[test]
fn every_table_s_runs_ascend_apart() {
    assert!(ascend_apart(
        GRAPHEME_BREAK.iter().map(|&(first, last, _)| (first, last))
    ));
    assert!(ascend_apart(
        INDIC_CONJUNCT_BREAK
            .iter()
            .map(|&(first, last, _)| (first, last))
    ));
    assert!(ascend_apart(
        EAST_ASIAN_WIDTH
            .iter()
            .map(|&(first, last, _)| (first, last))
    ));
    assert!(ascend_apart(EXTENDED_PICTOGRAPHIC.iter().copied()));
    assert!(ascend_apart(EMOJI_PRESENTATION.iter().copied()));
    assert!(ascend_apart(DEFAULT_IGNORABLE.iter().copied()));
    assert!(ascend_apart(ALPHABETIC.iter().copied()));
    assert!(ascend_apart(NUMERIC.iter().copied()));
    assert!(ascend_apart(CONTROL.iter().copied()));
    assert!(ascend_apart(WHITE_SPACE.iter().copied()));
    assert!(ascend_apart(CASED.iter().copied()));
    assert!(ascend_apart(CASE_IGNORABLE.iter().copied()));
    assert!(ascend_apart(LOWERCASE.iter().copied()));
    assert!(ascend_apart(UPPERCASE.iter().copied()));
}

/// What the database says of a few scalars, one from each kind of answer.
#[test]
fn a_scalar_s_properties_are_the_database_s() {
    assert_eq!(grapheme_break('a'), GraphemeBreak::Other);
    assert_eq!(grapheme_break('\r'), GraphemeBreak::Cr);
    assert_eq!(grapheme_break('\u{301}'), GraphemeBreak::Extend);
    assert_eq!(grapheme_break('\u{200D}'), GraphemeBreak::Zwj);
    assert_eq!(
        grapheme_break('\u{1F1E6}'),
        GraphemeBreak::RegionalIndicator
    );
    assert_eq!(grapheme_break('\u{AC00}'), GraphemeBreak::Lv);
    assert_eq!(indic_conjunct_break('\u{94D}'), IndicConjunctBreak::Linker);
    assert_eq!(
        indic_conjunct_break('\u{915}'),
        IndicConjunctBreak::Consonant
    );
    assert_eq!(east_asian_width('a'), EastAsianWidth::Na);
    assert_eq!(east_asian_width('\u{65E5}'), EastAsianWidth::W);
    assert_eq!(east_asian_width('\u{1F680}'), EastAsianWidth::W);
    assert_eq!(east_asian_width('\u{231B}'), EastAsianWidth::W);
    assert_eq!(east_asian_width('\u{20AC}'), EastAsianWidth::A);
    assert!(is_emoji_presentation('\u{1F680}'));
    assert!(!is_emoji_presentation('\u{2600}'));
    assert!(is_extended_pictographic('\u{2600}'));
    assert!(is_default_ignorable('\u{200D}'));
    assert!(is_default_ignorable('\u{3164}'));
    assert!(is_default_ignorable('\u{E0041}'));
    assert!(!is_default_ignorable('a'));
    assert!(!is_extended_pictographic('a'));
    assert!(is_alphabetic('a'));
    assert!(is_alphabetic('\u{558}'));
    assert!(!is_alphabetic('1'));
    assert!(is_numeric('1'));
    assert!(is_numeric('\u{2163}'));
    assert!(is_numeric('\u{BD}'));
    assert!(!is_numeric('a'));
    assert!(is_control('\u{7F}'));
    assert!(!is_control(' '));
    assert!(is_white_space('\u{A0}'));
    assert!(!is_white_space('\u{200B}'));
}

/// A packed table is its runs, seven bytes each, read back as they were written.
#[test]
fn a_packed_table_reads_back_as_its_runs() {
    let read = |bytes: &[u8]| -> Vec<(u32, u32, u8)> {
        bytes
            .chunks(PACKED_RUN)
            .map(|run| {
                (
                    u32::from_le_bytes([run[0], run[1], run[2], 0]),
                    u32::from_le_bytes([run[3], run[4], run[5], 0]),
                    run[6],
                )
            })
            .collect()
    };
    let tables = packed_tables();
    let grapheme = &tables[0];

    assert!(
        tables
            .iter()
            .all(|table| table.bytes.len() % PACKED_RUN == 0)
    );
    assert_eq!(grapheme.name, "grapheme_break");
    assert_eq!(
        read(&grapheme.bytes),
        GRAPHEME_BREAK
            .iter()
            .map(|&(first, last, class)| (first, last, class.code()))
            .collect::<Vec<_>>()
    );
}

/// What a mapping's records map `scalar` to, read as `/std` reads them: the record covering it, where `scalar` is one of the scalars it steps through, makes it the record's targets with the first moved as far as `scalar` is from the record's first; any other scalar stays itself.
fn by_records(records: &[(u32, u32, u8, &[u32])], scalar: u32) -> Vec<u32> {
    let covering = records
        .binary_search_by(
            |&(first, last, _, _)| match (scalar < first, last < scalar) {
                (true, _) => std::cmp::Ordering::Greater,
                (_, true) => std::cmp::Ordering::Less,
                _ => std::cmp::Ordering::Equal,
            },
        )
        .ok()
        .map(|record| records[record]);

    match covering {
        Some((first, _, step, targets)) if (scalar - first).is_multiple_of(u32::from(step)) => {
            let mut mapped = targets.to_vec();
            mapped[0] += scalar - first;
            mapped
        }
        _ => vec![scalar],
    }
}

/// What a mapping's rows map `scalar` to, as the database states it.
fn by_rows(rows: &[(u32, &[u32])], scalar: u32) -> Vec<u32> {
    rows.binary_search_by_key(&scalar, |&(from, _)| from)
        .map_or_else(|_| vec![scalar], |row| rows[row].1.to_vec())
}

/// A mapping's records ascend apart, step by one or two, become one to three scalars, and map every scalar as the database's rows do, which is what a reading of them in `/std` is held to.
#[test]
fn a_mapping_s_records_map_every_scalar_as_its_rows_do() {
    for (name, records, rows) in [
        ("upper", UPPER, UPPER_ROWS),
        ("lower", LOWER, LOWER_ROWS),
        ("fold", FOLD, FOLD_ROWS),
    ] {
        assert!(
            ascend_apart(records.iter().map(|&(first, last, _, _)| (first, last))),
            "{name}"
        );
        assert!(
            records.iter().all(|&(_, _, step, targets)| {
                (1..=2).contains(&step) && (1..=3).contains(&targets.len())
            }),
            "{name}"
        );

        let wrong: Vec<String> = (0..=0x10_FFFF)
            .filter(|&scalar| by_records(records, scalar) != by_rows(rows, scalar))
            .map(|scalar| format!("U+{scalar:04X}"))
            .collect();
        assert!(
            wrong.is_empty(),
            "{name}'s records map otherwise: {wrong:?}"
        );
    }
}

/// A packed mapping is its records, seventeen bytes each, read back as they were written, a target the record does not have read as U+0000.
#[test]
fn a_packed_mapping_reads_back_as_its_records() {
    let mappings = packed_mappings();

    for (mapping, (name, records)) in
        mappings
            .iter()
            .zip([("upper", UPPER), ("lower", LOWER), ("fold", FOLD)])
    {
        assert_eq!(mapping.name, name);
        assert_eq!(mapping.bytes.len(), PACKED_RECORD * records.len());
        check_records(&mapping.bytes, records);
    }
    assert_eq!(mappings.len(), 3);
}

/// Each record of `packed` reads back as the record of `records` beside it, a target the record does not have read as U+0000.
fn check_records(packed: &[u8], records: &[(u32, u32, u8, &[u32])]) {
    let three = |bytes: &[u8]| u32::from_le_bytes([bytes[0], bytes[1], bytes[2], 0]);

    for (packed, &(first, last, step, targets)) in packed.chunks(PACKED_RECORD).zip(records) {
        let mut padded = targets.to_vec();
        padded.resize(3, 0);
        assert_eq!(
            (
                three(&packed[0..]),
                three(&packed[3..]),
                packed[6],
                usize::from(packed[7])
            ),
            (first, last, step, targets.len())
        );
        assert_eq!(
            [
                three(&packed[8..]),
                three(&packed[11..]),
                three(&packed[14..])
            ],
            [padded[0], padded[1], padded[2]]
        );
    }
}
