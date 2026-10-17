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
