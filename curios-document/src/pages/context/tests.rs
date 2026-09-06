//! The pieces a page is cut into: keywords out of a signature, and the kind tally a module card shows.

use {
    super::*,
    crate::{Declaration, Kind, Signature},
};

/// The text the segments spell, links and keywords included.
fn plain(segments: &[Segment]) -> String {
    segments
        .iter()
        .map(|segment| match segment {
            Segment::Text(text) | Segment::Keyword(text) | Segment::Name(text) => text.as_str(),
            Segment::Link { text, .. } => text.as_str(),
        })
        .collect()
}

#[test]
fn a_signature_sets_its_keywords_apart_and_keeps_every_other_byte() {
    let text = "pub let get(@A: Type, m: Option(A), use Ordered(A), @ok: IsSome(m)) -> A";
    let mut segments = Vec::new();
    words(text, &mut segments);

    let keywords = segments
        .iter()
        .filter_map(|segment| match segment {
            Segment::Keyword(word) => Some(word.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(keywords, ["pub", "let", "use"]);
    assert_eq!(plain(&segments), text, "the segments spell the text");
    assert!(
        matches!(
            &segments[..3],
            [Segment::Keyword(_), Segment::Text(_), Segment::Keyword(_)]
        ),
        "a keyword is its own segment and the text between keywords is one"
    );
}

#[test]
fn a_module_card_tallies_kinds_in_keyword_order() {
    let declaration = |kind| Declaration {
        name: String::new(),
        home: curios_utilities::Qualifier::empty(),
        kind,
        signature: Signature {
            text: String::new(),
            marks: Vec::new(),
        },
        prose: None,
        members: Vec::new(),
        opaque: false,
        derived: false,
    };
    let declarations = [
        declaration(Kind::Witness),
        declaration(Kind::Definition),
        declaration(Kind::Inductive),
        declaration(Kind::Definition),
    ];

    assert_eq!(counts(&declarations), "2 let · 1 induct · 1 satisfy");
    assert_eq!(counts(&[]), "");
}
