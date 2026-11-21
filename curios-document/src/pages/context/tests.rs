//! The pieces a page is cut into: a signature at its marks, and the kind tally a module card shows.

use {
    super::*,
    crate::*,
    curios_common::{Class, Qualifier},
};

/// A declaration of `kind` named `name` in the module `home`, with nothing else stated.
fn declaration(home: &Qualifier, name: &str, kind: Kind) -> Declaration {
    Declaration {
        name: name.to_string(),
        home: *home,
        kind,
        signature: Signature {
            text: String::new(),
            marks: Vec::new(),
        },
        prose: None,
        members: Vec::new(),
        opaque: false,
        derived: false,
        proof: false,
        total: None,
        parties: Vec::new(),
        named: home.with(name),
        chip: None,
    }
}

/// The text the segments spell, every marked span included.
fn plain(segments: &[Segment]) -> String {
    segments
        .iter()
        .map(|segment| match segment {
            Segment::Text(text) | Segment::Marked { text, .. } => text.as_str(),
        })
        .collect()
}

/// A signature is cut at the marks its printer left, each a segment painted by its class's name and linked where it names a declaration of the bundle; what no mark spans — a binder named `concept` among it — stays text.
#[test]
fn a_signature_is_cut_at_its_marks_each_painted_by_its_class() {
    let module = Qualifier::from(["m"]);
    let record = Documentation {
        prefix: module,
        description: None,
        modules: vec![ModuleDocumentation {
            path: module,
            prose: None,
            children: Vec::new(),
            entries: vec![crate::Entry::Declaration(declaration(
                &module,
                "Colour",
                Kind::Inductive,
            ))],
        }],
    };
    let bundle = Bundle::new(&record);
    let text = "let pick(concept: Colour) -> Nat";
    let mark = |start, end, marked| Mark { start, end, marked };
    let signature = Signature {
        text: text.to_string(),
        marks: vec![
            mark(0, 3, Marked::Word(Class::Keyword)),
            mark(
                18,
                24,
                Marked::Name {
                    class: Some(Class::Type),
                    referent: module.with("Colour"),
                    within: true,
                },
            ),
            mark(
                29,
                32,
                Marked::Name {
                    class: Some(Class::Type),
                    referent: Qualifier::from(["std", "Nat"]),
                    within: false,
                },
            ),
        ],
    };

    let segments = segments(&bundle, 0, &signature);
    let marked = segments
        .iter()
        .filter_map(|segment| match segment {
            Segment::Marked { class, text, href } => Some((*class, text.as_str(), href.is_some())),
            Segment::Text(_) => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(plain(&segments), text, "the segments spell the text");
    assert_eq!(
        marked,
        [
            ("keyword", "let", false),
            ("type", "Colour", true),
            ("type", "Nat", false)
        ]
    );
}

#[test]
fn a_module_card_tallies_kinds_in_their_order() {
    let home = Qualifier::empty();
    let declarations = [
        declaration(&home, "", Kind::Witness),
        declaration(&home, "", Kind::Definition),
        declaration(&home, "", Kind::Inductive),
        declaration(&home, "", Kind::Definition),
    ];

    // A count and its word are joined by a no-break space, so the two never part at a line's end.
    assert_eq!(
        counts(declarations.iter()),
        "2\u{a0}let · 1\u{a0}induct · 1\u{a0}satisfy"
    );
    assert_eq!(counts([].iter()), "");
}
