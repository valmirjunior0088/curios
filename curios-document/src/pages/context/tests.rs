//! The pieces a page is cut into: a signature at its marks and in the lines its printer broke it into, and the facts a declaration's source does not show.

use {
    super::*,
    crate::*,
    curios_common::{Class, Qualifier},
};

/// A declaration of `kind` named `name` in the module `home`, with nothing else stated.
fn declaration(home: &Qualifier, name: &str, kind: Kind) -> Declaration {
    Declaration {
        name: name.to_string(),
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
        totality: None,
        parties: Vec::new(),
        named: home.with(name),
        chip: None,
    }
}

/// A record of one module, `/m`, which declares the inductive `Colour`.
fn record() -> Documentation {
    let module = Qualifier::from(["m"]);

    Documentation {
        prefix: module,
        description: None,
        modules: vec![ModuleDocumentation {
            path: module,
            prose: None,
            children: Vec::new(),
            entries: vec![Entry::Declaration(declaration(
                &module,
                "Colour",
                Kind::Inductive,
            ))],
        }],
    }
}

fn mark(start: usize, end: usize, marked: Marked) -> Mark {
    Mark { start, end, marked }
}

/// The text the segments spell, every span the page sets apart included.
fn spelled(segments: &[Segment]) -> String {
    segments
        .iter()
        .map(|segment| match segment {
            Segment::Text(text) | Segment::Marked { text, .. } | Segment::Declared(text) => {
                text.as_str()
            }
        })
        .collect()
}

/// A signature is cut at the marks its printer left, each a segment painted by its class's name and linked where it names a declaration of the bundle; what no mark spans — a binder named `concept` among it — stays text.
#[test]
fn a_signature_is_cut_at_its_marks_each_painted_by_its_class() {
    let record = record();
    let bundle = Bundle::new(&record);
    let text = "let pick(concept: Colour) -> Nat";
    let signature = Signature {
        text: text.to_string(),
        marks: vec![
            mark(0, 3, Marked::Word(Class::Keyword)),
            mark(
                18,
                24,
                Marked::Name {
                    class: Some(Class::Type),
                    referent: record.prefix.with("Colour"),
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
            Segment::Text(_) | Segment::Declared(_) => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(spelled(&segments), text, "the segments spell the text");
    assert_eq!(
        marked,
        [
            ("keyword", "let", false),
            ("type", "Colour", true),
            ("type", "Nat", false)
        ]
    );
}

/// A signature reaches a page in the lines its printer broke it into, each with the indent the printer gave it, so that a page folds a line too wide for it under that indent; the name it declares is set apart, a text ends at each space, where alone a line may fold, and what the block opens with joins the last line.
#[test]
fn a_signature_is_laid_out_in_the_lines_its_printer_broke_it_into() {
    let record = record();
    let bundle = Bundle::new(&record);
    let text = "pub let fold(\n    step: (B, A) -> B,\n    list: List(A),\n) -> B";
    let signature = Signature {
        text: text.to_string(),
        marks: vec![
            mark(0, 3, Marked::Word(Class::Keyword)),
            mark(4, 7, Marked::Word(Class::Keyword)),
        ],
    };

    let lines = lines(segments(&bundle, 0, &signature), Some("fold"));

    assert_eq!(
        lines
            .iter()
            .map(|line| (line.indent, spelled(&line.segments)))
            .collect::<Vec<_>>(),
        [
            (0, "pub let fold(".to_string()),
            (4, "step: (B, A) -> B,".to_string()),
            (4, "list: List(A),".to_string()),
            (0, ") -> B".to_string()),
        ]
    );
    let declared = |line: &Line| {
        line.segments
            .iter()
            .filter_map(|segment| match segment {
                Segment::Declared(name) => Some(name.clone()),
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(declared(&lines[0]), ["fold"]);
    // A line folds after a space and nowhere else: an arrow, a tuple's opening and a call stay whole, and no fold stands before the brace that closes the head.
    let texts = |line: &Line| {
        line.segments
            .iter()
            .filter_map(|segment| match segment {
                Segment::Text(text) => Some(text.clone()),
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(texts(&lines[1]), ["step: ", "(B, ", "A) ", "-> ", "B,"]);
    assert_eq!(texts(&lines[3]), [") ", "-> ", "B"]);
}

/// The name a signature declares is the first text after its keywords and nothing else: a parameter of the same name stays text, on the head's line or on one of its own, and so does a longer name that begins with it.
#[test]
fn only_the_name_a_signature_declares_is_set_apart() {
    let record = record();
    let bundle = Bundle::new(&record);
    let declared = |text: &str, name: &str| {
        let signature = Signature {
            text: text.to_string(),
            marks: vec![mark(0, 3, Marked::Word(Class::Keyword))],
        };

        lines(segments(&bundle, 0, &signature), Some(name))
            .iter()
            .flat_map(|line| &line.segments)
            .filter(|segment| matches!(segment, Segment::Declared(_)))
            .count()
    };

    assert_eq!(declared("let get(get: Nat) -> Nat", "get"), 1);
    assert_eq!(declared("let get_or(get: Nat) -> Nat", "get"), 0);
    assert_eq!(declared("let other(get: Nat) -> Nat", "get"), 0);
    assert_eq!(declared("let other(\n    get: Nat,\n) -> Nat", "get"), 0);
}

/// A declaration's facts are what its source does not show, in one order: how its representation stands, said both ways; what the compiler says the thing is; whether it is a proof; and whether it is proven total, said both ways. A fact that says something is public or proven takes the straight outline, a proof the squared one, and the rest the dashed one.
#[test]
fn a_declaration_states_what_its_source_does_not_show() {
    let home = Qualifier::from(["m"]);
    let stated = |declaration: &Declaration| {
        facts(declaration)
            .iter()
            .map(|fact| (fact.word.clone(), fact.drawing))
            .collect::<Vec<_>>()
    };
    let fact = |word: &str, drawing: &'static str| (word.to_string(), drawing);
    let of = |kind, opaque| Declaration {
        opaque,
        ..declaration(&home, "X", kind)
    };

    assert_eq!(
        stated(&of(Kind::Inductive, false)),
        [fact("transparent", "plain")]
    );
    assert_eq!(stated(&of(Kind::Structure, true)), [fact("opaque", "")]);
    assert_eq!(
        stated(&of(Kind::Concept, false)),
        [fact("unsealed", "plain")]
    );
    assert_eq!(stated(&of(Kind::Concept, true)), [fact("sealed", "")]);
    assert_eq!(stated(&of(Kind::Foreign, false)), []);
    assert_eq!(
        stated(&Declaration {
            proof: true,
            totality: Some(Totality::Total),
            ..of(Kind::Definition, false)
        }),
        [fact("proof", "proof"), fact("total", "plain")]
    );
    assert_eq!(
        stated(&Declaration {
            totality: Some(Totality::Partial),
            ..of(Kind::Definition, false)
        }),
        [fact("partial", "")]
    );
    assert_eq!(
        stated(&Declaration {
            totality: Some(Totality::Unfounded),
            ..of(Kind::Definition, false)
        }),
        [fact("unfounded", "")]
    );
    assert_eq!(
        stated(&Declaration {
            chip: Some("intrinsic".to_string()),
            ..of(Kind::Structure, true)
        }),
        [fact("opaque", ""), fact("intrinsic", "")]
    );
}

/// A member is anchored under its owner's name, a field as a projection writes it, and under the joint alone where its owner is what the page opens on; a witness by its head with its spaces dropped and the module that declares it, since two modules may write one head of two types.
#[test]
fn an_anchor_is_a_name_no_other_thing_of_the_page_takes() {
    let home = Qualifier::from(["m"]);
    let member = Member {
        name: "x".to_string(),
        start: 0,
        end: 0,
        prose: None,
    };
    let family = declaration(&home, "Shape", Kind::Inductive);
    let structure = declaration(&home, "Point", Kind::Structure);

    assert_eq!(member_anchor(&family, &member, false), "Shape/x");
    assert_eq!(member_anchor(&structure, &member, false), "Point.x");
    assert_eq!(member_anchor(&family, &member, true), "/x");
    assert_eq!(member_anchor(&structure, &member, true), ".x");
    assert_eq!(
        witness_anchor(
            &Qualifier::from(["m", "pairs"]),
            &Declaration {
                signature: Signature {
                    text: "satisfy (@A: Type, use Show(A)) =>\n    Show({A, A})".to_string(),
                    marks: Vec::new(),
                },
                ..declaration(&home, "", Kind::Witness)
            }
        ),
        "satisfy(@A:Type,useShow(A))=>Show({A,A})@/m/pairs"
    );
}
