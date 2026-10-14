//! What `wonder at` and `wonder declarations` answer: the names a file writes, read off its subject as it was lowered, each with the declaration it names.

use {
    crate::{Declaration, Named, Origin, Subject, Written},
    curios_core::Declared,
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::{Overlay, RootSource},
    std::path::Path,
};

/// Every text of `program`, asked the way the one-shot transport asks of standard input, reading through `overlay`.
fn asked(program: &str, overlay: &Overlay) -> Vec<Written> {
    crate::written(
        DEFAULT_STEP_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: program.to_string(),
            },
            declares: None,
            unlinked: None,
        },
        Path::new("<stdin>"),
        overlay,
        None,
    )
    .expect("the program lowers")
}

/// The name standing at the `nth` occurrence of `written` in `program`, `within` bytes into it.
fn at(program: &str, written: &str, nth: usize, within: usize) -> Option<Named> {
    let texts = asked(program, &Overlay::default());
    let (offset, _) = program
        .match_indices(written)
        .nth(nth)
        .expect("the program writes it");

    Named::at(texts.iter().flat_map(|text| &text.names), offset + within).cloned()
}

/// What a row says of `named`: the declaration, what it is declared as, whether this is where it is named, and the line and column its name is written at.
fn said(named: &Named) -> (&str, Declared, bool, Option<(usize, usize)>) {
    (
        named.name.as_str(),
        named.declared,
        named.declares,
        named
            .declaration
            .as_ref()
            .and_then(Declaration::place)
            .map(|place| place.line_column()),
    )
}

const REFUSED: &str = "let one: /std/Nat = true;\nlet two: /std/Nat = one;\n/std/print(\"\")";

/// A written name is resolved where it is lowered, so a program elaboration refuses answers as one it accepts: the first declaration here is a type mismatch.
#[test]
fn a_program_that_does_not_check_answers_what_it_writes() {
    let named = at(REFUSED, "one", 1, 0).expect("a name stands at the use");

    assert_eq!(
        said(&named),
        ("/one", Declared::Authored, false, Some((1, 5)))
    );
}

/// A declaration's own name answers the declaration, as a use of it does.
#[test]
fn a_declarations_own_name_answers_itself() {
    let named = at(REFUSED, "two", 0, 1).expect("a name stands at the label");

    assert_eq!(
        said(&named),
        ("/two", Declared::Authored, true, Some((2, 5)))
    );
}

/// An offset at a name's end is at the name, and one where nothing is named answers nothing.
#[test]
fn a_name_is_answered_through_its_end_and_no_further() {
    let ended = at(REFUSED, "one", 1, 3).expect("the offset after a name's last byte is at it");
    assert_eq!(ended.name, "/one");

    assert_eq!(at(REFUSED, "let", 0, 1), None);
    assert_eq!(at(REFUSED, "true", 0, 1), None);
}

const WITNESSED: &str = "\
concept Painted(Canvas: Type): pub Type {
    paint(Canvas) -> Canvas,
}

induct Colour: pub Type
| red()
end

satisfy Painted(Colour) {
    paint(shown) = shown,
}

/std/print(\"\")";

/// A witness is named by its head, which names its concept and its type inside it: the narrowest name holding an offset is the one that stands there, so the concept answers at its own name and the witness where the head names nothing else.
#[test]
fn the_narrowest_name_stands_at_an_offset() {
    let concept = at(WITNESSED, "Painted(Colour)", 0, 2).expect("the concept's name");
    assert_eq!(
        said(&concept),
        ("/Painted", Declared::Concept, false, Some((1, 9)))
    );

    let family = at(WITNESSED, "Painted(Colour)", 0, 9).expect("the type's name");
    assert_eq!(
        said(&family),
        ("/Colour", Declared::Inductive, false, Some((5, 8)))
    );

    let witness = at(WITNESSED, "Painted(Colour)", 0, 15).expect("the head's end");
    assert_eq!(
        said(&witness),
        ("Painted(Colour)", Declared::Witness, true, Some((9, 9)))
    );
}

/// What a file declares is each definition its texts name, in the order they are written: a type with its constructors, a concept with its methods, a witness by its head.
#[test]
fn a_files_declarations_are_listed_in_the_order_they_are_written() {
    let texts = asked(WITNESSED, &Overlay::default());
    let declared = texts
        .iter()
        .flat_map(|text| &text.names)
        .filter(|named| named.declares)
        .map(said)
        .collect::<Vec<_>>();

    assert_eq!(
        declared,
        [
            ("/Painted", Declared::Concept, true, Some((1, 9))),
            ("/Painted/paint", Declared::Method, true, Some((2, 5))),
            ("/Colour", Declared::Inductive, true, Some((5, 8))),
            ("/Colour/red", Declared::Constructor, true, Some((6, 3))),
            ("Painted(Colour)", Declared::Witness, true, Some((9, 9))),
        ]
    );
}

const LIBRARY_USE: &str = "/std/print(\"\")";

/// A declaration of the library a compiler was built with is placed where its file still holds the text it was read in, and left unplaced where it does not: an editor's text stands for the disk's, so a buffer that differs is a file that no longer holds it.
#[test]
fn a_declaration_of_the_compilers_own_library_is_placed_where_its_file_still_holds_it() {
    let print = |overlay: &Overlay| {
        let texts = asked(LIBRARY_USE, overlay);

        Named::at(texts.iter().flat_map(|text| &text.names), 6)
            .cloned()
            .expect("the program names the library's `print`")
    };

    let placed = print(&Overlay::default());
    let declaration = placed
        .declaration
        .expect("the library's `print` is written");
    let place = declaration
        .place()
        .expect("the checkout this compiler was built in holds the library's sources");
    let file = place
        .source
        .path
        .clone()
        .expect("a library file has a path");
    assert_eq!(&place.source.text[place.start..place.end], "print");

    let moved = Overlay::of([(file, "-- another text\n".to_string())]);
    let unplaced = print(&moved);
    assert_eq!(unplaced.name, placed.name);
    let unheld = unplaced
        .declaration
        .expect("the declaration is read whatever a file holds");
    assert_eq!(unheld.place(), None);

    // Its head and prose are read off the text its unit holds, whatever a file holds.
    let heading = unheld.heading().expect("the declaration's text states it");
    assert!(
        heading.head.starts_with("pub let print("),
        "{}",
        heading.head
    );
    assert!(!heading.prose.is_empty());
    assert_eq!(declaration.heading(), Some(heading));
}

/// A library is asked through one of its files, and answers as a program does.
#[test]
fn a_library_is_asked_through_one_of_its_files() {
    const LIBRARY: &str =
        "use /std/{Nat};\npub let answer: Nat = 42;\npub let again: Nat = answer;\n";
    let file = Path::new("answers.crs");
    let overlay = Overlay::of([(file.to_path_buf(), LIBRARY.to_string())]);
    let library = RootSource::labelled_module("answers", "answers.crs", LIBRARY)
        .expect("the text is written as a module");

    let texts = crate::written(
        DEFAULT_STEP_BUDGET,
        Subject::Unit {
            units: vec![library],
        },
        file,
        &overlay,
        None,
    )
    .expect("the library lowers");
    let offset = LIBRARY.rfind("answer").expect("the library writes it");
    let named = Named::at(texts.iter().flat_map(|text| &text.names), offset)
        .expect("a name stands at the use");

    assert_eq!(
        said(named),
        ("/answers/answer", Declared::Authored, false, Some((2, 9)))
    );
}
