use {
    super::{Job, Raised, SETTLE, SETTLE_SHARE, batch, named_at, offset_of, outline, position_of},
    crate::{Origin, Subject, Written},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
    lsp_types::{DocumentSymbol, Position, SymbolKind},
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
        time::Duration,
    },
};

/// The document every job here is raised for.
fn scratch() -> PathBuf {
    PathBuf::from("/scratch.crs")
}

/// What the editor holds once `scratch` reads `text`.
fn overlay(text: &str) -> BTreeMap<PathBuf, String> {
    BTreeMap::from([(scratch(), text.to_string())])
}

/// A keystroke leaving `scratch` at `text`.
fn edit(text: &str) -> Job {
    Job {
        document: scratch(),
        raised: Raised::Edited,
        documents: overlay(text),
    }
}

/// A burst settles before it is compiled: handed the first keystroke, an idle analyst asks to wait, and what arrives inside the wait is one batch from the text the burst ended on. Without the wait it would start on the first keystroke's text and publish an answer about a buffer two keystrokes out of date.
#[test]
fn a_burst_is_one_batch_from_the_text_it_ended_on() {
    let spent = Duration::from_secs(1);
    let mut rest = [edit("mm"), edit("mmm")].into_iter();
    let mut waits = Vec::new();

    let (documents, dirty) = batch(Some(spent), edit("m"), |wait| {
        waits.push(wait);
        rest.next()
    });

    assert_eq!(documents, overlay("mmm"));
    assert!(dirty == BTreeMap::from([(scratch(), Raised::Edited)]));
    assert_eq!(waits, [spent / SETTLE_SHARE; 3]);
}

/// The wait is its share of what the last check cost: nothing for a session's first check, which has no cost to weigh, and the cap for a check whose share would pass it.
#[test]
fn a_wait_is_a_share_of_the_last_check_and_never_past_the_cap() {
    let waits = |spent| {
        let mut waits = Vec::new();
        batch(spent, edit("m"), |wait| {
            waits.push(wait);
            None
        });

        waits
    };

    assert_eq!(waits(None), [Duration::ZERO]);
    assert_eq!(waits(Some(SETTLE)), [SETTLE / SETTLE_SHARE]);
    assert_eq!(waits(Some(SETTLE * SETTLE_SHARE * 2)), [SETTLE]);
}

/// The warming check is worth the whole cap by itself, a document arriving being the reason to drop it, and the wait is the document's once one joins.
#[test]
fn a_warming_batch_waits_the_cap_until_a_document_joins_it() {
    let warming = Job {
        document: PathBuf::from("/"),
        raised: Raised::Warming,
        documents: BTreeMap::new(),
    };
    let mut rest = [edit("m")].into_iter();
    let mut waits = Vec::new();

    let (documents, _) = batch(None, warming, |wait| {
        waits.push(wait);
        rest.next()
    });

    assert_eq!(documents, overlay("m"));
    assert_eq!(waits, [SETTLE, Duration::ZERO]);
}

/// What a check of `program` leaves for a place to be answered from, the program standing for a document.
fn checked(program: &str) -> Vec<Written> {
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
        &Overlay::default(),
        None,
    )
    .expect("the program lowers")
}

/// The declaration named at the `nth` occurrence of `written` in `buffer`, by what a check of `program` left.
fn named(program: &str, buffer: &str, written: &str, nth: usize) -> Option<String> {
    let (offset, _) = buffer
        .match_indices(written)
        .nth(nth)
        .expect("the buffer writes it");

    named_at(buffer, &checked(program), offset).map(|named| named.name.clone())
}

/// An outline as its names, each with the line its label stands on and the names under it.
fn outlined(symbols: &[DocumentSymbol]) -> Vec<(String, u32, Vec<String>)> {
    symbols
        .iter()
        .map(|symbol| {
            let members = symbol.children.iter().flatten();

            (
                symbol.name.clone(),
                symbol.selection_range.start.line,
                members.map(|member| member.name.clone()).collect(),
            )
        })
        .collect()
}

const CHECKED: &str = "\
let one: /std/Nat = 1;
let two: /std/Nat = one;
let three: /std/Nat = two;
/std/print(\"\")
";

/// A buffer that is the text its check read answers every place of it.
#[test]
fn a_buffer_as_it_was_checked_answers_every_name() {
    assert_eq!(named(CHECKED, CHECKED, "one", 1).as_deref(), Some("/one"));
    assert_eq!(named(CHECKED, CHECKED, "two", 1).as_deref(), Some("/two"));
    assert_eq!(
        named(CHECKED, CHECKED, "three", 0).as_deref(),
        Some("/three")
    );
    assert_eq!(named(CHECKED, CHECKED, "let", 0), None);
}

/// An edit inside one declaration leaves the others answering, the ones below it where they now stand, and the edited one answering nothing until it is checked again: its names are no longer where the check read them.
#[test]
fn a_declaration_edited_since_the_check_answers_nothing_and_the_others_where_they_stand() {
    let edited = CHECKED.replace("= one;", "= 1 + one;");

    assert_eq!(named(CHECKED, &edited, "one", 0).as_deref(), Some("/one"));
    assert_eq!(named(CHECKED, &edited, "one", 1), None);
    assert_eq!(named(CHECKED, &edited, "two", 0), None);
    assert_eq!(named(CHECKED, &edited, "two", 1).as_deref(), Some("/two"));
    assert_eq!(
        named(CHECKED, &edited, "three", 0).as_deref(),
        Some("/three")
    );
}

/// An edit touching a text is an edit of it where what it writes reads on as part of the text's own name: a keystroke extending a name leaves another name than the one checked, and one that only follows it leaves the name standing.
#[test]
fn a_name_an_edit_extends_waits_for_its_check_and_one_it_only_follows_stands() {
    const NAMED_LAST: &str = "let one: /std/Nat = 1;\none\n";
    assert_eq!(
        named(NAMED_LAST, NAMED_LAST, "one", 1).as_deref(),
        Some("/one")
    );

    let extended = NAMED_LAST.replace("\none\n", "\nones\n");
    assert_eq!(named(NAMED_LAST, &extended, "one", 1), None);

    let followed = NAMED_LAST.replace("\none\n", "\none + 1\n");
    assert_eq!(
        named(NAMED_LAST, &followed, "one", 1).as_deref(),
        Some("/one")
    );

    let preceded = NAMED_LAST.replace("\none\n", "\n1 + one\n");
    assert_eq!(
        named(NAMED_LAST, &preceded, "one", 1).as_deref(),
        Some("/one")
    );

    let prefixed = NAMED_LAST.replace("\none\n", "\nmy/one\n");
    assert_eq!(named(NAMED_LAST, &prefixed, "one", 1), None);
}

/// Text typed above a declaration that opens as the declaration does reads, from the start, as typed inside it: the declaration still stands, by what the texts share at their ends.
#[test]
fn a_declaration_typed_above_another_that_opens_alike_leaves_it_standing() {
    let above = format!("let zero: /std/Nat = 0;\n\n{CHECKED}");

    assert_eq!(named(CHECKED, &above, "one", 0).as_deref(), Some("/one"));
    assert_eq!(named(CHECKED, &above, "one", 1).as_deref(), Some("/one"));
    assert_eq!(named(CHECKED, &above, "zero", 0), None);
}

/// What a buffer declares is outlined from the declarations that stand in it, each where it now stands.
#[test]
fn an_outline_holds_the_declarations_that_stand_where_they_stand() {
    let texts = checked(CHECKED);
    assert_eq!(
        outlined(&outline(CHECKED, &texts)),
        [
            ("one".to_string(), 0, Vec::new()),
            ("two".to_string(), 1, Vec::new()),
            ("three".to_string(), 2, Vec::new()),
        ]
    );

    let edited = CHECKED.replace("= one;", "=\n    one;");
    assert_eq!(
        outlined(&outline(&edited, &texts)),
        [
            ("one".to_string(), 0, Vec::new()),
            ("three".to_string(), 3, Vec::new()),
        ]
    );
}

/// A constructor is outlined under its type and a method under its concept, and a witness by its head as written.
#[test]
fn an_outline_shows_a_member_under_what_declares_it() {
    const DECLARED: &str = "\
concept Painted(Canvas: Type): pub Type {
    paint(Canvas) -> Canvas,
}

induct Colour: pub Type
| red()
| green()
end

satisfy Painted(Colour) {
    paint(shown) = shown,
}

/std/print(\"\")
";
    let symbols = outline(DECLARED, &checked(DECLARED));

    assert_eq!(
        outlined(&symbols),
        [
            ("Painted".to_string(), 0, vec!["paint".to_string()]),
            (
                "Colour".to_string(),
                4,
                vec!["red".to_string(), "green".to_string()]
            ),
            ("Painted(Colour)".to_string(), 9, Vec::new()),
        ]
    );
    assert_eq!(
        symbols.iter().map(|symbol| symbol.kind).collect::<Vec<_>>(),
        [SymbolKind::INTERFACE, SymbolKind::ENUM, SymbolKind::OBJECT]
    );
}

/// A protocol position is the offset it was derived from, in a text whose characters take one unit and two: the two directions of the one place UTF-16 is spoken.
#[test]
fn a_position_names_the_offset_it_was_derived_from() {
    let text = "first\ns\u{e9}cond \u{1d538} line\n\nlast";

    for offset in (0..=text.len()).filter(|offset| text.is_char_boundary(*offset)) {
        assert_eq!(offset_of(text, position_of(text, offset)), offset);
    }

    // Past a line's end is its end, and past the last line the text's.
    let past = |line, character| offset_of(text, Position { line, character });
    assert_eq!(past(0, 40), 5);
    assert_eq!(past(9, 0), text.len());
}
