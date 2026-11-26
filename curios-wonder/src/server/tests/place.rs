//! What a place of a document answers: a hover, a word and a line's names alike through both transports, a buffer as it was checked, and what an edit since leaves standing.

use {
    super::test_support::*,
    crate::{server::*, *},
    curios_common::{Report, SourceName},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    lsp_types::Position,
    std::{path::Path, sync::Arc},
};

/// A member's hover shows it within its owner, as a block's entry does: a constructor by its family's head, its own line and the closing word, with its prose beneath.
#[test]
fn a_members_hover_shows_it_within_its_owner() {
    const PROGRAM: &str = "induct Tone: pub Type\n--- The high one.\n| high()\n| low()\nend\nlet one: Tone = Tone/high();\n/std/print(\"\")\n";
    let offset = PROGRAM.rfind("high").expect("the program names it");
    let (answer, _) =
        answered_at(PROGRAM, &checked_program(PROGRAM), offset).expect("a name stands there");

    assert_eq!(
        markup(&answer),
        "```curios\ninduct Tone: pub Type\n| high()\nend\n```\n\nThe high one."
    );
}

/// A bare name's hover ends on the `use` line that brought it into scope, as it is written, a glob's as a selection's; a name the program declares ends on none.
#[test]
fn a_bare_names_hover_ends_on_the_line_that_brought_it() {
    const SELECTED: &str =
        "use /std/{Nat, print};\nlet one: Nat = 1;\nlet two: Nat = one;\nprint(\"\")\n";
    const GLOBBED: &str = "use /std/*;\nlet one: Nat = 1;\nprint(\"\")\n";
    let hover = |program: &str, written: &str| {
        let offset = program.rfind(written).expect("the program writes it");
        let (answer, _) =
            answered_at(program, &checked_program(program), offset).expect("a name stands there");

        markup(&answer)
    };

    let selected = hover(SELECTED, "Nat = one");
    assert!(
        selected.ends_with("\n\n```curios\nuse /std/{Nat, print};\n```"),
        "{selected}"
    );
    let declared = hover(SELECTED, "one;");
    assert!(!declared.contains("use "), "{declared}");
    let globbed = hover(GLOBBED, "Nat");
    assert!(
        globbed.ends_with("\n\n```curios\nuse /std/*;\n```"),
        "{globbed}"
    );
}

/// Every place of a program answers one record through both transports: the server's, off what the document's check left, and `wonder --at`'s, off its own question of the place, a name, a literal and the term holding them among them.
#[test]
fn a_place_answers_alike_through_both_transports() {
    const PROGRAM: &str =
        "use /std/{Nat, print};\nlet one: Nat = 1;\nlet two: Nat = one + 1;\nprint(\"\")\n";
    let held = checked_program(PROGRAM);

    for (line, text) in PROGRAM.lines().enumerate() {
        for (column, _) in text.char_indices() {
            let offset = PROGRAM
                .lines()
                .take(line)
                .map(|text| text.len() + 1)
                .sum::<usize>()
                + column;
            let asked = crate::placed(
                DEFAULT_BUDGET,
                Subject::Entry {
                    units: Vec::new(),
                    origin: Origin::Text {
                        text: PROGRAM.to_string(),
                        written: WrittenIn::Program,
                    },
                    declares: None,
                    package: None,
                },
                Path::new("<stdin>"),
                (line + 1, column + 1),
                &Overlay::default(),
                None,
                &mut |_| {},
            )
            .expect("the program lowers");

            // Each answer as a hover renders it over the range it covers: the two checks read two allocations of the one text, which spans tell apart.
            let rendered = |answer: &Answer| (answer.range.start, answer.range.end, markup(answer));
            assert_eq!(
                answered_at(PROGRAM, &held, offset).map(|(answer, _)| rendered(&answer)),
                asked.as_ref().map(rendered),
                "at {}:{}",
                line + 1,
                column + 1
            );
        }
    }
}

/// A word elaboration resolves answers one record through both transports: a field's label, a projection's, a pattern's constructor and a concept's method.
#[test]
fn a_resolved_word_answers_alike_through_both_transports() {
    const PROGRAM: &str = "\
use /std/{Nat, Show, print};
struct Pair: Type {
    first: Nat,
}
induct Shade: Type
| dark(Nat)
end
satisfy Show(Shade) {
    show(_) = \"shade\",
}
let made: Pair = Pair { first = 1 };
let read(held: Pair) -> Nat = held.first;
let toned(shade: Shade) -> Nat =
    match shade
    | dark(depth) => depth
    end;
print(\"\")
";
    let held = checked_program(PROGRAM);

    for written in ["first = 1", "first;", "dark(depth)", "show(_)", "Pair {"] {
        let offset = PROGRAM.find(written).expect("the program writes it");
        let before = &PROGRAM[..offset];
        let line = before.matches('\n').count() + 1;
        let column = offset - before.rfind('\n').map_or(0, |at| at + 1) + 1;
        let asked = crate::placed(
            DEFAULT_BUDGET,
            Subject::Entry {
                units: Vec::new(),
                origin: Origin::Text {
                    text: PROGRAM.to_string(),
                    written: WrittenIn::Program,
                },
                declares: None,
                package: None,
            },
            Path::new("<stdin>"),
            (line, column),
            &Overlay::default(),
            None,
            &mut |_| {},
        )
        .expect("the program lowers");
        let rendered = |answer: &Answer| (answer.range.start, answer.range.end, markup(answer));

        assert!(asked.is_some(), "nothing stands at `{written}`");
        assert_eq!(
            answered_at(PROGRAM, &held, offset).map(|(answer, _)| rendered(&answer)),
            asked.as_ref().map(rendered),
            "at `{written}`"
        );
    }
}

/// A `use` and a `mod` line's names answer one record through both transports: a segment, a selector and the module a `mod` line declares.
#[test]
fn a_lines_names_answer_alike_through_both_transports() {
    const PROGRAM: &str = "\
pub mod Shapes
    pub induct Colour: pub Type
    | red()
    end
end
use /Shapes/{Colour};
let picked: Colour = Colour/red();
/std/print(\"\")
";
    let held = checked_program(PROGRAM);

    for written in ["Shapes\n", "Shapes/{", "Colour}"] {
        let offset = PROGRAM.find(written).expect("the program writes it");
        let before = &PROGRAM[..offset];
        let line = before.matches('\n').count() + 1;
        let column = offset - before.rfind('\n').map_or(0, |at| at + 1) + 1;
        let asked = crate::placed(
            DEFAULT_BUDGET,
            Subject::Entry {
                units: Vec::new(),
                origin: Origin::Text {
                    text: PROGRAM.to_string(),
                    written: WrittenIn::Program,
                },
                declares: None,
                package: None,
            },
            Path::new("<stdin>"),
            (line, column),
            &Overlay::default(),
            None,
            &mut |_| {},
        )
        .expect("the program lowers");
        let rendered = |answer: &Answer| (answer.range.start, answer.range.end, markup(answer));

        assert!(asked.is_some(), "nothing stands at `{written}`");
        assert_eq!(
            answered_at(PROGRAM, &held, offset).map(|(answer, _)| rendered(&answer)),
            asked.as_ref().map(rendered),
            "at `{written}`"
        );
    }
}

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

/// A program its elaboration refuses answers every place of it all the same, off the lowering its check made on the way: a file with one mistake still says where each of its names is declared.
#[test]
fn a_refused_program_answers_its_places_from_its_check() {
    let refused = CHECKED.replace("let two: /std/Nat = one;", "let two: /std/Bool = one;");

    assert!(
        !diagnosed(&refused).diagnostics.is_empty(),
        "the check refuses the program"
    );
    assert_eq!(named(&refused, &refused, "one", 1).as_deref(), Some("/one"));
    assert_eq!(named(&refused, &refused, "two", 1).as_deref(), Some("/two"));
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

/// A diagnostic placed where no file on the machine holds the text, as in the library a compiler carries, is filed under the checked document with no place, and a note placed there closes its message as a report spells it: an editor is handed no place it cannot open.
#[test]
fn a_place_no_file_holds_is_no_location_an_editor_is_handed() {
    let name = SourceName::of("std", Path::new("List.crs")).expect("a plain path");
    let library = Source::named(name, "let a = 1;\n");
    let report = Report::at(Span::new(Arc::clone(&library), 4, 5), "`a` is declared")
        .noting(Span::new(library, 4, 5), "first declared here");
    let (path, diagnostic) = adapt(
        &scratch(),
        &Diagnosis {
            severity: Severity::Error,
            report,
        },
    );

    assert_eq!(path, scratch());
    assert_eq!(diagnostic.range, Range::default());
    assert_eq!(diagnostic.related_information, None);
    assert!(
        diagnostic
            .message
            .ends_with("\nfirst declared here; /std/List.crs:1:5"),
        "{}",
        diagnostic.message
    );
}

/// A concept's and a type's witnesses are where they are written, read off the name at either, and a method's name, which is neither, answers none. Mutation-checked: answering where the declaration itself is written, as definition does, the concept's name answers the concept.
#[test]
fn a_concept_and_a_type_answer_the_witnesses_they_take_part_in() {
    let program = "use /std/{Str};\n\npub concept Named(A: Type): pub Type {\n    name(A) -> Str,\n}\n\npub struct Point: Type {\n    x: Str,\n}\n\nsatisfy Named(Point) {\n    name = (p) => p.x,\n}\n\n/std/print(Named/name(Point { x = \"here\" }))\n";
    let checked = checked_program(program);
    let head = program
        .find("Named(Point)")
        .expect("the program writes the witness's head");
    let at = |written: &str| {
        witnessed_at(
            program,
            &checked,
            program.rfind(written).expect("the program writes it"),
        )
        .map(|places| places.iter().map(|place| place.start).collect::<Vec<_>>())
    };

    assert_eq!(at("Named/"), Some(vec![head]));
    assert_eq!(at("Point {"), Some(vec![head]));
    assert_eq!(at("name("), None);
}
