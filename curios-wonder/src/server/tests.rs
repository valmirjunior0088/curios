use {
    super::*,
    crate::*,
    curios_common::{Report, SourceName},
    curios_elab::DEFAULT_BUDGET,
    curios_module::Overlay,
    lsp_types::{DocumentSymbol, Position, SymbolKind},
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
        sync::Arc,
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
        paths: vec![scratch()],
        raised: Raised::Document,
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
    assert!(dirty == BTreeMap::from([(scratch(), Raised::Document)]));
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
        paths: vec![PathBuf::from("/")],
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

/// A check of `program`, the program standing for a document: what the server's check answers and leaves behind it.
fn check(program: &str) -> Diagnosed {
    crate::checked(
        DEFAULT_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                text: program.to_string(),
                written: WrittenIn::Program,
            },
            declares: None,
            package: None,
        },
        &Overlay::default(),
        None,
        Path::new("<stdin>"),
        &mut |_| {},
    )
}

/// What a check of `program` leaves for a place to be answered from.
fn checked(program: &str) -> Placed {
    check(program).placed.expect("the program lowers")
}

/// The declaration named at the `nth` occurrence of `written` in `buffer`, by what a check of `program` left: by the path it is answered by.
fn named(program: &str, buffer: &str, written: &str, nth: usize) -> Option<String> {
    let (offset, _) = buffer
        .match_indices(written)
        .nth(nth)
        .expect("the buffer writes it");

    match answered_at(buffer, &checked(program), offset)?.0.stands {
        Stands::Declaration { described, .. } => Some(described.asked),
        Stands::Local { .. }
        | Stands::Field { .. }
        | Stands::Module { .. }
        | Stands::Term { .. } => None,
    }
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

/// A member's hover shows it within its owner, as a block's entry does: a constructor by its family's head, its own line and the closing word, with its prose beneath.
#[test]
fn a_members_hover_shows_it_within_its_owner() {
    const PROGRAM: &str = "induct Tone: pub Type\n--- The high one.\n| high()\n| low()\nend\nlet one: Tone = Tone/high();\n/std/print(\"\")\n";
    let offset = PROGRAM.rfind("high").expect("the program names it");
    let (answer, _) = answered_at(PROGRAM, &checked(PROGRAM), offset).expect("a name stands there");

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
            answered_at(program, &checked(program), offset).expect("a name stands there");

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
    let held = checked(PROGRAM);

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
    let held = checked(PROGRAM);

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
    let held = checked(PROGRAM);

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
        !check(&refused).diagnostics.is_empty(),
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

/// A written definition is outlined by the type it states: a type where it states a sort, a function where it states a function's, and a constant where it states any other.
#[test]
fn a_written_definition_is_outlined_by_the_type_it_states() {
    const DEFINED: &str = "\
let one: /std/Nat = 1;
let same(n: /std/Nat) -> /std/Nat = n;
let Count: Type = /std/Nat;

/std/print(\"\")
";
    let symbols = outline(DEFINED, &checked(DEFINED));

    assert_eq!(
        symbols.iter().map(|symbol| symbol.kind).collect::<Vec<_>>(),
        [
            SymbolKind::CONSTANT,
            SymbolKind::FUNCTION,
            SymbolKind::TYPE_PARAMETER
        ]
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

/// What `range` covers of `buffer`, a buffer of ASCII lines, where a character is a byte.
fn covered(buffer: &str, range: &Range) -> String {
    let offset = |position: Position| {
        buffer
            .split_inclusive('\n')
            .take(position.line as usize)
            .map(str::len)
            .sum::<usize>()
            + position.character as usize
    };

    buffer[offset(range.start)..offset(range.end)].to_string()
}

/// A selection grows from the innermost term holding a position through each term around it to its declaration, and a position between two declarations is held by none. Mutation-checked: with every term inside the last one kept whether or not it holds the position, the chain runs into the parameter's type.
#[test]
fn a_selection_grows_through_each_term_around_a_position_to_its_declaration() {
    let program = "pub let double(x: Nat) -> Nat = x + x * 2;\n\n/std/print(\"ok\")\n";
    let checked = checked(program);
    let mut grown = Vec::new();
    let mut selection = selected(
        program,
        &checked,
        program.find("2;").expect("the program writes it"),
    );
    while let Some(range) = selection {
        grown.push(covered(program, &range.range));
        selection = range.parent.map(|parent| *parent);
    }

    assert_eq!(
        grown,
        [
            "2",
            "x * 2",
            "x + x * 2",
            "pub let double(x: Nat) -> Nat = x + x * 2;"
        ]
    );
    assert_eq!(
        selected(
            program,
            &checked,
            program.find("\n\n").expect("a blank line") + 1
        ),
        None
    );
}

/// A declaration written over several lines folds from its first line to its last, and one written on one line does not. Mutation-checked: with a text folded on its one line too, the one-line declaration folds.
#[test]
fn a_declaration_over_several_lines_folds_and_one_on_a_line_does_not() {
    let program = "pub let one(x: Nat) -> Nat = x;\n\npub let two(x: Nat) -> Nat =\n    x + x;\n\n/std/print(\"ok\")\n";
    let folds = folded(program, &checked(program))
        .into_iter()
        .map(|fold| (fold.start_line, fold.end_line))
        .collect::<Vec<_>>();

    assert_eq!(folds, [(2, 3)]);
}

/// A segment of a path that reaches a module the program declares links to where its `mod` line names it, and the mount a `use` line starts from links nowhere. Mutation-checked: with a segment linked to itself, the link at `Shapes` lands on the call.
#[test]
fn a_path_through_a_module_links_to_where_its_mod_line_names_it() {
    let program = "use /std/{Nat};\n\nmod Shapes\n    use /std/{Nat};\n\n    pub let side() -> Nat = 3;\nend\n\n/std/print(Nat/to_str(Shapes/side()))\n";
    let checked = checked(program);
    let links = linked(program, &checked)
        .into_iter()
        .map(|(range, declared)| (covered(program, &range), declared.start))
        .collect::<Vec<_>>();
    let named = program.find("mod Shapes").expect("the program declares it") + 4;

    assert!(links.contains(&("Shapes".to_string(), named)), "{links:?}");
    assert!(
        links.iter().all(|(segment, _)| segment != "std"),
        "{links:?}"
    );
}

/// A `let`'s binder and a lambda's parameter written as a name with no type are hinted with the type they were opened at, after the name, and a `let` and a parameter that state theirs, or a match arm's binder, whose form takes none, are not. Mutation-checked: with every written binder noted as stating no type, the stated ones and the arm's are hinted too.
#[test]
fn a_binder_that_states_no_type_where_it_could_is_hinted_with_its_type() {
    let program = "use /std/{Nat, List, Option};\n\npub let total(n: Nat, o: Option(Nat)) -> Nat =\n    let sum = n + 1;\n    let stated: Nat = sum;\n    let held = match o | some(v) => v | none() => 0 end;\n    List/fold([1, 2], stated + held, (x, acc: Nat) => x + acc);\n\n/std/print(\"ok\")\n";
    let whole = Range {
        start: Position::new(0, 0),
        end: position_of(program, program.len()),
    };
    let hints = hinted(program, &checked(program), whole)
        .into_iter()
        .map(|hint| {
            let before = &program[..offset_of(program, hint.position)];
            let binder = before
                .rsplit(|character: char| !character.is_alphanumeric())
                .next()
                .expect("a hint follows a name");
            let InlayHintLabel::String(label) = hint.label else {
                panic!("a hint is written as one label");
            };

            (binder.to_string(), label)
        })
        .collect::<Vec<_>>();

    assert_eq!(
        hints,
        [
            ("sum".to_string(), ": Nat".to_string()),
            ("held".to_string(), ": Nat".to_string()),
            ("x".to_string(), ": Nat".to_string()),
        ]
    );
}

/// A concept's and a type's witnesses are where they are written, read off the name at either, and a method's name, which is neither, answers none. Mutation-checked: answering where the declaration itself is written, as definition does, the concept's name answers the concept.
#[test]
fn a_concept_and_a_type_answer_the_witnesses_they_take_part_in() {
    let program = "use /std/{Str};\n\npub concept Named(A: Type): pub Type {\n    name(A) -> Str,\n}\n\npub struct Point: Type {\n    x: Str,\n}\n\nsatisfy Named(Point) {\n    name = (p) => p.x,\n}\n\n/std/print(Named/name(Point { x = \"here\" }))\n";
    let checked = checked(program);
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

/// A check is told the editor as it goes where the client takes progress: a progress begun under a token the client is asked to create, a report for each unit the fold compiles and none for one it reuses or for the program, and an end; a client that advertises none is told none. Mutation-checked: with a report at a reused unit too, the reused prefix is reported.
#[test]
fn a_check_is_told_as_a_progress_reporting_each_unit_compiled() {
    let sent = Arc::new(std::sync::Mutex::new(Vec::new()));
    let sender: Box<dyn Fn(Message) -> Result<(), String> + Send> = {
        let sent = Arc::clone(&sent);
        Box::new(move |message| {
            sent.lock()
                .expect("the test holds the lock alone")
                .push(message);
            Ok(())
        })
    };
    let mut told = 0;
    let compiled = curios_common::Qualifier::from(["app"]);
    let reused = curios_common::Qualifier::from(["std"]);
    let mut reporting = Reporting {
        sender: &*sender,
        told: &mut told,
    };
    reporting.asked(&curios_package::Named::Stdin, None);
    reporting.step(curios_pipeline::Progress::Reused(&reused), None);
    reporting.step(
        curios_pipeline::Progress::Compiling {
            unit: &compiled,
            compiled: curios_module::Compiled::Built,
        },
        None,
    );
    reporting.step(curios_pipeline::Progress::Entry { held: false }, None);
    reporting.ended(Ended::Done);

    let heard = sent
        .lock()
        .expect("the test holds the lock alone")
        .iter()
        .map(|message| match message {
            Message::Request(request) => request.method.clone(),
            Message::Notification(notification) => {
                match serde_json::from_value::<ProgressParams>(notification.params.clone())
                    .expect("a progress")
                    .value
                {
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(begun)) => {
                        format!("begin {}", begun.message.unwrap_or_default())
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
                        format!("report {}", report.message.unwrap_or_default())
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::End(_)) => "end".to_string(),
                }
            }
            Message::Response(_) => "response".to_string(),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        heard,
        [
            "window/workDoneProgress/create".to_string(),
            "begin <stdin>".to_string(),
            format!("report compiling {}", compiled.join()),
            "end".to_string(),
        ]
    );

    let mut params = lsp_types::InitializeParams::default();
    assert!(!reports_progress(&params));
    params.capabilities.window = Some(lsp_types::WindowClientCapabilities {
        work_done_progress: Some(true),
        ..Default::default()
    });
    assert!(reports_progress(&params));
}

/// The tokens `buffer` is answered with by a check of `program`, each as its line, its column and its length in UTF-16, its type's name and its modifiers' names: the deltas the protocol carries, read back.
fn decoded(program: &str, buffer: &str) -> Vec<(u32, u32, u32, String, Vec<String>)> {
    let legend = legend();
    let mut at = (0, 0);

    tokens(buffer, &checked(program))
        .into_iter()
        .map(|token| {
            at = match token.delta_line {
                0 => (at.0, at.1 + token.delta_start),
                lines => (at.0 + lines, token.delta_start),
            };
            let type_ = legend.token_types[usize::try_from(token.token_type).unwrap()].as_str();
            let modifiers = MODIFIERS
                .iter()
                .enumerate()
                .filter(|(place, _)| token.token_modifiers_bitset & (1 << place) != 0)
                .map(|(_, modifier)| modifier.as_str().to_string())
                .collect();

            (at.0, at.1, token.length, type_.to_string(), modifiers)
        })
        .collect()
}

/// A program whose second line holds a character past the Basic Multilingual Plane before a name: two units of UTF-16, one scalar and four bytes.
const TOKENED: &str =
    "let one: /std/Str = \"𝄞\";\nlet two: {/std/Str, /std/Str} = (\"𝄞\", one);\n/std/print(two.0)";

/// A document's tokens are the classes its last check left, each at its line and its column in UTF-16, encoded over the legend with its modifiers: a value as a read-only variable, a segment and a type of `/std` as the default library's, and `declaration` where a name is declared.
#[test]
fn a_documents_tokens_are_its_classes_in_utf16() {
    let token = |column, length, type_: &str, modifiers: &[&str]| {
        (
            1,
            column,
            length,
            type_.to_string(),
            modifiers
                .iter()
                .map(|modifier| modifier.to_string())
                .collect(),
        )
    };
    let second = decoded(TOKENED, TOKENED)
        .into_iter()
        .filter(|(line, ..)| *line == 1)
        .collect::<Vec<_>>();

    assert_eq!(
        second,
        [
            token(4, 3, "variable", &["declaration", "readonly"]),
            token(11, 3, "namespace", &["defaultLibrary"]),
            token(15, 3, "type", &["defaultLibrary"]),
            token(21, 3, "namespace", &["defaultLibrary"]),
            token(25, 3, "type", &["defaultLibrary"]),
            token(39, 3, "variable", &["readonly"]),
        ]
    );
}

/// A declaration edited since its check answers no token, keeping the grammar's colours until it is checked again, and the others stand where they stand.
#[test]
fn a_declaration_edited_since_its_check_answers_no_token() {
    let edited = TOKENED.replace("\"𝄞\";", "\"𝄞𝄞\";");
    let lines = |buffer: &str| {
        decoded(TOKENED, buffer)
            .into_iter()
            .map(|(line, column, ..)| (line, column))
            .collect::<Vec<_>>()
    };
    let unedited = lines(TOKENED);

    assert!(unedited.iter().any(|(line, _)| *line == 0));
    assert_eq!(
        lines(&edited),
        unedited
            .into_iter()
            .filter(|(line, _)| *line != 0)
            .collect::<Vec<_>>()
    );
}

/// A refresh is asked only of a client that declares it takes one.
#[test]
fn a_refresh_is_asked_only_of_a_client_that_takes_one() {
    let client = |capabilities| {
        serde_json::from_value::<InitializeParams>(
            serde_json::json!({ "capabilities": capabilities }),
        )
        .expect("the parameters read")
    };

    assert!(refreshes_tokens(&client(serde_json::json!({
        "workspace": { "semanticTokens": { "refreshSupport": true } }
    }))));
    assert!(!refreshes_tokens(&client(serde_json::json!({
        "workspace": { "semanticTokens": { "refreshSupport": false } }
    }))));
    assert!(!refreshes_tokens(&client(serde_json::json!({}))));
}
