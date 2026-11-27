//! What the server answers of a document whole: its outline, a selection growing through its terms, its folds, its links, its type hints and its tokens.

use {
    super::test_support::*,
    crate::server::*,
    lsp_types::{DocumentSymbol, Position, SymbolKind},
};

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

/// The tokens `buffer` is answered with by a check of `program`, each as its line, its column and its length in UTF-16, its type's name and its modifiers' names: the deltas the protocol carries, read back.
fn decoded(program: &str, buffer: &str) -> Vec<(u32, u32, u32, String, Vec<String>)> {
    let legend = legend();
    let mut at = (0, 0);

    tokens(buffer, &checked_program(program))
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

/// What a buffer declares is outlined from the declarations that stand in it, each where it now stands.
#[test]
fn an_outline_holds_the_declarations_that_stand_where_they_stand() {
    let texts = checked_program(CHECKED);
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
    paint(shown) => shown,
}

/std/print(\"\")
";
    let symbols = outline(DECLARED, &checked_program(DECLARED));

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
let same(n: /std/Nat) -> /std/Nat => n;
let Count: Type = /std/Nat;

/std/print(\"\")
";
    let symbols = outline(DEFINED, &checked_program(DEFINED));

    assert_eq!(
        symbols.iter().map(|symbol| symbol.kind).collect::<Vec<_>>(),
        [
            SymbolKind::CONSTANT,
            SymbolKind::FUNCTION,
            SymbolKind::TYPE_PARAMETER
        ]
    );
}

/// A selection grows from the innermost term holding a position through each term around it to its declaration, and a position between two declarations is held by none. Mutation-checked: with every term inside the last one kept whether or not it holds the position, the chain runs into the parameter's type.
#[test]
fn a_selection_grows_through_each_term_around_a_position_to_its_declaration() {
    let program = "pub let double(x: Nat) -> Nat => x + x * 2;\n\n/std/print(\"ok\")\n";
    let checked = checked_program(program);
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
            "pub let double(x: Nat) -> Nat => x + x * 2;"
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
    let program = "pub let one(x: Nat) -> Nat => x;\n\npub let two(x: Nat) -> Nat =>\n    x + x;\n\n/std/print(\"ok\")\n";
    let folds = folded(program, &checked_program(program))
        .into_iter()
        .map(|fold| (fold.start_line, fold.end_line))
        .collect::<Vec<_>>();

    assert_eq!(folds, [(2, 3)]);
}

/// A segment of a path that reaches a module the program declares links to where its `mod` line names it, and the mount a `use` line starts from links nowhere. Mutation-checked: with a segment linked to itself, the link at `Shapes` lands on the call.
#[test]
fn a_path_through_a_module_links_to_where_its_mod_line_names_it() {
    let program = "use /std/{Nat};\n\nmod Shapes\n    use /std/{Nat};\n\n    pub let side() -> Nat => 3;\nend\n\n/std/print(Nat/to_str(Shapes/side()))\n";
    let checked = checked_program(program);
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
    let program = "use /std/{Nat, List, Option};\n\npub let total(n: Nat, o: Option(Nat)) -> Nat =>\n    let sum = n + 1;\n    let stated: Nat = sum;\n    let held = match o | some(v) => v | none() => 0 end;\n    List/fold([1, 2], stated + held, (x, acc: Nat) => x + acc);\n\n/std/print(\"ok\")\n";
    let whole = Range {
        start: Position::new(0, 0),
        end: position_of(program, program.len()),
    };
    let hints = hinted(program, &checked_program(program), whole)
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
