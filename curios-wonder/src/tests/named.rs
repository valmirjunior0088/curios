//! What `wonder --at` and the server's requests answer — what stands at each place of a file, read off its subject as its check's fold left it: a name with the declaration it names, or a term — and what `wonder --name` and `wonder --witnesses` answer of the declaration a path names.

use {
    crate::*,
    curios_common::{Qualifier, Source, Span},
    curios_core::{Declared, Totality},
    curios_document::ModuleDocumentation,
    curios_elab::DEFAULT_BUDGET,
    curios_module::{Overlay, RootSource},
    curios_surface::WrittenIn,
    std::collections::BTreeSet,
    std::path::{Path, PathBuf},
};

/// What a check of `program` leaves of it, read as the editor's check reads a document, of standard input, through `overlay`.
fn asked(program: &str, overlay: &Overlay) -> Placed {
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
        overlay,
        None,
        Path::new("<stdin>"),
        &mut |_| {},
    )
    .placed
    .expect("the program lowers")
}

/// What stands at the `nth` occurrence of `written` in `program`, `within` bytes into it.
fn at(program: &str, written: &str, nth: usize, within: usize) -> Option<Answer> {
    let placed = asked(program, &Overlay::default());
    let (offset, _) = program
        .match_indices(written)
        .nth(nth)
        .expect("the program writes it");

    placed.at(offset + within)
}

/// What a row says of the declaration a name stands for: the path it is answered by, or a witness's head as written, what it is declared as, whether the name is where it is named, and the line and column its name is written at.
fn row(described: &Described, declares: bool) -> (&str, Declared, bool, Option<(usize, usize)>) {
    (
        described.asked.as_str(),
        described.declared,
        declares,
        described
            .declaration
            .as_ref()
            .and_then(Declaration::place)
            .map(|place| place.line_column()),
    )
}

/// What a row says of the declaration the name `answer` stands at names.
fn said(answer: &Answer) -> (&str, Declared, bool, Option<(usize, usize)>) {
    match &answer.stands {
        Stands::Declaration {
            declares,
            described,
        } => row(described, *declares),
        Stands::Local { name, .. } => panic!("a local stands there, `{name}`, and no declaration"),
        Stands::Field { label, .. } => {
            panic!("a field stands there, `{label}`, and no declaration")
        }
        Stands::Module { path, .. } => {
            panic!("a module stands there, `{path}`, and no declaration")
        }
        Stands::Term { written, .. } => panic!("a term stands there, `{written}`, and no name"),
    }
}

const REFUSED: &str = "let one: /std/Nat = true;\nlet two: /std/Nat = one;\n/std/print(\"\")";

/// A written name is resolved where it is lowered, so a program elaboration refuses answers as one it accepts: the first declaration here is a type mismatch.
#[test]
fn a_program_that_does_not_check_answers_what_it_writes() {
    let named = at(REFUSED, "one", 1, 0).expect("a name stands at the use");

    assert_eq!(said(&named), ("/one", Declared::Value, false, Some((1, 5))));
}

/// A declaration's own name answers the declaration, as a use of it does.
#[test]
fn a_declarations_own_name_answers_itself() {
    let named = at(REFUSED, "two", 0, 1).expect("a name stands at the label");

    assert_eq!(said(&named), ("/two", Declared::Value, true, Some((2, 5))));
}

/// An offset at a name's end is at the name, and one where nothing is named answers nothing.
#[test]
fn a_name_is_answered_through_its_end_and_no_further() {
    let ended = at(REFUSED, "one", 1, 3).expect("the offset after a name's last byte is at it");
    assert_eq!(said(&ended).0, "/one");

    assert_eq!(at(REFUSED, "let", 0, 1), None);
}

/// A place no name stands at answers the narrowest term written there, as it is written: the literal a refusal stands at.
#[test]
fn a_place_no_name_stands_at_answers_the_narrowest_term() {
    let literal = at(REFUSED, "true", 0, 1).expect("a term stands there");

    assert_eq!(
        (literal.stands, literal.range.line_column()),
        (
            Stands::Term {
                written: "true".to_string(),
                type_: None
            },
            (1, 21)
        )
    );
}

/// The declaration named at `written`'s first occurrence in `program`, `within` bytes into it.
fn declared_at(program: &str, written: &str, within: usize) -> Described {
    match at(program, written, 0, within).map(|answer| answer.stands) {
        Some(Stands::Declaration { described, .. }) => described,
        stands => panic!("no name stands at `{written}`: {stands:?}"),
    }
}

const TYPED: &str = "let one: /std/Nat = true;\nlet two: /std/Nat = one;\nlet three: /std/Nat = 3;\n/std/print(\"\")";

const LOCAL: &str = "let twice(n: /std/Nat) -> /std/Nat = n + n;\nlet four: /std/Nat = twice(/std/Nat/add(1, 1));\n/std/print(\"\")";

/// A local answers where it is bound and its type, at each use and where it is bound alike: the type a use settled at, and the one its binder was opened at. A second read answers as the first does, at its own place.
#[test]
fn a_local_answers_where_it_is_bound_and_its_type() {
    let local = |nth| match at(LOCAL, "n", nth, 0).map(|answer| answer.stands) {
        Some(Stands::Local {
            declares,
            name,
            bound,
            type_,
            ..
        }) => (
            declares,
            name,
            bound.map(|bound| bound.line_column()),
            type_,
        ),
        stands => panic!("no local stands there: {stands:?}"),
    };
    let typed = |declares| {
        (
            declares,
            "n".to_string(),
            Some((1, 11)),
            Some("/std/Nat".to_string()),
        )
    };

    assert_eq!(local(0), typed(true));
    assert_eq!(local(1), typed(false));
    assert_eq!(local(2), typed(false));
}

const NAMED: &str = "let outer(given: /std/Nat) -> /std/Nat =\n    let inner(taken: /std/Nat) -> /std/Nat = taken;\n    inner(given);\n/std/print(\"\")";

/// A local's type is spelled with the names its own binders were written with: a local function read under a declaration whose type differs from its own by a parameter's name alone answers its own.
#[test]
fn a_locals_type_is_spelled_with_its_own_binders_names() {
    let read = match at(NAMED, "inner", 1, 0).map(|answer| answer.stands) {
        Some(Stands::Local { type_, .. }) => type_,
        stands => panic!("no local stands there: {stands:?}"),
    };

    assert_eq!(read, Some("(taken: /std/Nat) -> /std/Nat".to_string()));
}

const GROUP: &str = "let outer(given: /std/Nat) -> /std/Nat =\n    let left(k: /std/Nat) -> /std/Nat = right(k)\n    and right(m: /std/Nat) -> /std/Nat = m;\n    left(given);\n/std/print(\"\")";

/// A local group's member answers as a `let`'s binder does: where the group declares it, where a sibling reads it and where the block's tail reads it, each with where it is bound and the type it was opened at.
#[test]
fn a_local_groups_member_answers_as_a_lets_binder_does() {
    let local = |written, nth| match at(GROUP, written, nth, 0).map(|answer| answer.stands) {
        Some(Stands::Local {
            declares,
            bound,
            type_,
            ..
        }) => (declares, bound.map(|bound| bound.line_column()), type_),
        stands => panic!("no local stands at `{written}`: {stands:?}"),
    };
    let left = |declares| {
        (
            declares,
            Some((2, 9)),
            Some("(k: /std/Nat) -> /std/Nat".to_string()),
        )
    };
    let right = |declares| {
        (
            declares,
            Some((3, 9)),
            Some("(m: /std/Nat) -> /std/Nat".to_string()),
        )
    };

    assert_eq!(local("left", 0), left(true));
    assert_eq!(local("left", 1), left(false));
    assert_eq!(local("right", 0), right(false));
    assert_eq!(local("right", 1), right(true));
}

const TELESCOPE: &str = "induct Shape(A: Type): Type\n| circle(radius: A)\nend\nlet apply(f: (x: /std/Nat) -> /std/Nat) -> /std/Nat = f(1);\n/std/print(\"\")";

/// A telescope's binder answers where it is bound and where it is read, as a function's parameter does: a family's parameter, a constructor's payload binder and a function type's parameter.
#[test]
fn a_telescopes_binder_answers_where_it_is_bound_and_read() {
    let local = |written, nth| match at(TELESCOPE, written, nth, 0).map(|answer| answer.stands) {
        Some(Stands::Local {
            declares, bound, ..
        }) => (declares, bound.map(|bound| bound.line_column())),
        stands => panic!("no local stands at `{written}`: {stands:?}"),
    };

    assert_eq!(local("A", 0), (true, Some((1, 14))));
    assert_eq!(local("A", 1), (false, Some((1, 14))));
    assert_eq!(local("radius", 0), (true, Some((2, 10))));
    assert_eq!(local("x", 0), (true, Some((4, 15))));
}

const MEMBERS: &str = "struct Sized(n: /std/Nat): Type {\n    size: /std/Nat,\n    count: /std/Eq()(size, n),\n}\nconcept Shown(T: Type): Type {\n    shown: (value: T) -> /std/Nat,\n    again: (value: T) -> /std/Eq()(shown(value), shown(value)),\n}\nlet pairs: Type = {n: /std/Nat, p: /std/Eq()(n, n)};\n/std/print(\"\")";

/// A binder that declares a member answers as the member, where it is written and where a later member reads it: a structure's field as the field of its structure, a tuple type's field as a field of none, and a concept's method as the method.
#[test]
fn a_members_binder_answers_as_the_member() {
    let field = |written, within| match at(MEMBERS, written, 0, within).map(|answer| answer.stands)
    {
        Some(Stands::Field {
            label, of, bound, ..
        }) => (label, of, bound.map(|bound| bound.line_column())),
        stands => panic!("no field stands at `{written}`: {stands:?}"),
    };
    let sized = |line_column| {
        (
            "size".to_string(),
            Some("/Sized".to_string()),
            Some(line_column),
        )
    };
    let tupled = |line_column| ("n".to_string(), None, Some(line_column));

    assert_eq!(field("size: ", 0), sized((2, 5)));
    assert_eq!(field("(size, n)", 1), sized((2, 5)));
    assert_eq!(field("{n: ", 1), tupled((9, 20)));
    assert_eq!(field("(n, n)", 1), tupled((9, 20)));
    match at(MEMBERS, "Eq()(shown", 0, 5).map(|answer| answer.stands) {
        Some(Stands::Declaration {
            declares: false,
            described,
        }) => assert_eq!(
            (described.asked.as_str(), described.declared),
            ("/Shown/shown", Declared::Method)
        ),
        stands => panic!("no method stands at its later field's read: {stands:?}"),
    }
}

/// A term no name stands at answers the type it settled at, spelled where it is written: an application, where the offset stands between its arguments.
#[test]
fn a_term_answers_the_type_it_settled_at() {
    let applied = at(LOCAL, ", 1)", 0, 1).expect("a term stands there");

    assert_eq!(
        applied.stands,
        Stands::Term {
            written: "/std/Nat/add(1, 1)".to_string(),
            type_: Some("/std/Nat".to_string())
        }
    );
}

/// A declaration of the file's own that elaborated answers its type, spelled where it is written, in a file elaboration refused; and no totality, which the certifier never judged.
#[test]
fn a_declaration_of_the_files_own_that_elaborated_answers_its_type() {
    let three = declared_at(TYPED, "three", 0);

    assert_eq!(
        (three.type_.as_deref(), three.totality),
        (Some("/std/Nat"), None)
    );
}

/// A refused declaration answers what the lowering says of it and no type, and so does one reaching it, which elaboration withheld.
#[test]
fn a_refused_declaration_and_one_reaching_it_answer_the_lowerings_facts() {
    let one = declared_at(TYPED, "one", 0);
    let two = declared_at(TYPED, "two", 0);

    assert_eq!(
        (one.declared, one.type_, two.declared, two.type_),
        (Declared::Value, None, Declared::Value, None)
    );
}

/// Once the file checks, a declaration of its own answers the totality the certifier recorded beside its type.
#[test]
fn a_declaration_of_the_files_own_answers_its_totality_once_the_file_checks() {
    let checked = TYPED.replace("= true;", "= 1;");
    let one = declared_at(&checked, "one", 0);

    assert_eq!(
        (one.type_.as_deref(), one.totality),
        (Some("/std/Nat"), Some(Totality::Total))
    );
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
    let placed = asked(WITNESSED, &Overlay::default());
    let declared = placed
        .texts
        .iter()
        .flat_map(|text| &text.names)
        .filter(|named| named.declares)
        .map(|named| row(&placed.declarations[&named.named], true))
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

/// A declaration of the library a compiler carries is read and left unplaced: its sources hold their names and no file of the machine, whichever checkout the compiler was built in, and its head and prose are read off the text its unit holds.
#[test]
fn a_declaration_of_the_compilers_own_library_is_read_and_left_unplaced() {
    let placed = asked(LIBRARY_USE, &Overlay::default());
    let named = Named::at(placed.texts.iter().flat_map(|text| &text.names), 6)
        .expect("the program names the library's `print`");
    let declaration = placed.declarations[&named.named]
        .declaration
        .clone()
        .expect("the library's `print` is written");

    assert_eq!(declaration.place(), None);
    let label = &declaration.label;
    assert_eq!(&label.source.text()[label.start..label.end], "print");
    assert!(
        label
            .source
            .name()
            .is_some_and(|name| name.as_str().starts_with("/std/")),
        "{:?}",
        label.source.name()
    );

    let heading = declaration
        .heading()
        .expect("the declaration's text states it");
    assert!(
        heading.head.starts_with("pub let print("),
        "{}",
        heading.head
    );
    assert!(!heading.prose.is_empty());
}

/// A library is asked through one of its files, and answers as a program does.
#[test]
fn a_library_is_asked_through_one_of_its_files() {
    const LIBRARY: &str =
        "use /std/{Nat};\npub let answer: Nat = 42;\npub let again: Nat = answer;\n";
    let file = Path::new("answers.crs");
    let overlay = Overlay::of([(file.to_path_buf(), LIBRARY.to_string())]);
    let library = RootSource::supplied_module("answers", &Source::held("answers.crs", LIBRARY))
        .expect("the text is written as a module");

    let placed = crate::checked(
        DEFAULT_BUDGET,
        Subject::Unit {
            units: vec![library],
        },
        &overlay,
        None,
        file,
        &mut |_| {},
    )
    .placed
    .expect("the library lowers");
    let offset = LIBRARY.rfind("answer").expect("the library writes it");
    let answer = placed.at(offset).expect("a name stands at the use");

    assert_eq!(
        said(&answer),
        ("/answers/answer", Declared::Value, false, Some((2, 9)))
    );
}

/// What `path` names in the scope of `program`, asked the way the one-shot transport asks of standard input.
fn described(program: &str, path: &str) -> Option<Described> {
    crate::described(
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
        &Qualifier::from(path.trim_start_matches('/').split('/')),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .expect("the program checks")
}

/// What each witness `described` takes part in says of it, in the order it is answered.
fn keyed(described: &Described) -> Vec<&str> {
    described
        .witnesses
        .iter()
        .flatten()
        .map(|witnessed| witnessed.keyed.as_str())
        .collect()
}

const POINT: &str = "use /std/{Nat, Show, print};\nstruct Point: Type {\n    x: Nat,\n}\nsatisfy Show(Point) {\n    show(_) = \"point\",\n}\nprint(\"\")";

/// A definition of the standard library is answered off the unit that declares it: what it is, its type as that unit was elaborated, and its whole text, the prose above it included.
#[test]
fn a_library_definition_is_answered_with_its_type_and_text() {
    let described = described(POINT, "/std/Bool/not").expect("the library declares it");

    assert_eq!(
        (
            described.path.as_str(),
            described.declared,
            described.public,
            described.type_.as_deref(),
        ),
        (
            "/std/Bool/not",
            Declared::Function,
            Some(true),
            Some("(b: Bool) -> Bool")
        ),
    );
    assert_eq!(described.witnesses, None);
    assert_eq!(
        described.declaration.as_ref().map(Declaration::written),
        Some("--- The other value.\npub let not(b: Bool) -> Bool =\n    xor(b, true);"),
    );
}

/// A path a module exports a declaration at answers the declaration it exports: `/std` exports `/sys`'s `Option`, which is answered with its constructors and the concepts its witnesses satisfy.
#[test]
fn a_re_exported_path_answers_the_declaration_behind_it() {
    let described = described(POINT, "/std/Option").expect("the library exports it");

    assert_eq!(
        (
            described.asked.as_str(),
            described.path.as_str(),
            described.declared,
        ),
        ("/std/Option", "/sys/Option", Declared::Inductive),
    );
    assert_eq!(described.members, ["some", "none"]);
    assert_eq!(
        keyed(&described),
        [
            "/std/ops/Eql",
            "/std/Show",
            "/std/Spell",
            "/std/Monad",
            "/std/ops/Cmp",
            "/std/Ord",
            "/std/Hash"
        ]
    );
}

/// A concept is answered with its methods and the witnesses in scope, the program's own among them, each by what it is keyed on, written as a reader outside every module writes it: a carrier's head by its former's path as any other head by its own.
#[test]
fn a_concept_is_answered_with_its_witnesses_in_scope() {
    let described = described(POINT, "/std/Show").expect("the library declares it");

    assert_eq!(described.members, ["show"]);
    let point = described
        .witnesses
        .iter()
        .flatten()
        .next_back()
        .expect("the program's own witness is answered last");

    assert!(keyed(&described).contains(&"/std/Nat"));
    assert_eq!(
        (
            point.keyed.as_str(),
            point.place.as_ref().map(Span::line_column)
        ),
        ("/Point", Some((5, 9)))
    );
}

/// A declaration of the program's own is typed by the program's check, and one no module exports is answered by its own path.
#[test]
fn a_programs_own_private_declaration_is_answered_by_its_own_path() {
    let described = described(POINT, "/Point").expect("the program declares it");

    assert_eq!(
        (
            described.declared,
            described.public,
            described.type_.as_deref(),
            described.members.as_slice(),
            keyed(&described).as_slice(),
        ),
        (
            Declared::Struct,
            Some(false),
            Some("Type"),
            ["x".to_string()].as_slice(),
            ["/std/Show"].as_slice()
        ),
    );
}

/// A definition is answered with the totality the certifier recorded of it, off the unit that declares it: the program's own, a recursion that descends and one that does not, and the library's. A structure is answered with none, its former being no definition a reader wrote.
#[test]
fn a_definition_is_answered_with_its_totality() {
    let program = "use /std/{Nat, print};\nlet halve(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => match k | 0 => 0 | j + 1 => halve(j) + 1 end end;\nlet spin(n: Nat) -> Nat = spin(n + 1);\nprint(\"\")";
    let totality = |path| described(program, path).expect("in scope").totality;

    assert_eq!(totality("/halve"), Some(Totality::Total));
    assert_eq!(totality("/spin"), Some(Totality::Partial));
    assert_eq!(totality("/std/Bool/not"), Some(Totality::Total));
    assert_eq!(
        described(POINT, "/Point")
            .expect("the program declares it")
            .totality,
        None
    );
}

/// A path that names nothing in scope answers nothing.
#[test]
fn a_path_that_names_nothing_answers_nothing() {
    assert_eq!(described(POINT, "/std/Bool/nope"), None);
}

/// A place answers the declaration its name names as a path does, its type spelled where the name is written: a use of `/std/Bool/not` answers the library's definition, which the library's check typed and judged total.
#[test]
fn a_place_answers_the_declaration_its_name_names() {
    const PROGRAM: &str =
        "use /std/{Bool, print};\nlet flipped: Bool = Bool/not(true);\nprint(\"\")";
    let offset = PROGRAM.find("not").expect("the program writes it");
    let column = offset - PROGRAM[..offset].rfind('\n').expect("on the second line");
    let answer = crate::placed(
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
        (2, column),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .expect("the program lowers")
    .expect("a name stands there");
    let Stands::Declaration { described, .. } = answer.stands else {
        panic!("a name stands there");
    };

    assert_eq!(
        (
            described.path.as_str(),
            described.declared,
            described.public,
            described.type_.as_deref(),
            described.totality,
            described.witnesses,
        ),
        (
            "/std/Bool/not",
            Declared::Function,
            Some(true),
            Some("(b: Bool) -> Bool"),
            Some(Totality::Total),
            None
        ),
    );
}

/// A file one declaration of which is refused answers a dependency's name with what the dependency's check established, its type and its totality, beside the path it is exposed at and the one it is declared at.
#[test]
fn a_dependency_named_in_a_refused_file_answers_its_type_and_totality() {
    const PROGRAM: &str = "use /std/{List, Nat, print};\nlet doubled(xs: List(Nat)) -> List(Nat) = List/map(xs, (x) => x + x);\nlet broken: Nat = true;\nprint(\"\")";
    let mapped = at(PROGRAM, "List/map", 0, 5).expect("a name stands there");
    let Stands::Declaration { described, .. } = mapped.stands else {
        panic!("a name stands there");
    };

    assert_eq!(
        (
            described.asked.as_str(),
            described.path.as_str(),
            described.type_.as_deref(),
            described.totality,
        ),
        (
            "/std/List/map",
            "/sys/List/map",
            Some("(@A: Sort, @B: Sort, a: List(A), f: (A) -> B) -> List(B)"),
            Some(Totality::Total)
        ),
    );
}

/// The path `path` spells.
fn qualified(path: &str) -> Qualifier {
    Qualifier::from(path.trim_start_matches('/').split('/'))
}

/// What a consumer reaches through the module `path` names, asked in the prelude's scope alone, as a question with no subject is outside every package.
fn exported(path: &str) -> Option<ModuleDocumentation> {
    exposed(path).map(|exported| exported.module)
}

/// What `--exports` answers of the module `path` names in the prelude's scope alone: its record, and what each name it lists is declared as.
fn exposed(path: &str) -> Option<crate::Exported> {
    crate::exported(
        DEFAULT_BUDGET,
        Subject::prelude(),
        &qualified(path),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .expect("the prelude documents itself")
}

/// A name a module exposes is said to be declared as `--name` says it, read off the lowering: a constructor a `pub use` brings out of its family is a constructor and a concept's method a method, which the documentation record, keyed by the keyword a page writes, tells from no definition.
#[test]
fn a_name_a_module_exposes_is_declared_as_its_declaration_says() {
    let declared = |module: &str, name: &str| {
        let exposed = exposed(module).unwrap_or_else(|| panic!("{module} is exposed"));
        exposed
            .module
            .declarations
            .iter()
            .zip(exposed.declared)
            .find(|(declaration, _)| declaration.name == name)
            .map(|(_, declared)| declared)
            .unwrap_or_else(|| panic!("{module} exposes {name}"))
    };

    assert_eq!(declared("/std/Option", "some"), Declared::Constructor);
    assert_eq!(declared("/std/Option", "Option"), Declared::Inductive);
    assert_eq!(declared("/std/Option", "map"), Declared::Function);
    assert_eq!(declared("/std/Show", "show"), Declared::Method);
}

/// In the prelude's scope alone a path answers the standard library's declaration, and one no prelude unit holds answers nothing.
#[test]
fn the_prelude_alone_answers_the_standard_librarys_declarations_and_nothing_else() {
    let described = |path| {
        crate::described(
            DEFAULT_BUDGET,
            Subject::prelude(),
            &qualified(path),
            &Overlay::default(),
            None,
            &mut |_| {},
        )
        .unwrap_or_else(|_| panic!("the prelude's scope checks"))
    };
    let map = described("/std/List/map").expect("the standard library declares it");

    assert_eq!(
        (map.path.as_str(), map.declared),
        ("/sys/List/map", Declared::Function)
    );
    assert!(described("/app/main").is_none());
}

/// Every module of the standard library a consumer reaches is reached from its root by `--exports` alone, each child row's module answered in turn, and the modules so reached are the ones its documentation pages are: the listing and the pages are one reading of one record.
#[test]
fn every_module_of_the_standard_library_is_reached_from_its_root() {
    let std = Qualifier::from(["std"]);
    let record = curios_pipeline::prelude_documentation()
        .into_iter()
        .find(|record| record.prefix == std)
        .expect("the prelude documents /std");
    let mut reached = BTreeSet::new();
    let mut pending = vec![std];
    while let Some(path) = pending.pop() {
        let module = exported(&path.join()).unwrap_or_else(|| panic!("{} answers", path.join()));
        pending.extend(module.children.iter().copied());
        reached.insert(path);
    }

    let paged = record
        .modules
        .iter()
        .map(|module| module.path)
        .collect::<BTreeSet<_>>();
    assert_eq!(reached, paged);
}

/// A facade's path names a declaration and a module: `--name` answers the declaration, the `Map` structure, and `--exports` the module, among whose names is `insert`; a private module answers nothing, as a consumer reaches none.
#[test]
fn a_facade_answers_its_declaration_and_its_module_and_a_private_module_nothing() {
    let declaration = described("/std/print(\"x\")", "/std/Map").expect("the library declares it");
    assert_eq!(declaration.declared, Declared::Struct);

    let module = exported("/std/Map").expect("the library exposes the module");
    assert!(
        module
            .declarations
            .iter()
            .any(|declaration| declaration.name == "insert"),
        "{:?}",
        module
            .declarations
            .iter()
            .map(|declaration| &declaration.name)
            .collect::<Vec<_>>()
    );
    assert!(exported("/std/Map/internal").is_none());
}

/// A program writing a word in each place elaboration resolves one: a structure's field labels in a literal, a projection and a pattern, a concept's method in a witness, and a constructor in a match.
const LABELLED: &str = "\
use /std/{Nat, Show, print};
struct Pair(Left: Type): Type {
    first: Left,
    second: Nat,
}
induct Shade: Type
| light()
| dark(Nat)
end
satisfy Show(Shade) {
    show(_) = \"shade\",
}
let made: Pair(Nat) = Pair { first = 1, second = 2 };
let read(held: Pair(Nat)) -> Nat = held.second;
let opened(held: Pair(Nat)) -> Nat =
    let Pair { first = inner, second = _ } = held;
    inner;
let toned(shade: Shade) -> Nat =
    match shade
    | light() => 0
    | dark(depth) => depth
    end;
print(\"\")";

/// What a field's label answers ([`Stands::Field`]), where the structure writes it held as its line and column.
#[derive(Debug, PartialEq)]
struct Fielded {
    label: String,
    of: Option<String>,
    bound: Option<(usize, usize)>,
    type_: Option<String>,
}

/// What the field's label written at `written`, `within` bytes into it, answers.
fn field(written: &str, within: usize) -> Fielded {
    match at(LABELLED, written, 0, within).map(|answer| answer.stands) {
        Some(Stands::Field {
            label,
            of,
            bound,
            type_,
            ..
        }) => Fielded {
            label,
            of,
            bound: bound.map(|bound| bound.line_column()),
            type_,
        },
        stands => panic!("no field stands at `{written}`: {stands:?}"),
    }
}

/// A structure's field answers itself wherever its label is written — in a literal, a projection and a pattern — with where the structure writes it and its type as the structure declares it.
#[test]
fn a_fields_label_answers_the_field() {
    let declared = |label: &str, line, type_: &str| Fielded {
        label: label.to_string(),
        of: Some("/Pair".to_string()),
        bound: Some((line, 5)),
        type_: Some(type_.to_string()),
    };
    let (first, second) = (declared("first", 3, "Left"), declared("second", 4, "Nat"));

    assert_eq!(field("first = 1", 0), first);
    assert_eq!(field("second = 2", 0), second);
    assert_eq!(field(".second", 1), second);
    assert_eq!(field("first = inner", 0), first);
}

/// A word elaboration resolves to a declaration answers the declaration: a constructor a pattern names, and a method a witness defines.
#[test]
fn a_constructor_and_a_method_elaboration_resolves_answer_their_declarations() {
    let dark = at(LABELLED, "dark(depth)", 0, 0).expect("a name stands there");
    let show = at(LABELLED, "show(_)", 0, 0).expect("a name stands there");

    assert_eq!(
        said(&dark),
        ("/Shade/dark", Declared::Constructor, false, Some((8, 3)))
    );
    assert_eq!(said(&show).1, Declared::Method);
    assert!(said(&show).0.ends_with("/show"), "{:?}", said(&show));
}

/// A literal's head and a structure pattern's, which the lowering resolves into the node, answer the structure they name.
#[test]
fn a_heads_structure_is_answered() {
    let literal = at(LABELLED, "Pair {", 0, 0).expect("a name stands there");
    let pattern = at(LABELLED, "Pair {", 1, 0).expect("a name stands there");

    assert_eq!(
        said(&literal),
        ("/Pair", Declared::Struct, false, Some((2, 8)))
    );
    assert_eq!(said(&pattern), said(&literal));
}

/// A field of a structure a unit in scope declares answers where that unit writes it, read off the unit's own table of where its definitions are named.
#[test]
fn a_field_of_a_structure_in_scope_answers_where_its_unit_writes_it() {
    const SHAPES: &str = "use /std/{Nat};\npub struct Pair: pub Type {\n    first: Nat,\n}\n";
    const READS: &str =
        "use /std/{Nat};\nuse /shapes/{Pair};\npub let read(held: Pair) -> Nat = held.first;\n";
    let library = |mount, file, text| {
        RootSource::supplied_module(mount, &Source::held(file, text))
            .expect("the text is written as a module")
    };
    let file = Path::new("reads.crs");
    let overlay = Overlay::of([
        (PathBuf::from("shapes.crs"), SHAPES.to_string()),
        (file.to_path_buf(), READS.to_string()),
    ]);

    let placed = crate::checked(
        DEFAULT_BUDGET,
        Subject::Unit {
            units: vec![
                library("shapes", "shapes.crs", SHAPES),
                library("reads", "reads.crs", READS),
            ],
        },
        &overlay,
        None,
        file,
        &mut |_| {},
    )
    .placed
    .expect("the library lowers");
    let offset = READS.rfind("first").expect("the library writes it");

    match placed.at(offset).map(|answer| answer.stands) {
        Some(Stands::Field {
            label,
            of,
            bound: Some(bound),
            ..
        }) => {
            assert_eq!(
                (label.as_str(), of.as_deref()),
                ("first", Some("/shapes/Pair"))
            );
            assert_eq!((bound.source.text(), bound.line_column()), (SHAPES, (3, 5)));
        }
        stands => panic!("no placed field stands at the projection: {stands:?}"),
    }
}

/// A segment of a path answers what it reaches: a module a `mod` line declares, as a module placed at that line, and a family's namespace, as the family.
#[test]
fn a_paths_segment_answers_the_module_or_the_family_it_reaches() {
    const PROGRAM: &str = "\
pub mod Shapes
    pub induct Colour: pub Type
    | red()
    end
end
let picked: /Shapes/Colour = /Shapes/Colour/red();
/std/print(\"\")";
    let module = at(PROGRAM, "Shapes/Colour/red", 0, 0).expect("a module stands there");
    let family = at(PROGRAM, "Colour/red", 0, 0).expect("a name stands there");

    assert_eq!(moduled(module), ("/Shapes".to_string(), Some((1, 9))));
    assert_eq!(
        said(&family),
        ("/Shapes/Colour", Declared::Inductive, false, Some((2, 16)))
    );
}

/// A program writing a module's `mod` line, and a `use` line through it.
const LINES: &str = "\
pub mod Shapes
    pub induct Colour: pub Type
    | red()
    end
end
use /Shapes/{Colour};
let picked: Colour = Colour/red();
/std/print(\"\")";

/// What a module answers: its path, and the line and column its `mod` line names it at.
fn moduled(answer: Answer) -> (String, Option<(usize, usize)>) {
    match answer.stands {
        Stands::Module { path, place } => (path, place.map(|place| place.line_column())),
        stands => panic!("no module stands there: {stands:?}"),
    }
}

/// A `use` line answers what it names, and a `mod` line the module it declares: a segment its module, placed at its `mod` line, a selector the declaration it imports.
#[test]
fn a_use_and_a_mod_lines_names_answer_what_they_name() {
    let shapes = ("/Shapes".to_string(), Some((1, 9)));
    let segment = at(LINES, "Shapes/{", 0, 0).expect("a module stands there");
    let declared = at(LINES, "Shapes\n", 0, 0).expect("a module stands there");
    let selector = at(LINES, "Colour}", 0, 0).expect("a name stands there");

    assert_eq!(moduled(segment), shapes);
    assert_eq!(moduled(declared), shapes);
    assert_eq!(
        said(&selector),
        ("/Shapes/Colour", Declared::Inductive, false, Some((2, 16)))
    );
}
