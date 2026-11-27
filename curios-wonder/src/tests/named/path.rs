//! What `wonder --name` and `wonder --witnesses` answer of the declaration or the module a path names: the file's own, a library's, the prelude's, and what a module exposes.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::{Qualifier, Source, Span},
    curios_core::{Declared, Global, Totality},
    curios_elab::DEFAULT_BUDGET,
    curios_module::{Overlay, RootSource},
    curios_surface::WrittenIn,
    std::{collections::BTreeSet, path::Path},
};

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
        &Global::Authored(Qualifier::from(path.trim_start_matches('/').split('/'))),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .expect("the program checks")
}

const LIBRARY_USE: &str = "/std/print(\"\")";

/// What each witness `described` takes part in says of it, in the order it is answered.
fn keyed(described: &Described) -> Vec<&str> {
    described
        .witnesses
        .iter()
        .flatten()
        .map(|witnessed| witnessed.keyed.as_str())
        .collect()
}

const POINT: &str = "use /std/{Nat, Show, print};\nstruct Point: Type {\n    x: Nat,\n}\nsatisfy Show(Point) {\n    show(_) => \"point\",\n}\nprint(\"\")";

/// The path `path` spells.
fn qualified(path: &str) -> Qualifier {
    Qualifier::from(path.trim_start_matches('/').split('/'))
}

/// What `--name` answers of the module `path` names in the prelude's scope alone, as a question with no subject is outside every package.
fn exposed(path: &str) -> Option<Exported> {
    crate::referent(
        DEFAULT_BUDGET,
        Subject::prelude(),
        &Global::Authored(qualified(path)),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .unwrap_or_else(|_| panic!("the prelude's scope checks"))
    .ok()
    .and_then(|named| named.module)
}

/// What `--name` answers of the module `path` names in the scope of `program`, read of standard input.
fn exposed_in(program: &str, path: &str) -> Option<Exported> {
    named_in(program, path).and_then(|named| named.module)
}

/// A program whose module exposes a type the compiler writes, a selector of a `use` line, a definition, a family and, by a glob, the family's constructor, beside a definition it keeps, a public child and a private one.
const MODULES: &str = "\
mod helpers
    use /std/{Nat};
    pub use /std/{Bool};
    pub use /std/Option/{some};
    pub let twice(n: Nat) -> Nat => n + n;
    let hidden: Nat = 3;
    pub induct Colour: pub Type
    | red()
    end
    pub use Colour/*;
    pub mod deeper
        pub let one: /std/Nat = 1;
    end
    mod kept
        pub let zero: /std/Nat = 0;
    end
end
/std/print(\"\")";

/// What `--name` answers of `path` in the scope of `text`, read of standard input as `curios wonder -` admits it: a program, or a module of its own.
fn named_in(text: &str, path: &str) -> Option<Referent> {
    crate::referent(
        DEFAULT_BUDGET,
        stdin_subject(text, WrittenIn::Program),
        &Global::Authored(qualified(path)),
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .unwrap_or_else(|_| panic!("the program checks"))
    .ok()
}

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
        Some("--- The other value.\npub let not(b: Bool) -> Bool =>\n    xor(b, true);"),
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
    assert_eq!(described.members, ["none", "some"]);
    assert_eq!(
        keyed(&described),
        [
            "/std/ops/Eql",
            "/std/Show",
            "/std/Spell",
            "/std/Monad",
            "/std/ops/Ord",
            "/std/Hash",
            "/std/Hash/Key"
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
        (point.keyed.as_str(), point.place().map(Span::line_column)),
        ("/Point", Some((5, 9)))
    );
    // A witness is listed by its head as its author wrote it, read off its own text.
    assert_eq!(point.head(), "satisfy Show(Point)");
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
    let program = "use /std/{Nat, print};\nlet halve(n: Nat) -> Nat => match n | 0 => 0 | k + 1 => match k | 0 => 0 | j + 1 => halve(j) + 1 end end;\nlet spin(n: Nat) -> Nat => spin(n + 1);\nprint(\"\")";
    let totality = |path| described(program, path).expect("in scope").totality;

    assert_eq!(totality("/halve"), Some(Totality::Total));
    assert_eq!(totality("/spin"), Some(Totality::Unfounded));
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

/// A file one declaration of which is refused answers a dependency's name with what the dependency's check established, its type and its totality, beside the path it is exposed at and the one it is declared at.
#[test]
fn a_dependency_named_in_a_refused_file_answers_its_type_and_totality() {
    const PROGRAM: &str = "use /std/{List, Nat, print};\nlet doubled(xs: List(Nat)) -> List(Nat) => List/map(xs, (x) => x + x);\nlet broken: Nat = true;\nprint(\"\")";
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

/// A member a module exposes beside its owner is the line that does it: a family's constructors, and a concept's method. The owner stands at the module's own path, where it is answered before the module, so it is no entry of the module's and the line opens the listing.
#[test]
fn a_member_a_module_exposes_beside_its_owner_is_the_line_that_does_it() {
    let option = exposed("/std/Option").expect("the library exposes the module");
    assert_eq!(option.entries[0], "pub use Option/{none, some}");
    assert!(
        option
            .entries
            .iter()
            .all(|entry| !entry.starts_with("pub induct Option(")),
        "{:?}",
        option.entries
    );

    let show = exposed("/std/Show").expect("the library exposes the module");
    assert_eq!(show.entries, ["pub use Show/{show}"]);
    assert_eq!((show.names, show.modules), (1, 0));
}

/// In the prelude's scope alone a path answers the standard library's declaration, and one no prelude unit holds answers nothing — `/stdin` among them, the scope holding no text of standard input to mount there.
#[test]
fn the_prelude_alone_answers_the_standard_librarys_declarations_and_nothing_else() {
    let described = |path| {
        crate::described(
            DEFAULT_BUDGET,
            Subject::prelude(),
            &Global::Authored(qualified(path)),
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
    assert!(exposed("/stdin").is_none());
}

/// Every module of the standard library a consumer reaches is reached from its root by its entries alone, each child's `mod` line answered in turn, and the modules so reached are the ones its documentation pages are: the listing and the pages are one reading of the record.
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
        let module = exposed(&path.join()).unwrap_or_else(|| panic!("{} answers", path.join()));
        pending.extend(
            module
                .entries
                .iter()
                .filter_map(|entry| entry.strip_prefix("pub mod "))
                // The line says what the child exposes after its name.
                .map(|child| path.with(child.split(',').next().unwrap_or(child))),
        );
        reached.insert(path);
    }

    let paged = record
        .modules
        .iter()
        .map(|module| module.path)
        .collect::<BTreeSet<_>>();
    assert_eq!(reached, paged);
}

/// A facade's path names a declaration and a module: `--name` answers the declaration, the `Map` structure, and the module is listed as its documentation page lists it — `insert` among its names, its witnesses after them, which only a page lists, and its public children last.
#[test]
fn a_facade_answers_its_declaration_and_its_module() {
    let declaration = described("/std/print(\"x\")", "/std/Map").expect("the library declares it");
    assert_eq!(declaration.declared, Declared::Struct);

    let module = exposed("/std/Map").expect("the library exposes the module");
    let at = |opening: &str| {
        module
            .entries
            .iter()
            .position(|entry| entry.starts_with(opening))
            .unwrap_or_else(|| panic!("no entry opens `{opening}`: {:?}", module.entries))
    };
    assert_eq!(module.public, Some(true));
    assert!(at("pub let insert(") < at("satisfy "));
    assert!(at("satisfy ") < at("pub mod try_get"));
    assert_eq!(module.modules, 4);
}

/// A module no documentation page shows is listed off the lowering: `Map`'s `internal`, written without `pub`, answers that it is private, the family its `pub use` exposes, written whole from its own text, and its public child; and the child, written `pub` beneath a private module, lists its own names in the order its body writes them and the constructors its glob exposed after them, by name. The library the compiler was built with holds no file, so neither is placed.
#[test]
fn a_module_no_page_shows_is_listed_off_the_lowering() {
    let internal = exposed("/std/Map/internal").expect("the library declares the module");
    assert_eq!(
        internal,
        Exported {
            public: Some(false),
            place: None,
            names: 1,
            modules: 1,
            entries: vec![
                "pub induct Node(K: Type, V: Type): pub Type\n| empty()\n| leaf(key: K, value: V)\n| branch(at: Nat, twigs: Vec(Node(K, V), 16))\nend".to_string(),
                internal.entries[1].clone(),
            ],
        }
    );
    // The child is the line that declares it with what it exposes, which is more than the family its parent exposes under its name.
    assert!(
        internal.entries[1].starts_with("pub mod Node, exposing ")
            && internal.entries[1].ends_with(" more names"),
        "{:?}",
        internal.entries
    );

    let node = exposed("/std/Map/internal/Node").expect("the library declares the module");
    assert_eq!(
        (node.public, node.place, node.modules),
        (Some(true), None, 0)
    );
    // The library exports nothing at the child's path, its parent being private, so no declaration is answered before the module and its listing opens on the family.
    assert_eq!(node.entries[0], internal.entries[0]);
    assert!(
        node.entries[1].starts_with("pub let first_in("),
        "{:?}",
        node.entries
    );
    assert!(
        node.entries
            .contains(&"pub use Node/{branch, empty, leaf}".to_string()),
        "{:?}",
        node.entries
    );
}

/// A program's own module is listed off its lowering, placed at its `mod` line: what it exposes in the order its body writes it — a declaration no text is held of by the line that exposes it, a member of an owner it does not expose by the owner's path, a definition by its head, a family whole — then what a glob exposed, a member of an owner it exposes by the owner's name, then its public child, and nothing it keeps.
#[test]
fn a_programs_own_module_lists_what_its_body_exposes_in_the_order_it_writes_it() {
    let helpers = exposed_in(MODULES, "/helpers").expect("the program declares the module");

    assert_eq!(
        (
            helpers.public,
            helpers.place.map(|place| place.line_column()),
            helpers.names,
            helpers.modules,
        ),
        (Some(false), Some((1, 5)), 5, 1)
    );
    assert_eq!(
        helpers.entries,
        [
            "pub use /std/{Bool}",
            "pub use /std/Option/{some}",
            "pub let twice(n: Nat) -> Nat",
            "pub induct Colour: pub Type\n| red()\nend",
            "pub use Colour/{red}",
            "pub mod deeper, exposing 1 name",
        ]
    );
}

/// A path names a module where a `mod` line declares one: a child written `pub` answers as public, and a family's namespace, a declaration and a path nothing stands at answer no module.
#[test]
fn a_path_names_a_module_only_where_a_mod_line_declares_one() {
    let deeper = exposed_in(MODULES, "/helpers/deeper").expect("the program declares the module");

    assert_eq!(
        (deeper.public, deeper.entries),
        (Some(true), vec!["pub let one: /std/Nat".to_string()])
    );
    assert_eq!(exposed_in(MODULES, "/helpers/Colour"), None);
    assert_eq!(exposed_in(MODULES, "/helpers/twice"), None);
    assert_eq!(exposed_in(MODULES, "/nowhere"), None);
}

/// A path is answered wherever it names something: a module of functions as the module, a declaration as the declaration, a facade's path as both, a private module and a program's own as modules, and a path nothing stands at as nothing.
#[test]
fn a_path_answers_the_declaration_and_the_module_it_names() {
    const PROGRAM: &str = "mod helpers\n    pub let one: /std/Nat = 1;\nend\n/std/print(\"\")";
    let named = |path| {
        named_in(PROGRAM, path).map(|named| {
            (
                named.declaration.map(|described| described.declared),
                named.module.map(|module| module.names),
            )
        })
    };

    assert_eq!(named("/std/ops"), Some((None, Some(9))));
    assert_eq!(named("/std/print"), Some((Some(Declared::Function), None)));
    assert_eq!(
        named("/std/Map").map(|(declared, module)| (declared, module.is_some())),
        Some((Some(Declared::Struct), true))
    );
    assert_eq!(named("/std/Map/internal"), Some((None, Some(1))));
    assert_eq!(named("/helpers"), Some((None, Some(1))));
    assert_eq!(named("/helpers/one"), Some((Some(Declared::Value), None)));
    assert_eq!(named("/nowhere"), None);
}

/// A loose text written as a module is a unit of its own, mounted under the name it is asked by, and its own path answers the module: read off the lowering, since a loose module documents nothing.
#[test]
fn a_loose_modules_own_path_answers_the_module() {
    let module = named_in(
        "pub let twice(n: /std/Nat) -> /std/Nat => n + n;\n",
        "/stdin",
    )
    .and_then(|named| named.module)
    .expect("the text is a module");

    assert_eq!(
        (module.public, module.entries),
        (
            None,
            vec!["pub let twice(n: /std/Nat) -> /std/Nat".to_string()]
        )
    );
}
