//! A package's documentation record, built by the lowering and carried on the unit: what `curios document` renders, checked over the standard library — the largest interface in the tree, read off the image it ships in — and over a fixture package whose every rule of the record is written out.

use {
    curios_document::Kind,
    curios_package::{Governing, order},
    curios_pipeline::{CompileError, DEFAULT_STEP_BUDGET, with_units},
    curios_text::Overlay,
    curios_utilities::Qualifier,
    curios_wonder::documentation,
    std::{
        fs,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// The standard library's record, off the image the compiler was built with.
///
/// **The record of `/std`, named by its prefix rather than taken as the first one found.** Both prelude roots carry one — `/sys` documents itself so that `/std` has something to adopt its intrinsic declarations out of — and the fold puts `/sys` first, so a search for "the" record finds the wrong half.
fn standard_library() -> curios_document::Documentation {
    with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        None,
        |_| {},
        |prelude, _| {
            prelude
                .iter()
                .filter_map(|root| root.text().documentation())
                .find(|record| record.prefix == Qualifier::from(["std"]))
                .cloned()
                .ok_or_else(|| CompileError::failure("the image carries no record".to_string()))
        },
    )
    .expect("the standard library documents")
}

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    root
}

/// The standard library's record rides in the image it ships in: no sources, no checkout, no second compilation — the build that made the image built the record. It is the honest test of the record, holding opaque types, derived witnesses, concepts and re-exports.
#[test]
fn the_standard_library_documents_from_the_archive() {
    let documentation = standard_library();

    assert!(
        documentation.modules.len() > 50,
        "only {} modules",
        documentation.modules.len()
    );
    assert_eq!(documentation.modules[0].path.join(), "/std");
    assert!(
        documentation
            .description
            .as_deref()
            .is_some_and(|description| description.starts_with("The standard library")),
        "{:?}",
        documentation.description
    );

    let result = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Result")
        .expect("a Result module");
    let induct = result
        .declarations
        .iter()
        .find(|declaration| declaration.name == "Result")
        .expect("the Result type");
    assert_eq!(induct.kind, Kind::Inductive);
    assert!(
        induct
            .prose
            .as_ref()
            .is_some_and(|lines| lines[0].starts_with("Success or failure")),
        "the type's prose is the `-- |` block written above it: {:?}",
        induct.prose
    );
    assert_eq!(
        induct
            .members
            .iter()
            .map(|member| member.name.as_str())
            .collect::<Vec<_>>(),
        ["success", "failure"],
        "a public representation lists its constructors"
    );
    assert!(!induct.opaque);

    // `pub struct Map(V: Type): Type` exports its name and not its fields, and the record says so rather than leaving an empty member list to be read either way.
    let map = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Map")
        .and_then(|module| {
            module
                .declarations
                .iter()
                .find(|declaration| declaration.name == "Map")
        })
        .expect("the Map type");
    assert_eq!(map.kind, Kind::Structure);
    assert!(map.opaque && map.members.is_empty(), "{map:?}");

    // A concept's superclass edge is an anonymous field in the language and an anonymous member in the record, in its written place among the methods.
    let ordered = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Ordered")
        .and_then(|module| {
            module
                .declarations
                .iter()
                .find(|declaration| declaration.name == "Ordered")
        })
        .expect("the Ordered concept");
    assert_eq!(ordered.kind, Kind::Concept);
    assert_eq!(
        ordered
            .members
            .iter()
            .map(|member| member.name.as_str())
            .collect::<Vec<_>>(),
        ["", "cmp"],
        "{:?}",
        ordered.members
    );
    assert!(
        ordered.members[0].signature.text.starts_with("use "),
        "{:?}",
        ordered.members[0].signature
    );

    // A signature's reference to a declaration of the same unit links within the bundle.
    let pure = result
        .declarations
        .iter()
        .find(|declaration| declaration.name == "pure")
        .expect("Result/pure");
    assert!(
        pure.signature
            .marks
            .iter()
            .any(|mark| mark.referent.join() == "/std/Result/Result" && mark.within),
        "{:?}",
        pure.signature
    );

    // `pub use Result/*` puts the constructors in the module beside the type, which is where a consumer reaches them: `Result/success` and `Result` are siblings, so the page lists both. The constructor stays a member of its type as well, since that is where its shape belongs.
    let declared = result
        .declarations
        .iter()
        .find(|declaration| declaration.name == "Result")
        .expect("Result");
    assert!(
        declared
            .members
            .iter()
            .any(|member| member.name == "success"),
        "{:?}",
        declared.members
    );

    let constructor = result
        .declarations
        .iter()
        .find(|declaration| declaration.name == "success")
        .expect("the constructor beside its type");
    assert_eq!(constructor.home.join(), "/std/Result");
    assert_eq!(
        constructor.source.as_ref().map(Qualifier::join).as_deref(),
        Some("/std/Result/Result"),
        "a member's card names the declaration that holds it, and links to the row inside it"
    );
    assert!(
        constructor.signature.text.starts_with("success("),
        "{:?}",
        constructor.signature
    );
}

/// **A declaration that is not a `let` resolves the names in its signature too.**
///
/// The import scope a page resolves through is recorded per declaration by the lowering, and it was recorded for a `let` and a `test` and nothing else — so a concept, an inductive, a structure, a witness or a foreign had none, and `imports_of` fell back to the union of its module's `let`s. A module with no `let` therefore resolved nothing at all: `/std/Show` is a `use` line and a concept, and its one method's return type rendered as plain text while `/std/Spell`'s identical one linked, because `Spell` happens to have renderer `let`s that import the same name.
///
/// `/std/Show` is the whole shape of the bug in one module, which is why it is the subject. Asserted on a *member*, since no other test in this tree asserts on a member's marks at all.
#[test]
fn a_concept_method_resolves_the_names_in_its_signature() {
    let documentation = standard_library();

    let page = documentation
        .modules
        .iter()
        .find(|page| page.path == Qualifier::from(["std", "Show"]))
        .expect("/std/Show has a page");
    let show = page
        .declarations
        .iter()
        .find(|declaration| declaration.name == "Show")
        .expect("/std/Show declares the concept Show");
    let method = show
        .members
        .iter()
        .find(|member| member.name == "show")
        .expect("the concept Show declares the method show");

    let referents = method
        .signature
        .marks
        .iter()
        .map(|mark| {
            (
                &method.signature.text[mark.start..mark.end],
                mark.referent.join(),
                mark.within,
            )
        })
        .collect::<Vec<_>>();

    assert_eq!(
        referents,
        [("Str", "/std/Str/Str".to_string(), true)],
        "the concept's own parameter is a binder and stays plain, and its return type links to the page that shows it — got {:?}",
        method.signature.text
    );
}

/// **The adopted half of that claim: the declarations are there.** Hiding `/sys` is half a property, and the sibling below pins only the half that is an absence — which a bundle satisfies perfectly by dropping every adopted declaration on the floor. That is exactly what splitting the prelude into two units did: the surface tree a declaration was rendered from left with the unit, and 112 names across 23 re-exports vanished from the record without a single check going red.
///
/// Read off the record rather than the rendered pages, because what is at issue is whether the declaration exists at all — and asserted per carrier, since one surviving name would satisfy any count.
#[test]
fn every_intrinsic_carrier_reaches_a_page_through_its_std_module() {
    let documentation = standard_library();

    // One carrier per `/sys` type former the standard library re-exports, at the module a program reaches it through.
    let carriers = [
        ("std/Nat", "Nat"),
        ("std/Byte", "Byte"),
        ("std/Int", "Int"),
        ("std/Flt", "Flt"),
        ("std/Bool", "Bool"),
        ("std/Handle", "Handle"),
        ("std/List", "List"),
        ("std/Cell", "Cell"),
        ("std/Io", "Io"),
    ];

    let mut missing = Vec::new();
    for (module, label) in carriers {
        let path = Qualifier::from(module.split('/'));
        let found = documentation
            .modules
            .iter()
            .find(|page| page.path == path)
            .and_then(|page| {
                page.declarations
                    .iter()
                    .find(|declaration| declaration.name == label)
            });

        match found {
            // Adopted out of a root with no page here, so the card says what that root holds and names no path.
            Some(declaration) => {
                if declaration.chip.as_deref() != Some("intrinsic") {
                    missing.push(format!(
                        "/{module}/{label} is shown but chipped {:?}, not \"intrinsic\"",
                        declaration.chip
                    ));
                }
            }
            None => missing.push(format!("/{module}/{label} reaches no page at all")),
        }
    }

    assert!(
        missing.is_empty(),
        "{} of {} intrinsic carriers do not reach a page:\n{}",
        missing.len(),
        carriers.len(),
        missing.join("\n")
    );
}

/// **No page names a root a consumer may not write.** `/sys` is adopted rather than linked: its declarations appear under the `/std` module that exposes them, and their own paths appear nowhere a reader can see — not in a signature, a card header, a search row or a crumb.
///
/// Rendered and read back rather than checked against the record, because the record is only half the claim. A path can reach a reader through a template as easily as through a field, and the file is the thing a reader opens.
#[test]
fn no_internal_root_reaches_a_rendered_page() {
    let documentation = standard_library();

    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let directory = std::env::temp_dir().join(format!(
        "curios-document-internal-{}-{millis}",
        std::process::id()
    ));
    curios_document::write_documentation(&documentation, &directory).expect("the pages render");

    let mut offenders = Vec::new();
    let mut pending = vec![directory.clone()];
    while let Some(path) = pending.pop() {
        for entry in fs::read_dir(&path).expect("a written directory") {
            let entry = entry.expect("a written entry").path();
            if entry.is_dir() {
                pending.push(entry);
                continue;
            }
            // The fonts and the mark are bytes, and their licenses name no module: what this claim is about is everything a page is rendered from.
            let text = match entry.extension().and_then(|extension| extension.to_str()) {
                Some("html" | "js" | "css") => fs::read_to_string(&entry).expect("written text"),
                _ => continue,
            };

            for (line, text) in text.lines().enumerate() {
                if text.contains("/sys/") {
                    let name = entry.strip_prefix(&directory).unwrap_or(&entry);
                    offenders.push(format!("{}:{}: {}", name.display(), line + 1, text.trim()));
                }
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "an internal root reached {} rendered line(s):\n{}",
        offenders.len(),
        offenders.join("\n")
    );

    fs::remove_dir_all(&directory).ok();
}

/// Every rule of the record on one package: a private module and a private definition are absent, an opaque representation shows no constructors, prose attaches where it was written, a module's prose is the `mod` declaration's, a re-export is a link — or, out of a private module, the declaration itself on the facade's page — and a signature's names resolve to where they were declared — within the unit or in the standard library.
#[test]
fn a_package_documents_its_interface_for_its_consumers() {
    let root = tree(
        "document-package",
        &[
            (
                "curios.toml",
                "name = \"shapes\"\ndescription = \"Shapes and their areas.\"\n",
            ),
            (
                "lib.crs",
                concat!(
                    "use /std/{Nat, Option};\n\n",
                    "-- | Geometry.\npub mod geometry;\n\n",
                    "mod hidden;\n",
                    "pub use hidden/{unseen, Token};\n\n",
                    "-- | A shape.\n-- |\n-- | Round or square.\n",
                    "pub induct Shape: pub Type\n-- | Round.\n| circle(Nat)\n| square(Nat)\nend\n\n",
                    "pub induct Secret: Type\n| hidden(Nat)\nend\n\n",
                    "-- | The area.\n",
                    "pub let area(@A: Type, s: Shape, fallback: Option(A)) -> Nat =\n",
                    "    match s | circle(r) => r * r | square(w) => w * w end;\n\n",
                    "let helper: Nat =\n    1;\n\n",
                    "-- | Mint one.\n",
                    "pub let mint(t: Token) -> Nat =\n    1;\n\n",
                    "pub use geometry/{origin};\n",
                ),
            ),
            (
                "geometry.crs",
                "use /std/{Nat};\n\n-- | Where it starts.\npub let origin: Nat =\n    0;\n",
            ),
            (
                "hidden.crs",
                concat!(
                    "pub let unseen: /std/Nat =\n    2;\n\n",
                    "-- | A token.\n",
                    "pub induct Token: pub Type\n| token(/std/Nat)\nend\n\n",
                    "pub let kept: /std/Nat =\n    3;\n",
                ),
            ),
        ],
    );
    let governing = Governing::found(None, &root).expect("a governed package");
    let units = order(&governing).expect("a scope");

    let documentation = documentation(DEFAULT_STEP_BUDGET, units, &Overlay::default(), None)
        .expect("the package documents");
    fs::remove_dir_all(&root).unwrap();

    let paths = documentation
        .modules
        .iter()
        .map(|module| module.path.join())
        .collect::<Vec<_>>();
    assert_eq!(
        paths,
        ["/shapes", "/shapes/geometry"],
        "a private module has no page"
    );

    assert_eq!(
        documentation.description.as_deref(),
        Some("Shapes and their areas."),
        "the manifest's description rides in the record"
    );
    let library = &documentation.modules[0];
    assert_eq!(library.prose, None, "the root's prose is the manifest's");
    assert_eq!(library.children, [Qualifier::from(["shapes", "geometry"])]);
    assert_eq!(
        library
            .declarations
            .iter()
            .map(|declaration| declaration.name.as_str())
            .collect::<Vec<_>>(),
        [
            "unseen", "Token", "Shape", "Secret", "area", "mint", "origin"
        ],
        "the order the module writes them: the `pub use` out of the private module stands where it is written, above the declarations below it, and the one out of `geometry` last"
    );
    let named = |name: &str| {
        library
            .declarations
            .iter()
            .find(|declaration| declaration.name == name)
            .unwrap_or_else(|| panic!("{name} on the library's page"))
    };

    // Every name this page offers is named for this page, whether it was written here or exposed out of a module with none.
    for declaration in &library.declarations {
        assert_eq!(declaration.home, Qualifier::from(["shapes"]));
    }

    // The facade: `Token` and `unseen` are declared in `hidden`, which has no page — so their cards are this page's, under this page's name, with the prose and members written where they were declared. Nothing names `hidden`, since a consumer cannot write it.
    let token = named("Token");
    assert_eq!(token.kind, Kind::Inductive);
    assert_eq!(token.source, None, "a private module is not a page to name");
    assert_eq!(token.prose, Some(vec!["A token.".to_string()]));
    assert_eq!(
        token
            .members
            .iter()
            .map(|member| member.name.as_str())
            .collect::<Vec<_>>(),
        ["token"]
    );

    assert_eq!(
        named("mint")
            .signature
            .marks
            .iter()
            .map(|mark| (mark.referent.join(), mark.within))
            .collect::<Vec<_>>(),
        [
            ("/shapes/Token".to_string(), true),
            ("/sys/Nat/Nat".to_string(), false)
        ],
        "a mark names the declaration the way this bundle shows it, and leaves a name from another unit as it stands"
    );

    let shape = named("Shape");
    assert_eq!(
        shape.prose,
        Some(vec![
            "A shape.".to_string(),
            String::new(),
            "Round or square.".to_string()
        ])
    );
    assert_eq!(shape.signature.text, "pub induct Shape: pub Type");
    assert_eq!(
        shape
            .members
            .iter()
            .map(|member| (member.name.as_str(), member.signature.text.as_str()))
            .collect::<Vec<_>>(),
        [("circle", "circle(Nat)"), ("square", "square(Nat)")]
    );
    assert_eq!(shape.members[0].prose, Some(vec!["Round.".to_string()]));
    assert!(!shape.opaque);
    assert!(
        named("Secret").opaque && named("Secret").members.is_empty(),
        "an opaque representation is marked and shows no constructors"
    );

    let area = named("area");
    assert_eq!(
        area.signature.text,
        "pub let area(@A: Type, s: Shape, fallback: Option(A)) -> Nat"
    );
    let referents = area
        .signature
        .marks
        .iter()
        .map(|mark| {
            (
                &area.signature.text[mark.start..mark.end],
                mark.referent.join(),
                mark.within,
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        referents,
        [
            ("Shape", "/shapes/Shape".to_string(), true),
            ("Option", "/std/Option/Option".to_string(), false),
            // The canonical site, not the re-export the import went through: `/std/Nat` exposes the carrier `/sys` declares.
            ("Nat", "/sys/Nat/Nat".to_string(), false),
        ],
        "a binder is plain text, an own declaration links within, an import links outside"
    );

    // A re-export is a card of the page that offers it, naming the page it is written on; one out of a private module is a card too, and names nothing, since a consumer has no path to that module.
    let exposed = library
        .declarations
        .iter()
        .find(|declaration| declaration.name == "origin")
        .expect("origin");
    assert_eq!(exposed.home.join(), "/shapes");
    assert_eq!(
        exposed.source.as_ref().map(Qualifier::join).as_deref(),
        Some("/shapes/geometry/origin")
    );

    let facade = library
        .declarations
        .iter()
        .find(|declaration| declaration.name == "Token")
        .expect("Token");
    assert_eq!(facade.home.join(), "/shapes");
    assert_eq!(
        facade.source, None,
        "a private module is not a page to name"
    );

    let geometry = &documentation.modules[1];
    assert_eq!(geometry.prose, Some(vec!["Geometry.".to_string()]));
    assert_eq!(geometry.declarations[0].name, "origin");
    assert_eq!(
        geometry.declarations[0].prose,
        Some(vec!["Where it starts.".to_string()])
    );
}
