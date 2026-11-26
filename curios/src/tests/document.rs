//! A package's documentation record, made from the unit its compilation builds: what `curios document` renders, checked over the standard library — the largest interface in the tree, read off the image it ships in — and over a fixture package whose every rule of the record is written out.

use {
    curios_common::{Class, Qualifier, Source, test_support::*},
    curios_document::{Block, Entry, Kind, Marked, Signature, Totality},
    curios_elab::DEFAULT_BUDGET,
    curios_module::{Overlay, SYNTAX, written},
    curios_package::{Governing, order},
    curios_surface::{Format, Formatted, WrittenIn},
    curios_wonder::{std_documentation, test_support::*},
    std::fs,
};

/// The standard library's record, off the prelude the compiler was built with — exactly what `document --std` renders.
fn standard_library() -> curios_document::Documentation {
    std_documentation().expect("the standard library documents")
}

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("document", name);

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

    root
}

/// The standard library's record ships as an image beside the prelude's: no sources, no checkout, no second compilation — the build that lowered the prelude made the record. It is the honest test of the record, holding opaque types, derived witnesses, concepts and re-exports.
#[test]
fn the_standard_library_documents_from_the_archive() {
    let documentation = standard_library();

    assert!(
        documentation.modules.len() > 50,
        "only {} modules",
        documentation.modules.len()
    );
    assert_eq!(documentation.modules[0].path.join(), "/std");

    let try_get = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Map/try_get")
        .expect("the module of try_get");
    let checked = |name: &str| {
        try_get
            .declarations()
            .find(|declaration| declaration.name == name)
            .map(|declaration| (declaration.proof, declaration.totality))
    };
    assert_eq!(checked("try_get"), Some((false, Some(Totality::Total))));
    assert_eq!(checked("of_insert"), Some((true, Some(Totality::Total))));

    let nat = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Nat")
        .expect("the module of Nat");
    assert!(
        nat.declarations().any(|declaration| {
            declaration.kind == Kind::Witness
                && declaration.signature.text == "satisfy Show(Nat)"
                && declaration
                    .parties
                    .iter()
                    .map(Qualifier::join)
                    .eq(["/std/Show/Show", "/std/Nat"])
        }),
        "a witness of a carrier takes part in the concept and in the declaration that forms the carrier"
    );
    assert!(
        documentation
            .description
            .as_deref()
            .is_some_and(|description| description.starts_with("The standard library")),
        "{:?}",
        documentation.description
    );

    let ordering = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Ord/Ordering")
        .expect("an Ordering module");
    let induct = ordering
        .declarations()
        .find(|declaration| declaration.name == "Ordering")
        .expect("the Ordering type");
    assert_eq!(induct.kind, Kind::Inductive);
    assert!(
        induct
            .prose
            .as_ref()
            .is_some_and(|lines| lines[0].starts_with("The three-way answer")),
        "the type's prose is the `---` block written above it: {:?}",
        induct.prose
    );
    assert_eq!(
        induct
            .members
            .iter()
            .map(|member| member.name.as_str())
            .collect::<Vec<_>>(),
        ["lt", "eq", "gt"],
        "a public representation lists its constructors"
    );
    assert!(!induct.opaque);

    // `pub struct Map(K: Type, use Key(K), V: Type): Type` exports its name and not its fields, and the record says so rather than leaving an empty member list to be read either way.
    let map = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Map")
        .and_then(|module| {
            module
                .declarations()
                .find(|declaration| declaration.name == "Map")
        })
        .expect("the Map type");
    assert_eq!(map.kind, Kind::Structure);
    assert!(map.opaque && map.members.is_empty(), "{map:?}");

    // A concept's superclass edge is an anonymous field in the language and an anonymous member in the record, in its written place among the methods.
    let ordered = documentation
        .modules
        .iter()
        .find(|module| module.path.join() == "/std/Ord")
        .and_then(|module| {
            module
                .declarations()
                .find(|declaration| declaration.name == "Ord")
        })
        .expect("the Ord concept");
    assert_eq!(ordered.kind, Kind::Concept);
    assert_eq!(
        ordered
            .members
            .iter()
            .map(|member| member.name.as_str())
            .collect::<Vec<_>>(),
        [
            "", "ord", "lt", "le", "flips", "trans", "eq", "is_lt", "is_le"
        ],
        "{:?}",
        ordered.members
    );
    assert!(
        ordered.members[0].signature.text.starts_with("use "),
        "{:?}",
        ordered.members[0].signature
    );

    // A signature's reference to a declaration of the same unit links within the bundle.
    let flip = ordering
        .declarations()
        .find(|declaration| declaration.name == "flip")
        .expect("Ordering/flip");
    assert!(
        flip.signature.marks.iter().any(|mark| matches!(
            &mark.marked,
            Marked::Name { referent, within: true, .. }
                if referent.join() == "/std/Ord/Ordering/Ordering"
        )),
        "{:?}",
        flip.signature
    );

    // `pub use Ordering/*` exposes the constructors beside the type, which is where a consumer reaches them. A constructor is no declaration of its own: it stays a row of its type's block, and the page lists the line that exposes it, where the module writes it, each name in the order the type declares them and linked to its row.
    let declared = ordering
        .declarations()
        .find(|declaration| declaration.name == "Ordering")
        .expect("Ordering");
    assert!(
        declared.members.iter().any(|member| member.name == "lt"),
        "{:?}",
        declared.members
    );
    assert!(
        ordering
            .declarations()
            .all(|declaration| declaration.name != "lt"),
        "a constructor is no declaration of the page"
    );

    let exposed = ordering
        .entries
        .iter()
        .find_map(|entry| match entry {
            Entry::Exposure(line) => Some(line),
            Entry::Declaration(_) => None,
        })
        .expect("the line that exposes the constructors");
    assert_eq!(exposed.owner.join(), "/std/Ord/Ordering/Ordering");
    assert_eq!(exposed.names, ["lt", "eq", "gt"]);
    assert_eq!(exposed.signature.text, "pub use Ordering/{lt, eq, gt}");
    assert_eq!(
        exposed
            .signature
            .marks
            .iter()
            .filter_map(|mark| match &mark.marked {
                Marked::Name { referent, .. } => Some(referent.join()),
                Marked::Word(_) => None,
            })
            .collect::<Vec<_>>(),
        [
            "/std/Ord/Ordering/Ordering",
            "/std/Ord/Ordering/Ordering/lt",
            "/std/Ord/Ordering/Ordering/eq",
            "/std/Ord/Ordering/Ordering/gt",
        ],
        "the owner and each member are marked as a signature's names are"
    );
    // The line stands where the module writes it: right after the type it follows.
    let after = ordering
        .entries
        .iter()
        .position(|entry| matches!(entry, Entry::Declaration(declaration) if declaration.name == "Ordering"))
        .expect("Ordering");
    assert!(
        matches!(&ordering.entries[after + 1], Entry::Exposure(_)),
        "{:?}",
        ordering.entries[after + 1]
    );
}

/// **A declaration that is not a `let` resolves the names in its signature too.**
///
/// The import scope a page resolves through is recorded per declaration by the lowering, for a concept, an inductive, a structure, a witness and a foreign as for a `let` and a `test` — so a module with no `let` still resolves its signatures, where falling back to the union of its module's `let`s would resolve nothing: `/std/Show` is a `use` line and a concept, and its one method's return type links as `/std/Spell`'s identical one does.
///
/// `/std/Show` is the subject because it holds no `let` at all. Asserted on a *member*, since no other test in this tree asserts on a member's marks at all.
#[test]
fn a_concept_method_resolves_the_names_in_its_signature() {
    let documentation = standard_library();

    let page = documentation
        .modules
        .iter()
        .find(|page| page.path == Qualifier::from(["std", "Show"]))
        .expect("/std/Show has a page");
    let show = page
        .declarations()
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
        .filter_map(|mark| match &mark.marked {
            Marked::Name {
                referent, within, ..
            } => Some((
                &method.signature.text[mark.start..mark.end],
                referent.join(),
                *within,
            )),
            Marked::Word(_) => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(
        referents,
        [("Str", "/std/Str/Str".to_string(), true)],
        "the concept's own parameter is a binder and stays plain, and its return type links to the page that shows it — got {:?}",
        method.signature.text
    );
}

/// **The adopted half of that claim: the declarations are there.** Hiding `/sys` is half a property, and the sibling below pins only the half that is an absence — which a bundle would satisfy perfectly by dropping every adopted declaration on the floor, as rendering off a unit's own surface tree alone does once the prelude is two units.
///
/// Read off the record rather than the rendered pages, because what is at issue is whether the declaration exists at all — and asserted per carrier, since one surviving name would satisfy any count.
#[test]
fn every_intrinsic_carrier_reaches_a_page_through_its_std_module() {
    let documentation = standard_library();

    // One carrier per `/sys` type former the standard library re-exports, at the module a program reaches it through.
    let carriers = [
        ("/std/Nat", "Nat"),
        ("/std/Byte", "Byte"),
        ("/std/Int", "Int"),
        ("/std/Flt", "Flt"),
        ("/std/Bool", "Bool"),
        ("/std/List", "List"),
        ("/std/Io/Latch", "Latch"),
        ("/std/Io", "Io"),
    ];

    let mut missing = Vec::new();
    for (module, label) in carriers {
        let relative = module.strip_prefix('/').expect("an absolute path");
        let path = Qualifier::from(relative.split('/'));
        let found = documentation
            .modules
            .iter()
            .find(|page| page.path == path)
            .and_then(|page| {
                page.declarations()
                    .find(|declaration| declaration.name == label)
            });

        match found {
            // Adopted out of a root with no page here, so the entry says what that root holds and names no path.
            Some(declaration) => {
                if declaration.chip.as_deref() != Some("intrinsic") {
                    missing.push(format!(
                        "{module}/{label} is shown but chipped {:?}, not \"intrinsic\"",
                        declaration.chip
                    ));
                }
            }
            None => missing.push(format!("{module}/{label} reaches no page at all")),
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

/// The signature the module at `module`, an absolute path, shows `label` by, and the declaration each name it marks is linked to, in the order they are written.
fn shown(
    documentation: &curios_document::Documentation,
    module: &str,
    label: &str,
) -> (String, Vec<String>) {
    let relative = module.strip_prefix('/').expect("an absolute path");
    let path = Qualifier::from(relative.split('/'));
    let declaration = documentation
        .modules
        .iter()
        .find(|page| page.path == path)
        .and_then(|page| {
            page.declarations()
                .find(|declaration| declaration.name == label)
        })
        .unwrap_or_else(|| panic!("{module} shows no {label}"));
    let signature = &declaration.signature;

    (
        signature.text.clone(),
        signature
            .marks
            .iter()
            .filter_map(|mark| match &mark.marked {
                Marked::Name { referent, .. } => Some(referent.join()),
                Marked::Word(_) => None,
            })
            .collect(),
    )
}

/// **An adopted declaration links its carriers and its operations as an authored one does.** `/sys` is typed by core's table, so its types and preconditions hold intrinsic nodes rather than names. The printer writes each node through core's one table of how an intrinsic is written, naming every declaration it builds on by its path, so the card marks each one and the page links it.
#[test]
fn an_adopted_signature_marks_its_carriers_and_operations() {
    let documentation = standard_library();

    let (_, tripped) = shown(&documentation, "/std/Io/Latch", "tripped");
    assert_eq!(tripped, ["/std/Io/Latch", "/std/Io", "/std/Bool"]);

    // A precondition reads as a report reads it: an operator where the surface writes one, a call of the declaration where it does not.
    let (text, get) = shown(&documentation, "/std/Bytes", "get");
    assert!(text.contains("(i < "), "{text}");
    assert_eq!(
        get,
        [
            "/std/Bytes",
            "/std/Nat",
            "/std/Bool/Holds",
            "/std/Bytes/length",
            "/std/Byte"
        ]
    );
}

/// **A written name links what the lowering resolved it to.** `begins` names `Str/At/starts` through `Str`, which its module imports, and `At`, a child of that module: a page that resolved the text again by its own reading of the imports found only what a `use` brings in one level deep, and left the path plain. Read off the lowered unit at the place it was written, it is the declaration the compiler took it for.
#[test]
fn a_written_name_links_what_the_lowering_resolved_it_to() {
    let documentation = standard_library();

    let (text, begins) = shown(&documentation, "/std/Str/Valid", "begins");
    assert!(text.contains("Holds(Str/At/starts(b))"), "{text}");
    assert!(
        begins
            .iter()
            .any(|referent| referent == "/std/Str/At/starts"),
        "{begins:?}"
    );
}

/// **A constructor or a method a module exposes beside its owner is the line that exposes it, where the module writes it.** A glob over a declaration stands where it is written, its names in the order the declaration lists them; a named line files a declaration it names as one and the members it names of an owner as one line; and an owner another page declares is written by the name this module exposes it under.
#[test]
fn a_member_a_module_exposes_is_the_line_that_exposes_it() {
    let documentation = shades();
    let listed = documentation.modules[0]
        .entries
        .iter()
        .map(|entry| match entry {
            Entry::Declaration(declaration) => declaration.signature.text.clone(),
            Entry::Exposure(line) => format!("{} of {}", line.signature.text, line.owner.join()),
        })
        .collect::<Vec<_>>();

    assert_eq!(
        listed,
        [
            "pub induct Tone: pub Type",
            "pub let pitch: /std/Nat",
            "pub use Tone/{low} of /shades/tone/Tone",
            "pub induct Shade: pub Type",
            "pub let other: /std/Nat",
            "pub use Shade/{dark, light} of /shades/Shade",
            "pub concept Named(A: Type): pub Type",
            "satisfy Named(Shade)",
            "pub let same(s: Shade) -> /std/Eq()(s, s)",
        ]
    );
}

/// **A page says of a declaration what its checks established, and of a witness what it takes part in.** That a definition is a proof and that it is proven total are the checked module's and the certifier's to say, and a witness's concept and the type its key names are the lowering's.
#[test]
fn a_declaration_carries_what_its_checks_established() {
    let documentation = shades();
    let page = &documentation.modules[0];
    let named = |name: &str| {
        page.declarations()
            .find(|declaration| declaration.name == name)
            .unwrap_or_else(|| panic!("{name} on the library's page"))
    };

    let same = named("same");
    assert!(same.proof, "a definition whose type is a proposition");
    assert_eq!(same.totality, Some(Totality::Total));

    let other = named("other");
    assert!(!other.proof);
    assert_eq!(other.totality, Some(Totality::Total));

    // A type is neither, and a definition another page declares carries what that page's does.
    let shade = named("Shade");
    assert!(!shade.proof);
    assert_eq!(shade.totality, None);
    assert_eq!(named("pitch").totality, Some(Totality::Total));

    let witness = page
        .declarations()
        .find(|declaration| declaration.kind == Kind::Witness)
        .expect("the witness");
    assert_eq!(
        witness
            .parties
            .iter()
            .map(Qualifier::join)
            .collect::<Vec<_>>(),
        ["/shades/Named", "/shades/Shade"]
    );
    assert!(same.parties.is_empty());
}

/// The record of a package whose root exposes a family another module declares with one of its constructors, declares a family of its own with a glob over its constructors beneath, a concept with a witness, and a proof.
fn shades() -> curios_document::Documentation {
    let root = tree(
        "document-shades",
        &[
            (
                "curios.toml",
                concat!(
                    "compiler = \"",
                    env!("CARGO_PKG_VERSION"),
                    "\"\nname = \"shades\"\n"
                ),
            ),
            (
                "lib.crs",
                concat!(
                    "pub mod tone;\n",
                    "pub use tone/{Tone, pitch};\n",
                    "pub use tone/Tone/{low};\n\n",
                    "pub induct Shade: pub Type\n| dark()\n| light()\nend\n\n",
                    "pub let other: /std/Nat =\n    1;\n\n",
                    "pub use Shade/*;\n\n",
                    "pub concept Named(A: Type): pub Type {\n    name(A) -> /std/Str,\n}\n\n",
                    "satisfy Named(Shade) {\n    name(_) = \"shade\",\n}\n\n",
                    "pub let same(s: Shade) -> /std/Eq()(s, s) =\n    /std/Eq/refl();\n",
                ),
            ),
            (
                "tone.crs",
                concat!(
                    "pub induct Tone: pub Type\n| high()\n| low()\nend\n\n",
                    "pub let pitch: /std/Nat =\n    2;\n",
                ),
            ),
        ],
    );
    let governing = Governing::found(None, &root, None).expect("a governed package");
    let units = order(&governing).expect("a scope");

    documentation(DEFAULT_BUDGET, units, &Overlay::default(), None).expect("the package documents")
}

/// **A path the compiler wrote is spelled as a consumer writes it, and laid out as it is spelled.** `/sys` names what its declarations build on by absolute `/sys` paths, which no consumer may write; an adopted declaration shows each by its path below `/sys`, which `/std` re-exports it under, rather than in full beside names an author chose. The spelling is decided before the line is laid out, so `get` stays on the one line its shown text fits.
#[test]
fn an_adopted_signature_spells_its_names_below_their_root() {
    let documentation = standard_library();

    let (trip, _) = shown(&documentation, "/std/Io/Latch", "trip");
    assert_eq!(trip, "pub let trip(l: Io/Latch) -> Io({})");
    let (tripped, _) = shown(&documentation, "/std/Io/Latch", "tripped");
    assert_eq!(tripped, "pub let tripped(l: Io/Latch) -> Io(Bool)");
    let (get, _) = shown(&documentation, "/std/Bytes", "get");
    assert_eq!(
        get,
        "pub let get(b: Bytes, i: Nat, @ok: Bool/Holds(i < Bytes/length(b))) -> Byte"
    );

    let in_full = documentation
        .modules
        .iter()
        .flat_map(|page| {
            page.declarations()
                .map(move |declaration| (page, declaration))
        })
        .filter(|(_, declaration)| declaration.chip.as_deref() == Some("intrinsic"))
        .flat_map(|(page, declaration)| {
            std::iter::once(&declaration.signature)
                .chain(declaration.members.iter().map(|member| &member.signature))
                .filter(|signature| signature.text.contains("/std/"))
                .map(move |signature| format!("{}: {}", page.path.join(), signature.text))
        })
        .collect::<Vec<_>>();
    assert!(
        in_full.is_empty(),
        "an adopted declaration spells a name in full:\n{}",
        in_full.join("\n")
    );
}

/// **No page names a root a consumer may not write.** `/sys` is adopted rather than linked: its declarations appear under the `/std` module that exposes them, and their own paths appear nowhere a reader can see — not in a signature, a fact, a search row or a crumb.
///
/// Rendered and read back rather than checked against the record, because the record is only half the claim. A path can reach a reader through a template as easily as through a field, and the file is the thing a reader opens.
#[test]
fn no_internal_root_reaches_a_rendered_page() {
    let documentation = standard_library();

    let directory = Temporary::new("document", "internal");
    curios_document::write_documentation(&documentation, &directory).expect("the pages render");

    let mut offenders = Vec::new();
    let mut pending = vec![directory.to_path_buf()];
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
}

/// Every rule of the record on one package: a private module and a private definition are absent, an opaque representation shows no constructors, prose attaches where it was written, a module's prose is the `mod` declaration's, a re-export is a link — or, out of a private module, the declaration itself on the facade's page — and a signature's names resolve to where they were declared — within the unit or in the standard library.
#[test]
fn a_package_documents_its_interface_for_its_consumers() {
    let root = tree(
        "document-package",
        &[
            (
                "curios.toml",
                concat!(
                    "compiler = \"",
                    env!("CARGO_PKG_VERSION"),
                    "\"\nname = \"shapes\"\ndescription = \"Shapes and their areas.\"\n"
                ),
            ),
            (
                "lib.crs",
                concat!(
                    "use /std/{Nat, Option};\n\n",
                    "--- Geometry.\npub mod geometry;\n\n",
                    "mod hidden;\n",
                    "pub use hidden/{unseen, Token};\n\n",
                    "--- A shape.\n---\n--- Round or square.\n",
                    "pub induct Shape: pub Type\n--- Round.\n| circle(Nat)\n| square(Nat)\nend\n\n",
                    "pub induct Secret: Type\n| hidden(Nat)\nend\n\n",
                    "--- The area.\n",
                    "pub let area(@A: Type, s: Shape, fallback: Option(A)) -> Nat =\n",
                    "    match s | circle(r) => r * r | square(w) => w * w end;\n\n",
                    "let helper: Nat =\n    1;\n\n",
                    "--- Mint one.\n",
                    "pub let mint(t: Token) -> Nat =\n    1;\n\n",
                    "pub use geometry/{origin};\n",
                ),
            ),
            (
                "geometry.crs",
                "use /std/{Nat};\n\n--- Where it starts.\npub let origin: Nat =\n    0;\n",
            ),
            (
                "hidden.crs",
                concat!(
                    "pub let unseen: /std/Nat =\n    2;\n\n",
                    "--- A token.\n",
                    "pub induct Token: pub Type\n| token(/std/Nat)\nend\n\n",
                    "pub let kept: /std/Nat =\n    3;\n",
                ),
            ),
        ],
    );
    let governing = Governing::found(None, &root, None).expect("a governed package");
    let units = order(&governing).expect("a scope");

    let documentation = documentation(DEFAULT_BUDGET, units, &Overlay::default(), None)
        .expect("the package documents");

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
            .declarations()
            .map(|declaration| declaration.name.as_str())
            .collect::<Vec<_>>(),
        [
            "unseen", "Token", "Shape", "Secret", "area", "mint", "origin"
        ],
        "the order the module writes them: the `pub use` out of the private module stands where it is written, above the declarations below it, and the one out of `geometry` last"
    );
    let named = |name: &str| {
        library
            .declarations()
            .find(|declaration| declaration.name == name)
            .unwrap_or_else(|| panic!("{name} on the library's page"))
    };

    // The facade: `Token` and `unseen` are declared in `hidden`, which has no page — so their entries are this page's, under this page's name, with the prose and members written where they were declared. Nothing names `hidden`, since a consumer cannot write it.
    let token = named("Token");
    assert_eq!(token.kind, Kind::Inductive);
    assert_eq!(
        token.named,
        Qualifier::from(["shapes", "Token"]),
        "a private module is not a page to name: every mark names the declaration where this page shows it"
    );
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
            .filter_map(|mark| match &mark.marked {
                Marked::Name {
                    referent, within, ..
                } => Some((referent.join(), *within)),
                Marked::Word(_) => None,
            })
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
        .filter_map(|mark| match &mark.marked {
            Marked::Name {
                referent, within, ..
            } => Some((
                &area.signature.text[mark.start..mark.end],
                referent.join(),
                *within,
            )),
            Marked::Word(_) => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        referents,
        [
            ("Shape", "/shapes/Shape".to_string(), true),
            ("Option", "/sys/Option".to_string(), false),
            // The canonical site, not the re-export the import went through: `/std/Nat` exposes the carrier `/sys` declares.
            ("Nat", "/sys/Nat/Nat".to_string(), false),
        ],
        "a binder is plain text, an own declaration links within, an import links outside"
    );

    // A re-export is an entry of the page that offers it, named by the path of the page it is written on; one out of a private module is an entry too, named by this page's path, since a consumer has no path to that module.
    let exposed = library
        .declarations()
        .find(|declaration| declaration.name == "origin")
        .expect("origin");
    assert_eq!(exposed.named.join(), "/shapes/geometry/origin");

    let facade = library
        .declarations()
        .find(|declaration| declaration.name == "Token")
        .expect("Token");
    assert_eq!(
        facade.named.join(),
        "/shapes/Token",
        "a private module is not a page to name"
    );

    let geometry = &documentation.modules[1];
    assert_eq!(geometry.prose, Some(vec!["Geometry.".to_string()]));
    let origin = geometry.declarations().next().expect("origin");
    assert_eq!(origin.name, "origin");
    assert_eq!(origin.prose, Some(vec!["Where it starts.".to_string()]));
}

/// Each span a signature marks, as `(text, class)`: a word by its class, a name by its declaration's.
fn painted(signature: &Signature) -> Vec<(&str, Option<Class>)> {
    signature
        .marks
        .iter()
        .map(|mark| {
            let class = match &mark.marked {
                Marked::Word(class) => Some(*class),
                Marked::Name { class, .. } => *class,
            };
            (&signature.text[mark.start..mark.end], class)
        })
        .collect()
}

/// **A signature paints each word and each name by what it is.** The printer marks the words it writes, so a parameter spelled as a contextual word, or one whose first letters spell a keyword, is no word; and a name is classed by what the declaration it resolved to states — a type, a constructor, a concept and a function of `/std`, a value of the package's own, the concept a witness satisfies — wherever it is declared, a binder staying plain; a `foreign` signature's wire words are words, each naming a type.
#[test]
fn a_signature_paints_each_word_and_name_by_what_it_is() {
    let root = tree(
        "document-classes",
        &[
            (
                "curios.toml",
                concat!(
                    "compiler = \"",
                    env!("CARGO_PKG_VERSION"),
                    "\"\nname = \"probe\"\n"
                ),
            ),
            (
                "lib.crs",
                concat!(
                    "use /std/{Nat, Option, Show, Eq};\n\n",
                    "pub let seven: Nat =\n    7;\n\n",
                    "pub let pick(concept: Nat, letà: Nat) -> Nat =\n    concept + letà;\n\n",
                    "pub let need(@A: Type, use Show(A), p: Eq()(Option/some(Nat/add(seven, 1)), Option/some(8))) -> Nat =\n    0;\n\n",
                    "pub concept Sized(A: Type): pub Type {\n    size(A) -> Nat,\n}\n\n",
                    "satisfy Sized(Nat) {\n    size(n) = n,\n}\n\n",
                    "pub foreign halt(Nat, List(Bytes)) -> Never;\n",
                ),
            ),
        ],
    );
    let governing = Governing::found(None, &root, None).expect("a governed package");
    let units = order(&governing).expect("a scope");
    let documentation = documentation(DEFAULT_BUDGET, units, &Overlay::default(), None)
        .expect("the package documents");
    let signature = |label: &str| {
        &documentation.modules[0]
            .declarations()
            .find(|declaration| declaration.name == label)
            .unwrap_or_else(|| panic!("the package declares {label}"))
            .signature
    };

    let keyword = Some(Class::Keyword);
    let type_ = Some(Class::Type);
    assert_eq!(
        painted(signature("pick")),
        [
            ("pub", keyword),
            ("let", keyword),
            ("Nat", type_),
            ("Nat", type_),
            ("Nat", type_)
        ],
        "{}",
        signature("pick").text
    );
    assert_eq!(
        painted(signature("need")),
        [
            ("pub", keyword),
            ("let", keyword),
            ("Type", Some(Class::Sort)),
            ("use", keyword),
            ("Show", Some(Class::Concept)),
            ("Eq", type_),
            ("Option/some", Some(Class::Constructor)),
            ("Nat/add", Some(Class::Function)),
            ("seven", Some(Class::Value)),
            ("Option/some", Some(Class::Constructor)),
            ("Nat", type_)
        ],
        "{}",
        signature("need").text
    );

    let witness = &documentation.modules[0]
        .declarations()
        .find(|declaration| declaration.kind == Kind::Witness)
        .expect("the package declares a witness")
        .signature;
    assert_eq!(
        painted(witness),
        [
            ("satisfy", keyword),
            ("Sized", Some(Class::Concept)),
            ("Nat", type_)
        ],
        "{}",
        witness.text
    );
    assert_eq!(
        painted(signature("halt")),
        [
            ("pub", keyword),
            ("foreign", keyword),
            ("Nat", type_),
            ("List", type_),
            ("Bytes", type_),
            ("Never", type_)
        ],
        "{}",
        signature("halt").text
    );
}

/// **The block a page and a report write is the formatter's.** How a declaration writes its members — the bar before a constructor, the braces and the commas of fields and methods, the word that closes them — is stated once for every reader of a record ([`Block`]), beside the printer and not by it. So every declaration of the standard library that writes a block is written out as a report writes it ([`written`]), a text of its own, and the formatter leaves each as it is.
#[test]
fn a_declaration_written_out_is_as_the_formatter_writes_it() {
    let documentation = standard_library();
    let mut wrong = Vec::new();
    let mut blocks = 0;

    for page in &documentation.modules {
        for declaration in page.declarations() {
            if Block::of(declaration.kind, declaration.opaque).is_none() {
                continue;
            }
            blocks += 1;
            let members = declaration
                .members
                .iter()
                .map(|member| member.signature.text.as_str())
                .collect::<Vec<_>>();
            let text = format!(
                "{}\n",
                written(
                    declaration.kind,
                    declaration.opaque,
                    &declaration.signature.text,
                    &members
                )
            );
            let stands = format!("{}/{}", page.path.join(), declaration.name);
            match Formatted::from_source(
                &Source::inline(text.as_str()),
                WrittenIn::Program,
                Format::default(),
                &SYNTAX,
            ) {
                Ok(Formatted::Unchanged(_)) => {}
                Ok(Formatted::Changed(formatted)) => wrong.push(format!(
                    "{stands} is written\n{text}and formats to\n{formatted}"
                )),
                Err(refusal) => wrong.push(format!(
                    "{stands} is written\n{text}and is refused:\n{}",
                    refusal.render()
                )),
            }
        }
    }

    assert!(
        blocks > 100,
        "{blocks} declarations of the library write a block"
    );
    assert!(
        wrong.is_empty(),
        "{} of {blocks} declarations are not written as `curios format` writes them:\n\n{}",
        wrong.len(),
        wrong.join("\n")
    );
}
