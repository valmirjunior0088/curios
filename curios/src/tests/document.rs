//! A package's documentation record, built by the lowering and carried on the unit: what `curios document` renders, checked over the standard library — the largest interface in the tree, read off the image it ships in — and over a fixture package whose every rule of the record is written out.

use {
    curios_package::{Governing, order},
    curios_pipeline::{CompileError, DEFAULT_STEP_BUDGET, with_units},
    curios_text::{Kind, Overlay},
    curios_utilities::Qualifier,
    curios_wonder::documentation,
    std::{
        fs,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    },
};

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
    let documentation = with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        None,
        |_| {},
        |prelude, _| {
            prelude
                .text()
                .documentation()
                .cloned()
                .ok_or_else(|| CompileError::failure("the image carries no record".to_string()))
        },
    )
    .expect("the standard library documents");

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
            .is_some_and(|lines| lines[0].starts_with("Error first, value last")),
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

    // `pub use Result/*` exposes the constructors at the module, each a link to the type that declares them.
    assert!(
        result
            .reexports
            .iter()
            .any(|reexport| reexport.name == "success"
                && reexport.referent.join() == "/std/Result/Result/success"),
        "{:?}",
        result.reexports
    );
}

/// Every rule of the record on one package: a private module and a private definition are absent, an opaque representation shows no constructors, prose attaches where it was written, a module's prose is the `mod` declaration's, a re-export is a link, and a signature's names resolve to where they were declared — within the unit or in the standard library.
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
                    "mod hidden;\n\n",
                    "-- | A shape.\n-- |\n-- | Round or square.\n",
                    "pub induct Shape: pub Type\n-- | Round.\n| circle(Nat)\n| square(Nat)\nend\n\n",
                    "pub induct Secret: Type\n| hidden(Nat)\nend\n\n",
                    "-- | The area.\n",
                    "pub let area(@A: Type, s: Shape, fallback: Option(A)) -> Nat =\n",
                    "    match s | circle(r) => r * r | square(w) => w * w end;\n\n",
                    "let helper: Nat =\n    1;\n\n",
                    "pub use geometry/{origin};\n",
                ),
            ),
            (
                "geometry.crs",
                "use /std/{Nat};\n\n-- | Where it starts.\npub let origin: Nat =\n    0;\n",
            ),
            ("hidden.crs", "pub let unseen: /std/Nat =\n    2;\n"),
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
        ["Shape", "Secret", "area"],
        "a private definition is absent"
    );

    let shape = &library.declarations[0];
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
    assert!(
        library.declarations[1].members.is_empty(),
        "an opaque representation shows no constructors"
    );

    let area = &library.declarations[2];
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

    assert_eq!(
        library
            .reexports
            .iter()
            .map(|reexport| (
                reexport.name.as_str(),
                reexport.referent.join(),
                reexport.within
            ))
            .collect::<Vec<_>>(),
        [("origin", "/shapes/geometry/origin".to_string(), true)]
    );

    let geometry = &documentation.modules[1];
    assert_eq!(geometry.prose, Some(vec!["Geometry.".to_string()]));
    assert_eq!(geometry.declarations[0].name, "origin");
    assert_eq!(
        geometry.declarations[0].prose,
        Some(vec!["Where it starts.".to_string()])
    );
}
