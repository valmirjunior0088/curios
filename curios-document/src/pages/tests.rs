//! The bundle's addressing and what it writes: a mark's referent is found where the record shows the declaration, which for a facade is the re-exporting module's page; the index lists every module, named declaration and member at that address; and a page carries the field the script shows.

use {
    super::*,
    crate::{Declaration, Kind, Member, ModuleDocumentation, Signature},
    std::time::{SystemTime, UNIX_EPOCH},
};

fn signature(text: &str) -> Signature {
    Signature {
        text: text.to_string(),
        marks: Vec::new(),
    }
}

fn declaration(kind: Kind, name: &str, home: &[&str], members: &[&str]) -> Declaration {
    Declaration {
        name: name.to_string(),
        home: Qualifier::from(home.iter().copied()),
        kind,
        signature: signature(name),
        prose: None,
        members: members
            .iter()
            .map(|member| Member {
                name: member.to_string(),
                signature: signature(member),
                prose: None,
            })
            .collect(),
        opaque: false,
        derived: false,
    }
}

fn witness(text: &str) -> Declaration {
    Declaration {
        name: String::new(),
        home: Qualifier::from(["shapes"]),
        kind: Kind::Witness,
        signature: signature(text),
        prose: None,
        members: Vec::new(),
        opaque: false,
        derived: false,
    }
}

/// A root with an inductive and a concept of its own, a facade for a declaration of a private module, a witness, and one child module. The concept's first member is its superclass constraint, which the record keeps nameless so the block prints it.
fn record() -> Documentation {
    Documentation {
        prefix: Qualifier::from(["shapes"]),
        description: None,
        modules: vec![
            ModuleDocumentation {
                path: Qualifier::from(["shapes"]),
                prose: None,
                children: vec![Qualifier::from(["shapes", "geometry"])],
                declarations: vec![
                    declaration(Kind::Inductive, "Shape", &["shapes"], &["circle"]),
                    declaration(Kind::Concept, "Area", &["shapes"], &["", "area"]),
                    witness("satisfy Show(Shape)"),
                    // Declared in the private `hidden`, shown here through `pub use hidden/{Token}`.
                    declaration(Kind::Inductive, "Token", &["shapes", "hidden"], &["token"]),
                ],
                reexports: Vec::new(),
            },
            ModuleDocumentation {
                path: Qualifier::from(["shapes", "geometry"]),
                prose: None,
                children: Vec::new(),
                declarations: vec![declaration(
                    Kind::Definition,
                    "origin",
                    &["shapes", "geometry"],
                    &[],
                )],
                reexports: Vec::new(),
            },
        ],
    }
}

#[test]
fn a_referent_is_found_where_the_record_shows_it() {
    let record = record();
    let bundle = Bundle::new(&record);
    let href = |depth, path: &[&str]| bundle.href(depth, &Qualifier::from(path.iter().copied()));

    assert_eq!(
        href(0, &["shapes", "Shape"]).as_deref(),
        Some("index.html#Shape")
    );
    assert_eq!(
        href(0, &["shapes", "Shape", "circle"]).as_deref(),
        Some("index.html#Shape/circle")
    );
    assert_eq!(
        href(0, &["shapes", "geometry"]).as_deref(),
        Some("geometry.crs.html")
    );
    assert_eq!(
        href(1, &["shapes", "geometry", "origin"]).as_deref(),
        Some("../geometry.crs.html#origin")
    );
    // The facade: the mark names the declaration's home, and the link lands on the page that shows it.
    assert_eq!(
        href(0, &["shapes", "hidden", "Token"]).as_deref(),
        Some("index.html#Token")
    );
    assert_eq!(
        href(1, &["shapes", "hidden", "Token", "token"]).as_deref(),
        Some("../index.html#Token/token")
    );
    // A declaration nothing shows has no address, rather than an anchor no page defines.
    assert_eq!(href(0, &["shapes", "hidden", "unseen"]), None);
}

#[test]
fn the_index_lists_every_address_and_a_page_carries_the_field() {
    let record = record();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let directory =
        std::env::temp_dir().join(format!("curios-document-{}-{nanos}", std::process::id()));
    write_documentation(&record, &directory).unwrap();

    // The record's order, a member beneath its declaration by what its kind calls one, the facade at its home, and no row for the witness or the nameless constraint.
    let index = fs::read_to_string(directory.join("static").join("index.js")).unwrap();
    assert_eq!(
        index,
        concat!(
            "window.curiosIndex=[",
            "[\"mod\",\"/shapes\",\"index.html\"],",
            "[\"induct\",\"/shapes/Shape\",\"index.html#Shape\"],",
            "[\"case\",\"/shapes/Shape/circle\",\"index.html#Shape/circle\"],",
            "[\"concept\",\"/shapes/Area\",\"index.html#Area\"],",
            "[\"method\",\"/shapes/Area/area\",\"index.html#Area/area\"],",
            "[\"induct\",\"/shapes/hidden/Token\",\"index.html#Token\"],",
            "[\"case\",\"/shapes/hidden/Token/token\",\"index.html#Token/token\"],",
            "[\"mod\",\"/shapes/geometry\",\"geometry.crs.html\"],",
            "[\"let\",\"/shapes/geometry/origin\",\"geometry.crs.html#origin\"]",
            "];\n"
        )
    );

    // The page loads the index before the script, tells the script where the root is, and holds the field hidden; the rail lists the named declarations and not the witness.
    let landing = fs::read_to_string(directory.join("index.html")).unwrap();
    assert!(
        landing.contains("<html lang=\"en\" data-root=\"\">"),
        "{landing}"
    );
    assert!(
        landing.contains("<script src=\"static/index.js\" defer></script>"),
        "{landing}"
    );
    assert!(
        landing.contains("<form class=\"search\" role=\"search\" hidden>"),
        "{landing}"
    );
    assert!(
        landing.contains("<div class=\"results\" hidden></div>"),
        "{landing}"
    );
    assert!(landing.contains("href=\"#Token\""), "{landing}");
    assert!(landing.contains("id=\"satisfy-1\""), "{landing}");
    assert!(
        !landing.contains("href=\"#satisfy-1\""),
        "a witness is anonymous and stays out of the rail: {landing}"
    );

    fs::remove_dir_all(directory).unwrap();
}
