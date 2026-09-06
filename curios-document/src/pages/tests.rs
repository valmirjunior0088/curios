//! The bundle's addressing: a mark's referent is found where the record shows the declaration, which for a facade is the re-exporting module's page.

use {
    super::*,
    crate::{Declaration, Kind, Member, ModuleDocumentation, Signature},
};

fn declaration(name: &str, home: &[&str], members: &[&str]) -> Declaration {
    let signature = |text: &str| Signature {
        text: text.to_string(),
        marks: Vec::new(),
    };
    Declaration {
        name: name.to_string(),
        home: Qualifier::from(home.iter().copied()),
        kind: Kind::Inductive,
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

#[test]
fn a_referent_is_found_where_the_record_shows_it() {
    let record = Documentation {
        prefix: Qualifier::from(["shapes"]),
        description: None,
        modules: vec![
            ModuleDocumentation {
                path: Qualifier::from(["shapes"]),
                prose: None,
                children: vec![Qualifier::from(["shapes", "geometry"])],
                declarations: vec![
                    declaration("Shape", &["shapes"], &["circle"]),
                    // Declared in the private `hidden`, shown here through `pub use hidden/{Token}`.
                    declaration("Token", &["shapes", "hidden"], &["token"]),
                ],
                reexports: Vec::new(),
            },
            ModuleDocumentation {
                path: Qualifier::from(["shapes", "geometry"]),
                prose: None,
                children: Vec::new(),
                declarations: vec![declaration("origin", &["shapes", "geometry"], &[])],
                reexports: Vec::new(),
            },
        ],
    };
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
