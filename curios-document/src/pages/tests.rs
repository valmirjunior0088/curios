//! The bundle's addressing and what it writes: a declaration is found where it is drawn whole, which for a path that is a module too is the path's own page; the index lists every path once; and a page opens on the declaration at its path and lists what its module exposes, each thing once.

use {super::*, crate::*, curios_common::test_support::*};

fn signature(text: &str) -> Signature {
    Signature {
        text: text.to_string(),
        marks: Vec::new(),
    }
}

/// A declaration of `kind` a page shows as `name`, every mark naming it `named`, with the members `members` and a note of two paragraphs.
fn declaration(kind: Kind, name: &str, named: &[&str], members: &[&str]) -> Declaration {
    Declaration {
        name: name.to_string(),
        kind,
        signature: signature(&format!("{} {name}", keyword(kind))),
        prose: Some(vec![
            format!("What {name} is."),
            String::new(),
            format!("More of {name}."),
        ]),
        members: members
            .iter()
            .map(|member| Member {
                name: member.to_string(),
                signature: signature(member),
                prose: Some(vec![format!("What {member} holds.")]),
            })
            .collect(),
        opaque: false,
        derived: false,
        proof: false,
        totality: None,
        parties: Vec::new(),
        named: Qualifier::from(named.iter().copied()),
        chip: None,
    }
}

/// A witness the module `module` declares, written `text`, of the declarations `parties`.
fn witness(module: &[&str], text: &str, parties: &[&[&str]]) -> Declaration {
    Declaration {
        name: String::new(),
        kind: Kind::Witness,
        signature: signature(text),
        prose: None,
        members: Vec::new(),
        opaque: false,
        derived: false,
        proof: false,
        totality: None,
        parties: parties
            .iter()
            .map(|party| Qualifier::from(party.iter().copied()))
            .collect(),
        named: Qualifier::from(module.iter().copied()),
        chip: None,
    }
}

/// A unit whose root exposes: `Shape`, an inductive that is a module too, declared in that module; the line that exposes its constructors beside it; a concept of its own, whose first member is its superclass constraint and has no name; a witness; a facade for a declaration of a private module; a definition another module declares; and a module that is no declaration. The module `Shape` holds its own declaration, a definition, and the structure `Corner`, which is a module too and declared here, whose module holds a definition named as its field is. The module `geometry` holds a definition and a witness of `Shape`, declared apart from it.
fn record() -> Documentation {
    Documentation {
        prefix: Qualifier::from(["shapes"]),
        description: None,
        modules: vec![
            ModuleDocumentation {
                path: Qualifier::from(["shapes"]),
                prose: None,
                children: vec![
                    Qualifier::from(["shapes", "Shape"]),
                    Qualifier::from(["shapes", "geometry"]),
                ],
                entries: vec![
                    // `pub use Shape/Shape;`, the declaration of the module of the same name.
                    Entry::Declaration(declaration(
                        Kind::Inductive,
                        "Shape",
                        &["shapes", "Shape", "Shape"],
                        &["circle", "square"],
                    )),
                    // `pub use Shape/*;`, which exposes the constructors at `/shapes/circle` and `/shapes/square`.
                    Entry::Exposure(ExposureLine {
                        owner: Qualifier::from(["shapes", "Shape", "Shape"]),
                        names: vec!["circle".to_string(), "square".to_string()],
                        signature: signature("pub use Shape/{circle, square}"),
                    }),
                    Entry::Declaration(declaration(
                        Kind::Concept,
                        "Area",
                        &["shapes", "Area"],
                        &["", "area"],
                    )),
                    Entry::Declaration(witness(
                        &["shapes"],
                        "satisfy Show(Shape)",
                        &[&["std", "Show", "Show"], &["shapes", "Shape", "Shape"]],
                    )),
                    // Declared in the private `hidden` and shown here through `pub use hidden/{Token}`, so this page is the name it carries.
                    Entry::Declaration(declaration(
                        Kind::Inductive,
                        "Token",
                        &["shapes", "Token"],
                        &["token"],
                    )),
                    // `pub use geometry/origin;`, which `geometry` declares and shows.
                    Entry::Declaration(declaration(
                        Kind::Definition,
                        "origin",
                        &["shapes", "geometry", "origin"],
                        &[],
                    )),
                ],
            },
            ModuleDocumentation {
                path: Qualifier::from(["shapes", "Shape"]),
                prose: Some(vec!["Shapes, and what measures them.".to_string()]),
                children: vec![Qualifier::from(["shapes", "Shape", "Corner"])],
                entries: vec![
                    Entry::Declaration(declaration(
                        Kind::Inductive,
                        "Shape",
                        &["shapes", "Shape", "Shape"],
                        &["circle", "square"],
                    )),
                    Entry::Declaration(declaration(
                        Kind::Definition,
                        "area",
                        &["shapes", "Shape", "area"],
                        &[],
                    )),
                    Entry::Declaration(declaration(
                        Kind::Structure,
                        "Corner",
                        &["shapes", "Shape", "Corner"],
                        &["x"],
                    )),
                ],
            },
            ModuleDocumentation {
                path: Qualifier::from(["shapes", "Shape", "Corner"]),
                prose: None,
                children: Vec::new(),
                // A definition at the path the structure's field would have, were a field a path.
                entries: vec![Entry::Declaration(declaration(
                    Kind::Definition,
                    "x",
                    &["shapes", "Shape", "Corner", "x"],
                    &[],
                ))],
            },
            ModuleDocumentation {
                path: Qualifier::from(["shapes", "geometry"]),
                prose: Some(vec!["Points and distances.".to_string()]),
                children: Vec::new(),
                entries: vec![
                    Entry::Declaration(declaration(
                        Kind::Definition,
                        "origin",
                        &["shapes", "geometry", "origin"],
                        &[],
                    )),
                    Entry::Declaration(witness(
                        &["shapes", "geometry"],
                        "satisfy Area(Shape)",
                        &[&["shapes", "Area"], &["shapes", "Shape", "Shape"]],
                    )),
                ],
            },
        ],
    }
}

/// The pages of `record`, by the path each is written at.
fn pages(record: &Documentation) -> impl Fn(&str) -> String {
    let directory = Temporary::new("document-pages", "pages");
    write_documentation(record, &directory).unwrap();

    move |page| fs::read_to_string(directory.join(page)).unwrap()
}

/// The cluster of `page` anchored `anchor`.
fn cluster<'a>(page: &'a str, anchor: &str) -> &'a str {
    let start = page
        .find(&format!("<article class=\"cluster\" id=\"{anchor}\">"))
        .unwrap_or_else(|| panic!("no cluster of `{anchor}`: {page}"));
    let end = page[start..].find("</article>").unwrap();

    &page[start..start + end]
}

#[test]
fn a_referent_is_found_where_it_is_drawn_whole() {
    let record = record();
    let bundle = Bundle::new(&record);
    let href = |depth, path: &[&str]| bundle.href(depth, &Qualifier::from(path.iter().copied()));

    // A declaration that is a module too is its path's page, whichever page exposes it, and its constructor is anchored there under a joint, which no entry's anchor begins with.
    assert_eq!(
        href(0, &["shapes", "Shape", "Shape"]).as_deref(),
        Some("Shape.crs.html")
    );
    assert_eq!(
        href(0, &["shapes", "Shape", "Shape", "circle"]).as_deref(),
        Some("Shape.crs.html#/circle")
    );
    assert_eq!(
        href(0, &["shapes", "Shape"]).as_deref(),
        Some("Shape.crs.html")
    );
    // Any other is an entry of the page of the module a consumer finds it in, its member beneath it.
    assert_eq!(
        href(0, &["shapes", "Area"]).as_deref(),
        Some("index.html#Area")
    );
    assert_eq!(
        href(0, &["shapes", "Area", "area"]).as_deref(),
        Some("index.html#Area/area")
    );
    assert_eq!(
        href(0, &["shapes", "geometry"]).as_deref(),
        Some("geometry.crs.html")
    );
    // The root exposes `origin` too, and the link lands where `geometry` draws it whole.
    assert_eq!(
        href(1, &["shapes", "geometry", "origin"]).as_deref(),
        Some("../geometry.crs.html#origin")
    );
    assert_eq!(href(0, &["shapes", "origin"]), None);
    // The facade: the record names the declaration for the page that shows it, and the mark the builder writes carries that same name.
    assert_eq!(
        href(0, &["shapes", "Token"]).as_deref(),
        Some("index.html#Token")
    );
    assert_eq!(
        href(1, &["shapes", "Token", "token"]).as_deref(),
        Some("../index.html#Token/token")
    );
    // A field is no path: the path of its owner and its label is the definition's, whose address the field does not take.
    assert_eq!(
        href(0, &["shapes", "Shape", "Corner"]).as_deref(),
        Some("Shape/Corner.crs.html")
    );
    assert_eq!(
        href(0, &["shapes", "Shape", "Corner", "x"]).as_deref(),
        Some("Shape/Corner.crs.html#x")
    );
    // The path it was written at is not an address: nothing renders it, so nothing may resolve it either.
    assert_eq!(href(0, &["shapes", "hidden", "Token"]), None);
    // A declaration nothing shows has no address, rather than an anchor no page defines.
    assert_eq!(href(0, &["shapes", "hidden", "unseen"]), None);
}

#[test]
fn a_page_carries_the_units_paths_and_no_script() {
    let record = record();
    let pages = pages(&record);
    let landing = pages("index.html");

    // The page runs nothing: the rail is its index.
    assert!(!landing.contains("<script"), "{landing}");
    // The page names its icon; the checkbox behind the chevron is kept from sight by the stylesheet and never by the markup, so it stays in the tab order.
    assert!(
        landing.contains("<link rel=\"icon\" href=\"static/mark.svg\" type=\"image/svg+xml\">"),
        "{landing}"
    );
    assert!(
        landing.contains(
            "<input type=\"checkbox\" id=\"rail-toggle\" class=\"toggle\" aria-label=\"Show the index\">"
        ),
        "{landing}"
    );
    // The rail lists the root's paths that have a page, each by what it is, the declaration at it or a module alone, and none of the paths beneath one the page is not on.
    assert!(
        landing.contains(concat!(
            "<a class=\"row\" style=\"--depth: 0\" href=\"Shape.crs.html\">",
            "<span class=\"kind\">induct</span><span class=\"label\">Shape</span></a>"
        )),
        "{landing}"
    );
    assert!(
        landing.contains(concat!(
            "<a class=\"row\" style=\"--depth: 0\" href=\"geometry.crs.html\">",
            "<span class=\"kind\">mod</span><span class=\"label\">geometry</span></a>"
        )),
        "{landing}"
    );
    assert!(
        !landing.contains("<span class=\"label\">Corner</span>"),
        "{landing}"
    );
    // On a page beneath the root every link climbs to it, the branch the page is on is open with its own paths under it, and the page's own row is marked.
    let corner = pages("Shape/Corner.crs.html");
    assert!(
        corner.contains("<a class=\"brand\" href=\"../index.html\">"),
        "{corner}"
    );
    assert!(
        corner.contains(concat!(
            "<h1><span class=\"sep\">/</span><a href=\"../index.html\">shapes</a>",
            "<span class=\"sep\">/</span><a href=\"../Shape.crs.html\">Shape</a>",
            "<span class=\"sep\">/</span>Corner</h1>"
        )),
        "{corner}"
    );
    assert!(
        corner.contains(concat!(
            "<a class=\"row open\" style=\"--depth: 0\" href=\"../Shape.crs.html\">",
            "<span class=\"kind\">induct</span><span class=\"label\">Shape</span></a>",
            "<a class=\"row open current\" style=\"--depth: 1\" href=\"../Shape/Corner.crs.html\">",
            "<span class=\"kind\">struct</span><span class=\"label\">Corner</span></a>",
            "<a class=\"row\" style=\"--depth: 0\" href=\"../geometry.crs.html\">"
        )),
        "{corner}"
    );
}

#[test]
fn a_page_opens_on_the_declaration_at_its_path_and_lists_each_thing_once() {
    let record = record();
    let pages = pages(&record);
    let shape = pages("Shape.crs.html");

    // The declaration the parent exposes at the path opens the page, whole: every paragraph of its note, and its members' notes and anchors. It is the page, so it takes no anchor of its own, and the module's own entry for it is not listed again.
    let subject = shape
        .split_once("<div class=\"subject\">")
        .expect("the page opens on a declaration")
        .1
        .trim_start();
    assert!(
        subject.starts_with("<article class=\"cluster\">"),
        "{shape}"
    );
    assert!(shape.contains("More of Shape."), "{shape}");
    assert!(
        shape.contains("<div class=\"member\" id=\"/circle\">"),
        "{shape}"
    );
    // What its source does not show is a word on its corner, drawn by what it says.
    assert!(
        subject.contains("<div class=\"facts\"><span class=\"fact plain\" title=\""),
        "{shape}"
    );
    assert!(subject.contains("\">transparent</span></div>"), "{shape}");
    assert!(shape.contains("What circle holds."), "{shape}");
    assert!(!shape.contains("id=\"Shape\""), "{shape}");
    assert_eq!(shape.matches("More of Shape.").count(), 1, "{shape}");
    // What the module says of itself stands under the heading, since the declaration does not say it.
    assert!(
        shape.contains("<p class=\"lead\">Shapes, and what measures them.</p>"),
        "{shape}"
    );
    // What the path exposes beside it: two names, each in brief and then in full.
    assert!(
        shape.contains("<h2>Exposes<span class=\"tally\">2\u{a0}names</span></h2>"),
        "{shape}"
    );
    assert!(
        shape.contains(concat!(
            "<li><a href=\"#area\"><span class=\"kind\">let</span><span class=\"label\">area</span></a>",
            "<span class=\"said\">What area is.</span></li>"
        )),
        "{shape}"
    );
    assert!(cluster(&shape, "area").contains("More of area."), "{shape}");
    // And beside the page, by name.
    assert!(
        shape.contains(
            "<a href=\"#Corner\"><span class=\"kind\">struct</span><span class=\"label\">Corner</span></a>"
        ),
        "{shape}"
    );
    // A structure that is a module too and declared on its parent's page is whole on its own: its field is anchored as a projection writes it, under the joint alone, and the definition its module holds at the same name keeps the name.
    let corner = pages("Shape/Corner.crs.html");
    assert!(
        corner.contains("<div class=\"member deep\" id=\".x\">"),
        "{corner}"
    );
    assert!(
        corner.contains("<article class=\"cluster\" id=\"x\">"),
        "{corner}"
    );
}

#[test]
fn a_name_whole_on_another_page_is_shown_by_its_source_and_the_way_there() {
    let record = record();
    let pages = pages(&record);
    let landing = pages("index.html");

    // A name that is a module too: its head and its block without its members' notes or anchors, the paragraph its note opens with, and the line that declares the module, with what the module exposes beside it, as the way to its page.
    let shape = cluster(&landing, "Shape");
    assert!(shape.contains("<div class=\"member\">"), "{shape}");
    assert!(!shape.contains("What circle holds."), "{shape}");
    assert!(shape.contains("What Shape is."), "{shape}");
    assert!(!shape.contains("More of Shape."), "{shape}");
    assert!(
        shape.contains(concat!(
            "<a class=\"door\" href=\"Shape.crs.html\"><span class=\"keyword\">pub mod</span> <b>Shape</b>",
            "<span class=\"count\">2\u{a0}names</span><span class=\"arrow\">→</span></a>"
        )),
        "{shape}"
    );
    assert!(!shape.contains("declared at"), "{shape}");
    // The same of one declared on this page, from a page beneath the root.
    let corner = pages("Shape.crs.html");
    let corner = cluster(&corner, "Corner");
    assert!(corner.contains("<div class=\"member deep\">"), "{corner}");
    assert!(
        corner.contains("<a class=\"door\" href=\"Shape/Corner.crs.html\">"),
        "{corner}"
    );
    // A name another module declares: the same, and the path that declares it.
    let origin = cluster(&landing, "origin");
    assert!(origin.contains("What origin is."), "{origin}");
    assert!(!origin.contains("More of origin."), "{origin}");
    assert!(
        origin.contains(
            "declared at <a href=\"geometry.crs.html#origin\">/shapes/geometry/origin</a>"
        ),
        "{origin}"
    );
    // A name the page is the address of is whole: all of its note, and its member's anchor. The concept's superclass edge prints as a member line and, being anonymous, carries no anchor.
    let area = cluster(&landing, "Area");
    assert!(area.contains("More of Area."), "{area}");
    assert!(
        area.contains("<div class=\"member deep\" id=\"Area/area\">"),
        "{area}"
    );
    assert!(
        !area.contains("id=\"Area/\""),
        "an anonymous member has no anchor: {area}"
    );
    assert!(!area.contains("class=\"ways\""), "{area}");
    // A module that is no declaration stands after the names, by the line that declares it, the paragraph its note opens with and what it exposes.
    let geometry = cluster(&landing, "geometry");
    assert!(
        geometry.contains(concat!(
            "<span class=\"keyword\">pub</span> <wbr><span class=\"keyword\">mod</span> <wbr>",
            "<b class=\"declared\">geometry</b>"
        )),
        "{geometry}"
    );
    assert!(geometry.contains("Points and distances."), "{geometry}");
    assert!(
        geometry.contains(concat!(
            "<a class=\"door\" href=\"geometry.crs.html\">",
            "<span class=\"count\">1\u{a0}name</span><span class=\"arrow\">→</span></a>"
        )),
        "{geometry}"
    );
    assert!(
        landing.find("id=\"origin\"").unwrap() < landing.find("id=\"geometry\"").unwrap(),
        "{landing}"
    );
    // The line that exposes constructors beside their owner is a cluster of the line alone: no anchor, and no row of a list, each constructor being a line of its owner's block. A line may fold after each of its spaces, and nowhere inside the path and its braces.
    assert!(
        landing.contains("<article class=\"cluster exposure\">"),
        "{landing}"
    );
    assert!(
        landing.contains("pub <wbr>use <wbr>Shape/{circle, <wbr>square}"),
        "{landing}"
    );
    assert!(!landing.contains("href=\"#circle\""), "{landing}");
    // The count says the names and the modules apart, each constructor among the names.
    assert!(
        landing
            .contains("<h2>Exposes<span class=\"tally\">6\u{a0}names · 1\u{a0}module</span></h2>"),
        "{landing}"
    );
}

#[test]
fn a_name_whole_on_a_page_that_opens_on_it_is_said_by_that_pages_path() {
    let mut record = record();
    // `geometry` exposes `Shape` too, which the module `Shape` declares and whose page opens on it.
    record.modules[3]
        .entries
        .push(Entry::Declaration(declaration(
            Kind::Inductive,
            "Shape",
            &["shapes", "Shape", "Shape"],
            &["circle", "square"],
        )));
    let geometry = pages(&record)("geometry.crs.html");

    assert!(
        geometry.contains("declared at <a href=\"Shape.crs.html\">/shapes/Shape</a>"),
        "{geometry}"
    );
}

#[test]
fn a_witness_stands_on_the_page_of_what_it_takes_part_in() {
    let record = record();
    let pages = pages(&record);
    let shape = pages("Shape.crs.html");

    // The page of `Shape` lists the witnesses it takes part in, wherever they are declared, each anchored by its head and the module that declares it, which the row names.
    assert!(
        shape.contains(
            "<h2 id=\"satisfy\">Witnesses<span class=\"tally\">2\u{a0}in scope</span></h2>"
        ),
        "{shape}"
    );
    assert!(
        shape.contains("<li id=\"satisfyShow(Shape)@/shapes\">"),
        "{shape}"
    );
    assert!(
        shape.contains(
            "<a class=\"where\" href=\"index.html#satisfyShow(Shape)@/shapes\">/shapes</a>"
        ),
        "{shape}"
    );
    assert!(
        shape.contains(
            "<a class=\"where\" href=\"geometry.crs.html#satisfyArea(Shape)@/shapes/geometry\">/shapes/geometry</a>"
        ),
        "{shape}"
    );
    assert!(
        shape.contains("<a class=\"section\" href=\"#satisfy\">Witnesses</a>"),
        "{shape}"
    );
    // A page that opens on no declaration lists the witnesses its module declares, which need not say where.
    let geometry = pages("geometry.crs.html");
    assert!(
        geometry.contains("<h2 id=\"satisfy\">Witnesses<span class=\"tally\">1</span></h2>"),
        "{geometry}"
    );
    assert!(
        geometry.contains("<li id=\"satisfyArea(Shape)@/shapes/geometry\">"),
        "{geometry}"
    );
    assert!(!geometry.contains("class=\"where\""), "{geometry}");
    // A witness is anonymous and stays out of the list of what the page holds.
    assert!(
        !geometry.contains("href=\"#satisfyArea(Shape)@/shapes/geometry\""),
        "{geometry}"
    );
}

#[test]
fn a_page_with_both_kinds_of_witness_lists_them_apart() {
    let mut record = record();
    // The module `Shape` declares a witness `Shape` takes part in, whose body the compiler writes, and one of something else.
    record.modules[1]
        .entries
        .push(Entry::Declaration(Declaration {
            derived: true,
            ..witness(
                &["shapes", "Shape"],
                "satisfy Hash(Shape)",
                &[&["std", "Hash", "Hash"], &["shapes", "Shape", "Shape"]],
            )
        }));
    record.modules[1].entries.push(Entry::Declaration(witness(
        &["shapes", "Shape"],
        "satisfy Show(Corner)",
        &[&["std", "Show", "Show"], &["shapes", "Shape", "Corner"]],
    )));
    let shape = pages(&record)("Shape.crs.html");

    assert!(
        shape.contains(
            "<h2 id=\"satisfy\">Witnesses<span class=\"tally\">3\u{a0}in scope</span></h2>"
        ),
        "{shape}"
    );
    // One this page's module declares names it without a link, which would lead to the row itself, and says the compiler wrote its body.
    assert!(
        shape.contains(concat!(
            "<span class=\"aside\"><span class=\"fact\" title=\"the compiler writes its body\">derived</span>",
            "<span class=\"where\">/shapes/Shape</span></span>"
        )),
        "{shape}"
    );
    assert_eq!(shape.matches(">derived</span>").count(), 1, "{shape}");
    assert!(
        shape.contains(
            "<h2 id=\"satisfy-here\">Witnesses declared here<span class=\"tally\">1</span></h2>"
        ),
        "{shape}"
    );
    assert!(
        shape
            .find("id=\"satisfyHash(Shape)@/shapes/Shape\"")
            .unwrap()
            < shape.find("id=\"satisfy-here\"").unwrap(),
        "{shape}"
    );
    assert!(
        shape.find("id=\"satisfy-here\"").unwrap()
            < shape
                .find("id=\"satisfyShow(Corner)@/shapes/Shape\"")
                .unwrap(),
        "{shape}"
    );
    assert!(
        shape.contains("<a class=\"section\" href=\"#satisfy-here\">Witnesses declared here</a>"),
        "{shape}"
    );
}

#[test]
fn a_path_counts_the_names_and_the_modules_it_exposes() {
    let record = record();
    let bundle = Bundle::new(&record);
    let counts = |index: usize| bundle.counts(&record.modules[index]);

    // A count and its word are joined by a no-break space, so the two never part at a line's end; a line that exposes two constructors counts two names, the declaration a page opens on is not among what its path exposes, and a count of none is not said.
    assert_eq!(counts(0), "6\u{a0}names · 1\u{a0}module");
    assert_eq!(counts(1), "2\u{a0}names");
    assert_eq!(counts(2), "1\u{a0}name");
}

#[test]
fn what_a_module_says_of_itself_is_said_where_its_declaration_does_not() {
    let mut record = record();
    record.description = Some("Shapes, for a `page`.".to_string());
    // The module's note is the paragraph its declaration's opens with.
    record.modules[1].prose = Some(vec!["What Shape is.".to_string()]);
    let pages = pages(&record);
    // The landing page opens with what the unit is.
    let landing = pages("index.html");
    assert!(
        landing.contains("<p class=\"lead\">Shapes, for a <code>page</code>.</p>"),
        "{landing}"
    );
    let shape = pages("Shape.crs.html");

    assert!(!shape.contains("<p class=\"lead\">"), "{shape}");
    assert_eq!(shape.matches("What Shape is.").count(), 1, "{shape}");
    // A module that is no declaration says it of itself.
    let geometry = pages("geometry.crs.html");
    assert!(
        geometry.contains("<p class=\"lead\">Points and distances.</p>"),
        "{geometry}"
    );
}

#[test]
fn the_list_of_what_a_page_holds_stands_in_view_until_it_is_too_long() {
    let page = |names: usize, witnesses: usize| {
        let prefix = Qualifier::from(["many"]);
        let record = Documentation {
            prefix,
            description: None,
            modules: vec![ModuleDocumentation {
                path: prefix,
                prose: None,
                children: Vec::new(),
                entries: (0..names)
                    .map(|index| {
                        let name = format!("n{index}");
                        Entry::Declaration(declaration(
                            Kind::Definition,
                            &name,
                            &["many", &name],
                            &[],
                        ))
                    })
                    .chain((0..witnesses).map(|index| {
                        Entry::Declaration(witness(
                            &["many"],
                            &format!("satisfy Show(n{index})"),
                            &[],
                        ))
                    }))
                    .collect(),
            }],
        };

        pages(&record)("index.html")
    };

    // A list that fits a short window stays beside the page as it scrolls; a longer one stands at the page's top, so no list scrolls on its own. The witnesses' section is a row of it.
    assert!(page(22, 0).contains("<aside class=\"holds\">"));
    assert!(page(23, 0).contains("<aside class=\"holds long\">"));
    assert!(page(21, 3).contains("<aside class=\"holds\">"));
    assert!(page(22, 3).contains("<aside class=\"holds long\">"));
    // One name needs no list before it.
    assert!(!page(1, 0).contains("<ul class=\"brief\">"));
    assert!(page(2, 0).contains("<ul class=\"brief\">"));
}

#[test]
fn a_line_is_written_past_the_spaces_its_printer_indented_it_by() {
    let prefix = Qualifier::from(["lines"]);
    let record = Documentation {
        prefix,
        description: None,
        modules: vec![ModuleDocumentation {
            path: prefix,
            prose: None,
            children: Vec::new(),
            entries: vec![Entry::Declaration(Declaration {
                signature: signature("let pair(\n    left: Nat,\n    right: Nat,\n) -> Nat"),
                ..declaration(Kind::Definition, "pair", &["lines", "pair"], &[])
            })],
        }],
    };
    let page = pages(&record)("index.html");

    // The spaces are the source's, so a signature copied off a page is indented as it is written; a line may fold after any other space, under an indent the stylesheet reads off the line.
    for line in [
        "<div class=\"line\" style=\"--indent: 0\">let <wbr>pair(</div>",
        "<div class=\"line\" style=\"--indent: 4\">    left: <wbr>Nat,</div>",
        "<div class=\"line\" style=\"--indent: 4\">    right: <wbr>Nat,</div>",
        "<div class=\"line\" style=\"--indent: 0\">) <wbr>-&#62; <wbr>Nat</div>",
    ] {
        assert!(page.contains(line), "{line} is not in {page}");
    }
}

#[test]
fn a_page_opening_on_another_modules_declaration_is_not_its_address() {
    let prefix = Qualifier::from(["r"]);
    let made = || declaration(Kind::Inductive, "X", &["r", "a", "X"], &["x"]);
    // The root exposes `X`, which the module `a` declares, above a module `X` of its own that only shares the name.
    let record = Documentation {
        prefix,
        description: None,
        modules: vec![
            ModuleDocumentation {
                path: prefix,
                prose: None,
                children: vec![Qualifier::from(["r", "a"]), Qualifier::from(["r", "X"])],
                entries: vec![Entry::Declaration(made())],
            },
            ModuleDocumentation {
                path: Qualifier::from(["r", "X"]),
                prose: None,
                children: Vec::new(),
                entries: Vec::new(),
            },
            ModuleDocumentation {
                path: Qualifier::from(["r", "a"]),
                prose: None,
                children: Vec::new(),
                entries: vec![Entry::Declaration(made())],
            },
        ],
    };
    let bundle = Bundle::new(&record);
    let href = |path: &[&str]| bundle.href(0, &Qualifier::from(path.iter().copied()));

    // The page of `/r/X` opens on the declaration, as its path names it, and a link to the declaration still lands where its own module draws it.
    assert_eq!(href(&["r", "a", "X"]).as_deref(), Some("a.crs.html#X"));
    assert_eq!(
        href(&["r", "a", "X", "x"]).as_deref(),
        Some("a.crs.html#X/x")
    );
    assert!(pages(&record)("X.crs.html").contains("<div class=\"subject\">"));
}
