use {super::*, crate::*, curios_common::Qualifier};

fn name(path: &[&str]) -> Global {
    Global::Authored(Qualifier::from(path.iter().copied()))
}

fn written(path: &[&str]) -> Item {
    let name = name(path);

    Item::Let(Box::new(Definition {
        name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        proof: false,
        value_sort: None,
        type_: Term::type_ground(),
        body: Term::instance(name, Universes::default()),
    }))
}

/// The former of `owner`, a family in the root with a case of no payload for each of `tags`.
fn family(owner: &str, tags: &[&str]) -> Item {
    let case = |tag: &&str| {
        (
            Atom::from(*tag),
            InductParam::over(
                &[],
                Telescope::build(Vec::<(Free, Term)>::new(), Vec::new()),
            ),
        )
    };
    let declaration = InductDecl::new(
        UniverseContext::empty(),
        Telescope::build(
            Vec::<(Free, Term)>::new(),
            Telescope::build(Vec::<(Free, Term)>::new(), ()),
        ),
        tags.iter().map(case).collect(),
        Term::type_ground(),
        Qualifier::empty(),
        false,
    );

    Item::Let(Box::new(Definition::former(
        name(&[owner]),
        Qualifier::empty(),
        Nominal::Inductive(declaration),
    )))
}

fn definitions(items: Vec<Item>) -> Definitions {
    Definitions {
        items: items.into(),
    }
}

/// A declaration is one item with every name it declares: a family's former and its constructors, which no item of their own holds. A name is found at its item, which is its declaration, and the declarations are read back in the order their items stand.
#[test]
fn a_declaration_is_its_item_with_every_name_it_declares() {
    let declarations = Declarations::of(&definitions(vec![
        family("Bit", &["zero", "one"]),
        written(&["length"]),
    ]));

    assert_eq!(declarations.each().collect::<Vec<_>>(), [0, 1]);
    assert_eq!(**declarations.items(0), [0]);
    assert_eq!(**declarations.items(1), [1]);
    assert_eq!(declarations.item(&name(&["Bit", "one"])), Some(0));
    assert_eq!(declarations.declaration(&name(&["Bit", "zero"])), Some(0));
    assert_eq!(declarations.declaration(&name(&["length"])), Some(1));
    assert_eq!(declarations.declaration(&name(&["absent"])), None);
}

/// Every declaration of `module`, by its item's place, with its items and the declaration each name they declare is found in: what two tables are compared by.
fn read(
    declarations: &Declarations,
    definitions: &Definitions,
) -> Vec<(usize, Vec<usize>, Vec<Option<usize>>)> {
    declarations
        .each()
        .map(|declaration| {
            let items = declarations.items(declaration).to_vec();
            let found = items
                .iter()
                .flat_map(|item| definitions.items.names(*item))
                .map(|name| declarations.declaration(&name))
                .collect();
            (declaration, items, found)
        })
        .collect()
}

/// Declarations revised by what a version of their module changed are those the version makes: a family made again with a case more declares it, and one with a case fewer no longer does; a declaration taken out takes its names with it.
#[test]
fn a_revised_table_is_the_one_its_version_makes() {
    let earlier = definitions(vec![
        family("List", &["empty", "cons"]),
        written(&["length"]),
        family("Bit", &["zero", "one"]),
        written(&["gone"]),
    ]);
    let mut version = earlier.clone();
    version.items = version.items.version();
    let declarations = Declarations::of(&version);
    let places = version.items.places().collect::<Vec<_>>();

    let mut removed = Vec::new();
    let mut placed = Vec::new();
    // A family made again with a case more, at its place.
    removed.push((places[0], version.items.names(places[0])));
    version
        .items
        .replace(places[0], family("List", &["empty", "cons", "snoc"]));
    placed.push(places[0]);
    // A family made again with a case fewer.
    removed.push((places[2], version.items.names(places[2])));
    version.items.replace(places[2], family("Bit", &["zero"]));
    placed.push(places[2]);
    // A declaration taken out.
    removed.push((places[3], version.items.names(places[3])));
    version.items.remove(places[3]);

    let mut revised = declarations.clone();
    revised.revise(&version.items, &removed, &placed);
    let made = Declarations::of(&version);

    assert_eq!(read(&revised, &version), read(&made, &version));
    assert_eq!(revised.declaration(&name(&["gone"])), None);
    assert_eq!(revised.declaration(&name(&["Bit", "one"])), None);
    assert_eq!(
        revised.declaration(&name(&["List", "snoc"])),
        Some(places[0])
    );
}
