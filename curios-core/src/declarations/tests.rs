use {
    super::*,
    crate::{Atom, Definition, DefinitionKind, Free, Item, Term, Totality, UniverseContext},
    curios_utilities::Qualifier,
};

fn name(path: &[&str]) -> Global {
    Global::Authored(Qualifier::from(path.iter().copied()))
}

fn item(path: &[&str], kind: DefinitionKind) -> Item {
    let name = name(path);

    Item::Let(Definition {
        name,
        kind,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        type_: Term::type_ground(),
        body: Term::free_var(&Free::from(&name)),
    })
}

fn written(path: &[&str]) -> Item {
    item(path, DefinitionKind::Authored)
}

fn constructor(owner: &str, tag: &str) -> Item {
    item(
        &[owner, tag],
        DefinitionKind::InductiveConstructor {
            owner: Qualifier::from([owner]),
            tag: Atom::from(tag),
        },
    )
}

fn method(owner: &str, label: &str) -> Item {
    item(
        &[owner, label],
        DefinitionKind::ConceptMethod {
            owner: Qualifier::from([owner]),
        },
    )
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items: items.into(),
        ..Module::default()
    }
}

/// A declaration is its written item with every item generated from it, wherever each stands: a concept's method wrappers written ahead of it, a family's constructors with another declaration between them. A name is found at its own item and in its declaration, and the declarations are read back in the order their written items stand.
#[test]
fn a_declaration_holds_its_written_item_and_what_was_generated_from_it() {
    let declarations = Declarations::of(&module(vec![
        method("Show", "show"),
        written(&["Show"]),
        written(&["List"]),
        constructor("List", "empty"),
        written(&["length"]),
        constructor("List", "cons"),
    ]));

    assert_eq!(declarations.each().collect::<Vec<_>>(), [1, 2, 4]);
    assert_eq!(**declarations.items(1), [1, 0]);
    assert_eq!(**declarations.items(2), [2, 3, 5]);
    assert_eq!(**declarations.items(4), [4]);
    assert_eq!(declarations.item(&name(&["Show", "show"])), Some(0));
    assert_eq!(declarations.declaration(&name(&["Show", "show"])), Some(1));
    assert_eq!(declarations.declaration(&name(&["List", "cons"])), Some(2));
    assert_eq!(declarations.declaration(&name(&["absent"])), None);
}

/// Every declaration of `module`, by its written item's place, with its items and the names each declares: what two tables are compared by.
fn read(
    declarations: &Declarations,
    module: &Module,
) -> Vec<(usize, Vec<usize>, Vec<Option<usize>>)> {
    declarations
        .each()
        .map(|declaration| {
            let items = declarations.items(declaration).to_vec();
            let found = items
                .iter()
                .flat_map(|item| module.items.names(*item))
                .map(|name| declarations.declaration(name))
                .collect();
            (declaration, items, found)
        })
        .collect()
}

/// Declarations revised by what a version of their module changed are those the version makes: a former replaced in place keeps its constructors, a constructor added joins its former, a declaration taken out takes its names with it.
#[test]
fn a_revised_table_is_the_one_its_version_makes() {
    let earlier = module(vec![
        written(&["List"]),
        constructor("List", "empty"),
        written(&["length"]),
        constructor("List", "cons"),
        written(&["gone"]),
    ]);
    let mut version = earlier.clone();
    version.items = version.items.version();
    let declarations = Declarations::of(&version);
    let places = version.items.places().collect::<Vec<_>>();

    let mut removed = Vec::new();
    let mut placed = Vec::new();
    // The former written again, at its place.
    version.items.replace(places[0], written(&["List"]));
    removed.push((places[0], vec![name(&["List"])]));
    placed.push(places[0]);
    // A constructor added after `length`.
    let added = version
        .items
        .insert_after(Some(places[2]), constructor("List", "snoc"))
        .expect("room after a place");
    placed.push(added);
    // A declaration taken out.
    version.items.remove(places[4]);
    removed.push((places[4], vec![name(&["gone"])]));

    let mut revised = declarations.clone();
    revised.revise(&version.items, &removed, &placed);
    let made = Declarations::of(&version);

    assert_eq!(read(&revised, &version), read(&made, &version));
    assert_eq!(revised.declaration(&name(&["gone"])), None);
    assert_eq!(
        revised.declaration(&name(&["List", "snoc"])),
        Some(places[0])
    );
}

/// An item generated from a declaration the module does not hold is a declaration of its own.
#[test]
fn an_item_generated_from_a_declaration_the_module_does_not_hold_is_its_own() {
    let declarations = Declarations::of(&module(vec![constructor("Elsewhere", "case")]));

    assert_eq!(declarations.each().collect::<Vec<_>>(), [0]);
    assert_eq!(
        declarations.declaration(&name(&["Elsewhere", "case"])),
        Some(0)
    );
}
