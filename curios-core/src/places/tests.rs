use {
    super::*,
    crate::{Definition, DefinitionKind, Free, Totality, UniverseContext},
    curios_utilities::Qualifier,
};

fn definition(name: &str) -> Definition {
    let global = Global::Authored(Qualifier::from([name]));

    Definition {
        name: global,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        type_: Term::type_ground(),
        body: Term::free_var(&Free::from(&global)),
    }
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items: items.into(),
        ..Module::default()
    }
}

/// A name is held where its item sits, a group's member by its member.
#[test]
fn a_name_is_placed_at_the_item_and_the_member_that_declare_it() {
    let module = module(vec![
        Item::Let(definition("first")),
        Item::Rec(RecItem::new(vec![definition("even"), definition("odd")])),
        Item::Rec(RecItem::new(vec![definition("loop")])),
    ]);
    let places = Places::of(&module);
    let place = |name: &str| places.place(&Global::Authored(Qualifier::from([name])));

    assert_eq!(place("first"), Some(Place::Let(0)));
    assert_eq!(place("odd"), Some(Place::Member { item: 1, member: 1 }),);
    assert_eq!(place("loop"), Some(Place::Member { item: 2, member: 0 }),);
    assert_eq!(place("absent"), None);
}

/// A table of a module declaring a name twice would hold one of the two declarations for every reader, so none is made of it.
///
/// Mutation-checked against the assertion: with the later declaration left standing, a table is made.
#[test]
#[should_panic(expected = "a module declares one name twice")]
fn a_module_declaring_a_name_twice_has_no_places() {
    Places::of(&module(vec![
        Item::Let(definition("twice")),
        Item::Rec(RecItem::new(vec![definition("other"), definition("twice")])),
    ]));
}

/// A version of a module holds every item it did not change at the place it had, and an item new to it at a place between two others; a version of a version keeps every place, and its places follow from its module's with the names of what changed placed again.
#[test]
fn a_version_keeps_the_places_of_what_it_did_not_change() {
    let name = |name: &str| Global::Authored(Qualifier::from([name]));
    let module = module(vec![
        Item::Let(definition("first")),
        Item::Let(definition("second")),
        Item::Let(definition("third")),
    ]);
    let mut version = Module {
        items: module.items.version(),
        ..module.clone()
    };
    let first = version.items.places().next().expect("an item");
    let between = version
        .items
        .insert_after(Some(first), Item::Let(definition("between")))
        .expect("a place between two");
    let held = Places::of(&version);
    let names = |module: &Module| {
        module
            .items
            .iter()
            .flat_map(Item::declared_names)
            .copied()
            .collect::<Vec<_>>()
    };
    let named = |names: &[&str]| names.iter().map(|each| name(each)).collect::<Vec<_>>();
    assert_eq!(
        names(&version),
        named(&["first", "between", "second", "third"])
    );
    assert_eq!(
        held.place(&name("between")),
        Some(Place::Let(between as u64))
    );

    let mut later = Module {
        items: version.items.version(),
        ..version.clone()
    };
    let second = held.place(&name("second")).expect("placed").item();
    later.items.remove(second);
    let mut carried = held.clone();
    carried.forget(&name("second"));

    assert_eq!(names(&later), named(&["first", "between", "third"]));
    assert_eq!(
        names(&version),
        named(&["first", "between", "second", "third"])
    );
    assert_eq!(carried.place(&name("third")), held.place(&name("third")));
    assert_eq!(carried.place(&name("second")), None);
    assert_eq!(
        Places::of(&later).place(&name("third")),
        held.place(&name("third"))
    );
}
