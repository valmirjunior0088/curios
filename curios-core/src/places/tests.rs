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
        parameter_field: None,
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

/// A version made of a module that placed its items at their indices has room before its first item, where an item named before every other goes.
#[test]
fn a_version_takes_an_item_before_its_first() {
    let module = module(vec![Item::Let(definition("b")), Item::Let(definition("c"))]);
    let mut items = module.items.version();

    let first = items.insert_after(None, Item::Let(definition("a")));

    assert!(first.is_some(), "a place before the first item");
    assert_eq!(first, items.places().next());
}

/// What a version changed of the items it was made from is what it replaced, put between two others and took out, and nothing it shares with them; items that are no version say nothing of what they share.
#[test]
fn a_version_says_what_it_changed_of_its_items() {
    let module = module(vec![
        Item::Let(definition("first")),
        Item::Let(definition("second")),
        Item::Let(definition("third")),
    ]);
    let earlier = module.items.version();
    let places = earlier.places().collect::<Vec<_>>();
    let mut later = earlier.clone();
    later.replace(places[1], Item::Let(definition("second")));
    later.remove(places[2]);
    let between = later
        .insert_after(Some(places[0]), Item::Let(definition("between")))
        .expect("a place between two");

    let changes = later.changed_since(&earlier).expect("two versions");

    assert_eq!(changes.replaced, [places[1]]);
    assert_eq!(changes.inserted, [between]);
    assert_eq!(changes.removed, [places[2]]);
    assert_eq!(module.items.changed_since(&earlier), None);
}
