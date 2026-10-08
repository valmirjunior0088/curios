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
        type_: Term::type_ground(),
        body: Term::free_var(&Free::from(&global)),
    }
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items,
        ..Module::default()
    }
}

/// A name is held where its item sits, a group's member by its member and by a slot counted across the module's groups.
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
    assert_eq!(
        place("odd"),
        Some(Place::Member {
            item: 1,
            member: 1,
            slot: 1,
        }),
    );
    assert_eq!(
        place("loop"),
        Some(Place::Member {
            item: 2,
            member: 0,
            slot: 2,
        }),
    );
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
