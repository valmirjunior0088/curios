use {
    super::*,
    crate::{
        Definition, DefinitionKind, Free, Global, Items, Outlines, Term, Totality, UniverseContext,
    },
    curios_archive::Bytes,
    curios_utilities::{Member, Object, Qualifier},
};

/// A definition `name` of the ground type, whose body is `body`.
fn item(name: &str, body: Term) -> Item {
    Item::Let(Box::new(Definition {
        name: Global::Authored(Qualifier::from([name])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        value_sort: None,
        type_: Term::type_ground(),
        body,
    }))
}

/// Two items spelling one structure apiece, and a third.
fn items() -> Vec<Item> {
    let shared = || Term::tuple([Term::type_ground(), Term::type_ground()]);

    vec![
        item("first", shared()),
        item("second", shared()),
        item(
            "third",
            Term::free_var(&Free::Global(Global::Authored(Qualifier::from(["first"])))),
        ),
    ]
}

/// `items` stored as the members of one run held in `bytes`, each named by its identity.
fn stored(items: &[Item], bytes: Bytes) -> Items {
    let parts = items
        .iter()
        .enumerate()
        .map(|(member, item)| Object {
            digest: ItemRun::identity(item).expect("an item archives"),
            bytes: bytes.clone(),
            member: Some(Member {
                kind: ItemRun::KIND,
                index: u32::try_from(member).expect("a few members"),
            }),
        })
        .collect();

    Items::parted(Outlines::of(&Items::from(items.to_vec())), parts).expect("a part for each item")
}

/// Every member of a run read for one unit is restored as the item it was, out of the run read whole.
#[test]
fn a_member_of_a_run_read_is_the_item_it_was() {
    let items = items();
    let run = ItemRun::of(&items).expect("a run archives");
    let stored = stored(&items, Bytes::from(run));

    for (place, item) in items.iter().enumerate() {
        assert_eq!(&stored[place], item);
    }
}

/// Every member of a run that is a constant of the executable is restored as the item it was, through the view checked once.
#[test]
fn a_member_of_a_constant_run_is_the_item_it_was() {
    let items = items();
    let run: &'static [u8] = ItemRun::of(&items).expect("a run archives").leak();
    let stored = stored(&items, Bytes::Constant(run));

    for (place, item) in items
        .iter()
        .rev()
        .enumerate()
        .map(|(at, item)| (items.len() - 1 - at, item))
    {
        assert_eq!(&stored[place], item);
    }
}

/// An item is named by its form alone: restored out of a run it shared a table with, it is named as it was before it was stored.
#[test]
fn an_item_is_named_alike_whatever_it_was_stored_beside() {
    let items = items();
    let stored = stored(
        &items,
        Bytes::from(ItemRun::of(&items).expect("a run archives")),
    );

    for (place, item) in items.iter().enumerate() {
        assert_eq!(ItemRun::identity(&stored[place]), ItemRun::identity(item));
    }
    assert_ne!(ItemRun::identity(&items[0]), ItemRun::identity(&items[1]));
}

/// Members read alone out of a run are the items it was made of, in the order they were asked for, and a member the run does not hold is refused.
#[test]
fn members_read_alone_are_the_items_the_run_holds() {
    let items = items();
    let run = ItemRun::of(&items).expect("a run archives");

    assert_eq!(
        ItemRun::members(&run, &[2, 0]).expect("the run holds both"),
        vec![items[2].clone(), items[0].clone()]
    );
    assert!(ItemRun::members(&run, &[3]).is_err());
}
