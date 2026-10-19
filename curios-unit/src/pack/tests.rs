use {
    super::*,
    curios_core::{Definition, DefinitionKind, Global, Term, Totality, UniverseContext},
    curios_utilities::Qualifier,
    std::io::Cursor,
};

/// One object of its own bytes.
fn whole(bytes: &[u8]) -> Object {
    Object::of(bytes.to_vec())
}

/// Every object a pack holds is found by its name, as the bytes it was packed as, and a name it does not hold finds nothing.
#[test]
fn an_object_is_found_by_its_name() {
    let objects = [b"one".as_slice(), b"two", b"three"].map(whole);
    let pack = Pack::read(Bytes::from(packed(&objects))).expect("a pack reads back");

    assert_eq!(pack.len(), 3);
    for object in &objects {
        let found = pack.get(&object.digest).expect("an object packed");
        assert_eq!(found.bytes.as_slice(), object.bytes.as_slice());
        assert_eq!(found.member, None);
    }
    assert!(pack.get(&Digest::of(b"four")).is_none());
}

/// Which objects a pack holds is read off its end, in the order of their names, and a file that closes on no table names none.
#[test]
fn what_a_pack_holds_is_read_off_its_end() {
    let objects = [b"one".as_slice(), b"two", b"three"].map(whole);
    let bytes = packed(&objects);
    let mut names = objects
        .iter()
        .map(|object| object.digest)
        .collect::<Vec<_>>();
    names.sort();

    assert_eq!(pack_names(&mut Cursor::new(&bytes)), Some(names));
    assert_eq!(
        pack_names(&mut Cursor::new(b"not a pack, longer than its end")),
        None
    );
    assert_eq!(pack_names(&mut Cursor::new(&bytes[..8])), None);
}

/// A pack is a function of what it holds: the same objects in another order, or one handed twice, pack to the same bytes and so the same name.
#[test]
fn the_same_objects_pack_to_the_same_name() {
    let one = packed(&[whole(b"a"), whole(b"b"), whole(b"c")]);
    let other = packed(&[whole(b"c"), whole(b"a"), whole(b"b"), whole(b"a")]);

    assert_eq!(pack_name(&one), pack_name(&other));
    assert_eq!(one, other);
}

/// Members of one run are written once, each found as the run with which member it is.
#[test]
fn members_of_one_run_share_its_bytes() {
    let run = Bytes::from(b"a run of two members".to_vec());
    let member = |form: &[u8], member| Object {
        digest: Digest::of(form),
        bytes: run.clone(),
        member: Some(member),
    };
    let bytes = packed(&[member(b"first", 0), member(b"second", 1), whole(b"alone")]);
    let pack = Pack::read(Bytes::from(bytes.clone())).expect("a pack reads back");

    let second = pack.get(&Digest::of(b"second")).expect("a member packed");
    assert_eq!(second.bytes.as_slice(), run.as_slice());
    assert_eq!(second.member, Some(1));
    assert_eq!(
        bytes
            .windows(run.as_slice().len())
            .filter(|window| *window == run.as_slice())
            .count(),
        1,
        "the run is written once"
    );
}

/// Bytes that are no pack are refused: another file, one cut short, or a table naming bytes past where the objects end.
#[test]
fn bytes_that_are_no_pack_are_refused() {
    let bytes = packed(&[whole(b"one"), whole(b"two")]);

    assert!(Pack::read(Bytes::from(b"not a pack".to_vec())).is_none());
    assert!(Pack::read(Bytes::from(bytes[..bytes.len() - 1].to_vec())).is_none());
    assert!(Pack::read(Bytes::from(bytes[20..].to_vec())).is_none());
}

/// A definition `name` of the ground type, its body the ground type too.
fn item(name: &str) -> Item {
    Item::Let(Definition {
        name: Global::Authored(Qualifier::from([name])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        type_: Term::type_ground(),
        body: Term::type_ground(),
    })
}

/// A pack of `items`, each a member of one run, beside `alone`.
fn pack_of(items: &[Item], alone: &[Object]) -> Pack {
    let run = Bytes::from(ItemRun::of(items).expect("a run archives"));
    let mut objects = alone.to_vec();
    for (member, item) in items.iter().enumerate() {
        objects.push(Object {
            digest: ItemRun::identity(item).expect("an item archives"),
            bytes: run.clone(),
            member: Some(u32::try_from(member).expect("a few members")),
        });
    }

    Pack::read(Bytes::from(packed(&objects))).expect("a pack reads back")
}

/// Packs merged hold every object each held: one stored alone as it was, and every member in one run consed again, each found as the item it was.
#[test]
fn a_merge_conses_its_runs_into_one() {
    let (first, second) = ([item("a"), item("b")], [item("c")]);
    let alone = [whole(b"alone")];
    let merged = repacked([&pack_of(&first, &alone), &pack_of(&second, &[])]).expect("packs merge");
    let merged = Pack::read(Bytes::from(merged)).expect("a merge is a pack");

    assert_eq!(merged.len(), 4);
    let found = merged.get(&alone[0].digest).expect("an object alone");
    assert_eq!(found.bytes.as_slice(), b"alone");
    let members = first
        .iter()
        .chain(&second)
        .map(|item| {
            let digest = ItemRun::identity(item).expect("an item archives");
            (item, merged.get(&digest).expect("a member merged"))
        })
        .collect::<Vec<_>>();
    let run = members[0].1.bytes.as_slice();
    for (item, member) in &members {
        assert_eq!(member.bytes.as_slice(), run, "one run holds every member");
        let read = curios_archive::from_bytes::<ItemRun>(member.bytes.as_slice())
            .expect("a run reads back");
        let at = usize::try_from(member.member.expect("a member")).expect("a small member");
        assert_eq!(&read.items[at], *item);
    }
}
