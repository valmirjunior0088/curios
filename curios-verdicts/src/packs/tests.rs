use {super::*, curios_utilities::test_support::Temporary};

/// One object of its own bytes.
fn object(text: &str) -> Object {
    Object::of(text.as_bytes().to_vec())
}

/// The time every handle here is opened at, unless a test moves it.
fn now() -> SystemTime {
    SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000_000)
}

/// The time `days` days after [`now`].
fn after(days: u64) -> SystemTime {
    now() + Duration::from_secs(days * 24 * 60 * 60)
}

/// An object written is found by its name as the bytes it was written as, and a name nothing wrote is missing.
#[test]
fn an_object_written_is_found_and_one_never_written_is_missing() {
    let directory = Temporary::new("packs", "found");
    let packs = Packs::at(directory.to_path_buf(), now());
    let written = [object("one"), object("two")];
    packs.write(&written).expect("a pack is written");

    let found = Packs::at(directory.to_path_buf(), now())
        .find(&written[1].digest)
        .expect("an object written");
    assert_eq!(found.bytes.as_slice(), b"two");
    let never = object("three");
    assert_eq!(
        packs.missing([&written[0].digest, &never.digest, &written[1].digest]),
        vec![never.digest]
    );
}

/// A filing at a time, the address stays geometric: however many packs were written, it holds no more than a logarithm of the objects, each still found, and another handle on the directory finds them too.
#[test]
fn filings_one_at_a_time_keep_the_packs_geometric() {
    let directory = Temporary::new("packs", "geometric");
    let packs = Packs::at(directory.to_path_buf(), now());
    let mut written = Vec::new();
    for at in 0..100 {
        let filed = vec![object(&format!("object {at}"))];
        packs.write(&filed).expect("a pack is written");
        written.extend(filed);
    }

    let held = packs.list();
    assert!(held.len() <= 8, "{} packs hold 100 objects", held.len());
    let another = Packs::at(directory.to_path_buf(), now());
    for object in &written {
        let found = another
            .find(&object.digest)
            .expect("every object written is held");
        assert_eq!(found.bytes.as_slice(), object.bytes.as_slice());
    }
}

/// The candidates made from one input are found together, whichever pack each was written in, and none made from another input or of another kind of object is.
#[test]
fn candidates_of_an_input_are_found_across_packs() {
    let directory = Temporary::new("packs", "candidates");
    let packs = Packs::at(directory.to_path_buf(), now());
    let (input, other) = (Digest::of(b"input"), Digest::of(b"other"));
    let made = |input: &Digest, text: &str| {
        curios_utilities::candidate(Facet::Elaboration, input, text.as_bytes().to_vec())
    };
    let first = made(&input, "first");
    packs
        .write(&[first.clone(), made(&other, "elsewhere"), object("plain")])
        .expect("a pack is written");
    let second = made(&input, "second");
    packs
        .write(std::slice::from_ref(&second))
        .expect("a pack is written");

    let mut found = Packs::at(directory.to_path_buf(), now())
        .candidates(Facet::Elaboration, &input)
        .into_iter()
        .map(|object| object.bytes.as_slice().to_vec())
        .collect::<Vec<_>>();
    found.sort();
    assert_eq!(found, vec![b"first".to_vec(), b"second".to_vec()]);
    assert_eq!(
        packs
            .object(&second.digest)
            .map(|object| object.bytes.as_slice().to_vec()),
        Some(b"second".to_vec())
    );
    assert!(
        first
            .digest
            .as_bytes()
            .starts_with(&curios_utilities::candidate_prefix(
                Facet::Elaboration,
                &input
            ))
    );
}

/// A pack whose bytes are not the ones its name says is not believed: what only it held is not found, the pack is taken away, and what it held is missing again for the next filing to write.
#[test]
fn a_pack_other_than_its_name_is_taken_away() {
    let directory = Temporary::new("packs", "damaged");
    let packs = Packs::at(directory.to_path_buf(), now());
    let written = [object("one")];
    packs.write(&written).expect("a pack is written");
    let name = packs.list().pop().expect("one pack");
    let path = packs.path(&name);
    let mut bytes = fs::read(&path).expect("the pack reads");
    bytes[0] ^= 1;
    fs::write(&path, bytes).expect("the pack is damaged");

    let reader = Packs::at(directory.to_path_buf(), now());
    assert_eq!(
        reader.missing([&written[0].digest]),
        vec![],
        "its table is believed until it is read"
    );
    assert!(reader.find(&written[0].digest).is_none());
    assert!(!path.exists());
    assert_eq!(
        reader.missing([&written[0].digest]),
        vec![written[0].digest]
    );
}

/// Which of `objects` the packs at `directory` hold, as a handle opened afresh finds them.
fn held(directory: &Path, objects: &[Object]) -> Vec<bool> {
    let packs = Packs::at(directory.to_path_buf(), now());

    objects
        .iter()
        .map(|object| packs.find(&object.digest).is_some())
        .collect()
}

/// `objects`, by their digests.
fn named(objects: &[&Object]) -> BTreeSet<Digest> {
    objects.iter().map(|object| object.digest).collect()
}

/// An object the slot stops naming is dropped by the first collection that finds it unnamed five days after a collection first did, and kept by every one before; an object the slot names is kept whatever its pack holds beside it.
#[test]
fn an_object_unnamed_for_five_days_is_dropped() {
    let directory = Temporary::new("packs", "collected");
    let objects = [object("named"), object("dropped")];
    Packs::at(directory.to_path_buf(), now())
        .write(&objects)
        .expect("a pack is written");
    let only = named(&[&objects[0]]);
    let collect = |day| {
        Packs::at(directory.to_path_buf(), after(day))
            .collect(&only)
            .expect("an address is collected");
    };

    collect(0);
    collect(1);
    assert_eq!(
        held(&directory, &objects),
        [true, true],
        "found unnamed today"
    );
    collect(5);
    assert_eq!(
        held(&directory, &objects),
        [true, true],
        "unnamed for four days"
    );
    collect(6);
    assert_eq!(
        held(&directory, &objects),
        [true, false],
        "unnamed for five days"
    );
}

/// An address is collected at most once a day: a filing within a day of the last collection drops nothing, however long what it would drop has gone unnamed.
#[test]
fn an_address_is_collected_once_a_day() {
    let directory = Temporary::new("packs", "daily");
    let objects = [object("named"), object("dropped")];
    Packs::at(directory.to_path_buf(), now())
        .write(&objects)
        .expect("a pack is written");
    let only = named(&[&objects[0]]);
    let collect = |day: SystemTime| {
        Packs::at(directory.to_path_buf(), day)
            .collect(&only)
            .expect("an address is collected");
    };

    collect(after(0));
    collect(after(1));
    collect(after(10));
    collect(after(10) + Duration::from_secs(60 * 60));
    assert_eq!(
        held(&directory, &objects),
        [true, false],
        "dropped on day ten"
    );

    let again = [object("named"), object("young")];
    Packs::at(directory.to_path_buf(), after(10))
        .write(&again[1..])
        .expect("a pack is written");
    collect(after(10) + Duration::from_secs(2 * 60 * 60));
    collect(after(20) + Duration::from_secs(3 * 60 * 60));
    assert_eq!(
        held(&directory, &again),
        [true, true],
        "found unnamed by the first collection a day after the last"
    );
}

/// An object named again before its five days are out is forgotten, and its time starts over if the slot stops naming it again.
#[test]
fn an_object_named_again_is_forgotten() {
    let directory = Temporary::new("packs", "renamed");
    let objects = [object("named"), object("returns")];
    Packs::at(directory.to_path_buf(), now())
        .write(&objects)
        .expect("a pack is written");
    let collect = |day, named: &BTreeSet<Digest>| {
        Packs::at(directory.to_path_buf(), after(day))
            .collect(named)
            .expect("an address is collected");
    };
    let (both, first) = (named(&[&objects[0], &objects[1]]), named(&[&objects[0]]));

    collect(0, &first);
    collect(1, &first);
    collect(4, &both);
    collect(5, &first);
    collect(9, &first);
    assert_eq!(
        held(&directory, &objects),
        [true, true],
        "unnamed for four days again"
    );
    collect(10, &first);
    assert_eq!(held(&directory, &objects), [true, false]);
}

/// The smallest packs are merged until each left holds twice the one before it: the top of a geometric run stays, and what is merged with the small end grows to cover a pack it outweighs.
#[test]
fn a_split_merges_the_small_end_until_the_rest_is_geometric() {
    assert_eq!(split(&[]), 0);
    assert_eq!(split(&[5]), 0);
    assert_eq!(split(&[1, 2, 4, 8]), 0);
    assert_eq!(split(&[1, 1]), 2);
    assert_eq!(split(&[1, 1, 8]), 2);
    assert_eq!(split(&[1, 1, 2, 16]), 3);
    assert_eq!(split(&[3, 4, 5, 64]), 3);
}
