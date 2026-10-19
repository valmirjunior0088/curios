use {super::*, curios_utilities::test_support::Temporary};

/// One object of its own bytes.
fn object(text: &str) -> Object {
    Object::of(text.as_bytes().to_vec())
}

/// An object written is found by its name as the bytes it was written as, and a name nothing wrote is missing.
#[test]
fn an_object_written_is_found_and_one_never_written_is_missing() {
    let directory = Temporary::new("packs", "found");
    let packs = Packs::at(directory.to_path_buf());
    let written = [object("one"), object("two")];
    packs.write(&written).expect("a pack is written");

    let found = Packs::at(directory.to_path_buf())
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
    let packs = Packs::at(directory.to_path_buf());
    let mut written = Vec::new();
    for at in 0..100 {
        let filed = vec![object(&format!("object {at}"))];
        packs.write(&filed).expect("a pack is written");
        written.extend(filed);
    }

    let held = packs.list();
    assert!(held.len() <= 8, "{} packs hold 100 objects", held.len());
    let another = Packs::at(directory.to_path_buf());
    for object in &written {
        let found = another
            .find(&object.digest)
            .expect("every object written is held");
        assert_eq!(found.bytes.as_slice(), object.bytes.as_slice());
    }
}

/// A pack whose bytes are not the ones its name says is not believed: what only it held is not found, the pack is taken away, and what it held is missing again for the next filing to write.
#[test]
fn a_pack_other_than_its_name_is_taken_away() {
    let directory = Temporary::new("packs", "damaged");
    let packs = Packs::at(directory.to_path_buf());
    let written = [object("one")];
    packs.write(&written).expect("a pack is written");
    let name = packs.list().pop().expect("one pack");
    let path = packs.path(&name);
    let mut bytes = fs::read(&path).expect("the pack reads");
    bytes[0] ^= 1;
    fs::write(&path, bytes).expect("the pack is damaged");

    let reader = Packs::at(directory.to_path_buf());
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
