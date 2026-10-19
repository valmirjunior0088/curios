use {super::*, std::io::Cursor};

/// One object of its own bytes.
fn whole(bytes: &[u8]) -> Packed<'_> {
    Packed {
        digest: Digest::of(bytes),
        bytes,
        member: None,
    }
}

/// Every object a pack holds is found by its name, as the bytes it was packed as, and a name it does not hold finds nothing.
#[test]
fn an_object_is_found_by_its_name() {
    let objects = [b"one".as_slice(), b"two", b"three"];
    let pack = Pack::read(Bytes::from(packed(
        objects.iter().map(|bytes| whole(bytes)),
    )))
    .expect("a pack reads back");

    assert_eq!(pack.len(), 3);
    for bytes in objects {
        let found = pack.get(&Digest::of(bytes)).expect("an object packed");
        assert_eq!(found.bytes.as_slice(), bytes);
        assert_eq!(found.member, None);
    }
    assert!(pack.get(&Digest::of(b"four")).is_none());
}

/// Which objects a pack holds is read off its end, in the order of their names, and a file that closes on no table names none.
#[test]
fn what_a_pack_holds_is_read_off_its_end() {
    let objects = [b"one".as_slice(), b"two", b"three"];
    let bytes = packed(objects.iter().map(|bytes| whole(bytes)));
    let mut names = objects.map(Digest::of).to_vec();
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
    let one = packed([whole(b"a"), whole(b"b"), whole(b"c")]);
    let other = packed([whole(b"c"), whole(b"a"), whole(b"b"), whole(b"a")]);

    assert_eq!(pack_name(&one), pack_name(&other));
    assert_eq!(one, other);
}

/// Members of one run are written once, each found as the run with which member it is.
#[test]
fn members_of_one_run_share_its_bytes() {
    let run = b"a run of two members".as_slice();
    let members = [b"first".as_slice(), b"second"].map(|form| Packed {
        digest: Digest::of(form),
        bytes: run,
        member: None,
    });
    let [first, second] = members;
    let bytes = packed([
        Packed {
            member: Some(0),
            ..first
        },
        Packed {
            member: Some(1),
            ..second
        },
        whole(b"alone"),
    ]);
    let pack = Pack::read(Bytes::from(bytes.clone())).expect("a pack reads back");

    let second = pack.get(&Digest::of(b"second")).expect("a member packed");
    assert_eq!(second.bytes.as_slice(), run);
    assert_eq!(second.member, Some(1));
    assert_eq!(
        bytes
            .windows(run.len())
            .filter(|window| *window == run)
            .count(),
        1,
        "the run is written once"
    );
}

/// Bytes that are no pack are refused: another file, one cut short, or a table naming bytes past where the objects end.
#[test]
fn bytes_that_are_no_pack_are_refused() {
    let bytes = packed([whole(b"one"), whole(b"two")]);

    assert!(Pack::read(Bytes::from(b"not a pack".to_vec())).is_none());
    assert!(Pack::read(Bytes::from(bytes[..bytes.len() - 1].to_vec())).is_none());
    assert!(Pack::read(Bytes::from(bytes[20..].to_vec())).is_none());
}
