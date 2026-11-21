use {super::*, curios_common::test_support::*, std::path::Path};

/// One blob of its own bytes.
fn blob(text: &str) -> Blob {
    Blob::of(text.as_bytes().to_vec())
}

/// The time every handle here is opened at, unless a test moves it.
fn now() -> SystemTime {
    SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000_000)
}

/// The time `days` days after [`now`].
fn after(days: u64) -> SystemTime {
    now() + Duration::from_secs(days * 24 * 60 * 60)
}

/// A blob written is found by its name as the bytes it was written as, and a name nothing wrote is missing.
#[test]
fn a_blob_written_is_found_and_one_never_written_is_missing() {
    let directory = Temporary::new("packs", "found");
    let packs = Packs::at(directory.to_path_buf(), now());
    let written = [blob("one"), blob("two")];
    packs.write(&written).expect("a pack is written");

    let found = Packs::at(directory.to_path_buf(), now())
        .find(&written[1].digest)
        .expect("a blob written");
    assert_eq!(found.bytes.as_slice(), b"two");
    let never = blob("three");
    assert_eq!(
        packs.missing([&written[0].digest, &never.digest, &written[1].digest]),
        vec![never.digest]
    );
}

/// A filing at a time, the address stays geometric: however many packs were written, it holds no more than a logarithm of the blobs, each still found, and another handle on the directory finds them too.
#[test]
fn filings_one_at_a_time_keep_the_packs_geometric() {
    let directory = Temporary::new("packs", "geometric");
    let packs = Packs::at(directory.to_path_buf(), now());
    let mut written = Vec::new();
    for at in 0..100 {
        let filed = vec![blob(&format!("blob {at}"))];
        packs.write(&filed).expect("a pack is written");
        written.extend(filed);
    }

    let held = packs.list();
    assert!(held.len() <= 8, "{} packs hold 100 blobs", held.len());
    let another = Packs::at(directory.to_path_buf(), now());
    for blob in &written {
        let found = another
            .find(&blob.digest)
            .expect("every blob written is held");
        assert_eq!(found.bytes.as_slice(), blob.bytes.as_slice());
    }
}

/// The candidates made from one input are found together, whichever pack each was written in, and none made from another input or of another kind of blob is.
#[test]
fn candidates_of_an_input_are_found_across_packs() {
    let directory = Temporary::new("packs", "candidates");
    let packs = Packs::at(directory.to_path_buf(), now());
    let (input, other) = (Digest::of(b"input"), Digest::of(b"other"));
    let made = |input: &Digest, text: &str| Blob {
        digest: curios_common::candidate_name(Facet::Elaboration, input, text.as_bytes()),
        bytes: text.as_bytes().to_vec().into(),
        member: None,
    };
    let first = made(&input, "first");
    packs
        .write(&[first.clone(), made(&other, "elsewhere"), blob("plain")])
        .expect("a pack is written");
    let second = made(&input, "second");
    packs
        .write(std::slice::from_ref(&second))
        .expect("a pack is written");

    let mut found = Packs::at(directory.to_path_buf(), now())
        .candidates(Facet::Elaboration, &input)
        .into_iter()
        .map(|blob| blob.bytes.as_slice().to_vec())
        .collect::<Vec<_>>();
    found.sort();
    assert_eq!(found, vec![b"first".to_vec(), b"second".to_vec()]);
    assert_eq!(
        packs
            .blob(&second.digest)
            .map(|blob| blob.bytes.as_slice().to_vec()),
        Some(b"second".to_vec())
    );
    assert!(
        first
            .digest
            .as_bytes()
            .starts_with(&curios_common::candidate_prefix(Facet::Elaboration, &input))
    );
}

/// A pack whose bytes are not the ones its name says is not believed: what only it held is not found, the pack is taken away, and what it held is missing again for the next filing to write.
#[test]
fn a_pack_other_than_its_name_is_taken_away() {
    let directory = Temporary::new("packs", "damaged");
    let packs = Packs::at(directory.to_path_buf(), now());
    let written = [blob("one")];
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

/// Which of `blobs` the packs at `directory` hold, as a handle opened afresh finds them.
fn held(directory: &Path, blobs: &[Blob]) -> Vec<bool> {
    let packs = Packs::at(directory.to_path_buf(), now());

    blobs
        .iter()
        .map(|blob| packs.find(&blob.digest).is_some())
        .collect()
}

/// `blobs`, by their digests.
fn named(blobs: &[&Blob]) -> BTreeSet<Digest> {
    blobs.iter().map(|blob| blob.digest).collect()
}

/// A blob the object stops naming is dropped by the first collection that finds it unnamed five days after a collection first did, and kept by every one before; a blob the object names is kept whatever its pack holds beside it.
#[test]
fn a_blob_unnamed_for_five_days_is_dropped() {
    let directory = Temporary::new("packs", "collected");
    let blobs = [blob("named"), blob("dropped")];
    Packs::at(directory.to_path_buf(), now())
        .write(&blobs)
        .expect("a pack is written");
    let only = named(&[&blobs[0]]);
    let collect = |day| {
        Packs::at(directory.to_path_buf(), after(day))
            .collect(&only)
            .expect("an address is collected");
    };

    collect(0);
    collect(1);
    assert_eq!(
        held(&directory, &blobs),
        [true, true],
        "found unnamed today"
    );
    collect(5);
    assert_eq!(
        held(&directory, &blobs),
        [true, true],
        "unnamed for four days"
    );
    collect(6);
    assert_eq!(
        held(&directory, &blobs),
        [true, false],
        "unnamed for five days"
    );
}

/// What a filer stopped short of in the address's directory — an object's staged unit file as a pack's — is swept by the first collection that finds it five days old, and one younger is left to a filer that may still be writing it.
#[test]
fn a_collection_sweeps_a_staging_five_days_old() {
    let directory = Temporary::new("packs", "staged");
    fs::create_dir_all(&directory).expect("a directory for the address");
    let staged = |name: &str, written: SystemTime| {
        let path = directory.join(name);
        fs::write(&path, "half of a verdict").expect("a staging is written");
        fs::File::options()
            .write(true)
            .open(&path)
            .and_then(|file| file.set_modified(written))
            .expect("a staging's time is set");
        path
    };
    let aged = staged("verdict.1.part", now());
    let young = staged("verdict.2.part", after(2));
    let collect = |day| {
        Packs::at(directory.to_path_buf(), after(day))
            .collect(&BTreeSet::new())
            .expect("an address is collected");
    };

    collect(0);
    collect(5);

    assert!(!aged.exists(), "a staging five days old is swept");
    assert!(young.exists(), "a younger one is left");
}

/// An address is collected at most once a day: a filing within a day of the last collection drops nothing, however long what it would drop has gone unnamed.
#[test]
fn an_address_is_collected_once_a_day() {
    let directory = Temporary::new("packs", "daily");
    let blobs = [blob("named"), blob("dropped")];
    Packs::at(directory.to_path_buf(), now())
        .write(&blobs)
        .expect("a pack is written");
    let only = named(&[&blobs[0]]);
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
        held(&directory, &blobs),
        [true, false],
        "dropped on day ten"
    );

    let again = [blob("named"), blob("young")];
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

/// A blob named again before its five days are out is forgotten, and its time starts over if the object stops naming it again.
#[test]
fn a_blob_named_again_is_forgotten() {
    let directory = Temporary::new("packs", "renamed");
    let blobs = [blob("named"), blob("returns")];
    Packs::at(directory.to_path_buf(), now())
        .write(&blobs)
        .expect("a pack is written");
    let collect = |day, named: &BTreeSet<Digest>| {
        Packs::at(directory.to_path_buf(), after(day))
            .collect(named)
            .expect("an address is collected");
    };
    let (both, first) = (named(&[&blobs[0], &blobs[1]]), named(&[&blobs[0]]));

    collect(0, &first);
    collect(1, &first);
    collect(4, &both);
    collect(5, &first);
    collect(9, &first);
    assert_eq!(
        held(&directory, &blobs),
        [true, true],
        "unnamed for four days again"
    );
    collect(10, &first);
    assert_eq!(held(&directory, &blobs), [true, false]);
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
