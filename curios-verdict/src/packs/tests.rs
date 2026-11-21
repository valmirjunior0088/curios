use {
    super::*,
    curios_common::{Facet, candidate_name, candidate_prefix, test_support::*},
    std::{path::Path, time::Duration},
};

/// One blob of its own bytes.
fn blob(text: &str) -> Blob {
    Blob::of(text.as_bytes().to_vec())
}

/// The time every handle here is opened at, unless a test moves it.
fn now() -> SystemTime {
    SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000_000)
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
        packs
            .collect(&written.iter().map(|blob| blob.digest).collect())
            .expect("an address is collected");
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
        digest: candidate_name(Facet::Elaboration, input, text.as_bytes()),
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
        .prefixed(&candidate_prefix(Facet::Elaboration, &input))
        .into_iter()
        .map(|blob| blob.bytes.as_slice().to_vec())
        .collect::<Vec<_>>();
    found.sort();
    assert_eq!(found, vec![b"first".to_vec(), b"second".to_vec()]);
    assert_eq!(
        packs
            .find(&second.digest)
            .map(|blob| blob.bytes.as_slice().to_vec()),
        Some(b"second".to_vec())
    );
    assert!(
        first
            .digest
            .as_bytes()
            .starts_with(&candidate_prefix(Facet::Elaboration, &input))
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

/// A pack at least half of whose blobs no record names is written again holding the rest, a pack nothing names is taken away, and a pack less than half unnamed stands whole: what a rewrite carries is paid for by what went.
#[test]
fn a_pack_is_rewritten_once_half_of_it_is_unnamed() {
    let directory = Temporary::new("packs", "halved");
    let packs = Packs::at(directory.to_path_buf(), now());
    let blobs = [blob("one"), blob("two"), blob("three")];
    packs.write(&blobs).expect("a pack is written");

    packs
        .collect(&named(&[&blobs[0], &blobs[1]]))
        .expect("an address is collected");
    assert_eq!(
        held(&directory, &blobs),
        [true, true, true],
        "a third unnamed stands"
    );
    packs
        .collect(&named(&[&blobs[0]]))
        .expect("an address is collected");
    assert_eq!(
        held(&directory, &blobs),
        [true, false, false],
        "two thirds unnamed is written again"
    );
    packs
        .collect(&BTreeSet::new())
        .expect("an address is collected");
    assert_eq!(
        held(&directory, &blobs),
        [false, false, false],
        "and none named is taken away"
    );
    assert!(packs.list().is_empty(), "leaving no pack behind");
}

/// A merge keeps what the records name alone: a blob no record names rides no merge, though its pack was less than half unnamed.
#[test]
fn a_merge_drops_what_no_record_names() {
    let directory = Temporary::new("packs", "merged");
    let packs = Packs::at(directory.to_path_buf(), now());
    let older = [blob("a"), blob("b"), blob("gone")];
    let newer = [blob("c"), blob("d")];
    packs.write(&older).expect("a pack is written");
    packs.write(&newer).expect("a pack is written");

    packs
        .collect(&named(&[&older[0], &older[1], &newer[0], &newer[1]]))
        .expect("an address is collected");
    assert_eq!(packs.list().len(), 1, "three blobs beside two are merged");
    assert_eq!(held(&directory, &older), [true, true, false]);
    assert_eq!(held(&directory, &newer), [true, true]);
}

/// What a filer stopped short of in the address's directory — an object's staged unit file as a pack's — is swept by a collection once it is a day old, and one younger is left to a filer that may still be writing it.
#[test]
fn a_collection_sweeps_a_staging_a_day_old() {
    let directory = Temporary::new("packs", "staged");
    fs::create_dir_all(&directory).expect("a directory for the address");
    let staged = |name: &str, written: SystemTime| {
        let path = directory.join(name);
        fs::write(&path, "half of a unit file").expect("a staging is written");
        fs::File::options()
            .write(true)
            .open(&path)
            .and_then(|file| file.set_modified(written))
            .expect("a staging's time is set");
        path
    };
    let aged = staged("unit.1.part", now());
    let young = staged("unit.2.part", now() + Duration::from_secs(60 * 60));

    Packs::at(directory.to_path_buf(), now() + crate::STALE)
        .collect(&BTreeSet::new())
        .expect("an address is collected");

    assert!(!aged.exists(), "a staging a day old is swept");
    assert!(young.exists(), "a younger one is left");
}

/// Counted: a hundred filings each replacing one blob of a thousand write fewer blobs than twice the pack the thousand were filed in, which dropping each blob as it went would write again at nearly every filing.
#[test]
fn edits_rewrite_in_proportion_to_what_went() {
    let directory = Temporary::new("packs", "edits");
    let packs = Packs::at(directory.to_path_buf(), now());
    let digests = |blobs: &[Blob]| {
        blobs
            .iter()
            .map(|blob| blob.digest)
            .collect::<BTreeSet<_>>()
    };
    let mut live = (0..1000)
        .map(|at| blob(&format!("first {at}")))
        .collect::<Vec<_>>();
    packs.write(&live).expect("a pack is written");
    packs
        .collect(&digests(&live))
        .expect("an address is collected");

    let mut written = 0;
    for edit in 0..100 {
        let replaced = digests(&live);
        live[edit] = blob(&format!("edit {edit}"));
        let before = packs.list().into_iter().collect::<BTreeSet<_>>();
        packs.write(&live[edit..=edit]).expect("a pack is written");
        packs
            .collect(&digests(&live).union(&replaced).copied().collect())
            .expect("an address is collected");
        written += packs
            .list()
            .into_iter()
            .filter(|name| !before.contains(name))
            .map(|name| packs.names(&name).map_or(0, |names| names.len()))
            .sum::<usize>();
    }

    assert!(
        written < 2 * 1000,
        "{written} blobs written over a hundred one-blob edits"
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
