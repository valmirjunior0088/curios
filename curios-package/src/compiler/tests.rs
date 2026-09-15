use {super::*, curios_utilities::test_support::Temporary};

/// A store of its own, in a directory that goes away with the test, beside the guard that removes it.
///
/// Hermetic without asking: nothing here sets `CURIOS_CACHE`, so the shared half falls back to the project's own directory and no test can reach a developer's real cache.
fn store(name: &str) -> (Temporary, Store) {
    let root = Temporary::new("compiler", name);
    fs::create_dir_all(&root).unwrap();
    let store = Store::at(root.to_path_buf());

    (root, store)
}

/// One compiler, one identity — and asking twice costs a `stat` the second time, because the digest is memoized against the binary's size and modification time.
#[test]
fn a_compiler_identifies_itself_the_same_way_twice() {
    let (_root, store) = store("compiler-stable");

    let first = compiler(&store).expect("a running binary can be stat'd and read");
    let record = store.compiler();
    assert!(record.is_file(), "the digest is recorded for next time");

    let recorded = fs::read_to_string(&record).unwrap();
    assert_eq!(compiler(&store).as_deref(), Some(first.as_str()));
    assert_eq!(
        fs::read_to_string(&record).unwrap(),
        recorded,
        "the second ask reuses the record rather than rewriting it"
    );
    assert!(recorded.ends_with(&first), "{recorded}");
}

/// A stale record is not believed. Its stamp is what says which binary it describes, so a record naming a different one is recomputed rather than trusted — which is the whole reason the stamp is stored beside the digest.
/// The memo is staged beside its place and renamed over it, so what the store holds afterwards is the memo and nothing else: no staging file outlives the write, and a reader never opens a half-written one.
#[test]
fn the_memo_is_the_only_file_left_beside_it() {
    let (_root, store) = store("compiler-staged");

    compiler(&store).expect("a compiler that can identify itself");

    let beside = fs::read_dir(store.compiler().parent().unwrap())
        .unwrap()
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(beside, vec!["compiler".to_string()]);
}

#[test]
fn a_record_describing_another_binary_is_recomputed() {
    let (_root, store) = store("compiler-stale");
    let truth = compiler(&store).expect("a running binary");

    let record = store.compiler();
    fs::write(&record, "0 0 deadbeef").unwrap();

    assert_eq!(compiler(&store).as_deref(), Some(truth.as_str()));
    assert!(
        !fs::read_to_string(&record).unwrap().contains("deadbeef"),
        "the stale record is replaced"
    );
}

/// The digest is of the contents, so two different files never share one — which is what lets a reproducible rebuild keep its cache while a behaviourally different compiler loses it.
#[test]
fn two_binaries_digest_differently() {
    let root = Temporary::new("compiler", "compiler-digest");
    fs::create_dir_all(&root).unwrap();
    fs::write(root.join("one"), b"a compiler").unwrap();
    fs::write(root.join("two"), b"a compilor").unwrap();

    assert_ne!(digest(&root.join("one")), digest(&root.join("two")));
    assert_eq!(digest(&root.join("one")), digest(&root.join("one")));
}
