//! An object's records: its address names no compiler, the header of each names the compiler that filed it, each compiler believes its own records alone, and the object holds what its last two filings name.

use {super::*, curios_common::candidate_name, curios_unit::pack_names};

/// A handle on the own cabinet of the project at `root`, filing as the compiler identifying itself as `compiler`, in place of the binary running the suite.
fn filing_as(root: &Path, compiler: &str) -> Verdicts {
    let verdicts = Verdicts::at(own(root));
    verdicts
        .compiler
        .set(Some(compiler.to_string()))
        .expect("a fresh handle's compiler");

    verdicts
}

/// What the fold did to the one mounted unit, compiled by `compiler` through the project's own cabinet.
fn folded_by(root: &Path, compiler: &str) -> Vec<String> {
    folded_through(
        root,
        &root.join("shape"),
        &filing_as(root, compiler),
        Compiled::Built,
    )
}

/// The addresses of the objects the project at `root` holds.
fn addresses(root: &Path) -> Vec<String> {
    fs::read_dir(own(root).objects())
        .expect("a cabinet with objects in it")
        .map(|object| {
            object
                .expect("a listed object")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect()
}

/// The packs the object at `address` of the project at `root` holds, by name.
fn packs(root: &Path, address: &str) -> BTreeSet<String> {
    fs::read_dir(own(root).object(address))
        .expect("an object's directory")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();

            name.ends_with(".pack").then_some(name)
        })
        .collect()
}

/// The blobs the packs of the object at `address` of the project at `root` hold, as their tables say.
fn held(root: &Path, address: &str) -> Vec<Digest> {
    packs(root, address)
        .into_iter()
        .flat_map(|pack| {
            let mut file =
                fs::File::open(own(root).object(address).join(pack)).expect("a pack opens");

            pack_names(&mut file).expect("a pack ends with its table")
        })
        .collect()
}

/// A compiler change files its unit at the address the last compiler filed it at, beside the blobs that one wrote: the address names no compiler, and a blob two compilers make alike is written once.
#[test]
fn two_compilers_file_one_object_and_each_blob_once() {
    let root = project("two-compilers");

    folded_by(&root, "one");
    let [address] = addresses(&root).try_into().expect("one object");
    let written = packs(&root, &address);
    folded_by(&root, "two");

    assert_eq!(
        addresses(&root),
        std::slice::from_ref(&address),
        "one object a unit"
    );
    assert_eq!(
        packs(&root, &address),
        written,
        "the second filing writes no blob the first wrote"
    );
}

/// Another compiler's record is read for nothing: not a hit, since its verdicts are that compiler's, and not a baseline, since a baseline's are believed too. The compiler's own record is both.
#[test]
fn another_compilers_object_is_no_hit_and_no_baseline() {
    let root = project("another-compiler");

    assert_eq!(folded_by(&root, "one"), ["compiling /shape"]);
    assert_eq!(
        folded_by(&root, "two"),
        ["compiling /shape"],
        "another compiler's record is no hit"
    );
    write(&root, "shape/lib.crs", &library("second"));
    assert_eq!(
        folded_by(&root, "three"),
        ["compiling /shape"],
        "and no baseline: a third compiles whole"
    );
    assert_eq!(
        folded_by(&root, "three"),
        ["reused /shape"],
        "where its own record is a hit"
    );
    write(&root, "shape/lib.crs", &library("third"));
    assert_eq!(
        folded_by(&root, "three"),
        ["recompiling /shape"],
        "and a baseline"
    );
}

/// An object keeps the record its last filing replaced, so two compilers alternating on one project each hit their own: a switch compiles nothing.
#[test]
fn alternating_compilers_each_hit_their_own_record() {
    let root = project("alternating-compilers");

    folded_by(&root, "one");
    folded_by(&root, "two");

    assert_eq!(
        folded_by(&root, "one"),
        ["reused /shape"],
        "the record the second filing replaced"
    );
    assert_eq!(
        folded_by(&root, "two"),
        ["reused /shape"],
        "and the newer record"
    );
}

/// An edit undone before the next filing is a hit on the record that filing replaced, the older of the compiler's two.
#[test]
fn an_edit_undone_hits_the_record_it_replaced() {
    let root = project("undone");

    folded_by(&root, "one");
    write(&root, "shape/lib.crs", &library("second"));
    folded_by(&root, "one");
    write(&root, "shape/lib.crs", &library("first"));

    assert_eq!(folded_by(&root, "one"), ["reused /shape"]);
}

/// A filing keeps what the record it replaces names whichever compiler filed that one: a compiler that filed one text reads it still after another filed another text over it.
#[test]
fn a_filing_keeps_what_the_record_it_replaces_names_whoever_filed_it() {
    let root = project("replaced-across");

    folded_by(&root, "one");
    write(&root, "shape/lib.crs", &library("second"));
    folded_by(&root, "two");
    write(&root, "shape/lib.crs", &library("first"));

    assert_eq!(folded_by(&root, "one"), ["reused /shape"]);
}

/// An object holds every blob its two records name, and fewer than twice as many: a filing keeps the blobs of the record it replaces, so an edit undone finds them, and takes away what no record names wherever that is half of a pack, so what only older records named does not pile up.
#[test]
fn an_object_holds_what_its_records_name_and_under_twice_as_much() {
    let root = project("two-records");

    for at in 0..8 {
        write(&root, "shape/lib.crs", &library(&format!("text {at}")));
        folded_by(&root, "one");
        let [address] = addresses(&root).try_into().expect("one object");
        let named = filing_as(&root, "one").named(&address);
        let holds = held(&root, &address);

        assert!(
            named.iter().all(|digest| holds.contains(digest)),
            "every blob a record names is held after filing {at}"
        );
        assert!(
            holds.len() < 2 * named.len(),
            "{} blobs held where the records name {} after filing {at}",
            holds.len(),
            named.len()
        );
    }
}

/// A candidate is held to the compiler that made it by a record of that compiler's naming its blob: the packs at an address hold what every compiler filing there wrote, and none of what another compiler's records name is offered.
#[test]
fn a_candidate_only_another_compilers_record_names_is_not_offered() {
    let root = project("candidates-held");

    folded_by(&root, "one");
    let [address] = addresses(&root).try_into().expect("one object");

    assert!(
        !filing_as(&root, "one").named(&address).is_empty(),
        "the compiler that filed names its blobs"
    );
    assert!(
        filing_as(&root, "two").named(&address).is_empty(),
        "another names none of them"
    );
}

/// What an address offers a compile is what its packs hold under a candidate's name that a record of the running compiler names, and nothing else its packs hold.
#[test]
fn an_offered_candidate_is_one_a_record_names() {
    let directory = Temporary::new("cache", "offered");
    let packs = Rc::new(Packs::at(directory.to_path_buf(), SystemTime::now()));
    let input = Digest::of(b"input");
    let made = |text: &str| Blob {
        digest: candidate_name(Facet::Elaboration, &input, text.as_bytes()),
        bytes: text.as_bytes().to_vec().into(),
        member: None,
    };
    let (named, unnamed) = (made("named"), made("unnamed"));
    packs
        .write(&[named.clone(), unnamed.clone()])
        .expect("a pack is written");
    let offered = Offered {
        packs,
        named: Rc::new(BTreeSet::from([named.digest])),
    };

    let found = offered
        .candidates(Facet::Elaboration, &input)
        .into_iter()
        .map(|blob| blob.digest)
        .collect::<Vec<_>>();
    assert_eq!(found, [named.digest]);
    assert!(
        offered.blob(&unnamed.digest).is_some(),
        "a blob is still found by its name, which is the digest of what it holds"
    );
}
