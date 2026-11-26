//! A cabinet's objects are one compiler's: an address names no compiler, each compiler files under its own name and reads nothing another filed, a filing takes another's away, and an object holds what its last two filings name.

use {
    super::test_support::*,
    crate::verdicts::*,
    curios_archive::Bytes,
    curios_common::{candidate_name, test_support::*},
    curios_unit::{Pack, Unit, pack_name, pack_names, packed},
    std::{collections::BTreeMap, path::Path},
};

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

/// The addresses of the objects the project at `root` holds, whichever compiler filed each.
fn addresses(root: &Path) -> Vec<String> {
    own(root).addresses().into_iter().collect()
}

/// The packs the object `compiler` filed at `address` of the project at `root` holds, by name.
fn packs(root: &Path, compiler: &str, address: &str) -> BTreeSet<String> {
    fs::read_dir(own(root).object(compiler, address))
        .expect("an object's directory")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();

            name.ends_with(".pack").then_some(name)
        })
        .collect()
}

/// The blobs the packs of the object `compiler` filed at `address` of the project at `root` hold, as their tables say.
fn held(root: &Path, compiler: &str, address: &str) -> Vec<Digest> {
    packs(root, compiler, address)
        .into_iter()
        .flat_map(|pack| {
            let mut file = fs::File::open(own(root).object(compiler, address).join(pack))
                .expect("a pack opens");

            pack_names(&mut file).expect("a pack ends with its table")
        })
        .collect()
}

/// What the records of the object `compiler` filed at `address` of the project at `root` name, read as a filing reads them to keep a pack's blobs: a record whose index is not the bytes it was written with names nothing.
fn named(root: &Path, compiler: &str, address: &str) -> BTreeSet<Digest> {
    filing_as(root, compiler)
        .records(address)
        .into_iter()
        .filter_map(|(opened, record)| {
            let index = opened.artifact()?;

            intact(&record.unit, &index)
                .then(|| Unit::named(&index).ok())
                .flatten()
        })
        .flatten()
        .collect()
}

/// A compiler change files its unit at the address the last compiler filed it at, under its own name, every blob its record names among its own packs, and takes away what the other filed: the address names no compiler, and a cabinet keeps one compiler's objects.
#[test]
fn a_compiler_change_files_the_address_again_and_takes_the_other_compilers_objects_away() {
    let root = project("two-compilers");

    folded_by(&root, "one");
    let [address] = addresses(&root).try_into().expect("one object");
    folded_by(&root, "two");

    assert_eq!(
        addresses(&root),
        std::slice::from_ref(&address),
        "one object a unit"
    );
    assert!(
        !own(&root).objects_of("one").exists(),
        "what the first compiler filed is taken away"
    );
    let named = named(&root, "two", &address);
    let holds = held(&root, "two", &address);
    assert!(
        !named.is_empty() && named.iter().all(|digest| holds.contains(digest)),
        "every blob the second compiler's record names is among its own packs"
    );
}

/// What an earlier layout left among the objects, a unit's directory at the family's own level, is neither read nor kept: a compiler reads under its own name alone, and its filing takes away every other entry of the family.
#[test]
fn an_object_an_earlier_layout_filed_is_neither_read_nor_kept() {
    let root = project("earlier-layout");
    let stray = own(&root).objects().join("0f".repeat(32));
    fs::create_dir_all(&stray).expect("a directory among the objects");
    fs::write(stray.join("unit"), "no unit this compiler filed").expect("a file in it");

    assert_eq!(folded_by(&root, "one"), ["compiling /shape"]);
    assert!(
        !stray.exists(),
        "what no compiler's name holds is taken away"
    );
    assert_eq!(folded_by(&root, "one"), ["reused /shape"]);
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

/// A cabinet keeps one compiler's objects, so two compilers used in turn on one project each compile it whole at every switch.
#[test]
fn compilers_used_in_turn_each_compile_whole() {
    let root = project("alternating-compilers");

    folded_by(&root, "one");
    folded_by(&root, "two");

    assert_eq!(
        folded_by(&root, "one"),
        ["compiling /shape"],
        "what it filed went as the other filed"
    );
    assert_eq!(
        folded_by(&root, "two"),
        ["compiling /shape"],
        "and the other's as it filed again"
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

/// An object holds every blob its two records name, and fewer than twice as many: a filing keeps the blobs of the record it replaces, so an edit undone finds them, and takes away what no record names wherever that is half of a pack, so what only older records named does not pile up.
#[test]
fn an_object_holds_what_its_records_name_and_under_twice_as_much() {
    let root = project("two-records");

    for at in 0..8 {
        write(&root, "shape/lib.crs", &library(&format!("text {at}")));
        folded_by(&root, "one");
        let [address] = addresses(&root).try_into().expect("one object");
        let named = named(&root, "one", &address);
        let holds = held(&root, "one", &address);

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

/// A compile looks among its own compiler's packs at an address, and an object another compiler filed holds none of them.
#[test]
fn a_compile_looks_among_its_own_compilers_packs_alone() {
    let root = project("candidates-held");

    folded_by(&root, "one");
    let [address] = addresses(&root).try_into().expect("one object");
    let held = |compiler: &str| {
        filing_as(&root, compiler)
            .packs(&address)
            .expect("a handle on a cabinet")
            .prefixed(&[])
    };

    assert!(
        !held("one").is_empty(),
        "the compiler that filed holds its blobs"
    );
    assert!(held("two").is_empty(), "another holds none of them");
}

/// What an address offers a compile is every blob its packs hold under a candidate's name, one no record names among them: a blob two filings back stays until its pack is rewritten, and a candidate is believed on the digests of the parts it read. Mutation-checked: with `Offered` handing over any fewer, the unnamed candidate is not found.
#[test]
fn every_candidate_the_packs_hold_is_offered_whatever_a_record_names() {
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
    let offered = Offered { packs };

    let found = offered
        .candidates(Facet::Elaboration, &input)
        .into_iter()
        .map(|blob| blob.digest)
        .collect::<BTreeSet<_>>();

    assert_eq!(found, BTreeSet::from([named.digest, unnamed.digest]));
}

/// Every pack the project at `root` holds, written again with its runs unreadable: its table names what it named, and each run members lie in is bytes no layout reads, as a run one compiler archived is to a compiler of another layout.
fn with_unreadable_runs(root: &Path) {
    for pack in super::test_support::packs(root) {
        let read = Pack::read(Bytes::from(fs::read(&pack).expect("a pack reads")))
            .expect("a pack's bytes");
        // Each run by where it lies, so the members of one run stay the members of one.
        let mut runs = BTreeMap::<(usize, usize), Bytes>::new();
        let blobs = read
            .blobs()
            .map(|blob| match blob.member {
                Some(_) => {
                    let run = blob.bytes.as_slice();
                    let unreadable = runs
                        .entry((run.as_ptr() as usize, run.len()))
                        .or_insert_with(|| Bytes::from(vec![0xff; run.len()]))
                        .clone();

                    Blob {
                        bytes: unreadable,
                        ..blob
                    }
                }
                None => blob,
            })
            .collect::<Vec<_>>();
        let bytes = packed(&blobs);

        fs::remove_file(&pack).expect("a pack is taken away");
        fs::write(
            pack.with_file_name(format!("{}.pack", pack_name(&bytes))),
            bytes,
        )
        .expect("a pack is written");
    }
}

/// A compiler reads the runs it archived and no other's: a member another compiler holds in a run this one cannot read is filed again by this one, and a recompile over what it filed reads its own.
#[test]
fn a_run_another_compiler_archived_is_never_read() {
    let root = project("anothers-run");

    folded_by(&root, "one");
    with_unreadable_runs(&root);

    assert_eq!(folded_by(&root, "two"), ["compiling /shape"]);
    write(&root, "shape/lib.crs", &library("second"));
    assert_eq!(
        folded_by(&root, "two"),
        ["recompiling /shape"],
        "over the unit it filed, read out of runs it archived"
    );
}
