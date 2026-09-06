//! What the store does across two compilations of the same project.
//!
//! Every test here compiles for real, twice, and reads the answer off the fold's own progress events rather than off the store's internals: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with verification broken.

use {
    super::*,
    curios_pipeline::{Progress, compile_with_units},
    curios_text::Entrypoint,
    std::{
        collections::BTreeMap,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// The entry every project here compiles: it uses the dependency, so the dependency is a unit of the compilation.
const ENTRY: &str = "use /std/{Fmt};\nuse /shape/{message};\n\nFmt/print(\"%\\n\")(message)\n";

/// Whether the one mounted unit of a compilation came from the store.
fn reused(root: &std::path::Path) -> bool {
    reused_from(root, &root.join("shape"))
}

/// The same, for a project whose dependency is mounted from somewhere other than beside it.
fn reused_from(root: &std::path::Path, shape: &std::path::Path) -> bool {
    let verdicts = Verdicts::at(root.to_path_buf());

    reused_through(root, shape, &verdicts)
}

/// The same, for a compilation reading through `overlay` — what the `wonder` engine asks, verified by [`Verdicts::get_overlaid`] and placed without filing.
fn reused_overlaid(root: &std::path::Path, overlay: &Overlay) -> bool {
    struct Overlaid<'a>(&'a Verdicts, &'a Overlay);

    impl Cache for Overlaid<'_> {
        fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
            self.0.get_overlaid(source, self.1)
        }

        fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
            self.0.place(source, unit);
        }
    }

    let verdicts = Verdicts::at(root.to_path_buf());

    reused_through(root, &root.join("shape"), &Overlaid(&verdicts, overlay))
}

fn reused_through(root: &std::path::Path, shape: &std::path::Path, cache: &dyn Cache) -> bool {
    let library = curios_package::mounted(&[shape.to_path_buf()]).expect("a mountable package");
    let (entrypoint, loader, _source) =
        Entrypoint::opened(&root.join("exe.crs")).expect("an openable entrypoint");

    let mut reused = false;
    compile_with_units(
        1_000_000,
        &library,
        &entrypoint,
        &loader,
        Some(cache),
        |_| {},
        |progress| {
            if matches!(progress, Progress::Reused(_)) {
                reused = true;
            }
        },
    )
    .expect("a compiling program");

    reused
}

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!(
        "curios-cache-{name}-{}-{millis}",
        std::process::id()
    ))
}

/// A project with one dependency and an entry that uses it, at a directory of its own.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);

    write(&root, "shape/curios.toml", "name = \"shape\"\n");
    write(&root, "shape/lib.crs", &library("first"));
    write(&root, "exe.crs", ENTRY);

    root
}

/// Copy every slot `from`'s store holds into `into`'s, replacing whatever was there.
///
/// How both cross-project tests stage a shared store: by copying rather than by setting `CURIOS_CACHE`, since the store's own hermeticity rests on no test ever setting it, and what is under test is the verification, which cannot tell how a foreign slot arrived.
fn stage(from: &std::path::Path, into: &std::path::Path) {
    let (from, into) = (from.join(".curios/verdicts"), into.join(".curios/verdicts"));

    let _ = fs::remove_dir_all(&into);
    fs::create_dir_all(&into).unwrap();

    for slot in fs::read_dir(&from).expect("a store with units in it") {
        let slot = slot.unwrap().path();
        fs::copy(&slot, into.join(slot.file_name().unwrap())).unwrap();
    }
}

/// The dependency's library, saying `word`.
fn library(word: &str) -> String {
    format!("use /std/{{Str}};\n\npub let message: Str =\n    \"{word}\";\n")
}

fn write(root: &std::path::Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// The point of the thing: source that has not changed is not compiled again.
#[test]
fn an_unchanged_unit_is_reused() {
    let root = project("unchanged");

    assert!(!reused(&root), "nothing is stored for the first compile");
    assert!(reused(&root), "and the second finds what the first filed");

    fs::remove_dir_all(root).unwrap();
}

/// The regression for what the tree-hashed scheme got wrong. Filing a unit writes into `.curios/`, which sits inside the very directory that scheme hashed into the unit's address — so a package's own library missed forever and the store grew a directory per compile. What a unit was compiled from is now recorded and verified rather than addressed, and a generated file is not something it was compiled from.
#[test]
fn writing_into_the_store_does_not_invalidate_it() {
    let root = project("store-writes");

    reused(&root);
    assert!(reused(&root), "the store's own writes are not source");

    write(&root, ".curios/unrelated", "not a source file");
    assert!(reused(&root), "and neither is anything else under it");

    fs::remove_dir_all(root).unwrap();
}

/// A compilation reading through an overlay is verified against the text it would read: a file the unit was compiled from hits while the editor's text is the disk's and misses once it differs, and an open file the unit never read — the executable beside a package's library, in the directory the library reads from — is no reason to compile the library again. That last case used to cost the language server the whole library on every keystroke in a program file, since the hit was refused whenever any open document lay under the unit's directory.
#[test]
fn an_overlaid_compilation_is_verified_by_the_text_it_would_read() {
    let root = project("overlaid");
    let held = |path: &str, text: String| Overlay::of(BTreeMap::from([(root.join(path), text)]));

    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    assert!(
        reused_overlaid(&root, &held("shape/lib.crs", library("first"))),
        "an open file holding the text on disk is the text the unit was compiled from"
    );
    assert!(
        !reused_overlaid(&root, &held("shape/lib.crs", library("second"))),
        "and one the editor has changed is not"
    );
    assert!(
        reused_overlaid(
            &root,
            &held("shape/exe.crs", "-- a program beside the library".into())
        ),
        "a file the unit never read leaves the hit standing, wherever it lies"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A slot is addressed without its contents, so it is the *verification* that has to notice an edit. This is the half that would still pass if the record were never checked.
#[test]
fn an_edited_unit_is_not_reused() {
    let root = project("edited");

    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    write(&root, "shape/lib.crs", &library("second"));
    assert!(!reused(&root), "and refused once its source differs");

    fs::remove_dir_all(root).unwrap();
}

/// A slot filed by one project must not answer for another's, even when both address it identically — which two projects holding a package of one name, compiled by one compiler after one chain, always do.
///
/// The store is shared whenever `CURIOS_CACHE` names one, so this is reachable.
#[test]
fn a_slot_does_not_answer_for_another_projects_source() {
    let mine = project("mine");
    let theirs = project("theirs");

    write(&theirs, "shape/lib.crs", &library("theirs"));
    reused(&theirs);
    reused(&mine);

    stage(&theirs, &mine);

    assert!(
        !reused(&mine),
        "their record names their files, which are not mine to have read"
    );

    fs::remove_dir_all(mine).unwrap();
    fs::remove_dir_all(theirs).unwrap();
}

/// The other half of that clause, and the reason it checks containment rather than re-deriving the read set: a dependency materialized once and read from that same path by every project *is* shared, and must still hit.
///
/// The dependency sits outside both projects, standing in for the `sources/` tree `curate` materializes under a shared store — the only way two projects ever read one unit's source from one path. `mine` never compiles it before the staging, so a hit has nowhere to come from but the slot `theirs` filed.
#[test]
fn a_slot_answers_for_a_dependency_both_projects_read() {
    let materialized = temporary("materialized");
    write(&materialized, "curios.toml", "name = \"shape\"\n");
    write(&materialized, "lib.crs", &library("first"));

    let theirs = temporary("theirs-sharing");
    let mine = temporary("mine-sharing");
    write(&theirs, "exe.crs", ENTRY);
    write(&mine, "exe.crs", ENTRY);

    assert!(
        !reused_from(&theirs, &materialized),
        "nothing is stored for the first compile, which files it"
    );

    stage(&theirs, &mine);

    assert!(
        reused_from(&mine, &materialized),
        "and the record names a path mine reads from too, so the unit crosses"
    );

    fs::remove_dir_all(materialized).unwrap();
    fs::remove_dir_all(mine).unwrap();
    fs::remove_dir_all(theirs).unwrap();
}

/// The unit's own digest is in its record, so a unit damaged after it was filed is a miss rather than a belief — bytecheck confirms its structure and nothing about its contents, and a flipped byte inside a string would otherwise read back as a different string.
#[test]
fn a_damaged_unit_is_not_reused() {
    let root = project("damaged");

    reused(&root);
    assert!(reused(&root), "filed by the first compilation");

    let slot = fs::read_dir(root.join(".curios/verdicts"))
        .expect("a store with units in it")
        .map(|slot| slot.unwrap().path())
        .next()
        .expect("one slot");
    let mut bytes = fs::read(&slot).unwrap();
    let last = bytes.len() - 1;
    bytes[last] ^= 0xff;
    fs::write(&slot, &bytes).unwrap();

    assert!(!reused(&root), "damaged bytes are not the bytes filed");
    assert!(reused(&root), "and the recompile repairs the slot");

    fs::remove_dir_all(root).unwrap();
}

/// The store holds one slot per unit rather than one per compile — the property the address exists to have, and the one the previous scheme lost.
#[test]
fn compiling_repeatedly_files_one_slot() {
    let root = project("bounded");

    for _ in 0..4 {
        reused(&root);
    }

    let slots = fs::read_dir(root.join(".curios").join("verdicts"))
        .expect("a store with units in it")
        .count();

    assert_eq!(slots, 1, "four compiles of one unit, one slot");

    fs::remove_dir_all(root).unwrap();
}
