//! What the store does across two compilations of the same project.
//!
//! Every test here compiles for real, twice, and reads the answer off the fold's own progress events rather than off the store's internals: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with verification broken.

use {
    super::*,
    curios_pipeline::{Progress, compile_with_units},
    curios_text::Entrypoint,
    std::time::{SystemTime, UNIX_EPOCH},
};

/// Whether the one mounted unit of a compilation came from the store.
fn reused(root: &std::path::Path) -> bool {
    let library = curios_package::mounted(&[root.join("shape")]).expect("a mountable package");
    let (entrypoint, loader) =
        Entrypoint::opened(&root.join("exe.crs")).expect("an openable entrypoint");

    let mut reused = false;
    compile_with_units(
        1_000_000,
        &library,
        &entrypoint,
        loader,
        Some(&Verdicts::at(root.to_path_buf()) as &dyn Cache),
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

/// A project with one dependency and an entry that uses it, at a directory of its own.
fn project(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!(
        "curios-cache-{name}-{}-{millis}",
        std::process::id()
    ));

    write(&root, "shape/curios.toml", "name = \"shape\"\n");
    write(&root, "shape/lib.crs", &library("first"));
    write(
        &root,
        "exe.crs",
        "use /std/{Fmt};\nuse /shape/{message};\n\nFmt/print(\"%\\n\")(message)\n",
    );

    root
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
}

/// The regression for what the tree-hashed scheme got wrong. Filing a unit writes into `.curios/`, which sits inside the very directory that scheme hashed into the unit's address — so a package's own library missed forever and the store grew a directory per compile. What a unit was compiled from is now recorded and verified rather than addressed, and a generated file is not something it was compiled from.
#[test]
fn writing_into_the_store_does_not_invalidate_it() {
    let root = project("store-writes");

    reused(&root);
    assert!(reused(&root), "the store's own writes are not source");

    write(&root, ".curios/unrelated", "not a source file");
    assert!(reused(&root), "and neither is anything else under it");
}

/// A slot is addressed without its contents, so it is the *verification* that has to notice an edit. This is the half that would still pass if the record were never checked.
#[test]
fn an_edited_unit_is_not_reused() {
    let root = project("edited");

    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    write(&root, "shape/lib.crs", &library("second"));
    assert!(!reused(&root), "and refused once its source differs");
}

/// A slot filed by one project must not answer for another's, even when both address it identically — which two projects holding a package of one name, compiled by one compiler after one chain, always do.
///
/// The store is shared whenever `CURIOS_CACHE` names one, so this is reachable. It is staged by copying rather than by setting that variable: the store's own hermeticity rests on no test ever setting it, and what is being tested is the verification, which cannot tell how the foreign slot arrived.
#[test]
fn a_slot_does_not_answer_for_another_projects_source() {
    let mine = project("mine");
    let theirs = project("theirs");

    write(&theirs, "shape/lib.crs", &library("theirs"));
    reused(&theirs);
    reused(&mine);

    let (from, into) = (theirs.join(".curios/unit"), mine.join(".curios/unit"));
    fs::remove_dir_all(&into).unwrap();
    fs::create_dir_all(&into).unwrap();

    for slot in fs::read_dir(&from).unwrap() {
        let slot = slot.unwrap().path();
        let target = into.join(slot.file_name().unwrap());
        fs::create_dir_all(&target).unwrap();

        for file in fs::read_dir(&slot).unwrap() {
            let file = file.unwrap().path();
            fs::copy(&file, target.join(file.file_name().unwrap())).unwrap();
        }
    }

    assert!(
        !reused(&mine),
        "their record names their files, which are not mine to have read"
    );
}

/// The store holds one slot per unit rather than one per compile — the property the address exists to have, and the one the previous scheme lost.
#[test]
fn compiling_repeatedly_files_one_slot() {
    let root = project("bounded");

    for _ in 0..4 {
        reused(&root);
    }

    let slots = fs::read_dir(root.join(".curios").join("unit"))
        .expect("a store with units in it")
        .count();

    assert_eq!(slots, 1, "four compiles of one unit, one slot");
}
