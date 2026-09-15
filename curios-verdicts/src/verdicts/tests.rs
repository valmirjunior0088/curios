//! What the store does across two compilations of the same project.
//!
//! Every test here compiles for real, twice, and reads the answer off the fold's own progress events rather than off the store's internals: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with verification broken.

use {
    super::*,
    curios_package::{Governing, order},
    curios_pipeline::{Progress, compile_with_units},
    curios_text::Entrypoint,
    curios_utilities::test_support::Temporary,
    std::{collections::BTreeMap, path::Path},
};

/// The entry every project here compiles: it uses the dependency, so the dependency is a unit of the compilation.
const ENTRY: &str = "use /std/{Fmt};\nuse /shape/{message};\n\nFmt/print(\"%\\n\")(message)\n";

/// Whether the one mounted unit of a compilation came from the store.
fn reused(root: &Path) -> bool {
    reused_from(root, &root.join("shape"))
}

/// The same, for a project whose dependency is mounted from somewhere other than beside it.
fn reused_from(root: &Path, shape: &Path) -> bool {
    let verdicts = Verdicts::at(root.to_path_buf());

    reused_through(root, shape, &verdicts)
}

/// The same, for a compilation reading through `overlay` — what the `wonder` engine asks, verified by [`Verdicts::get_overlaid`] and placed without filing.
fn reused_overlaid(root: &Path, overlay: &Overlay) -> bool {
    let verdicts = Verdicts::at(root.to_path_buf());

    reused_through(root, &root.join("shape"), &Overlaid(&verdicts, overlay))
}

/// The `wonder` engine's reading of the store: verified through an overlay, placed rather than filed, and taking a disagreeing slot as a baseline.
struct Overlaid<'a>(&'a Verdicts, &'a Overlay);

impl Cache for Overlaid<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.0.get_overlaid(source, self.1)
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<Unit>) -> Option<Unit> {
        self.0.earlier(source).or(offered)
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.0.place(source, unit);
    }
}

fn reused_through(root: &Path, shape: &Path, cache: &dyn Cache) -> bool {
    folded_through(root, shape, cache).contains(&"reused /shape".to_string())
}

/// What the fold did to the one mounted unit, through `cache`: read off the progress events, as every test here reads its answer.
fn folded_through(root: &Path, shape: &Path, cache: &dyn Cache) -> Vec<String> {
    let governing = Governing::of(shape).expect("a governed package");
    let library = order(&governing).expect("a resolvable library");
    let (entrypoint, loader, _source) =
        Entrypoint::opened(&root.join("exe.crs")).expect("an openable entrypoint");

    let mut events = Vec::new();
    compile_with_units(
        1_000_000,
        &library,
        &entrypoint,
        &loader,
        Some(cache),
        |_| {},
        |progress| match progress {
            Progress::Compiling(prefix) => events.push(format!("compiling {}", prefix.join())),
            Progress::Recompiling(prefix) => {
                events.push(format!("recompiling {}", prefix.join()));
            }
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        },
    )
    .expect("a compiling program");

    events
}

/// What a build's fold did to the one unit, through the store itself.
fn folded(root: &Path) -> Vec<String> {
    let verdicts = Verdicts::at(root.to_path_buf());

    folded_through(root, &root.join("shape"), &verdicts)
}

/// What a query's fold did to the one unit, through the store read as the `wonder` engine reads it.
fn folded_overlaid(root: &Path, overlay: &Overlay) -> Vec<String> {
    let verdicts = Verdicts::at(root.to_path_buf());

    folded_through(root, &root.join("shape"), &Overlaid(&verdicts, overlay))
}

/// A directory of its own, in this file's family.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cache", name)
}

/// A project with one dependency and an entry that uses it, at a directory of its own.
fn project(name: &str) -> Temporary {
    let root = temporary(name);

    write(&root, "shape/curios.toml", "name = \"shape\"\n");
    write(&root, "shape/lib.crs", &library("first"));
    write(&root, "exe.crs", ENTRY);

    root
}

/// Copy every slot `from`'s store holds into `into`'s, replacing whatever was there.
///
/// How both cross-project tests stage a shared store: by copying rather than by setting `CURIOS_CACHE`, since the store's own hermeticity rests on no test ever setting it, and what is under test is the verification, which cannot tell how a foreign slot arrived.
fn stage(from: &Path, into: &Path) {
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

fn write(root: &Path, path: &str, contents: &str) {
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
}

/// Opening a store is not a decision to file anything in it: the compiler's memo is written by the first slot that needs the identity, so a compilation refused before any unit is addressed leaves no `.curios/` behind.
#[test]
fn opening_a_store_writes_nothing_until_a_slot_is_addressed() {
    let root = temporary("untouched");
    fs::create_dir_all(&root).unwrap();

    let verdicts = Verdicts::at(root.to_path_buf());
    assert!(
        !root.join(".curios").exists(),
        "opening the store created its directory"
    );

    verdicts.compiler();
    assert!(
        root.join(".curios").join("compiler").is_file(),
        "asking for the identity did not write the memo"
    );
}

/// The bytes of every slot the store at `root` holds.
fn slots(root: &Path) -> Vec<Vec<u8>> {
    let mut slots = fs::read_dir(root.join(".curios").join("verdicts"))
        .expect("a store with units in it")
        .map(|slot| fs::read(slot.unwrap().path()).unwrap())
        .collect::<Vec<_>>();
    slots.sort();

    slots
}

/// A slot whose files were edited since it was filed is a baseline for a query, which compiles the unit over it, and a whole compile for a build, which compiles the unit from nothing and files it.
#[test]
fn an_edited_slot_is_a_baseline_for_a_query_and_a_whole_compile_for_a_build() {
    let root = project("baseline-edited");
    reused(&root);

    write(&root, "shape/lib.crs", &library("second"));

    assert_eq!(
        folded_overlaid(&root, &Overlay::default()),
        ["recompiling /shape"]
    );
    assert_eq!(folded(&root), ["compiling /shape"]);
}

/// What a query compiles over a baseline is placed and never filed, so the slot holds what it held; a build files what it compiled.
#[test]
fn a_unit_compiled_over_a_baseline_leaves_the_slot_as_it_was() {
    let root = project("baseline-unfiled");
    reused(&root);
    let before = slots(&root);

    write(&root, "shape/lib.crs", &library("second"));

    assert_eq!(
        folded_overlaid(&root, &Overlay::default()),
        ["recompiling /shape"]
    );
    assert_eq!(slots(&root), before, "a query files nothing");

    assert_eq!(folded(&root), ["compiling /shape"]);
    assert_ne!(slots(&root), before, "a build files what it compiled");
}

/// Another project's slot names files this project could not have read, so it is no baseline either — the containment clause a hit is held to.
#[test]
fn another_projects_slot_is_no_baseline() {
    let mine = project("baseline-mine");
    let theirs = project("baseline-theirs");

    write(&theirs, "shape/lib.crs", &library("theirs"));
    reused(&theirs);

    stage(&theirs, &mine);

    assert_eq!(
        folded_overlaid(&mine, &Overlay::default()),
        ["compiling /shape"]
    );
}
