//! What a question takes from the store and its session, and what it gives back: a hit verified against the text the compilation would read, a unit filed while the fold is one a build would have run over the disk and kept and placed otherwise, and a baseline — filed or kept — that a unit is recompiled over only while the units before it still hold what they held when it was made.

use {
    super::test_support::{mounted, mounted_project, write},
    crate::{Overlaid, Severity, Subject, diagnosed, diagnostics},
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, Fold, Progress},
    curios_text::{Overlay, UnitSource},
    curios_unit::Unit,
    curios_verdicts::{Session, Verdicts},
    std::{
        cell::Cell,
        collections::BTreeMap,
        fs,
        path::{Path, PathBuf},
    },
};

/// A question files each unit it compiled whole from the text on disk, so the next command starts from the store, whichever command it is.
#[test]
fn a_question_files_what_it_compiled_whole_from_the_disk() {
    let root = mounted_project("question-files");

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["compiling /alpha", "compiling /beta"],
        "nothing is filed for a package nothing has compiled"
    );
    assert_eq!(slots(&root).len(), 2, "one slot per unit");
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "the next question starts from the store"
    );
    assert_eq!(
        built(&root),
        ["reused /alpha", "reused /beta"],
        "and so does a build"
    );
}

/// One unit, whoever files it: the slots a question files hold the bytes a build files there, record and unit alike.
///
/// This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
#[test]
fn a_question_and_a_build_file_the_same_bytes() {
    let root = mounted_project("one-unit");

    folded(&root, &Overlay::default());
    let asked = slots(&root);
    fs::remove_dir_all(root.join(".curios/verdicts")).expect("the slots a question filed");
    built(&root);

    assert_eq!(asked.len(), 2, "one slot per unit");
    assert!(
        slots(&root) == asked,
        "a build filed other bytes than the question did"
    );
}

/// A document open and unchanged files as any other: the test is the record against the disk, and an overlay holding the disk's own text reads nothing the disk does not hold.
#[test]
fn a_unit_read_through_an_open_document_is_filed_while_the_disk_holds_its_text() {
    let root = mounted_project("held-files");

    assert_eq!(
        folded(&root, &held(&root, "a/lib.crs")),
        ["compiling /alpha", "compiling /beta"]
    );
    assert_eq!(slots(&root).len(), 2, "both units stand on the disk's text");
}

/// A unit whose record still agrees comes from the store, however many units before it missed.
///
/// A missing unit is compiled whole from the same text into the same bytes and filed where it was, so the unit after it, whose record vouches for those bytes, is reused. Which slot is the first unit's is not spelled here, so each is taken out in turn and the two outcomes are read together.
///
/// Read off the fold's own progress events, as the store's own tests are: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with the chain broken.
#[test]
fn a_miss_does_not_refuse_the_units_after_it() {
    let root = mounted_project("chain");
    built(&root);

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "both units are the store's when nothing is missing"
    );

    let slots = fs::read_dir(root.join(".curios/verdicts"))
        .expect("a store with both units in it")
        .map(|slot| slot.unwrap().path())
        .collect::<Vec<_>>();
    assert_eq!(slots.len(), 2, "one slot per unit: {slots:?}");

    let mut outcomes = slots
        .iter()
        .map(|slot| {
            let aside = slot.with_extension("aside");
            fs::rename(slot, &aside).unwrap();
            let events = folded(&root, &Overlay::default());
            fs::rename(&aside, slot).unwrap();
            events
        })
        .collect::<Vec<_>>();
    outcomes.sort();

    assert_eq!(
        outcomes,
        [
            ["compiling /alpha", "reused /beta"],
            ["reused /alpha", "compiling /beta"],
        ],
        "the unit after a missing one is still the store's, since the one compiled again holds the bytes it held"
    );
}

/// A unit compiled after one from unsaved text is not filed, and its slot holds the bytes it held.
///
/// An edited predecessor recompiles the units after it: a unit's arena is the whole prefix's, so its record vouches for the bytes every unit before it contained, and a predecessor that no longer contains them is a disagreement whether or not anything is imported from it. `/beta` is then compiled whole from text the disk holds — and filed, it would take the place of the slot a build filed, chained after an `/alpha` nothing on disk reproduces.
#[test]
fn a_unit_after_one_compiled_from_unsaved_text_is_not_filed() {
    let root = mounted_project("unsaved-before");
    built(&root);
    let before = slots(&root);

    assert_eq!(
        folded(&root, &edited(&root, "a/lib.crs")),
        ["recompiling /alpha", "compiling /beta"],
        "the overlay edits /alpha alone, which is compiled over its slot; /beta's record vouched for the /alpha it was compiled after, and its slot was filed after a chain that has moved, so it is neither a hit nor a baseline"
    );
    assert!(
        slots(&root) == before,
        "a keystroke in /alpha's document moved a slot"
    );
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "so the disk's text is the store's still"
    );
}

/// A unit compiled after one compiled over a baseline is not filed either, the disk holding every text both read: what a question compiles over a baseline stays its own, and a unit chained after it would be a slot no build reaches.
#[test]
fn a_unit_after_one_compiled_over_a_baseline_is_not_filed() {
    let root = mounted_project("baseline-before");
    built(&root);
    let before = slots(&root);

    write(
        &root,
        "a/lib.crs",
        "use /std/{Str};\n\npub let said: Str =\n    \"alpha, saved\";\n",
    );

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["recompiling /alpha", "compiling /beta"]
    );
    assert!(
        slots(&root) == before,
        "a question filed over a baseline, or after one"
    );

    assert_eq!(built(&root), ["compiling /alpha", "compiling /beta"]);
    assert!(slots(&root) != before, "a build files what it compiled");
}

/// A unit whose slot disagrees with its files is compiled over that slot's unit rather than from nothing: the slot is a baseline, and every declaration the edit did not reach is reused. A query is what takes it; the disk is untouched either way.
#[test]
fn an_edited_document_recompiles_its_unit_over_the_stored_one() {
    let root = mounted_project("baseline");
    built(&root);

    assert_eq!(
        folded(&root, &edited(&root, "b/lib.crs")),
        ["reused /alpha", "recompiling /beta"],
        "an edited last unit is compiled over its own slot, after a predecessor that is the store's"
    );
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "and nothing was filed, so the disk's text is the store's still"
    );
}

/// A hit is verified against the text the compilation would read, so an open document refuses it only when its text differs from what the unit was compiled from — and a document the unit never read, wherever it lies, refuses nothing. A containment rule would refuse a package's library whenever any document under its directory is open, which is where every executable of the package lives.
#[test]
fn an_open_document_refuses_a_hit_only_when_it_is_edited() {
    let root = mounted_project("overlay");
    built(&root);

    assert_eq!(
        folded(&root, &held(&root, "a/lib.crs")),
        ["reused /alpha", "reused /beta"],
        "an open document holding the disk's text is the text the unit was compiled from"
    );

    let beside = Overlay::of(BTreeMap::from([(
        root.join("a/exe.crs"),
        "-- a program beside the library".to_string(),
    )]));
    assert_eq!(
        folded(&root, &beside),
        ["reused /alpha", "reused /beta"],
        "a document the unit never read leaves the hit standing"
    );
}

/// A file rewritten while its unit compiles leaves the store as it was: the disk is asked as the unit is handed over, and no longer holds the text the unit was compiled from.
///
/// The rewrite lands between the compile and the hand-over, which is where a save during a long check lands. `/beta` is then a unit after one that was not filed.
#[test]
fn a_file_rewritten_while_its_unit_compiles_leaves_the_store_as_it_was() {
    /// The question's cache, with one file rewritten as the first unit is handed over.
    struct Rewriting<'a> {
        reached: Overlaid<'a>,
        file: PathBuf,
        rewritten: Cell<bool>,
    }

    impl Cache for Rewriting<'_> {
        fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
            self.reached.get(source)
        }

        fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Unit> {
            self.reached.baseline(source, offered)
        }

        fn put(&self, source: &UnitSource<'_>, unit: &Unit, followed: bool) {
            if !self.rewritten.replace(true) {
                fs::write(
                    &self.file,
                    "use /std/{Str};\n\npub let said: Str =\n    \"rewritten\";\n",
                )
                .expect("a rewritten module");
            }
            self.reached.put(source, unit, followed);
        }
    }

    let root = mounted_project("rewritten");
    let overlay = Overlay::default();
    let store = Verdicts::at(root.to_path_buf());
    let rewriting = Rewriting {
        reached: Overlaid::over(&store, &overlay),
        file: root.join("a/lib.crs"),
        rewritten: Cell::new(false),
    };

    assert_eq!(
        folded_through(&root, &overlay, &rewriting),
        ["compiling /alpha", "compiling /beta"]
    );
    assert!(
        slots(&root).is_empty(),
        "a unit was filed for a text the disk no longer holds, or after one"
    );
}

/// A package the store cannot take recompiles over what the session compiled last, rather than compiling whole every time: no slot can be written, so the unit a check produced is the only baseline there is.
///
/// **The baseline advances without the store.** A unit is kept whether or not it is filed, so a session over a store nobody can write still recompiles the closure of each edit, and a question with no session is where it was.
#[test]
fn a_session_recompiles_what_the_store_could_not_take() {
    let root = mounted_project("session-unfiled");
    unwritable(&root);
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /alpha", "compiling /beta"],
        "nothing filed and nothing kept yet"
    );
    assert_eq!(
        folded_reusing(&root, &edited(&root, "b/lib.crs"), &session),
        ["recompiling /alpha", "recompiling /beta"],
        "both over what the first check kept"
    );
    assert_eq!(
        folded(&root, &edited(&root, "b/lib.crs")),
        ["compiling /alpha", "compiling /beta"],
        "and a question with no session is what it was"
    );
}

/// A unit after an edited one is offered what the session kept for it, once the edited one is back to what it held when that unit was kept. The store cannot do this: its slot for the second unit was filed after the first unit's disk text, which the edit has moved away from for as long as the edit stands.
#[test]
fn a_session_offers_a_unit_downstream_of_an_edit_the_store_cannot() {
    let root = mounted_project("session-downstream");
    built(&root);
    let session = Session::default();
    let overlay = edited(&root, "a/lib.crs");

    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["recompiling /alpha", "compiling /beta"],
        "the first check has only the store, whose slot for /beta vouches for the /alpha on disk"
    );
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["recompiling /alpha", "recompiling /beta"],
        "the second has what the first kept, compiled after the /alpha the overlay still holds"
    );
}

/// A unit compiled in a second scope replaces what the session kept for it in the first, rather than sitting beside it: the session holds one unit per unit, so what it holds is bounded by the units the editor reached and never by how often their scope moved.
///
/// Observed from the far side of the bound, over a `/beta` held edited throughout, so that the store never holds it and the session is all there is. `/beta` is compiled after `/alpha`, then with its dependency dropped from the manifest, then after `/alpha` again — and the third check compiles it whole, since the second replaced what the first kept. A session keyed by slot would still hold the first scope's unit, and would have recompiled over it, beside a stranded unit per scope the session had ever seen.
#[test]
fn a_unit_kept_in_a_new_scope_replaces_what_its_old_scope_kept() {
    let root = mounted_project("session-rescoped");
    let manifest = fs::read_to_string(root.join("b/curios.toml")).expect("a written manifest");
    let session = Session::default();
    let overlay = edited(&root, "b/lib.crs");

    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["compiling /alpha", "compiling /beta"],
        "after /alpha"
    );

    write(&root, "b/curios.toml", "name = \"beta\"\n");
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["compiling /beta"],
        "alone, a scope nothing was kept in"
    );

    write(&root, "b/curios.toml", &manifest);
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["reused /alpha", "compiling /beta"],
        "after /alpha again: /alpha's unit is the store's, and /beta's was replaced"
    );
}

/// A session files a dependency when it compiles it, and not the unit its editor holds edited: every keystroke after that leaves the store as it was.
#[test]
fn a_session_files_a_dependency_and_no_keystroke() {
    let root = mounted_project("session-files");
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &edited(&root, "b/lib.crs"), &session),
        ["compiling /alpha", "compiling /beta"]
    );
    let filed = slots(&root);
    assert_eq!(
        filed.len(),
        1,
        "/alpha, compiled whole from the disk; /beta is held edited"
    );

    let keystroke = Overlay::of(BTreeMap::from([(
        root.join("b/lib.crs"),
        "use /std/{Str};\n\npub let said: Str =\n    \"beta, typed\";\n".to_string(),
    )]));
    assert_eq!(
        folded_reusing(&root, &keystroke, &session),
        ["reused /alpha", "recompiling /beta"]
    );
    assert!(slots(&root) == filed, "a keystroke moved the store");
}

/// A unit a session compiles whole after a saved manifest edit moved its scope is filed where its new scope addresses it, beside the slots its old scope filed.
#[test]
fn a_unit_compiled_whole_in_a_new_scope_is_filed_there() {
    let root = mounted_project("session-rescoped-files");
    let session = Session::default();

    folded_reusing(&root, &Overlay::default(), &session);
    let before = slots(&root);
    assert_eq!(before.len(), 2, "/alpha, and /beta after it");

    write(&root, "b/curios.toml", "name = \"beta\"\n");
    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /beta"],
        "alone, a scope no slot was filed in and nothing was kept for"
    );
    let after = slots(&root);
    assert_eq!(after.len(), 3, "/beta alone is another slot");
    assert!(
        before
            .iter()
            .all(|(slot, bytes)| after.get(slot) == Some(bytes)),
        "and the old scope's slots hold what they held"
    );
}

/// A kept unit is refused once a unit before it holds something else, and what it would have hidden is reported.
///
/// **The guard a slot cannot provide.** A slot addresses a unit's predecessors by where they are, not by what they hold, and the recompile diffs a unit's own lowered items alone — a reference into an edited predecessor lowers to the same name either way. So without the guard, `/beta`'s kept unit would be offered after `/alpha` changed its declared type, the diff would be empty, every item reused, and the mismatch this asserts never reported.
#[test]
fn a_kept_unit_after_a_changed_predecessor_is_refused() {
    let root = mounted_project("session-guard");
    write(&root, "b/lib.crs", READER);
    let session = Session::default();

    let clean = checked(&root, &Overlay::default(), &session);
    assert!(clean.is_empty(), "{clean:?}");

    let retyped = Overlay::of(BTreeMap::from([(
        root.join("a/lib.crs"),
        RETYPED.to_string(),
    )]));
    assert_mismatch(&checked(&root, &retyped, &session));
}

/// The guard holds over a predecessor the store could not take: a unit is kept, and what it read is logged, whether or not it is filed.
///
/// `/beta` is held edited, so it is kept and never filed, and `/alpha` is compiled whole from the disk twice, the store taking neither. A cache that logged only what it did not file would keep `/beta` after an empty log, offer it again after `/alpha` had changed its declared type, and report nothing.
#[test]
fn a_kept_unit_is_refused_once_a_predecessor_the_store_could_not_take_has_changed() {
    let root = mounted_project("session-guard-unfiled");
    unwritable(&root);
    let session = Session::default();
    let overlay = Overlay::of(BTreeMap::from([(
        root.join("b/lib.crs"),
        format!("{READER}\n-- held\n"),
    )]));

    let clean = checked(&root, &overlay, &session);
    assert!(clean.is_empty(), "{clean:?}");

    write(&root, "a/lib.crs", RETYPED);
    assert_mismatch(&checked(&root, &overlay, &session));
}

/// A declaration the parser cannot read answers with its own record, over a baseline as with none.
///
/// **Recovery and the item-level recompile meet here.** A question takes a baseline where a build compiles whole, so this path is the editor's and `curios lint`'s alone. A broken item withholds its dependents, and a withheld item leaves no refusal behind it — so a recompile that reassembled the lowered order expecting every item would look for items its own elaboration deliberately did not produce, and a keystroke leaving a half-written declaration with a dependent would kill the analyst instead of answering.
///
/// Two declarations are the smallest shape that shows it: one broken, and one naming it, which is the one that goes missing.
#[test]
fn a_broken_declaration_over_a_baseline_is_its_own_record() {
    let root = mounted_project("broken");
    let module = "use /std/{Str};\n\npub let greeting: Str =\n    \"beta\";\n\npub let said: Str =\n    greeting;\n";
    write(&root, "b/lib.crs", module);
    built(&root);

    let overlay = Overlay::of(BTreeMap::from([(
        root.join("b/lib.crs"),
        module.replace("\"beta\"", ""),
    )]));
    let store = Verdicts::at(root.to_path_buf());
    let diagnostics = diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Unit {
            units: crate::overlaid(mounted(&root), &overlay),
        },
        &overlay,
        Some(&store),
    );

    let [record] = diagnostics.as_slice() else {
        panic!("one record, got {diagnostics:?}");
    };
    assert_eq!(record.severity, Severity::Error);
    assert!(
        record.report.message.contains("expected a term"),
        "{}",
        record.report.render()
    );
}

/// A store that cannot be written is named beside the answer, which is the one it would have been.
#[test]
fn a_store_that_cannot_be_written_is_named_beside_the_answer() {
    let refused = mounted_project("unfiled-named");
    unwritable(&refused);
    let taken = mounted_project("unfiled-none");
    let overlay = Overlay::default();
    let asked = |root: &Path| {
        let store = Verdicts::at(root.to_path_buf());

        diagnosed(
            DEFAULT_STEP_BUDGET,
            Subject::Unit {
                units: crate::overlaid(mounted(root), &overlay),
            },
            &overlay,
            Some(&store),
        )
    };

    let answer = asked(&refused);
    assert!(answer.diagnostics.is_empty(), "{:?}", answer.diagnostics);
    assert!(
        answer
            .unfiled
            .as_deref()
            .is_some_and(|refusal| refusal.contains("verdicts")),
        "the refusal names the slot it happened at: {:?}",
        answer.unfiled
    );

    let answer = asked(&taken);
    assert!(answer.diagnostics.is_empty(), "{:?}", answer.diagnostics);
    assert_eq!(
        answer.unfiled, None,
        "a store that took the units says nothing"
    );
}

/// `/beta`'s library reading a name of `/alpha`'s, at the type `/alpha` first declares it.
const READER: &str = "use /std/{Str};\n\npub let said: Str =\n    /alpha/said;\n";

/// `/alpha`'s library with that name declared at another type.
const RETYPED: &str = "use /std/{Nat};\n\npub let said: Nat =\n    1;\n";

/// That a check reported `/beta`'s reference at the type it no longer has.
fn assert_mismatch(diagnostics: &[crate::Diagnosis]) {
    assert!(
        diagnostics
            .iter()
            .any(|record| record.severity == Severity::Error
                && record.report.message.contains("type mismatch")),
        "{:?}",
        diagnostics
            .iter()
            .map(|record| record.report.render())
            .collect::<Vec<_>>()
    );
}

/// What a build did to each of `root`'s units, filing them through the store itself.
fn built(root: &Path) -> Vec<String> {
    let store = Verdicts::at(root.to_path_buf());

    folded_through(root, &Overlay::default(), &store)
}

/// What the fold did to each of `root`'s units, checked through [`Overlaid`] over `overlay` — the store as `wonder` hands a query.
fn folded(root: &Path, overlay: &Overlay) -> Vec<String> {
    folded_reusing(root, overlay, &Session::default())
}

/// [`folded`], compiling over what `session` kept — the store the language server hands a query.
fn folded_reusing(root: &Path, overlay: &Overlay, session: &Session) -> Vec<String> {
    let mut store = Verdicts::at(root.to_path_buf());
    store.reuse(session.clone());

    folded_through(root, overlay, &Overlaid::over(&store, overlay))
}

/// What the fold did to each of `root`'s units, read through `overlay` and `cache`.
fn folded_through(root: &Path, overlay: &Overlay, cache: &dyn Cache) -> Vec<String> {
    let units = crate::overlaid(mounted(root), overlay);

    let mut events = Vec::new();
    Fold::new(DEFAULT_STEP_BUDGET, &units, Some(cache))
        .check_units(|progress| match progress {
            Progress::Compiling(prefix) => events.push(format!("compiling {}", prefix.join())),
            Progress::Recompiling(prefix) => {
                events.push(format!("recompiling {}", prefix.join()));
            }
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        })
        .expect("two compiling units");

    events
}

/// Every record the second package reports over `overlay`, compiled over what `session` kept — the question the language server asks about a library.
fn checked(root: &Path, overlay: &Overlay, session: &Session) -> Vec<crate::Diagnosis> {
    let mut store = Verdicts::at(root.to_path_buf());
    store.reuse(session.clone());

    diagnostics(
        DEFAULT_STEP_BUDGET,
        Subject::Unit {
            units: crate::overlaid(mounted(root), overlay),
        },
        overlay,
        Some(&store),
    )
}

/// Every unit slot the store at `root` holds, by name, with its bytes.
fn slots(root: &Path) -> BTreeMap<String, Vec<u8>> {
    let Ok(entries) = fs::read_dir(root.join(".curios/verdicts")) else {
        return BTreeMap::new();
    };

    entries
        .map(|slot| {
            let slot = slot.expect("a readable store");
            (
                slot.file_name().to_string_lossy().into_owned(),
                fs::read(slot.path()).expect("a readable slot"),
            )
        })
        .collect()
}

/// Leave the store at `root` unable to take a unit: a file where its slots would go.
fn unwritable(root: &Path) {
    write(root, ".curios/verdicts", "");
}

/// An overlay holding `path`'s own text: a document an editor has open and has not yet changed.
fn held(root: &Path, path: &str) -> Overlay {
    let path = root.join(path);
    let text = fs::read_to_string(&path).expect("a written module");

    Overlay::of(BTreeMap::from([(path, text)]))
}

/// An overlay holding `path` with a line added: a document an editor has open and changed.
fn edited(root: &Path, path: &str) -> Overlay {
    let path = root.join(path);
    let text = fs::read_to_string(&path).expect("a written module");

    Overlay::of(BTreeMap::from([(path, format!("{text}\n-- edited\n"))]))
}
