//! What a question takes from the cabinet and its session, and what it gives back: a hit verified against the text the compilation would read, a unit filed where the disk holds every text the fold has read, whole or over a baseline, and kept and placed otherwise, and a baseline — filed or kept — that a unit is recompiled over only while the units before it still hold what they held when it was made.

use {
    super::test_support::*,
    crate::*,
    curios_common::{RootKind, test_support::*},
    curios_elab::DEFAULT_BUDGET,
    curios_module::{Overlay, RootSource, UnitSource},
    curios_package::Cabinet,
    curios_pipeline::{Cache, Earlier, EntryTail, Fold, Progress},
    curios_unit::Unit,
    curios_verdict::{Session, Verdicts},
    std::{
        cell::{Cell, RefCell},
        collections::BTreeMap,
        fs,
        path::{Path, PathBuf},
    },
};

/// A question files each unit it compiled whole from the text on disk, so the next command starts from the cabinet, whichever command it is.
#[test]
fn a_question_files_what_it_compiled_whole_from_the_disk() {
    let root = mounted_project("question-files");

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["compiling /alpha", "compiling /beta"],
        "nothing is filed for a package nothing has compiled"
    );
    assert_eq!(objects(&root).len(), 2, "one address per unit");
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "the next question starts from the cabinet"
    );
    assert_eq!(
        built(&root),
        ["reused /alpha", "reused /beta"],
        "and so does a build"
    );
}

/// One unit, whoever files it: the objects a question files hold the bytes a build files there, record and unit alike.
///
/// This is what lets a build believe a question's unit, and it fails if a question's fold ever stops short of a build's.
#[test]
fn a_question_and_a_build_file_the_same_bytes() {
    let root = mounted_project("one-unit");

    folded(&root, &Overlay::default());
    let asked = objects(&root);
    fs::remove_dir_all(Cabinet::of(&root, None).objects()).expect("the objects a question filed");
    built(&root);

    assert_eq!(asked.len(), 2, "one address per unit");
    assert!(
        objects(&root) == asked,
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
    assert_eq!(
        objects(&root).len(),
        2,
        "both units stand on the disk's text"
    );
}

/// A unit whose record still agrees comes from the cabinet, however many units before it missed.
///
/// A missing unit is compiled whole from the same text into the same bytes and filed where it was, so the unit after it, whose record vouches for those bytes, is reused. Which object is the first unit's is not spelled here, so each is taken out in turn and the two outcomes are read together.
///
/// Read off the fold's own progress events, as the cabinet's own tests are: whether a unit was reused is exactly the question a caller asks, and asserting on an object's address would pass just as happily with the chain broken.
#[test]
fn a_miss_does_not_refuse_the_units_after_it() {
    let root = mounted_project("chain");
    built(&root);

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "both units are the cabinet's when nothing is missing"
    );

    let units = Cabinet::of(&root, None).unit_files();
    assert_eq!(units.len(), 2, "one object per unit: {units:?}");

    let mut outcomes = units
        .iter()
        .map(|unit| {
            let aside = unit.with_extension("aside");
            fs::rename(unit, &aside).unwrap();
            let events = folded(&root, &Overlay::default());
            fs::rename(&aside, unit).unwrap();
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
        "the unit after a missing one is still the cabinet's, since the one compiled again holds the bytes it held"
    );
}

/// A unit compiled after one from unsaved text is not filed, and its object holds the bytes it held.
///
/// An edited unit recompiles the units that could have read it: a unit's record vouches for what each unit it may name contained, and one that no longer contains it is a disagreement whether or not anything was read of it. `/beta` is then compiled over its object from text the disk holds — and filed, it would take the place of the object a build filed, under a record naming an `/alpha` nothing on disk reproduces.
#[test]
fn a_unit_after_one_compiled_from_unsaved_text_is_not_filed() {
    let root = mounted_project("unsaved-before");
    built(&root);
    let before = objects(&root);

    assert_eq!(
        folded(&root, &edited(&root, "a/lib.crs")),
        ["recompiling /alpha", "recompiling /beta"],
        "the overlay edits /alpha alone, which is compiled over its object; /beta's record vouched for the /alpha it was compiled after, so its object is no hit, and is the baseline it is compiled over"
    );
    assert!(
        objects(&root) == before,
        "a keystroke in /alpha's document moved an object"
    );
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "so the disk's text is the cabinet's still"
    );
}

/// One unit, however it was compiled: over a text saved since the cabinet was filed, a question compiles the unit over its object and files it, and the unit after it, and each object holds the bytes a build files there.
///
/// This is what lets a build believe a unit a question compiled over a baseline, and the next question start from it.
#[test]
fn a_unit_compiled_over_a_baseline_is_filed_as_a_build_files_it() {
    let root = mounted_project("baseline-files");
    built(&root);
    let before = objects(&root);

    write(
        &root,
        "a/lib.crs",
        "use /std/{Str};\n\npub let said: Str =\n    \"alpha, saved\";\n",
    );

    assert_eq!(
        folded(&root, &Overlay::default()),
        ["recompiling /alpha", "recompiling /beta"]
    );
    let asked = objects(&root);
    assert!(asked != before, "a question filed nothing for a saved text");
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "the next question starts from what the first filed"
    );

    fs::remove_dir_all(Cabinet::of(&root, None).objects()).expect("the objects a question filed");
    assert_eq!(built(&root), ["compiling /alpha", "compiling /beta"]);
    assert!(
        objects(&root) == asked,
        "a build filed other bytes than the question did over a baseline"
    );
}

/// A unit whose object disagrees with its files is compiled over that object's unit rather than from nothing: the object is a baseline, and every declaration the edit did not reach is reused. A query is what takes it; the disk is untouched either way.
#[test]
fn an_edited_document_recompiles_its_unit_over_the_stored_one() {
    let root = mounted_project("baseline");
    built(&root);

    assert_eq!(
        folded(&root, &edited(&root, "b/lib.crs")),
        ["reused /alpha", "recompiling /beta"],
        "an edited last unit is compiled over its own object, after a predecessor that is the cabinet's"
    );
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["reused /alpha", "reused /beta"],
        "and nothing was filed, so the disk's text is the cabinet's still"
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

/// A file rewritten while its unit compiles leaves the cabinet as it was: the disk is asked as the unit is handed over, and no longer holds the text the unit was compiled from.
///
/// The rewrite lands between the compile and the hand-over, which is where a save during a long check lands. `/beta` is then a unit after one that was not filed.
#[test]
fn a_file_rewritten_while_its_unit_compiles_leaves_the_cabinet_as_it_was() {
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

        fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
            self.reached.baseline(source, offered)
        }

        fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
            if !self.rewritten.replace(true) {
                fs::write(
                    &self.file,
                    "use /std/{Str};\n\npub let said: Str =\n    \"rewritten\";\n",
                )
                .expect("a rewritten module");
            }
            self.reached.put(source, unit);
        }
    }

    let root = mounted_project("rewritten");
    let overlay = Overlay::default();
    let verdicts = Verdicts::at(Cabinet::of(&root, None));
    let rewriting = Rewriting {
        reached: Overlaid::over(&verdicts, &overlay),
        file: root.join("a/lib.crs"),
        rewritten: Cell::new(false),
    };

    assert_eq!(
        folded_through(&root, &overlay, &rewriting),
        ["compiling /alpha", "compiling /beta"]
    );
    assert!(
        objects(&root).is_empty(),
        "a unit was filed for a text the disk no longer holds, or after one"
    );
}

/// A package the cabinet cannot take recompiles over what the session compiled last, rather than compiling whole every time: no object can be written, so the unit a check produced is the only baseline there is.
///
/// **The baseline advances without the cabinet.** A unit is kept whether or not it is filed, so a session over a cabinet nobody can write still recompiles what each edit moved, and a question with no session is where it was.
#[test]
fn a_session_recompiles_what_the_cabinet_could_not_take() {
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

/// A unit after an edited one is compiled over a baseline at every check: over the cabinet's object at the first, though that was filed after the first unit's disk text, each stage asking again what it read of the unit that moved, and over what the session kept from then on, which was compiled after the text the edit still holds.
#[test]
fn a_unit_downstream_of_an_edit_is_compiled_over_a_baseline_from_the_first_check() {
    let root = mounted_project("session-downstream");
    built(&root);
    let session = Session::default();
    let overlay = edited(&root, "a/lib.crs");

    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["recompiling /alpha", "recompiling /beta"],
        "the first check has only the cabinet, whose object for /beta is a baseline though it vouches for the /alpha on disk"
    );
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["recompiling /alpha", "recompiling /beta"],
        "the second has what the first kept, compiled after the /alpha the overlay still holds"
    );
}

/// A unit compiled in a second scope replaces what the session kept for it in the first, rather than sitting beside it: the session holds one unit per unit, so what it holds is bounded by the units the editor reached and never by how often their scope moved.
///
/// Observed from the far side of the bound, over a `/beta` held edited throughout, so that the cabinet never holds it and the session is all there is. `/beta` is compiled after `/alpha`, then with its dependency dropped from the manifest, then after `/alpha` again — and the third check compiles it whole, since the second replaced what the first kept. A session keyed by object would still hold the first scope's unit, and would have recompiled over it, beside a stranded unit per scope the session had ever seen.
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

    write(
        &root,
        "b/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"beta\"\n"
        ),
    );
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["compiling /beta"],
        "alone, a scope nothing was kept in"
    );

    write(&root, "b/curios.toml", &manifest);
    assert_eq!(
        folded_reusing(&root, &overlay, &session),
        ["reused /alpha", "compiling /beta"],
        "after /alpha again: /alpha's unit is the cabinet's, and /beta's was replaced"
    );
}

/// A session keeps a unit the cabinet restored for it as it keeps one it compiled: with the objects gone, the next check recompiles each unit over the copy in hand, where a question with no session has only what that check filed.
///
/// What this spares an editor is the second read of an object: the first edit after a check the cabinet answered misses, asks for a baseline, and is handed the unit already decoded.
#[test]
fn a_session_keeps_a_unit_the_cabinet_restored() {
    let root = mounted_project("session-restored");
    built(&root);
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["reused /alpha", "reused /beta"]
    );

    fs::remove_dir_all(Cabinet::of(&root, None).objects()).expect("the objects a build filed");
    assert_eq!(
        folded_reusing(&root, &edited(&root, "b/lib.crs"), &session),
        ["recompiling /alpha", "recompiling /beta"],
        "each over what the first check restored"
    );
    assert_eq!(
        folded(&root, &edited(&root, "b/lib.crs")),
        ["reused /alpha", "compiling /beta"],
        "and a question with no session has only the cabinet, where the /alpha the disk holds was filed again and the /beta held edited was not"
    );
}

/// A session answers a hit on an object it filed with the unit it compiled, and reads nothing of the object past its record: with every filed artifact damaged, the session's next check still takes each unit from the cabinet, where a question with no session reads the damage and compiles each again.
///
/// What this spares an editor is the decode after a save: the save's check files the unit, the question after it hits that object, and the session goes on holding the unit it compiled in place of a copy read back out of the bytes it just wrote.
#[test]
fn a_session_answers_a_hit_on_what_it_filed_with_the_unit_it_compiled() {
    let root = mounted_project("session-filed");
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /alpha", "compiling /beta"]
    );

    damaged(&root);
    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["reused /alpha", "reused /beta"],
        "each a hit on what the session filed, answered with the unit in hand"
    );
    assert_eq!(
        folded(&root, &Overlay::default()),
        ["compiling /alpha", "compiling /beta"],
        "and a question with no session reads each damaged artifact, and neither believes nor compiles over it"
    );
}

/// A session files a dependency when it compiles it, and not the unit its editor holds edited: every keystroke after that leaves the cabinet as it was.
#[test]
fn a_session_files_a_dependency_and_no_keystroke() {
    let root = mounted_project("session-files");
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &edited(&root, "b/lib.crs"), &session),
        ["compiling /alpha", "compiling /beta"]
    );
    let filed = objects(&root);
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
    assert!(objects(&root) == filed, "a keystroke moved the cabinet");
}

/// A unit a session compiles whole after a saved manifest edit moved its scope is filed where its new scope addresses it, beside the objects its old scope filed.
#[test]
fn a_unit_compiled_whole_in_a_new_scope_is_filed_there() {
    let root = mounted_project("session-rescoped-files");
    let session = Session::default();

    folded_reusing(&root, &Overlay::default(), &session);
    let before = objects(&root);
    assert_eq!(before.len(), 2, "/alpha, and /beta after it");

    write(
        &root,
        "b/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"beta\"\n"
        ),
    );
    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /beta"],
        "alone, a scope no object was filed in and nothing was kept for"
    );
    let after = objects(&root);
    assert_eq!(after.len(), 3, "/beta alone is another object");
    assert!(
        before
            .iter()
            .all(|(address, bytes)| after.get(address) == Some(bytes)),
        "and the old scope's objects hold what they held"
    );
}

/// A kept unit is refused once a unit before it holds something else, and what it would have hidden is reported.
///
/// **The guard an address cannot provide.** An address names nothing of what a unit's predecessors hold, and the recompile diffs a unit's own lowered items alone — a reference into an edited predecessor lowers to the same name either way. So without the guard, `/beta`'s kept unit would be offered after `/alpha` changed its declared type, the diff would be empty, every item reused, and the mismatch this asserts never reported.
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

/// The guard holds over a predecessor the cabinet could not take: a unit is kept, and what it read is logged, whether or not it is filed.
///
/// `/beta` is held edited, so it is kept and never filed, and `/alpha` is compiled whole from the disk twice, the cabinet taking neither. A cache that logged only what it did not file would keep `/beta` after an empty log, offer it again after `/alpha` had changed its declared type, and report nothing.
#[test]
fn a_kept_unit_is_refused_once_a_predecessor_the_cabinet_could_not_take_has_changed() {
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
/// **Recovery and the item-level recompile meet here.** A question compiles over a baseline, the session's or the cabinet's. A broken item withholds its dependents, and a withheld item leaves no refusal behind it — so a recompile that expected an item of every declaration would look for items its own elaboration deliberately did not produce, and a keystroke leaving a half-written declaration with a dependent would kill the analyst instead of answering.
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
    let verdicts = Verdicts::at(Cabinet::of(&root, None));
    let diagnostics = diagnostics(
        DEFAULT_BUDGET,
        Subject::Unit {
            units: crate::overlaid(mounted(&root), &overlay),
        },
        &overlay,
        Some(&verdicts),
        &mut |_| {},
    )
    .diagnostics;

    let [record] = diagnostics.as_slice() else {
        panic!("one record, got {diagnostics:?}");
    };
    assert_eq!(record.severity, Severity::Error);
    assert!(
        record.report.message().contains("expected: a term"),
        "{}",
        record.report.render()
    );
}

/// A cabinet that cannot be written costs nothing of the answer, which is the one it would have been, and holds why it took nothing for whoever narrates the compile.
#[test]
fn a_cabinet_that_cannot_be_written_answers_as_one_that_can() {
    let refused = mounted_project("unfiled-named");
    unwritable(&refused);
    let taken = mounted_project("unfiled-none");
    let overlay = Overlay::default();
    let asked = |root: &Path| {
        let verdicts = Verdicts::at(Cabinet::of(root, None));
        let answer = diagnostics(
            DEFAULT_BUDGET,
            Subject::Unit {
                units: crate::overlaid(mounted(root), &overlay),
            },
            &overlay,
            Some(&verdicts),
            &mut |_| {},
        );

        (answer, verdicts.refused())
    };

    let (answer, refusal) = asked(&refused);
    assert!(answer.diagnostics.is_empty(), "{:?}", answer.diagnostics);
    assert!(
        refusal
            .as_deref()
            .is_some_and(|refusal| refusal.contains("objects")),
        "the refusal names the family it happened in: {refusal:?}"
    );

    let (answer, refusal) = asked(&taken);
    assert!(answer.diagnostics.is_empty(), "{:?}", answer.diagnostics);
    assert_eq!(refusal, None, "a cabinet that took the units says nothing");
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
                && record.report.message().contains("type mismatch")),
        "{:?}",
        diagnostics
            .iter()
            .map(|record| record.report.render())
            .collect::<Vec<_>>()
    );
}

/// What a build did to each of `root`'s units, filing them through the cabinet itself.
fn built(root: &Path) -> Vec<String> {
    let verdicts = Verdicts::at(Cabinet::of(root, None));

    folded_through(root, &Overlay::default(), &verdicts)
}

/// What the fold did to each of `root`'s units, checked through [`Overlaid`] over `overlay` — the cabinet as `wonder` hands a query.
fn folded(root: &Path, overlay: &Overlay) -> Vec<String> {
    folded_reusing(root, overlay, &Session::default())
}

/// [`folded`], compiling over what `session` kept — the cabinet the language server hands a query.
fn folded_reusing(root: &Path, overlay: &Overlay, session: &Session) -> Vec<String> {
    let mut verdicts = Verdicts::at(Cabinet::of(root, None));
    verdicts.reuse(session.clone());

    folded_through(root, overlay, &Overlaid::over(&verdicts, overlay))
}

/// What the fold did to each of `root`'s units, read through `overlay` and `cache`.
fn folded_through(root: &Path, overlay: &Overlay, cache: &dyn Cache) -> Vec<String> {
    let units = crate::overlaid(mounted(root), overlay);

    let mut events = Vec::new();
    Fold::new(DEFAULT_BUDGET, &units, Some(cache))
        .check_units(|progress| match progress {
            Progress::Compiling { unit: prefix, .. } => {
                events.push(format!("compiling {}", prefix.join()))
            }
            Progress::Recompiling { unit: prefix, .. } => {
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
    let mut verdicts = Verdicts::at(Cabinet::of(root, None));
    verdicts.reuse(session.clone());

    diagnostics(
        DEFAULT_BUDGET,
        Subject::Unit {
            units: crate::overlaid(mounted(root), overlay),
        },
        overlay,
        Some(&verdicts),
        &mut |_| {},
    )
    .diagnostics
}

/// Every object the cabinet at `root` holds, by its address, with its unit file's bytes.
fn objects(root: &Path) -> BTreeMap<String, Vec<u8>> {
    let Ok(entries) = fs::read_dir(Cabinet::of(root, None).objects()) else {
        return BTreeMap::new();
    };

    entries
        .map(|object| {
            let name = object
                .expect("a readable object")
                .file_name()
                .to_string_lossy()
                .into_owned();
            let bytes =
                fs::read(Cabinet::of(root, None).unit(&name)).expect("a readable unit file");
            (name, bytes)
        })
        .collect()
}

/// Damage the last byte of every object's unit file the cabinet at `root` holds: the artifact's, behind a record that still reads.
fn damaged(root: &Path) {
    for (name, mut bytes) in objects(root) {
        let last = bytes.last_mut().expect("a unit file holds an artifact");
        *last ^= 0xff;
        fs::write(Cabinet::of(root, None).unit(&name), bytes).expect("a writable unit file");
    }
}

/// Leave the cabinet at `root` unable to take a unit: a file where its objects would go.
fn unwritable(root: &Path) {
    let objects = Cabinet::of(root, None).objects();
    fs::create_dir_all(objects.parent().unwrap()).unwrap();
    fs::write(objects, "").unwrap();
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

/// A cache read through, noting what it offered the entry's own unit: `recompiling` where it offered a baseline, `compiling` where it offered none.
struct Noting<'a> {
    cache: &'a dyn Cache,
    entry: RefCell<Vec<&'static str>>,
}

impl Cache for Noting<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.cache.get(source)
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        let earlier = self.cache.baseline(source, offered);
        if source.is_entry() {
            self.entry.borrow_mut().push(match earlier {
                Some(_) => "recompiling",
                None => "compiling",
            });
        }

        earlier
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.cache.put(source, unit);
    }
}

/// A file no package declares has no cabinet, and the session that asks about it keeps what it compiled of it all the same. A module is compiled whole at the first check and over what the session kept at the next, with the text an editor holds; and nothing is filed, there being nowhere to file it.
///
/// Mutation-checked: with a handle that has no cabinet given no object to keep a unit at, the second check compiles the module whole.
#[test]
fn a_file_no_package_declares_is_compiled_over_what_its_session_kept() {
    let root = Temporary::new("wonder", "loose-module");
    let file = root.join("Loose.crs");
    let text = "use /std/{Nat};\n\npub let one: Nat = 1;\n\npub let two: Nat = 2;\n";
    write(&root, "Loose.crs", text);
    let session = Session::default();
    let folded = |overlay: &Overlay| {
        let verdicts = Verdicts::kept_by(session.clone());
        // Mounted at its stem, as a loose module is.
        let unit = RootSource::mounted(
            "Loose",
            RootKind::Ordinary,
            &file,
            RootSource::namespace_of(&file),
        );
        let units = crate::overlaid(vec![unit], overlay);

        let mut events = Vec::new();
        Fold::new(
            DEFAULT_BUDGET,
            &units,
            Some(&Overlaid::over(&verdicts, overlay)),
        )
        .check_units(|progress| match progress {
            Progress::Compiling { unit: prefix, .. } => {
                events.push(format!("compiling {}", prefix.join()))
            }
            Progress::Recompiling { unit: prefix, .. } => {
                events.push(format!("recompiling {}", prefix.join()))
            }
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        })
        .expect("a compiling module");

        events
    };

    assert_eq!(folded(&Overlay::default()), ["compiling /Loose"]);
    let held = Overlay::of(BTreeMap::from([(file.clone(), text.replace("= 2", "= 3"))]));
    assert_eq!(folded(&held), ["recompiling /Loose"]);
    assert!(
        !root.join(".curios").exists(),
        "a file with no project was filed"
    );
}

/// The same of a program no package declares: its declarations are a unit the session keeps, and the next question about it compiles them over that unit.
#[test]
fn a_program_no_package_declares_is_compiled_over_what_its_session_kept() {
    let root = Temporary::new("wonder", "loose-program");
    let file = root.join("loose.crs");
    let text = "use /std/{Nat};\n\nlet one: Nat = 1;\n\n/std/print(/std/Nat/to_str(one))\n";
    write(&root, "loose.crs", text);
    let session = Session::default();
    let asked = |overlay: &Overlay| {
        let verdicts = Verdicts::kept_by(session.clone());
        let reached = Overlaid::over(&verdicts, overlay);
        let noting = Noting {
            cache: &reached,
            entry: RefCell::new(Vec::new()),
        };
        let (entrypoint, loader) = crate::open(Origin::File(file.clone()), None, None, overlay)
            .expect("the program parses");
        Fold::new(DEFAULT_BUDGET, &[], Some(&noting))
            .check(&entrypoint, &loader, EntryTail::Program, |_| {})
            .expect("the program lowers")
            .verdict
            .expect("the program checks");

        noting.entry.take()
    };

    assert_eq!(asked(&Overlay::default()), ["compiling"]);
    let held = Overlay::of(BTreeMap::from([(file.clone(), text.replace("= 1", "= 2"))]));
    assert_eq!(asked(&held), ["recompiling"]);
}

/// A program's declarations under `index`, the type `Code`'s one constructor fixes its index at, and `f`, which matches on `Code(X)` and adds to `x`: checked at `Nat`, and refused at `Bool`, which adds to nothing.
fn coded(index: &str) -> String {
    format!(
        "induct Code: (Type) -> Type
| big(): ({index})
end

let f(X: Type, c: Code(X), x: X) -> X =
    match c
    | big() => x + 0
    end;

/std/print(\"\")
"
    )
}

/// Two programs no package declares are one unit to a session where they stand in one directory — the prefix every entry claims, read from one place — so the second is compiled over what the first kept, and judged as its own text says: `b.crs` declares the `f` of `a.crs` over another `Code`, which refuses `f`, and the check of `b.crs` reports the refusal.
#[test]
fn a_program_compiled_over_another_files_unit_is_judged_as_its_own_text_says() {
    let root = Temporary::new("wonder", "neighbours");
    write(&root, "a.crs", &coded("/std/Nat"));
    write(&root, "b.crs", &coded("/std/Bool"));
    let session = Session::default();
    let overlay = Overlay::default();
    let asked = |file: &str| {
        let verdicts = Verdicts::kept_by(session.clone());
        let reached = Overlaid::over(&verdicts, &overlay);
        let noting = Noting {
            cache: &reached,
            entry: RefCell::new(Vec::new()),
        };
        let (entrypoint, loader) = crate::open(Origin::File(root.join(file)), None, None, &overlay)
            .expect("the program parses");
        let verdict = Fold::new(DEFAULT_BUDGET, &[], Some(&noting))
            .check(&entrypoint, &loader, EntryTail::Program, |_| {})
            .expect("the program lowers")
            .verdict
            .map(|_| ())
            .map_err(|error| error.to_string());

        (noting.entry.take(), verdict)
    };

    assert_eq!(asked("a.crs"), (vec!["compiling"], Ok(())));
    let (offered, verdict) = asked("b.crs");
    assert_eq!(
        offered,
        ["recompiling"],
        "the unit of `a.crs` was not offered"
    );
    let refusal = verdict.expect_err("`f` in `b.crs` is refused");
    assert!(
        refusal.contains("wanted: `/std/ops/Add(/std/Bool)`"),
        "{refusal}"
    );
}
