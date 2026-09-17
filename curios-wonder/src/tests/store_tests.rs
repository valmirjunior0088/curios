//! What a question takes from the store and from its session, and never gives back: a hit verified against the text the compilation would read, a unit placed without being filed, and a baseline — filed or kept — that a unit is recompiled over only while the units before it still hold what they held when it was made.

use {
    super::test_support::{mounted, mounted_project, write},
    crate::{ReadOnly, Severity, Subject, diagnostics},
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, Fold, Progress},
    curios_text::Overlay,
    curios_verdicts::{Session, Verdicts},
    std::{collections::BTreeMap, fs, path::Path},
};

/// A unit whose record still agrees comes from the store, however many units before it missed.
///
/// **The regression for placing and filing having been one decision.** [`ReadOnly`] drops the write, and used to drop the placement with it — but a slot is addressed after the units placed before it, so the first miss shifted every later address by one and one miss became a miss for the whole tail. Two mounted units are the smallest shape that can show it: the second is what the first's absence from the chain moves.
///
/// The miss is a slot taken out of the store rather than a document edited in the overlay. An edited predecessor recompiles into different bytes, and the successor's record — which vouches for the bytes every predecessor contained, since its arena is the whole prefix's — then disagrees on its own account, which reads exactly as a dropped placement would; a missing slot recompiles the same text into the same bytes, so the successor is reused precisely when it was placed after them. Which slot is the first unit's is not spelled here, so each is taken out in turn and the two outcomes are read together.
///
/// Read off the fold's own progress events, as the store's own tests are: whether a unit was reused is exactly the question a caller asks, and asserting on a slot name would pass just as happily with the chain broken.
#[test]
fn a_miss_does_not_refuse_the_units_after_it() {
    let root = mounted_project("read-only-chain");

    // Filed by an ordinary build, since a query reads a store it never writes.
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
        "the unit after a missing one is still the store's, since the recompiled one was placed before it"
    );
}

/// An edited predecessor recompiles the units after it: a unit's arena is the whole prefix's, so its record vouches for the bytes every unit before it contained, and a predecessor that no longer contains them is a disagreement whether or not anything is imported from it.
#[test]
fn an_edited_unit_recompiles_the_units_after_it() {
    let root = mounted_project("read-only-edited");
    built(&root);

    assert_eq!(
        folded(&root, &edited(&root, "a/lib.crs")),
        ["recompiling /alpha", "compiling /beta"],
        "the overlay edits /alpha alone, which is compiled over its slot; /beta's record vouched for the /alpha it was compiled after, and its slot was filed after a chain that has moved, so it is neither a hit nor a baseline"
    );
}

/// A unit whose slot disagrees with its files is compiled over that slot's unit rather than from nothing: the slot is a baseline, and every declaration the edit did not reach is reused. A query is what takes it; the disk is untouched either way.
#[test]
fn an_edited_document_recompiles_its_unit_over_the_stored_one() {
    let root = mounted_project("read-only-baseline");
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

/// A hit is verified against the text the compilation would read, so an open document refuses it only when its text differs from what the unit was compiled from — and a document the unit never read, wherever it lies, refuses nothing. The containment rule this replaced refused a package's library whenever any document under its directory was open, which is where every executable of the package lives.
#[test]
fn an_open_document_refuses_a_hit_only_when_it_is_edited() {
    let root = mounted_project("read-only-overlay");
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

/// A package nothing was filed for recompiles over what the session compiled last, rather than compiling whole every time: the store has no slot, so the unit a check produced is the only baseline there is.
///
/// **The regression for the baseline never advancing.** A fresh checkout, and every package after a compiler upgrade, has no slot — the key carries the compiler's digest — and a question never files, so an editing session on one compiled its whole unit on every keystroke, measured at 2.2 s against 0.6 s once built.
#[test]
fn a_session_recompiles_what_nothing_was_filed_for() {
    let root = mounted_project("session-unfiled");
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
/// Observed from the far side of the bound. `/beta` is compiled after `/alpha`, then with its dependency dropped from the manifest, then after `/alpha` again — and the third check compiles it whole, since the second replaced what the first kept. A session keyed by slot would still hold the first scope's unit, and would have recompiled over it, beside a stranded unit per scope the session had ever seen.
#[test]
fn a_unit_kept_in_a_new_scope_replaces_what_its_old_scope_kept() {
    let root = mounted_project("session-rescoped");
    let manifest = fs::read_to_string(root.join("b/curios.toml")).expect("a written manifest");
    let session = Session::default();

    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /alpha", "compiling /beta"],
        "after /alpha"
    );

    write(&root, "b/curios.toml", "name = \"beta\"\n");
    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["compiling /beta"],
        "alone, a scope nothing was kept in"
    );

    write(&root, "b/curios.toml", &manifest);
    assert_eq!(
        folded_reusing(&root, &Overlay::default(), &session),
        ["recompiling /alpha", "compiling /beta"],
        "after /alpha again: /alpha's unit was never replaced, /beta's was"
    );
}

/// A kept unit is refused once a unit before it holds something else, and what it would have hidden is reported.
///
/// **The regression for the guard a slot cannot provide.** A slot addresses a unit's predecessors by where they are, not by what they hold, and the recompile diffs a unit's own lowered items alone — a reference into an edited predecessor lowers to the same name either way. So without the guard, `/beta`'s kept unit was offered after `/alpha` changed its declared type, the diff was empty, every item was reused, and the mismatch this asserts was never reported.
#[test]
fn a_kept_unit_after_a_changed_predecessor_is_refused() {
    let root = mounted_project("session-guard");
    write(
        &root,
        "b/lib.crs",
        "use /std/{Str};\n\npub let said: Str =\n    /alpha/said;\n",
    );
    let session = Session::default();

    let clean = checked(&root, &Overlay::default(), &session);
    assert!(clean.is_empty(), "{clean:?}");

    let retyped = Overlay::of(BTreeMap::from([(
        root.join("a/lib.crs"),
        "use /std/{Nat};\n\npub let said: Nat =\n    1;\n".to_string(),
    )]));
    let diagnostics = checked(&root, &retyped, &session);
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

/// A declaration the parser cannot read answers with its own record, over a baseline as with none.
///
/// **The regression for recovery and the item-level recompile never having crossed.** A question takes a baseline where a build compiles whole, so this path is the editor's and `curios lint`'s alone. A broken item withholds its dependents, and a withheld item leaves no refusal behind it — so the recompile reassembled the lowered order looking for items its own elaboration had deliberately not produced, and every keystroke that left a half-written declaration with a dependent in it killed the analyst instead of answering.
///
/// Two declarations are the smallest shape that shows it: one broken, and one naming it, which is the one that goes missing.
#[test]
fn a_broken_declaration_over_a_baseline_is_its_own_record() {
    let root = mounted_project("read-only-broken");
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

/// File both units the way an ordinary build does, through the store itself.
fn built(root: &Path) {
    let store = Verdicts::at(root.to_path_buf());

    Fold::new(
        DEFAULT_STEP_BUDGET,
        &mounted(root),
        Some(&store as &dyn Cache),
    )
    .check_units(|_| {})
    .expect("two compiling units");
}

/// What the fold did to each of `root`'s units, checked through [`ReadOnly`] over `overlay` — the store `wonder` hands a query.
fn folded(root: &Path, overlay: &Overlay) -> Vec<String> {
    folded_reusing(root, overlay, &Session::default())
}

/// [`folded`], compiling over what `session` kept — the store the language server hands a query.
fn folded_reusing(root: &Path, overlay: &Overlay, session: &Session) -> Vec<String> {
    let units = crate::overlaid(mounted(root), overlay);
    let mut store = Verdicts::at(root.to_path_buf());
    store.reuse(session.clone());
    let read_only = ReadOnly {
        cache: &store,
        overlay,
    };

    let mut events = Vec::new();
    Fold::new(DEFAULT_STEP_BUDGET, &units, Some(&read_only as &dyn Cache))
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
fn checked(root: &Path, overlay: &Overlay, session: &Session) -> Vec<crate::Diagnostic> {
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
