//! What a question takes from the store and from its session, and never gives back: a hit verified against the text the compilation would read, a unit placed without being filed, and a baseline — filed or kept — that a unit is recompiled over only while the units before it still hold what they held when it was made.

use {
    super::test_support::{mounted, mounted_project, write},
    crate::{ReadOnly, Severity, Subject, diagnostics},
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, Progress, check_units_with_prelude},
    curios_text::Overlay,
    curios_verdicts::Verdicts,
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

    check_units_with_prelude(
        DEFAULT_STEP_BUDGET,
        &mounted(root),
        Some(&store as &dyn Cache),
        |_| {},
    )
    .expect("two compiling units");
}

/// What the fold did to each of `root`'s units, checked through [`ReadOnly`] over `overlay` — the store `wonder` hands a query.
fn folded(root: &Path, overlay: &Overlay) -> Vec<String> {
    let units = crate::overlaid(mounted(root), overlay);
    let store = Verdicts::at(root.to_path_buf());
    let read_only = ReadOnly {
        cache: &store,
        overlay,
    };

    let mut events = Vec::new();
    check_units_with_prelude(
        DEFAULT_STEP_BUDGET,
        &units,
        Some(&read_only as &dyn Cache),
        |progress| match progress {
            Progress::Compiling(prefix) => events.push(format!("compiling {}", prefix.join())),
            Progress::Recompiling(prefix) => {
                events.push(format!("recompiling {}", prefix.join()));
            }
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        },
    )
    .expect("two compiling units");

    events
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
