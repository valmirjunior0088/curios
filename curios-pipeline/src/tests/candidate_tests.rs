//! How a recompile takes a declaration's elaboration from a candidate a store holds: found by what the declaration was lowered to, and taken where every answer it was given stands and every part it read digests as it did.

use {
    super::test_support::{Store, assert_stored_alike, mounted, recompile_over, unit_of},
    crate::{DEFAULT_BUDGET, compile_unit_over},
    curios_core::{Global, ItemRun},
    curios_elab::{declaration_input, lowered_digest},
    curios_prelude::with_prelude,
    curios_text::{SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    curios_utilities::{Candidates, Digest, Facet, Qualifier},
};

/// `source` compiled as the unit `/lib` over `baseline`, against the prelude, taking what it can from `store`.
fn recompile_from(source: &str, baseline: &Unit, store: &Store) -> Unit {
    let modules = mounted("lib", source);

    with_prelude(|prelude| {
        compile_unit_over(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::mounted(&modules),
            baseline,
            &vec![false; prelude.len()],
            Some(store),
        )
    })
    .map_err(String::from)
    .expect("the unit compiles over its baseline")
}

/// The name `/lib/<name>`.
fn named(name: &str) -> Global {
    Global::Authored(Qualifier::from(["lib", name]))
}

/// The identity of the elaborated item `unit` declares `name` by.
fn identity(unit: &Unit, name: &str) -> Digest {
    let place = unit
        .places()
        .place(&named(name))
        .expect("the unit declares the name");

    ItemRun::identity(&unit.core().items[place.item()]).expect("an item archives")
}

/// The digest of the erased form `unit` holds for the item declaring `name`.
fn form(unit: &Unit, name: &str) -> Digest {
    let place = unit
        .places()
        .place(&named(name))
        .expect("the unit declares the name")
        .item();
    let at = unit
        .core()
        .items
        .places()
        .position(|held| held == place)
        .expect("the unit holds the item");

    unit.erased()
        .items_objects()
        .expect("an erased form archives")[at]
        .digest
}

/// What the declaration `unit` lowered as `name` was made from, as a candidate is found by.
fn input(unit: &Unit, name: &str) -> Digest {
    let (lowered, declarations) = (unit.text().core(), unit.text().declarations());
    let declaration = declarations
        .declaration(&named(name))
        .expect("the unit declares the name");

    declaration_input(
        declarations
            .items(declaration)
            .iter()
            .map(|item| lowered_digest(lowered, *item).expect("a lowered item archives")),
    )
}

const FIRST: &str = "use /std/{Nat};\n\npub let base: Nat = 1;\n\npub let next: Nat = base;\n";

const EDITED: &str = "use /std/{Nat};\n\npub let base: Nat = 2;\n\npub let next: Nat = base;\n";

/// An edit undone elaborates, erases and judges nothing: the declaration it made again is lowered as it was first and every part and binding read stands, so its item is read out of the store, then its erased form and the one of the declaration that read its value, and its verdict is a record the store holds, complete before the unit is filed; the unit comes out as a whole compile of the first text.
#[test]
fn an_edit_undone_takes_what_it_made_from_the_store() {
    let first = unit_of(FIRST);
    let mut store = Store::default();
    store.file(&first);
    let edited = recompile_over(EDITED, &first).expect("the edit compiles");
    store.file(&edited);

    let undone = recompile_from(FIRST, &edited, &store);

    assert!(
        undone
            .certification()
            .records()
            .all(|(_, record)| record.completed()),
        "every verdict is one a filing completed",
    );
    assert_stored_alike(&first, &undone);
    assert_eq!(
        *store.read.borrow(),
        vec![
            identity(&first, "base"),
            form(&first, "base"),
            form(&first, "next")
        ]
    );
}

const ALIASED: &str =
    "use /std/{Int, Nat};\n\npub let Number: Type = Nat;\n\npub let one: Number = 1;\n";

const RESPELLED: &str =
    "use /std/{Int, Nat};\n\npub let Number: Type = Nat;\n\npub let one: Number = 0 + 1;\n";

const RETYPED: &str =
    "use /std/{Int, Nat};\n\npub let Number: Type = Int;\n\npub let one: Number = 1;\n";

/// A candidate whose reads moved is passed over: the declaration is lowered as it was first, so the store holds a candidate of it, but what it read of the alias is another body now, so it is elaborated, as a whole compile elaborates it.
#[test]
fn a_candidate_that_read_a_part_since_moved_is_passed_over() {
    let first = unit_of(ALIASED);
    let mut store = Store::default();
    store.file(&first);
    let respelled = recompile_over(RESPELLED, &first).expect("the respelling compiles");
    store.file(&respelled);

    let retyped = recompile_from(RETYPED, &respelled, &store);

    assert_eq!(
        store
            .candidates(Facet::Elaboration, &input(&retyped, "one"))
            .len(),
        1,
        "the store holds what the first text's declaration was made into"
    );
    assert_stored_alike(&unit_of(RETYPED), &retyped);
    assert!(store.read.borrow().is_empty());
}
