//! The certifier's record a compiled unit files beside its definitions: what it covers, what it says, and what an item-level recompile keeps of the baseline's.

use {
    super::test_support::{recompile_over, reuses_body, unit_of},
    curios_core::Totality,
    curios_prelude::with_prelude,
    curios_unit::Unit,
};

/// A total function, one that recurses without descending — a single-member `rec` group, since its body names itself — and a value reaching the first.
const SOURCE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let spin(n: Nat) -> Nat = spin(n);

pub let twice: Nat = double(2);
";

/// The certifier's classification of the definition `unit` calls `symbol`.
fn classified(unit: &Unit, symbol: &str) -> Option<Totality> {
    let certification = unit.certification()?;
    unit.core()
        .items
        .iter()
        .flat_map(|item| item.definitions())
        .find(|definition| definition.name.symbol().ends_with(&format!("/{symbol}")))
        .and_then(|definition| certification.totality(&definition.name))
}

/// A unit the pipeline compiled carries the kernel's record of every definition it holds, each classified as the kernel's own closure found it.
#[test]
fn a_compiled_unit_files_the_certifiers_record_of_every_definition() {
    let unit = unit_of(SOURCE);

    let certification = unit
        .certification()
        .expect("a compiled unit carries a record");
    assert!(certification.covers(unit.core()));
    assert_eq!(classified(&unit, "double"), Some(Totality::Total));
    assert_eq!(classified(&unit, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&unit, "twice"), Some(Totality::Total));
}

/// An item-level recompile walks only the closure the edit reached, so its record joins the baseline's classifications for what it reused to its own walk's — covering the unit whole, with a reused item classified as the walk that filed the baseline classified it.
#[test]
fn an_item_level_recompile_keeps_the_baselines_record_for_what_it_reuses() {
    let baseline = unit_of(SOURCE);
    let edited = SOURCE.replace("double(2)", "double(3)");

    let recompiled = recompile_over(&edited, &baseline).expect("the edit compiles");

    assert!(reuses_body(&baseline, &recompiled, "double"));
    assert!(!reuses_body(&baseline, &recompiled, "twice"));
    let certification = recompiled
        .certification()
        .expect("a recompiled unit carries a record");
    assert!(certification.covers(recompiled.core()));
    assert_eq!(classified(&recompiled, "double"), Some(Totality::Total));
    assert_eq!(classified(&recompiled, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&recompiled, "twice"), Some(Totality::Total));
}

/// The fixed prelude is lent with the record its build's certification filed, so a walk with the prelude in scope reads the certifier's verdict on every `/sys` and `/std` definition and classifies none of them for itself.
#[test]
fn the_restored_prelude_carries_a_record_covering_every_definition() {
    with_prelude(|prelude| {
        for unit in prelude {
            let certification = unit
                .certification()
                .expect("the restored prelude carries its record");

            assert!(certification.covers(unit.core()));
        }
    });
}
