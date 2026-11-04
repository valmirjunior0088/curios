//! A unit read from Core text: compiled where a program's unit is lowered and elaborated, judged and erased as any unit is, and holding no lowering.

use {
    super::test_support::*,
    crate::{After, DEFAULT_BUDGET, Earlier, test_support::*},
    curios_abi::ForeignStore,
    curios_common::{Mount, Qualifier, RootKind},
    curios_core::Definitions,
    curios_prelude::with_prelude,
    curios_surface::{CoreFile, to_core},
    curios_unit::{Predecessors, Unit},
};

/// The module the text `text` reads to over the prelude.
fn read(text: &str) -> Definitions {
    let file = text.parse::<CoreFile>().expect("the text parses");
    let (definitions, entry) = to_core(&file, Some(&prelude_form_scope())).expect("the text reads");
    assert!(entry.is_none(), "a unit's text closes with no term");

    definitions
}

/// The unit `text` reads to, claiming `/lib` over the prelude, over `baseline` where one is handed.
fn compiled(text: &str, baseline: Option<&Earlier>) -> Result<Unit, Unmade> {
    with_prelude(|prelude| {
        compile_read(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            vec![Mount::new(Qualifier::from(["lib"]), RootKind::Ordinary)],
            (read(text), ForeignStore::default()),
            baseline,
        )
    })
}

#[test]
fn a_unit_read_from_core_text_holds_no_lowering_and_restores_as_it_was_stored() {
    let unit = compiled("let /lib/one : #NatType = 1;\n", None).expect("the unit is made");
    let mounts = |unit: &Unit| {
        unit.mounts()
            .iter()
            .map(|mount| mount.prefix)
            .collect::<Vec<_>>()
    };
    let lib = [Qualifier::from(["lib"])];

    assert!(unit.text().is_none());
    assert_eq!(mounts(&unit), lib);
    assert_eq!(unit.core().items.len(), 1);

    let restored = restored(&unit);
    assert!(restored.text().is_none());
    assert_eq!(mounts(&restored), lib);
    assert_eq!(
        curios_archive::to_bytes(restored.core()).expect("a module serializes")[..],
        curios_archive::to_bytes(unit.core()).expect("a module serializes")[..]
    );
}

#[test]
fn a_unit_read_from_core_text_is_refused_by_the_kernels_own_verdict() {
    let Err(refused) = compiled("let /lib/bad : #NatType = #BoolType;\n", None) else {
        panic!("the kernel refuses a type standing where a number does");
    };

    assert!(
        matches!(&refused, Unmade::Kernel(verdict) if matches!(verdict.error, curios_cert::Error::Mismatch { .. })),
        "{refused:?}"
    );
}

/// The kernel decides over a baseline what it adopts, so a text read again over its earlier reading is the unit a whole compile of it is.
#[test]
fn a_unit_read_over_its_earlier_reading_is_the_unit_read_whole() {
    let text = "let /lib/one : #NatType = 1;\nlet /lib/two : #NatType = 2;\n";
    let first = compiled(text, None).expect("the unit is made");
    let earlier = Earlier {
        unit: first.clone(),
        after: After::Scope,
        candidates: None,
    };
    let again = compiled(text, Some(&earlier)).expect("the unit is made over its baseline");

    assert_eq!(
        curios_archive::to_bytes(again.core()).expect("a module serializes")[..],
        curios_archive::to_bytes(first.core()).expect("a module serializes")[..]
    );
}
