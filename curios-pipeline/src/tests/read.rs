//! A unit read from Core text: compiled where a program's unit is lowered and elaborated, judged and erased as any unit is, and holding no lowering.

use {
    super::test_support::*,
    crate::{After, DEFAULT_BUDGET, Earlier, recheck, test_support::*},
    curios_abi::ForeignStore,
    curios_cert::{Error, Verdict},
    curios_common::{Mount, Qualifier, RootKind},
    curios_core::{Definitions, Entrypoint, Global, Program, Term, Universes, Zonked},
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_surface::{CoreFile, FormScope, to_core},
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

/// `/lib/T` is `Bool`, written with a decorator stating that its value is a proposition, and `/lib/H` a predicate over it.
const FORGED: &str = "#{value_sort = Prop}\nlet /lib/T : Type = #BoolType;\nlet /lib/H : (x: /lib/T) -> Prop = (x: /lib/T) => /sys/Bool/Holds(x);\n";

/// A term at `/lib/H(false)` built from one at `/lib/H(true)`: it stands at `Holds(false)`, which is `False`, wherever `true` and `false` are one at `/lib/T`, as irrelevance makes them where `/lib/T` is a proposition by its publication.
const CROSSED: &str =
    "((h: /lib/H(false)) => h)(((k: /lib/H(true)) => k)(#{node = Variant} /sys/Bool/True/qed()))\n";

/// What the kernel says of the term `text` closes with, read as Core text over the prelude and `units` and put at `False` as a module built by hand is.
fn closing_over(units: &[&Unit], text: &str) -> Vec<Verdict> {
    with_prelude(|prelude| {
        let scope = prelude
            .iter()
            .copied()
            .chain(units.iter().copied())
            .collect::<Vec<_>>();
        let predecessors = Predecessors::over(&scope);
        let file = text.parse::<CoreFile>().expect("the text parses");
        let (definitions, entry) =
            to_core(&file, Some(&FormScope::over(SYNTAX, predecessors.cores())))
                .expect("the text reads");
        let program = Program {
            definitions,
            entrypoint: Entrypoint {
                body: entry.expect("the text closes with its term").body,
                type_: Some(Term::instance(
                    Global::Authored(SYNTAX.proof.false_type.qualifier()),
                    Universes::default(),
                )),
            },
        };
        let program = Zonked::project(&program).unwrap_or_else(|refusal| panic!("{refusal}"));

        recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
    })
}

// A unit stores the module its kernel's walk handed back, so a unit after it reads `/lib/T` by the sort the walk stated its value inhabits, `Type`, and not by the decorator the unit's text arrived with: the two calls of `/lib/H` stay apart.
//
// Mutation-check: have `compile_read` store the module as it arrived rather than `rechecked.definitions`, and the closing term is admitted at `False`.
#[test]
fn a_value_sort_a_unit_arrives_with_is_not_what_a_unit_after_it_reads() {
    let unit = compiled(FORGED, None).expect("the unit is made");

    let verdicts = closing_over(&[&restored(&unit)], CROSSED);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

// `/lib/T`'s terms are its baseline's, so the walk over the baseline does not judge it again and hands it back with what the baseline's walk stated of it, `Type`, whatever its decorator says now; a unit after it reads that, and the two calls of `/lib/H` stay apart.
//
// Mutation-check: have `kernel::stating::carried_marks` keep the item's own `value_sort` in place of the baseline's, and the closing term is admitted at `False`.
#[test]
fn a_value_sort_an_item_its_baseline_holds_arrives_with_is_the_baselines() {
    let first = compiled("let /lib/T : Type = #BoolType;\n", None).expect("the unit is made");
    let earlier = Earlier {
        unit: restored(&first),
        after: After::Scope,
        candidates: None,
    };
    let again = compiled(FORGED, Some(&earlier)).expect("the unit is made over its baseline");

    let verdicts = closing_over(&[&restored(&again)], CROSSED);

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}
