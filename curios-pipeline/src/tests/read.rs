//! A text of Core text read through the product's door, and a unit read from Core text: compiled where a program's unit is lowered and elaborated, judged and erased as any unit is, holding no lowering, and placed where its text writes each declaration.

use {
    super::test_support::*,
    crate::{
        After, CompileError, Earlier, Read, Unmade, Unread, compile_read, read_core_text,
        test_support::*,
    },
    curios_cert::{Error, Verdict},
    curios_common::{About, Mount, Qualifier, RootKind, Source, Style},
    curios_core::{Anchors, Entrypoint, Global, Intrinsic, Program, Term, Universes},
    curios_elab::DEFAULT_BUDGET,
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_surface::{CoreFile, FormScope, Refusal, to_core},
    curios_unit::{Predecessors, Unit},
};

/// The type a program's closing term is put at: `Io({})`.
fn contract() -> Term {
    Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()))
}

/// What the text `text` reads to over the prelude, a closing term put at the program contract.
fn reading(text: &str) -> Result<Read, Unread> {
    read_core_text(&Source::inline(text), &prelude_form_scope(), &contract())
}

/// What the text `text` reads to over the prelude, closing with no term.
fn read(text: &str) -> Read {
    let read = reading(text).expect("the text reads");
    assert!(read.entry.is_none(), "a unit's text closes with no term");

    read
}

/// The unit `read` is, claiming `/lib` over the prelude, over `baseline` where one is handed.
fn compiled_read(read: &Read, baseline: Option<&Earlier>) -> Result<Unit, Unmade> {
    with_prelude(|prelude| {
        compile_read(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            vec![Mount::new(Qualifier::from(["lib"]), RootKind::Ordinary)],
            read,
            baseline,
        )
    })
}

/// The unit `text` reads to, claiming `/lib` over the prelude, over `baseline` where one is handed.
fn compiled(text: &str, baseline: Option<&Earlier>) -> Result<Unit, Unmade> {
    compiled_read(&read(text), baseline)
}

/// The line `name` is written on, as the anchors a text was read with place it, counted from one.
fn line_of(anchors: &Anchors, name: &str) -> Option<usize> {
    let name = Global::Authored(Qualifier::from(name.split('/').collect::<Vec<_>>()));
    let span = Anchors::label(&[anchors], &name)?;

    Some(span.source.text()[..span.start].matches('\n').count() + 1)
}

/// Each declaration a text writes is anchored where its name is written, and the term it closes with where that term is: what a report about the module is placed by.
#[test]
fn a_text_is_anchored_where_it_writes_each_declaration() {
    let read = reading("let /lib/one : #NatType = 1;\n\nlet /lib/two : #NatType = 2;\n/lib/two\n")
        .expect("the text reads");

    assert_eq!(line_of(&read.anchors, "lib/one"), Some(1));
    assert_eq!(line_of(&read.anchors, "lib/two"), Some(3));
    let entry = read
        .anchors
        .texts_of(curios_core::Anchor::of(curios_core::Anchored::Entry));
    assert!(
        matches!(&entry[..], [(_, span)] if span.source.text()[span.start..span.end] == *"/lib/two"),
        "{entry:?}"
    );
}

/// A closing term is put at the type its question puts it at: a text stating none and one stating that type are read alike, and one stating another is refused where it states it.
#[test]
fn a_closing_term_is_put_at_its_questions_type_and_no_other() {
    for text in ["0\n", "0\n: #IoType({})\n"] {
        let read = reading(text).expect("the text reads");
        assert_eq!(
            read.entry.and_then(|entry| entry.type_),
            Some(contract()),
            "{text}"
        );
    }

    let Err(Unread::Read(error)) = reading("0\n: #NatType\n") else {
        panic!("a type other than the program contract is refused");
    };
    assert!(
        matches!(error.refusal, Refusal::PutElsewhere(_)),
        "{error:?}"
    );
    let span = error
        .span
        .expect("the refusal stands where the type is written");
    assert_eq!(&span.source.text()[span.start..span.end], "#NatType");
}

/// The rows a text's foreign forms declare are what the unit read from it declares, as a lowered unit declares the rows its `foreign` declarations state.
#[test]
fn a_texts_foreign_rows_are_what_its_unit_declares() {
    let read = read("foreign /lib/host_echo(Bytes) -> Bytes;\n");

    assert_eq!(read.foreigns.iter().count(), 1);
}

/// A declaration the certifier refuses is reported where the text writes its name, as a lowered unit's refusal is placed where its lowering wrote it.
#[test]
fn a_refusal_of_a_unit_read_from_core_text_stands_at_the_declaration_it_refuses() {
    let read = read("let /lib/one : #NatType = 1;\nlet /lib/bad : #NatType = #BoolType;\n");
    let Err(refused) = compiled_read(&read, None) else {
        panic!("the certifier refuses a type standing where a number does");
    };
    let reported =
        with_prelude(|prelude| refused.reported(&read, Predecessors::over(prelude), &SYNTAX));

    let CompileError::Failure(reports) = &reported else {
        panic!("a refusal is a failure: {reported:?}");
    };
    let Some(About::Place(span)) = &reports[0].about else {
        panic!("the refusal is placed: {reports:?}");
    };
    let place = span.place(&Style::plain());
    assert!(place.ends_with("2:5"), "{place}");
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
fn a_unit_read_from_core_text_is_refused_by_the_certifiers_own_verdict() {
    let Err(refused) = compiled("let /lib/bad : #NatType = #BoolType;\n", None) else {
        panic!("the certifier refuses a type standing where a number does");
    };

    assert!(
        matches!(&refused, Unmade::Certifier(verdict) if matches!(verdict.error, curios_cert::Error::Mismatch { .. })),
        "{refused:?}"
    );
}

/// The certifier decides over a baseline what it adopts, so a text read again over its earlier reading is the unit a whole compile of it is.
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

/// What the certifier says of the term `text` closes with, read as Core text over the prelude and `units` and put at `False` as a module built by hand is.
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
        recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
    })
}

// A unit stores the module its certifier's walk handed back, so a unit after it reads `/lib/T` by the sort the walk stated its value inhabits, `Type`, and not by the decorator the unit's text arrived with: the two calls of `/lib/H` stay apart.
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
// Mutation-check: have `certifier::stating::carried_marks` keep the item's own `value_sort` in place of the baseline's, and the closing term is admitted at `False`.
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
