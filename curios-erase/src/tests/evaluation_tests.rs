//! What erasure computes and keeps: a bound value where it is written and no bound type, a top-level value where its unit is erased, the totality a record marks, and a unit erased over a baseline.

use {
    crate::test_support::{
        bound, definition, erase, erase_alone, fresh, global, module, nat_lit, nominal, stating,
        try_erase, zonked,
    },
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module, certify_program},
    curios_elab::DEFAULT_BUDGET,
    std::collections::BTreeSet,
};

/// What certifying `unit` with nothing in scope records: what a part of the unit is digested under, by an erasure over it and by one of it.
fn certified(unit: &curios_core::Zonked<curios_core::Module>) -> curios_core::Certification {
    let globals = Globals::default();

    certify_module(unit, DEFAULT_BUDGET, &globals, SYNTAX)
        .0
        .certification
}

/// [`erase_alone`] over `baseline`.
fn erase_over(
    unit: &curios_core::Zonked<curios_core::Module>,
    baseline: ErasedBaseline<'_>,
) -> Result<ErasedUnit, Error> {
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_unit_over(&mut kernel, Erasures::nothing(), unit, baseline)
}

/// Where [`a_bound_call`] binds its call: a local `let` opening the entry, or a top-level item.
#[derive(Clone, Copy)]
enum Binder {
    Let,
    Item,
}

/// `callee(4)` bound and never used, before an entry that answers `7`: `callee` is a function of one `Nat`, returning a type where `returns_a_type` and a `Nat` otherwise, so the two readings differ only in whether the binding's type is a sort.
fn a_bound_call(binder: Binder, returns_a_type: bool) -> curios_anf::Module {
    let n = fresh(Some("n"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let (result_type, result) = match returns_a_type {
        true => (curios_core::Term::type_ground(), nat.clone()),
        false => (
            nat.clone(),
            curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                curios_core::Term::free_var(&n),
                nat_lit(1),
            )),
        ),
    };
    let mut items = vec![definition(
        "callee",
        curios_core::Term::func_type([(n, nat.clone())], result_type.clone()),
        curios_core::Term::func([(n, nat.clone())], result),
    )];
    let call =
        curios_core::Term::apply(curios_core::Term::free_var(&global("callee")), [nat_lit(4)]);
    let body = match binder {
        Binder::Let => {
            let bound = fresh(Some("bound"));
            curios_core::Term::let_(&bound, result_type, call, nat_lit(7))
        }
        Binder::Item => {
            items.push(definition("bound", result_type, call));
            nat_lit(7)
        }
    };
    erase(&module(items, body), nat)
}

fn calls(erased: &curios_anf::Module) -> usize {
    bound(erased)
        .filter(|rhs| matches!(rhs, curios_anf::Rhs::Apply { .. }))
        .count()
}

/// `identity : (Nat) -> Nat`, stamped `stamp` by elaboration, and an entry applying it — erased through the path production takes, as the program it links to.
fn identity_arena(stamp: curios_core::Totality) -> ErasedProgram {
    let x = fresh(Some("x"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let mut items = vec![definition(
        "identity",
        curios_core::Term::func_type([(x, nat.clone())], nat.clone()),
        curios_core::Term::func([(x, nat.clone())], curios_core::Term::free_var(&x)),
    )];
    let curios_core::Item::Let(declared) = &mut items[0] else {
        unreachable!("the fixture declares a definition");
    };
    declared.totality = stamp;
    let body = curios_core::Term::apply(
        curios_core::Term::free_var(&global("identity")),
        [nat_lit(4)],
    );

    let program = zonked(&stating(&module(items, body), nat));
    let globals = Globals::default();
    let (_, mut kernel) = certify_program(&program, DEFAULT_BUDGET, &globals, SYNTAX);
    erase_program(&mut kernel, Erasures::nothing(), &program).expect("the program erases")
}

/// Every erased function's termination flag.
fn totals(erased: &curios_anf::Module) -> Vec<bool> {
    erased
        .functions()
        .iter()
        .flatten()
        .map(|function| function.total)
        .collect()
}

/// Erasure marks nothing total, whatever elaboration stamped: the flag is the certifier's record's to set, where the sealed program meets it, and a stamp is read by nothing below Core.
#[test]
fn erasure_marks_no_function_total_whatever_its_stamp() {
    for stamp in [curios_core::Totality::Total, curios_core::Totality::Partial] {
        let erased = identity_arena(stamp).into_module();

        assert_eq!(totals(&erased), [false], "stamped {stamp:?}");
    }
}

/// A record marks exactly the functions its `Total` definitions erased to — found through the binding erasure made — and nothing it classifies otherwise or does not name, whatever the stamp says: a `Partial` stamp does not stop a `Total` record, and a `Total` stamp does not stand in for one.
#[test]
fn a_record_marks_the_functions_its_total_definitions_erased_to() {
    let identity = nominal("identity");
    let cases = [
        (
            curios_core::Totality::Partial,
            curios_core::Certification::of(
                [(
                    identity,
                    curios_core::Certified {
                        totality: curios_core::Totality::Total,
                        ..curios_core::Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            true,
        ),
        (
            curios_core::Totality::Total,
            curios_core::Certification::of(
                [(
                    identity,
                    curios_core::Certified {
                        totality: curios_core::Totality::Partial,
                        ..curios_core::Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            false,
        ),
        (
            curios_core::Totality::Total,
            curios_core::Certification::default(),
            false,
        ),
    ];

    for (stamp, record, marked) in cases {
        let mut arena = identity_arena(stamp);
        arena.mark_total(&record);

        assert_eq!(
            totals(&arena.into_module()),
            [marked],
            "stamped {stamp:?}, recorded {record:?}"
        );
    }
}

#[test]
fn a_bound_type_is_not_computed() {
    // A binding is a kept slot and a type fills it with a stand-in, so the call that would have computed the type is never emitted — the direct call `erase_apply` keeps at an erasable type is not reached at all.
    for binder in [Binder::Let, Binder::Item] {
        let erased = a_bound_call(binder, true);
        assert_eq!(
            calls(&erased),
            0,
            "{}",
            curios_anf::test_support::shape(&erased)
        );
    }
}

#[test]
fn a_bound_value_is_computed_where_it_is_written() {
    // The control: the same binding at a `Nat` is evaluated under call-by-value, used or not. A local one is the call it was written as; a top-level one is what the call computes, computed where its item is erased.
    let erased = a_bound_call(Binder::Let, false);
    assert_eq!(
        calls(&erased),
        1,
        "{}",
        curios_anf::test_support::shape(&erased)
    );

    let erased = a_bound_call(Binder::Item, false);
    assert_eq!(
        calls(&erased),
        0,
        "{}",
        curios_anf::test_support::shape(&erased)
    );
    assert!(
        bound(&erased).any(|rhs| matches!(rhs, curios_anf::Rhs::Alias(_))),
        "{}",
        curios_anf::test_support::shape(&erased)
    );
}

/// A unit of `step`, a function adding `by` to its argument, and `stepped`, which is `step(4)`, with the unit erased whole.
fn stepping(by: usize) -> (curios_core::Zonked<curios_core::Module>, ErasedUnit) {
    let n = fresh(Some("n"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition(
                    "step",
                    curios_core::Term::func_type([(n, nat.clone())], nat.clone()),
                    curios_core::Term::func(
                        [(n, nat.clone())],
                        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                            curios_core::Term::free_var(&n),
                            nat_lit(by),
                        )),
                    ),
                ),
                definition(
                    "stepped",
                    nat,
                    curios_core::Term::apply(
                        curios_core::Term::free_var(&global("step")),
                        [nat_lit(4)],
                    ),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_alone(&unit).expect("the unit erases whole");

    (unit, erased)
}

/// A top-level value is computed where its unit is erased: `stepped` is `step(4)`, and its erased form is the `5` that comes to, naming nothing of `step`. So the unit holds no item that computes when a program starts, and a program links `stepped` where it names it and `step` where it applies it.
#[test]
fn a_top_level_value_is_computed_where_its_unit_is_erased() {
    let (_, unit) = stepping(1);
    let stepped = unit
        .find(&nominal("stepped"), Sorts::NONE)
        .expect("the unit declares it");

    assert_eq!(unit.computing().count(), 0);
    assert!(stepped.functions().is_empty() && stepped.values().is_empty());
    assert_eq!(
        curios_anf::test_support::shape(stepped.module()),
        "\
items
  Let ~v0$/stepped = Alias Nat(5)
no entry
"
    );
}

/// A value computed where it is erased ran the erased form of what it applied, whole, so it is computed again where that form is another: `step` is elaborated again to add `2`, under the signature and the interface `stepped` read it by, and `stepped`, whose declaration is the baseline's, holds the `6` a whole erasure computes.
///
/// Mutation-checked against the question about a form whole: with every form an evaluation ran taken to stand, `stepped` keeps the baseline's `5`.
#[test]
fn a_value_is_computed_again_where_a_form_its_evaluation_ran_is_another() {
    let (before, baseline) = stepping(1);
    let (edited, whole) = stepping(2);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            certification: &certified(&before),
            erased: &baseline,
            made: &BTreeSet::from([nominal("step")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            recorded: &certified(&edited),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_ne!(form(&whole, "stepped"), form(&baseline, "stepped"));
    assert_eq!(form(&over, "stepped"), form(&whole, "stepped"));
}

/// A value computed where it is erased is kept where every form its evaluation ran is the one it ran: `step` is elaborated again to what it was, so its erased form is stored as the bytes it was, and `stepped` holds the baseline's own erased form.
#[test]
fn a_computed_value_is_kept_where_what_it_ran_was_erased_again_to_what_it_was() {
    let (before, baseline) = stepping(1);
    let (edited, _) = stepping(1);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            certification: &certified(&before),
            erased: &baseline,
            made: &BTreeSet::from([nominal("step")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            recorded: &certified(&edited),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(
            unit.find(&nominal("stepped"), Sorts::NONE)
                .expect("the unit declares it"),
        )
    };

    assert_eq!(held(&over), held(&baseline), "`stepped` was erased again");
}

/// A unit of two definitions, `callee` and `caller`, which is `callee` by name, and the same unit erased whole.
fn calling(callee: usize) -> (curios_core::Zonked<curios_core::Module>, ErasedUnit) {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition("callee", nat.clone(), nat_lit(callee)),
                definition(
                    "caller",
                    nat,
                    curios_core::Term::free_var(&global("callee")),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_alone(&unit).expect("the unit erases whole");

    (unit, erased)
}

/// How a unit holds the item declaring `name`, as its printed erased form.
fn form(unit: &ErasedUnit, name: &str) -> String {
    format!(
        "{:?}",
        unit.find(&nominal(name), Sorts::NONE)
            .expect("the unit declares it")
    )
}

/// A unit erased over a baseline erases the items the elaborator made again and keeps the others. `kept` is given another body since the baseline was erased and is not named among the made, so what the unit holds for it is still what the baseline erased it to: an item erased again would show the new body.
#[test]
fn a_unit_erased_over_a_baseline_erases_what_was_elaborated_again_and_no_other() {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = |kept: usize, redone: usize| {
        zonked(
            &module(
                vec![
                    definition("kept", nat.clone(), nat_lit(kept)),
                    definition("redone", nat.clone(), nat_lit(redone)),
                ],
                nat_lit(0),
            )
            .module,
        )
    };

    let before = unit(1, 2);
    let baseline = erase_alone(&before).expect("the baseline erases");
    let edited = unit(3, 4);
    let whole = erase_alone(&edited).expect("the unit erases whole");
    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            certification: &certified(&before),
            erased: &baseline,
            made: &BTreeSet::from([nominal("redone")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            recorded: &certified(&edited),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_eq!(form(&over, "kept"), form(&baseline, "kept"));
    assert_ne!(form(&over, "kept"), form(&whole, "kept"));
    assert_eq!(form(&over, "redone"), form(&whole, "redone"));
}

/// An erased form the elaborator kept is erased again where a name it uses is bound another way: `caller` reads `callee` as the constant it is bound to, so with `callee` elaborated again to another constant, `caller`'s erased form is the one a whole erasure makes, though its declaration is the baseline's.
///
/// Mutation-checked against the question about a binding: with no erased form's reads noted, `caller` keeps the baseline's, which holds the old constant.
#[test]
fn an_erased_form_is_erased_again_where_a_name_it_uses_is_bound_another_way() {
    let (before, baseline) = calling(1);
    let (edited, whole) = calling(2);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            certification: &certified(&before),
            erased: &baseline,
            made: &BTreeSet::from([nominal("callee")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            recorded: &certified(&edited),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_ne!(form(&whole, "caller"), form(&baseline, "caller"));
    assert_eq!(form(&over, "caller"), form(&whole, "caller"));
}

/// An erased form the elaborator kept is kept where everything its erasure read digests as it did, whatever was elaborated again beside it: `callee` is elaborated again to the constant it was, so `caller`, which read that binding, holds the baseline's own erased form.
#[test]
fn an_erased_form_is_kept_where_what_it_read_came_out_as_it_was() {
    let (before, baseline) = calling(1);
    let (edited, _) = calling(1);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            certification: &certified(&before),
            erased: &baseline,
            made: &BTreeSet::from([nominal("callee")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            recorded: &certified(&edited),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(
            unit.find(&nominal("caller"), Sorts::NONE)
                .expect("the unit declares it"),
        )
    };

    assert_eq!(held(&over), held(&baseline), "`caller` was erased again");
}

#[test]
fn a_computed_only_evaluation_cycle_is_rejected_as_an_error() {
    let a = fresh(Some("a"));
    let b = fresh(Some("b"));
    // rec a = b; b = a — a mutual value-level cycle no forcing order satisfies. The verifier rejects it; erasure surfaces the diagnostic as an error, never a panic. (A *self*-knot `rec loop = loop` is admitted only while nothing outside its initializer reads it, which is when the lowering drops it.)
    let type_ = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let body = curios_core::Term::rec(
        vec![
            (a, type_.clone(), curios_core::Term::free_var(&b)),
            (b, type_.clone(), curios_core::Term::free_var(&a)),
        ],
        curios_core::Term::free_var(&a),
    );
    let error =
        try_erase(&module(Vec::new(), body), type_).expect_err("the value-level cycle is rejected");
    assert!(matches!(error, Error::EvaluationCycle { .. }), "{error:?}");
}
