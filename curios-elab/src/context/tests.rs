use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        CalleeId, Free, Global, Intrinsic, Level, MetavarId, Nat, Term, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseMetaId, UniverseRole, WitnessOrigin,
    },
    curios_utilities::Qualifier,
    std::collections::BTreeSet,
};

fn context() -> Context {
    Context::with_default_budget(SYNTAX)
}

#[test]
fn universe_dependencies_of_a_solved_meta_follow_only_its_materialized_solution() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);
    let solution = UniverseMetaId(2);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetavarId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::meta(solution)));

    assert_eq!(context.universe_metas_in(&Term::hole(0)), [solution].into());
}

#[test]
fn universe_dependencies_of_an_unsolved_meta_keep_its_birth_context() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetavarId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );

    assert_eq!(
        context.universe_metas_in(&Term::hole(0)),
        [result, telescope].into()
    );
}

/// A witness goal deferred for want of a table entry, as resolution defers one.
fn deferred(context: &mut Context, slot: usize) -> ParkedProblem {
    ParkedProblem {
        work: ParkedWork::Witness {
            slot: MetavarId(slot),
            goal: Term::type_ground(),
            // The callee is irrelevant here -- these fixtures exercise parked-problem bookkeeping, not how a report names one.
            provenance: WitnessOrigin {
                func: CalleeId::Anonymous,
                binder: "w".to_string(),
            },
        },
        origin: Term::type_ground(),
        frame: context.freeze_frame(),
        watching: BTreeSet::new(),
    }
}

fn stamps(deferred: Vec<(ItemStamp, ParkedProblem)>) -> Vec<ItemStamp> {
    deferred.into_iter().map(|(item, _)| item).collect()
}

#[test]
fn a_deferred_witness_goal_keeps_the_stamp_of_the_item_that_raised_it() {
    let mut context = context();
    context.begin_item(ItemStamp(3));
    let parked = deferred(&mut context, 0);
    context.defer_witness(parked);
    context.begin_item(ItemStamp(4));

    assert_eq!(stamps(context.take_deferred_witnesses()), [ItemStamp(3)]);
}

#[test]
fn dropping_one_items_deferred_goals_leaves_the_others() {
    let mut context = context();
    context.begin_item(ItemStamp(1));
    let parked = deferred(&mut context, 0);
    context.defer_witness(parked);
    context.begin_item(ItemStamp(2));
    let parked = deferred(&mut context, 1);
    context.defer_witness(parked);

    context.drop_deferred_of(ItemStamp(1));

    assert_eq!(stamps(context.take_deferred_witnesses()), [ItemStamp(2)]);
}

#[test]
fn removing_a_witness_hands_back_every_key_it_held() {
    let mut context = context();
    let concept = Global::Authored(Qualifier::from(["Show"]));
    let name = Global::Authored(Qualifier::from(["show"]));
    let witness = || Witness {
        name,
        module: Qualifier::empty(),
        universe_context: UniverseContext::empty(),
        signature: Term::type_ground(),
    };
    let nat = WitnessKey(vec![HeadKey::Nat]);
    let bool_ = WitnessKey(vec![HeadKey::Bool]);
    assert!(
        context
            .insert_witness(concept, nat.clone(), witness())
            .is_none()
    );
    assert!(
        context
            .insert_witness(concept, bool_.clone(), witness())
            .is_none()
    );

    let removed = context
        .remove_witness(&name)
        .into_iter()
        .collect::<BTreeSet<_>>();

    assert_eq!(
        removed,
        BTreeSet::from([(concept, nat.clone()), (concept, bool_.clone())])
    );
    assert!(context.witness(&concept, &nat).is_none());
    assert!(context.witness(&concept, &bool_).is_none());
    assert!(context.remove_witness(&name).is_empty());
}

#[test]
fn a_forgotten_declaration_is_bound_nowhere() {
    let mut context = context();
    let name = Free::from(&Global::Authored(Qualifier::from(["gone"])));
    context.define_assuming(&name, &Term::type_ground(), &Term::type_ground(), None);
    context.assume_witness(&name, &Term::type_ground());

    context.forget(&name);

    assert!(context.assumption(&name).is_none());
    assert!(context.definition_body(&name).is_none());
    assert!(!context.locals().iter().any(|(bound, _)| *bound == name));
    assert!(
        !context
            .witness_scope()
            .iter()
            .any(|(bound, _)| *bound == name)
    );
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

/// An arm re-types `x` under its own name; a problem parked there is retried where `x` is live at its unspecialized type — the enclosing frame, where a drain or a turnaround after the arm runs it. The retry sees the arm's type, because the frame it runs in is the one the problem froze. Reapplying only names not already live would lose exactly this.
#[test]
fn a_retry_sees_a_local_at_the_type_its_problem_froze() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    context.with_frame(|context| {
        context.assume(&x, &nat());
        let frozen = context.with_frame(|context| {
            context.assume(&x, &bool_type());
            context.freeze_frame()
        });

        assert_eq!(context.assumption(&x), Some(&nat()));
        let seen = context.with_retry_frame(&frozen, |context| context.assumption(&x).cloned());
        assert_eq!(seen, Some(bool_type()));
        assert_eq!(context.assumption(&x), Some(&nat()));
    });
}

/// A refinement of the context a retry happens to run in is none of the problem's: the problem froze before the arm registering it was entered, so the retry decides it without the equation, and the arm has it again once the retry is over.
#[test]
fn a_retry_does_not_see_a_refinement_of_the_context_it_runs_in() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0_usize)));

    context.with_frame(|context| {
        context.assume(&x, &nat());
        let frozen = context.freeze_frame();

        context.with_frame(|context| {
            context.refine(&x, &zero);
            assert_eq!(context.var_reduct(&x), Some(&zero));

            let seen = context.with_retry_frame(&frozen, |context| context.var_reduct(&x).cloned());
            assert_eq!(seen, None);
            assert_eq!(context.var_reduct(&x), Some(&zero));
        });
    });
}

/// A metavariable born in a retry is born in the problem's context: its telescope holds the locals the problem froze, and none of the live context's, which its spine would otherwise carry into a term that never bound them.
#[test]
fn a_metavariable_born_in_a_retry_has_exactly_the_frozen_locals() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));

    let frozen = context.with_frame(|context| {
        context.assume(&a, &nat());
        context.freeze_frame()
    });

    context.with_frame(|context| {
        context.assume(&b, &nat());
        let names = context.with_retry_frame(&frozen, |context| {
            let (telescope, _) = context.identity_snapshot();
            telescope.iter().map(|(name, _)| *name).collect::<Vec<_>>()
        });
        assert_eq!(names, vec![a]);
    });
}

/// A retry resolves witnesses through the `use` binders its problem froze, never through the live context's.
#[test]
fn a_retry_resolves_through_the_frozen_witness_scope_alone() {
    let mut context = context();
    let live = context.fresh(Some("live"));

    let frozen = context.with_frame(|context| context.freeze_frame());

    context.with_frame(|context| {
        context.assume_witness(&live, &nat());
        let scope = context.with_retry_frame(&frozen, |context| context.witness_scope().to_vec());
        assert!(scope.is_empty(), "{scope:?}");
        assert_eq!(context.witness_scope().len(), 1);
    });
}

/// A retry inside a retry hides the outer retry's frame in turn, and gives the outer retry its own floor back when it ends: the live context the outer retry hid stays hidden until the outer retry is over.
#[test]
fn a_nested_retry_restores_the_floor_around_it() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let live = context.fresh(Some("live"));

    let outer = context.with_frame(|context| {
        context.assume(&a, &nat());
        context.freeze_frame()
    });
    let inner = context.with_frame(|context| {
        context.assume(&b, &nat());
        context.freeze_frame()
    });

    context.with_frame(|context| {
        context.assume(&live, &nat());
        context.with_retry_frame(&outer, |context| {
            let (sees_a, sees_b) = context.with_retry_frame(&inner, |context| {
                (
                    context.assumption(&a).is_some(),
                    context.assumption(&b).is_some(),
                )
            });
            assert!(!sees_a && sees_b);
            assert!(context.assumption(&a).is_some());
            assert!(
                context.assumption(&live).is_none(),
                "the outer retry's floor is back"
            );
        });
        assert!(context.assumption(&live).is_some());
    });
}

/// A metavariable born at an id the counter has not reached is never minted again: the next fresh id lies past it. A fixture states its metavariables that way, and a mint landing on one births a second metavariable over its record — which is how a candidate's re-validation, minting while it checks, would replace a test's solved hole with its own.
#[test]
fn a_metavariable_born_ahead_of_the_counter_is_never_minted_again() {
    let mut context = context();
    context.birth_metavar(MetavarId(3), Vec::new(), Term::type_ground());

    let minted = context.mint_metavar();

    assert!(minted.0 > 3, "minted ?{} at or below a born id", minted.0);
}

/// Inside an oracle bracket, a term whose elaboration writes is elaborated once and answered from the bracket's table after: the verdict is all an oracle hands back, and a second run would repeat the first one's writes and reach it again. Outside a bracket the cache's purity gate keeps it out, and each ask runs — the control.
#[test]
fn an_oracle_elaborates_a_term_that_writes_once() {
    let mut context = context();
    let term = Term::intrinsic(Intrinsic::nat_add(nat(), nat()));
    let elaborate_writing = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&term, None, |context| {
                *runs += 1;
                let hole = context.mint_metavar();
                context.birth_metavar(hole, Vec::new(), nat());
                Ok::<_, ()>((term.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };

    let mut outside = 0;
    elaborate_writing(&mut context, &mut outside);
    elaborate_writing(&mut context, &mut outside);
    let inside = context.with_oracle(&Refinements::default(), |context| {
        let mut inside = 0;
        elaborate_writing(context, &mut inside);
        elaborate_writing(context, &mut inside);
        inside
    });

    assert_eq!(outside, 2);
    assert_eq!(inside, 1);
}

/// What an oracle declined is that oracle's record. A site that wants to park inside one is refused and the oracle records it; an oracle inside it starts with nothing declined and leaves the enclosing record as it found it; and outside every oracle a site may park and nothing is recorded.
#[test]
fn a_declined_park_is_recorded_by_the_oracle_it_was_declined_in() {
    let mut context = context();
    assert!(context.may_park());
    assert!(!context.declined_to_park());

    let recorded = context.with_oracle(&Refinements::default(), |context| {
        let before = context.declined_to_park();
        let inner = context.with_oracle(&Refinements::default(), |context| {
            (context.may_park(), context.declined_to_park())
        });
        let after = context.declined_to_park();
        let may = context.may_park();

        (before, inner, after, may, context.declined_to_park())
    });

    assert_eq!(recorded, (false, (false, true), false, false, true));
    assert!(!context.declined_to_park());
    assert!(context.may_park());
}

/// A rollback invalidates what can rest on what it undid. One that unwound nothing keeps the cached reducts — a witness probe rolls back after every trial, and a clear at each would throw away the reducts the next node needs — while one that unwound a solution clears them, since a reduct cached since may have read it.
#[test]
fn a_rollback_keeps_the_reducts_unless_it_unwound_a_solution() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let sum = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    context.reduce(sum.clone(), &literal(3));

    let mark = context.solution_mark();
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), Some(literal(3)));

    let mark = context.solution_mark();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), literal(0));
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), None);
}

/// A reduct is remembered for the declaration that computed it and no longer. A hit on it is free, so an entry that outlived its declaration would let the declarations compiled first decide what a later one can afford.
///
/// Mutation-checked: keeping the reduction table across [`Context::restore_budget`] fails it.
#[test]
fn a_closed_reduct_does_not_outlive_its_declaration() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let sum = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    context.reduce(sum.clone(), &literal(3));
    assert_eq!(context.cached_reduced(&sum), Some(literal(3)));

    context.restore_budget();
    assert_eq!(context.cached_reduced(&sum), None);
}

/// What a declaration spends does not depend on what the declarations before it reduced. The first declaration here reduces a definition's body and then the definition's name, and the second reduces the name again: the second spends exactly what reducing it costs a context that reduced nothing before it — `curios-cert`'s test of the same name, put to the elaborator.
///
/// Mutation-checked: keeping the reduction table across [`Context::restore_budget`] fails it.
#[test]
fn what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it() {
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let name = Free::from(&Global::Authored(Qualifier::from(["chain"])));
    let body = (0..64).fold(literal(0), |sum, _| {
        Term::intrinsic(Intrinsic::nat_add(sum, literal(1)))
    });
    let occurrence = Term::free_var(&name);
    let defining = || {
        let mut context = context();
        context.define(&name, &body, None);
        context
    };
    let spent = |context: &mut Context, term: Term| {
        let before = context.consumed().units();
        reduce(context, term).expect("reduces");
        context.consumed().units() - before
    };

    let alone = spent(&mut defining(), occurrence.clone());

    let mut after = defining();
    spent(&mut after, body.clone());
    spent(&mut after, occurrence.clone());
    after.restore_budget();
    let again = spent(&mut after, occurrence);

    assert!(alone > 1, "reducing the name reduces its body");
    assert_eq!(
        again, alone,
        "the name costs what it costs a context that reduced nothing first"
    );
}

#[test]
fn a_proof_credits_the_written_binder_a_local_was_opened_from_and_a_rollback_withdraws_it() {
    let mut context = context();
    let declaration = Global::Authored(Qualifier::from(["f"]));
    context.enter_item(Some(declaration));
    let opened = context.fresh_for(Some("p"), Some(2));
    let unrelated = context.fresh(Some("q"));

    let mark = context.solution_mark();
    context.credit(&Term::free_var(&opened));
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert!(
        context.credited().is_empty(),
        "a proof written inside what is rolled back reads nothing that stands"
    );

    context.credit(&Term::free_var(&opened));
    context.credit(&Term::free_var(&unrelated));
    assert_eq!(context.credited(), BTreeSet::from([(Some(declaration), 2)]));
}

/// An equation whose scrutinee names no local once an arm refines a variable answers nothing in that arm, and answers again after it. The kernel substitutes an arm's solution through the equations in force and records none under a closed spelling, so an equation left answering here would accept in the arm a proof the kernel refuses.
///
/// The control is an equation that names a second variable the arm does not refine: its instance still names a local, the kernel still records it, and it answers at that instance, the recorded spelling stepping aside for the arm.
///
/// Mutation-checked: with `Context::refine` withholding no equation its refinement closes, the guard's instance at the arm's value is in force there, a closed term the kernel records nothing under.
#[test]
fn an_equation_a_refinement_closes_is_withheld_for_the_arm() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let literal = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let closing = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), literal(3)));
    let open = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), Term::free_var(&m)));

    context.with_frame(|context| {
        context.assume(&n, &nat());
        context.assume(&m, &nat());
        context.refine_scrutinee_spellings(
            vec![
                (closing.clone(), closing.clone(), false),
                (open.clone(), open.clone(), false),
            ],
            &truth,
        );
        assert_eq!(context.scrutinee_reduct(&closing, &closing), Some(&truth));

        context.with_frame(|context| {
            context.refine(&n, &literal(5));

            assert_eq!(
                context.scrutinee_reduct(&closing, &closing),
                None,
                "the guard is about a closed term in this arm"
            );
            let instance = Term::intrinsic(Intrinsic::nat_lt(literal(5), Term::free_var(&m)));
            assert_eq!(
                context.scrutinee_reduct(&instance, &instance),
                Some(&truth),
                "an equation that still names a local stands, at the instance the arm is checked at"
            );
            assert_eq!(
                context.scrutinee_reduct(&open, &open),
                None,
                "and its recorded spelling steps aside for the instance"
            );
            assert_eq!(
                context
                    .visible_scrutinee_entries()
                    .map(|(_, key, _)| key.clone())
                    .collect::<Vec<_>>(),
                vec![instance]
            );
        });

        assert_eq!(
            context.scrutinee_reduct(&open, &open),
            Some(&truth),
            "which answers again once the arm is left"
        );

        assert_eq!(
            context.scrutinee_reduct(&closing, &closing),
            Some(&truth),
            "and the guard answers again once the arm is left"
        );
    });
}

/// A withheld equation stays withheld where the refinements it was withheld under are installed again, as a metavariable's solution and a parked problem's retry install the ones they were born under: the entry that withholds it travels with them and shadows the equation as it did in the arm.
#[test]
fn a_withheld_equation_stays_withheld_where_its_refinements_are_installed_again() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let literal = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let guard = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), literal(3)));

    let (outside, inside) = context.with_frame(|context| {
        context.assume(&n, &nat());
        context.refine_scrutinee_spellings(vec![(guard.clone(), guard.clone(), false)], &truth);
        let outside = context.refinement_snapshot();

        let inside = context.with_frame(|context| {
            context.refine(&n, &literal(5));
            context.refinement_snapshot()
        });

        (outside, inside)
    });

    assert_eq!(
        context.with_refinements(&outside, |context| context
            .scrutinee_reduct(&guard, &guard)
            .cloned()),
        Some(truth),
        "the guard's own arm answers"
    );
    assert_eq!(
        context.with_refinements(&inside, |context| context
            .scrutinee_reduct(&guard, &guard)
            .cloned()),
        None,
        "the arm that closed it does not"
    );
}

/// An elaboration is remembered by the reduction it ran under, in the cache and in an oracle's table: a run under plain reduction, which asks the elaborator's conversion nothing, is not answered from what a judgment's run remembered, nor a judgment's from plain reduction's. A judgment's run may rest on a fold or an equation its conversion decided, which plain reduction refuses, and asking writes nothing, so the purity gate lets such a run in.
///
/// Mutation-checked for each table: with its key read as a judgment's whichever reduction asks, the plain run is answered by the judgment's.
#[test]
fn an_elaboration_is_remembered_by_the_reduction_it_ran_under() {
    let mut context = context();
    // Two terms, so the oracle's half is not answered from what the cache's half remembered.
    let kept = Term::intrinsic(Intrinsic::nat_add(nat(), nat()));
    let written = Term::intrinsic(Intrinsic::NatMul(nat(), nat()));
    let elaborate_pure = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&kept, None, |_| {
                *runs += 1;
                Ok::<_, ()>((kept.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };
    let elaborate_writing = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&written, None, |context| {
                *runs += 1;
                let hole = context.mint_metavar();
                context.birth_metavar(hole, Vec::new(), nat());
                Ok::<_, ()>((written.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };
    // A judgment's run, plain reduction's twice, and a judgment's again: one run each where each keeps its own.
    let across = |context: &mut Context, elaborate: &dyn Fn(&mut Context, &mut usize)| {
        let (mut judged, mut plain) = (0, 0);
        elaborate(context, &mut judged);
        context.plainly(|context| {
            elaborate(context, &mut plain);
            elaborate(context, &mut plain);
        });
        elaborate(context, &mut judged);
        (judged, plain)
    };

    assert_eq!(across(&mut context, &elaborate_pure), (1, 1));
    let inside = context.with_oracle(&Refinements::default(), |context| {
        across(context, &elaborate_writing)
    });
    assert_eq!(inside, (1, 1));
}

/// A sort is remembered by the reduction that read it, as an elaboration is: what plain reduction classified is not answered where a judgment asks, nor a judgment's where plain reduction does.
///
/// Mutation-checked both ways: with the table read as a judgment's whichever reduction asks, plain reduction finds nothing of what it filed, and with the answer filed there whichever reduction read it, a judgment is handed plain reduction's.
#[test]
fn a_sort_is_remembered_by_the_reduction_that_read_it() {
    let mut context = context();
    let read = Term::intrinsic(Intrinsic::BoolType);
    let judged = Term::prop();

    context
        .plainly(|context| Sort::of(context, &read))
        .expect("classifies");
    assert!(context.cached_sort(&read).is_none());
    assert!(
        context
            .plainly(|context| context.cached_sort(&read))
            .is_some()
    );

    Sort::of(&mut context, &judged).expect("classifies");
    assert!(
        context
            .plainly(|context| context.cached_sort(&judged))
            .is_none()
    );
    assert!(context.cached_sort(&judged).is_some());
}

/// A rollback that withdrew level constraints clears the reducts where a judgment's reduction put a question to conversion inside the scope, and only there. A question is answered on the constraints that stand, so a reduct taken inside may rest on one the rollback withdraws; a scope that asked nothing leaves the reducts alone, no rule of reduction reading a level.
///
/// Mutation-checked both ways: with the reducts kept whatever was asked, the second scope's reduct outlives its constraint, and with them cleared whatever was asked, the first scope loses a reduct it had no reason to.
#[test]
fn a_rollback_that_withdrew_a_level_constraint_clears_what_a_question_may_rest_on() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let sum = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    let this = context.fresh_universe(UniverseRole::Flexible, None);
    let that = context.fresh_universe(UniverseRole::Flexible, None);
    let equate = |context: &mut Context| {
        context
            .universes_mut()
            .add_eq(
                this.clone(),
                that.clone(),
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .expect("two fresh levels may be one");
    };

    context.reduce(sum.clone(), &literal(3));
    let mark = context.solution_mark();
    equate(&mut context);
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(
        context.cached_reduced(&sum),
        Some(literal(3)),
        "no question was asked inside the scope"
    );

    let mark = context.solution_mark();
    equate(&mut context);
    context.note_question();
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), None);
}
