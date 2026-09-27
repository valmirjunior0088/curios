use {
    crate::*, curios_analysis::fixture::SYNTAX, curios_core::CalleeId, curios_core::*,
    curios_utilities::Qualifier, std::collections::BTreeSet,
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
        name: name.clone(),
        module: Qualifier::empty(),
        universe_context: UniverseContext::empty(),
        signature: Term::type_ground(),
    };
    let nat = WitnessKey(vec![HeadKey::Nat]);
    let bool_ = WitnessKey(vec![HeadKey::Bool]);
    assert!(
        context
            .insert_witness(concept.clone(), nat.clone(), witness())
            .is_none()
    );
    assert!(
        context
            .insert_witness(concept.clone(), bool_.clone(), witness())
            .is_none()
    );

    let removed = context
        .remove_witness(&name)
        .into_iter()
        .collect::<BTreeSet<_>>();

    assert_eq!(
        removed,
        BTreeSet::from([
            (concept.clone(), nat.clone()),
            (concept.clone(), bool_.clone())
        ])
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

/// An arm re-types `x` under its own name; a problem parked there is retried where `x` is live at its unspecialized type — the enclosing frame, where a drain or a turnaround after the arm runs it. The retry sees the arm's type, because the frame it runs in is the one the problem froze. Reapplying only names not already live lost exactly this.
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
            telescope
                .iter()
                .map(|(name, _)| name.clone())
                .collect::<Vec<_>>()
        });
        assert_eq!(names, vec![a.clone()]);
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
