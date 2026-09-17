use {
    crate::*, curios_analysis::fixture::SYNTAX, curios_core::*, curios_utilities::Qualifier,
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
            provenance: WitnessOrigin {
                func: "f".to_string(),
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
