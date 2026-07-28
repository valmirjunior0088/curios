use crate::*;

fn context() -> Context {
    Context::with_default_budget()
}

fn nat() -> Term {
    Subterm::Prim(Prim::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Prim(Prim::Nat(Nat::new(n))).into()
}

fn lowered_module(body: Term, universe_seeds: Vec<UniverseSeed>) -> Module {
    Module {
        items: Vec::new(),
        universe_seeds,
        induct_decls: Default::default(),
        struct_decls: Default::default(),
        concepts: Default::default(),
        witnesses: Default::default(),
        binder_floor: 0,
        type_: None,
        body,
    }
}

#[test]
fn lowered_module_validation_rejects_a_truncated_universe_seed_table() {
    let module = lowered_module(Term::type_at(Level::meta(UniverseMetaId(0))), Vec::new());

    assert!(matches!(
        validate_lowered_universe_seeds(&module, 0),
        Err(Error::UniverseInvariant(message)) if message.contains("?u0")
    ));
}

#[test]
fn lowered_module_validation_rejects_a_seed_floor_mismatch() {
    let module = lowered_module(
        Term::type_ground(),
        vec![UniverseSeed {
            role: UniverseRole::Flexible,
            origin: None,
        }],
    );

    assert!(matches!(
        validate_lowered_universe_seeds(&module, 0),
        Err(Error::UniverseInvariant(message)) if message.contains("seed table")
    ));
}

#[test]
fn zonk_leaves_a_meta_free_term_unchanged() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    let term = Term::func([(x, Term::type_ground())], nat_lit(0));
    let zonked = zonk(&context, &term).unwrap();

    assert_eq!(zonked, term);
}

#[test]
fn zonk_replaces_a_solved_metavariable_with_its_solution() {
    let mut context = context();

    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    context.solve_metavar(MetaId(0), nat());

    let zonked = zonk(&context, &Term::metavar(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn zonk_resolves_a_metavariable_in_an_inductive_match_default() {
    let mut context = context();

    context.birth_metavar(MetaId(0), Vec::new(), nat());
    context.solve_metavar(MetaId(0), nat_lit(7));

    // The catch-all default is a real term position, so a solved metavar sitting
    // in it is resolved like any other.
    let scrutinee = context.fresh(Some("r"));
    let motive = context.fresh(Some("m"));
    let term = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&motive),
        nat(),
        [("none", Vec::<crate::Free>::new(), nat_lit(0))],
        Term::metavar(0),
    );

    let expected = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&motive),
        nat(),
        [("none", Vec::<crate::Free>::new(), nat_lit(0))],
        nat_lit(7),
    );

    assert_eq!(zonk(&context, &term).unwrap(), expected);
}

#[test]
fn zonk_resolves_a_metavariable_nested_in_a_structure() {
    let mut context = context();

    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    context.solve_metavar(MetaId(0), nat());

    // A tuple `{ ?0 }` zonks to `{ Nat }`.
    let term = Subterm::Tuple(Tuple {
        fields: vec![Term::metavar(0)],
        names: vec![],
    })
    .into();

    let zonked = zonk(&context, &term).unwrap();

    let expected = Subterm::Tuple(Tuple {
        fields: vec![nat()],
        names: vec![],
    })
    .into();

    assert_eq!(zonked, expected);
    assert!(zonked.metavars().is_empty());
}

#[test]
fn zonk_chases_a_solution_that_mentions_another_metavariable() {
    let mut context = context();

    // ?0 := ?1, ?1 := Nat. Zonking ?0 must resolve through to `Nat`.
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());
    context.solve_metavar(MetaId(1), nat());
    context.solve_metavar(MetaId(0), Term::metavar(1));

    let zonked = zonk(&context, &Term::metavar(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn universe_dependencies_of_a_solved_meta_follow_only_its_materialized_solution() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);
    let solution = UniverseMetaId(2);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetaId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );
    context.solve_metavar(MetaId(0), Term::type_at(Level::meta(solution)));

    assert_eq!(
        context.universe_metas_in(&Term::metavar(0)),
        [solution].into()
    );
}

#[test]
fn universe_dependencies_of_an_unsolved_meta_keep_its_birth_context() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetaId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );

    assert_eq!(
        context.universe_metas_in(&Term::metavar(0)),
        [result, telescope].into()
    );
}

#[test]
fn zonk_rejects_an_unsolved_metavariable() {
    let mut context = context();

    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    let result = zonk(&context, &Term::metavar(0));

    assert!(result.is_err());
}

#[test]
fn zonk_reports_a_solved_goal() {
    let mut context = context();

    // A written goal `?` errors even when solved — the report carries the
    // frozen scope, the goal's type, and the committed solution.
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetaId(0), vec![(x.clone(), nat())], nat());
    context.solve_metavar(MetaId(0), nat_lit(7));

    let error = zonk(&context, &Term::goal(0)).unwrap_err();

    assert!(matches!(
        &error,
        Error::Goal { scope, goal, solution: Some(solution) }
            if **goal == nat() && **solution == nat_lit(7)
                && *scope == vec![(Term::free_var(&x), nat())]
    ));
}

#[test]
fn zonk_reports_an_unsolved_goal_as_undetermined() {
    let mut context = context();

    context.birth_metavar(MetaId(0), Vec::new(), nat());

    let error = zonk(&context, &Term::goal(0)).unwrap_err();

    assert!(matches!(
        &error,
        Error::Goal { scope, goal, solution: None } if **goal == nat() && scope.is_empty()
    ));
}
