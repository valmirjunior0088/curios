use crate::*;
use curios_core::*;

fn context() -> Context {
    Context::with_default_budget(crate::SYNTAX)
}

fn nat() -> Term {
    Subterm::Intrinsic(Intrinsic::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n))).into()
}

fn lowered_module(body: Term, universe_seeds: Vec<UniverseSeed>) -> Module {
    Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds,
        induct_decls: Default::default(),
        struct_decls: Default::default(),
        concepts: Default::default(),
        witnesses: Default::default(),
        tests: Default::default(),
        binder_floor: 0,
        entry: Some(Entrypoint { body, type_: None }),
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
fn leaves_a_meta_free_term_unchanged() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    let term = Term::func([(x, Term::type_ground())], nat_lit(0));
    let zonked = zonk(&context, &term).unwrap();

    assert_eq!(zonked, term);
}

#[test]
fn replaces_a_solved_metavariable_with_its_solution() {
    let mut context = context();

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.solve_metavar(MetavarId(0), nat());

    let zonked = zonk(&context, &Term::hole(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn resolves_a_metavariable_in_an_inductive_match_default() {
    let mut context = context();

    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), nat_lit(7));

    // The catch-all default is a real term position, so a solved metavar sitting in it is resolved like any other.
    let scrutinee = context.fresh(Some("r"));
    let motive = context.fresh(Some("m"));
    let term = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&motive),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        Term::hole(0),
    );

    let expected = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&motive),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        nat_lit(7),
    );

    assert_eq!(zonk(&context, &term).unwrap(), expected);
}

#[test]
fn resolves_a_metavariable_nested_in_a_structure() {
    let mut context = context();

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.solve_metavar(MetavarId(0), nat());

    // A tuple `{ ?0 }` zonks to `{ Nat }`.
    let term = Subterm::Tuple(Tuple {
        fields: vec![Term::hole(0)],
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
fn chases_a_solution_that_mentions_another_metavariable() {
    let mut context = context();

    // ?0 := ?1, ?1 := Nat. Zonking ?0 must resolve through to `Nat`.
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());
    context.solve_metavar(MetavarId(1), nat());
    context.solve_metavar(MetavarId(0), Term::hole(1));

    let zonked = zonk(&context, &Term::hole(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn rejects_an_unsolved_metavariable() {
    let mut context = context();

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    let result = zonk(&context, &Term::hole(0));

    assert!(result.is_err());
}

#[test]
fn reports_a_solved_goal() {
    let mut context = context();

    // A written goal `?` errors even when solved — the report carries the frozen scope, the goal's type, and the committed solution.
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), vec![(x.clone(), nat())], nat());
    context.solve_metavar(MetavarId(0), nat_lit(7));

    let error = zonk(&context, &Term::goal(0)).unwrap_err();

    assert!(matches!(
        &error,
        Error::Goal { scope, goal, solution: Some(solution) }
            if **goal == nat() && **solution == nat_lit(7)
                && *scope == vec![(Term::free_var(&x), nat())]
    ));
}

#[test]
fn reports_an_unsolved_goal_as_undetermined() {
    let mut context = context();

    context.birth_metavar(MetavarId(0), Vec::new(), nat());

    let error = zonk(&context, &Term::goal(0)).unwrap_err();

    assert!(matches!(
        &error,
        Error::Goal { scope, goal, solution: None } if **goal == nat() && scope.is_empty()
    ));
}
