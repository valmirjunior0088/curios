use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Anchor, Anchored, Cost, Definition, DefinitionKind, Extent, Free, Global, Intrinsic, Item,
        Level, MetavarId, Minted, Module, Nat, Subterm, Term, Totality, Tuple, UniverseContext,
        UniverseMetaId, UniverseParam, test_support::declaring,
    },
    curios_utilities::Qualifier,
};

fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

fn nat() -> Term {
    Subterm::Intrinsic(Intrinsic::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n))).into()
}

/// A lowered module holding `body` as its one definition's value.
fn lowered_module(body: Term) -> Module {
    declaring(
        Module {
            mounts: Vec::new(),
            items: vec![Item::Let(Definition {
                name: Global::Authored(Qualifier::from(["held"])),
                kind: DefinitionKind::Authored,
                universe_context: UniverseContext::empty(),
                island: Qualifier::empty(),
                totality: Totality::default(),
                proof: false,
                variances: Vec::new(),
                type_: nat(),
                body,
            })]
            .into(),
            witnesses: Default::default(),
            tests: Default::default(),
        },
        [],
        [],
        [],
    )
}

#[test]
fn lowered_module_validation_rejects_a_truncated_universe_seed_table() {
    let module = lowered_module(Term::type_at(Level::meta(UniverseMetaId(0))));

    assert!(matches!(
        validate_lowered_universe_seeds(&module, &Minted::default()),
        Err(Error::UniverseInvariant(message)) if message.contains("?u0")
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
    context.birth_metavar(MetavarId(0), vec![(x, nat())], nat());
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

/// Materializing a solved metavariable at the base of a graph costs the graph, not its tree: each level sums the one below with itself, so the tree doubles per level, and sixty levels are past anything a tree walk finishes. The result stays a graph — both operands of its root are one node.
#[test]
fn materializes_a_shared_graph_once_per_node() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), nat_lit(1));

    let mut term = Term::hole(0);
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    let zonked = zonk_solved_term_metas(&context, &term);

    assert!(zonked.metavars().is_empty());
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &*zonked else {
        panic!("zonking kept the root: {zonked}");
    };
    assert!(std::ptr::eq::<Subterm>(&**left, &**right));
}

/// A module's goals are gathered once per node of its graph, not once per path: one written goal at the base of sixty levels that each sum the one below with itself is met once.
#[test]
fn goals_are_gathered_once_per_node() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());

    let mut body = Term::goal(0);
    for _ in 0..60 {
        body = Term::intrinsic(Intrinsic::nat_add(body.clone(), body));
    }
    let sites = goal_sites(&context, &lowered_module(body), None);

    assert_eq!(sites.len(), 1);
}

/// A value's universes are validated once per node of its graph, not once per path: a level at the base of sixty levels that each sum the one below with itself is checked in the graph's size.
#[test]
fn universes_are_validated_once_per_node() {
    let mut value = Term::type_at(Level::param(UniverseParam(0)));
    for _ in 0..60 {
        value = Term::intrinsic(Intrinsic::nat_add(value.clone(), value));
    }

    assert!(validate_bound_universes(&value, 1, "doubled").is_ok());
    assert!(validate_bound_universes(&value, 0, "doubled").is_err());
}

/// The strict zonk splices a solution once per node of a graph, not once per path: a solved hole at the base of sixty levels that each sum the one below with itself is zonked in the graph's size, and the result stays a graph.
#[test]
fn a_shared_graph_is_zonked_once_per_node() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), nat_lit(1));

    let mut term = Term::hole(0);
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    let zonked = zonk(&context, &term).expect("the hole is solved");

    assert!(zonked.metavars().is_empty());
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &*zonked else {
        panic!("zonking kept the root: {zonked}");
    };
    assert!(std::ptr::eq::<Subterm>(&**left, &**right));
}

/// A hole zonked once still lands at each of its occurrences under that occurrence's extent, so a refusal downstream of the zonk points at the occurrence it is about.
#[test]
fn a_hole_zonked_once_keeps_each_occurrence_s_extent() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), nat_lit(1));
    let extent = |start| Extent {
        anchor: Anchor::of(Anchored::Entry),
        start,
        end: start + 1,
        stamp: 0,
    };
    let hole = Term::hole(0);
    let term = Term::tuple([
        hole.clone().with_extent(extent(0)),
        hole.with_extent(extent(2)),
    ]);

    let zonked = zonk(&context, &term).expect("the hole is solved");

    let Subterm::Tuple(tuple) = &*zonked else {
        panic!("zonking changed the shape: {zonked}");
    };
    assert_eq!(tuple.fields[0].extent(), Some(extent(0)));
    assert_eq!(tuple.fields[1].extent(), Some(extent(2)));
}

/// A budget that runs out while a term under a binder is zonked is refused in words. The level the walk cannot pay for is the body's, which it reads where it stands, its binder's occurrence a loose index; a report over that body is one the printer has no name for, so the report is over the nearest enclosing term that holds no loose index.
///
/// Mutation-checked: with the body reported as it stands, rendering the refusal panics on the loose index.
#[test]
fn a_budget_spent_under_a_binder_is_reported_over_a_term_that_prints() {
    // The function's level and its body's are two frames, and this pays for one and a half.
    let mut context = Context::new(Cost::FRAME.get() + Cost::FRAME.get() / 2, SYNTAX);
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.solve_metavar(MetavarId(0), nat());
    let x = Free::local(0, Some("x"));
    let function = Term::func(
        [(x, nat())],
        Term::tuple([Term::free_var(&x), Term::hole(0)]),
    );

    let refused = zonk(&context, &function).expect_err("the body's level is past the budget");

    assert!(
        refused
            .format_with(&lowered_module(nat()), &[], &SYNTAX, &[])
            .contains("ran out"),
    );
    let Error::ReduceExhausted { term, .. } = &refused else {
        panic!("a spent budget: {refused:?}");
    };
    assert_eq!(
        **term, function,
        "the report names the function, which holds no loose index"
    );
}
