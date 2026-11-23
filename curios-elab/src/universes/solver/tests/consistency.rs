//! Consistency: positive cycles and contradictory bounds refused with the least conflict and the origins that show it, constraints discharged at insertion, a context held to the naturals, and speculation rolled back or abandoned.

use {
    crate::universes::solver::*,
    curios_common::{Plicity, Qualifier, Symbol},
    curios_core::{
        Anchor, Anchored, ArgumentSite, CalleeId, Extent, Global, Mention, SortForm, WrittenSort,
    },
};

/// A scheme stating one constraint.
fn raised_in_a_scheme() -> UniverseContext {
    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: Level::param(UniverseParam(0)),
            upper: Level::param(UniverseParam(1)),
        }],
    }
}

/// A constraint an occurrence instantiates is restated where the occurrence is written and names the declaration the occurrence instantiates: the conflict it closes is reported at a term its reader wrote, with whose bound it is.
#[test]
fn a_constraint_an_occurrence_instantiates_takes_its_place_and_names_what_it_instantiates() {
    let scheme = raised_in_a_scheme();
    let used = Global::Authored(Qualifier::from(["app", "coerce"]));
    let written = Extent {
        anchor: Anchor::of(Anchored::Entry),
        start: 9,
        end: 15,
        stamp: 0,
    };
    let mut solver = UniverseSolver::new(0);

    solver
        .instantiate(
            &scheme,
            Occurrence {
                instantiated: used,
                extent: Some(written),
            },
        )
        .unwrap();

    let constraints = solver.constraints();
    let [(_, origin)] = constraints.as_slice() else {
        panic!("one constraint, got {constraints:?}");
    };
    assert_eq!(
        **origin,
        UniverseConstraintOrigin::Restated {
            declaration: used,
            extent: Some(written),
        }
    );
}

#[test]
fn solver_rejects_direct_and_long_positive_cycles_with_paths() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
    assert!(matches!(
        error,
        UniverseError::Inconsistency { path, .. } if path.len() == 1
    ));

    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    let w = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(v),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(v),
            Level::meta(w),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(w),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
    assert!(
        matches!(
        error,
        UniverseError::Inconsistency { ref path, .. } if path.len() == 3
        ),
        "{error:?}"
    );
}

/// A constraint keeps where it was raised through an assignment that rewrites its inequality, and through the rollback of that rewrite: the store moves the inequality and never its origin, so a conflict found after the rewrite still names the slot the term was checked as.
#[test]
fn a_held_constraint_keeps_its_slot_through_a_rewrite_and_its_rollback() {
    let held = UniverseConstraintOrigin::Held {
        extent: None,
        proposition: false,
        site: Some(ArgumentSite {
            callee: CalleeId::Function(Mention::Declaration(Global::Authored(Qualifier::from([
                "app", "coerce",
            ])))),
            parameter: Some(Symbol::new("A")),
            plicity: Plicity::Explicit,
            position: 0,
        }),
    };
    let mut solver = UniverseSolver::new(0);
    let lower = solver.fresh(UniverseRole::Flexible, None);
    let upper = solver.fresh(UniverseRole::Generalizable, None);
    let chosen = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(Level::meta(lower), Level::meta(upper), held.clone())
        .unwrap();
    let raised = UniverseConstraint {
        lower: Level::meta(lower),
        upper: Level::meta(upper),
    };

    let mark = solver.mark();
    solver.assign(lower, Level::meta(chosen)).unwrap();
    let rewritten = UniverseConstraint {
        lower: Level::meta(chosen),
        upper: Level::meta(upper),
    };
    assert_eq!(solver.constraints(), vec![(&rewritten, &held)]);

    solver.rollback(mark);
    solver.release(mark);
    assert_eq!(solver.constraints(), vec![(&raised, &held)]);
}

#[test]
fn seeded_written_type_origin_survives_into_an_inconsistency_path() {
    let written = WrittenSort {
        extent: None,
        form: SortForm::Type,
        local: false,
    };
    let mut solver = UniverseSolver::new(0);
    solver.seed(&[UniverseSeed {
        role: UniverseRole::Generalizable,
        written: Some(written.clone()),
    }]);

    solver
        .add_leq(
            Level::meta(UniverseMetaId(0)).succ().unwrap(),
            Level::meta(UniverseMetaId(0)),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();

    assert!(matches!(
        error,
        UniverseError::Inconsistency { path, .. }
            if path.contains(&UniverseConstraintOrigin::Written(written.clone()))
    ));
}

#[test]
fn solver_rejects_cycles_hidden_behind_right_hand_maxima() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::max([Level::meta(u), Level::meta(v)]),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(v),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
    // `u + 1 ≤ max(u, v)` holds alone, at `v` above `u`: the conflict needs `v ≤ u` too.
    assert!(
        matches!(&error, UniverseError::Inconsistency { path, .. } if path.len() == 2),
        "{error:?}"
    );
}

#[test]
fn solver_rejects_inconsistent_constant_bounds_through_atoms() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::constant(3),
            Level::meta(u).succ().unwrap(),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::constant(2),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
    assert!(matches!(error, UniverseError::Inconsistency { .. }));
}

/// A refusal names the constraints its conflict needs and no other: a loop of two beside a bound on the same levels that holds alone, raised from elsewhere, names the loop's two.
#[test]
fn an_inconsistency_names_no_constraint_its_conflict_does_not_need() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u),
            Level::meta(v),
            UniverseConstraintOrigin::Stated,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(v),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(v),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();

    let error = solver.check_consistent().unwrap_err();
    assert!(
        matches!(
            &error,
            UniverseError::Inconsistency { path, .. }
                if *path == vec![UniverseConstraintOrigin::Converted; 2]
        ),
        "{error:?}"
    );
}

/// A refusal is worded from the constraint of its conflict raised last, the one the rest refuses.
#[test]
fn an_inconsistency_is_worded_from_the_constraint_its_conflict_raised_last() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(v),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver
        .add_leq(
            Level::meta(v),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();

    let error = solver.check_consistent().unwrap_err();
    assert!(
        matches!(
            &error,
            UniverseError::Inconsistency { lower, upper, .. }
                if *lower == Level::meta(v) && *upper == Level::meta(u)
        ),
        "{error:?}"
    );
}

#[test]
fn ground_and_reflexive_constraints_discharge_at_insertion() {
    let mut solver = UniverseSolver::new(0);
    solver
        .add_leq(
            Level::zero(),
            Level::constant(1),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_eq(
            Level::meta(u),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    assert!(solver.constraints().is_empty());

    assert!(matches!(
        solver.add_leq(Level::constant(1), Level::zero(), UniverseConstraintOrigin::Converted),
        Err(UniverseError::Inconsistency { path, .. }) if path.len() == 1
    ));
}

#[test]
fn rollback_restores_constraints_and_solutions() {
    let mut solver = UniverseSolver::new(0);
    let input = solver.fresh(UniverseRole::Generalizable, None);
    let output = solver.fresh(UniverseRole::Flexible, None);
    let mark = solver.mark();
    solver
        .add_leq(
            Level::meta(input),
            Level::meta(output),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert!(solver.solution(output).is_some());
    solver.rollback(mark);
    assert!(solver.constraints().is_empty());
    assert!(solver.solution(output).is_none());
}

/// Every level a two-parameter context can state within three rungs: the constants, each parameter at each offset, and the maxima that mix them: the alphabet the sweep below builds every context of one and two constraints from.
fn small_levels() -> Vec<Level> {
    let param = |index: usize| Level::param(UniverseParam(index));
    let plus = |level: Level, offset: u32| level.checked_add(offset).expect("offset");

    vec![
        Level::constant(0),
        Level::constant(1),
        Level::constant(2),
        param(0),
        plus(param(0), 1),
        plus(param(0), 2),
        param(1),
        plus(param(1), 1),
        plus(param(1), 2),
        Level::max([param(0), param(1)]),
        Level::max([plus(param(0), 1), param(1)]),
        Level::max([param(0), plus(param(1), 1)]),
        Level::max([plus(param(0), 2), param(1)]),
        Level::max([Level::constant(1), param(0)]),
        Level::max([Level::constant(2), plus(param(0), 1)]),
        Level::max([Level::constant(2), param(1)]),
        Level::max([Level::constant(1), param(0), param(1)]),
        Level::max([Level::constant(3), plus(param(0), 1), plus(param(1), 2)]),
    ]
}

/// What `level` denotes when the parameters are assigned `at`: a maximum over its constant and each atom's head plus its offset.
fn level_value(level: &Level, at: &[u64]) -> u64 {
    let mut value = u64::from(level.constant_part());
    for (head, offset) in level.atoms() {
        let base = match head {
            curios_core::LevelHead::Param(param) => at[param.0],
            curios_core::LevelHead::Meta(_) => panic!("the sweep states no metavariable"),
        };
        value = value.max(base + u64::from(offset));
    }

    value
}

/// Whether some assignment of naturals below `bound` satisfies every constraint — the ground truth a context's validity is held to.
fn satisfied_somewhere(constraints: &[UniverseConstraint], params: usize, bound: u64) -> bool {
    let mut at = vec![0u64; params];
    let total = (bound + 1).pow(params as u32);
    for index in 0..total {
        let mut rest = index;
        for slot in at.iter_mut() {
            *slot = rest % (bound + 1);
            rest /= bound + 1;
        }
        if constraints.iter().all(|constraint| {
            level_value(&constraint.lower, &at) <= level_value(&constraint.upper, &at)
        }) {
            return true;
        }
    }

    false
}

/// A context is valid exactly where an assignment of naturals satisfies it.
///
/// `curios-analysis`'s `satisfy::tests::a_set_is_satisfiable_exactly_when_it_has_a_model_in_the_naturals` holds the decision itself over single constraints and pairs. This holds the route a context takes here — its constraints raised as the store raises them, through `add_constraint`, which discharges what holds structurally and refuses what is ground and false, then the decision asked of what the store keeps — over every ordered pair of [`small_levels`] as one constraint and every set of two of those, and adds the case neither reaches: sets of **three** constraints, over the narrower alphabet a loop through a right-hand maximum needs, since a maximum read the other way is a disjunction and the set that separates a conjunctive derivation from the naturals' total order wants a clause to bound each part of one.
///
/// The unsound direction is a context called valid that no assignment satisfies: a walk assumes it, `entails` derives every level relation under it and `check_instance` discharges nothing. Both directions are asserted, and both answers must occur, or the sweep would be testing a constant.
#[test]
fn a_context_is_valid_exactly_where_an_assignment_of_naturals_satisfies_it() {
    const PARAMS: usize = 2;
    const BOUND: u64 = 12;
    let levels = small_levels();
    let constraint = |lower: &Level, upper: &Level| UniverseConstraint {
        lower: lower.clone(),
        upper: upper.clone(),
    };

    let singles = levels
        .iter()
        .flat_map(|lower| levels.iter().map(move |upper| constraint(lower, upper)))
        .collect::<Vec<_>>();

    let mut mistaken: Vec<(bool, Vec<UniverseConstraint>)> = Vec::new();
    let mut answers = [0usize; 2];
    let mut sets = 0usize;

    let mut judge = |constraints: Vec<UniverseConstraint>| {
        let truth = satisfied_somewhere(&constraints, PARAMS, BOUND);
        let context = UniverseContext {
            parameter_count: PARAMS,
            quality_count: 0,
            constraints: constraints.clone(),
        };
        let valid = universe_context_validate(&context).is_ok();

        if valid != truth {
            mistaken.push((valid, constraints));
        }
        answers[usize::from(valid)] += 1;
        sets += 1;
    };

    for one in &singles {
        judge(vec![one.clone()]);
    }
    for first in &singles {
        for second in &singles {
            judge(vec![first.clone(), second.clone()]);
        }
    }

    // Three constraints over the narrower alphabet a loop through a right-hand maximum needs: a maximum is a disjunction read the other way, so the set that separates a conjunctive derivation from the naturals' total order wants a clause to bound each part of one.
    let narrow = [
        Level::constant(0),
        Level::constant(1),
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(0)).succ().expect("successor"),
        Level::param(UniverseParam(1)),
        Level::max([
            Level::param(UniverseParam(0)),
            Level::param(UniverseParam(1)),
        ]),
        Level::max([
            Level::param(UniverseParam(0)).succ().expect("successor"),
            Level::param(UniverseParam(1)),
        ]),
    ];
    let triples = narrow
        .iter()
        .flat_map(|lower| narrow.iter().map(move |upper| constraint(lower, upper)))
        .collect::<Vec<_>>();
    for first in &triples {
        for second in &triples {
            for third in &triples {
                judge(vec![first.clone(), second.clone(), third.clone()]);
            }
        }
    }

    assert!(
        mistaken.is_empty(),
        "{} of {sets} contexts were judged against the naturals, the first (judged valid: {}) being {:?}",
        mistaken.len(),
        mistaken.first().is_some_and(|(valid, _)| *valid),
        mistaken.first().map(|(_, constraints)| constraints),
    );
    assert!(
        answers.iter().all(|count| *count > 0),
        "every one of {sets} contexts was judged alike, so the sweep compared nothing: {answers:?}",
    );
}

/// A refusal unwinds through brackets that never release, leaving their scopes open and the journal recording pre-images nothing will roll back to; the item boundary closes them all at once.
#[test]
fn abandoning_speculation_closes_every_scope_a_refusal_left_open() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Flexible, None);
    let _outer = solver.mark();
    let _inner = solver.mark();
    solver
        .add_leq(
            Level::zero(),
            Level::meta(u),
            UniverseConstraintOrigin::Converted,
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(solver.open_scopes(), 2);

    solver.abandon_speculation();

    assert_eq!(solver.open_scopes(), 0);
    assert_eq!(solver.journal_len(), 0);
}
