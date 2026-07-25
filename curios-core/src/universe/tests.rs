use super::*;
use std::{
    collections::{BTreeSet, hash_map::DefaultHasher},
    hash::{Hash, Hasher},
};

fn origin(label: &str) -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Other(label.into()))
}

fn hash(value: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn level_max_is_canonical() {
    let u = Level::meta(UniverseMetaId(0));
    let v = Level::meta(UniverseMetaId(1));
    let left = Level::max([
        Level::zero(),
        u.clone(),
        v.succ().unwrap(),
        u.checked_add(3).unwrap(),
    ]);
    let right = Level::max([
        u.checked_add(3).unwrap(),
        Level::max([v.succ().unwrap(), u]),
    ]);
    assert_eq!(left, right);
    assert_eq!(hash(&left), hash(&right));
    assert_eq!(left.to_string(), "max(?u0+3,?u1+1)");
}

#[test]
fn successor_distributes_and_overflow_is_checked() {
    let level = Level::max([
        Level::constant(2),
        Level::param(UniverseParam(0)).checked_add(4).unwrap(),
    ]);
    assert_eq!(level.checked_add(3).unwrap().to_string(), "u+7");
    assert_eq!(
        Level::constant(u32::MAX).succ(),
        Err(UniverseError::OffsetOverflow)
    );
}

#[test]
fn solver_rejects_direct_and_long_positive_cycles_with_paths() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let error = solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(u),
            origin("direct"),
        )
        .unwrap_err();
    assert!(matches!(
        error,
        UniverseError::Inconsistency { path, .. } if path.len() == 1
    ));

    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    let w = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(Level::meta(u).succ().unwrap(), Level::meta(v), origin("uv"))
        .unwrap();
    solver
        .add_leq(Level::meta(v), Level::meta(w), origin("vw"))
        .unwrap();
    let error = solver
        .add_leq(Level::meta(w), Level::meta(u), origin("wu"))
        .unwrap_err();
    assert!(
        matches!(
        error,
        UniverseError::Inconsistency { ref path, .. } if path.len() == 3
        ),
        "{error:?}"
    );
}

#[test]
fn seeded_written_type_origin_survives_into_an_inconsistency_path() {
    let written = UniverseConstraintOrigin::new(UniverseConstraintKind::WrittenType);
    let mut solver = UniverseSolver::new(0);
    solver.seed(&[UniverseSeed {
        role: UniverseRole::Generalizable,
        origin: Some(written.clone()),
    }]);

    let error = solver
        .add_leq(
            Level::meta(UniverseMetaId(0)).succ().unwrap(),
            Level::meta(UniverseMetaId(0)),
            origin("cycle"),
        )
        .unwrap_err();

    assert!(matches!(
        error,
        UniverseError::Inconsistency { path, .. } if path.contains(&written)
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
            origin("max"),
        )
        .unwrap();
    let error = solver
        .add_leq(Level::meta(v), Level::meta(u), origin("back"))
        .unwrap_err();
    assert!(matches!(
        error,
        UniverseError::Inconsistency { path, .. } if !path.is_empty()
    ));
}

#[test]
fn solver_rejects_inconsistent_constant_bounds_through_atoms() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::constant(3),
            Level::meta(u).succ().unwrap(),
            origin("lower"),
        )
        .unwrap();
    let error = solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::constant(2),
            origin("upper"),
        )
        .unwrap_err();
    assert!(matches!(error, UniverseError::Inconsistency { .. }));
}

#[test]
fn incremental_consistency_matches_the_exact_solver() {
    let mut state = 0x4d59_5df4_d0f3_3173_u64;
    for trial in 0..1_000 {
        let mut solver = UniverseSolver::new(0);
        let u = solver.fresh(UniverseRole::Generalizable, None);
        let v = solver.fresh(UniverseRole::Generalizable, None);
        let levels = [
            Level::zero(),
            Level::constant(1),
            Level::meta(u),
            Level::meta(v),
            Level::meta(u).succ().unwrap(),
            Level::meta(v).succ().unwrap(),
            Level::max([Level::meta(u), Level::meta(v)]),
            Level::max([Level::meta(u).succ().unwrap(), Level::meta(v)]),
        ];
        for step in 0..12 {
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1);
            let lower = levels[(state as usize) % levels.len()].clone();
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1);
            let upper = levels[(state as usize) % levels.len()].clone();
            let constraint = UniverseConstraint {
                lower,
                upper,
                origin: origin(&format!("trial {trial}, step {step}")),
            };

            let mut exact = solver.clone();
            exact.consistency = None;
            let normalized = UniverseConstraint {
                lower: exact.zonk(&constraint.lower).unwrap(),
                upper: exact.zonk(&constraint.upper).unwrap(),
                origin: constraint.origin.clone(),
            };
            let exact_result = if normalized.is_tautology() {
                Ok(())
            } else if normalized.lower.atoms.is_empty() && normalized.upper.atoms.is_empty() {
                if normalized.lower.constant <= normalized.upper.constant {
                    Ok(())
                } else {
                    Err(UniverseError::Inconsistency {
                        lower: normalized.lower,
                        upper: normalized.upper,
                        path: vec![normalized.origin],
                    })
                }
            } else {
                exact.constraints.push(normalized);
                exact.check_consistent_full()
            };
            let incremental_result = solver.add_constraint(constraint);
            assert_eq!(
                incremental_result.is_ok(),
                exact_result.is_ok(),
                "consistency diverged in trial {trial} at step {step}"
            );
        }
    }
}

#[test]
fn ground_and_reflexive_constraints_discharge_at_insertion() {
    let mut solver = UniverseSolver::new(0);
    solver
        .add_leq(Level::zero(), Level::constant(1), origin("ground"))
        .unwrap();
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_eq(Level::meta(u), Level::meta(u), origin("reflexive"))
        .unwrap();
    assert!(solver.constraints().is_empty());

    assert!(matches!(
        solver.add_leq(Level::constant(1), Level::zero(), origin("inconsistent")),
        Err(UniverseError::Inconsistency { path, .. }) if path.len() == 1
    ));
}

#[test]
fn flexible_levels_take_least_solution_and_default_to_zero() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let output = solver.fresh(UniverseRole::Flexible, None);
    let unused = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(output),
            origin("bound"),
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(
        solver.solution(output),
        Some(&Level::meta(u).succ().unwrap())
    );
    assert_eq!(solver.solution(unused), Some(&Level::zero()));
}

#[test]
fn flexible_solver_cancels_matching_successor_offsets() {
    let mut solver = UniverseSolver::new(0);
    let input = solver.fresh(UniverseRole::Generalizable, None);
    let output = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::meta(input).succ().unwrap(),
            Level::meta(output).succ().unwrap(),
            origin("cancel"),
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(solver.solution(output), Some(&Level::meta(input)));
}

#[test]
fn non_principal_flexible_bound_is_not_arbitrarily_defaulted() {
    let mut solver = UniverseSolver::new(0);
    let left = solver.fresh(UniverseRole::Flexible, None);
    let right = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::constant(1),
            Level::max([Level::meta(left), Level::meta(right)]),
            origin("choice"),
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(solver.solution(left), None);
    assert_eq!(solver.solution(right), None);
}

#[test]
fn constraint_identity_ignores_diagnostic_provenance() {
    let semantic = || UniverseConstraint {
        lower: Level::param(UniverseParam(0)),
        upper: Level::param(UniverseParam(1)),
        origin: origin("first"),
    };
    let left = semantic();
    let mut right = semantic();
    right.origin = origin("second");
    right.origin.span = Some(Span {
        source: std::rc::Rc::new(curios_base::Source {
            path: None,
            text: "Type".into(),
        }),
        start: 0,
        end: 4,
    });

    assert_eq!(left, right);
    assert_eq!(hash(&left), hash(&right));
}

#[test]
fn flexible_dependency_chain_propagates_after_defaulting_its_floor() {
    let mut solver = UniverseSolver::new(0);
    let floor = solver.fresh(UniverseRole::Flexible, None);
    let output = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::meta(floor).succ().unwrap(),
            Level::meta(output),
            origin("successor"),
        )
        .unwrap();

    solver.solve_flexible().unwrap();

    assert_eq!(solver.solution(floor), Some(&Level::zero()));
    assert_eq!(solver.solution(output), Some(&Level::constant(1)));
}

#[test]
fn contexts_are_alpha_stable_and_instances_are_fresh() {
    let context = UniverseContext {
        parameter_count: 2,
        outer_parameter_count: 0,
        constraints: vec![UniverseConstraint {
            lower: Level::param(UniverseParam(0)).succ().unwrap(),
            upper: Level::param(UniverseParam(1)),
            origin: origin("residual"),
        }],
    };
    context.validate().unwrap();
    let mut solver = UniverseSolver::new(7);
    let first = solver
        .instantiate(&context, UniverseRole::Generalizable)
        .unwrap();
    let second = solver
        .instantiate(&context, UniverseRole::Generalizable)
        .unwrap();
    let first_metas = first.iter().flat_map(Level::metas).collect::<BTreeSet<_>>();
    let second_metas = second
        .iter()
        .flat_map(Level::metas)
        .collect::<BTreeSet<_>>();
    assert!(first_metas.is_disjoint(&second_metas));
}

#[test]
fn rollback_restores_constraints_and_solutions() {
    let mut solver = UniverseSolver::new(0);
    let input = solver.fresh(UniverseRole::Generalizable, None);
    let output = solver.fresh(UniverseRole::Flexible, None);
    let mark = solver.mark();
    solver
        .add_leq(Level::meta(input), Level::meta(output), origin("probe"))
        .unwrap();
    solver.solve_flexible().unwrap();
    assert!(solver.solution(output).is_some());
    solver.rollback(mark);
    assert!(solver.constraints().is_empty());
    assert!(solver.solution(output).is_none());
}

#[test]
fn generalization_is_deterministic_and_closed() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(Level::meta(u).succ().unwrap(), Level::meta(v), origin("uv"))
        .unwrap();
    let (context, replacement) = solver.generalize([v, u]).unwrap();
    assert_eq!(replacement[&u], Level::param(UniverseParam(0)));
    assert_eq!(replacement[&v], Level::param(UniverseParam(1)));
    assert_eq!(context.parameter_count, 2);
    context.validate().unwrap();
}

#[test]
fn forced_equalities_share_one_generalized_parameter() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_eq(Level::meta(u), Level::meta(v), origin("equal"))
        .unwrap();

    let context = solver.finalize([u, v], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
}

#[test]
fn non_principal_flexible_levels_are_promoted_to_residual_parameters() {
    let mut solver = UniverseSolver::new(0);
    let left = solver.fresh(UniverseRole::Flexible, None);
    let right = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::constant(1),
            Level::max([Level::meta(left), Level::meta(right)]),
            origin("choice"),
        )
        .unwrap();

    let context = solver.finalize([left, right], []).unwrap();
    assert_eq!(context.parameter_count, 2);
    assert_eq!(context.constraints.len(), 1);
    context.validate().unwrap();
}

#[test]
fn local_finalization_follows_solved_dependencies_to_protected_metas() {
    let mut solver = UniverseSolver::new(0);
    let local = solver.fresh(UniverseRole::Generalizable, None);
    let protected = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::meta(local),
            Level::meta(protected),
            origin("ambient dependency"),
        )
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(solver.solution(protected), Some(&Level::meta(local)));

    let context = solver.finalize_excluding([local], [], [protected]).unwrap();
    assert_eq!(context, UniverseContext::empty());
    assert!(solver.solution(local).is_none());
}

#[test]
fn generalization_preserves_scoped_parameter_constraints() {
    let mut solver = UniverseSolver::new(0);
    let outer = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(outer),
            Level::param(UniverseParam(0)),
            origin("scoped upper"),
        )
        .unwrap();
    solver
        .add_leq(
            Level::param(UniverseParam(1)),
            Level::meta(outer),
            origin("scoped lower"),
        )
        .unwrap();

    let context = solver.finalize([outer], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert_eq!(context.constraints.len(), 2);
    assert_eq!(context.constraints[0].lower, Level::param(UniverseParam(0)));
    assert_eq!(context.constraints[0].upper, Level::param(UniverseParam(1)));
    assert_eq!(context.constraints[1].lower, Level::param(UniverseParam(2)));
    assert_eq!(context.constraints[1].upper, Level::param(UniverseParam(0)));
    assert_eq!(context.outer_parameter_count, 2);
    context.validate().unwrap();
}
