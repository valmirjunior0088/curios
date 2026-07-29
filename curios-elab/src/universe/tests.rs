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

/// The companion to [`non_principal_flexible_bound_is_not_arbitrarily_defaulted`].
///
/// Mutual bounds force an equality and therefore *do* have a least solution,
/// so leaving them open is not conservatism — it is a level that never gets
/// solved. Witness dispatch emits exactly this shape, and every `%` slot in a
/// module body failed to elaborate while propagation stalled on it: each level
/// waits for the other, and neither is defaultable because both occur above.
#[test]
fn mutually_bounded_flexible_levels_close_at_their_floor() {
    let mut solver = UniverseSolver::new(0);
    let left = solver.fresh(UniverseRole::Flexible, None);
    let right = solver.fresh(UniverseRole::Flexible, None);
    let guarded = |meta| Level::max([Level::constant(1), Level::meta(meta)]);
    solver
        .add_leq(guarded(left), guarded(right), origin("dispatch"))
        .unwrap();
    solver
        .add_leq(guarded(right), guarded(left), origin("dispatch"))
        .unwrap();

    solver.solve_flexible().unwrap();

    assert_eq!(solver.solution(left), Some(&Level::zero()));
    assert_eq!(solver.solution(right), Some(&Level::zero()));
}

/// The same cycle, but forced above zero by a direct bound. `left` solves to 2
/// from that bound, leaving `2 ≤ max(1, right)`. Its upper side is not a bare
/// atom, so cancelling the offset alone does not answer it — but `max`'s own
/// constant of 1 cannot supply 2, so `right ≥ 2` is determined rather than
/// guessed. Before that was recognized the floor attempt tried `right := 0`,
/// found `2 ≤ max(1, 0)` inconsistent, rolled back, and left `right` unsolved
/// — surfacing as "level escapes its universe parameter context".
#[test]
fn a_mutually_bounded_cycle_closes_at_a_non_zero_floor() {
    let mut solver = UniverseSolver::new(0);
    let left = solver.fresh(UniverseRole::Flexible, None);
    let right = solver.fresh(UniverseRole::Flexible, None);
    let guarded = |meta| Level::max([Level::constant(1), Level::meta(meta)]);
    solver
        .add_leq(Level::constant(2), Level::meta(left), origin("direct"))
        .unwrap();
    solver
        .add_leq(guarded(left), guarded(right), origin("dispatch"))
        .unwrap();
    solver
        .add_leq(guarded(right), guarded(left), origin("dispatch"))
        .unwrap();

    solver.solve_flexible().unwrap();

    assert_eq!(solver.solution(left), Some(&Level::constant(2)));
    assert_eq!(solver.solution(right), Some(&Level::constant(2)));
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

/// A declaration context is closed: universe polymorphism belongs to
/// declarations, and there is no enclosing scheme whose parameters a
/// constraint could still mention. A parameter reaching finalization is
/// therefore an escape, not an outer reference to be recorded.
#[test]
fn generalization_rejects_a_constraint_mentioning_an_enclosing_parameter() {
    let mut solver = UniverseSolver::new(0);
    let outer = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(outer),
            Level::param(UniverseParam(0)),
            origin("scoped upper"),
        )
        .unwrap();

    assert!(matches!(
        solver.finalize([outer], []),
        Err(UniverseError::EscapingLevel)
    ));
}

#[test]
fn level_parameter_names_stay_alphabetic_past_the_sixth() {
    let name = |index: usize| Level::param(UniverseParam(index)).to_string();
    assert_eq!(name(0), "u");
    assert_eq!(name(5), "z");
    // `u + 6` is `{` in ASCII; a stepping scheme that runs off `z` prints
    // punctuation where a level name belongs.
    assert_eq!(name(6), "u1");
    assert_eq!(name(8), "w1");
    assert_eq!(name(12), "u2");
    for index in 0..64 {
        assert!(
            name(index).chars().all(|c| c.is_ascii_alphanumeric()),
            "level parameter {index} printed as {}",
            name(index)
        );
    }
}
