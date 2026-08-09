use {super::*, std::collections::BTreeSet};

fn origin(label: &str) -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Other(label.into()))
}

#[test]
fn solver_rejects_direct_and_long_positive_cycles_with_paths() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(u),
            origin("direct"),
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
        .add_leq(Level::meta(u).succ().unwrap(), Level::meta(v), origin("uv"))
        .unwrap();
    solver
        .add_leq(Level::meta(v), Level::meta(w), origin("vw"))
        .unwrap();
    solver
        .add_leq(Level::meta(w), Level::meta(u), origin("wu"))
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

#[test]
fn seeded_written_type_origin_survives_into_an_inconsistency_path() {
    let written = UniverseConstraintOrigin::new(UniverseConstraintKind::WrittenType);
    let mut solver = UniverseSolver::new(0);
    solver.seed(&[UniverseSeed {
        role: UniverseRole::Generalizable,
        origin: Some(written.clone()),
    }]);

    solver
        .add_leq(
            Level::meta(UniverseMetaId(0)).succ().unwrap(),
            Level::meta(UniverseMetaId(0)),
            origin("cycle"),
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();

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
    solver
        .add_leq(Level::meta(v), Level::meta(u), origin("back"))
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
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
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::constant(2),
            origin("upper"),
        )
        .unwrap();
    let error = solver.check_consistent().unwrap_err();
    assert!(matches!(error, UniverseError::Inconsistency { .. }));
}

#[test]
fn consistency_checks_match_the_exact_solver() {
    for trial in 0..1_000_u64 {
        // One independent generator per trial, indexed off the high bits: an LCG's low three bits cycle with period 8, and one generator threaded across trials at a step count divisible by 8 kept every trial at the same phase — the shape under which this test once compared four distinct constraints, all discharged before either solver, twelve thousand times.
        let mut state = 0x4d59_5df4_d0f3_3173_u64 ^ trial.wrapping_mul(0x9e37_79b9_7f4a_7c15);
        let mut draw = || {
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1);
            (state >> 33) as usize
        };
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
            let lower = levels[draw() % levels.len()].clone();
            let upper = levels[draw() % levels.len()].clone();
            let constraint = UniverseConstraint {
                lower,
                upper,
                origin: origin(&format!("trial {trial}, step {step}")),
            };

            // The exact side: the same normalization and insertion-time discharges `add_constraint` performs, then the ground-up exponential check over the pushed store.
            let mut exact = solver.clone();
            exact.consistency = None;
            let normalized = UniverseConstraint {
                lower: exact.zonk(&constraint.lower).unwrap(),
                upper: exact.zonk(&constraint.upper).unwrap(),
                origin: constraint.origin.clone(),
            };
            let exact_result = if normalized.lower.structurally_leq(&normalized.upper) {
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

            // The incremental side decides in two moves — `add_constraint` refuses only the ground case and records otherwise; `check_consistent` is the verdict — so the comparison targets the pair, not the insertion alone.
            let incremental_result = solver
                .add_constraint(constraint)
                .and_then(|()| solver.check_consistent());
            assert_eq!(
                incremental_result.is_ok(),
                exact_result.is_ok(),
                "consistency diverged in trial {trial} at step {step}"
            );

            // An inconsistent store stays inconsistent — every later step would re-compare the same verdict — so a trial ends at its first refusal.
            if exact_result.is_err() {
                break;
            }
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
/// Mutual bounds force an equality and therefore *do* have a least solution, so leaving them open is not conservatism — it is a level that never gets solved. Witness dispatch emits exactly this shape, and every `%` slot in a module body failed to elaborate while propagation stalled on it: each level waits for the other, and neither is defaultable because both occur above.
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

/// The same cycle, but forced above zero by a direct bound. `left` solves to 2 from that bound, leaving `2 ≤ max(1, right)`. Its upper side is not a bare atom, so cancelling the offset alone does not answer it — but `max`'s own constant of 1 cannot supply 2, so `right ≥ 2` is determined rather than guessed. Before that was recognized the floor attempt tried `right := 0`, found `2 ≤ max(1, 0)` inconsistent, rolled back, and left `right` unsolved — surfacing as "level escapes its universe parameter context".
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
    universe_context_validate(&context).unwrap();
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
fn a_shape_equal_equation_defaults_its_single_differing_metas() {
    let mut solver = UniverseSolver::new(0);
    let a = solver.fresh(UniverseRole::Generalizable, None);
    let b = solver.fresh(UniverseRole::Flexible, None);
    let left = Level::max([Level::zero().succ().unwrap(), Level::meta(a)]);
    let right = Level::max([Level::zero().succ().unwrap(), Level::meta(b)]);
    solver.add_eq(left, right, origin("shape-equal")).unwrap();
    assert_eq!(solver.solution(b), Some(&Level::meta(a)));
    assert!(solver.solution(a).is_none());
}

#[test]
fn a_multi_difference_equation_is_not_defaulted() {
    let mut solver = UniverseSolver::new(0);
    let a = solver.fresh(UniverseRole::Generalizable, None);
    let b = solver.fresh(UniverseRole::Generalizable, None);
    let c = solver.fresh(UniverseRole::Generalizable, None);
    let d = solver.fresh(UniverseRole::Generalizable, None);
    let left = Level::max([Level::meta(a), Level::meta(c)]);
    let right = Level::max([Level::meta(b), Level::meta(d)]);
    solver.add_eq(left, right, origin("multi")).unwrap();
    assert!(solver.solution(a).is_none());
    assert!(solver.solution(b).is_none());
    assert!(solver.solution(c).is_none());
    assert!(solver.solution(d).is_none());
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
    universe_context_validate(&context).unwrap();
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
    universe_context_validate(&context).unwrap();
}

/// A declaration context is closed: universe polymorphism belongs to declarations, and there is no enclosing scheme whose parameters a constraint could still mention. A parameter reaching finalization is therefore an escape, not an outer reference to be recorded.
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

/// The two checkers decide universe-context validity separately, and this is where that decision is held to account.
///
/// `curios-cert/src/satisfy.rs` records the reason for the duplication: a transcription would inherit whatever the original gets wrong and agree for that reason, so the kernel's decision is written from the constraint semantics instead, and "a disagreement is a signal". A signal nothing can observe is not one. [`universe_context_validate`] runs during elaboration, so a context it rejects never becomes part of a module, and the kernel's `closed`/`satisfiable` are asked only about contexts this function has already passed — for every program in the corpus. Comparing them therefore has to be done directly, which is what this does, and it is the obligation that argument incurs rather than an extra precaution.
///
/// The direction that matters is the *kernel* being the more permissive of the two: it assumes an item's constraints while checking it, so a set it wrongly calls satisfiable is a hypothesis set from which every level relation follows, and `check_instance` stops discharging anything. The elaborator being the more permissive one is ordinary incompleteness and surfaces as a refusal. Neither is observable from a program, which is why the assertion is equality rather than an implication.
///
/// The final two assertions are the control: a table whose contexts were all valid, or all invalid, would pass this while comparing nothing.
#[test]
fn both_checkers_decide_universe_context_validity_alike() {
    let leq = |lower: Level, upper: Level| UniverseConstraint {
        lower,
        upper,
        origin: origin("differential"),
    };
    let param = |index: usize| Level::param(UniverseParam(index));

    let contexts: Vec<(&str, UniverseContext)> = vec![
        ("empty", UniverseContext::empty()),
        (
            "one bound",
            UniverseContext {
                parameter_count: 2,
                constraints: vec![leq(param(0), param(1))],
            },
        ),
        // Mutually bounded parameters are legal and satisfiable: they force equality, not a contradiction.
        (
            "cyclic but satisfiable",
            UniverseContext {
                parameter_count: 2,
                constraints: vec![leq(param(0), param(1)), leq(param(1), param(0))],
            },
        ),
        (
            "direct positive cycle",
            UniverseContext {
                parameter_count: 1,
                constraints: vec![leq(param(0).succ().expect("successor"), param(0))],
            },
        ),
        (
            "long positive cycle",
            UniverseContext {
                parameter_count: 3,
                constraints: vec![
                    leq(param(0).succ().expect("successor"), param(1)),
                    leq(param(1), param(2)),
                    leq(param(2), param(0)),
                ],
            },
        ),
        // A right-hand maximum is what makes the kernel's decision a search: nothing local says which alternative bounds the left side.
        (
            "right-hand maximum",
            UniverseContext {
                parameter_count: 2,
                constraints: vec![leq(
                    Level::max([Level::constant(1), param(0)]),
                    Level::max([Level::constant(1), param(1)]),
                )],
            },
        ),
        (
            "constant bounds that cross",
            UniverseContext {
                parameter_count: 1,
                constraints: vec![
                    leq(Level::constant(3), param(0)),
                    leq(param(0).succ().expect("successor"), Level::constant(2)),
                ],
            },
        ),
        (
            "offsets that shift",
            UniverseContext {
                parameter_count: 2,
                constraints: vec![leq(
                    param(0).checked_add(2).expect("offset"),
                    param(1).checked_add(5).expect("offset"),
                )],
            },
        ),
        // Not a stronger hypothesis but a meaningless one: instantiation has nothing to substitute for `P3`.
        (
            "escaping parameter",
            UniverseContext {
                parameter_count: 1,
                constraints: vec![leq(param(3), param(0))],
            },
        ),
        // Elaboration residue a zonked module cannot contain.
        (
            "metavariable residue",
            UniverseContext {
                parameter_count: 1,
                constraints: vec![leq(Level::meta(UniverseMetaId(0)), param(0))],
            },
        ),
        // A ceiling with no floor: `P1 ≤ 0` forces `P0 + 1 ≤ 0`, which only the zero floor refutes. "Constant bounds that cross" above carries `3 ≤ P0`, whose return arc closes a negative cycle, so it is decided without that floor and cannot separate the two solvers.
        (
            "ceiling with no floor",
            UniverseContext {
                parameter_count: 2,
                constraints: vec![
                    leq(param(0).succ().expect("successor"), param(1)),
                    leq(param(1), Level::zero()),
                ],
            },
        ),
    ];

    let mut verdicts = Vec::new();
    for (label, context) in &contexts {
        let elaborator = universe_context_validate(context).is_ok();
        let kernel = context.is_closed() && curios_cert::satisfiable(&context.constraints);

        assert_eq!(
            elaborator,
            kernel,
            "the two checkers disagree about `{label}`: the elaborator says {}, the kernel says {}",
            match elaborator {
                true => "valid",
                false => "invalid",
            },
            match kernel {
                true => "valid",
                false => "invalid",
            },
        );
        verdicts.push(elaborator);
    }

    assert!(verdicts.contains(&true), "no context in the table is valid");
    assert!(
        verdicts.contains(&false),
        "no context in the table is invalid",
    );
}
