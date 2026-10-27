//! Consistency: positive cycles and contradictory bounds refused with the path and origins that show them, constraints discharged at insertion, and speculation rolled back or abandoned.

use {
    super::test_support::*,
    super::*,
    curios_core::{Anchor, Anchored, Extent, Global},
    curios_utilities::Qualifier,
};

/// A scheme stating one constraint, raised in a declaration of another unit.
fn raised_in_a_scheme() -> UniverseContext {
    let raised = UniverseConstraintOrigin {
        extent: Some(Extent {
            anchor: Anchor::of(Anchored::Entry),
            start: 0,
            end: 4,
            stamp: 0,
        }),
        kind: UniverseConstraintKind::Cumulativity,
        declaration: Some("/lib/wrap".into()),
        binder: Some("A".into()),
        instantiated: None,
    };
    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: Level::param(UniverseParam(0)),
            upper: Level::param(UniverseParam(1)),
            origin: raised,
        }],
    }
}

/// A constraint instantiated from a scheme with no occurrence in hand carries no position, and names the declaration and binder the scheme's own was raised for: where that was raised is a line of the scheme's declaration, another unit's among them.
#[test]
fn a_constraint_instantiated_from_a_scheme_carries_no_position() {
    let scheme = raised_in_a_scheme();
    let mut solver = UniverseSolver::new(0);

    solver.instantiate(&scheme, None).unwrap();

    let [instantiated] = solver.constraints() else {
        panic!("one constraint, got {:?}", solver.constraints());
    };
    assert_eq!(instantiated.origin.extent, None);
    assert_eq!(
        instantiated.origin.kind,
        UniverseConstraintKind::SchemeInstantiation
    );
    assert_eq!(
        instantiated.origin.declaration.as_deref(),
        Some("/lib/wrap")
    );
    assert_eq!(instantiated.origin.binder.as_deref(), Some("A"));
    assert_eq!(instantiated.origin.instantiated, None);
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
            Some(Occurrence {
                instantiated: used,
                extent: Some(written),
            }),
        )
        .unwrap();

    let [instantiated] = solver.constraints() else {
        panic!("one constraint, got {:?}", solver.constraints());
    };
    assert_eq!(instantiated.origin.extent, Some(written));
    assert_eq!(
        instantiated.origin.kind,
        UniverseConstraintKind::SchemeInstantiation
    );
    assert_eq!(instantiated.origin.instantiated, Some(used));
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
        // One independent generator per trial, indexed off the high bits: an LCG's low three bits cycle with period 8, and one generator threaded across trials at a step count divisible by 8 would keep every trial at the same phase.
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
                exact.check_consistent_full().map(|_| ())
            };

            // What a decided store answers of a constraint it does not hold, where it answers, is the exact verdict on the store that holds it.
            if let Some(refused) = solver.refused_as_decided(&constraint.lower, &constraint.upper) {
                assert_eq!(
                    refused,
                    exact_result.is_err(),
                    "a decided store answered otherwise in trial {trial} at step {step}"
                );
            }

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
                quality_count: 0,
                constraints: vec![leq(param(0), param(1))],
            },
        ),
        // Mutually bounded parameters are legal and satisfiable: they force equality, not a contradiction.
        (
            "cyclic but satisfiable",
            UniverseContext {
                parameter_count: 2,
                quality_count: 0,
                constraints: vec![leq(param(0), param(1)), leq(param(1), param(0))],
            },
        ),
        (
            "direct positive cycle",
            UniverseContext {
                parameter_count: 1,
                quality_count: 0,
                constraints: vec![leq(param(0).succ().expect("successor"), param(0))],
            },
        ),
        (
            "long positive cycle",
            UniverseContext {
                parameter_count: 3,
                quality_count: 0,
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
                quality_count: 0,
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
                quality_count: 0,
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
                quality_count: 0,
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
                quality_count: 0,
                constraints: vec![leq(param(3), param(0))],
            },
        ),
        // Elaboration residue a zonked module cannot contain.
        (
            "metavariable residue",
            UniverseContext {
                parameter_count: 1,
                quality_count: 0,
                constraints: vec![leq(Level::meta(UniverseMetaId(0)), param(0))],
            },
        ),
        // A ceiling with no floor: `P1 ≤ 0` forces `P0 + 1 ≤ 0`, which only the zero floor refutes. "Constant bounds that cross" above carries `3 ≤ P0`, whose return arc closes a negative cycle, so it is decided without that floor and cannot separate the two solvers.
        (
            "ceiling with no floor",
            UniverseContext {
                parameter_count: 2,
                quality_count: 0,
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

/// A refusal unwinds through brackets that never release, leaving their scopes open and the journal recording pre-images nothing will roll back to; the item boundary closes them all at once.
#[test]
fn abandoning_speculation_closes_every_scope_a_refusal_left_open() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Flexible, None);
    let _outer = solver.mark();
    let _inner = solver.mark();
    solver
        .add_leq(Level::zero(), Level::meta(u), origin("bound"))
        .unwrap();
    solver.solve_flexible().unwrap();
    assert_eq!(solver.open_scopes(), 2);

    solver.abandon_speculation();

    assert_eq!(solver.open_scopes(), 0);
    assert_eq!(solver.journal_len(), 0);
}
