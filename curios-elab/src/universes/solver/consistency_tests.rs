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

/// Every level a two-parameter context can state within three rungs: the constants, each parameter at each offset, and the maxima that mix them. The table above names eleven contexts by hand; this is the alphabet the sweep below builds every context of two and three constraints from.
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

/// Whether some assignment of naturals below `bound` satisfies every constraint — the ground truth the two decisions are held to.
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

/// Both checkers call a universe context satisfiable exactly where an assignment of naturals satisfies it.
///
/// Two tests stand beside this and neither states it. [`both_checkers_decide_universe_context_validity_alike`] above compares the two decisions to *each other* over a table written by hand, which can only hold the shapes someone thought of, and nothing in it is held to the naturals. `curios-cert`'s `satisfy::tests::a_set_is_satisfiable_exactly_when_it_has_a_model_in_the_naturals` holds the kernel's half to the naturals over singles and pairs, and the elaborator's solver — the half that *runs a search*, with a node budget it can exhaust — is no part of it.
///
/// So this holds the elaborator's decision to the naturals, over every ordered pair of [`small_levels`] as one constraint and every set of two of those, and adds the case neither reaches: sets of **three** constraints, over the narrower alphabet a loop through a right-hand maximum needs, since a maximum read the other way is a disjunction and the set that separates a conjunctive derivation from the naturals' total order wants a clause to bound each part of one. The bound on the search is well above the least model any set in these alphabets can have, whose values a clause can raise by at most the largest offset once per head.
///
/// The unsound direction is the *kernel* calling satisfiable a set no assignment satisfies: the walk assumes an item's constraints while judging it, so such a set is a hypothesis set from which `entails` derives every level relation and `check_instance` discharges nothing — the universe discipline that keeps the paradox out stops applying. That direction is asserted to be empty, and so is the kernel being the more permissive of the two. The other two disagreements are incompleteness, which surfaces as a refusal, and are asserted empty as well, so a change that trades one for the other shows here.
#[test]
fn both_checkers_agree_with_the_naturals_over_every_small_context() {
    const PARAMS: usize = 2;
    const BOUND: u64 = 12;
    let levels = small_levels();
    let constraint = |lower: &Level, upper: &Level| UniverseConstraint {
        lower: lower.clone(),
        upper: upper.clone(),
        origin: origin("sweep"),
    };

    let singles = levels
        .iter()
        .flat_map(|lower| levels.iter().map(move |upper| constraint(lower, upper)))
        .collect::<Vec<_>>();

    let mut admitted_without_a_model: Vec<Vec<UniverseConstraint>> = Vec::new();
    let mut refused_with_a_model = 0usize;
    let mut elaborator_alone = 0usize;
    let mut kernel_alone: Vec<Vec<UniverseConstraint>> = Vec::new();
    let mut sets = 0usize;

    let mut judge = |constraints: Vec<UniverseConstraint>| {
        let kernel = curios_cert::satisfiable(&constraints);
        let truth = satisfied_somewhere(&constraints, PARAMS, BOUND);
        let context = UniverseContext {
            parameter_count: PARAMS,
            quality_count: 0,
            constraints: constraints.clone(),
        };
        let elaborator = universe_context_validate(&context).is_ok();

        if kernel && !truth {
            admitted_without_a_model.push(constraints.clone());
        }
        if !kernel && truth {
            refused_with_a_model += 1;
        }
        if elaborator && !kernel {
            elaborator_alone += 1;
        }
        if kernel && !elaborator {
            kernel_alone.push(constraints);
        }
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
        admitted_without_a_model.is_empty(),
        "the kernel called {} of {sets} sets satisfiable with no assignment satisfying them, the first being {:?}",
        admitted_without_a_model.len(),
        admitted_without_a_model.first(),
    );
    assert!(
        kernel_alone.is_empty(),
        "the kernel is the more permissive of the two over {} of {sets} sets, the first being {:?}",
        kernel_alone.len(),
        kernel_alone.first(),
    );
    assert_eq!(
        (refused_with_a_model, elaborator_alone),
        (0, 0),
        "a decision refused a set an assignment satisfies, or the two disagree, over {sets} sets",
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
