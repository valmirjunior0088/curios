//! Generalizing a declaration's levels into its scheme: deterministic and closed, forced equalities shared, levels bounded by zero, and residual parameters.

use {super::test_support::*, super::*, std::collections::BTreeSet};

#[test]
fn contexts_are_alpha_stable_and_instances_are_fresh() {
    let context = UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![UniverseConstraint {
            lower: Level::param(UniverseParam(0)).succ().unwrap(),
            upper: Level::param(UniverseParam(1)),
            origin: origin("residual"),
        }],
    };
    universe_context_validate(&context).unwrap();
    let mut solver = UniverseSolver::new(7);
    let first = solver.instantiate(&context, None).unwrap();
    let second = solver.instantiate(&context, None).unwrap();
    let first_metas = first
        .levels
        .iter()
        .flat_map(Level::metas)
        .collect::<BTreeSet<_>>();
    let second_metas = second
        .levels
        .iter()
        .flat_map(Level::metas)
        .collect::<BTreeSet<_>>();
    assert!(first_metas.is_disjoint(&second_metas));
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

    let context = solver.finalize([u, v], [], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
}

/// `u ≤ 0` holds over the naturals only at zero, so `u` is the constant rather than a parameter every caller would have to supply as zero; what it bounded keeps the bound, `u + 1 ≤ v` becoming `1 ≤ v`.
#[test]
fn a_level_bounded_by_zero_generalizes_to_the_constant() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(Level::meta(u), Level::zero(), origin("pinned"))
        .unwrap();
    solver
        .add_leq(
            Level::meta(u).succ().unwrap(),
            Level::meta(v),
            origin("above"),
        )
        .unwrap();

    let context = solver.finalize([u, v], [], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    let bounds = context
        .constraints
        .iter()
        .map(|constraint| (constraint.lower.clone(), constraint.upper.clone()))
        .collect::<Vec<_>>();
    assert_eq!(
        bounds,
        vec![(Level::constant(1), Level::param(UniverseParam(0)))]
    );
    assert_eq!(solver.zonk(&Level::meta(u)).unwrap(), Level::zero());
}

/// A maximum bounded by zero holds every part at zero, which is how one constraint pins several levels at once.
#[test]
fn a_maximum_bounded_by_zero_holds_every_part_at_zero() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    let w = solver.fresh(UniverseRole::Generalizable, None);
    let both = Level::max([Level::meta(u), Level::meta(v)]);
    solver
        .add_leq(both.clone(), Level::zero(), origin("pinned"))
        .unwrap();
    solver
        .add_leq(both, Level::meta(w), origin("below"))
        .unwrap();

    let context = solver.finalize([u, v, w], [], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
    assert_eq!(solver.zonk(&Level::meta(u)).unwrap(), Level::zero());
    assert_eq!(solver.zonk(&Level::meta(v)).unwrap(), Level::zero());
}

/// `u = max(v, w)` determines `u`: it is solved to the maximum, not generalized beside the levels it is the maximum of — and `u = v + 1`, which the mutual-atom merge leaves because the offsets differ, is solved the same way.
#[test]
fn a_lone_level_equal_to_another_is_that_level() {
    let mut solver = UniverseSolver::new(0);
    let u = solver.fresh(UniverseRole::Generalizable, None);
    let v = solver.fresh(UniverseRole::Generalizable, None);
    let w = solver.fresh(UniverseRole::Generalizable, None);
    let x = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_eq(
            Level::meta(u),
            Level::max([Level::meta(v), Level::meta(w)]),
            origin("maximum"),
        )
        .unwrap();
    solver
        .add_eq(
            Level::meta(x),
            Level::meta(v).succ().unwrap(),
            origin("successor"),
        )
        .unwrap();

    let context = solver.finalize([u, v, w, x], [], []).unwrap();
    assert_eq!(context.parameter_count, 2);
    assert!(context.constraints.is_empty());
    let (first, second) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
    );
    assert_eq!(
        solver.zonk(&Level::meta(u)).unwrap(),
        Level::max([first.clone(), second])
    );
    assert_eq!(solver.zonk(&Level::meta(x)).unwrap(), first.succ().unwrap());
}

/// Minimizing can determine what the merge before it could not: with `c` a body-only level, `a ≤ b` and `b ≤ max(a, c)` say nothing about `a` and `b` until `c` is solved at zero, and then they are mutual. The merge runs again once minimizing is done, so the scheme has one parameter rather than two bound to each other.
#[test]
fn a_level_determined_once_minimizing_settles_another_is_merged() {
    let mut solver = UniverseSolver::new(0);
    let a = solver.fresh(UniverseRole::Generalizable, None);
    let b = solver.fresh(UniverseRole::Generalizable, None);
    let c = solver.fresh(UniverseRole::Generalizable, None);
    solver
        .add_leq(Level::meta(a), Level::meta(b), origin("below"))
        .unwrap();
    solver
        .add_leq(
            Level::meta(b),
            Level::max([Level::meta(a), Level::meta(c)]),
            origin("above"),
        )
        .unwrap();

    let context = solver.finalize([a, b], [c], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
}

/// The control: an equation between two maxima determines neither side — `max(a, b) = max(c, d)` holds with the four levels apart — so it stays the two constraints it is, and all four are generalized.
#[test]
fn an_equation_between_maxima_determines_neither_side() {
    let mut solver = UniverseSolver::new(0);
    let [a, b, c, d] = [(); 4].map(|()| solver.fresh(UniverseRole::Generalizable, None));
    solver
        .add_eq(
            Level::max([Level::meta(a), Level::meta(b)]),
            Level::max([Level::meta(c), Level::meta(d)]),
            origin("maxima"),
        )
        .unwrap();

    let context = solver.finalize([a, b, c, d], [], []).unwrap();
    assert_eq!(context.parameter_count, 4);
    assert_eq!(context.constraints.len(), 2);
    universe_context_validate(&context).unwrap();
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

    let context = solver.finalize([left, right], [], []).unwrap();
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
        solver.finalize([outer], [], []),
        Err(UniverseError::EscapingLevel)
    ));
}

/// A declaration is generalized over a level its signature mentions, however far conversion aliased it.
///
/// The two sets [`UniverseSolver::finalize`] partitions are computed syntactically — the interface from the declaration's type, the internal levels from its body. Conversion aliases one meta onto another whenever two spellings of a level are forced equal, and the direction is not the signature's to choose, so the interface set can name metas that were solved away while the ones still carrying the level are reached by the body walk instead. Left on the internal side they are minimized, and the declaration comes back at a ground level rather than generalized over its own parameter — which every polymorphic caller is then refused for, by the kernel, for supplying a parameter where the callee demands a constant.
///
/// A function over a struct carrying a `Type` field leaves such a chain, with the representative in the body's set. One link of following is not enough, which is what this pins — the meta a link lands on may itself be solved.
#[test]
fn an_interface_level_is_generalized_through_a_chain_of_aliases() {
    let mut solver = UniverseSolver::new(0);
    let signature = solver.fresh(UniverseRole::Generalizable, None);
    let middle = solver.fresh(UniverseRole::Generalizable, None);
    let carrier = solver.fresh(UniverseRole::Generalizable, None);

    // The chain conversion leaves behind: the signature's meta solved onward, twice.
    solver.assign(signature, Level::meta(middle)).unwrap();
    solver.assign(middle, Level::meta(carrier)).unwrap();

    // The body mentions the level too, so the walk that collects internal levels reaches the carrier.
    let context = solver.finalize([signature], [carrier], []).unwrap();

    assert_eq!(
        context.parameter_count, 1,
        "the level the signature mentions is the declaration's parameter, however far it was aliased"
    );
}
