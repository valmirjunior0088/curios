//! Minimizing flexible levels: the least solution, the floor a cycle closes at, and the equations that default a level and those that do not.

use {super::test_support::*, super::*};

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
/// Mutual bounds force an equality and therefore *do* have a least solution, so leaving them open is not conservatism — it is a level that never gets solved. Witness dispatch emits exactly this shape, and every `%` slot in a module body would fail to elaborate while propagation stalled on it: each level waits for the other, and neither is defaultable because both occur above.
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

/// The same cycle, but forced above zero by a direct bound. `left` solves to 2 from that bound, leaving `2 ≤ max(1, right)`. Its upper side is not a bare atom, so cancelling the offset alone does not answer it — but `max`'s own constant of 1 cannot supply 2, so `right ≥ 2` is determined rather than guessed. Without that, the floor attempt would try `right := 0`, find `2 ≤ max(1, 0)` inconsistent, roll back, and leave `right` unsolved — surfacing as "level escapes its universe parameter context".
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

/// A cycle whose floors feed one another. `max(base, payload) ≤ payload` with `payload ≤ base` and `1 ≤ payload` has the least solution of one for both, but neither has a principal bound while the other is open, and one round of floors from zero puts `payload` at one and `base` at zero, which `payload ≤ base` refuses. Each round reads the floors the round before found, so `base` follows `payload` to one and the component closes. This is what a `!` region meets at a payload above level zero: the region's monad typed at one sort for its domain and codomain, the `Try` witness bounding the payload's level by the base's, and the payload bounding the domain from below.
#[test]
fn a_cycle_through_a_maximum_closes_at_its_least_solution() {
    let mut solver = UniverseSolver::new(0);
    let base = solver.fresh(UniverseRole::Flexible, None);
    let payload = solver.fresh(UniverseRole::Flexible, None);
    solver
        .add_leq(
            Level::max([Level::meta(base), Level::meta(payload)]),
            Level::meta(payload),
            origin("region monad"),
        )
        .unwrap();
    solver
        .add_leq(Level::meta(payload), Level::meta(base), origin("witness"))
        .unwrap();
    solver
        .add_leq(Level::constant(1), Level::meta(payload), origin("payload"))
        .unwrap();

    solver.solve_flexible().unwrap();

    assert_eq!(solver.solution(base), Some(&Level::constant(1)));
    assert_eq!(solver.solution(payload), Some(&Level::constant(1)));
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
