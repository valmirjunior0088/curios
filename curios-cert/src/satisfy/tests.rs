use {
    super::*,
    curios_core::{UniverseConstraintKind, UniverseConstraintOrigin, UniverseParam},
};

fn param(index: usize) -> Level {
    Level::param(UniverseParam(index))
}

fn leq(lower: Level, upper: Level) -> UniverseConstraint {
    UniverseConstraint {
        lower,
        upper,
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    }
}

#[test]
fn the_empty_context_is_satisfiable() {
    assert!(satisfiable(&[]));
}

/// An ordinary scheme: one parameter below another, and a successor between them.
#[test]
fn a_chain_of_parameters_is_satisfiable() {
    let (u, v, w) = (param(0), param(1), param(2));
    let raised = u.succ().expect("level has a successor");

    assert!(satisfiable(&[leq(u, v.clone()), leq(raised, w)]));
    assert!(satisfiable(&[leq(v, param(3))]));
}

/// The direct contradiction: nothing is strictly below itself.
#[test]
fn a_parameter_strictly_below_itself_is_unsatisfiable() {
    let u = param(0);
    let raised = u.succ().expect("level has a successor");

    assert!(!satisfiable(&[leq(raised, u)]));
}

/// A contradiction spread across a cycle, which no single constraint reveals.
#[test]
fn a_cycle_that_gains_a_level_is_unsatisfiable() {
    let (u, v) = (param(0), param(1));
    let raised = u.succ().expect("level has a successor");

    assert!(satisfiable(&[leq(u.clone(), v.clone())]));
    assert!(!satisfiable(&[leq(raised, v.clone()), leq(v, u)]));
}

/// The shape that forces the search to branch.
///
/// `max(1, P0) ≤ max(1, P1)` bounds `P0` by either `P1` or the constant, and nothing local decides which — this is the residue `/std/Fmt/go_with` produces, so the branching path is reached by real code rather than only by fixtures.
#[test]
fn a_right_hand_maximum_is_decided_by_branching() {
    let (u, v) = (param(0), param(1));
    let one = Level::constant(1);
    let lower = Level::max([one.clone(), u.clone()]);
    let upper = Level::max([one, v.clone()]);

    assert!(satisfiable(&[leq(lower.clone(), upper.clone())]));
    // Both directions at once still has a model: every parameter equal.
    assert!(satisfiable(&[
        leq(lower.clone(), upper.clone()),
        leq(upper, lower)
    ]));
}

/// A right-hand maximum does not rescue a contradiction that holds under every choice.
#[test]
fn a_right_hand_maximum_with_no_viable_choice_is_unsatisfiable() {
    let u = param(0);
    let raised = u.succ().expect("level has a successor");
    // `max(P0 + 1) ≤ max(P0)`: the only alternative is the one that closes the cycle.
    assert!(!satisfiable(&[leq(raised, Level::max([u]))]));
}

/// A constant is bounded by an atom only once that atom's offset covers it.
#[test]
fn a_constant_needs_the_offset_to_cover_it() {
    let u = param(0);
    let three = Level::constant(3);

    assert!(satisfiable(&[leq(
        three.clone(),
        u.checked_add(3).expect("offset")
    )]));
    // `3 ≤ P0` is satisfiable — `P0` may simply be three — where `3 ≤ 0` is not.
    assert!(satisfiable(&[leq(three.clone(), u)]));
    assert!(!satisfiable(&[leq(three, Level::zero())]));
}
