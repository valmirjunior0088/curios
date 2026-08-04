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

/// The zero floor, which no clause states: a parameter ranges over the naturals, so `P0 + 1 ≤ P1` together with `P1 ≤ 0` has no model even though the difference graph closes no cycle without it.
///
/// [`bound`] reads non-negativity *locally* — a lower part whose offset exceeds a constant upper part is `Impossible` — which decides `P0 + 1 ≤ 0` written as one constraint and nothing that reaches the same conclusion through another parameter. The graph carried no `0 - head ≤ 0` arc of its own, so `Zero` was free to sit above the parameter bounding it and the search found a model over the integers.
///
/// This is the direction this module's own note names unsound. `recheck_module_verdicts` decides every context before assuming any, precisely because an unsatisfiable set is a hypothesis set from which [`entails`](crate::entails) proves whatever it is asked, and this predicate is the whole of that gate. `curios-elab`'s solver has grounded its heads all along, so the two disagreed with the certifier the more permissive of them — and `both_checkers_decide_universe_context_validity_alike` could not see it, because every context in its table that a ceiling makes contradictory also carries a floor (`3 ≤ P0`) supplying the return arc that closes the cycle.
///
/// Verified while the hole was open by calling `satisfiable` on the witness directly: it answered `true`. No `.crs` reaches it — a recorded context is the residue of generalization after the elaborator's solver found a model, and `universe_context_validate` refuses anything else before it becomes part of a module — so this fixture is the demonstration rather than a program.
///
/// The control is one `succ` away from the witness and must stay satisfiable at `P0 = P1 = 0`: grounding the heads may not become a refusal of every parameter a constant bounds from above.
#[test]
fn a_parameter_forced_below_zero_through_another_is_unsatisfiable() {
    let (u, v) = (param(0), param(1));
    let raised = u.succ().expect("level has a successor");

    assert!(!satisfiable(&[
        leq(raised, v.clone()),
        leq(v.clone(), Level::zero())
    ]));
    assert!(satisfiable(&[leq(u, v.clone()), leq(v, Level::zero())]));
}
