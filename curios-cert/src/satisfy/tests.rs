//! Whether a constraint set has a model in the naturals: the shapes each rule of the decision meets, and the decision held to a brute-force search for a model.

use {
    super::*,
    curios_core::{
        Level, LevelHead, UniverseConstraintKind, UniverseConstraintOrigin, UniverseParam,
    },
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

/// The shape a disjunctive reading has to branch on.
///
/// `max(1, P0) ≤ max(1, P1)` bounds `P0` by either `P1` or the constant, and nothing local decides which — this is the residue `/std/Fmt/go_with` produces, so it is reached by real code rather than only by fixtures. Read as a clause, the maximum is derived rather than chosen, and no branch is taken.
#[test]
fn a_right_hand_maximum_is_decided_without_branching() {
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

/// The zero floor, which no clause states: a parameter ranges over the naturals, so `P0 + 1 ≤ P1` together with `P1 ≤ 0` has no model, though no constraint says so on its own.
///
/// The decision this replaced once read non-negativity only *locally* — a lower part whose offset exceeded a constant upper part was impossible — which decided `P0 + 1 ≤ 0` written as one constraint and nothing reaching the same conclusion through another parameter, and it found a model over the integers. Forward, the zero is the model's floor, below every head, so the pair derives `0 + 1 ≤ 0` through `P0` and `P1`, which is a loop.
///
/// This is the direction this module's own note names unsound. `recheck_module_verdicts` decides every context before assuming any, precisely because an unsatisfiable set is a hypothesis set from which [`entails`](crate::entails) proves whatever it is asked, and this predicate is the whole of that gate. `curios-elab`'s solver has grounded its heads all along, so while the hole was open the two disagreed with the certifier the more permissive of them — and `both_checkers_decide_universe_context_validity_alike` could not see it, because every context in its table that a ceiling makes contradictory also carries a floor (`3 ≤ P0`) closing the loop by another route.
///
/// No `.crs` reaches it — a recorded context is the residue of generalization after the elaborator's solver found a model, and `universe_context_validate` refuses anything else before it becomes part of a module — so this fixture is the demonstration rather than a program.
///
/// The control is one `succ` away from the witness and must stay satisfiable at `P0 = P1 = 0`: the zero below every head may not become a refusal of every parameter a constant bounds from above.
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

/// Every level over two parameters with a constant below three and offsets below three.
fn levels() -> Vec<Level> {
    let mut levels = Vec::new();
    for constant in 0..3u32 {
        for first in [None, Some(0), Some(1), Some(2)] {
            for second in [None, Some(0), Some(1), Some(2)] {
                let mut parts = vec![Level::constant(constant)];
                for (index, offset) in [(0usize, first), (1usize, second)] {
                    if let Some(offset) = offset {
                        parts.push(
                            param(index)
                                .checked_add(offset)
                                .expect("the offset is small"),
                        );
                    }
                }
                levels.push(Level::max(parts));
            }
        }
    }
    levels
}

/// The single constraints over [`levels`], and the pairs drawn from those whose constants and offsets stay below two — enough for a loop that needs two constraints, through the zero, a maximum or an offset, as well as one stated outright.
fn constraint_sets() -> Vec<Vec<UniverseConstraint>> {
    let all = levels();
    let mut sets = all
        .iter()
        .flat_map(|lower| {
            all.iter()
                .map(move |upper| vec![leq(lower.clone(), upper.clone())])
        })
        .collect::<Vec<_>>();
    let pool = all
        .iter()
        .filter(|level| level.constant_part() < 2 && level.atoms().all(|(_, offset)| offset < 2))
        .collect::<Vec<_>>();
    let constraints = pool
        .iter()
        .flat_map(|lower| {
            pool.iter()
                .map(|upper| leq((*lower).clone(), (*upper).clone()))
        })
        .collect::<Vec<_>>();
    for first in &constraints {
        for second in &constraints {
            sets.push(vec![first.clone(), second.clone()]);
        }
    }
    sets
}

/// `level` under `assignment`.
fn value(level: &Level, assignment: &[u32]) -> u32 {
    level
        .atoms()
        .map(|(head, offset)| match head {
            LevelHead::Param(param) => assignment[param.0] + offset,
            LevelHead::Meta(_) => unreachable!("the sweep writes parameters only"),
        })
        .chain([level.constant_part()])
        .max()
        .unwrap_or(0)
}

/// Whether some assignment of `0..=10` to the two parameters satisfies `constraints` — room for every model the sets above need, since a model is read off the least model, whose values stay under a ceiling of the largest offset raised once per head by the largest gain.
fn has_small_model(constraints: &[UniverseConstraint]) -> bool {
    (0..=10).any(|first| {
        (0..=10).any(|second| {
            let assignment = [first, second];
            constraints.iter().all(|constraint| {
                value(&constraint.lower, &assignment) <= value(&constraint.upper, &assignment)
            })
        })
    })
}

/// The decision against the naturals themselves, in both directions: a set is called satisfiable exactly when a brute-force search finds a model.
///
/// The counts at the end keep it honest: both answers must occur, or the sweep would be testing a constant.
#[test]
fn a_set_is_satisfiable_exactly_when_it_has_a_model_in_the_naturals() {
    let mut answers = [0usize; 2];
    for constraints in constraint_sets() {
        let decided = satisfiable(&constraints);
        assert_eq!(
            decided,
            has_small_model(&constraints),
            "{constraints:?} was decided {decided}",
        );
        answers[usize::from(decided)] += 1;
    }

    assert!(answers[0] > 0 && answers[1] > 0, "{answers:?}");
}
