//! Held levels: what a signature holds where conversion compares it stays the declaration's parameter with its bounds, where a level nothing holds settles or is minimized.

use super::*;

fn conversion() -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion)
}

/// The level of one occurrence: what a use of a declaration taking one universe parameter mints.
fn occurrence(solver: &mut UniverseSolver) -> UniverseMetaId {
    let scheme = UniverseContext {
        parameter_count: 1,
        constraints: Vec::new(),
    };
    let instance = solver.instantiate(&scheme).unwrap();

    bare_meta(&instance[0]).expect("an instance's level is one bare meta")
}

fn bounds(context: &UniverseContext) -> Vec<(Level, Level)> {
    context
        .constraints
        .iter()
        .map(|constraint| (constraint.lower.clone(), constraint.upper.clone()))
        .collect()
}

// An occurrence's level bounded from below settles at the bound: where nothing compares it, a parameter there offers a caller nothing. Held, it is the declaration's parameter and the bound is the declaration's constraint, so a caller that needs it higher says so by instantiating it.
#[test]
fn a_held_occurrence_keeps_its_bound_where_one_nothing_holds_settles_at_it() {
    for held in [false, true] {
        let mut solver = UniverseSolver::new(0);
        let level = occurrence(&mut solver);
        solver
            .add_leq(Level::constant(1), Level::meta(level), conversion())
            .unwrap();

        let context = solver.finalize([level], [], held.then_some(level)).unwrap();

        match held {
            true => {
                assert_eq!(context.parameter_count, 1);
                assert_eq!(
                    bounds(&context),
                    [(Level::constant(1), Level::param(UniverseParam(0)))]
                );
            }
            false => {
                assert_eq!(context.parameter_count, 0);
                assert_eq!(
                    solver.zonk(&Level::meta(level)).unwrap(),
                    Level::constant(1)
                );
            }
        }
    }
}

// A `Type` written outside an input position is a classifier, and takes its least solution. One the signature holds where conversion compares it, a result's tuple field or a former's argument, is no classifier: it is a parameter.
#[test]
fn a_held_classifier_is_not_minimized() {
    for (held, parameters) in [(false, 0), (true, 1)] {
        let mut solver = UniverseSolver::new(0);
        let level = solver.fresh(UniverseRole::Flexible, None);

        let context = solver.finalize([level], [], held.then_some(level)).unwrap();

        assert_eq!(context.parameter_count, parameters, "held: {held}");
    }
}

// The level a signature mentions may have been identified with another before the declaration closes, and the one that survives is not the signature's choice. A position is followed to the level that carries it, so the class is held whichever member that is.
#[test]
fn a_held_level_is_followed_to_the_level_that_carries_it() {
    for (held, parameters) in [(false, 0), (true, 1)] {
        let mut solver = UniverseSolver::new(0);
        let written = solver.fresh(UniverseRole::Flexible, None);
        let carrier = occurrence(&mut solver);
        solver.assign(written, Level::meta(carrier)).unwrap();
        solver
            .add_leq(Level::constant(1), Level::meta(carrier), conversion())
            .unwrap();

        let context = solver
            .finalize([written], [carrier], held.then_some(written))
            .unwrap();

        assert_eq!(context.parameter_count, parameters, "held: {held}");
    }
}

// Holding a level takes nothing from one it bounds: an occurrence above a held level still settles at it, and the two are one parameter.
#[test]
fn an_occurrence_above_a_held_level_settles_at_it() {
    let mut solver = UniverseSolver::new(0);
    let held = occurrence(&mut solver);
    let above = occurrence(&mut solver);
    solver
        .add_leq(Level::meta(held), Level::meta(above), conversion())
        .unwrap();

    let context = solver.finalize([held, above], [], [held]).unwrap();

    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
}
