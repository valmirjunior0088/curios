//! Weak equations: what conversion records at a position a nominal family is irrelevant in, joined where a declaration's levels settle and dropped where the store refuses them.

use super::*;

fn conversion() -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion)
}

fn open(solver: &mut UniverseSolver) -> UniverseMetaId {
    solver.fresh(UniverseRole::Generalizable, None)
}

// Two levels nothing else relates are two parameters; met at an irrelevant position they are one, which is what an equation there gave.
#[test]
fn a_weak_equation_joins_two_open_levels_where_the_declaration_closes() {
    for (weak, parameters) in [(true, 1), (false, 2)] {
        let mut solver = UniverseSolver::new(0);
        let u = open(&mut solver);
        let v = open(&mut solver);
        if weak {
            solver.add_weak_eq(Level::meta(u), Level::meta(v));
        }

        let context = solver.finalize([u, v], [], []).unwrap();
        assert_eq!(context.parameter_count, parameters);
        assert!(context.constraints.is_empty());
    }
}

// `1 ≤ u` and a weak `u = 0`: the pair is dropped and the declaration closes with its bound. The same pair as an equation refuses it, which is the refusal a weak equation exists not to make.
#[test]
fn a_weak_equation_the_store_refuses_is_dropped() {
    let bounded = |solver: &mut UniverseSolver| {
        let u = open(solver);
        solver
            .add_leq(Level::constant(1), Level::meta(u), conversion())
            .unwrap();
        u
    };

    let mut solver = UniverseSolver::new(0);
    let u = bounded(&mut solver);
    solver.add_weak_eq(Level::meta(u), Level::zero());
    let context = solver.finalize([u], [], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert_eq!(
        context
            .constraints
            .iter()
            .map(|constraint| (constraint.lower.clone(), constraint.upper.clone()))
            .collect::<Vec<_>>(),
        vec![(Level::constant(1), Level::param(UniverseParam(0)))]
    );

    let mut solver = UniverseSolver::new(0);
    let u = bounded(&mut solver);
    solver
        .add_eq(Level::meta(u), Level::zero(), conversion())
        .unwrap();
    assert!(solver.finalize([u], [], []).is_err());
}

// The batch is refused by one pair, and the others are still joined: retaken one at a time, `v = w` holds beside the dropped `u = 0`.
#[test]
fn a_refused_weak_equation_does_not_take_the_others_with_it() {
    let mut solver = UniverseSolver::new(0);
    let u = open(&mut solver);
    let v = open(&mut solver);
    let w = open(&mut solver);
    solver
        .add_leq(Level::constant(1), Level::meta(u), conversion())
        .unwrap();
    solver.add_weak_eq(Level::meta(u), Level::zero());
    solver.add_weak_eq(Level::meta(v), Level::meta(w));

    let context = solver.finalize([u, v, w], [], []).unwrap();
    assert_eq!(context.parameter_count, 2);
    assert_eq!(
        solver.zonk(&Level::meta(v)).unwrap(),
        solver.zonk(&Level::meta(w)).unwrap()
    );
}

// Where two pairs cannot both hold, the one met first is kept: `a + 1 ≤ b` keeps `a` and `b` apart, and `u` goes with whichever it met first.
#[test]
fn of_two_weak_equations_that_cannot_both_hold_the_first_met_is_kept() {
    for first_is_lower in [true, false] {
        let mut solver = UniverseSolver::new(0);
        let u = open(&mut solver);
        let a = open(&mut solver);
        let b = open(&mut solver);
        solver
            .add_leq(Level::meta(a).succ().unwrap(), Level::meta(b), conversion())
            .unwrap();
        let (first, second) = match first_is_lower {
            true => (a, b),
            false => (b, a),
        };
        solver.add_weak_eq(Level::meta(u), Level::meta(first));
        solver.add_weak_eq(Level::meta(u), Level::meta(second));

        let context = solver.finalize([u, a, b], [], []).unwrap();
        assert_eq!(context.parameter_count, 2);
        let level = |meta| solver.zonk(&Level::meta(meta)).unwrap();
        assert_eq!(level(u), level(first));
        assert_ne!(level(u), level(second));
    }
}

// A weak equation met inside a scope that is rolled back was met by nothing that stands.
#[test]
fn a_weak_equation_is_withdrawn_with_the_scope_that_met_it() {
    let mut solver = UniverseSolver::new(0);
    let u = open(&mut solver);
    let v = open(&mut solver);
    let mark = solver.mark();
    solver.add_weak_eq(Level::meta(u), Level::meta(v));
    solver.rollback(mark);
    solver.release(mark);

    let context = solver.finalize([u, v], [], []).unwrap();
    assert_eq!(context.parameter_count, 2);
}

// Two constants, or a constant and a parameter, hold no open level: there is nothing to choose, and a pair of them neither refuses the declaration nor binds the parameter.
#[test]
fn a_pair_with_no_open_level_is_dropped_unread() {
    let mut solver = UniverseSolver::new(0);
    let u = open(&mut solver);
    solver.add_weak_eq(Level::zero(), Level::constant(1));
    solver.add_weak_eq(Level::param(UniverseParam(0)), Level::zero());

    let context = solver.finalize([u], [], []).unwrap();
    assert_eq!(context.parameter_count, 1);
    assert!(context.constraints.is_empty());
    assert_eq!(solver.constraint_count(), 0);
}

// An elaboration that met a weak equation moved the solver: where its declaration's levels settle depends on it, so whoever asks whether a computation wrote to the solver is told it did. A pair of one level is none.
#[test]
fn a_weak_equation_is_a_write_to_the_solver() {
    let mut solver = UniverseSolver::new(0);
    let u = open(&mut solver);
    let v = open(&mut solver);

    let before = solver.state_token();
    solver.add_weak_eq(Level::meta(u), Level::meta(u));
    assert_eq!(solver.state_token(), before);
    solver.add_weak_eq(Level::meta(u), Level::meta(v));
    assert_ne!(solver.state_token(), before);
}

// Two levels a declaration holds are its callers' to choose apart. A weak pair of them is dropped, and so is the second of two that would make them one through an occurrence: `u` goes with the one it met first.
#[test]
fn a_weak_equation_joins_no_two_levels_a_declaration_holds() {
    for (held, parameters) in [(true, 2), (false, 1)] {
        let mut solver = UniverseSolver::new(0);
        let a = open(&mut solver);
        let b = open(&mut solver);
        solver.add_weak_eq(Level::meta(a), Level::meta(b));
        let compared = if held { vec![a, b] } else { Vec::new() };
        let context = solver.finalize([a, b], [], compared).unwrap();
        assert_eq!(context.parameter_count, parameters, "held: {held}");
        assert!(context.constraints.is_empty());

        let mut solver = UniverseSolver::new(0);
        let a = open(&mut solver);
        let b = open(&mut solver);
        let u = open(&mut solver);
        solver.add_weak_eq(Level::meta(u), Level::meta(a));
        solver.add_weak_eq(Level::meta(u), Level::meta(b));
        let compared = if held { vec![a, b] } else { Vec::new() };
        let context = solver.finalize([a, b, u], [], compared).unwrap();
        assert_eq!(context.parameter_count, parameters, "held: {held}");
        assert!(context.constraints.is_empty());
        let level = |meta| solver.zonk(&Level::meta(meta)).unwrap();
        assert_eq!(level(u), level(a));
    }
}

// A join that keeps two held levels apart can still bound one by the other, or by a constant, through what the occurrence it joins already stood above. That bound is one a caller would have to meet for a position nothing reads, so the pair is dropped and the occurrence settles from its own bounds.
#[test]
fn a_weak_equation_puts_no_bound_on_a_level_a_declaration_holds() {
    for bound_is_held in [true, false] {
        let mut solver = UniverseSolver::new(0);
        let a = open(&mut solver);
        let b = open(&mut solver);
        let u = open(&mut solver);
        let below = match bound_is_held {
            true => Level::meta(b),
            false => Level::constant(1),
        };
        solver.add_leq(below, Level::meta(u), conversion()).unwrap();
        solver.add_weak_eq(Level::meta(u), Level::meta(a));

        let context = solver.finalize([a, b], [u], [a, b]).unwrap();
        assert_eq!(context.parameter_count, 2, "{bound_is_held}");
        assert!(context.constraints.is_empty(), "{bound_is_held}");
    }
}

// A bound the store already holds between two held levels is no new one: the join that restates it is kept.
#[test]
fn a_weak_equation_joins_what_the_held_levels_already_bound() {
    let mut solver = UniverseSolver::new(0);
    let a = open(&mut solver);
    let b = open(&mut solver);
    let u = open(&mut solver);
    let above = Level::meta(b).succ().unwrap();
    solver
        .add_leq(above.clone(), Level::meta(a), conversion())
        .unwrap();
    solver.add_leq(above, Level::meta(u), conversion()).unwrap();
    solver.add_weak_eq(Level::meta(u), Level::meta(a));

    let context = solver.finalize([a, b, u], [], [a, b]).unwrap();
    assert_eq!(context.parameter_count, 2);
    assert_eq!(context.constraints.len(), 1);
    let level = |meta| solver.zonk(&Level::meta(meta)).unwrap();
    assert_eq!(level(u), level(a));
}
