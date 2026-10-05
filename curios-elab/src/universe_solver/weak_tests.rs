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
