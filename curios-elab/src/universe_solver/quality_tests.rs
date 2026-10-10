//! Quality metavariables: what unifying two decides, what a rollback takes back, and what a closing leaves.

use {super::UniverseSolver, curios_core::Quality};

#[test]
fn an_unsolved_quality_is_solved_to_the_one_it_is_held_to() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);

    assert!(solver.unify_qualities(quality, Quality::Prop));
    assert_eq!(solver.quality(quality), Quality::Prop);
}

#[test]
fn two_qualities_that_state_different_sorts_are_not_held_one() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);

    assert!(solver.unify_qualities(quality, Quality::Prop));
    assert!(!solver.unify_qualities(quality, Quality::Type));
    assert!(!solver.unify_qualities(Quality::Type, Quality::Prop));
    assert_eq!(solver.quality(quality), Quality::Prop);
}

#[test]
fn two_unsolved_qualities_held_one_are_decided_together() {
    let mut solver = UniverseSolver::new(0);
    let this = solver.fresh_quality(None);
    let that = solver.fresh_quality(None);

    assert!(solver.unify_qualities(this, that));
    assert!(solver.unify_qualities(that, Quality::Prop));
    assert_eq!(solver.quality(this), Quality::Prop);
}

#[test]
fn a_rollback_takes_back_the_qualities_solved_since_its_mark() {
    let mut solver = UniverseSolver::new(0);
    let before = solver.fresh_quality(None);
    let after = solver.fresh_quality(None);
    assert!(solver.unify_qualities(before, Quality::Type));

    let mark = solver.mark();
    assert!(solver.unify_qualities(after, Quality::Prop));
    solver.rollback(mark);
    solver.release(mark);

    assert_eq!(solver.quality(before), Quality::Type);
    assert_eq!(solver.quality(after), after);
    assert!(solver.unify_qualities(after, Quality::Type));
}

#[test]
fn a_quality_nothing_decided_closes_as_a_types() {
    let mut solver = UniverseSolver::new(0);
    let decided = solver.fresh_quality(None);
    let open = solver.fresh_quality(None);
    assert!(solver.unify_qualities(decided, Quality::Prop));

    solver.default_qualities();

    assert_eq!(solver.quality(decided), Quality::Prop);
    assert_eq!(solver.quality(open), Quality::Type);
}

#[test]
fn solving_a_quality_moves_the_state_a_cache_is_filed_under() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);
    let before = solver.state_token();

    assert!(solver.unify_qualities(quality, Quality::Prop));

    assert!(!solver.state_token().requires_the_same_as(&before));
}
