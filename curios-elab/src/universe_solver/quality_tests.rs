//! Quality metavariables: what unifying two decides, what a rollback takes back, and what a closing leaves.

use {
    super::UniverseSolver,
    curios_core::{Quality, QualityMetaId, QualityParam},
};

/// The solver of a declaration that wrote `count` sorts, with the quality of each.
fn writing(count: usize) -> (UniverseSolver, Vec<Quality>) {
    let mut solver = UniverseSolver::new(0);
    solver.seed_qualities(&vec![None; count]);
    let written = (0..count)
        .map(|index| Quality::Meta(QualityMetaId(index)))
        .collect();

    (solver, written)
}

#[test]
fn an_unsolved_quality_is_solved_to_the_one_it_is_held_to() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);

    assert!(solver.unify_qualities(quality, Quality::Prop, None));
    assert_eq!(solver.quality(quality), Quality::Prop);
}

#[test]
fn two_qualities_that_state_different_sorts_are_not_held_one() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);

    assert!(solver.unify_qualities(quality, Quality::Prop, None));
    assert!(!solver.unify_qualities(quality, Quality::Type, None));
    assert!(!solver.unify_qualities(Quality::Type, Quality::Prop, None));
    assert_eq!(solver.quality(quality), Quality::Prop);
}

#[test]
fn two_unsolved_qualities_held_one_are_decided_together() {
    let mut solver = UniverseSolver::new(0);
    let this = solver.fresh_quality(None);
    let that = solver.fresh_quality(None);

    assert!(solver.unify_qualities(this, that, None));
    assert!(solver.unify_qualities(that, Quality::Prop, None));
    assert_eq!(solver.quality(this), Quality::Prop);
}

#[test]
fn a_rollback_takes_back_the_qualities_solved_since_its_mark() {
    let mut solver = UniverseSolver::new(0);
    let before = solver.fresh_quality(None);
    let after = solver.fresh_quality(None);
    assert!(solver.unify_qualities(before, Quality::Type, None));

    let mark = solver.mark();
    assert!(solver.unify_qualities(after, Quality::Prop, None));
    solver.rollback(mark);
    solver.release(mark);

    assert_eq!(solver.quality(before), Quality::Type);
    assert_eq!(solver.quality(after), after);
    assert!(solver.unify_qualities(after, Quality::Type, None));
}

#[test]
fn a_quality_nothing_decided_closes_as_a_types() {
    let mut solver = UniverseSolver::new(0);
    let decided = solver.fresh_quality(None);
    let open = solver.fresh_quality(None);
    assert!(solver.unify_qualities(decided, Quality::Prop, None));

    solver.default_qualities();

    assert_eq!(solver.quality(decided), Quality::Prop);
    assert_eq!(solver.quality(open), Quality::Type);
}

#[test]
fn solving_a_quality_moves_the_state_a_cache_is_filed_under() {
    let mut solver = UniverseSolver::new(0);
    let quality = solver.fresh_quality(None);
    let before = solver.state_token();

    assert!(solver.unify_qualities(quality, Quality::Prop, None));

    assert!(!solver.state_token().requires_the_same_as(&before));
}

#[test]
fn a_written_sort_nothing_decides_closes_as_a_parameter() {
    let (mut solver, written) = writing(2);

    assert_eq!(solver.generalize_qualities(), Ok(2));
    assert_eq!(solver.quality(written[0]), Quality::Param(QualityParam(0)));
    assert_eq!(solver.quality(written[1]), Quality::Param(QualityParam(1)));
}

#[test]
fn two_written_sorts_held_one_close_as_one_parameter() {
    let (mut solver, written) = writing(2);
    assert!(solver.unify_qualities(written[0], written[1], None));

    assert_eq!(solver.generalize_qualities(), Ok(1));
    assert_eq!(solver.quality(written[0]), Quality::Param(QualityParam(0)));
    assert_eq!(solver.quality(written[1]), Quality::Param(QualityParam(0)));
}

#[test]
fn an_invented_quality_held_to_a_written_one_is_its_parameter() {
    let (mut solver, written) = writing(1);
    let invented = solver.fresh_quality(None);
    assert!(solver.unify_qualities(written[0], invented, None));

    assert_eq!(solver.generalize_qualities(), Ok(1));
    assert_eq!(solver.quality(invented), Quality::Param(QualityParam(0)));
}

#[test]
fn an_invented_quality_alone_is_no_parameter() {
    let mut solver = UniverseSolver::new(0);
    let invented = solver.fresh_quality(None);

    assert_eq!(solver.generalize_qualities(), Ok(0));
    assert_eq!(solver.quality(invented), invented);
}

#[test]
fn a_written_sort_its_declaration_decides_is_refused_with_what_it_is() {
    let (mut solver, written) = writing(1);
    let between = solver.fresh_quality(None);
    assert!(solver.unify_qualities(written[0], between, None));
    assert!(solver.unify_qualities(between, Quality::Prop, None));

    let decided = solver
        .generalize_qualities()
        .expect_err("the declaration decided the sort it wrote");
    assert_eq!(decided.sort, Quality::Prop);
}

#[test]
fn the_sorts_an_occurrence_states_are_pinned_to_its_owners_parameters() {
    let mut solver = UniverseSolver::new(0);
    let (first, second) = (solver.fresh_quality(None), solver.fresh_quality(None));

    assert!(solver.pin_qualities(&[first, second]).is_ok());
    assert_eq!(solver.quality(first), Quality::Param(QualityParam(0)));
    assert_eq!(solver.quality(second), Quality::Param(QualityParam(1)));

    // One already decided stands for no parameter of the owner.
    let decided = solver.fresh_quality(None);
    assert!(solver.unify_qualities(decided, Quality::Type, None));
    assert!(solver.pin_qualities(&[decided]).is_err());
}

#[test]
fn a_record_over_two_written_sorts_is_found_where_both_close_as_parameters() {
    let (mut solver, written) = writing(2);
    solver.note_quality_join(written[0], written[1], None);

    assert_eq!(solver.generalize_qualities(), Ok(2));
    assert_eq!(solver.open_quality_join(), Some(None));
}

#[test]
fn a_record_over_two_written_sorts_held_one_is_over_one() {
    let (mut solver, written) = writing(2);
    solver.note_quality_join(written[0], written[1], None);
    assert!(solver.unify_qualities(written[0], written[1], None));

    assert_eq!(solver.generalize_qualities(), Ok(1));
    assert_eq!(solver.open_quality_join(), None);
}

#[test]
fn a_rollback_forgets_the_records_noted_since_its_mark() {
    let (mut solver, written) = writing(2);

    let mark = solver.mark();
    solver.note_quality_join(written[0], written[1], None);
    solver.rollback(mark);
    solver.release(mark);

    assert_eq!(solver.generalize_qualities(), Ok(2));
    assert_eq!(solver.open_quality_join(), None);
}
