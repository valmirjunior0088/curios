//! The alignment rule on the examples it is stated with.

use {
    super::{Misaligned, align},
    curios_utilities::Plicity::{self, Explicit as P, Implicit as A, Witness as U},
};

/// `(@A: Type, use Show(A), @B: Type, use Show(B), a: A, b: B)`.
const TWO: [Plicity; 6] = [A, U, A, U, P, P];

/// `(@A: Type, use Show(A), values: List(A))`.
const JOIN: [Plicity; 3] = [A, U, P];

#[test]
fn hidden_members_may_all_be_left_out() {
    assert_eq!(
        align(&TWO, &[P, P]),
        Ok(vec![None, None, None, None, Some(0), Some(1)])
    );
    assert_eq!(align(&JOIN, &[P]), Ok(vec![None, None, Some(0)]));
}

#[test]
fn hidden_members_are_written_from_the_first_of_their_run() {
    assert_eq!(
        align(&TWO, &[A, P, P]),
        Ok(vec![Some(0), None, None, None, Some(1), Some(2)])
    );
    assert_eq!(
        align(&TWO, &[A, U, A, P, P]),
        Ok(vec![Some(0), Some(1), Some(2), None, Some(3), Some(4)])
    );
    assert_eq!(
        align(&JOIN, &[A, U, P]),
        Ok(vec![Some(0), Some(1), Some(2)])
    );
}

/// The member written first fills the first slot of the run whatever it was meant for: there is no claiming the next slot of a mark.
#[test]
fn a_member_fills_its_position_in_the_run_and_not_the_next_slot_of_its_mark() {
    assert_eq!(align(&TWO, &[A, P, P]).map(|fills| fills[0]), Ok(Some(0)));
    assert_eq!(
        align(&TWO, &[U, P, P]),
        Err(Misaligned::Mark { member: 0, slot: 0 })
    );
    assert_eq!(
        align(&JOIN, &[U, P]),
        Err(Misaligned::Mark { member: 0, slot: 0 })
    );
}

#[test]
fn a_hidden_member_after_the_plain_one_it_precedes_has_no_slot() {
    assert_eq!(
        align(&JOIN, &[P, U]),
        Err(Misaligned::Surplus { member: 1 })
    );
    assert_eq!(
        align(&JOIN, &[A, U, U, P]),
        Err(Misaligned::Surplus { member: 2 })
    );
}

/// Hidden slots after the last plain one are a run like any other.
#[test]
fn a_trailing_run_is_written_from_its_first_slot() {
    let tail = [P, A, U];

    assert_eq!(align(&tail, &[P]), Ok(vec![Some(0), None, None]));
    assert_eq!(align(&tail, &[P, A]), Ok(vec![Some(0), Some(1), None]));
    assert_eq!(
        align(&tail, &[P, U]),
        Err(Misaligned::Mark { member: 1, slot: 1 })
    );
    assert_eq!(
        align(&tail, &[P, A, U, A]),
        Err(Misaligned::Surplus { member: 3 })
    );
}

#[test]
fn plain_members_are_exactly_the_plain_slots() {
    assert_eq!(align(&JOIN, &[]), Err(Misaligned::Plain));
    assert_eq!(align(&JOIN, &[P, P]), Err(Misaligned::Plain));
}
