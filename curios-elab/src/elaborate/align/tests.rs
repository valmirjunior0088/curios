//! The alignment rule on the examples it is stated with.

use {
    super::*,
    curios_common::Plicity::{self, Explicit, Implicit, Witness},
};

/// `(@A: Type, use Show(A), @B: Type, use Show(B), a: A, b: B)`.
const TWO: [Plicity; 6] = [Implicit, Witness, Implicit, Witness, Explicit, Explicit];

/// `(@A: Type, use Show(A), values: List(A))`.
const JOIN: [Plicity; 3] = [Implicit, Witness, Explicit];

#[test]
fn hidden_members_may_all_be_left_out() {
    assert_eq!(
        align(&TWO, &[Explicit, Explicit]),
        Ok(vec![None, None, None, None, Some(0), Some(1)])
    );
    assert_eq!(align(&JOIN, &[Explicit]), Ok(vec![None, None, Some(0)]));
}

#[test]
fn hidden_members_are_written_from_the_first_of_their_run() {
    assert_eq!(
        align(&TWO, &[Implicit, Explicit, Explicit]),
        Ok(vec![Some(0), None, None, None, Some(1), Some(2)])
    );
    assert_eq!(
        align(&TWO, &[Implicit, Witness, Implicit, Explicit, Explicit]),
        Ok(vec![Some(0), Some(1), Some(2), None, Some(3), Some(4)])
    );
    assert_eq!(
        align(&JOIN, &[Implicit, Witness, Explicit]),
        Ok(vec![Some(0), Some(1), Some(2)])
    );
}

/// The member written first fills the first slot of the run whatever it was meant for: there is no claiming the next slot of a mark.
#[test]
fn a_member_fills_its_position_in_the_run_and_not_the_next_slot_of_its_mark() {
    assert_eq!(
        align(&TWO, &[Implicit, Explicit, Explicit]).map(|fills| fills[0]),
        Ok(Some(0))
    );
    assert_eq!(
        align(&TWO, &[Witness, Explicit, Explicit]),
        Err(Misaligned::Mark { member: 0, slot: 0 })
    );
    assert_eq!(
        align(&JOIN, &[Witness, Explicit]),
        Err(Misaligned::Mark { member: 0, slot: 0 })
    );
}

#[test]
fn a_hidden_member_after_the_plain_one_it_precedes_has_no_slot() {
    assert_eq!(
        align(&JOIN, &[Explicit, Witness]),
        Err(Misaligned::Surplus { member: 1 })
    );
    assert_eq!(
        align(&JOIN, &[Implicit, Witness, Witness, Explicit]),
        Err(Misaligned::Surplus { member: 2 })
    );
}

/// Hidden slots after the last plain one are a run like any other.
#[test]
fn a_trailing_run_is_written_from_its_first_slot() {
    let tail = [Explicit, Implicit, Witness];

    assert_eq!(align(&tail, &[Explicit]), Ok(vec![Some(0), None, None]));
    assert_eq!(
        align(&tail, &[Explicit, Implicit]),
        Ok(vec![Some(0), Some(1), None])
    );
    assert_eq!(
        align(&tail, &[Explicit, Witness]),
        Err(Misaligned::Mark { member: 1, slot: 1 })
    );
    assert_eq!(
        align(&tail, &[Explicit, Implicit, Witness, Implicit]),
        Err(Misaligned::Surplus { member: 3 })
    );
}

/// A hidden member with no slot left in its run stands at the plain slot the plain members before it have reached, or past them all.
#[test]
fn a_surplus_hidden_member_stands_at_the_plain_slot_reached() {
    assert_eq!(
        stands_at(&JOIN, &[Implicit, Witness, Witness, Explicit], 2),
        Some(2)
    );
    assert_eq!(stands_at(&TWO, &[Explicit, Implicit, Explicit], 1), Some(5));
    assert_eq!(stands_at(&JOIN, &[Explicit, Witness], 1), None);
}

#[test]
fn plain_members_are_exactly_the_plain_slots() {
    assert_eq!(align(&JOIN, &[]), Err(Misaligned::Missing));
    assert_eq!(
        align(&JOIN, &[Explicit, Explicit]),
        Err(Misaligned::Extra { member: 1 })
    );
}
