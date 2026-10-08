//! Which payload binders an index target set determines, and the position a value of the family carries each one at — the answer erasure reads a pinned payload back from once the constructor itself is gone.

use super::*;

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

/// `refl(@z) : (z, z)` targets its binder twice; the first position is the one read back.
#[test]
fn a_binder_targeted_twice_is_pinned_at_its_first_position() {
    let z = binder(1, "z");

    let pinned = pinned_by_targets(&[Term::free_var(&z), Term::free_var(&z)]);

    assert_eq!(pinned.position(&z), Some(0));
}

/// `mk(@x, @y) : (y, x)`: reading every binder from the first position would hand each payload the other's value.
#[test]
fn swapped_targets_pin_each_binder_at_its_own_position() {
    let (x, y) = (binder(1, "x"), binder(2, "y"));

    let pinned = pinned_by_targets(&[Term::free_var(&y), Term::free_var(&x)]);

    assert_eq!(
        (pinned.position(&x), pinned.position(&y)),
        (Some(1), Some(0))
    );
}

/// `mk(@a) : (blur(a))` mentions its binder without determining it, and the control that occurrence is not determination.
#[test]
fn a_binder_under_a_function_is_not_pinned() {
    let (a, blur) = (binder(1, "a"), binder(2, "blur"));

    let pinned = pinned_by_targets(&[Term::apply(Term::free_var(&blur), vec![Term::free_var(&a)])]);

    assert!(!pinned.contains(&a));
}

/// `refl(@z) : (z, z)` holds nothing its indices do not state, and neither does a constructor with no payload.
#[test]
fn a_constructor_whose_every_payload_is_a_target_is_wholly_pinned() {
    let z = binder(1, "z");

    assert!(wholly_pinned(
        &[z],
        &[Term::free_var(&z), Term::free_var(&z)]
    ));
    assert!(wholly_pinned(&[], &[]));
}

/// `intro(@x, below) : (x)` pins `x` and holds `below` beside it, which no index states: a proof is no exception.
#[test]
fn a_payload_beside_a_pinned_one_is_not_wholly_pinned() {
    let (x, below) = (binder(1, "x"), binder(2, "below"));

    assert!(!wholly_pinned(&[x, below], &[Term::free_var(&x)]));
}
