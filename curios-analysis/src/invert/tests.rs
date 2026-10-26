//! Which payload binders an index target set determines, and the position a value of the family carries each one at — the answer erasure reads a pinned payload back from once the constructor itself is gone.

use {
    super::*,
    curios_core::{Global, Level},
    curios_utilities::Qualifier,
};

fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
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

/// `refl(@z) : (z, z)` at a scrutinee of `Eq()(a, b)` has `a` for its payload and builds `(a, a)`, which is the scrutinee's instance exactly where `a` and `b` are one.
#[test]
fn a_pinned_instance_reads_each_binder_off_its_first_index() {
    let (z, a, b) = (binder(1, "z"), binder(2, "a"), binder(3, "b"));

    let instance = pinned_instance(
        &[z],
        &[Term::free_var(&z), Term::free_var(&z)],
        &[Term::free_var(&a), Term::free_var(&b)],
    );

    assert_eq!(
        instance,
        Some((
            vec![Term::free_var(&a)],
            vec![Term::free_var(&a), Term::free_var(&a)]
        ))
    );
}

/// `intro(@x, below) : (x)` builds none: no index says what `below` is.
#[test]
fn a_binder_no_index_pins_builds_no_instance() {
    let (x, below, n) = (binder(1, "x"), binder(2, "below"), binder(3, "n"));

    let instance = pinned_instance(&[x, below], &[Term::free_var(&x)], &[Term::free_var(&n)]);

    assert_eq!(instance, None);
}

/// `mk(@a) : (Tag/t(a))` holds its binder under a constructor, and the walk does not descend into one: at a proposition `Tag/t(0)` and `Tag/t(7)` are one value, so the target recovers nothing, and a pinned `a` would be a payload a program reads out of what irrelevance identified. `curios`'s `tests::board::large_elimination_tests::a_payload_under_a_propositions_constructor_is_not_pinned` holds the `False` that reading reaches.
#[test]
fn a_binder_under_a_constructor_is_not_pinned() {
    let a = binder(1, "a");
    let tag = Global::Authored(Qualifier::from(["Tag"]));

    let pinned = pinned_by_targets(&[Term::variant(
        tag,
        Vec::<Term>::new(),
        "t",
        [Term::free_var(&a)],
    )]);

    assert!(!pinned.contains(&a));
}

/// `mk(@a) : (a.{0})` names its binder through a universe instance — the occurrence the board's two `through an instance` tickets were each missed at, here landing in the refusing direction. The walk reads a bare `Var` target and no `Instance` wrapping one, so an instance-wrapped target determines nothing and the arm stays mandatory: under-pinning refuses the large elimination, where over-pinning would eliminate a proposition's payload into data. A local carries no scheme, so the elaborator never spells a payload binder this way; a module built by hand can, as the board's tickets are.
#[test]
fn a_binder_under_a_universe_instance_is_not_pinned() {
    let a = binder(1, "a");
    let instanced = Term::instance_of(&a, vec![Level::zero()]);

    assert!(!pinned_by_targets(&[instanced.clone()]).contains(&a));
    assert!(!wholly_pinned(&[a], &[instanced]));
}
