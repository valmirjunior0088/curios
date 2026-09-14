//! Structural equality modulo the identities a lowering mints.

use {
    super::*,
    crate::{Level, MetavarId, UniverseMetaId},
};

fn hole(id: usize) -> Term {
    Term::hole(MetavarId::from(id))
}

fn type_at(id: usize) -> Term {
    Term::type_at(Level::meta(UniverseMetaId::from(id)))
}

fn arrow(domain: Term, codomain: Term) -> Term {
    Term::func_type([(Free::local(0, Some("x")), domain)], codomain)
}

#[test]
fn two_holes_of_different_ids_are_equal_under_a_fresh_renaming() {
    assert_ne!(hole(1), hole(7), "by id they differ");
    assert!(hole(1).equal_modulo_metas(&hole(7), &mut MetaRenaming::default()));
}

/// The renaming is a bijection: one id standing at two positions on one side has to stand at two positions on the other.
#[test]
fn a_hole_shared_on_one_side_and_split_on_the_other_is_unequal() {
    let shared = arrow(hole(1), hole(1));
    let split = arrow(hole(2), hole(3));

    assert!(!shared.equal_modulo_metas(&split, &mut MetaRenaming::default()));
    assert!(!split.equal_modulo_metas(&shared, &mut MetaRenaming::default()));
    assert!(shared.equal_modulo_metas(&arrow(hole(4), hole(4)), &mut MetaRenaming::default()));
}

/// A written goal and a silent hole are two different things a lowering writes, whatever their ids.
#[test]
fn a_hole_and_a_goal_are_unequal() {
    assert!(!hole(1).equal_modulo_metas(
        &Term::goal(MetavarId::from(1)),
        &mut MetaRenaming::default()
    ));
}

/// A binding made while comparing one term holds for every later term compared under the same renaming, which is what lets one declaration's type and body share their ids.
#[test]
fn a_bound_universe_meta_is_held_to_its_binding() {
    let mut renaming = MetaRenaming::default();

    assert!(type_at(1).equal_modulo_metas(&type_at(5), &mut renaming));
    assert!(type_at(1).equal_modulo_metas(&type_at(5), &mut renaming));
    assert!(!type_at(1).equal_modulo_metas(&type_at(6), &mut renaming));
    assert!(
        !type_at(2).equal_modulo_metas(&type_at(5), &mut renaming),
        "5 is already bound on the other side"
    );
}

#[test]
fn a_structural_difference_is_unequal_under_any_renaming() {
    assert!(
        !arrow(hole(1), type_at(1))
            .equal_modulo_metas(&arrow(type_at(2), hole(2)), &mut MetaRenaming::default())
    );
    assert!(!arrow(hole(1), hole(2)).equal_modulo_metas(&hole(3), &mut MetaRenaming::default()));
}
