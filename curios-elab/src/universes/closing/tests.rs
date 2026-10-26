//! What a declaration's closing reads off its value.

use {
    super::*,
    curios_core::{Intrinsic, Level, UniverseParam},
    curios_utilities::Qualifier,
};

fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A declaration's instance is read once per node of a value's graph, not once per path: the family occurs at the base of sixty levels that each sum the one below with itself.
#[test]
fn a_declaration_instance_is_read_once_per_node() {
    let family = nominal("Family");
    let level = Level::param(UniverseParam(0));
    let mut value = Term::induct_type_at(
        family,
        vec![level.clone()],
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    );
    for _ in 0..60 {
        value = Term::intrinsic(Intrinsic::nat_add(value.clone(), value));
    }

    assert_eq!(
        declaration_instance(&value, &family),
        Some(Universes::levels(vec![level]))
    );
}
