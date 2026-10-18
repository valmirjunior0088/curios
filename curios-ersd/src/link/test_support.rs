//! Fixtures the link tests and the taken-suffix tests share: a family declared in one fragment and imported by another.

use crate::*;

/// A fragment declaring the family `Digit` of `count` constructors without a payload, and nothing else, with the constructors in their order.
pub(super) fn digits(count: usize) -> (Module, FamilyId, Vec<ConstructorId>) {
    let mut builder = ErsdBuilder::new();
    let family = builder.family(Some("Digit".into()));
    let constructors = (0..count)
        .map(|index| builder.constructor(family, Some(format!("d{index}")), Vec::new()))
        .collect();

    (
        builder.into_module().expect("the fragment verifies"),
        family,
        constructors,
    )
}

/// A fragment importing `Digit`, of `count` constructors, and constructing the one at `position`, bound as its one item.
pub(super) fn digit(count: usize, position: usize) -> (Module, FamilyId) {
    let mut builder = ErsdBuilder::new();
    let family = builder.import_family(Some("Digit".into()), count);
    let constructor = builder.import_constructor(family, position, Some(format!("d{position}")), 0);
    builder.item_value(
        None,
        Rhs::Construct {
            constructor,
            fields: Vec::new(),
        },
    );

    (
        builder.into_module().expect("the fragment verifies"),
        family,
    )
}
