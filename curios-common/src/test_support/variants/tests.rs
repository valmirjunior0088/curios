//! What an enum's listed variants answer.

use crate::{test_support::Variants, variants};

/// One variant of each kind, holding nothing a test reads.
enum Shapes {
    Unit,
    Tuple(()),
    Struct {},
}

variants!(Shapes {
    Unit,
    Tuple,
    Struct
});

#[test]
fn every_kind_of_variant_is_named_as_it_is_declared() {
    assert_eq!(Shapes::NAMES, ["Unit", "Tuple", "Struct"]);
    assert_eq!(Shapes::Unit.variant_name(), "Unit");
    assert_eq!(Shapes::Tuple(()).variant_name(), "Tuple");
    assert_eq!(Shapes::Struct {}.variant_name(), "Struct");
}
