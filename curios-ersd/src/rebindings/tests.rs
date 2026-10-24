//! Resolution keeps schema and forcing boundaries while following shared bindings.

use {
    super::*,
    crate::{Constant, ConstantId, ErsdBuilder, Field, ProductFields, ProductSchema, Terminator},
};

fn fixture(matching: bool) -> (Rebindings, Atom, Atom) {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: ProductFields::Shared(1),
    });
    let other = builder.product(ProductSchema {
        debug_name: None,
        fields: ProductFields::Declared(vec![Field::opaque(None)]),
    });
    let unit = Atom::Constant(builder.constant(Constant::Unit));
    builder.open_block();
    let inner = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![unit],
        },
    );
    let outer = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(inner)],
        },
    );
    let alias = builder.let_value(None, Rhs::Alias(Atom::Value(outer)));
    let first = builder.let_value(
        None,
        Rhs::Project {
            schema: if matching { schema } else { other },
            product: Atom::Value(alias),
            field: 0,
        },
    );
    let second = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(first),
            field: 0,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(second)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the fixture verifies");
    (Rebindings::analyze(&module), Atom::Value(second), unit)
}

#[test]
fn nested_projections_follow_aliases_to_the_original_atom() {
    let (bindings, queried, unit) = fixture(true);
    assert_eq!(bindings.resolve(queried), unit);
}

#[test]
fn a_schema_mismatch_preserves_the_original_projection() {
    let (bindings, queried, _) = fixture(false);
    assert_eq!(bindings.resolve(queried), queried);
}

#[test]
fn an_unresolved_projection_is_not_its_product() {
    let value = ValueId(0);
    let input = Atom::Value(ValueId(1));
    let mut bindings = Rebindings::default();
    bindings.projections.insert(value, (ProductId(0), input, 0));
    assert_eq!(bindings.resolve(Atom::Value(value)), Atom::Value(value));
}

#[test]
fn a_malformed_alias_cycle_terminates() {
    let a = ValueId(0);
    let b = ValueId(1);
    let mut bindings = Rebindings::default();
    bindings.aliases.insert(a, Atom::Value(b));
    bindings.aliases.insert(b, Atom::Value(a));
    assert_eq!(bindings.resolve(Atom::Value(a)), Atom::Value(a));
}

#[test]
fn a_computed_member_stops_resolution_before_its_initializer() {
    let member = ValueId(0);
    let alias = ValueId(1);
    let mut bindings = Rebindings::default();
    bindings.computed.insert(member);
    bindings
        .aliases
        .insert(member, Atom::Constant(ConstantId(0)));
    bindings.aliases.insert(alias, Atom::Value(member));
    assert_eq!(bindings.resolve(Atom::Value(alias)), Atom::Value(member));
}
