//! A fragment's imported rows, held to what each import states.

use crate::*;

fn fault_of(builder: &ErsdBuilder) -> Option<StructuralRule> {
    builder
        .module()
        .structure_fault(Entry::Absent)
        .map(|fault| fault.rule)
}

/// A fragment importing a family of three constructors, holding those at `positions`, whose one item matches a value of it with an arm for each it holds and no default.
fn matching(positions: &[usize]) -> ErsdBuilder {
    let mut builder = ErsdBuilder::new();
    let family = builder.import_family(Some("Tri".into()), 3);
    let constructors = positions
        .iter()
        .map(|&position| builder.import_constructor(family, position, None, 0))
        .collect::<Vec<_>>();
    let scrutinee = builder.import_value(Some("tri".into()));
    let arms = constructors
        .into_iter()
        .map(|constructor| {
            builder.open_block();
            let block = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
            VariantArm {
                constructor,
                bindings: Vec::new(),
                block,
            }
        })
        .collect();
    builder.item_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(scrutinee),
            arms,
            default: None,
        },
    );

    builder
}

/// An imported family lists only the constructors a fragment names, so a match is exhaustive by the count its import states, not by the list.
#[test]
fn a_match_over_an_imported_family_covers_every_constructor_it_declares() {
    assert_eq!(
        fault_of(&matching(&[0, 2])),
        Some(StructuralRule::NonExhaustiveMatch)
    );
    assert_eq!(fault_of(&matching(&[0, 1, 2])), None);
}

#[test]
fn an_imported_constructor_stands_at_a_position_its_family_has() {
    let mut builder = ErsdBuilder::new();
    let family = builder.import_family(None, 2);
    builder.import_constructor(family, 2, None, 0);

    assert_eq!(fault_of(&builder), Some(StructuralRule::BrokenRowImport));
}

#[test]
fn two_imported_constructors_stand_at_two_positions() {
    let mut builder = ErsdBuilder::new();
    let family = builder.import_family(None, 2);
    builder.import_constructor(family, 1, None, 0);
    builder.import_constructor(family, 1, None, 0);

    assert_eq!(fault_of(&builder), Some(StructuralRule::BrokenRowImport));
}

/// A fragment importing a product of width three, whose one item projects field `field` out of an imported value of it.
fn projecting(field: u32) -> ErsdBuilder {
    let mut builder = ErsdBuilder::new();
    let schema = builder.import_product(Some("Triple".into()), 3, false);
    let product = builder.import_value(Some("triple".into()));
    builder.item_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(product),
            field,
        },
    );

    builder
}

/// An imported product lists no field, and is projected out of at the width its import states.
#[test]
fn an_imported_product_is_read_at_its_width() {
    assert!(projecting(2).module().products()[0].fields.is_empty());
    assert_eq!(fault_of(&projecting(2)), None);
    assert_eq!(
        fault_of(&projecting(3)),
        Some(StructuralRule::ProjectionWidth)
    );
}
