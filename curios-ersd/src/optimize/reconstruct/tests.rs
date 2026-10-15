//! Reconstruction preserves full layouts and evaluation while exposing existing values.

use {
    super::*,
    crate::{Analysis, Constant, ErsdBuilder, Field, ProductSchema, VariantArm},
};

fn finish(module: &mut Module) {
    let analysis = Analysis::analyze(module);
    super::super::prune_unreachable(module, &analysis);
    module.compact();
    module
        .verify()
        .expect("reconstruction preserves the module");
}

fn product_fixture(order: &[u32], depth: usize) -> Module {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None); 2],
        shared: true,
    });
    let function = builder.reserve_function();
    let input = builder.value(Some("input".into()));
    builder.open_block();
    let mut base = input;
    for _ in 0..depth {
        let fields = order
            .iter()
            .map(|&field| {
                Atom::Value(builder.let_value(
                    None,
                    Rhs::Project {
                        schema,
                        product: Atom::Value(base),
                        field,
                    },
                ))
            })
            .collect();
        base = builder.let_value(None, Rhs::Product { schema, fields });
    }
    let body = builder.seal_block(Terminator::Return(Atom::Value(base)));
    builder.define_function(function, None, vec![input], body);
    builder.open_block();
    builder.let_functions(vec![function]);
    let entry = builder.seal_block(Terminator::Return(Atom::Function(function)));
    builder.set_entry(entry);
    builder.finalize().expect("the fixture verifies")
}

#[test]
fn a_complete_product_is_its_base() {
    let mut module = product_fixture(&[0, 1], 1);
    assert!(eliminate_reconstruction(&mut module));
    finish(&mut module);
    let function = module.functions().iter().flatten().next().unwrap();
    let body = module.block(function.body).unwrap();
    assert!(body.statements.is_empty());
    assert!(
        matches!(body.terminator, Terminator::Return(Atom::Value(value)) if value == function.params[0])
    );
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn reordered_fields_still_build_a_product() {
    let mut module = product_fixture(&[1, 0], 1);
    assert!(!eliminate_reconstruction(&mut module));
    module.verify().unwrap();
}

#[test]
fn shared_reconstruction_chains_settle_in_one_invocation() {
    let mut module = product_fixture(&[0, 1], 1000);
    assert!(eliminate_reconstruction(&mut module));
    finish(&mut module);
    let function = module.functions().iter().flatten().next().unwrap();
    assert!(module.block(function.body).unwrap().statements.is_empty());
    assert!(!eliminate_reconstruction(&mut module));
}

fn variant_fixture(wrapper: bool, effect: bool, default: bool) -> Module {
    let mut builder = ErsdBuilder::new();
    let family = builder.family(None);
    let some = builder.constructor(family, None, vec![Field::opaque(None)]);
    let none = builder.constructor(family, None, vec![]);
    let wrap = builder.reserve_function();
    let payload = builder.value(None);
    builder.open_block();
    let constructed = builder.let_value(
        None,
        Rhs::Construct {
            constructor: some,
            fields: vec![Atom::Value(payload)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(constructed)));
    builder.define_function(wrap, None, vec![payload], body);
    builder.item_functions(vec![wrap]);

    let function = builder.reserve_function();
    let input = builder.value(None);
    let field = builder.value(None);
    builder.open_block();
    if effect {
        builder.let_value(
            None,
            Rhs::Cell {
                operation: crate::CellOperation::New,
                operands: vec![],
            },
        );
    }
    let rhs = if wrapper {
        Rhs::Apply {
            callee: Atom::Function(wrap),
            arguments: vec![Atom::Value(field)],
        }
    } else {
        Rhs::Construct {
            constructor: some,
            fields: vec![Atom::Value(field)],
        }
    };
    let rebuilt = builder.let_value(None, rhs);
    let some_arm = builder.seal_block(Terminator::Return(Atom::Value(rebuilt)));
    builder.open_block();
    let returned = if default {
        Atom::Value(input)
    } else {
        Atom::Value(builder.let_value(
            None,
            Rhs::Construct {
                constructor: none,
                fields: vec![],
            },
        ))
    };
    let none_arm = builder.seal_block(Terminator::Return(returned));
    builder.open_block();
    let mut arms = vec![VariantArm {
        constructor: some,
        bindings: vec![field],
        block: some_arm,
    }];
    if !default {
        arms.push(VariantArm {
            constructor: none,
            bindings: vec![],
            block: none_arm,
        });
    }
    let result = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(input),
            arms,
            default: default.then_some(none_arm),
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.define_function(function, None, vec![input], body);
    builder.open_block();
    builder.let_functions(vec![function]);
    let entry = builder.seal_block(Terminator::Return(Atom::Function(function)));
    builder.set_entry(entry);
    builder.finalize().expect("the fixture verifies")
}

#[test]
fn exhaustive_constructor_reconstruction_is_its_scrutinee() {
    for wrapper in [false, true] {
        let mut module = variant_fixture(wrapper, false, false);
        assert!(eliminate_reconstruction(&mut module));
        finish(&mut module);
        assert!(!module.statements().iter().flatten().any(|s| matches!(
            s,
            Statement::Let {
                rhs: Rhs::MatchVariant { .. },
                ..
            }
        )));
        assert!(!eliminate_reconstruction(&mut module));
    }
}

#[test]
fn a_default_returning_the_scrutinee_is_an_identity() {
    let mut module = variant_fixture(true, false, true);
    assert!(eliminate_reconstruction(&mut module));
    finish(&mut module);
}

#[test]
fn an_arm_performing_state_work_is_preserved() {
    let mut module = variant_fixture(true, true, false);
    assert!(!eliminate_reconstruction(&mut module));
    module.verify().unwrap();
}

#[test]
fn a_retained_unit_is_not_assumed_to_equal_an_opaque_field() {
    let mut module = product_fixture(&[0, 1], 1);
    let unit = Atom::Constant(module.intern_constant(Constant::Unit));
    let id = module
        .statements()
        .iter()
        .enumerate()
        .find_map(|(i, s)| {
            matches!(
                s,
                Some(Statement::Let {
                    rhs: Rhs::Product { .. },
                    ..
                })
            )
            .then_some(StatementId(i as u32))
        })
        .unwrap();
    let mut statement = module.statement(id).unwrap().clone();
    if let Statement::Let {
        rhs: Rhs::Product { fields, .. },
        ..
    } = &mut statement
    {
        fields[1] = unit;
    }
    module.set_statement(id, statement);
    assert!(!eliminate_reconstruction(&mut module));
    module.verify().unwrap();
}
