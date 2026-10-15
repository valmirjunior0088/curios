//! Reconstruction preserves full layouts and evaluation while exposing existing values.

use {
    super::*,
    crate::{
        Analysis, CellOperation, Constant, ErsdBuilder, Field, NatCase, Operation, ProductSchema,
        VariantArm,
    },
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

    let forward = builder.reserve_function();
    let forwarded = builder.value(None);
    builder.open_block();
    let alias = builder.let_value(None, Rhs::Alias(Atom::Function(wrap)));
    let called = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Value(alias),
            arguments: vec![Atom::Value(forwarded)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(called)));
    builder.define_function(forward, None, vec![forwarded], body);
    builder.item_functions(vec![forward]);

    let function = builder.reserve_function();
    let input = builder.value(None);
    let field = builder.value(None);
    builder.open_block();
    if effect {
        builder.let_value(
            None,
            Rhs::Cell {
                operation: CellOperation::New,
                operands: vec![],
            },
        );
    }
    let rhs = if wrapper {
        Rhs::Apply {
            callee: Atom::Function(forward),
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

fn recursive_unit_fixture(mixed: bool) -> Module {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None); 2],
        shared: true,
    });
    let zero = Atom::Constant(builder.constant(Constant::Nat(0u32.into())));
    let one = Atom::Constant(builder.constant(Constant::Nat(1u32.into())));
    let unit = Atom::Constant(builder.constant(Constant::Unit));
    let function = builder.reserve_function();
    let n = builder.value(None);
    builder.open_block();
    let base = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![zero, if mixed { one } else { unit }],
        },
    );
    let base = builder.seal_block(Terminator::Return(Atom::Value(base)));
    builder.open_block();
    let pred = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), one],
        },
    );
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Value(pred)],
        },
    );
    let at = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(call),
            field: 0,
        },
    );
    let rebuilt = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(at), unit],
        },
    );
    let recursive = builder.seal_block(Terminator::Return(Atom::Value(rebuilt)));
    builder.open_block();
    let result = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: base,
            }],
            default: recursive,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.define_function(function, Some("walk".into()), vec![n], body);
    builder.open_block();
    builder.let_functions(vec![function]);
    let entry = builder.seal_block(Terminator::Return(Atom::Function(function)));
    builder.set_entry(entry);
    builder.finalize().unwrap()
}

#[test]
fn a_recursive_return_establishes_its_retained_unit_field() {
    let mut module = recursive_unit_fixture(false);
    assert!(eliminate_reconstruction(&mut module));
    finish(&mut module);
    let recursive = module
        .statements()
        .iter()
        .flatten()
        .find_map(|statement| match statement {
            Statement::Let {
                rhs: Rhs::SwitchNat { default, .. },
                ..
            } => module.block(*default),
            _ => None,
        })
        .unwrap();
    let last = module
        .statement(*recursive.statements.last().unwrap())
        .unwrap();
    assert!(
        matches!(last, Statement::Let { result, rhs: Rhs::Apply { .. } } if matches!(recursive.terminator, Terminator::Return(Atom::Value(returned)) if returned == *result))
    );
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn one_nonunit_return_prevents_the_unit_rewrite() {
    let mut module = recursive_unit_fixture(true);
    assert!(!eliminate_reconstruction(&mut module));
    module.verify().unwrap();
}

#[test]
fn state_work_in_a_producer_survives_forwarding_its_answer() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None); 2],
        shared: true,
    });
    let unit = Atom::Constant(builder.constant(Constant::Unit));
    let function = builder.reserve_function();
    builder.open_block();
    builder.let_value(
        None,
        Rhs::Cell {
            operation: CellOperation::New,
            operands: vec![],
        },
    );
    let pair = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![unit, unit],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(pair)));
    builder.define_function(function, None, vec![], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![],
        },
    );
    let at = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(call),
            field: 0,
        },
    );
    let result = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(at), unit],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().unwrap();
    assert!(eliminate_reconstruction(&mut module));
    finish(&mut module);
    assert!(module.statements().iter().flatten().any(|s| matches!(
        s,
        Statement::Let {
            rhs: Rhs::Apply { .. },
            ..
        }
    )));
    assert!(module.statements().iter().flatten().any(|s| matches!(
        s,
        Statement::Let {
            rhs: Rhs::Cell { .. },
            ..
        }
    )));
}

#[test]
fn a_computed_member_keeps_its_force_even_when_its_initializer_builds_unit() {
    for unit_field in [false, true] {
        let mut builder = ErsdBuilder::new();
        let schema = builder.product(ProductSchema {
            debug_name: None,
            fields: vec![Field::opaque(None); 2],
            shared: true,
        });
        let unit = Atom::Constant(builder.constant(Constant::Unit));
        let member = builder.value(None);
        builder.open_block();
        let pair = builder.let_value(
            None,
            Rhs::Product {
                schema,
                fields: vec![unit, unit],
            },
        );
        let init = builder.seal_block(Terminator::Return(Atom::Value(pair)));
        let group = builder.rec_group(vec![], vec![(member, init)]);
        builder.open_block();
        builder.let_rec(group);
        let at = builder.let_value(
            None,
            Rhs::Project {
                schema,
                product: Atom::Value(member),
                field: 0,
            },
        );
        let last = if unit_field {
            unit
        } else {
            Atom::Value(builder.let_value(
                None,
                Rhs::Project {
                    schema,
                    product: Atom::Value(member),
                    field: 1,
                },
            ))
        };
        let result = builder.let_value(
            None,
            Rhs::Product {
                schema,
                fields: vec![Atom::Value(at), last],
            },
        );
        let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
        builder.set_entry(entry);
        let mut module = builder.finalize().unwrap();
        assert!(!eliminate_reconstruction(&mut module));
        module.verify().unwrap();
    }
}

fn many_returns(count: usize) -> Module {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None); 2],
        shared: true,
    });
    let unit = Atom::Constant(builder.constant(Constant::Unit));
    let function = builder.reserve_function();
    let n = builder.value(None);
    let mut blocks = Vec::new();
    for _ in 0..count {
        builder.open_block();
        let pair = builder.let_value(
            None,
            Rhs::Product {
                schema,
                fields: vec![Atom::Value(n), unit],
            },
        );
        blocks.push(builder.seal_block(Terminator::Return(Atom::Value(pair))));
    }
    let default = blocks.pop().unwrap();
    builder.open_block();
    let result = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: blocks
                .into_iter()
                .enumerate()
                .map(|(i, block)| NatCase {
                    key: i as u32,
                    block,
                })
                .collect(),
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.define_function(function, None, vec![n], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let zero = Atom::Constant(builder.constant(Constant::Nat(0u32.into())));
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![zero],
        },
    );
    let at = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(call),
            field: 0,
        },
    );
    let rebuilt = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(at), unit],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(rebuilt)));
    builder.set_entry(entry);
    builder.finalize().unwrap()
}

#[test]
fn an_origin_set_widens_conservatively_at_its_precision_limit() {
    let mut admitted = many_returns(super::facts::ORIGIN_LIMIT);
    assert!(eliminate_reconstruction(&mut admitted));
    finish(&mut admitted);
    let mut declined = many_returns(super::facts::ORIGIN_LIMIT + 1);
    assert!(!eliminate_reconstruction(&mut declined));
    declined.verify().unwrap();
}

#[test]
fn a_prefix_or_another_schema_is_not_the_original_product() {
    for width in [1, 2, 3] {
        let mut module = product_fixture(&[0, 1], 1);
        let schema = module.add_product(ProductSchema {
            debug_name: None,
            fields: vec![Field::opaque(None); width],
            shared: false,
        });
        let (id, mut statement) = module
            .statements()
            .iter()
            .enumerate()
            .find_map(|(i, s)| {
                let statement = s.as_ref()?;
                matches!(
                    statement,
                    Statement::Let {
                        rhs: Rhs::Product { .. },
                        ..
                    }
                )
                .then_some((StatementId(i as u32), statement.clone()))
            })
            .unwrap();
        if let Statement::Let {
            rhs: Rhs::Product {
                schema: built,
                fields,
            },
            ..
        } = &mut statement
        {
            *built = schema;
            fields.resize(width, fields[0]);
        }
        module.set_statement(id, statement);
        module.verify().unwrap();
        assert!(!eliminate_reconstruction(&mut module));
    }
}

#[test]
fn fields_from_two_bases_are_not_an_identity() {
    let mut module = product_fixture(&[0, 1], 1);
    let other = module.add_value(None);
    let function = module.function_ids().next().unwrap();
    let mut definition = module.function(function).unwrap().clone();
    definition.params.push(other);
    module.set_function(function, definition);
    let (id, mut statement) = module
        .statements()
        .iter()
        .enumerate()
        .find_map(|(i, s)| {
            let statement = s.as_ref()?;
            matches!(
                statement,
                Statement::Let {
                    rhs: Rhs::Project { field: 1, .. },
                    ..
                }
            )
            .then_some((StatementId(i as u32), statement.clone()))
        })
        .unwrap();
    if let Statement::Let {
        rhs: Rhs::Project { product, .. },
        ..
    } = &mut statement
    {
        *product = Atom::Value(other);
    }
    module.set_statement(id, statement);
    module.verify().unwrap();
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn changing_a_constructor_payload_is_not_an_identity() {
    let mut module = variant_fixture(false, false, false);
    let unit = Atom::Constant(module.intern_constant(Constant::Unit));
    let changes = module
        .statements()
        .iter()
        .enumerate()
        .filter_map(|(i, s)| {
            let mut statement = s.as_ref()?.clone();
            let Statement::Let {
                rhs: Rhs::Construct { fields, .. },
                ..
            } = &mut statement
            else {
                return None;
            };
            if fields.is_empty() {
                return None;
            }
            fields[0] = unit;
            Some((StatementId(i as u32), statement))
        })
        .collect::<Vec<_>>();
    for (id, statement) in changes {
        module.set_statement(id, statement);
    }
    module.verify().unwrap();
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn a_nonidentity_or_trapping_default_is_preserved() {
    for trap in [false, true] {
        let mut module = variant_fixture(true, false, true);
        let unit = Atom::Constant(module.intern_constant(Constant::Unit));
        let default = module
            .statements()
            .iter()
            .flatten()
            .find_map(|statement| match statement {
                Statement::Let {
                    rhs: Rhs::MatchVariant { default, .. },
                    ..
                } => *default,
                _ => None,
            })
            .unwrap();
        module.set_block_terminator(
            default,
            if trap {
                Terminator::Unreachable
            } else {
                Terminator::Return(unit)
            },
        );
        module.verify().unwrap();
        assert!(!eliminate_reconstruction(&mut module));
    }
}

#[test]
fn a_trap_before_constructor_reconstruction_is_preserved() {
    let mut module = variant_fixture(true, false, false);
    let arm = module
        .statements()
        .iter()
        .flatten()
        .find_map(|statement| match statement {
            Statement::Let {
                rhs: Rhs::MatchVariant { arms, .. },
                ..
            } => Some(arms[0].block),
            _ => None,
        })
        .unwrap();
    let zero = Atom::Constant(module.intern_constant(Constant::Nat(0u32.into())));
    let result = module.add_value(None);
    let trap = module.add_statement(Statement::Let {
        result,
        rhs: Rhs::Operation {
            operation: Operation::NatDiv,
            operands: vec![zero, zero],
        },
    });
    let mut statements = vec![trap];
    statements.extend(&module.block(arm).unwrap().statements);
    module.set_block_statements(arm, statements);
    module.verify().unwrap();
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn an_unknown_callback_does_not_establish_a_unit_field() {
    let mut module = recursive_unit_fixture(false);
    let parameter = module.functions().iter().flatten().next().unwrap().params[0];
    let (id, mut statement) = module
        .statements()
        .iter()
        .enumerate()
        .find_map(|(i, s)| {
            let statement = s.as_ref()?;
            matches!(
                statement,
                Statement::Let {
                    rhs: Rhs::Apply { .. },
                    ..
                }
            )
            .then_some((StatementId(i as u32), statement.clone()))
        })
        .unwrap();
    if let Statement::Let {
        rhs: Rhs::Apply { callee, .. },
        ..
    } = &mut statement
    {
        *callee = Atom::Value(parameter);
    }
    module.set_statement(id, statement);
    module.verify().unwrap();
    assert!(!eliminate_reconstruction(&mut module));
}

#[test]
fn cyclic_constructor_forwarding_is_not_a_builder() {
    let mut module = variant_fixture(true, false, false);
    let wrapper = module.function_ids().next().unwrap();
    let definition = module.function(wrapper).unwrap();
    let param = definition.params[0];
    let statement = definition.body;
    let id = module.block(statement).unwrap().statements[0];
    let Statement::Let { result, .. } = *module.statement(id).unwrap() else {
        unreachable!()
    };
    module.set_statement(
        id,
        Statement::Let {
            result,
            rhs: Rhs::Apply {
                callee: Atom::Function(wrapper),
                arguments: vec![Atom::Value(param)],
            },
        },
    );
    module.verify().unwrap();
    assert!(!eliminate_reconstruction(&mut module));
}
