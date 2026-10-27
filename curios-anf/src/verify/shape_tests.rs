//! A module's well-formedness: carriers stated where they may be, scope, arity and saturation, a match's coverage and keys, a block's owner, a projection's range, and a deep module checked without native stack.

use {super::test_support::nat_atom, crate::*};

/// An operation names its result's carrier, so a register stated on its result would state one fact twice, and could state it otherwise than the operation computes it.
#[test]
fn a_carrier_stated_on_an_operations_result_is_refused() {
    let mut builder = Builder::new();
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let sum = builder.let_value(
        Some("sum".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![one, one],
        },
    );
    builder.state(sum, RegisterShape::Flt);
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::StatedCarrier,
                ..
            })
        ),
        "a carrier stated on an operation's result is refused: {fault:?}"
    );
}

/// A field read names no carrier — a polymorphic slot hands back a reference — so the binding may state the one its type names.
#[test]
fn a_carrier_stated_on_a_field_read_verifies() {
    let mut builder = Builder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Pair".into()),
        fields: ProductFields::Declared(vec![
            Field::opaque(Some("a".into())),
            Field::opaque(Some("b".into())),
        ]),
    });
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let pair = builder.let_value(
        Some("pair".into()),
        Rhs::Product {
            schema,
            fields: vec![one, one],
        },
    );
    let read = builder.let_value(
        Some("read".into()),
        Rhs::Project {
            schema,
            product: Atom::Value(pair),
            field: 0,
        },
    );
    builder.state(read, RegisterShape::Immediate);
    let entry = builder.seal_block(Terminator::Return(Atom::Value(read)));
    builder.set_entry(entry);

    assert!(builder.module().structure_fault(Entry::Required).is_none());
}

#[test]
fn an_unbound_value_use_is_out_of_scope() {
    let mut builder = Builder::new();
    let unbound = builder.value(Some("unbound".into()));
    builder.open_block();
    let early = builder.let_value(None, Rhs::Alias(Atom::Value(unbound)));
    let entry = builder.seal_block(Terminator::Return(Atom::Value(early)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::ValueOutOfScope,
                ..
            })
        ),
        "an unbound use is out of scope: {fault:?}"
    );
}

#[test]
fn a_value_bound_inside_a_function_is_out_of_scope_after_it() {
    let mut builder = Builder::new();
    let function = builder.reserve_function();
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let local = builder.let_value(Some("local".into()), Rhs::Alias(one));
    let body = builder.seal_block(Terminator::Return(Atom::Value(local)));
    builder.define_function(function, None, vec![], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(local)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::ValueOutOfScope,
                ..
            })
        ),
        "the local escaped its scope: {fault:?}"
    );
}

#[test]
fn an_operation_arity_mismatch_is_rejected() {
    let mut builder = Builder::new();
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::OperationArity,
                ..
            })
        ),
        "NatAdd takes two operands: {fault:?}"
    );
}

#[test]
fn an_unsaturated_direct_application_is_rejected() {
    let mut builder = Builder::new();
    let function = builder.reserve_function();
    let param = builder.value(None);
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(param)));
    builder.define_function(function, None, vec![param], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::ApplicationArity,
                ..
            })
        ),
        "direct application is saturated: {fault:?}"
    );
}

/// The mapper is `ListMap`'s second operand — the list comes first, like every sequence operation. A rule reading the first operand would check nothing, since a list value never looks like a function atom.
#[test]
fn a_two_parameter_mapper_is_rejected() {
    let mut builder = Builder::new();
    let mapper = builder.reserve_function();
    let left = builder.value(None);
    let right = builder.value(None);
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(left)));
    builder.define_function(mapper, Some("mapper".into()), vec![left, right], body);
    builder.item_functions(vec![mapper]);

    builder.open_block();
    let list = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let mapped = builder.let_value(
        None,
        Rhs::Intrinsic {
            intrinsic: Intrinsic::ListMap,
            operands: vec![Atom::Value(list), Atom::Function(mapper)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(mapped)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::MapperArity,
                ..
            })
        ),
        "a mapper takes one element: {fault:?}"
    );
}

#[test]
fn a_match_missing_a_constructor_is_rejected() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Shape".into()));
    let circle = builder.constructor(family, Some("circle".into()), vec![Field::opaque(None)]);
    let _square = builder.constructor(family, Some("square".into()), vec![Field::opaque(None)]);

    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let shape = builder.let_value(
        None,
        Rhs::Construct {
            constructor: circle,
            fields: vec![one],
        },
    );
    builder.open_block();
    let radius = builder.value(Some("radius".into()));
    let arm = builder.seal_block(Terminator::Return(Atom::Value(radius)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(shape),
            arms: vec![VariantArm {
                constructor: circle,
                bindings: vec![radius],
                block: arm,
            }],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::NonExhaustiveMatch,
                ..
            })
        ),
        "the square arm is missing: {fault:?}"
    );
}

#[test]
fn duplicate_switch_keys_are_rejected() {
    let mut builder = Builder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    builder.open_block();
    let first = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let second = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let default = builder.seal_block(Terminator::Return(zero));
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: zero,
            cases: vec![
                NatCase {
                    key: 3,
                    block: first,
                },
                NatCase {
                    key: 3,
                    block: second,
                },
            ],
            default,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::DuplicateCaseKey,
                ..
            })
        ),
        "keys must be distinct: {fault:?}"
    );
}

#[test]
fn a_block_with_two_owners_is_rejected() {
    let mut builder = Builder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    builder.open_block();
    let shared = builder.seal_block(Terminator::Return(zero));
    let condition = nat_bool(&mut builder);
    let switched = builder.let_value(
        None,
        Rhs::SwitchBool {
            scrutinee: condition,
            if_false: shared,
            if_true: shared,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::BlockOwnedTwice,
                ..
            })
        ),
        "both arms own the same block: {fault:?}"
    );
}

fn nat_bool(builder: &mut Builder) -> Atom {
    let constant = builder.constant(Constant::Bool(true));
    Atom::Constant(constant)
}

#[test]
fn a_leaked_block_is_rejected() {
    let mut builder = Builder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let leaked = builder.seal_block(Terminator::Return(zero));
    let _ = leaked;
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(zero));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::BlockUnowned,
                ..
            })
        ),
        "the sealed block has no owner: {fault:?}"
    );
}

#[test]
fn a_projection_out_of_range_is_rejected() {
    let mut builder = Builder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: ProductFields::Declared(vec![Field::opaque(None), Field::opaque(None)]),
    });
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let one = nat_atom(&mut builder, 1);
    let pair = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![zero, one],
        },
    );
    let projected = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(pair),
            field: 2,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(projected)));
    builder.set_entry(entry);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::ProjectionWidth,
                ..
            })
        ),
        "field 2 does not exist: {fault:?}"
    );
}

/// A deeply nested module verifies (and a malformed one diagnoses) on the default test-thread stack: the walk recurses inside `recurse`, which continues on a fresh stack segment rather than overflowing the thread's.
#[test]
fn a_deep_module_verifies_without_native_stack() {
    let builder = deep_switch_chain(50_000, false);
    builder.finalize().expect("the deep module verifies");
}

#[test]
fn a_deep_malformed_module_diagnoses_without_native_stack() {
    let builder = deep_switch_chain(50_000, true);
    let fault = builder.module().structure_fault(Entry::Required);
    assert!(
        matches!(
            fault,
            Some(StructuralFault {
                rule: StructuralRule::ValueOutOfScope,
                ..
            })
        ),
        "the innermost block is malformed: {fault:?}"
    );
}

/// Build a `depth`-deep chain of nested `SwitchNat` defaults. With `malformed` the innermost block references a value that is never bound.
fn deep_switch_chain(depth: usize, malformed: bool) -> Builder {
    let mut builder = Builder::new();
    let zero = nat_atom(&mut builder, 0);
    let scrutinee = builder.item_value(Some("scrutinee".into()), Rhs::Alias(zero));

    builder.open_block();
    let innermost = if malformed {
        Atom::Value(builder.value(Some("ghost".into())))
    } else {
        Atom::Value(scrutinee)
    };
    let mut chain = builder.seal_block(Terminator::Return(innermost));
    for _ in 0..depth {
        builder.open_block();
        let leaf = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
        builder.open_block();
        let switched = builder.let_value(
            None,
            Rhs::SwitchNat {
                scrutinee: Atom::Value(scrutinee),
                cases: vec![NatCase {
                    key: 0,
                    block: leaf,
                }],
                default: chain,
            },
        );
        chain = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    }
    builder.set_entry(chain);
    builder
}
