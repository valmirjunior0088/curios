use {crate::*, curios_num::Floating};

fn doubling_module() -> Module {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(Constant::Nat(1));
    let bound = builder.item_value(Some("one".into()), Rhs::Alias(Atom::Constant(one)));
    builder.open_block();
    let doubled = builder.let_value(
        Some("doubled".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(bound), Atom::Value(bound)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(doubled)));
    builder.set_entry(entry);
    builder.finalize().expect("the module verifies")
}

#[test]
fn a_representative_module_prints_exactly() {
    assert_eq!(
        doubling_module().to_string(),
        "\
~v0$one = alias 1
entry {
    ~v1$doubled = NatAdd(~v0$one, ~v0$one)
    return ~v1$doubled
}
"
    );
}

#[test]
fn a_recursive_module_prints_exactly() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let zero = builder.constant(Constant::Nat(0));
    let one = builder.constant(Constant::Nat(1));
    builder.open_block();
    let zero_case = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.open_block();
    let predecessor = builder.let_value(
        Some("predecessor".into()),
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Value(predecessor)],
        },
    );
    let default = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_case,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.define_function(function, Some("loop".into()), vec![n], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let ten = builder.constant(Constant::Nat(10));
    let run = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Constant(ten)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(run)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
functions ~f0$loop
entry {
    ~v4 = apply ~f0$loop(10)
    return ~v4
}
function ~f0$loop(~v0$n) {
    ~v3 = switch-nat ~v0$n {
        0 => {
            return 0
        }
        default => {
            ~v1$predecessor = NatSub(~v0$n, 1)
            ~v2 = apply ~f0$loop(~v1$predecessor)
            return ~v2
        }
    }
    return ~v3
}
"
    );
}

#[test]
fn schemas_and_constants_print_deterministically() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Pair".into()),
        fields: vec![Field::opaque(Some("lhs".into())), Field::opaque(None)],
    });
    let family = builder.family(Some("Shape".into()));
    let circle = builder.constructor(
        family,
        Some("circle".into()),
        vec![Field::opaque(Some("radius".into()))],
    );
    builder.open_block();
    let negative_zero = builder.constant(Constant::Flt(Floating::from_f32(-0.0)));
    let byte = builder.constant(Constant::Byte(7));
    let pair = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Constant(negative_zero), Atom::Constant(byte)],
        },
    );
    let shape = builder.let_value(
        None,
        Rhs::Construct {
            constructor: circle,
            fields: vec![Atom::Value(pair)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(shape)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
product ~p0$Pair(lhs, 1)
family ~d0$Shape { ~t0$circle(radius) }
entry {
    ~v0 = product ~p0(-0.0:flt, 7:byte)
    ~v1 = construct ~t0(~v0)
    return ~v1
}
"
    );
}

#[test]
fn printing_is_deterministic_across_constructions() {
    assert_eq!(doubling_module().to_string(), doubling_module().to_string());
}

/// Printing a deep module costs heap, not native stack.
#[test]
fn a_deep_module_prints_without_native_stack() {
    let mut builder = ErsdBuilder::new();
    let zero = builder.constant(Constant::Nat(0));
    let scrutinee = builder.item_value(Some("scrutinee".into()), Rhs::Alias(Atom::Constant(zero)));
    builder.open_block();
    let mut chain = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
    for _ in 0..50_000 {
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
    let module = builder.finalize().expect("the deep module verifies");

    let printed = module.to_string();
    assert!(printed.starts_with("~v0$scrutinee = alias 0\nentry {\n"));
    assert!(printed.ends_with("}\n"));
}
