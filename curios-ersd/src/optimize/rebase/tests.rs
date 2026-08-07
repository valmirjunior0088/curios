use {
    crate::*,
    curios_abi::{ForeignFunction, WireSignature, WireType},
};

/// rec count(n) = switch-nat n { 0 => 0, default => count(n - 1) + 1 }; entry: count(k) with k a runtime-ish parameterless alias (kept opaque by referencing the function itself so evaluation cannot close it).
#[test]
fn a_monoid_deferred_recursion_gains_a_worker() {
    let mut builder = ErsdBuilder::new();
    let count = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let zero = builder.constant(Constant::Nat(0));
    let one = builder.constant(Constant::Nat(1));

    builder.open_block();
    let zero_arm = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.open_block();
    let pred = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(count),
            arguments: vec![Atom::Value(pred)],
        },
    );
    let combined = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(recur), Atom::Constant(one)],
        },
    );
    let default = builder.seal_block(Terminator::Return(Atom::Value(combined)));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_arm,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.define_function(count, Some("count".into()), vec![n], body);
    builder.item_functions(vec![count]);

    builder.open_block();
    let row = std::sync::Arc::new(ForeignFunction {
        namespace: "sys",
        name: "poll".into(),
        subject: Some("Handle".into()),
        label: "poll".into(),
        signature: WireSignature {
            params: vec![],
            results: vec![("r".into(), WireType::Nat)],
        },
    });
    let foreign = builder.foreign(row);
    let opaque = builder.let_value(
        Some("opaque".into()),
        Rhs::Foreign {
            foreign,
            operands: vec![],
        },
    );
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(count),
            arguments: vec![Atom::Value(opaque)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize_ir(&mut module);
    let printed = module.to_string();
    assert!(printed.contains("count@w"), "{printed}");
}
