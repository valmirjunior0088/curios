use crate::ir::*;

/// rec count(n) = switch-nat n { 0 => 0, default => count(n - 1) + 1 };
/// entry: count(k) with k a runtime-ish parameterless alias (kept opaque by
/// referencing the function itself so evaluation cannot close it).
#[test]
fn a_monoid_deferred_recursion_gains_a_worker() {
    let mut builder = ErsdBuilder::new();
    let count = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let zero = builder.constant(Constant::Nat(0));
    let one = builder.constant(Constant::Nat(1));

    builder.open_block();
    let zero_arm = builder.seal_block(Terminator::Return(ErasedAtom::Constant(zero)));
    builder.open_block();
    let pred = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![ErasedAtom::Value(n), ErasedAtom::Constant(one)],
        },
    );
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: ErasedAtom::Function(count),
            arguments: vec![ErasedAtom::Value(pred)],
        },
    );
    let combined = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![ErasedAtom::Value(recur), ErasedAtom::Constant(one)],
        },
    );
    let default = builder.seal_block(Terminator::Return(ErasedAtom::Value(combined)));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: ErasedAtom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_arm,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(ErasedAtom::Value(switched)));
    builder.define_function(count, Some("count".into()), vec![n], body);
    builder.item_functions(vec![count]);

    builder.open_block();
    let row = std::sync::Arc::new(curios_abi::ForeignFunction {
        namespace: "sys",
        name: "poll".into(),
        label: "poll".into(),
        signature: curios_abi::WireSignature {
            params: vec![],
            results: vec![("r".into(), curios_abi::WireType::Nat)],
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
            callee: ErasedAtom::Function(count),
            arguments: vec![ErasedAtom::Value(opaque)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize_ir(&mut module);
    let printed = module.to_string();
    assert!(printed.contains("count@w"), "{printed}");
}
