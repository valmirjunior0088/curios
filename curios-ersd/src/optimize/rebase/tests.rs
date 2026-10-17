use {
    crate::*,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_num::Natural,
    std::sync::Arc,
};

/// A module whose entry calls `f` on a `Nat` a host call answers, so evaluation cannot close it, and whose `f(n)` dispatches on `n`: `0` answers `base`, and any other `n` answers what `leaf` builds, handed the builder, `f`, `n` and `n - 1`. The module after optimization, printed.
fn optimized(
    base: Constant,
    leaf: impl FnOnce(&mut ErsdBuilder, FunctionId, ValueId, ValueId) -> Atom,
) -> String {
    let mut builder = ErsdBuilder::new();
    let f = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let base = builder.constant(base);
    let one = builder.constant(Constant::Nat(Natural::from(1u32)));

    builder.open_block();
    let zero_arm = builder.seal_block(Terminator::Return(Atom::Constant(base)));
    builder.open_block();
    let pred = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let answer = leaf(&mut builder, f, n, pred);
    let default = builder.seal_block(Terminator::Return(answer));
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
    builder.define_function(f, Some("f".into()), vec![n], body);
    builder.item_functions(vec![f]);

    builder.open_block();
    let row = Arc::new(ForeignFunction {
        name: "/poll".into(),
        signature: WireSignature {
            params: vec![],
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    });
    let foreign = builder.foreign(row);
    // A first call's lane: no token, and not dropped (`curios_abi::LANE_IN`).
    let none = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let asking = builder.constant(Constant::Bool(false));
    let opaque = builder.let_value(
        Some("opaque".into()),
        Rhs::Foreign {
            foreign,
            operands: vec![Atom::Value(none), Atom::Constant(asking)],
        },
    );
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![Atom::Value(opaque)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);
    module.to_string()
}

/// `n - 1` handed to `f`, and its answer.
fn recur(builder: &mut ErsdBuilder, f: FunctionId, pred: ValueId) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![Atom::Value(pred)],
        },
    ))
}

fn operation(builder: &mut ErsdBuilder, operation: Operation, left: Atom, right: Atom) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Operation {
            operation,
            operands: vec![left, right],
        },
    ))
}

/// `f(n) = f(n - 1) + 1`.
#[test]
fn a_monoid_deferred_recursion_gains_a_worker() {
    let printed = optimized(Constant::Nat(Natural::zero()), |builder, f, _, pred| {
        let one = Atom::Constant(builder.constant(Constant::Nat(Natural::from(1u32))));
        let recursion = recur(builder, f, pred);
        operation(builder, Operation::NatAdd, recursion, one)
    });
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = (f(n - 1) + n) + 1`: the chain's two addends fold into the one accumulator.
#[test]
fn a_chain_of_combines_folds_into_one_accumulator() {
    let printed = optimized(Constant::Nat(Natural::zero()), |builder, f, n, pred| {
        let one = Atom::Constant(builder.constant(Constant::Nat(Natural::from(1u32))));
        let recursion = recur(builder, f, pred);
        let first = operation(builder, Operation::NatAdd, recursion, Atom::Value(n));
        operation(builder, Operation::NatAdd, first, one)
    });
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = (n - 1 < n) && f(n - 1)`, the shape `List/eql` erases to: a conjunction is a monoid the oracle names.
#[test]
fn a_conjunction_around_its_recursion_gains_a_worker() {
    let printed = optimized(Constant::Bool(true), |builder, f, n, pred| {
        let below = operation(builder, Operation::NatLt, Atom::Value(pred), Atom::Value(n));
        let recursion = recur(builder, f, pred);
        operation(builder, Operation::BoolAnd, below, recursion)
    });
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = f(n - 1) + n * n`, the product computed after the call: only the combines may move across it, so the recursion is left as written.
#[test]
fn an_addend_computed_after_the_call_is_declined() {
    let printed = optimized(Constant::Nat(Natural::zero()), |builder, f, n, pred| {
        let recursion = recur(builder, f, pred);
        let square = operation(builder, Operation::NatMul, Atom::Value(n), Atom::Value(n));
        operation(builder, Operation::NatAdd, recursion, square)
    });
    assert!(!printed.contains("f@w"), "{printed}");
}

/// `f(n) = r + r` for `r = f(n - 1)`: a context reading the call's answer twice is no word with one hole, so the recursion is left as written.
#[test]
fn a_hole_read_twice_is_declined() {
    let printed = optimized(Constant::Nat(Natural::zero()), |builder, f, _, pred| {
        let recursion = recur(builder, f, pred);
        operation(builder, Operation::NatAdd, recursion, recursion)
    });
    assert!(!printed.contains("f@w"), "{printed}");
}
