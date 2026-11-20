use {
    crate::*,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_num::Natural,
    std::sync::Arc,
};

/// items: pure_unused = NatAdd(1,1); used = 2; spin() = spin(); diverging_unused = spin(); effectful_unused = Foreign(...); entry returns used. What the entry reaches stays and every other item drops, whatever its evaluation does: a program computes a top-level value where it first reads it, so one nothing reads never runs.
#[test]
fn keeps_what_the_entry_reaches_and_drops_every_other_item() {
    let mut builder = Builder::new();
    let one = builder.constant(Constant::Nat(Natural::from(1u32)));
    let _pure_unused = builder.item_value(
        Some("pure_unused".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Constant(one), Atom::Constant(one)],
        },
    );
    let two = builder.constant(Constant::Nat(Natural::from(2u32)));
    let used = builder.item_value(Some("used".into()), Rhs::Alias(Atom::Constant(two)));
    // A function that calls itself, not marked total: a call of it may not end.
    let spin = builder.reserve_function();
    builder.open_block();
    let again = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(spin),
            arguments: vec![],
        },
    );
    let spin_body = builder.seal_block(Terminator::Return(Atom::Value(again)));
    builder.define_function(spin, Some("spin".into()), vec![], spin_body);
    builder.item_functions(vec![spin]);
    let _diverging_unused = builder.item_value(
        Some("diverging_unused".into()),
        Rhs::Apply {
            callee: Atom::Function(spin),
            arguments: vec![],
        },
    );
    let row = Arc::new(ForeignFunction {
        name: "/beep".into(),
        signature: WireSignature {
            params: vec![],
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    });
    let foreign = builder.foreign(row);
    // A first call's lane: no token, and not dropped (`curios_abi::LANE_IN`).
    let none = builder.item_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let asking = builder.constant(Constant::Bool(false));
    let _effectful_unused = builder.item_value(
        Some("effectful_unused".into()),
        Rhs::Foreign {
            foreign,
            operands: vec![Atom::Value(none), Atom::Constant(asking)],
        },
    );
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(used)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);

    let printed = module.to_string();
    for dropped in [
        "pure_unused",
        "diverging_unused",
        "effectful_unused",
        "spin",
    ] {
        assert!(!printed.contains(dropped), "{dropped}: {printed}");
    }
    // `used`, the one item the entry reaches.
    assert_eq!(module.items().len(), 1, "{printed}");
}

/// A dead function web (mutually recursive combinators, never referenced) drops whole; a reached recursive group survives whole.
#[test]
fn drops_dead_function_webs_and_keeps_reached_groups_whole() {
    let mut builder = Builder::new();

    // Dead web: rec { a() = b(); b() = a() }
    let a = builder.reserve_function();
    let b = builder.reserve_function();
    builder.open_block();
    let call_b = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(b),
            arguments: vec![],
        },
    );
    let a_body = builder.seal_block(Terminator::Return(Atom::Value(call_b)));
    builder.define_function(a, Some("dead_a".into()), vec![], a_body);
    builder.open_block();
    let call_a = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(a),
            arguments: vec![],
        },
    );
    let b_body = builder.seal_block(Terminator::Return(Atom::Value(call_a)));
    builder.define_function(b, Some("dead_b".into()), vec![], b_body);
    builder.item_functions(vec![a, b]);

    // Live loop: rec live(x) = live(x)
    let live = builder.reserve_function();
    let x = builder.value(Some("x".into()));
    builder.open_block();
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(live),
            arguments: vec![Atom::Value(x)],
        },
    );
    let live_body = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.define_function(live, Some("live".into()), vec![x], live_body);
    builder.item_functions(vec![live]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(live)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);

    let printed = module.to_string();
    assert!(!printed.contains("dead_a"), "{printed}");
    assert!(printed.contains("live"), "{printed}");
}

/// Pruning is idempotent and deterministic.
#[test]
fn pruning_is_deterministic() {
    let build = || {
        let mut builder = Builder::new();
        let one = builder.constant(Constant::Nat(Natural::from(1u32)));
        let _dead = builder.item_value(
            None,
            Rhs::Operation {
                operation: Operation::NatAdd,
                operands: vec![Atom::Constant(one), Atom::Constant(one)],
            },
        );
        builder.open_block();
        let entry = builder.seal_block(Terminator::Return(Atom::Constant(one)));
        builder.set_entry(entry);
        let mut module = builder.finalize().expect("verifies");
        optimize(&mut module);
        module.to_string()
    };
    assert_eq!(build(), build());
}
