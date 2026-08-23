use {
    crate::*,
    curios_abi::{ForeignFunction, Namespace, WireSignature, WireType},
};

/// items: pure_unused = NatAdd(1,1); used = 2; effectful = Foreign(...); entry returns used. The pure unused item drops; the others stay.
#[test]
fn keeps_reached_and_effectful_items_and_drops_the_pure_rest() {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(Constant::Nat(1));
    let _pure_unused = builder.item_value(
        Some("pure_unused".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Constant(one), Atom::Constant(one)],
        },
    );
    let two = builder.constant(Constant::Nat(2));
    let used = builder.item_value(Some("used".into()), Rhs::Alias(Atom::Constant(two)));
    let row = std::sync::Arc::new(ForeignFunction {
        namespace: Namespace::Sys,
        name: "beep".into(),
        subject: Some("Handle".into()),
        label: "beep".into(),
        signature: WireSignature {
            params: vec![],
            results: vec![("r".into(), WireType::Nat)],
        },
    });
    let foreign = builder.foreign(row);
    let _effectful_unused = builder.item_value(
        Some("effectful_unused".into()),
        Rhs::Foreign {
            foreign,
            operands: vec![],
        },
    );
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(used)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);

    let printed = module.to_string();
    assert!(!printed.contains("pure_unused"), "{printed}");
    assert!(printed.contains("effectful_unused"), "{printed}");
    assert_eq!(module.items().len(), 2, "{printed}");
}

/// A dead function web (mutually recursive combinators, never referenced) drops whole; a reached recursive group survives whole.
#[test]
fn drops_dead_function_webs_and_keeps_reached_groups_whole() {
    let mut builder = ErsdBuilder::new();

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
        let mut builder = ErsdBuilder::new();
        let one = builder.constant(Constant::Nat(1));
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
