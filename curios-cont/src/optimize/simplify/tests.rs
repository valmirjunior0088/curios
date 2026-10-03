//! Dead code, jump and call forwarding, atom rewriting, and the intrinsic identities the simplifier folds.

use {
    super::{
        eliminate_dead_bindings, eliminate_dead_parameters, fold_intrinsic_identities,
        forward_aggregate_projections, forward_calls, forward_continuations, rewrite_atoms,
    },
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Intrinsic, Literal,
        Module, Node, NodeId, ValueExpr, ValueId, test_support::unary_intrinsic_module,
    },
    curios_num::{Floating, Integer, Natural, Rounding},
    std::collections::BTreeMap,
};

#[test]
fn dead_binding_elimination_preserves_traps_and_drops_total_literals() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let return_node = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
    }));
    let dead_total = module.add_value(Some("dead total".into()));
    let total_node = module.add_node(Node::LetIntrinsic {
        result: dead_total,
        op: Intrinsic::NatEql,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ],
        next: return_node,
    });
    let dead_trap = module.add_value(Some("dead trap".into()));
    let trap_node = module.add_node(Node::LetIntrinsic {
        result: dead_trap,
        op: Intrinsic::NatDiv,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(0u32))),
        ],
        next: total_node,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: trap_node,
            droppable: false,
        },
    );
    module.set_entry(entry);

    assert!(eliminate_dead_bindings(&mut module));
    assert!(module.node(total_node).is_none());
    assert!(matches!(
        module.node(trap_node),
        Some(Node::LetIntrinsic {
            op: Intrinsic::NatDiv,
            next,
            ..
        }) if *next == return_node
    ));
    module.verify().unwrap();
}

#[test]
fn dead_parameter_elimination_rewrites_known_calls() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let callee = module.reserve_function();
    let kept = module.add_value(Some("kept".into()));
    let removed = module.add_value(Some("removed".into()));
    let callee_return = module.reserve_continuation();
    let callee_body = module.add_node(Node::ApplyCont(Edge {
        target: callee_return,
        args: vec![Atom::Value(kept)],
    }));
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![kept, removed],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );
    let main_return = module.reserve_continuation();
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ],
        return_to: main_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);

    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(callee).unwrap().params, vec![kept]);
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyFun { args, .. })
            if args == &[Atom::Literal(Literal::Nat(Natural::from(1u32)))]
    ));
    module.verify().unwrap();
}

#[test]
fn forwarding_composes_jump_arguments_instead_of_only_retargeting() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let target_left = module.add_value(Some("target left".into()));
    let target_right = module.add_value(Some("target right".into()));
    let target_body = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(target_right)],
    }));
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("target".into()),
            params: vec![target_left, target_right],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Value(forwarded),
        ],
    }));
    module.define_continuation(
        forwarding,
        Continuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let call = module.add_node(Node::ApplyCont(Edge {
        target: forwarding,
        args: vec![Atom::Literal(Literal::Nat(Natural::from(7u32)))],
    }));
    let body = module.add_node(Node::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target: actual, args }))
            if *actual == target
                && args == &[
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                    Atom::Literal(Literal::Nat(Natural::from(7u32))),
                ]
    ));
    module.verify().unwrap();
}

/// A NaN literal riding a jump must not keep `forward_continuations` reporting a change on every round: `thread_edge` compares the edge it rebuilt against the edge it read, and under IEEE equality on a float literal a NaN is unequal to itself, so an untouched edge would read as rewritten and the fixpoint would run to its backstop. `Literal::Flt` is bitwise, and this pins the consequence — the second call over a settled module reports nothing.
#[test]
fn forwarding_a_nan_literal_settles_in_one_round() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let received = module.add_value(Some("received".into()));
    let target_body = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(received)],
    }));
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("target".into()),
            params: vec![received],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![Atom::Value(forwarded)],
    }));
    module.define_continuation(
        forwarding,
        Continuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let nan = Atom::Literal(Literal::Flt(Floating::from(f64::NAN)));
    let call = module.add_node(Node::ApplyCont(Edge {
        target: forwarding,
        args: vec![nan.clone()],
    }));
    let body = module.add_node(Node::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyCont(Edge { target: actual, args }))
            if *actual == target && args == std::slice::from_ref(&nan)
    ));
    assert!(
        !forward_continuations(&mut module),
        "a settled module must report no change"
    );
    module.verify().unwrap();
}

#[test]
fn rewrite_atoms_remaps_and_devirtualizes_a_closure_callee() {
    // The closure callee holds its target in a value that `visit_atoms_mut` never reaches. A forwarded value must follow (else the callee dangles when the original value is deleted), and a known function devirtualizes.
    let mut module = Module::new();
    let ret = module.reserve_continuation();
    let old = module.add_value(Some("old".into()));
    let new = module.add_value(Some("new".into()));
    let target = module.reserve_function();

    let value_call = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(old),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(old, Atom::Value(new))]),
    ));
    assert!(
        matches!(module.node(value_call), Some(Node::ApplyFun { callee: Callee::Closure(v), .. }) if *v == new),
        "a forwarded value keeps the closure callee pointing at a live value"
    );

    let fun_call = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(new),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(new, Atom::Fun(target))]),
    ));
    assert!(
        matches!(module.node(fun_call), Some(Node::ApplyFun { callee: Callee::Known(f), .. }) if *f == target),
        "a known function devirtualizes the closure call"
    );
}

/// `main(a)`: `t1 = (a, 1); p = t1.0; t2 = (p, 2); q = t2.0; return q`. One sweep forwards both projections and the return carries `a` — not `p`, which the same sweep deletes — because the replacements are collapsed through each other before anything is rewritten.
#[test]
fn forwards_a_chain_of_projections_in_one_sweep() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let t1 = module.add_value(Some("t1".into()));
    let p = module.add_value(Some("p".into()));
    let t2 = module.add_value(Some("t2".into()));
    let q = module.add_value(Some("q".into()));

    let deliver = module.add_node(Node::ApplyCont(Edge {
        target: entry_return,
        args: vec![Atom::Value(q)],
    }));
    let read_q = module.add_node(Node::LetIntrinsic {
        result: q,
        op: Intrinsic::TupleGet(0),
        args: vec![Atom::Value(t2)],
        next: deliver,
    });
    let build_t2 = module.add_node(Node::LetValue {
        result: t2,
        value: ValueExpr::Tuple(vec![
            Atom::Value(p),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ]),
        next: read_q,
    });
    let read_p = module.add_node(Node::LetIntrinsic {
        result: p,
        op: Intrinsic::TupleGet(0),
        args: vec![Atom::Value(t1)],
        next: build_t2,
    });
    let build_t1 = module.add_node(Node::LetValue {
        result: t1,
        value: ValueExpr::Tuple(vec![
            Atom::Value(a),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ]),
        next: read_p,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![a],
            return_cont: entry_return,
            body: build_t1,
            droppable: false,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(forward_aggregate_projections(&mut module));
    module.verify().unwrap();
    assert!(
        matches!(
            module.node(deliver),
            Some(Node::ApplyCont(Edge { args, .. })) if args == &[Atom::Value(a)]
        ),
        "the return carries the origin of the chain:\n{module}"
    );
    assert!(
        module.node(read_p).is_none() && module.node(read_q).is_none(),
        "both projections are spliced out"
    );
    assert!(
        !forward_aggregate_projections(&mut module),
        "and nothing is left for a second call"
    );
}

#[test]
fn identity_folds_forward_the_surviving_operand() {
    let cases = [
        (Intrinsic::NatAdd, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatAdd, Literal::Nat(Natural::from(0u32)), false),
        (Intrinsic::NatSub, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatMul, Literal::Nat(Natural::from(1u32)), true),
        (Intrinsic::NatMul, Literal::Nat(Natural::from(1u32)), false),
        (Intrinsic::NatDiv, Literal::Nat(Natural::from(1u32)), true),
        (Intrinsic::NatOr, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatXor, Literal::Nat(Natural::from(0u32)), false),
        (Intrinsic::NatShl, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::IntAdd, Literal::Int(Integer::from(0)), false),
        (Intrinsic::IntSub, Literal::Int(Integer::from(0)), true),
        (Intrinsic::IntMul, Literal::Int(Integer::from(1)), true),
        (Intrinsic::IntShr, Literal::Nat(Natural::from(0u32)), true),
    ];
    for (op, literal, literal_on_right) in cases {
        let x = ValueId(0);
        let args = if literal_on_right {
            vec![Atom::Value(x), Atom::Literal(literal.clone())]
        } else {
            vec![Atom::Literal(literal.clone()), Atom::Value(x)]
        };
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(
            fold_intrinsic_identities(&mut module),
            "{op:?} with {literal:?} must fold"
        );
        assert!(module.node(intrinsic).is_none(), "{op:?} binding survives");
        let returns_x =
            module.nodes().iter().flatten().any(
                |node| matches!(node, Node::ApplyCont(edge) if edge.args == vec![Atom::Value(x)]),
            );
        assert!(returns_x, "{op:?} must forward the surviving operand");
        module.verify().unwrap();
    }
}

#[test]
fn identity_folds_pin_absorbing_results() {
    let cases = [
        (
            Intrinsic::NatMul,
            Literal::Nat(Natural::from(0u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::NatAnd,
            Literal::Nat(Natural::from(0u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::NatRem,
            Literal::Nat(Natural::from(1u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::IntMul,
            Literal::Int(Integer::from(0)),
            Literal::Int(Integer::from(0)),
        ),
        (
            Intrinsic::IntRem,
            Literal::Int(Integer::from(1)),
            Literal::Int(Integer::from(0)),
        ),
    ];
    for (op, literal, expected) in cases {
        let x = ValueId(0);
        let args = vec![Atom::Value(x), Atom::Literal(literal.clone())];
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(
            fold_intrinsic_identities(&mut module),
            "{op:?} with {literal:?} must fold"
        );
        assert!(
            matches!(
                module.node(intrinsic),
                Some(Node::LetValue {
                    value: ValueExpr::Literal(pinned),
                    ..
                }) if *pinned == expected
            ),
            "{op:?} must pin {expected:?}"
        );
        module.verify().unwrap();
    }
}

#[test]
fn identity_folds_leave_traps_and_flt_untouched() {
    let x = ValueId(0);
    let cases = [
        (
            Intrinsic::NatDiv,
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Nat(Natural::from(0u32))),
            ],
        ),
        (
            Intrinsic::NatAdd,
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ],
        ),
        (
            Intrinsic::FltAdd(Rounding::TiesToEven),
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Flt(Floating::from(0.0))),
            ],
        ),
        (
            Intrinsic::FltMul(Rounding::TiesToEven),
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Flt(Floating::from(1.0))),
            ],
        ),
    ];
    for (op, args) in cases {
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(!fold_intrinsic_identities(&mut module), "{op:?} must stay");
        assert!(
            matches!(module.node(intrinsic), Some(Node::LetIntrinsic { .. })),
            "{op:?} binding must survive"
        );
    }
}

/// A join whose body calls the function its jump hands it, with the jump's other argument as the call's, and the one jump into it — handing over the known function `arm`, or, when `known` is false, a closure the entry holds in its parameter. Answers the jump, `arm` and the entry's return continuation.
fn calling_join(known: bool) -> (Module, NodeId, FunctionId, ContinuationId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let arm = module.reserve_function();
    let arm_return = module.reserve_continuation();
    let arm_param = module.add_value(Some("arm argument".into()));
    let arm_body = module.add_node(Node::ApplyCont(Edge {
        target: arm_return,
        args: vec![Atom::Value(arm_param)],
    }));
    module.define_function(
        arm,
        Function {
            debug_name: Some("arm".into()),
            params: vec![arm_param],
            return_cont: arm_return,
            body: arm_body,
            droppable: false,
        },
    );

    let join = module.reserve_continuation();
    let join_callee = module.add_value(Some("join callee".into()));
    let join_argument = module.add_value(Some("join argument".into()));
    let join_body = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(join_callee),
        args: vec![Atom::Value(join_argument)],
        return_to: entry_return,
    });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![join_callee, join_argument],
            body: join_body,
        },
    );

    let (entry_params, handed) = match known {
        true => (vec![], Atom::Fun(arm)),
        false => {
            let closure = module.add_value(Some("closure".into()));

            (vec![closure], Atom::Value(closure))
        }
    };
    let jump = module.add_node(Node::ApplyCont(Edge {
        target: join,
        args: vec![handed, Atom::Literal(Literal::Nat(Natural::from(7u32)))],
    }));
    let conts = module.add_node(Node::LetCont {
        continuations: vec![join],
        body: jump,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![arm],
        body: conts,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: entry_params,
            return_cont: entry_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    (module, jump, arm, entry_return)
}

/// The shape a convoy leaves once its evidence is erased: each arm jumps its function to a join that only calls it. The call is made at the jump instead, with the jump's other argument standing for the join's parameter and the join's return continuation kept, so the function has a known caller that inlining can consume.
#[test]
fn a_known_function_jumped_to_a_join_that_calls_it_is_called_at_the_jump() {
    let (mut module, jump, arm, entry_return) = calling_join(true);

    assert!(forward_calls(&mut module));
    assert!(matches!(
        module.node(jump),
        Some(Node::ApplyFun { callee: Callee::Known(callee), args, return_to })
            if *callee == arm
                && *return_to == entry_return
                && args == &[Atom::Literal(Literal::Nat(Natural::from(7u32)))]
    ));
    module.verify().unwrap();
}

/// A callee still held in a value gains nothing from moving: the call would stay indirect, and the node would only be copied into every jump.
#[test]
fn a_closure_value_jumped_to_a_join_that_calls_it_stays_a_jump() {
    let (mut module, jump, _, _) = calling_join(false);

    assert!(!forward_calls(&mut module));
    assert!(matches!(module.node(jump), Some(Node::ApplyCont(_))));
    module.verify().unwrap();
}
