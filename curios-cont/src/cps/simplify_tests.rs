//! Dead code, jump forwarding, atom rewriting, and the intrinsic identities the simplifier folds.

use {
    super::test_support::unary_intrinsic_module,
    crate::cps::simplify::{
        eliminate_dead_bindings, eliminate_dead_parameters, fold_intrinsic_identities,
        forward_aggregate_projections, forward_continuations, rewrite_atoms,
    },
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsic, CpsLiteral,
        CpsModule, CpsNode, CpsValueExpr, CpsValueId,
    },
    curios_num::Floating,
    std::collections::BTreeMap,
};

#[test]
fn dead_binding_elimination_preserves_traps_and_drops_total_literals() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    let dead_total = module.add_value(Some("dead total".into()));
    let total_node = module.add_node(CpsNode::LetIntrinsic {
        result: dead_total,
        op: CpsIntrinsic::NatEql,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ],
        next: return_node,
    });
    let dead_trap = module.add_value(Some("dead trap".into()));
    let trap_node = module.add_node(CpsNode::LetIntrinsic {
        result: dead_trap,
        op: CpsIntrinsic::NatDiv,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(0)),
        ],
        next: total_node,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body: trap_node,
        },
    );
    module.set_entry(entry);

    assert!(eliminate_dead_bindings(&mut module));
    assert!(module.node(total_node).is_none());
    assert!(matches!(
        module.node(trap_node),
        Some(CpsNode::LetIntrinsic {
            op: CpsIntrinsic::NatDiv,
            next,
            ..
        }) if *next == return_node
    ));
    module.verify().unwrap();
}

#[test]
fn dead_parameter_elimination_rewrites_known_calls() {
    let mut module = CpsModule::new();
    let main = module.reserve_function();
    let callee = module.reserve_function();
    let kept = module.add_value(Some("kept".into()));
    let removed = module.add_value(Some("removed".into()));
    let callee_return = module.reserve_continuation();
    let callee_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: callee_return,
        args: vec![CpsAtom::Value(kept)],
    }));
    module.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![kept, removed],
            return_cont: callee_return,
            body: callee_body,
        },
    );
    let main_return = module.reserve_continuation();
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ],
        return_to: main_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        main,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
        },
    );
    module.set_entry(main);

    assert!(eliminate_dead_parameters(&mut module));
    assert_eq!(module.function(callee).unwrap().params, vec![kept]);
    assert!(matches!(
        module.node(call),
        Some(CpsNode::ApplyFun { args, .. })
            if args == &[CpsAtom::Literal(CpsLiteral::Nat(1))]
    ));
    module.verify().unwrap();
}

#[test]
fn forwarding_composes_jump_arguments_instead_of_only_retargeting() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let target_left = module.add_value(Some("target left".into()));
    let target_right = module.add_value(Some("target right".into()));
    let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(target_right)],
    }));
    module.define_continuation(
        target,
        CpsContinuation {
            debug_name: Some("target".into()),
            params: vec![target_left, target_right],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Value(forwarded),
        ],
    }));
    module.define_continuation(
        forwarding,
        CpsContinuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: forwarding,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::ApplyCont(CpsEdge { target: actual, args }))
            if *actual == target
                && args == &[
                    CpsAtom::Literal(CpsLiteral::Nat(1)),
                    CpsAtom::Literal(CpsLiteral::Nat(7)),
                ]
    ));
    module.verify().unwrap();
}

/// A NaN literal riding a jump used to keep `forward_continuations` reporting a change on every round: `thread_edge` compared the edge it rebuilt against the edge it read, and under IEEE equality on an `f32` literal a NaN is unequal to itself, so an untouched edge read as rewritten and the fixpoint ran to its backstop. `CpsLiteral::Flt` is bitwise now, and this pins the consequence — the second call over a settled module reports nothing.
#[test]
fn forwarding_a_nan_literal_settles_in_one_round() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let target = module.reserve_continuation();
    let received = module.add_value(Some("received".into()));
    let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(received)],
    }));
    module.define_continuation(
        target,
        CpsContinuation {
            debug_name: Some("target".into()),
            params: vec![received],
            body: target_body,
        },
    );
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let forwarding_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target,
        args: vec![CpsAtom::Value(forwarded)],
    }));
    module.define_continuation(
        forwarding,
        CpsContinuation {
            debug_name: Some("forwarding".into()),
            params: vec![forwarded],
            body: forwarding_body,
        },
    );
    let nan = CpsAtom::Literal(CpsLiteral::Flt(Floating::from_f32(f32::NAN)));
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: forwarding,
        args: vec![nan.clone()],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);

    assert!(forward_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::ApplyCont(CpsEdge { target: actual, args }))
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
    let mut module = CpsModule::new();
    let ret = module.reserve_continuation();
    let old = module.add_value(Some("old".into()));
    let new = module.add_value(Some("new".into()));
    let target = module.reserve_function();

    let value_call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(old),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(old, CpsAtom::Value(new))]),
    ));
    assert!(
        matches!(module.node(value_call), Some(CpsNode::ApplyFun { callee: CpsCallee::Closure(v), .. }) if *v == new),
        "a forwarded value keeps the closure callee pointing at a live value"
    );

    let fun_call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(new),
        args: vec![],
        return_to: ret,
    });
    assert!(rewrite_atoms(
        &mut module,
        &BTreeMap::from([(new, CpsAtom::Fun(target))]),
    ));
    assert!(
        matches!(module.node(fun_call), Some(CpsNode::ApplyFun { callee: CpsCallee::Known(f), .. }) if *f == target),
        "a known function devirtualizes the closure call"
    );
}

/// `main(a)`: `t1 = (a, 1); p = t1.0; t2 = (p, 2); q = t2.0; return q`. One sweep forwards both projections and the return carries `a` — not `p`, which the same sweep deletes — because the replacements are collapsed through each other before anything is rewritten.
#[test]
fn forwards_a_chain_of_projections_in_one_sweep() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let t1 = module.add_value(Some("t1".into()));
    let p = module.add_value(Some("p".into()));
    let t2 = module.add_value(Some("t2".into()));
    let q = module.add_value(Some("q".into()));

    let deliver = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: entry_return,
        args: vec![CpsAtom::Value(q)],
    }));
    let read_q = module.add_node(CpsNode::LetIntrinsic {
        result: q,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(t2)],
        next: deliver,
    });
    let build_t2 = module.add_node(CpsNode::LetValue {
        result: t2,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Value(p),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ]),
        next: read_q,
    });
    let read_p = module.add_node(CpsNode::LetIntrinsic {
        result: p,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(t1)],
        next: build_t2,
    });
    let build_t1 = module.add_node(CpsNode::LetValue {
        result: t1,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Value(a),
            CpsAtom::Literal(CpsLiteral::Nat(1)),
        ]),
        next: read_p,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![a],
            return_cont: entry_return,
            body: build_t1,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(forward_aggregate_projections(&mut module));
    module.verify().unwrap();
    assert!(
        matches!(
            module.node(deliver),
            Some(CpsNode::ApplyCont(CpsEdge { args, .. })) if args == &[CpsAtom::Value(a)]
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
        (CpsIntrinsic::NatAdd, CpsLiteral::Nat(0), true),
        (CpsIntrinsic::NatAdd, CpsLiteral::Nat(0), false),
        (CpsIntrinsic::NatSub, CpsLiteral::Nat(0), true),
        (CpsIntrinsic::NatMul, CpsLiteral::Nat(1), true),
        (CpsIntrinsic::NatMul, CpsLiteral::Nat(1), false),
        (CpsIntrinsic::NatDiv, CpsLiteral::Nat(1), true),
        (CpsIntrinsic::NatOr, CpsLiteral::Nat(0), true),
        (CpsIntrinsic::NatXor, CpsLiteral::Nat(0), false),
        (CpsIntrinsic::NatShl, CpsLiteral::Nat(0), true),
        (CpsIntrinsic::IntAdd, CpsLiteral::Int(0), false),
        (CpsIntrinsic::IntSub, CpsLiteral::Int(0), true),
        (CpsIntrinsic::IntMul, CpsLiteral::Int(1), true),
        (CpsIntrinsic::IntShr, CpsLiteral::Int(0), true),
    ];
    for (op, literal, literal_on_right) in cases {
        let x = CpsValueId(0);
        let args = if literal_on_right {
            vec![CpsAtom::Value(x), CpsAtom::Literal(literal.clone())]
        } else {
            vec![CpsAtom::Literal(literal.clone()), CpsAtom::Value(x)]
        };
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(
            fold_intrinsic_identities(&mut module),
            "{op:?} with {literal:?} must fold"
        );
        assert!(module.node(intrinsic).is_none(), "{op:?} binding survives");
        let returns_x = module.nodes().iter().flatten().any(
            |node| matches!(node, CpsNode::ApplyCont(edge) if edge.args == vec![CpsAtom::Value(x)]),
        );
        assert!(returns_x, "{op:?} must forward the surviving operand");
        module.verify().unwrap();
    }
}

#[test]
fn identity_folds_pin_absorbing_results() {
    let cases = [
        (CpsIntrinsic::NatMul, CpsLiteral::Nat(0), CpsLiteral::Nat(0)),
        (CpsIntrinsic::NatAnd, CpsLiteral::Nat(0), CpsLiteral::Nat(0)),
        (CpsIntrinsic::NatRem, CpsLiteral::Nat(1), CpsLiteral::Nat(0)),
        (CpsIntrinsic::IntMul, CpsLiteral::Int(0), CpsLiteral::Int(0)),
        (CpsIntrinsic::IntRem, CpsLiteral::Int(1), CpsLiteral::Int(0)),
    ];
    for (op, literal, expected) in cases {
        let x = CpsValueId(0);
        let args = vec![CpsAtom::Value(x), CpsAtom::Literal(literal.clone())];
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(
            fold_intrinsic_identities(&mut module),
            "{op:?} with {literal:?} must fold"
        );
        assert!(
            matches!(
                module.node(intrinsic),
                Some(CpsNode::LetValue {
                    value: CpsValueExpr::Literal(pinned),
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
    let x = CpsValueId(0);
    let cases = [
        (
            CpsIntrinsic::NatDiv,
            vec![CpsAtom::Value(x), CpsAtom::Literal(CpsLiteral::Nat(0))],
        ),
        (
            CpsIntrinsic::NatAdd,
            vec![CpsAtom::Value(x), CpsAtom::Literal(CpsLiteral::Nat(2))],
        ),
        (
            CpsIntrinsic::FltAdd,
            vec![
                CpsAtom::Value(x),
                CpsAtom::Literal(CpsLiteral::Flt(Floating::from_f32(0.0))),
            ],
        ),
        (
            CpsIntrinsic::FltMul,
            vec![
                CpsAtom::Value(x),
                CpsAtom::Literal(CpsLiteral::Flt(Floating::from_f32(1.0))),
            ],
        ),
    ];
    for (op, args) in cases {
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(!fold_intrinsic_identities(&mut module), "{op:?} must stay");
        assert!(
            matches!(module.node(intrinsic), Some(CpsNode::LetIntrinsic { .. })),
            "{op:?} binding must survive"
        );
    }
}
