use {
    crate::cps::{
        analysis::{analyze_sccs, function_nodes, known_values},
        contify::contify_calls,
        evaluate::evaluate,
        inline::{inline_known_calls, inline_single_use_continuations},
        optimize::{
            BRANCH_CLONE_LIMIT, BRANCH_SPECIALIZATION_GROWTH_LIMIT, SCC_CLONE_LIMIT,
            SCC_CLONE_NODE_LIMIT, optimize,
        },
        simplify::{
            dissolve_rec_init, eliminate_dead_bindings, eliminate_dead_parameters,
            forward_aggregate_projections, forward_continuations, rewrite_atoms, simplify_nodes,
        },
        specialize::{specialize_call_patterns, specialize_scc_calls},
    },
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral,
        CpsModule, CpsNode, CpsNodeId, CpsPrimOp, CpsValueExpr, CpsValueId, atoms,
    },
    std::collections::{BTreeMap, BTreeSet},
};

fn call_graph(edges: &[(u32, &[u32])]) -> BTreeMap<CpsFunId, BTreeSet<CpsFunId>> {
    edges
        .iter()
        .map(|(function, successors)| {
            (
                CpsFunId(*function),
                successors.iter().map(|s| CpsFunId(*s)).collect(),
            )
        })
        .collect()
}

#[test]
fn sccs_group_cycles_and_stay_deterministic() {
    // 0 <-> 1 form a cycle; 1 -> 2 and 2 -> 2 leaves 2 a self-looping singleton; 3 is isolated.
    let graph = call_graph(&[(0, &[1]), (1, &[0, 2]), (2, &[2]), (3, &[])]);
    let sccs = analyze_sccs(&graph);

    let component = |function: u32| sccs.component_of[&CpsFunId(function)];
    assert_eq!(component(0), component(1));
    assert_ne!(component(0), component(2));
    assert_ne!(component(2), component(3));
    assert_eq!(sccs.members.len(), 3);
    assert_eq!(
        sccs.members[component(0)],
        vec![CpsFunId(0), CpsFunId(1)],
        "cycle members are reported in CpsFunId order"
    );
    assert_eq!(sccs.members[component(2)], vec![CpsFunId(2)]);

    let again = analyze_sccs(&graph);
    assert_eq!(sccs.component_of, again.component_of);
    assert_eq!(sccs.members, again.members);
}

#[test]
fn preserves_traps_and_folds_exact_u32_nat_add() {
    assert_eq!(
        evaluate(
            CpsPrimOp::NatAdd,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(20)),
                CpsAtom::Literal(CpsLiteral::Nat(22)),
            ],
        ),
        Some(CpsLiteral::Nat(42))
    );
    // The numeric law: the folder computes in exact u32; the i31 envelope is the backend's problem (an out-of-range literal traps at materialization).
    assert_eq!(
        evaluate(
            CpsPrimOp::NatAdd,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(0x7fff_ffff)),
                CpsAtom::Literal(CpsLiteral::Nat(1)),
            ],
        ),
        Some(CpsLiteral::Nat(0x8000_0000))
    );
    assert_eq!(
        evaluate(
            CpsPrimOp::NatDiv,
            &[
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
        ),
        None
    );
}

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
    let total_node = module.add_node(CpsNode::LetPrim {
        result: dead_total,
        op: CpsPrimOp::NatEql,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ],
        next: return_node,
    });
    let dead_trap = module.add_value(Some("dead trap".into()));
    let trap_node = module.add_node(CpsNode::LetPrim {
        result: dead_trap,
        op: CpsPrimOp::NatDiv,
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
        Some(CpsNode::LetPrim {
            op: CpsPrimOp::NatDiv,
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
fn known_continuation_values_are_not_substituted_across_scopes() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let seed = module.add_value(Some("seed".into()));
    let forwarding = module.reserve_continuation();
    let forwarded = module.add_value(Some("forwarded".into()));
    let target = module.reserve_continuation();
    let target_param = module.add_value(Some("target".into()));
    let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(target_param)],
    }));
    module.define_continuation(
        target,
        CpsContinuation {
            debug_name: Some("target".into()),
            params: vec![target_param],
            body: target_body,
        },
    );
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
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: forwarding,
        args: vec![CpsAtom::Value(seed)],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![forwarding, target],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![seed],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);

    optimize(&mut module);

    assert!(
        module
            .nodes()
            .iter()
            .flatten()
            .flat_map(atoms)
            .all(|atom| atom != &CpsAtom::Value(forwarded))
    );
    module.verify().unwrap();
}

#[test]
fn known_value_analysis_leaves_local_continuation_parameters_to_beta_reduction() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let continuation = module.reserve_continuation();
    let parameter = module.add_value(None);
    let continuation_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(parameter)],
    }));
    module.define_continuation(
        continuation,
        CpsContinuation {
            debug_name: None,
            params: vec![parameter],
            body: continuation_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: continuation,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![continuation],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(!known_values(&module).contains_key(&parameter));
    assert!(inline_single_use_continuations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
            if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
    ));
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

#[test]
fn continuation_beta_rewrites_parameters_captured_by_nested_functions() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let continuation = module.reserve_continuation();
    let captured = module.add_value(Some("captured".into()));

    let nested = module.reserve_function();
    let nested_return = module.reserve_continuation();
    let nested_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: nested_return,
        args: vec![CpsAtom::Value(captured)],
    }));
    module.define_function(
        nested,
        CpsFunction {
            debug_name: Some("nested".into()),
            params: vec![],
            return_cont: nested_return,
            body: nested_body,
        },
    );
    let return_nested = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Fun(nested)],
    }));
    let continuation_body = module.add_node(CpsNode::LetFun {
        functions: vec![nested],
        body: return_nested,
    });
    module.define_continuation(
        continuation,
        CpsContinuation {
            debug_name: Some("capture scope".into()),
            params: vec![captured],
            body: continuation_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: continuation,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![continuation],
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

    assert!(inline_single_use_continuations(&mut module));
    assert!(matches!(
        module.node(nested_body),
        Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
            if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
    ));
    module.verify().unwrap();
}

#[test]
fn known_call_inlining_clones_recursive_local_continuations() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let callee_param = module.add_value(None);
    let local_cont = module.reserve_continuation();
    let local_param = module.add_value(None);
    let local_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: local_cont,
        args: vec![CpsAtom::Value(local_param)],
    }));
    module.define_continuation(
        local_cont,
        CpsContinuation {
            debug_name: None,
            params: vec![local_param],
            body: local_body,
        },
    );
    let enter_local = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: local_cont,
        args: vec![CpsAtom::Value(callee_param)],
    }));
    let callee_body = module.add_node(CpsNode::LetCont {
        continuations: vec![local_cont],
        body: enter_local,
    });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: None,
            params: vec![callee_param],
            return_cont: callee_return,
            body: callee_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: None,
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(inline_known_calls(&mut module));
    assert!(matches!(
        module.node(call),
        Some(CpsNode::LetCont { continuations, .. }) if continuations != &[local_cont]
    ));
    module.verify().unwrap();
}

#[test]
fn contifies_a_single_entry_tail_loop_and_bridges_switch_returns() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let loop_param = module.add_value(Some("loop argument".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur argument".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        CpsContinuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(loop_param),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(loop_param)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(loop_param)],
        }),
    });
    let loop_body = module.add_node(CpsNode::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        CpsFunction {
            debug_name: Some("loop".into()),
            params: vec![loop_param],
            return_cont: loop_return,
            body: loop_body,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_function],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);

    optimize(&mut module);

    assert!(module.function(loop_function).is_none());
    let loop_cont = module
        .continuations()
        .iter()
        .flatten()
        .find(|continuation| continuation.debug_name.as_deref() == Some("loop"))
        .unwrap();
    assert_eq!(loop_cont.params, vec![loop_param]);
    let return_bridge = module
        .continuations()
        .iter()
        .flatten()
        .find(|continuation| continuation.debug_name.as_deref() == Some("contified return"))
        .unwrap();
    assert!(matches!(
        module.node(return_bridge.body),
        Some(CpsNode::ApplyCont(CpsEdge { target, .. })) if *target == entry_return
    ));
    let CpsNode::Switch { cases, .. } = module.node(switch).unwrap() else {
        panic!("loop switch changed shape")
    };
    assert_ne!(cases[&0].target, entry_return);
    module.verify().unwrap();
}

#[test]
fn scc_invariant_known_argument_propagates_into_recursive_member() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    // A trivial helper used only as an invariant first-class argument.
    let helper = module.reserve_function();
    let helper_return = module.reserve_continuation();
    let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: helper_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    module.define_function(
        helper,
        CpsFunction {
            debug_name: Some("helper".into()),
            params: vec![],
            return_cont: helper_return,
            body: helper_body,
        },
    );

    // loop(invariant, counter): the recursive call forwards `invariant` unchanged and replaces `counter`, so `invariant` is loop-invariant and `counter` is not.
    let loop_function = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let invariant = module.add_value(Some("invariant".into()));
    let counter = module.add_value(Some("counter".into()));
    let recur = module.reserve_continuation();
    let recur_param = module.add_value(Some("recur".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Value(invariant), CpsAtom::Value(recur_param)],
        return_to: loop_return,
    });
    module.define_continuation(
        recur,
        CpsContinuation {
            debug_name: Some("recur".into()),
            params: vec![recur_param],
            body: recur_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(counter),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(counter)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(counter)],
        }),
    });
    let loop_body = module.add_node(CpsNode::LetCont {
        continuations: vec![recur],
        body: switch,
    });
    module.define_function(
        loop_function,
        CpsFunction {
            debug_name: Some("loop".into()),
            params: vec![invariant, counter],
            return_cont: loop_return,
            body: loop_body,
        },
    );

    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_function),
        args: vec![CpsAtom::Fun(helper), CpsAtom::Literal(CpsLiteral::Nat(3))],
        return_to: entry_return,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_function, helper],
        body: call,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    let known = known_values(&module);
    assert_eq!(
        known.get(&invariant),
        Some(&CpsAtom::Fun(helper)),
        "the invariant recursive parameter is recognized as the known function"
    );
    assert!(
        !known.contains_key(&counter),
        "the varying recursive parameter stays unknown"
    );
}

struct PolymorphicLoop {
    module: CpsModule,
    call1: CpsNodeId,
    call2: CpsNodeId,
    loop_fn: CpsFunId,
}

/// Build `loop(op, n)` which indirectly calls `op(n)` and recurses forwarding `op`, called from `entry` as `loop(add, 3)` then `loop(second, 4)`. When `second` differs from `add` the two contexts disagree. `padding` prepends dead `LetPrim` nodes to `loop`'s body to inflate its node count.
fn polymorphic_loop(second_is_mul: bool, padding: usize) -> PolymorphicLoop {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let trivial = |module: &mut CpsModule, name: &str| {
        let function = module.reserve_function();
        let function_return = module.reserve_continuation();
        let param = module.add_value(Some(format!("{name} x")));
        let function_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: function_return,
            args: vec![CpsAtom::Value(param)],
        }));
        module.define_function(
            function,
            CpsFunction {
                debug_name: Some(name.into()),
                params: vec![param],
                return_cont: function_return,
                body: function_body,
            },
        );
        function
    };
    let add = trivial(&mut module, "add");
    let mul = trivial(&mut module, "mul");

    let loop_fn = module.reserve_function();
    let loop_return = module.reserve_continuation();
    let op = module.add_value(Some("op".into()));
    let n = module.add_value(Some("n".into()));
    let after = module.reserve_continuation();
    let after_r = module.add_value(Some("after r".into()));
    let after_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Value(op), CpsAtom::Value(after_r)],
        return_to: loop_return,
    });
    module.define_continuation(
        after,
        CpsContinuation {
            debug_name: Some("after".into()),
            params: vec![after_r],
            body: after_body,
        },
    );
    let recur = module.reserve_continuation();
    let recur_m = module.add_value(Some("recur m".into()));
    let recur_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(op),
        args: vec![CpsAtom::Value(recur_m)],
        return_to: after,
    });
    module.define_continuation(
        recur,
        CpsContinuation {
            debug_name: Some("recur".into()),
            params: vec![recur_m],
            body: recur_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(n),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: loop_return,
                args: vec![CpsAtom::Value(n)],
            },
        )]),
        default: Some(CpsEdge {
            target: recur,
            args: vec![CpsAtom::Value(n)],
        }),
    });
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![recur, after],
        body: switch,
    });
    let mut loop_body = scope;
    for _ in 0..padding {
        let dead = module.add_value(None);
        loop_body = module.add_node(CpsNode::LetPrim {
            result: dead,
            op: CpsPrimOp::NatAdd,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
            next: loop_body,
        });
    }
    module.define_function(
        loop_fn,
        CpsFunction {
            debug_name: Some("loop".into()),
            params: vec![op, n],
            return_cont: loop_return,
            body: loop_body,
        },
    );

    let second = if second_is_mul { mul } else { add };
    let x1 = module.add_value(Some("x1".into()));
    let call2 = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Fun(second), CpsAtom::Literal(CpsLiteral::Nat(4))],
        return_to: entry_return,
    });
    let k1 = module.reserve_continuation();
    module.define_continuation(
        k1,
        CpsContinuation {
            debug_name: Some("k1".into()),
            params: vec![x1],
            body: call2,
        },
    );
    let call1 = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(loop_fn),
        args: vec![CpsAtom::Fun(add), CpsAtom::Literal(CpsLiteral::Nat(3))],
        return_to: k1,
    });
    let outer = module.add_node(CpsNode::LetCont {
        continuations: vec![k1],
        body: call1,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![loop_fn, add, mul],
        body: outer,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    PolymorphicLoop {
        module,
        call1,
        call2,
        loop_fn,
    }
}

fn known_callee(module: &CpsModule, node: CpsNodeId) -> CpsFunId {
    match module.node(node).unwrap() {
        CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            ..
        } => *callee,
        _ => panic!("call site changed shape"),
    }
}

#[test]
fn specializes_a_polymorphic_recursive_scc_per_call_context() {
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, 0);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        specialize_scc_calls(&mut module, &mut budget),
        "a disagreeing call context is specialized"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT - 1, "one clone consumed the budget");
    module.verify().unwrap();

    let first = known_callee(&module, call1);
    let second = known_callee(&module, call2);
    assert_ne!(
        first, second,
        "the two contexts now call different functions"
    );
    assert!(
        first == loop_fn || second == loop_fn,
        "one context keeps the original polymorphic function"
    );
    assert!(
        first != loop_fn || second != loop_fn,
        "one context is repointed to a fresh clone"
    );
}

#[test]
fn agreeing_call_contexts_are_not_specialized() {
    // Both sites pass `add`, so the module-wide analysis already knows the argument and cloning would add nothing.
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(false, 0);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "no clone is made when callers agree"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT);
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_respects_the_clone_budget() {
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, 0);

    let mut budget = 0;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "an exhausted budget makes no clone"
    );
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_respects_the_node_budget() {
    // Inflate `loop` past SCC_CLONE_NODE_LIMIT live nodes.
    let PolymorphicLoop {
        mut module,
        call1,
        call2,
        loop_fn,
    } = polymorphic_loop(true, SCC_CLONE_NODE_LIMIT + 1);

    let mut budget = SCC_CLONE_LIMIT;
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "an oversized SCC is not cloned"
    );
    assert_eq!(budget, SCC_CLONE_LIMIT);
    assert_eq!(known_callee(&module, call1), loop_fn);
    assert_eq!(known_callee(&module, call2), loop_fn);
}

#[test]
fn specialization_is_deterministic() {
    let run = || {
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            ..
        } = polymorphic_loop(true, 0);
        let mut budget = SCC_CLONE_LIMIT;
        specialize_scc_calls(&mut module, &mut budget);
        (
            known_callee(&module, call1).0,
            known_callee(&module, call2).0,
        )
    };
    assert_eq!(run(), run(), "specialization output is a pure function");
}

#[test]
fn optimization_specializes_away_the_polymorphic_indirect_call() {
    // With each caller peeled into its own clone, invariant-known propagation resolves every `op` to a direct callee, leaving no closure calls.
    let PolymorphicLoop { mut module, .. } = polymorphic_loop(true, 0);
    optimize(&mut module);
    module.verify().unwrap();
    assert!(
        module.nodes().iter().flatten().all(|node| !matches!(
            node,
            CpsNode::ApplyFun {
                callee: CpsCallee::Closure(_),
                ..
            }
        )),
        "specialization turned every recursive indirect call into a direct call"
    );
}

// Build `helper(x) = x`, non-escaping, called from `entry` at one or two external sites. Returns the module and the helper function.
fn helper_called(two_sites: bool) -> (CpsModule, CpsFunId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let helper = module.reserve_function();
    let helper_return = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: helper_return,
        args: vec![CpsAtom::Value(x)],
    }));
    module.define_function(
        helper,
        CpsFunction {
            debug_name: Some("helper".into()),
            params: vec![x],
            return_cont: helper_return,
            body: helper_body,
        },
    );

    let inner = if two_sites {
        let call2 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
            return_to: entry_return,
        });
        let param = module.add_value(None);
        let bridge = module.reserve_continuation();
        module.define_continuation(
            bridge,
            CpsContinuation {
                debug_name: None,
                params: vec![param],
                body: call2,
            },
        );
        let call1 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            return_to: bridge,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![bridge],
            body: call1,
        })
    } else {
        module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(helper),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            return_to: entry_return,
        })
    };
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![helper],
        body: inner,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, helper)
}

#[test]
fn contifies_a_nonrecursive_single_call_function() {
    let (mut module, helper) = helper_called(false);
    assert!(
        contify_calls(&mut module),
        "the single-call helper is contified"
    );
    assert!(
        module.function(helper).is_none(),
        "the contified function is replaced by a local continuation"
    );
    module.verify().unwrap();
}

#[test]
fn does_not_contify_a_multi_site_function() {
    // Two return contexts: single-site placement cannot cover both, so this is left for common-dominator contification in the machine CFG.
    let (mut module, helper) = helper_called(true);
    assert!(
        !contify_calls(&mut module),
        "a function with two call sites is not contified here"
    );
    assert!(module.function(helper).is_some());
}

// Build `main` whose body is a `RecInit` over `f` and computed value `v`. `v` is produced by a `rec/v` continuation and returned at the ready point. When `captures` is set, `f` forward-references `v` (a live mixed knot); otherwise `f` is independent and the knot is already broken.
fn rec_init_module(captures: bool) -> (CpsModule, CpsNodeId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let f = module.reserve_function();
    let f_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let v = module.add_value(Some("v".into()));
    let f_result = if captures {
        CpsAtom::Value(v)
    } else {
        CpsAtom::Value(a)
    };
    let f_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: f_return,
        args: vec![f_result],
    }));
    module.define_function(
        f,
        CpsFunction {
            debug_name: Some("f".into()),
            params: vec![a],
            return_cont: f_return,
            body: f_body,
        },
    );

    let ready = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: entry_return,
        args: vec![CpsAtom::Value(v)],
    }));
    let rec_v = module.reserve_continuation();
    module.define_continuation(
        rec_v,
        CpsContinuation {
            debug_name: Some("rec/v".into()),
            params: vec![v],
            body: ready,
        },
    );
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: rec_v,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
    }));
    let init_body = module.add_node(CpsNode::LetCont {
        continuations: vec![rec_v],
        body: enter,
    });
    let rec_init = module.add_node(CpsNode::RecInit {
        functions: vec![f],
        values: vec![v],
        ready,
        body: init_body,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body: rec_init,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, rec_init)
}

#[test]
fn dissolves_a_broken_recursive_initializer() {
    let (mut module, rec_init) = rec_init_module(false);
    assert!(
        dissolve_rec_init(&mut module),
        "a knot no member still captures dissolves"
    );
    assert!(
        matches!(module.node(rec_init), Some(CpsNode::LetFun { .. })),
        "the initializer becomes an ordinary function group"
    );
    module.verify().unwrap();
}

#[test]
fn retains_a_live_recursive_initializer() {
    let (mut module, rec_init) = rec_init_module(true);
    assert!(
        !dissolve_rec_init(&mut module),
        "a member still capturing a computed value keeps the fallback"
    );
    assert!(matches!(
        module.node(rec_init),
        Some(CpsNode::RecInit { .. })
    ));
}

// Build `main` calling a non-recursive `consume(t)` once per entry in `sites`. `consume` projects the tag and a field out of its tuple parameter and switches on the tag, so a known tagged tuple at a call site unlocks the fold. Each site `i` passes the tuple `(sites[i], i)`. `padding` pads `consume` with dead bindings to grow its live-node count. Returns the module, the call node per site (in `sites` order), and `consume`.
fn tagged_consumer(padding: usize, sites: &[u32]) -> (CpsModule, Vec<CpsNodeId>, CpsFunId) {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let consume = module.reserve_function();
    let consume_return = module.reserve_continuation();
    let t = module.add_value(Some("t".into()));
    let tag = module.add_value(Some("tag".into()));
    let val = module.add_value(Some("val".into()));
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: consume_return,
                    args: vec![CpsAtom::Value(val)],
                },
            ),
            (
                1,
                CpsEdge {
                    target: consume_return,
                    args: vec![CpsAtom::Literal(CpsLiteral::Nat(999))],
                },
            ),
        ]),
        default: Some(CpsEdge {
            target: consume_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }),
    });
    let project_val = module.add_node(CpsNode::LetPrim {
        result: val,
        op: CpsPrimOp::TplGet(1),
        args: vec![CpsAtom::Value(t)],
        next: switch,
    });
    let mut consume_body = module.add_node(CpsNode::LetPrim {
        result: tag,
        op: CpsPrimOp::TplGet(0),
        args: vec![CpsAtom::Value(t)],
        next: project_val,
    });
    for _ in 0..padding {
        let dead = module.add_value(None);
        consume_body = module.add_node(CpsNode::LetPrim {
            result: dead,
            op: CpsPrimOp::NatAdd,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
            next: consume_body,
        });
    }
    module.define_function(
        consume,
        CpsFunction {
            debug_name: Some("consume".into()),
            params: vec![t],
            return_cont: consume_return,
            body: consume_body,
        },
    );

    // Build the call chain forward so the search visits `sites[0]` first. Each site's return continuation is introduced by its own `LetCont`, and returning from site `i` runs site `i + 1`.
    let count = sites.len();
    let results: Vec<CpsValueId> = (0..count)
        .map(|i| module.add_value(Some(format!("r{i}"))))
        .collect();
    let ctors: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let calls: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let scopes: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
    let conts: Vec<CpsContId> = (0..count).map(|_| module.reserve_continuation()).collect();
    let tail = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: entry_return,
        args: vec![match results.last() {
            Some(&last) => CpsAtom::Value(last),
            None => CpsAtom::Literal(CpsLiteral::Nat(0)),
        }],
    }));
    for i in 0..count {
        let value = module.add_value(Some(format!("v{i}")));
        module.define_node(
            ctors[i],
            CpsNode::LetValue {
                result: value,
                value: CpsValueExpr::Tuple(vec![
                    CpsAtom::Literal(CpsLiteral::Nat(sites[i])),
                    CpsAtom::Literal(CpsLiteral::Nat(i as u32)),
                ]),
                next: calls[i],
            },
        );
        module.define_node(
            calls[i],
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(consume),
                args: vec![CpsAtom::Value(value)],
                return_to: conts[i],
            },
        );
        module.define_node(
            scopes[i],
            CpsNode::LetCont {
                continuations: vec![conts[i]],
                body: ctors[i],
            },
        );
        let next = if i + 1 < count { scopes[i + 1] } else { tail };
        module.define_continuation(
            conts[i],
            CpsContinuation {
                debug_name: Some(format!("k{i}")),
                params: vec![results[i]],
                body: next,
            },
        );
    }
    let first = scopes.first().copied().unwrap_or(tail);
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![consume],
        body: first,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, calls, consume)
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

fn has_switch(module: &CpsModule, function: CpsFunId) -> bool {
    function_nodes(module, function)
        .iter()
        .any(|&id| matches!(module.node(id), Some(CpsNode::Switch { .. })))
}

#[test]
fn specializes_a_constructor_argument_and_collapses_the_switch() {
    let (mut module, calls, consume) = tagged_consumer(0, &[0]);
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(
        specialize_call_patterns(&mut module, &mut budget),
        "a known tagged-tuple argument is specialized"
    );
    assert_eq!(
        budget,
        BRANCH_CLONE_LIMIT - 1,
        "one clone consumed the budget"
    );
    module.verify().unwrap();

    let clone = known_callee(&module, calls[0]);
    assert_ne!(clone, consume, "the call is repointed to a fresh clone");
    assert!(
        has_switch(&module, consume),
        "the general function keeps its switch"
    );

    // The rebuilt constructor lets the existing folds collapse the switch.
    while forward_aggregate_projections(&mut module) | simplify_nodes(&mut module) {}
    assert!(
        !has_switch(&module, clone),
        "projection and known-switch folding collapse the clone's dispatch"
    );
    module.verify().unwrap();
}

#[test]
fn equivalent_constructor_sites_share_one_clone() {
    // Two tag-0 sites match one pattern; a tag-1 site is a different pattern.
    let (mut module, calls, consume) = tagged_consumer(0, &[0, 0, 1]);
    let before = module.functions().iter().flatten().count();
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(specialize_call_patterns(&mut module, &mut budget));
    module.verify().unwrap();

    let clone = known_callee(&module, calls[0]);
    assert_ne!(clone, consume);
    assert_eq!(
        known_callee(&module, calls[1]),
        clone,
        "an equivalent site reuses the one clone"
    );
    assert_eq!(
        known_callee(&module, calls[2]),
        consume,
        "a non-matching pattern keeps the original function"
    );
    let after = module.functions().iter().flatten().count();
    assert_eq!(after, before + 1, "exactly one clone is created");
}

#[test]
fn call_pattern_specialization_respects_the_growth_budget() {
    // Inflate `consume` past BRANCH_SPECIALIZATION_GROWTH_LIMIT live nodes.
    let (mut module, calls, consume) =
        tagged_consumer(BRANCH_SPECIALIZATION_GROWTH_LIMIT + 1, &[0]);
    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(
        !specialize_call_patterns(&mut module, &mut budget),
        "an oversized callee is not specialized"
    );
    assert_eq!(budget, BRANCH_CLONE_LIMIT);
    assert_eq!(known_callee(&module, calls[0]), consume);
}

#[test]
fn call_pattern_specialization_respects_the_clone_budget() {
    let (mut module, calls, consume) = tagged_consumer(0, &[0]);
    let mut budget = 0;
    assert!(
        !specialize_call_patterns(&mut module, &mut budget),
        "an exhausted budget makes no clone"
    );
    assert_eq!(known_callee(&module, calls[0]), consume);
}

#[test]
fn call_pattern_specialization_is_deterministic() {
    let run = || {
        let (mut module, calls, _) = tagged_consumer(0, &[0, 0]);
        let mut budget = BRANCH_CLONE_LIMIT;
        specialize_call_patterns(&mut module, &mut budget);
        (
            known_callee(&module, calls[0]).0,
            known_callee(&module, calls[1]).0,
        )
    };
    assert_eq!(run(), run(), "specialization output is a pure function");
}

#[test]
fn optimization_eliminates_a_constructor_dispatch() {
    // A multi-site, oversized-for-inlining consumer: only specialization can resolve the tagged dispatch, and folding then removes every switch.
    let (mut module, _, _) = tagged_consumer(8, &[0, 0]);
    optimize(&mut module);
    module.verify().unwrap();
    assert!(
        module
            .nodes()
            .iter()
            .flatten()
            .all(|node| !matches!(node, CpsNode::Switch { .. })),
        "specialization and folding leave no residual tag dispatch"
    );
}

#[test]
fn specialization_peels_a_recursive_callee_into_the_general_function() {
    // consume(t): leaf returns the field; node recurses on the child.
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let consume = module.reserve_function();
    let consume_return = module.reserve_continuation();
    let t = module.add_value(Some("t".into()));
    let tag = module.add_value(Some("tag".into()));
    let child = module.add_value(Some("child".into()));
    let leaf = module.reserve_continuation();
    let node = module.reserve_continuation();
    let leaf_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: consume_return,
        args: vec![CpsAtom::Value(child)],
    }));
    module.define_continuation(
        leaf,
        CpsContinuation {
            debug_name: Some("leaf".into()),
            params: vec![],
            body: leaf_body,
        },
    );
    let node_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(consume),
        args: vec![CpsAtom::Value(child)],
        return_to: consume_return,
    });
    module.define_continuation(
        node,
        CpsContinuation {
            debug_name: Some("node".into()),
            params: vec![],
            body: node_body,
        },
    );
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: leaf,
                args: vec![],
            },
        )]),
        default: Some(CpsEdge {
            target: node,
            args: vec![],
        }),
    });
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![leaf, node],
        body: switch,
    });
    let project_child = module.add_node(CpsNode::LetPrim {
        result: child,
        op: CpsPrimOp::TplGet(1),
        args: vec![CpsAtom::Value(t)],
        next: scope,
    });
    let project_tag = module.add_node(CpsNode::LetPrim {
        result: tag,
        op: CpsPrimOp::TplGet(0),
        args: vec![CpsAtom::Value(t)],
        next: project_child,
    });
    module.define_function(
        consume,
        CpsFunction {
            debug_name: Some("consume".into()),
            params: vec![t],
            return_cont: consume_return,
            body: project_tag,
        },
    );

    let root = module.add_value(Some("root".into()));
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(consume),
        args: vec![CpsAtom::Value(root)],
        return_to: entry_return,
    });
    let ctor = module.add_node(CpsNode::LetValue {
        result: root,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(5)),
        ]),
        next: call,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![consume],
        body: ctor,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    let mut budget = BRANCH_CLONE_LIMIT;
    assert!(specialize_call_patterns(&mut module, &mut budget));
    module.verify().unwrap();

    let clone = known_callee(&module, call);
    assert_ne!(clone, consume);
    let recursive_target =
        function_nodes(&module, clone)
            .into_iter()
            .find_map(|id| match module.node(id) {
                Some(CpsNode::ApplyFun {
                    callee: CpsCallee::Known(target),
                    ..
                }) => Some(*target),
                _ => None,
            });
    assert_eq!(
        recursive_target,
        Some(consume),
        "the clone peels one level and recurses into the general function"
    );
}
