//! Inlining a known call and a single-use continuation, and what each must clone rather than share.

use {
    super::{inline_known_calls, inline_single_use_continuations},
    crate::{
        Atom, Callee, Continuation, CopyPlan, Edge, Function, FunctionId, Literal,
        MULTI_SITE_INLINE_LIMIT, Module, Node, NodeId, SCC_CLONE_NODE_LIMIT, ValueExpr,
        test_support::{capture_unmentioned_by_owner, nest_helpers},
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

#[test]
fn continuation_beta_rewrites_parameters_captured_by_nested_functions() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let continuation = module.reserve_continuation();
    let captured = module.add_value(Some("captured".into()));

    let nested = module.reserve_function();
    let nested_return = module.reserve_continuation();
    let nested_body = module.add_node(Node::ApplyCont(Edge {
        target: nested_return,
        args: vec![Atom::Value(captured)],
    }));
    module.define_function(
        nested,
        Function {
            debug_name: Some("nested".into()),
            params: vec![],
            return_cont: nested_return,
            body: nested_body,
            droppable: false,
        },
    );
    let return_nested = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Fun(nested)],
    }));
    let continuation_body = module.add_node(Node::LetFun {
        functions: vec![nested],
        body: return_nested,
    });
    module.define_continuation(
        continuation,
        Continuation {
            debug_name: Some("capture scope".into()),
            params: vec![captured],
            body: continuation_body,
        },
    );
    let call = module.add_node(Node::ApplyCont(Edge {
        target: continuation,
        args: vec![Atom::Literal(Literal::Nat(Natural::from(7u32)))],
    }));
    let body = module.add_node(Node::LetCont {
        continuations: vec![continuation],
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

    assert!(inline_single_use_continuations(&mut module));
    assert!(matches!(
        module.node(nested_body),
        Some(Node::ApplyCont(Edge { args, .. }))
            if args == &[Atom::Literal(Literal::Nat(Natural::from(7u32)))]
    ));
    module.verify().unwrap();
}

#[test]
fn known_call_inlining_clones_recursive_local_continuations() {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let callee_param = module.add_value(None);
    let local_cont = module.reserve_continuation();
    let local_param = module.add_value(None);
    let local_body = module.add_node(Node::ApplyCont(Edge {
        target: local_cont,
        args: vec![Atom::Value(local_param)],
    }));
    module.define_continuation(
        local_cont,
        Continuation {
            debug_name: None,
            params: vec![local_param],
            body: local_body,
        },
    );
    let enter_local = module.add_node(Node::ApplyCont(Edge {
        target: local_cont,
        args: vec![Atom::Value(callee_param)],
    }));
    let callee_body = module.add_node(Node::LetCont {
        continuations: vec![local_cont],
        body: enter_local,
    });
    module.define_function(
        callee,
        Function {
            debug_name: None,
            params: vec![callee_param],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: None,
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();

    assert!(inline_known_calls(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::LetCont { continuations, .. }) if continuations != &[local_cont]
    ));
    module.verify().unwrap();
}

/// `helper` captures `v`, which `owner` never names: the call is inside `helper`'s `LetFun`, so `v` is in scope at the site, and the inline is admitted on scope rather than on what `owner`'s body mentions.
#[test]
fn known_call_inlining_admits_a_capture_the_owner_never_mentions() {
    let (mut module, helper, _) = capture_unmentioned_by_owner();
    assert!(inline_known_calls(&mut module), "the sweep inlines");
    module.verify().unwrap();
    let still_called = module.nodes().iter().flatten().any(|node| {
        matches!(
            node,
            Node::ApplyFun { callee: Callee::Known(callee), .. } if *callee == helper
        )
    });
    assert!(!still_called, "the capturing helper is inlined:\n{module}");
}

/// The same helper, kept out of line: it fits the budget as before, and its call stays a call.
#[test]
fn known_call_inlining_leaves_a_function_kept_out_of_line_called() {
    let (mut module, helper, _) = capture_unmentioned_by_owner();
    module.keep_out_of_line(helper);

    inline_known_calls(&mut module);
    module.verify().unwrap();
    let still_called = module.nodes().iter().flatten().any(|node| {
        matches!(
            node,
            Node::ApplyFun { callee: Callee::Known(callee), .. } if *callee == helper
        )
    });
    assert!(still_called, "the helper is still called:\n{module}");
}

#[test]
fn a_call_handing_a_filler_to_an_applied_parameter_is_declined_without_minting() {
    // `callee(p) = p()`, called as `callee(filler)`. A filler reaches an argument position from `split_workers`' padding and from dead-parameter elimination, and it is no more nameable as a callee than a literal is — so the pre-minting bail must decline this call rather than let `map_callee` meet it after the copy has reserved values, nodes and continuations. The arena counts are the assertion: a declined attempt leaves nothing behind.
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();

    let p = module.add_value(Some("p".into()));
    let callee_body = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(p),
        args: vec![],
        return_to: callee_return,
    });
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![p],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Filler],
        return_to: entry_return,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body: call,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: entry_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);

    let before = (
        module.nodes().len(),
        module.continuations().len(),
        module.values().len(),
    );
    assert!(
        !inline_known_calls(&mut module),
        "the sweep declines the call"
    );
    let after = (
        module.nodes().len(),
        module.continuations().len(),
        module.values().len(),
    );
    assert_eq!(before, after, "a declined attempt mints nothing:\n{module}");
}

fn arenas(module: &Module) -> [usize; 4] {
    [
        module.nodes().len(),
        module.functions().len(),
        module.continuations().len(),
        module.values().len(),
    ]
}

fn pad(module: &mut Module, function: FunctionId, nodes: usize) {
    let size = CopyPlan::functions(module, BTreeSet::from([function]))
        .nodes
        .len();
    for _ in size..nodes {
        let result = module.add_value(None);
        let body = module.add_node(Node::LetValue {
            result,
            value: ValueExpr::Literal(Literal::Nat(Natural::from(0u32))),
            next: module.function(function).unwrap().body,
        });
        module.function_mut(function).unwrap().body = body;
    }
}

fn sized_calls(nodes: usize, sites: usize, escaping: bool) -> (Module, FunctionId, Vec<NodeId>) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let input = module.add_value(None);
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let param = module.add_value(None);
    let body = module.add_node(Node::ApplyCont(Edge {
        target: callee_return,
        args: vec![Atom::Value(param)],
    }));
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![param],
            return_cont: callee_return,
            body,
            droppable: false,
        },
    );
    pad(&mut module, callee, nodes);
    let mut calls = Vec::new();
    let mut continuations = Vec::new();
    let mut cases = BTreeMap::new();
    for index in 0..sites {
        let continuation = module.reserve_continuation();
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(callee),
            args: vec![Atom::Value(input)],
            return_to: entry_return,
        });
        module.define_continuation(
            continuation,
            Continuation {
                debug_name: None,
                params: vec![],
                body: call,
            },
        );
        calls.push(call);
        continuations.push(continuation);
        cases.insert(
            index as u32,
            Edge {
                target: continuation,
                args: vec![],
            },
        );
    }
    let dispatch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(input),
        cases,
        default: None,
    });
    let mut body = module.add_node(Node::LetCont {
        continuations,
        body: dispatch,
    });
    if escaping {
        let held = module.add_value(None);
        body = module.add_node(Node::LetValue {
            result: held,
            value: ValueExpr::Tuple(vec![Atom::Fun(callee)]),
            next: body,
        });
    }
    let body = module.add_node(Node::LetFun {
        functions: vec![callee],
        body,
    });
    module.define_function(
        entry,
        Function {
            debug_name: Some("main".into()),
            params: vec![input],
            return_cont: entry_return,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);
    module.verify().unwrap();
    (module, callee, calls)
}

#[test]
fn only_single_site_nonescaping_callees_receive_the_larger_allowance() {
    for (sites, escaping, limit) in [
        (1, false, SCC_CLONE_NODE_LIMIT),
        (2, false, MULTI_SITE_INLINE_LIMIT),
        (1, true, MULTI_SITE_INLINE_LIMIT),
    ] {
        for nested in [false, true] {
            for nodes in [limit, limit + 1] {
                let (mut module, callee, calls) =
                    sized_calls(if nested { 1 } else { nodes }, sites, escaping);
                if nested {
                    nest_helpers(&mut module, callee, nodes);
                }
                assert_eq!(
                    CopyPlan::functions(&module, BTreeSet::from([callee]))
                        .nodes
                        .len(),
                    nodes,
                );
                let before = arenas(&module);
                let text = module.to_string();
                let admitted = nodes <= limit;
                assert_eq!(inline_known_calls(&mut module), admitted);
                for call in calls {
                    assert_eq!(
                        matches!(module.node(call), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == callee),
                        !admitted,
                        "sites={sites}, escaping={escaping}, nested={nested}, nodes={nodes}",
                    );
                }
                if !admitted {
                    assert_eq!(arenas(&module), before);
                    assert_eq!(module.to_string(), text);
                }
                module.verify().unwrap();
            }
        }
    }
}

#[test]
fn a_callee_enlarged_earlier_in_the_sweep_is_measured_at_its_current_extent() {
    let (mut module, callee, calls) = sized_calls(1, 2, false);
    let helper = module.reserve_function();
    let helper_return = module.reserve_continuation();
    let param = module.add_value(None);
    let body = module.add_node(Node::ApplyCont(Edge {
        target: helper_return,
        args: vec![Atom::Value(param)],
    }));
    module.define_function(
        helper,
        Function {
            debug_name: Some("helper".into()),
            params: vec![param],
            return_cont: helper_return,
            body,
            droppable: false,
        },
    );
    pad(&mut module, helper, MULTI_SITE_INLINE_LIMIT + 1);
    let definition = module.function(callee).unwrap().clone();
    module.set_node(
        definition.body,
        Node::ApplyFun {
            callee: Callee::Known(helper),
            args: vec![Atom::Value(definition.params[0])],
            return_to: definition.return_cont,
        },
    );
    let entry = module.function(module.entry().unwrap()).unwrap().body;
    let Node::LetFun { functions, .. } = module.node_mut(entry).unwrap() else {
        unreachable!()
    };
    functions.push(helper);
    assert!(calls.iter().all(|call| definition.body < *call));
    module.verify().unwrap();

    assert!(inline_known_calls(&mut module));
    assert_eq!(
        CopyPlan::functions(&module, BTreeSet::from([callee]))
            .nodes
            .len(),
        MULTI_SITE_INLINE_LIMIT + 1,
    );
    for call in calls {
        assert!(
            matches!(module.node(call), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == callee)
        );
    }
    module.verify().unwrap();
}
