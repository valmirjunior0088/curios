//! Inlining a known call, and what it must clone rather than share.

use {
    super::*,
    crate::{test_support::*, *},
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

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

    assert!(inline_known_calls(
        &mut module,
        &mut known_function_allowance()
    ));
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
    assert!(
        inline_known_calls(&mut module, &mut known_function_allowance()),
        "the sweep inlines"
    );
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

    inline_known_calls(&mut module, &mut known_function_allowance());
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
        !inline_known_calls(&mut module, &mut known_function_allowance()),
        "the sweep declines the call"
    );
    let after = (
        module.nodes().len(),
        module.continuations().len(),
        module.values().len(),
    );
    assert_eq!(before, after, "a declined attempt mints nothing:\n{module}");
}

fn known_function_allowance() -> CopyBudget {
    CopyBudget::new(KNOWN_FUNCTION_COPY_NODE_LIMIT)
}

/// A callee of `nodes` nodes taking a value and a function, called at `sites` sites, each handing it a function of its own: the callee calls the function on the value where `applies`, and answers the value otherwise.
fn calls_handing_a_function(
    nodes: usize,
    sites: usize,
    applies: bool,
) -> (Module, FunctionId, Vec<NodeId>) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();
    let input = module.add_value(None);
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let value = module.add_value(None);
    let step = module.add_value(None);
    let body = module.add_node(if applies {
        Node::ApplyFun {
            callee: Callee::Closure(step),
            args: vec![Atom::Value(value)],
            return_to: callee_return,
        }
    } else {
        Node::ApplyCont(Edge {
            target: callee_return,
            args: vec![Atom::Value(value)],
        })
    });
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![value, step],
            return_cont: callee_return,
            body,
            droppable: false,
        },
    );
    pad(&mut module, callee, nodes);
    let steps = (0..sites).map(|index| {
        let function = module.reserve_function();
        let function_return = module.reserve_continuation();
        let param = module.add_value(None);
        let body = module.add_node(Node::ApplyCont(Edge {
            target: function_return,
            args: vec![Atom::Value(param)],
        }));
        module.define_function(
            function,
            Function {
                debug_name: Some(format!("step{index}")),
                params: vec![param],
                return_cont: function_return,
                body,
                droppable: false,
            },
        );
        function
    });
    let steps = steps.collect::<Vec<_>>();
    let mut calls = Vec::new();
    let mut continuations = Vec::new();
    let mut cases = BTreeMap::new();
    for (index, passed) in steps.iter().enumerate() {
        let continuation = module.reserve_continuation();
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(callee),
            args: vec![Atom::Value(input), Atom::Fun(*passed)],
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
    let body = module.add_node(Node::LetCont {
        continuations,
        body: dispatch,
    });
    let body = module.add_node(Node::LetFun {
        functions: [callee].into_iter().chain(steps).collect(),
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

fn calls_callee(module: &Module, call: NodeId, callee: FunctionId) -> bool {
    matches!(module.node(call), Some(Node::ApplyFun { callee: Callee::Known(target), .. }) if *target == callee)
}

#[test]
fn a_callee_of_several_sites_is_copied_into_a_call_that_hands_it_a_function_it_applies() {
    let nodes = MULTI_SITE_INLINE_LIMIT + 8;
    let (mut module, callee, calls) = calls_handing_a_function(nodes, 2, true);
    let mut allowance = known_function_allowance();

    assert!(inline_known_calls(&mut module, &mut allowance));
    for call in calls {
        assert!(!calls_callee(&module, call, callee));
    }
    assert_eq!(
        allowance,
        CopyBudget::new(KNOWN_FUNCTION_COPY_NODE_LIMIT - 2 * nodes),
        "each copy is charged its extent, a sweep counting the callee's sites as they stood when it began",
    );
    module.verify().unwrap();
}

#[test]
fn a_callee_that_only_receives_the_function_keeps_the_multi_site_allowance() {
    let (mut module, callee, calls) =
        calls_handing_a_function(MULTI_SITE_INLINE_LIMIT + 8, 2, false);
    let mut allowance = known_function_allowance();

    assert!(!inline_known_calls(&mut module, &mut allowance));
    for call in calls {
        assert!(calls_callee(&module, call, callee));
    }
    assert_eq!(allowance, known_function_allowance());
}

#[test]
fn a_copy_for_a_known_function_past_the_allowance_left_is_declined() {
    let nodes = MULTI_SITE_INLINE_LIMIT + 8;
    let (mut module, callee, calls) = calls_handing_a_function(nodes, 3, true);
    let mut allowance = CopyBudget::new(nodes);

    assert!(inline_known_calls(&mut module, &mut allowance));
    assert_eq!(
        calls
            .iter()
            .filter(|call| calls_callee(&module, **call, callee))
            .count(),
        2,
        "the allowance pays for one copy, and the two calls left keep the callee shared",
    );
    module.verify().unwrap();
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
                assert_eq!(
                    inline_known_calls(&mut module, &mut known_function_allowance()),
                    admitted
                );
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

    assert!(inline_known_calls(
        &mut module,
        &mut known_function_allowance()
    ));
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
