//! Component sweeps preserve generic cleanup, lexical scope and lifetime copy accounting across many contexts.

use {
    super::*,
    crate::{test_support::*, *},
    curios_num::{Floating, Natural},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

#[test]
fn independent_components_specialize_together_and_charge_their_complete_extents() {
    let fixture = polymorphic_loop(true, 0);
    let mut module = fixture.module;
    let entry = module.entry().unwrap();
    let ret = module.function(entry).unwrap().return_cont;
    let plan = CopyPlan::functions(&module, BTreeSet::from([entry]));
    let copies = copy_bodies(&mut module, plan);
    let copied_entry = copies.functions[&entry];
    let copied_loop = copies.functions[&fixture.loop_fn];
    let body = module.function(entry).unwrap().body;
    let scope = module.add_node(Node::LetFun {
        functions: vec![copied_entry],
        body,
    });
    module.function_mut(entry).unwrap().body = scope;
    let result = module.add_value(None);
    let run_copy = module.add_node(Node::ApplyFun {
        callee: Callee::Known(copied_entry),
        args: vec![],
        return_to: ret,
    });
    let resume = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![result],
        body: run_copy,
    });
    let Node::ApplyFun { return_to, .. } = module.node_mut(fixture.call2).unwrap() else {
        unreachable!()
    };
    *return_to = resume;
    let call2 = fixture.call2;
    let owner = module
        .live_continuations()
        .find_map(|(id, definition)| (definition.body == call2).then_some(id))
        .unwrap();
    let body = module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: call2,
    });
    module.continuation_mut(owner).unwrap().body = body;
    module.verify().unwrap();
    let cost: usize = [fixture.loop_fn, copied_loop]
        .into_iter()
        .map(|root| {
            CopyPlan::functions(&module, BTreeSet::from([root]))
                .nodes()
                .len()
        })
        .sum();
    let before = module.functions().len();
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_scc_calls(&mut module, &mut budget));
    assert_eq!(module.functions().len() - before, 2);
    assert_eq!(budget, CopyBudget::new(0));
    module.verify().unwrap();
}

#[test]
fn many_independent_contexts_advance_without_a_one_context_per_round_throttle() {
    let fixture = polymorphic_loop(true, 0);
    let mut module = fixture.module;
    let entry = module.entry().unwrap();
    let ret = module.function(entry).unwrap().return_cont;
    let choice = module.add_value(None);
    module.function_mut(entry).unwrap().params.push(choice);
    let intro = module.function(entry).unwrap().body;
    let Node::LetFun { body: scope, .. } = *module.node(intro).unwrap() else {
        unreachable!()
    };
    let Node::LetCont { body: first, .. } = *module.node(scope).unwrap() else {
        unreachable!()
    };
    let original = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![],
        body: first,
    });
    let mut definitions = vec![original];
    let mut cases = BTreeMap::from([(
        0,
        Edge {
            target: original,
            args: vec![],
        },
    )]);
    let mut sites = vec![fixture.call1, fixture.call2];
    let mut helpers = Vec::new();
    for index in 0..68 {
        let helper = module.reserve_function();
        let helper_ret = module.reserve_continuation();
        let param = module.add_value(None);
        let body = module.add_node(Node::ApplyCont(Edge {
            target: helper_ret,
            args: vec![Atom::Value(param)],
        }));
        module.define_function(
            helper,
            Function {
                debug_name: None,
                params: vec![param],
                return_cont: helper_ret,
                body,
                droppable: false,
            },
        );
        helpers.push(helper);
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(fixture.loop_fn),
            args: vec![
                Atom::Fun(helper),
                Atom::Literal(Literal::Nat(Natural::from(3u32))),
            ],
            return_to: ret,
        });
        sites.push(call);
        let before = module.add_continuation(Continuation {
            debug_name: None,
            params: vec![],
            body: call,
        });
        definitions.push(before);
        cases.insert(
            index + 1,
            Edge {
                target: before,
                args: vec![],
            },
        );
    }
    let Node::LetFun { functions, .. } = module.node_mut(intro).unwrap() else {
        unreachable!()
    };
    functions.extend(helpers);
    let dispatch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(choice),
        cases,
        default: None,
    });
    let Node::LetCont {
        continuations,
        body,
    } = module.node_mut(scope).unwrap()
    else {
        unreachable!()
    };
    continuations.extend(definitions);
    *body = dispatch;
    module.verify().unwrap();
    let cost = CopyPlan::functions(&module, BTreeSet::from([fixture.loop_fn]))
        .nodes()
        .len();
    let mut budget = CopyBudget::new(SCC_COPY_NODE_LIMIT);
    assert!(specialize_scc_calls(&mut module, &mut budget));
    let copied = sites
        .iter()
        .filter(|site| known_callee(&module, **site) != fixture.loop_fn)
        .count();
    assert_eq!(
        copied, 69,
        "the final context stays generic for ordinary propagation"
    );
    assert_eq!(budget, CopyBudget::new(SCC_COPY_NODE_LIMIT - copied * cost));
    module.verify().unwrap();
}

/// A callback bound only at its call site, out of the component's scope, is handed to a copy that joins the callback's own group, inside the call's scope, which the component precedes: the call names the copy, and the copy's body may name the callback, which the next substitution then puts in place of its parameter.
#[test]
fn a_callback_bound_only_at_its_call_site_is_handed_to_a_copy_beside_it() {
    let fixture = polymorphic_loop(true, 0);
    let mut module = fixture.module;
    let entry = module.entry().unwrap();
    let intro = module.function(entry).unwrap().body;
    let Node::LetFun { functions, .. } = module.node_mut(intro).unwrap() else {
        unreachable!()
    };
    functions.retain(|function| *function == fixture.loop_fn);
    for caller in [fixture.call1, fixture.call2] {
        let Node::ApplyFun { ref args, .. } = *module.node(caller).unwrap() else {
            unreachable!()
        };
        let Atom::Fun(helper) = args[0] else {
            unreachable!()
        };
        let body = module.add_node(Node::LetFun {
            functions: vec![helper],
            body: caller,
        });
        let predecessor = module.live_nodes().find_map(|(id, node)| {
            matches!(node, Node::LetCont { body, .. } if *body == caller).then_some(id)
        });
        if let Some(predecessor) = predecessor {
            let Node::LetCont { body: next, .. } = module.node_mut(predecessor).unwrap() else {
                unreachable!()
            };
            *next = body;
        } else {
            let owner = module
                .live_continuations()
                .find_map(|(id, definition)| (definition.body == caller).then_some(id))
                .unwrap();
            module.continuation_mut(owner).unwrap().body = body;
        }
    }
    module.verify().unwrap();
    let cost = CopyPlan::functions(&module, BTreeSet::from([fixture.loop_fn]))
        .nodes()
        .len();
    let mut budget = CopyBudget::new(SCC_COPY_NODE_LIMIT);
    assert!(specialize_scc_calls(&mut module, &mut budget));
    module.verify().unwrap();
    let mut copied = 0;
    for caller in [fixture.call1, fixture.call2] {
        let callee = known_callee(&module, caller);
        if callee == fixture.loop_fn {
            continue;
        }
        copied += 1;
        let Node::ApplyFun { ref args, .. } = *module.node(caller).unwrap() else {
            unreachable!()
        };
        let Atom::Fun(helper) = args[0] else {
            unreachable!()
        };
        assert!(
            module.live_nodes().any(|(_, node)| matches!(
                node,
                Node::LetFun { functions, .. } if functions.contains(&helper) && functions.contains(&callee)
            )),
            "the copy joins the group that binds the callback it is handed",
        );
    }
    assert!(
        copied > 0,
        "a call handing a callback out of scope is given a copy"
    );
    assert_eq!(budget, CopyBudget::new(SCC_COPY_NODE_LIMIT - copied * cost));
}

#[test]
fn equal_context_keys_find_the_same_group_with_nan_and_signed_zero_literals() {
    let mut groups = HashMap::new();
    let operands = [
        Atom::Literal(Literal::Flt(Floating::from_bits(0x7ff8_0000_0000_0001))),
        Atom::Literal(Literal::Flt(Floating::from_bits(0x7ff8_0000_0000_0002))),
        Atom::Literal(Literal::Flt(Floating::from_bits(0))),
        Atom::Literal(Literal::Flt(Floating::from_bits(1 << 63))),
        Atom::Literal(Literal::Nat(Natural::from(0u32))),
        Atom::Filler,
    ];
    for (index, atom) in operands.iter().enumerate() {
        groups.insert(
            Context {
                entry: FunctionId(0),
                args: vec![atom.clone()],
            },
            index,
        );
    }
    assert_eq!(groups.len(), operands.len());
    for (index, atom) in operands.into_iter().enumerate() {
        assert_eq!(
            groups.get(&Context {
                entry: FunctionId(0),
                args: vec![atom]
            }),
            Some(&index)
        );
    }
}

#[test]
fn insufficient_allowance_mints_nothing_and_the_exact_extent_is_charged_once() {
    for sufficient in [false, true] {
        let fixture = polymorphic_loop(true, 0);
        let mut module = fixture.module;
        let cost = CopyPlan::functions(&module, BTreeSet::from([fixture.loop_fn]))
            .nodes()
            .len();
        let allowance = cost - usize::from(!sufficient);
        let mut budget = CopyBudget::new(allowance);
        let arenas = (
            module.functions().len(),
            module.continuations().len(),
            module.nodes().len(),
            module.values().len(),
        );
        let original = module.to_string();
        assert_eq!(specialize_scc_calls(&mut module, &mut budget), sufficient);
        if sufficient {
            assert_eq!(budget, CopyBudget::new(0));
            assert_eq!(
                [fixture.call1, fixture.call2]
                    .into_iter()
                    .filter(|site| known_callee(&module, *site) != fixture.loop_fn)
                    .count(),
                1
            );
        } else {
            assert_eq!(budget, CopyBudget::new(allowance));
            assert_eq!(
                arenas,
                (
                    module.functions().len(),
                    module.continuations().len(),
                    module.nodes().len(),
                    module.values().len()
                )
            );
            assert_eq!(module.to_string(), original);
        }
        module.verify().unwrap();
    }
}

/// `main() = g(add, 3); g(mul, 4)`, where `g(op, n) = op(again, n)` hands `op` a function it defines, `again(m) = g(op, m)`: `g` calls itself only through `again`, which forwards `op` and replaces `n`.
fn a_component_recursive_through_a_function_it_defines() -> (Module, NodeId, NodeId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let step = |module: &mut Module, name: &str| {
        let function = module.reserve_function();
        let function_return = module.reserve_continuation();
        let next = module.add_value(Some(format!("{name} next")));
        let x = module.add_value(Some(format!("{name} x")));
        let body = module.add_node(Node::ApplyCont(Edge {
            target: function_return,
            args: vec![Atom::Value(x)],
        }));
        module.define_function(
            function,
            Function {
                debug_name: Some(name.into()),
                params: vec![next, x],
                return_cont: function_return,
                body,
                droppable: false,
            },
        );
        function
    };
    let add = step(&mut module, "add");
    let mul = step(&mut module, "mul");

    let g = module.reserve_function();
    let g_return = module.reserve_continuation();
    let op = module.add_value(Some("op".into()));
    let n = module.add_value(Some("n".into()));
    let again = module.reserve_function();
    let again_return = module.reserve_continuation();
    let m = module.add_value(Some("m".into()));
    let again_body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![Atom::Value(op), Atom::Value(m)],
        return_to: again_return,
    });
    module.define_function(
        again,
        Function {
            debug_name: Some("again".into()),
            params: vec![m],
            return_cont: again_return,
            body: again_body,
            droppable: false,
        },
    );
    let apply = module.add_node(Node::ApplyFun {
        callee: Callee::Closure(op),
        args: vec![Atom::Fun(again), Atom::Value(n)],
        return_to: g_return,
    });
    let g_body = module.add_node(Node::LetFun {
        functions: vec![again],
        body: apply,
    });
    module.define_function(
        g,
        Function {
            debug_name: Some("g".into()),
            params: vec![op, n],
            return_cont: g_return,
            body: g_body,
            droppable: false,
        },
    );

    let x1 = module.add_value(Some("x1".into()));
    let call2 = module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![
            Atom::Fun(mul),
            Atom::Literal(Literal::Nat(Natural::from(4u32))),
        ],
        return_to: entry_return,
    });
    let k1 = module.reserve_continuation();
    module.define_continuation(
        k1,
        Continuation {
            debug_name: Some("k1".into()),
            params: vec![x1],
            body: call2,
        },
    );
    let call1 = module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![
            Atom::Fun(add),
            Atom::Literal(Literal::Nat(Natural::from(3u32))),
        ],
        return_to: k1,
    });
    let outer = module.add_node(Node::LetCont {
        continuations: vec![k1],
        body: call1,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![g, add, mul],
        body: outer,
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
    module.verify().unwrap();
    (module, call1, call2)
}

/// A call a function nested in a member makes back into the component is one the copy reproduces, so it constrains every context: `n` is replaced at each turn and never known, and the copy for `add` leaves the original to `mul`, each a context its group already agrees on, so a second sweep finds nothing to copy. The function each call hands is then substituted into its own copy.
#[test]
fn a_component_recursive_through_a_function_it_defines_is_copied_once_a_context() {
    let (mut module, call1, call2) = a_component_recursive_through_a_function_it_defines();
    let mut budget = CopyBudget::new(SCC_COPY_NODE_LIMIT);

    assert!(specialize_scc_calls(&mut module, &mut budget));
    module.verify().unwrap();
    assert!(
        !specialize_scc_calls(&mut module, &mut budget),
        "each copy's context is the one its group agrees on:\n{module}"
    );

    let first = known_callee(&module, call1);
    let second = known_callee(&module, call2);
    assert_ne!(first, second, "each context runs a version of its own");
    let known = known_values(&module);
    for (callee, handed) in [(first, "add"), (second, "mul")] {
        let op = module.function(callee).unwrap().params[0];
        let Some(Atom::Fun(function)) = known.get(&op) else {
            panic!("`op` is known in the version handed `{handed}`:\n{module}");
        };
        assert_eq!(
            module.function(*function).unwrap().debug_name.as_deref(),
            Some(handed)
        );
    }
}

/// `walk(k, n) = match n | 0 => k | _ => walk(k, n + 1)`, called as `walk(1, 3)` and `walk(2, 4)`: `k` is passed on at every turn, and the two calls disagree on it.
fn a_loop_passing_on_the_literal_each_call_hands_it() -> (Module, FunctionId, NodeId, NodeId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let entry_return = module.reserve_continuation();

    let walk = module.reserve_function();
    let walk_return = module.reserve_continuation();
    let k = module.add_value(Some("k".into()));
    let n = module.add_value(Some("n".into()));
    let next = module.add_value(Some("next".into()));
    let again = module.add_node(Node::ApplyFun {
        callee: Callee::Known(walk),
        args: vec![Atom::Value(k), Atom::Value(next)],
        return_to: walk_return,
    });
    let step = module.add_node(Node::LetIntrinsic {
        result: next,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(n),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: again,
    });
    let turn = module.reserve_continuation();
    module.define_continuation(
        turn,
        Continuation {
            debug_name: Some("turn".into()),
            params: vec![],
            body: step,
        },
    );
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(n),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: walk_return,
                args: vec![Atom::Value(k)],
            },
        )]),
        default: Some(Edge {
            target: turn,
            args: vec![],
        }),
    });
    let walk_body = module.add_node(Node::LetCont {
        continuations: vec![turn],
        body: switch,
    });
    module.define_function(
        walk,
        Function {
            debug_name: Some("walk".into()),
            params: vec![k, n],
            return_cont: walk_return,
            body: walk_body,
            droppable: false,
        },
    );

    let first_result = module.add_value(Some("first".into()));
    let call2 = module.add_node(Node::ApplyFun {
        callee: Callee::Known(walk),
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
            Atom::Literal(Literal::Nat(Natural::from(4u32))),
        ],
        return_to: entry_return,
    });
    let k1 = module.reserve_continuation();
    module.define_continuation(
        k1,
        Continuation {
            debug_name: Some("k1".into()),
            params: vec![first_result],
            body: call2,
        },
    );
    let call1 = module.add_node(Node::ApplyFun {
        callee: Callee::Known(walk),
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(3u32))),
        ],
        return_to: k1,
    });
    let outer = module.add_node(Node::LetCont {
        continuations: vec![k1],
        body: call1,
    });
    let body = module.add_node(Node::LetFun {
        functions: vec![walk],
        body: outer,
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
    module.verify().unwrap();
    (module, walk, call1, call2)
}

/// A literal is substituted where every entry agrees on it and is no reason to copy a component where calls disagree: what one context's literal would buy a copy is a branch or a fold at most, and here not even that, so nothing is copied and nothing is charged.
#[test]
fn a_literal_a_call_hands_is_no_context_for_a_copy() {
    let (mut module, walk, call1, call2) = a_loop_passing_on_the_literal_each_call_hands_it();
    let original = module.to_string();
    let mut budget = CopyBudget::new(SCC_COPY_NODE_LIMIT);

    assert!(!specialize_scc_calls(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(SCC_COPY_NODE_LIMIT));
    assert_eq!(known_callee(&module, call1), walk);
    assert_eq!(known_callee(&module, call2), walk);
    assert_eq!(module.to_string(), original);
}
