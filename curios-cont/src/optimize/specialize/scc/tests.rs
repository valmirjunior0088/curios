//! Component sweeps preserve generic cleanup, lexical scope and lifetime copy accounting across many contexts.

use {
    super::Context,
    crate::{
        Atom, Callee, Continuation, CopyBudget, CopyPlan, Edge, Function, FunctionId, Literal,
        Node, SCC_COPY_NODE_LIMIT, copy_bodies, specialize_scc_calls,
        test_support::{known_callee, polymorphic_loop},
    },
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
                .nodes
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
        .nodes
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

#[test]
fn a_callback_bound_only_at_its_call_site_does_not_justify_a_copy() {
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
    let original = module.to_string();
    let mut budget = CopyBudget::new(SCC_COPY_NODE_LIMIT);
    assert!(!specialize_scc_calls(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(SCC_COPY_NODE_LIMIT));
    assert_eq!(module.to_string(), original);
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
            .nodes
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
