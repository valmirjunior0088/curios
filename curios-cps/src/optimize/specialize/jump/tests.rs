//! A jump sweep batches independent roots, defers changed entry facts and charges actual copied nodes.

use {
    crate::{
        Atom, Continuation, ContinuationId, CopyBudget, CopyPlan, Edge, Function, Intrinsic,
        JUMP_COPY_NODE_LIMIT, Literal, Module, Node, NodeId, ValueExpr, specialize_jump_patterns,
        test_support::{tagged_family, tagged_join},
    },
    curios_num::Natural,
    std::collections::BTreeMap,
};

fn nat(n: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(n)))
}

fn joins(chained: bool) -> (Module, [ContinuationId; 2], [NodeId; 2]) {
    let mut module = Module::new();
    let family = tagged_family(&mut module, "family");
    let main = module.reserve_function();
    let ret = module.reserve_continuation();
    let choice = module.add_value(None);
    let some = module.add_value(None);
    let none = module.add_value(None);
    let roots = [module.reserve_continuation(), module.reserve_continuation()];
    let mut definitions = roots.to_vec();
    let mut cases = BTreeMap::new();
    let mut picked = Vec::new();
    for (index, root) in roots.iter().copied().enumerate() {
        let param = module.add_value(None);
        let tag = module.add_value(None);
        let next = module.add_node(Node::ApplyCont(Edge {
            target: if chained && index == 0 { roots[1] } else { ret },
            args: vec![Atom::Value(if chained && index == 0 { param } else { tag })],
        }));
        let body = module.add_node(Node::LetIntrinsic {
            result: tag,
            op: Intrinsic::RowGet(family, 0),
            args: vec![Atom::Value(param)],
            next,
        });
        module.define_continuation(
            root,
            Continuation {
                debug_name: None,
                params: vec![param],
                body,
            },
        );
        for (offset, value) in [some, none].into_iter().enumerate() {
            let before = module.reserve_continuation();
            let body = module.add_node(Node::ApplyCont(Edge {
                target: root,
                args: vec![Atom::Value(value)],
            }));
            if offset == 0 {
                picked.push(body);
            }
            module.define_continuation(
                before,
                Continuation {
                    debug_name: None,
                    params: vec![],
                    body,
                },
            );
            definitions.push(before);
            cases.insert(
                (index * 2 + offset) as u32,
                Edge {
                    target: before,
                    args: vec![],
                },
            );
        }
    }
    let body = module.add_node(Node::Switch {
        scrutinee: Atom::Value(choice),
        cases,
        default: None,
    });
    let body = module.add_node(Node::LetCont {
        continuations: definitions,
        body,
    });
    let body = module.add_node(Node::LetValue {
        result: none,
        value: ValueExpr::Row(family, vec![nat(1), Atom::Filler]),
        next: body,
    });
    let body = module.add_node(Node::LetValue {
        result: some,
        value: ValueExpr::Row(family, vec![nat(0), nat(42)]),
        next: body,
    });
    module.define_function(
        main,
        Function {
            debug_name: None,
            params: vec![choice],
            return_cont: ret,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    (module, roots, picked.try_into().unwrap())
}

fn target(module: &Module, node: NodeId) -> ContinuationId {
    let Node::ApplyCont(edge) = module.node(node).unwrap() else {
        panic!("predecessor remains a jump")
    };
    edge.target
}

#[test]
fn independent_roots_advance_together_and_spend_the_actual_copy_cost() {
    let (mut module, roots, sites) = joins(false);
    let cost: usize = roots
        .iter()
        .map(|root| CopyPlan::continuation(&module, *root).nodes.len() + 1)
        .sum();
    let before = module.nodes().len();
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    for (root, site) in roots.into_iter().zip(sites) {
        assert_ne!(target(&module, site), root);
    }
    assert_eq!(module.nodes().len() - before, cost);
    assert_eq!(budget, CopyBudget::new(0));
    module.verify().unwrap();
}

#[test]
fn a_fresh_transfer_into_another_root_defers_that_roots_snapshot() {
    let (mut module, roots, sites) = joins(true);
    let mut budget = CopyBudget::new(JUMP_COPY_NODE_LIMIT);
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    assert_ne!(target(&module, sites[0]), roots[0]);
    assert_eq!(target(&module, sites[1]), roots[1]);
    module.verify().unwrap();
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    assert_ne!(target(&module, sites[1]), roots[1]);
    module.verify().unwrap();
}

#[test]
fn an_insufficient_allowance_mints_nothing_and_an_exact_allowance_succeeds() {
    let (mut module, join, _, _, _) = tagged_join();
    let cost = CopyPlan::continuation(&module, join).nodes.len() + 1;
    let original = module.to_string();
    let arenas = (
        module.nodes().len(),
        module.functions().len(),
        module.continuations().len(),
        module.values().len(),
    );
    let mut budget = CopyBudget::new(cost - 1);
    assert!(!specialize_jump_patterns(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(cost - 1));
    assert_eq!(module.to_string(), original);
    assert_eq!(
        arenas,
        (
            module.nodes().len(),
            module.functions().len(),
            module.continuations().len(),
            module.values().len()
        )
    );
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(0));
    module.verify().unwrap();
}

#[test]
fn stable_ties_choose_the_same_sites_and_mint_the_same_graph() {
    let (module, _, _) = joins(false);
    let mut left = module.clone();
    let mut right = module;
    specialize_jump_patterns(&mut left, &mut CopyBudget::new(JUMP_COPY_NODE_LIMIT));
    specialize_jump_patterns(&mut right, &mut CopyBudget::new(JUMP_COPY_NODE_LIMIT));
    assert_eq!(left.to_string(), right.to_string());
}

#[test]
fn an_inner_specialization_defers_the_outer_source_it_changes() {
    let (mut module, roots, sites) = joins(false);
    let anchor = module.live_nodes().find_map(|(id, node)| {
        matches!(node, Node::LetCont { continuations, .. } if continuations.contains(&roots[0]) && continuations.contains(&roots[1])).then_some(id)
    }).unwrap();
    let Node::LetCont { body: dispatch, .. } = *module.node(anchor).unwrap() else {
        unreachable!()
    };
    let Node::Switch { cases, .. } = module.node_mut(dispatch).unwrap() else {
        unreachable!()
    };
    let some = cases.remove(&2).unwrap();
    let none = cases.remove(&3).unwrap();
    let Node::LetCont { continuations, .. } = module.node_mut(anchor).unwrap() else {
        unreachable!()
    };
    continuations.retain(|id| ![roots[1], some.target, none.target].contains(id));
    let body = module.continuation(roots[0]).unwrap().body;
    let Node::LetIntrinsic {
        result: tag, next, ..
    } = *module.node(body).unwrap()
    else {
        unreachable!()
    };
    module.set_node(
        next,
        Node::Switch {
            scrutinee: Atom::Value(tag),
            cases: BTreeMap::from([(0, some.clone())]),
            default: Some(none.clone()),
        },
    );
    let nested = module.add_node(Node::LetCont {
        continuations: vec![roots[1], some.target, none.target],
        body,
    });
    module.continuation_mut(roots[0]).unwrap().body = nested;
    module.verify().unwrap();
    let mut budget = CopyBudget::new(JUMP_COPY_NODE_LIMIT);
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    assert_eq!(target(&module, sites[0]), roots[0]);
    assert_ne!(target(&module, sites[1]), roots[1]);
    module.verify().unwrap();
}

#[test]
fn more_matching_sites_win_a_budget_that_fits_only_one_copy() {
    let (mut module, roots, sites) = joins(false);
    let anchor = module.live_nodes().find_map(|(id, node)| {
        matches!(node, Node::LetCont { continuations, .. } if continuations.contains(&roots[0]) && continuations.contains(&roots[1])).then_some(id)
    }).unwrap();
    let body = module.add_node(module.node(sites[1]).unwrap().clone());
    let extra = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![],
        body,
    });
    let Node::LetCont {
        continuations,
        body: dispatch,
    } = module.node_mut(anchor).unwrap()
    else {
        unreachable!()
    };
    continuations.push(extra);
    let dispatch = *dispatch;
    let Node::Switch { cases, .. } = module.node_mut(dispatch).unwrap() else {
        unreachable!()
    };
    cases.insert(
        99,
        Edge {
            target: extra,
            args: vec![],
        },
    );
    module.verify().unwrap();
    let cost = CopyPlan::continuation(&module, roots[1]).nodes.len() + 1;
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_jump_patterns(&mut module, &mut budget));
    assert_eq!(target(&module, sites[0]), roots[0]);
    assert_ne!(target(&module, sites[1]), roots[1]);
    assert_eq!(target(&module, sites[1]), target(&module, body));
    assert_eq!(budget, CopyBudget::new(0));
    module.verify().unwrap();
}
