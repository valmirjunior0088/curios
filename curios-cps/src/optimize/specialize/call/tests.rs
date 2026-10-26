//! Call sweeps share equivalent sites, batch independent roots and defer callees whose incoming calls changed.

use {
    crate::{
        Atom, BRANCH_COPY_NODE_LIMIT, Callee, CopyBudget, CopyPlan, FunctionId, Module, Node,
        NodeId, copy_bodies, function_nodes, prune_unreachable, specialize_call_patterns,
        test_support::{known_callee, tagged_consumer},
    },
    std::collections::BTreeSet,
};

fn siblings(tags: &[u32], split: usize) -> (Module, Vec<NodeId>, [FunctionId; 2]) {
    let (mut module, calls, original) = tagged_consumer(0, tags);
    let copy = CopyPlan::functions(&module, BTreeSet::from([original]));
    let sibling = copy_bodies(&mut module, copy).functions[&original];
    let entry = module.entry().unwrap();
    let intro = module.function(entry).unwrap().body;
    let Node::LetFun { functions, .. } = module.node_mut(intro).unwrap() else {
        unreachable!()
    };
    functions.push(sibling);
    for &id in &calls[split..] {
        let Node::ApplyFun { callee, .. } = module.node_mut(id).unwrap() else {
            unreachable!()
        };
        *callee = Callee::Known(sibling);
    }
    module.verify().unwrap();
    (module, calls, [original, sibling])
}

#[test]
fn independent_callees_advance_together_under_one_shared_lexical_anchor() {
    let (mut module, calls, roots) = siblings(&[0, 1, 0, 1], 2);
    let cost: usize = roots
        .iter()
        .map(|root| {
            CopyPlan::functions(&module, BTreeSet::from([*root]))
                .nodes
                .len()
                + 1
        })
        .sum();
    let before = module.nodes().len();
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_call_patterns(&mut module, &mut budget));
    for (root, group) in roots.into_iter().zip(calls.chunks(2)) {
        assert!(
            group
                .iter()
                .any(|site| known_callee(&module, *site) != root)
        );
    }
    assert_eq!(budget, CopyBudget::new(0));
    assert_eq!(module.nodes().len() - before, cost);
    module.verify().unwrap();
}

#[test]
fn a_copied_call_into_another_generic_callee_defers_its_snapshot() {
    let (mut module, calls, roots) = siblings(&[0, 0, 1], 2);
    let definition = module.function(roots[0]).unwrap();
    let param = definition.params[0];
    let ret = definition.return_cont;
    let end = function_nodes(&module, roots[0])
        .into_iter()
        .find(|id| matches!(module.node(*id), Some(Node::Switch { .. })))
        .unwrap();
    module.set_node(
        end,
        Node::ApplyFun {
            callee: Callee::Known(roots[1]),
            args: vec![Atom::Value(param)],
            return_to: ret,
        },
    );
    module.verify().unwrap();
    let mut budget = CopyBudget::new(BRANCH_COPY_NODE_LIMIT);
    assert!(specialize_call_patterns(&mut module, &mut budget));
    assert_ne!(known_callee(&module, calls[0]), roots[0]);
    assert_eq!(
        known_callee(&module, calls[0]),
        known_callee(&module, calls[1])
    );
    assert_eq!(known_callee(&module, calls[2]), roots[1]);
    module.verify().unwrap();
    assert!(specialize_call_patterns(&mut module, &mut budget));
    assert_ne!(known_callee(&module, calls[2]), roots[1]);
    module.verify().unwrap();
}

#[test]
fn an_insufficient_allowance_preserves_arenas_and_an_exact_allowance_succeeds() {
    let (mut module, calls, callee) = tagged_consumer(0, &[0]);
    let cost = CopyPlan::functions(&module, BTreeSet::from([callee]))
        .nodes
        .len()
        + 1;
    let original = module.to_string();
    let sizes = (
        module.nodes().len(),
        module.functions().len(),
        module.continuations().len(),
        module.values().len(),
    );
    let mut budget = CopyBudget::new(cost - 1);
    assert!(!specialize_call_patterns(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(cost - 1));
    assert_eq!(module.to_string(), original);
    assert_eq!(
        sizes,
        (
            module.nodes().len(),
            module.functions().len(),
            module.continuations().len(),
            module.values().len()
        )
    );
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_call_patterns(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(0));
    assert_ne!(known_callee(&module, calls[0]), callee);
    module.verify().unwrap();
}

#[test]
fn orphaned_call_sites_neither_win_priority_nor_retarget() {
    let (mut module, calls, root) = tagged_consumer(0, &[0, 1]);
    let stale = module.node(calls[1]).unwrap().clone();
    let orphans = (0..8)
        .map(|_| module.add_node(stale.clone()))
        .collect::<Vec<_>>();
    let cost = CopyPlan::functions(&module, BTreeSet::from([root]))
        .nodes
        .len()
        + 1;
    let mut budget = CopyBudget::new(cost);
    assert!(specialize_call_patterns(&mut module, &mut budget));
    assert_eq!(budget, CopyBudget::new(0));
    assert_ne!(
        known_callee(&module, calls[0]),
        root,
        "only the live sites count toward sharing priority"
    );
    assert_eq!(known_callee(&module, calls[1]), root);
    for orphan in orphans {
        assert_eq!(
            known_callee(&module, orphan),
            root,
            "an unowned node is not an incoming site"
        );
    }
    prune_unreachable(&mut module);
    module.verify().unwrap();
}
