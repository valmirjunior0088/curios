//! The rebuild in place: each function some calls hand a unique value at every parameter it takes apart, and others a shared one, copied for the first as its reuse version; then each construction of a row the module proves may write into a value of that row dying beside it becomes a [`ValueExpr::Rebuilt`] of that value, writing the slots it changes.

use {
    super::{
        Atom, Callee, CopyBudget, CopyPlan, FunctionId, Module, Node, NodeId,
        REBUILD_CLONE_NODE_LIMIT, REBUILD_COPY_NODE_LIMIT, ValueExpr, World, copy_bodies,
        function_nodes, rebuilds, routes,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Copy the reuse versions `routes` finds within their allowance, then rewrite every construction of a row `rebuilds` admits as a rebuild of the value it admits — after the optimizer and before the representation is decided, so no pass that copies, moves or merges a computation meets one. Which calls reach a copy only decides what the proof can find: the proof itself is made over the module as the copies leave it.
pub fn rebuild_in_place(module: &mut Module) {
    curios_profile::profile!("rebuild_in_place");
    copy_reuse_versions(module, CopyBudget::new(REBUILD_COPY_NODE_LIMIT));
    for rebuild in rebuilds(module) {
        if let Some(Node::LetValue { value, .. }) = module.node_mut(rebuild.node) {
            *value = ValueExpr::Rebuilt {
                of: Atom::Value(rebuild.of),
                row: rebuild.row,
                slots: rebuild.slots,
            };
        }
    }
}

/// One reuse version made: the copy, each copied node's copy, the nodes of the original's own body, and the functions the copy holds a copy of.
struct Version {
    copy: FunctionId,
    nodes: BTreeMap<NodeId, NodeId>,
    own: BTreeSet<NodeId>,
    nested: BTreeMap<FunctionId, FunctionId>,
}

/// Copy each function whose reuse version some call reaches beside its base one, bind the copy where the original is bound, and point every call at the version it reaches: a call in a copy is judged in the reuse version where the copied function makes it and in the base one where a function defined within it does. A call a copy makes to a function defined within the copied body keeps the copy of that function the copy holds, which no other copy is in scope of.
fn copy_reuse_versions(module: &mut Module, mut budget: CopyBudget) {
    let routes = routes(module);
    if routes.copied.is_empty() {
        return;
    }
    let calls = module
        .live_nodes()
        .filter_map(|(node, definition)| match definition {
            Node::ApplyFun {
                callee: Callee::Known(callee),
                ..
            } => Some((node, *callee)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut versions = BTreeMap::<FunctionId, Version>::new();
    for function in routes.copied {
        let Some(plan) = CopyPlan::functions_with_limit(
            module,
            BTreeSet::from([function]),
            REBUILD_CLONE_NODE_LIMIT,
        ) else {
            continue;
        };
        let size = plan.nodes.len();
        if !budget.admits(size) {
            continue;
        }
        let own = function_nodes(module, function)
            .into_iter()
            .collect::<BTreeSet<_>>();
        let copied = copy_bodies(module, plan);
        let copy = copied.functions[&function];
        let binder = module
            .live_nodes()
            .find_map(|(node, definition)| match definition {
                Node::LetFun { functions, .. } if functions.contains(&function) => Some(node),
                _ => None,
            });
        if let Some(Node::LetFun { functions, .. }) =
            binder.and_then(|binder| module.node_mut(binder))
        {
            functions.push(copy);
        }
        budget.charge(size);
        versions.insert(
            function,
            Version {
                copy,
                nodes: copied.nodes,
                own,
                nested: copied.functions,
            },
        );
    }
    let target = |callee: FunctionId, reuse: bool| match reuse {
        true => versions.get(&callee).map_or(callee, |version| version.copy),
        false => callee,
    };
    let mut retargets = Vec::new();
    for (call, callee) in calls {
        if routes.reuse.contains(&(call, World::Base)) {
            retargets.push((call, target(callee, true)));
        }
        for (original, version) in &versions {
            let Some(copied) = version.nodes.get(&call) else {
                continue;
            };
            let world = match version.own.contains(&call) {
                true => World::Reuse,
                false => World::Base,
            };
            let reuse = routes.reuse.contains(&(call, world));
            if callee == *original {
                retargets.push((*copied, if reuse { version.copy } else { callee }));
            } else if !version.nested.contains_key(&callee) {
                retargets.push((*copied, target(callee, reuse)));
            }
        }
    }
    for (node, callee) in retargets {
        if let Some(Node::ApplyFun { callee: called, .. }) = module.node_mut(node) {
            *called = Callee::Known(callee);
        }
    }
}

#[cfg(test)]
mod tests;
