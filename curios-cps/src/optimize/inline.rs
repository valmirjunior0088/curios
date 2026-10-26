use {
    crate::{
        Atom, Callee, Continuation, ContinuationId, CopyPlan, Function, FunctionId,
        MULTI_SITE_INLINE_LIMIT, Mapping, Module, Node, NodeId, SCC_CLONE_NODE_LIMIT, ValueId,
        analyze_calls, clone_node, prune_unreachable,
    },
    std::collections::{BTreeMap, BTreeSet},
};

pub(crate) fn inline_known_calls(module: &mut Module) -> bool {
    let mut changed = false;
    // Inline in sweeps: build the whole-module call analysis once, then inline every candidate it exposes before rebuilding. Rebuilding per inline would make this quadratic on a large unoptimized module. Each call and its complete copy extent are read from the current graph: an earlier inline inside a callee can enlarge its body in this sweep. Snapshot site counts only tighten the size budget as calls disappear, so deferred work is picked up by the next sweep's fresh analysis. New calls inside a copied body likewise wait for the following sweep.
    for _ in 0..10_000 {
        let analysis = analyze_calls(module);
        let mut inlined_any = false;
        for index in 0..module.nodes().len() {
            let node_id = NodeId(index as u32);
            // Re-read: an earlier inline in this sweep may have removed or rewritten this node.
            let Some(Node::ApplyFun {
                callee: Callee::Known(callee),
                args,
                return_to,
            }) = module.node(node_id)
            else {
                continue;
            };
            let (callee, return_to) = (*callee, *return_to);
            if Some(callee) == module.entry()
                || analysis.recursive.contains(&callee)
                || module.is_out_of_line(callee)
            {
                continue;
            }
            if !analysis.node_owners.contains_key(&node_id) {
                continue;
            }
            let sites = analysis.call_sites.get(&callee).map_or(0, Vec::len);
            let duplicated = sites > 1 || analysis.escaping.contains(&callee);
            let limit = if duplicated {
                MULTI_SITE_INLINE_LIMIT
            } else {
                SCC_CLONE_NODE_LIMIT
            };
            // The extent rather than the body: `function_nodes` walks a `LetFun`'s continuation and not its members, so it understates the duplication of a body that nests definitions — and a multi-site inline copies those members once per site. Bound the request itself so an oversized callee is not walked in full at every site.
            let Some(plan) =
                CopyPlan::functions_with_limit(module, BTreeSet::from([callee]), limit)
            else {
                continue;
            };
            let args = args.clone();
            if inline_call(module, node_id, callee, &args, return_to, plan) {
                changed = true;
                inlined_any = true;
            }
        }
        if !inlined_any {
            break;
        }
        // Prune once per sweep, as `inline_single_use_continuations` below does: an inlined callee's original body is dead but still carries call sites, and a site in dead code would turn the next sweep's single-site callee into a duplicated one — the compounding that makes a sequencing chain's continuations exponential in its length.
        prune_unreachable(module);
    }
    changed
}
/// Index every continuation to the nodes that transfer control to it, one entry per referencing node in ascending node order. Building this once per rewrite pass keeps a reader linear instead of rescanning every node for each continuation.
pub(crate) fn continuation_transfers(module: &Module) -> BTreeMap<ContinuationId, Vec<NodeId>> {
    let mut transfers: BTreeMap<ContinuationId, Vec<NodeId>> = BTreeMap::new();
    let mut targets = BTreeSet::new();
    for (id, node) in module.live_nodes() {
        targets.clear();
        collect_control_targets(node, &mut targets);
        for &target in &targets {
            transfers.entry(target).or_default().push(id);
        }
    }
    transfers
}
pub(crate) fn collect_control_targets(node: &Node, targets: &mut BTreeSet<ContinuationId>) {
    match node {
        Node::ApplyFun { return_to, .. }
        | Node::Foreign { return_to, .. }
        | Node::Cell { return_to, .. }
        | Node::Channel { return_to, .. }
        | Node::Intrinsic { return_to, .. } => {
            targets.insert(*return_to);
        }
        Node::ApplyCont(edge) => {
            targets.insert(edge.target);
        }
        Node::Switch { cases, default, .. } => {
            targets.extend(cases.values().chain(default.iter()).map(|edge| edge.target));
        }
        Node::LetValue { .. }
        | Node::LetIntrinsic { .. }
        | Node::LetFun { .. }
        | Node::LetCont { .. }
        | Node::Halt { .. }
        | Node::Panic(_)
        | Node::Unreachable => {}
    }
}
pub(crate) fn inline_call(
    module: &mut Module,
    call: NodeId,
    callee: FunctionId,
    args: &[Atom],
    return_to: ContinuationId,
    plan: CopyPlan,
) -> bool {
    let function = module.function(callee).unwrap().clone();
    let node_ids: Vec<NodeId> = plan.nodes.into_iter().collect();
    let nested_defs: BTreeMap<FunctionId, Function> = plan
        .nested
        .iter()
        .map(|&id| (id, module.function(id).unwrap().clone()))
        .collect();
    let nodes = node_ids
        .iter()
        .map(|id| (*id, module.node(*id).unwrap().clone()))
        .collect::<BTreeMap<_, _>>();
    let local_continuations = nodes
        .values()
        .filter_map(|node| match node {
            Node::LetCont { continuations, .. } => Some(continuations.as_slice()),
            _ => None,
        })
        .flatten()
        .copied()
        .collect::<BTreeSet<_>>();

    let mut values = function
        .params
        .iter()
        .copied()
        .zip(args.iter().cloned())
        .collect::<BTreeMap<_, _>>();

    // Bail before minting anything: only a parameter can map a closure callee to a literal or a filler (locals map to fresh values below), so this check is complete against the parameter substitutions alone, and an aborted attempt must leave no orphaned arena entries behind. The filler half is what `split_workers`' padding and dead-parameter elimination can put in an argument position; neither is a callee `map_callee` could name, and reaching it there would panic after minting rather than declining here.
    if nodes.values().any(|node| {
        matches!(node, Node::ApplyFun { callee: Callee::Closure(value), .. }
            if matches!(values.get(value), Some(Atom::Literal(_) | Atom::Filler)))
    }) {
        return false;
    }

    let mut owned: Vec<ValueId> = Vec::new();
    for node in nodes.values() {
        match node {
            Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. } => {
                owned.push(*result)
            }
            _ => {}
        }
    }
    // A definition nested in the inlined body is copied with it, so its parameters are owned by the copy just as the body's own bindings are.
    for definition in nested_defs.values() {
        owned.extend(definition.params.iter().copied());
    }
    for old in owned {
        let definition = module.value(old).unwrap().clone();
        let fresh = module.add_value_def(definition);
        values.insert(old, Atom::Value(fresh));
    }
    let mut functions: BTreeMap<FunctionId, FunctionId> = BTreeMap::new();
    let mut returns: BTreeMap<ContinuationId, ContinuationId> = BTreeMap::new();
    for (&id, definition) in &nested_defs {
        functions.insert(id, module.reserve_function());
        returns.insert(definition.return_cont, module.reserve_continuation());
    }

    let mut continuations = BTreeMap::new();
    let continuation_defs = local_continuations
        .iter()
        .map(|id| (*id, module.continuation(*id).unwrap().clone()))
        .collect::<BTreeMap<_, _>>();
    for (&id, continuation) in &continuation_defs {
        let fresh = module.reserve_continuation();
        continuations.insert(id, fresh);
        for &param in &continuation.params {
            let definition = module.value(param).unwrap().clone();
            let fresh = module.add_value_def(definition);
            values.insert(param, Atom::Value(fresh));
        }
    }

    let mut node_map = BTreeMap::from([(function.body, call)]);
    for &node in &node_ids {
        if node != function.body {
            node_map.insert(node, module.reserve_node());
        }
    }

    let map_function = |id: FunctionId| functions.get(&id).copied().unwrap_or(id);
    // A nested definition is copied, so every reference to it inside the copy has to name the copy — as an atom, and as a direct callee.
    let map_atom = |atom: &Atom| match atom {
        Atom::Value(value) => values.get(value).cloned().unwrap_or(Atom::Value(*value)),
        Atom::Fun(function) => Atom::Fun(map_function(*function)),
        Atom::Literal(_) | Atom::Filler => atom.clone(),
    };
    let map_value = |value: ValueId| match values.get(&value) {
        Some(Atom::Value(value)) => *value,
        _ => value,
    };
    let map_cont = |target: ContinuationId| {
        if target == function.return_cont {
            return_to
        } else {
            returns
                .get(&target)
                .copied()
                .or_else(|| continuations.get(&target).copied())
                .unwrap_or(target)
        }
    };

    // A callee parameter may map to a function reference, which turns an indirect call in the body into a direct one as the copy is made.
    let map_callee = |callee: &Callee| match callee {
        Callee::Known(function) => Callee::Known(map_function(*function)),
        Callee::Closure(value) => match map_atom(&Atom::Value(*value)) {
            Atom::Value(value) => Callee::Closure(value),
            Atom::Fun(function) => Callee::Known(function),
            // The pre-minting bail above already rejected every closure callee a parameter maps to a literal or a filler, and bailing here would leak the minted values and reserved slots.
            Atom::Literal(_) | Atom::Filler => unreachable!(),
        },
    };
    let map = Mapping {
        value: &map_value,
        atom: &map_atom,
        cont: &map_cont,
        callee: &map_callee,
        function: &map_function,
        node: &|id| node_map[&id],
    };
    let mut cloned_nodes = BTreeMap::new();
    for (&old, node) in &nodes {
        // A closure call an argument makes a call of a function of another arity is copied as what it is — see [`Module::applies`].
        let copy = match node {
            Node::ApplyFun {
                callee: Callee::Closure(value),
                args,
                ..
            } if matches!(
                values.get(value),
                Some(Atom::Fun(function)) if !module.applies(*function, args.len())
            ) =>
            {
                Node::Unreachable
            }
            _ => clone_node(node, &map),
        };
        cloned_nodes.insert(node_map[&old], copy);
    }

    for (&old, continuation) in &continuation_defs {
        module.define_continuation(
            continuations[&old],
            Continuation {
                debug_name: continuation.debug_name.clone(),
                params: continuation
                    .params
                    .iter()
                    .map(|id| map_value(*id))
                    .collect(),
                body: node_map[&continuation.body],
            },
        );
    }
    // Each nested definition is copied into the caller alongside the body that introduces it. Its return sentinel stays a reserved, bodyless identity, which is what the verifier requires of one.
    for (&old, definition) in &nested_defs {
        module.define_function(
            functions[&old],
            Function {
                debug_name: definition.debug_name.clone(),
                params: definition.params.iter().map(|id| map_value(*id)).collect(),
                return_cont: returns[&definition.return_cont],
                body: node_map[&definition.body],
                droppable: definition.droppable,
            },
        );
    }
    // The callee's body clone lands on the live call node (`node_map` seeds `function.body -> call`); every other clone fills a slot reserved above.
    for (id, node) in cloned_nodes {
        if id == call {
            module.set_node(id, node);
        } else {
            module.define_node(id, node);
        }
    }
    true
}

#[cfg(test)]
mod tests;
