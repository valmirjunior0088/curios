use {
    super::*,
    super::{
        analysis::{analyze_calls, available_values, free_values, function_nodes, nodes_from},
        clone::{Mapping, clone_node, copied_extent},
        optimize::{MULTI_SITE_INLINE_LIMIT, SCC_CLONE_NODE_LIMIT},
        reachable::prune_unreachable,
    },
    std::collections::{BTreeMap, BTreeSet},
};

pub(super) fn inline_known_calls(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Inline in sweeps: build the whole-module call analysis once, then inline every candidate it exposes before rebuilding. Rebuilding per inline is what made this quadratic on a large unoptimized module. Per-callee facts (`free_values`, body shape) are stable across a sweep because inlining a call copies the callee rather than mutating it, and a surviving call node keeps its owner; only the site counts go stale within a sweep, and a stale count only tightens the size budget, so the calls it defers are picked up by the next sweep's fresh analysis. Inlining that exposes a call inside a copied body is likewise handled by the following sweep.
    for _ in 0..10_000 {
        let analysis = analyze_calls(module);
        let mut inlined_any = false;
        for index in 0..module.nodes.len() {
            let node_id = CpsNodeId(index as u32);
            // Re-read: an earlier inline in this sweep may have removed or rewritten this node.
            let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                return_to,
            }) = module.node(node_id)
            else {
                continue;
            };
            let (callee, args, return_to) = (*callee, args.clone(), *return_to);
            if Some(callee) == module.entry || analysis.recursive.contains(&callee) {
                continue;
            }
            let Some(&owner) = analysis.node_owners.get(&node_id) else {
                continue;
            };
            // The extent rather than the body: `function_nodes` walks a `LetFun`'s continuation and not its members, which cost nothing while such a body was refused outright and understates the duplication now that one is admitted — and a multi-site inline copies those members once per site.
            let (nodes, _) = copied_extent(module, function_nodes(module, callee));
            let owner_values = available_values(module, owner);
            if !free_values(module, callee).is_subset(&owner_values) {
                continue;
            }
            let sites = analysis.call_sites.get(&callee).map_or(0, Vec::len);
            let duplicated = sites > 1 || analysis.escaping.contains(&callee);
            let limit = if duplicated {
                MULTI_SITE_INLINE_LIMIT
            } else {
                SCC_CLONE_NODE_LIMIT
            };
            if nodes.len() > limit {
                continue;
            }
            if inline_call(module, node_id, callee, &args, return_to) {
                changed = true;
                inlined_any = true;
            }
        }
        if !inlined_any {
            break;
        }
    }
    changed
}
pub(super) fn inline_single_use_continuations(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Inline in sweeps: build the recursive-value set and the transfer index once per sweep rather than once per inline. Inlining a single-use continuation moves its one transfer without duplicating it, so it never changes another continuation's transfer count — the snapshot stays valid for the rest of the sweep. Each candidate is re-read against the live module, and the module is pruned once at the end of each sweep rather than after every inline.
    for _ in 0..10_000 {
        let recursive_values = module
            .nodes
            .slots()
            .iter()
            .flatten()
            .filter_map(|node| match node {
                CpsNode::RecInit { values, .. } => Some(values.as_slice()),
                _ => None,
            })
            .flatten()
            .copied()
            .collect::<BTreeSet<_>>();
        let transfers_by_target = continuation_transfers(module);
        let mut inlined_any = false;
        for index in 0..module.continuations.len() {
            let target = CpsContId(index as u32);
            // Re-read: an earlier inline (and its prune) in this sweep may have removed or rewritten this continuation.
            let Some(continuation) = module.continuation(target) else {
                continue;
            };
            if continuation
                .params
                .iter()
                .any(|value| recursive_values.contains(value))
            {
                continue;
            }
            let Some(transfers) = transfers_by_target.get(&target) else {
                continue;
            };
            if transfers.len() != 1 || transfers[0] == continuation.body {
                continue;
            }
            let call = transfers[0];
            let params_len = continuation.params.len();
            let Some(CpsNode::ApplyCont(edge)) = module.node(call) else {
                continue;
            };
            if edge.target != target || edge.args.len() != params_len {
                continue;
            }
            let args = edge.args.clone();
            if inline_continuation(module, target, call, &args) {
                changed = true;
                inlined_any = true;
            }
        }
        if !inlined_any {
            break;
        }
        // Prune once per sweep rather than once per inline: `nodes_from` tolerates the transient dangling `LetCont` references left within the sweep, so the repair only has to happen before the next sweep rebuilds its transfer index.
        prune_unreachable(module);
    }
    changed
}
/// Index every continuation to the nodes that transfer control to it, one entry per referencing node in ascending node order. Building this once per rewrite pass keeps single-use detection linear instead of rescanning every node for each continuation.
pub(super) fn continuation_transfers(module: &CpsModule) -> BTreeMap<CpsContId, Vec<CpsNodeId>> {
    let mut transfers: BTreeMap<CpsContId, Vec<CpsNodeId>> = BTreeMap::new();
    let mut targets = BTreeSet::new();
    for (id, node) in module.nodes.iter_live() {
        targets.clear();
        collect_control_targets(node, &mut targets);
        for &target in &targets {
            transfers.entry(target).or_default().push(id);
        }
    }
    transfers
}
fn collect_control_targets(node: &CpsNode, targets: &mut BTreeSet<CpsContId>) {
    match node {
        CpsNode::ApplyFun { return_to, .. }
        | CpsNode::Foreign { return_to, .. }
        | CpsNode::Cell { return_to, .. }
        | CpsNode::Intrinsic { return_to, .. } => {
            targets.insert(*return_to);
        }
        CpsNode::ApplyCont(edge) => {
            targets.insert(edge.target);
        }
        CpsNode::Switch { cases, default, .. } => {
            targets.extend(cases.values().chain(default.iter()).map(|edge| edge.target));
        }
        CpsNode::LetValue { .. }
        | CpsNode::LetIntrinsic { .. }
        | CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Exit { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => {}
    }
}
pub(super) fn inline_continuation(
    module: &mut CpsModule,
    continuation: CpsContId,
    call: CpsNodeId,
    args: &[CpsAtom],
) -> bool {
    let definition = module.continuation(continuation).unwrap().clone();
    let substitutions = definition
        .params
        .iter()
        .copied()
        .zip(args.iter().cloned())
        .collect::<BTreeMap<_, _>>();
    let body_nodes = nodes_from(module, definition.body);
    let mut substitution_nodes = body_nodes.iter().copied().collect::<BTreeSet<_>>();
    let mut function_work = body_nodes
        .iter()
        .filter_map(|node| match module.node(*node).unwrap() {
            CpsNode::LetFun { functions, .. } | CpsNode::RecInit { functions, .. } => {
                Some(functions.as_slice())
            }
            _ => None,
        })
        .flatten()
        .copied()
        .collect::<Vec<_>>();
    let mut functions = BTreeSet::new();
    while let Some(function) = function_work.pop() {
        if !functions.insert(function) {
            continue;
        }
        for node in function_nodes(module, function) {
            if substitution_nodes.insert(node) {
                match module.node(node).unwrap() {
                    CpsNode::LetFun { functions, .. } | CpsNode::RecInit { functions, .. } => {
                        function_work.extend(functions.iter().copied());
                    }
                    _ => {}
                }
            }
        }
    }
    if substitution_nodes.iter().any(|node| {
        matches!(
            module.node(*node),
            Some(CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
                ..
            }) if matches!(substitutions.get(value), Some(CpsAtom::Literal(_)))
        )
    }) {
        return false;
    }

    for &node in &substitution_nodes {
        let node = module.nodes.get_mut(node).unwrap();
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Value(value) = atom
                && let Some(replacement) = substitutions.get(value)
            {
                *atom = replacement.clone();
            }
        });
        if let CpsNode::ApplyFun { callee, .. } = node
            && let CpsCallee::Closure(value) = *callee
            && let Some(replacement) = substitutions.get(&value)
        {
            *callee = match replacement {
                CpsAtom::Value(value) => CpsCallee::Closure(*value),
                CpsAtom::Fun(function) => CpsCallee::Known(*function),
                CpsAtom::Literal(_) => unreachable!(),
            };
        }
    }

    let body = module.nodes.remove(definition.body).unwrap();
    module.nodes.set(call, body);
    module.continuations.remove(continuation);
    for param in definition.params {
        module.values.remove(param);
    }
    true
}
pub(super) fn inline_call(
    module: &mut CpsModule,
    call: CpsNodeId,
    callee: CpsFunId,
    args: &[CpsAtom],
    return_to: CpsContId,
) -> bool {
    let function = module.function(callee).unwrap().clone();
    let (extent, nested) = copied_extent(module, function_nodes(module, callee));
    let node_ids: Vec<CpsNodeId> = extent.into_iter().collect();
    let nested_defs: BTreeMap<CpsFunId, CpsFunction> = nested
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
            CpsNode::LetCont { continuations, .. } => Some(continuations.as_slice()),
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

    // Bail before minting anything: only a parameter can map a closure callee to a literal (locals map to fresh values below), so this check is complete against the parameter substitutions alone, and an aborted attempt must leave no orphaned arena entries behind.
    if nodes.values().any(|node| {
        matches!(node, CpsNode::ApplyFun { callee: CpsCallee::Closure(value), .. }
            if matches!(values.get(value), Some(CpsAtom::Literal(_))))
    }) {
        return false;
    }

    let mut owned: Vec<CpsValueId> = Vec::new();
    for node in nodes.values() {
        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
                owned.push(*result)
            }
            CpsNode::RecInit { values, .. } => owned.extend(values.iter().copied()),
            _ => {}
        }
    }
    // A definition nested in the inlined body is copied with it, so its parameters are owned by the copy just as the body's own bindings are.
    for definition in nested_defs.values() {
        owned.extend(definition.params.iter().copied());
    }
    for old in owned {
        let definition = module.values.get(old).unwrap().clone();
        let fresh = module.add_value(definition.debug_name);
        values.insert(old, CpsAtom::Value(fresh));
    }
    let mut functions: BTreeMap<CpsFunId, CpsFunId> = BTreeMap::new();
    let mut returns: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
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
            let definition = module.values.get(param).unwrap().clone();
            let fresh = module.add_value(definition.debug_name);
            values.insert(param, CpsAtom::Value(fresh));
        }
    }

    let mut node_map = BTreeMap::from([(function.body, call)]);
    for &node in &node_ids {
        if node != function.body {
            node_map.insert(node, module.reserve_node());
        }
    }

    let map_function = |id: CpsFunId| functions.get(&id).copied().unwrap_or(id);
    // A nested definition is copied, so every reference to it inside the copy has to name the copy — as an atom, and as a direct callee.
    let map_atom = |atom: &CpsAtom| match atom {
        CpsAtom::Value(value) => values.get(value).cloned().unwrap_or(CpsAtom::Value(*value)),
        CpsAtom::Fun(function) => CpsAtom::Fun(map_function(*function)),
        CpsAtom::Literal(_) => atom.clone(),
    };
    let map_value = |value: CpsValueId| match values.get(&value) {
        Some(CpsAtom::Value(value)) => *value,
        _ => value,
    };
    let map_cont = |target: CpsContId| {
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
    let map_callee = |callee: &CpsCallee| match callee {
        CpsCallee::Known(function) => CpsCallee::Known(map_function(*function)),
        CpsCallee::Closure(value) => match map_atom(&CpsAtom::Value(*value)) {
            CpsAtom::Value(value) => CpsCallee::Closure(value),
            CpsAtom::Fun(function) => CpsCallee::Known(function),
            // The pre-minting bail above already rejected every literal-mapped closure callee, and bailing here would leak the minted values and reserved slots.
            CpsAtom::Literal(_) => unreachable!(),
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
        cloned_nodes.insert(node_map[&old], clone_node(node, &map));
    }

    for (&old, continuation) in &continuation_defs {
        module.continuations.define(
            continuations[&old],
            CpsContinuation {
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
            CpsFunction {
                debug_name: definition.debug_name.clone(),
                params: definition.params.iter().map(|id| map_value(*id)).collect(),
                return_cont: returns[&definition.return_cont],
                body: node_map[&definition.body],
            },
        );
    }
    // The callee's body clone lands on the live call node (`node_map` seeds `function.body -> call`); every other clone fills a slot reserved above.
    for (id, node) in cloned_nodes {
        if id == call {
            module.nodes.set(id, node);
        } else {
            module.nodes.define(id, node);
        }
    }
    true
}
