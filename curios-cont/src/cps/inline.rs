use {
    super::*,
    super::{
        analysis::{analyze_calls, available_values, free_values, function_nodes, nodes_from},
        optimize::{MULTI_SITE_INLINE_LIMIT, SCC_CLONE_NODE_LIMIT},
        reachable::prune_unreachable,
    },
    std::collections::{BTreeMap, BTreeSet},
};

pub(super) fn inline_known_calls(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Inline in sweeps: build the whole-module call analysis once, then inline
    // every candidate it exposes before rebuilding. Rebuilding per inline is what
    // made this quadratic on a large unoptimized module. Per-callee facts
    // (`free_values`, body shape) are stable across a sweep because inlining a call
    // copies the callee rather than mutating it, and a surviving call node keeps
    // its owner; only the site counts go stale within a sweep, and a stale count
    // only tightens the size budget, so the calls it defers are picked up by the
    // next sweep's fresh analysis. Inlining that exposes a call inside a copied body
    // is likewise handled by the following sweep.
    for _ in 0..10_000 {
        let analysis = analyze_calls(module);
        let mut inlined_any = false;
        for index in 0..module.nodes.len() {
            let node_id = CpsNodeId(index as u32);
            // Re-read: an earlier inline in this sweep may have removed or rewritten
            // this node.
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
            let nodes = function_nodes(module, callee);
            let owner_values = available_values(module, owner);
            if !free_values(module, callee).is_subset(&owner_values) {
                continue;
            }
            if nodes.iter().any(|node| {
                matches!(
                    module.node(*node).unwrap(),
                    CpsNode::LetFun { .. } | CpsNode::RecInit { .. }
                )
            }) {
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
    // Inline in sweeps: build the recursive-value set and the transfer index once
    // per sweep rather than once per inline. Inlining a single-use continuation
    // moves its one transfer without duplicating it, so it never changes another
    // continuation's transfer count — the snapshot stays valid for the rest of the
    // sweep. Each candidate is re-read against the live module, and the module is
    // pruned once at the end of each sweep rather than after every inline.
    for _ in 0..10_000 {
        let recursive_values = module
            .nodes
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
            // Re-read: an earlier inline (and its prune) in this sweep may have
            // removed or rewritten this continuation.
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
        // Prune once per sweep rather than once per inline: `nodes_from` tolerates
        // the transient dangling `LetCont` references left within the sweep, so the
        // repair only has to happen before the next sweep rebuilds its transfer
        // index.
        prune_unreachable(module);
    }
    changed
}
/// Index every continuation to the nodes that transfer control to it, one entry
/// per referencing node in ascending node order. Building this once per rewrite
/// pass keeps single-use detection linear instead of rescanning every node for
/// each continuation.
fn continuation_transfers(module: &CpsModule) -> BTreeMap<CpsContId, Vec<CpsNodeId>> {
    let mut transfers: BTreeMap<CpsContId, Vec<CpsNodeId>> = BTreeMap::new();
    let mut targets = BTreeSet::new();
    for (index, node) in module.nodes.iter().enumerate() {
        let Some(node) = node.as_ref() else {
            continue;
        };
        targets.clear();
        collect_control_targets(node, &mut targets);
        for &target in &targets {
            transfers
                .entry(target)
                .or_default()
                .push(CpsNodeId(index as u32));
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
        | CpsNode::LetPrim { .. }
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

    for node in &substitution_nodes {
        let node = module.nodes[node.index()].as_mut().unwrap();
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

    let body = module.nodes[definition.body.index()].take().unwrap();
    module.nodes[call.index()] = Some(body);
    module.continuations[continuation.index()] = None;
    for param in definition.params {
        module.values[param.index()] = None;
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
    let node_ids = function_nodes(module, callee);
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

    // Bail before minting anything: only a parameter can map a closure callee
    // to a literal (locals map to fresh values below), so this check is
    // complete against the parameter substitutions alone, and an aborted
    // attempt must leave no orphaned arena entries behind.
    if nodes.values().any(|node| {
        matches!(node, CpsNode::ApplyFun { callee: CpsCallee::Closure(value), .. }
            if matches!(values.get(value), Some(CpsAtom::Literal(_))))
    }) {
        return false;
    }

    for node in nodes.values() {
        if let CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } = node {
            let definition = module.values[result.index()].as_ref().unwrap().clone();
            let fresh = module.add_value(definition.debug_name);
            values.insert(*result, CpsAtom::Value(fresh));
        }
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
            let definition = module.values[param.index()].as_ref().unwrap().clone();
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

    let map_atom = |atom: &CpsAtom| match atom {
        CpsAtom::Value(value) => values.get(value).cloned().unwrap_or(CpsAtom::Value(*value)),
        atom => atom.clone(),
    };
    let map_value = |value: CpsValueId| match values.get(&value) {
        Some(CpsAtom::Value(value)) => *value,
        _ => value,
    };
    let map_cont = |target: CpsContId| {
        if target == function.return_cont {
            return_to
        } else {
            continuations.get(&target).copied().unwrap_or(target)
        }
    };
    let map_edge = |edge: &CpsEdge| CpsEdge {
        target: map_cont(edge.target),
        args: edge.args.iter().map(&map_atom).collect(),
    };

    let mut cloned_nodes = BTreeMap::new();
    for (&old, node) in &nodes {
        let cloned = match node {
            CpsNode::LetValue {
                result,
                value,
                next,
            } => CpsNode::LetValue {
                result: map_value(*result),
                value: match value {
                    CpsValueExpr::Literal(literal) => CpsValueExpr::Literal(literal.clone()),
                    CpsValueExpr::List(atoms) => {
                        CpsValueExpr::List(atoms.iter().map(&map_atom).collect())
                    }
                    CpsValueExpr::Tuple(atoms) => {
                        CpsValueExpr::Tuple(atoms.iter().map(&map_atom).collect())
                    }
                },
                next: node_map[next],
            },
            CpsNode::LetPrim {
                result,
                op,
                args,
                next,
            } => CpsNode::LetPrim {
                result: map_value(*result),
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                next: node_map[next],
            },
            CpsNode::LetCont {
                continuations: members,
                body,
            } => CpsNode::LetCont {
                continuations: members.iter().map(|id| continuations[id]).collect(),
                body: node_map[body],
            },
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => CpsNode::ApplyFun {
                callee: match callee {
                    CpsCallee::Known(function) => CpsCallee::Known(*function),
                    CpsCallee::Closure(value) => match map_atom(&CpsAtom::Value(*value)) {
                        CpsAtom::Value(value) => CpsCallee::Closure(value),
                        CpsAtom::Fun(function) => CpsCallee::Known(function),
                        CpsAtom::Literal(_) => return false,
                    },
                },
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::ApplyCont(edge) => CpsNode::ApplyCont(map_edge(edge)),
            CpsNode::Switch {
                scrutinee,
                cases,
                default,
            } => CpsNode::Switch {
                scrutinee: map_atom(scrutinee),
                cases: cases
                    .iter()
                    .map(|(tag, edge)| (*tag, map_edge(edge)))
                    .collect(),
                default: default.as_ref().map(map_edge),
            },
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => CpsNode::Foreign {
                function: function.clone(),
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => CpsNode::Cell {
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Intrinsic {
                op,
                args,
                return_to,
            } => CpsNode::Intrinsic {
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Exit { value } => CpsNode::Exit {
                value: value.as_ref().map(&map_atom),
            },
            CpsNode::Unreachable => CpsNode::Unreachable,
            CpsNode::LetFun { .. } | CpsNode::RecInit { .. } => return false,
        };
        cloned_nodes.insert(node_map[&old], cloned);
    }

    for (&old, continuation) in &continuation_defs {
        module.continuations[continuations[&old].index()] = Some(CpsContinuation {
            debug_name: continuation.debug_name.clone(),
            params: continuation
                .params
                .iter()
                .map(|id| map_value(*id))
                .collect(),
            body: node_map[&continuation.body],
        });
    }
    for (id, node) in cloned_nodes {
        module.nodes[id.index()] = Some(node);
    }
    true
}
