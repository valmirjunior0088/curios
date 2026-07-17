//! Deterministic high-CPS canonicalization and propagation.

use {
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsLiteral, CpsModule,
        CpsNode, CpsNodeId, CpsPrimOp, CpsUseTarget, CpsValueExpr, CpsValueId, atoms,
        visit_atoms_mut,
    },
    std::collections::{BTreeMap, BTreeSet, VecDeque},
};

pub(super) const MULTI_SITE_INLINE_LIMIT: usize = 8;
pub(super) const BRANCH_SPECIALIZATION_GROWTH_LIMIT: usize = 24;
pub(super) const PURE_EVALUATION_STEP_LIMIT: usize = 10_000;
pub(super) const PURE_EVALUATION_DEPTH_LIMIT: usize = 256;
pub(super) const SCC_CLONE_LIMIT: usize = 64;
pub(super) const SCC_CLONE_NODE_LIMIT: usize = 256;

#[derive(Clone)]
enum Knowledge {
    Unknown,
    Known(CpsAtom),
    Conflict,
}

impl Knowledge {
    fn merge(&mut self, incoming: Option<&CpsAtom>) {
        match (&*self, incoming) {
            (Self::Conflict, _) | (Self::Unknown, None) => {}
            (Self::Unknown, Some(atom)) => *self = Self::Known(atom.clone()),
            (Self::Known(_), None) => *self = Self::Conflict,
            (Self::Known(current), Some(incoming)) if current == incoming => {}
            (Self::Known(_), Some(_)) => *self = Self::Conflict,
        }
    }
}

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are
/// rebuilt at deterministic boundaries instead of being kept as shadow state.
pub(crate) fn optimize(module: &mut CpsModule) {
    module
        .verify()
        .expect("invalid high CPS before optimization");

    let _fixed_limits = (
        MULTI_SITE_INLINE_LIMIT,
        BRANCH_SPECIALIZATION_GROWTH_LIMIT,
        PURE_EVALUATION_STEP_LIMIT,
        PURE_EVALUATION_DEPTH_LIMIT,
        SCC_CLONE_LIMIT,
        SCC_CLONE_NODE_LIMIT,
    );

    for _ in 0..32 {
        let substitutions = known_values(module);
        let changed = rewrite_atoms(module, &substitutions)
            | forward_continuations(module)
            | forward_aggregate_projections(module)
            | simplify_nodes(module)
            | eliminate_dead_bindings(module)
            | eliminate_dead_parameters(module)
            | inline_single_use_continuations(module)
            | inline_known_calls(module)
            | contify_self_tail_calls(module)
            | prune_unreachable(module);
        if !changed {
            break;
        }
    }

    module.rebuild_uses();
    module
        .verify()
        .expect("invalid high CPS after optimization");
}

#[derive(Default)]
struct CallAnalysis {
    call_sites: BTreeMap<CpsFunId, Vec<CpsNodeId>>,
    call_graph: BTreeMap<CpsFunId, BTreeSet<CpsFunId>>,
    node_owners: BTreeMap<CpsNodeId, CpsFunId>,
    escaping: BTreeSet<CpsFunId>,
    recursive: BTreeSet<CpsFunId>,
}

fn analyze_calls(module: &CpsModule) -> CallAnalysis {
    let mut analysis = CallAnalysis::default();
    for (index, function) in module.functions.iter().enumerate() {
        if function.is_some() {
            let function = CpsFunId(index as u32);
            analysis.call_sites.entry(function).or_default();
            analysis.call_graph.entry(function).or_default();
        }
    }

    for (owner_index, function) in module.functions.iter().enumerate() {
        let Some(_) = function else { continue };
        let owner = CpsFunId(owner_index as u32);
        for node_id in function_nodes(module, owner) {
            analysis.node_owners.insert(node_id, owner);
            let node = module.node(node_id).unwrap();
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                ..
            } = node
            {
                analysis
                    .call_sites
                    .entry(*callee)
                    .or_default()
                    .push(node_id);
                analysis
                    .call_graph
                    .entry(owner)
                    .or_default()
                    .insert(*callee);
            }
            for atom in atoms(node) {
                if let CpsAtom::Fun(function) = atom {
                    analysis.escaping.insert(*function);
                }
            }
        }
    }

    for &start in analysis.call_graph.keys() {
        let mut work = analysis.call_graph[&start]
            .iter()
            .copied()
            .collect::<Vec<_>>();
        let mut visited = BTreeSet::new();
        while let Some(function) = work.pop() {
            if function == start {
                analysis.recursive.insert(start);
                break;
            }
            if visited.insert(function)
                && let Some(next) = analysis.call_graph.get(&function)
            {
                work.extend(next.iter().copied());
            }
        }
    }
    analysis
}

fn function_nodes(module: &CpsModule, function: CpsFunId) -> Vec<CpsNodeId> {
    let mut found = BTreeSet::new();
    let mut work = vec![module.function(function).unwrap().body];
    while let Some(node_id) = work.pop() {
        if !found.insert(node_id) {
            continue;
        }
        match module.node(node_id).unwrap() {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => work.push(*body),
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations.iter().rev() {
                    work.push(module.continuation(*continuation).unwrap().body);
                }
            }
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Unreachable => {}
        }
    }
    found.into_iter().collect()
}

fn inline_known_calls(module: &mut CpsModule) -> bool {
    let mut changed = false;
    for _ in 0..10_000 {
        let analysis = analyze_calls(module);
        let mut selected = None;
        for (index, node) in module.nodes.iter().enumerate() {
            let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                return_to,
            }) = node
            else {
                continue;
            };
            if Some(*callee) == module.entry || analysis.recursive.contains(callee) {
                continue;
            }
            let nodes = function_nodes(module, *callee);
            let owner = analysis.node_owners[&CpsNodeId(index as u32)];
            let owner_values = available_values(module, owner);
            if !free_values(module, *callee).is_subset(&owner_values) {
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
            let sites = analysis.call_sites.get(callee).map_or(0, Vec::len);
            let duplicated = sites > 1 || analysis.escaping.contains(callee);
            let limit = if duplicated {
                MULTI_SITE_INLINE_LIMIT
            } else {
                SCC_CLONE_NODE_LIMIT
            };
            if nodes.len() > limit {
                continue;
            }
            selected = Some((CpsNodeId(index as u32), *callee, args.clone(), *return_to));
            break;
        }

        let Some((call, callee, args, return_to)) = selected else {
            break;
        };
        if !inline_call(module, call, callee, &args, return_to) {
            break;
        }
        changed = true;
    }
    changed
}

fn inline_single_use_continuations(module: &mut CpsModule) -> bool {
    let mut changed = false;
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
        let mut selected = None;
        for (index, continuation) in module.continuations.iter().enumerate() {
            let Some(continuation) = continuation else {
                continue;
            };
            let target = CpsContId(index as u32);
            if continuation
                .params
                .iter()
                .any(|value| recursive_values.contains(value))
            {
                continue;
            }
            let transfers = module
                .nodes
                .iter()
                .enumerate()
                .filter_map(|(index, node)| {
                    let node = node.as_ref()?;
                    (control_target_count(node, target) != 0).then_some(CpsNodeId(index as u32))
                })
                .collect::<Vec<_>>();
            if transfers.len() != 1 || transfers[0] == continuation.body {
                continue;
            }
            let call = transfers[0];
            let Some(CpsNode::ApplyCont(edge)) = module.node(call) else {
                continue;
            };
            if edge.target == target && edge.args.len() == continuation.params.len() {
                selected = Some((target, call, edge.args.clone()));
                break;
            }
        }

        let Some((continuation, call, args)) = selected else {
            break;
        };
        if !inline_continuation(module, continuation, call, &args) {
            break;
        }
        prune_unreachable(module);
        changed = true;
    }
    changed
}

fn control_target_count(node: &CpsNode, target: CpsContId) -> usize {
    match node {
        CpsNode::ApplyFun { return_to, .. }
        | CpsNode::Foreign { return_to, .. }
        | CpsNode::Cell { return_to, .. }
        | CpsNode::Intrinsic { return_to, .. } => usize::from(*return_to == target),
        CpsNode::ApplyCont(edge) => usize::from(edge.target == target),
        CpsNode::Switch { cases, default, .. } => cases
            .values()
            .chain(default.iter())
            .filter(|edge| edge.target == target)
            .count(),
        CpsNode::LetValue { .. }
        | CpsNode::LetPrim { .. }
        | CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Exit { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => 0,
    }
}

fn inline_continuation(
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
    module.rebuild_uses();
    true
}

fn nodes_from(module: &CpsModule, body: CpsNodeId) -> Vec<CpsNodeId> {
    let mut found = BTreeSet::new();
    let mut work = vec![body];
    while let Some(node_id) = work.pop() {
        if !found.insert(node_id) {
            continue;
        }
        match module.node(node_id).unwrap() {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => work.push(*body),
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations.iter().rev() {
                    work.push(module.continuation(*continuation).unwrap().body);
                }
            }
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Unreachable => {}
        }
    }
    found.into_iter().collect()
}

fn contify_self_tail_calls(module: &mut CpsModule) -> bool {
    let analysis = analyze_calls(module);
    let mut selected = None;

    for (index, function) in module.functions.iter().enumerate() {
        let Some(function) = function else { continue };
        let callee = CpsFunId(index as u32);
        if Some(callee) == module.entry
            || analysis.escaping.contains(&callee)
            || !analysis.recursive.contains(&callee)
            || !analysis.call_graph[&callee].contains(&callee)
        {
            continue;
        }

        let sites = &analysis.call_sites[&callee];
        let external = sites
            .iter()
            .copied()
            .filter(|site| analysis.node_owners[site] != callee)
            .collect::<Vec<_>>();
        if external.len() != 1 {
            continue;
        }
        let external_owner = analysis.node_owners[&external[0]];
        if function_reaches(&analysis.call_graph, callee, external_owner)
            || !free_values(module, callee).is_subset(&available_values(module, external_owner))
        {
            continue;
        }

        let mut compatible = true;
        for &site in sites {
            let CpsNode::ApplyFun { return_to, .. } = module.node(site).unwrap() else {
                unreachable!()
            };
            if analysis.node_owners[&site] == callee && *return_to != function.return_cont {
                compatible = false;
                break;
            }
        }
        if compatible {
            selected = Some((callee, external[0]));
            break;
        }
    }

    let Some((callee, call)) = selected else {
        return false;
    };
    contify_self_tail_call(module, callee, call);
    true
}

fn function_reaches(
    graph: &BTreeMap<CpsFunId, BTreeSet<CpsFunId>>,
    start: CpsFunId,
    target: CpsFunId,
) -> bool {
    let mut visited = BTreeSet::new();
    let mut work = graph[&start].iter().copied().collect::<Vec<_>>();
    while let Some(function) = work.pop() {
        if function == target {
            return true;
        }
        if visited.insert(function)
            && let Some(next) = graph.get(&function)
        {
            work.extend(next.iter().copied());
        }
    }
    false
}

fn contify_self_tail_call(module: &mut CpsModule, callee: CpsFunId, call: CpsNodeId) {
    let function = module.function(callee).unwrap().clone();
    let CpsNode::ApplyFun {
        callee: CpsCallee::Known(found),
        args,
        return_to,
    } = module.node(call).unwrap().clone()
    else {
        unreachable!()
    };
    assert_eq!(found, callee);

    let loop_cont = module.reserve_continuation();
    let return_bridge = module.reserve_continuation();
    let return_value = module.add_value(Some("contified return".into()), false);
    let return_body = module.reserve_node();
    let loop_scope = module.reserve_node();
    for node_id in function_nodes(module, callee) {
        let node = module.nodes[node_id.index()].as_mut().unwrap();
        match node {
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(target),
                args,
                return_to: target_return,
            } if *target == callee => {
                debug_assert_eq!(*target_return, function.return_cont);
                *node = CpsNode::ApplyCont(CpsEdge {
                    target: loop_cont,
                    args: std::mem::take(args),
                });
            }
            CpsNode::ApplyFun {
                return_to: target, ..
            }
            | CpsNode::Foreign {
                return_to: target, ..
            }
            | CpsNode::Cell {
                return_to: target, ..
            }
            | CpsNode::Intrinsic {
                return_to: target, ..
            } if *target == function.return_cont => *target = return_to,
            CpsNode::ApplyCont(edge) if edge.target == function.return_cont => {
                edge.target = return_to;
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    if edge.target == function.return_cont {
                        edge.target = return_bridge;
                    }
                }
            }
            _ => {}
        }
    }

    let initial = module.reserve_node();
    module.nodes[initial.index()] = Some(CpsNode::ApplyCont(CpsEdge {
        target: loop_cont,
        args,
    }));
    module.nodes[return_body.index()] = Some(CpsNode::ApplyCont(CpsEdge {
        target: return_to,
        args: vec![CpsAtom::Value(return_value)],
    }));
    module.continuations[return_bridge.index()] = Some(CpsContinuation {
        debug_name: Some("contified return".into()),
        params: vec![return_value],
        body: return_body,
    });
    module.nodes[loop_scope.index()] = Some(CpsNode::LetCont {
        continuations: vec![return_bridge],
        body: function.body,
    });
    module.continuations[loop_cont.index()] = Some(CpsContinuation {
        debug_name: function.debug_name,
        params: function.params,
        body: loop_scope,
    });
    module.nodes[call.index()] = Some(CpsNode::LetCont {
        continuations: vec![loop_cont],
        body: initial,
    });
    module.functions[callee.index()] = None;
    for node in module.nodes.iter_mut().flatten() {
        match node {
            CpsNode::LetFun { functions, .. } | CpsNode::RecInit { functions, .. } => {
                functions.retain(|function| *function != callee);
            }
            _ => {}
        }
    }
    module.rebuild_uses();
}

fn owned_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let mut owned = module
        .function(function)
        .unwrap()
        .params
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    for node_id in function_nodes(module, function) {
        match module.node(node_id).unwrap() {
            CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } => {
                owned.insert(*result);
            }
            CpsNode::LetCont { continuations, .. } => {
                for continuation in continuations {
                    owned.extend(
                        module
                            .continuation(*continuation)
                            .unwrap()
                            .params
                            .iter()
                            .copied(),
                    );
                }
            }
            _ => {}
        }
    }
    owned
}

fn free_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let owned = owned_values(module, function);
    let mut used = BTreeSet::new();
    for node_id in function_nodes(module, function) {
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if let CpsAtom::Value(value) = atom {
                used.insert(*value);
            }
        }
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Closure(value),
            ..
        } = node
        {
            used.insert(*value);
        }
    }
    used.difference(&owned).copied().collect()
}

fn available_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let mut available = owned_values(module, function);
    available.extend(free_values(module, function));
    available
}

fn inline_call(
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
    for node in nodes.values() {
        if let CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } = node {
            let definition = module.values[result.index()].as_ref().unwrap().clone();
            let fresh = module.add_value(definition.debug_name, definition.candidate);
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
            let fresh = module.add_value(definition.debug_name, definition.candidate);
            values.insert(param, CpsAtom::Value(fresh));
        }
    }

    if nodes.values().any(|node| {
        matches!(node, CpsNode::ApplyFun { callee: CpsCallee::Closure(value), .. }
            if matches!(values.get(value), Some(CpsAtom::Literal(_))))
    }) {
        return false;
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
    module.rebuild_uses();
    true
}

fn prune_unreachable(module: &mut CpsModule) -> bool {
    let Some(entry) = module.entry else {
        return false;
    };
    let mut functions = BTreeSet::new();
    let mut continuations = BTreeSet::new();
    let mut nodes = BTreeSet::new();
    let mut function_work = VecDeque::from([entry]);
    let mut node_work = VecDeque::new();

    while !function_work.is_empty() || !node_work.is_empty() {
        while let Some(function) = function_work.pop_front() {
            if functions.insert(function) {
                node_work.push_back(module.function(function).unwrap().body);
            }
        }
        let Some(node_id) = node_work.pop_front() else {
            continue;
        };
        if !nodes.insert(node_id) {
            continue;
        }
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom {
                function_work.push_back(*function);
            }
        }
        let mut queue_cont = |target: CpsContId| {
            if let Some(continuation) = module.continuation(target)
                && continuations.insert(target)
            {
                node_work.push_back(continuation.body);
            }
        };
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => {
                node_work.push_back(*next)
            }
            CpsNode::LetFun { body, .. } => node_work.push_back(*body),
            CpsNode::LetCont { body, .. } => node_work.push_back(*body),
            CpsNode::RecInit {
                functions, body, ..
            } => {
                function_work.extend(functions.iter().copied());
                node_work.push_back(*body);
            }
            CpsNode::ApplyFun {
                callee, return_to, ..
            } => {
                if let CpsCallee::Known(function) = callee {
                    function_work.push_back(*function);
                }
                queue_cont(*return_to);
            }
            CpsNode::ApplyCont(edge) => queue_cont(edge.target),
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    queue_cont(edge.target);
                }
            }
            CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => queue_cont(*return_to),
            CpsNode::Exit { .. } | CpsNode::Unreachable => {}
        }
    }

    let old = (
        module.functions.iter().flatten().count(),
        module.continuations.iter().flatten().count(),
        module.nodes.iter().flatten().count(),
        module.values.iter().flatten().count(),
    );
    for (index, function) in module.functions.iter_mut().enumerate() {
        if !functions.contains(&CpsFunId(index as u32)) {
            *function = None;
        }
    }
    for (index, continuation) in module.continuations.iter_mut().enumerate() {
        if !continuations.contains(&CpsContId(index as u32)) {
            *continuation = None;
        }
    }
    for (index, node) in module.nodes.iter_mut().enumerate() {
        if !nodes.contains(&CpsNodeId(index as u32)) {
            *node = None;
            continue;
        }
        match node.as_mut().unwrap() {
            CpsNode::LetFun {
                functions: members, ..
            }
            | CpsNode::RecInit {
                functions: members, ..
            } => members.retain(|function| functions.contains(function)),
            CpsNode::LetCont {
                continuations: members,
                ..
            } => members.retain(|continuation| continuations.contains(continuation)),
            _ => {}
        }
    }
    let mut values = BTreeSet::new();
    for function in module.functions.iter().flatten() {
        values.extend(function.params.iter().copied());
    }
    for continuation in module.continuations.iter().flatten() {
        values.extend(continuation.params.iter().copied());
    }
    for node in module.nodes.iter().flatten() {
        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } => {
                values.insert(*result);
            }
            CpsNode::RecInit {
                values: recursive_values,
                ..
            } => values.extend(recursive_values.iter().copied()),
            _ => {}
        }
    }
    for (index, value) in module.values.iter_mut().enumerate() {
        if !values.contains(&CpsValueId(index as u32)) {
            *value = None;
        }
    }
    module.rebuild_uses();
    old != (
        module.functions.iter().flatten().count(),
        module.continuations.iter().flatten().count(),
        module.nodes.iter().flatten().count(),
        module.values.iter().flatten().count(),
    )
}

fn known_values(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut known = BTreeMap::new();

    for node in module.nodes.iter().flatten() {
        if let CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(literal),
            ..
        } = node
        {
            known.insert(*result, CpsAtom::Literal(literal.clone()));
        }
    }

    let recursive_functions = analyze_calls(module).recursive;

    let mut function_inputs = BTreeMap::<CpsFunId, Vec<Knowledge>>::new();
    for (index, function) in module.functions.iter().enumerate() {
        if let Some(function) = function {
            function_inputs.insert(
                CpsFunId(index as u32),
                vec![Knowledge::Unknown; function.params.len()],
            );
        }
    }

    for node in module.nodes.iter().flatten() {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(function),
            args,
            ..
        } = node
            && let Some(inputs) = function_inputs.get_mut(function)
        {
            merge_inputs(inputs, Some(args));
        }

        for atom in atoms(node) {
            if let CpsAtom::Fun(function) = atom
                && let Some(inputs) = function_inputs.get_mut(function)
            {
                merge_inputs(inputs, None);
            }
        }
    }

    for (function, inputs) in function_inputs {
        if recursive_functions.contains(&function) {
            continue;
        }
        let params = &module.function(function).unwrap().params;
        record_known_literals(params, &inputs, &mut known);
    }

    // Collapse deterministic substitution chains once at the phase boundary.
    let keys = known.keys().copied().collect::<Vec<_>>();
    for key in keys {
        let mut value = known[&key].clone();
        let mut seen = BTreeSet::new();
        while let CpsAtom::Value(next) = value {
            if !seen.insert(next) {
                break;
            }
            let Some(replacement) = known.get(&next) else {
                break;
            };
            value = replacement.clone();
        }
        known.insert(key, value);
    }
    known
}

fn merge_inputs(inputs: &mut [Knowledge], args: Option<&[CpsAtom]>) {
    for (index, input) in inputs.iter_mut().enumerate() {
        input.merge(args.and_then(|args| args.get(index)));
    }
}

fn record_known_literals(
    params: &[CpsValueId],
    inputs: &[Knowledge],
    known: &mut BTreeMap<CpsValueId, CpsAtom>,
) {
    for (&param, input) in params.iter().zip(inputs) {
        if let Knowledge::Known(CpsAtom::Literal(literal)) = input {
            known.insert(param, CpsAtom::Literal(literal.clone()));
        }
    }
}

fn rewrite_atoms(module: &mut CpsModule, known: &BTreeMap<CpsValueId, CpsAtom>) -> bool {
    let mut changed = false;
    for node in module.nodes.iter_mut().flatten() {
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Value(value) = atom
                && let Some(replacement) = known.get(value)
                && atom != replacement
            {
                *atom = replacement.clone();
                changed = true;
            }
        });

        if let CpsNode::ApplyFun { callee, .. } = node
            && let CpsCallee::Closure(value) = *callee
            && let Some(CpsAtom::Fun(function)) = known.get(&value)
        {
            *callee = CpsCallee::Known(*function);
            changed = true;
        }
    }
    if changed {
        module.rebuild_uses();
    }
    changed
}

fn forward_continuations(module: &mut CpsModule) -> bool {
    let forwarding = module
        .continuations
        .iter()
        .enumerate()
        .filter_map(|(index, continuation)| {
            let continuation = continuation.as_ref()?;
            let CpsNode::ApplyCont(edge) = module.node(continuation.body)? else {
                return None;
            };
            if module.continuation(edge.target).is_none() {
                return None;
            }
            Some((
                CpsContId(index as u32),
                (continuation.params.clone(), edge.clone()),
            ))
        })
        .collect::<BTreeMap<_, _>>();
    if forwarding.is_empty() {
        return false;
    }

    let identity = forwarding
        .iter()
        .filter_map(|(&continuation, (params, edge))| {
            (params.len() == edge.args.len()
                && params
                    .iter()
                    .zip(&edge.args)
                    .all(|(param, arg)| arg == &CpsAtom::Value(*param)))
            .then_some((continuation, edge.target))
        })
        .collect::<BTreeMap<_, _>>();
    let resolve_identity = |original: CpsContId| {
        let mut target = original;
        let mut seen = BTreeSet::new();
        loop {
            if !seen.insert(target) {
                return original;
            }
            let Some(next) = identity.get(&target) else {
                break;
            };
            target = *next;
        }
        target
    };

    let mut changed = false;
    for node in module.nodes.iter_mut().flatten() {
        match node {
            CpsNode::ApplyCont(edge) => {
                thread_edge(edge, &forwarding, &mut changed);
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    thread_edge(edge, &forwarding, &mut changed);
                }
            }
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                retarget(return_to, &resolve_identity, &mut changed);
            }
            _ => {}
        }
    }
    if changed {
        module.rebuild_uses();
    }
    changed
}

fn thread_edge(
    edge: &mut CpsEdge,
    forwarding: &BTreeMap<CpsContId, (Vec<CpsValueId>, CpsEdge)>,
    changed: &mut bool,
) {
    let original = edge.clone();
    let mut replacement = original.clone();
    let mut seen = BTreeSet::new();
    loop {
        if !seen.insert(replacement.target) {
            return;
        }
        let Some((params, outgoing)) = forwarding.get(&replacement.target) else {
            break;
        };
        if params.len() != replacement.args.len() {
            return;
        }
        let substitutions = params
            .iter()
            .copied()
            .zip(replacement.args.iter().cloned())
            .collect::<BTreeMap<_, _>>();
        replacement = CpsEdge {
            target: outgoing.target,
            args: outgoing
                .args
                .iter()
                .map(|arg| match arg {
                    CpsAtom::Value(value) => substitutions
                        .get(value)
                        .cloned()
                        .unwrap_or_else(|| arg.clone()),
                    _ => arg.clone(),
                })
                .collect(),
        };
    }
    if replacement.target != original.target || replacement.args != original.args {
        *edge = replacement;
        *changed = true;
    }
}

fn retarget(target: &mut CpsContId, resolve: &impl Fn(CpsContId) -> CpsContId, changed: &mut bool) {
    let replacement = resolve(*target);
    if replacement != *target {
        *target = replacement;
        *changed = true;
    }
}

fn simplify_nodes(module: &mut CpsModule) -> bool {
    let mut changed = false;
    for slot in &mut module.nodes {
        let Some(node) = slot else { continue };
        match node {
            CpsNode::LetPrim {
                result,
                op,
                args,
                next,
            } => {
                if let Some(literal) = evaluate(*op, args) {
                    *node = CpsNode::LetValue {
                        result: *result,
                        value: CpsValueExpr::Literal(literal),
                        next: *next,
                    };
                    changed = true;
                }
            }
            CpsNode::Switch {
                scrutinee: CpsAtom::Literal(CpsLiteral::Nat(tag)),
                cases,
                default,
            } => {
                if let Some(edge) = cases.get(tag).or(default.as_ref()).cloned() {
                    *node = CpsNode::ApplyCont(edge);
                    changed = true;
                }
            }
            _ => {}
        }
    }
    if changed {
        module.rebuild_uses();
    }
    changed
}

fn forward_aggregate_projections(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let aggregates = module
            .nodes
            .iter()
            .flatten()
            .filter_map(|node| match node {
                CpsNode::LetValue {
                    result,
                    value: CpsValueExpr::Tuple(fields),
                    ..
                } => Some((*result, fields.clone())),
                _ => None,
            })
            .collect::<BTreeMap<_, _>>();
        let selected = module.nodes.iter().enumerate().find_map(|(index, node)| {
            let CpsNode::LetPrim {
                result,
                op: CpsPrimOp::TplGet(field),
                args,
                next,
            } = node.as_ref()?
            else {
                return None;
            };
            let [CpsAtom::Value(tuple)] = args.as_slice() else {
                return None;
            };
            let replacement = aggregates.get(tuple)?.get(*field)?.clone();
            Some((CpsNodeId(index as u32), *result, *next, replacement))
        });
        let Some((node, result, next, replacement)) = selected else {
            break;
        };

        rewrite_atoms(module, &BTreeMap::from([(result, replacement)]));
        rewire_node(module, node, next);
        module.nodes[node.index()] = None;
        module.values[result.index()] = None;
        module.rebuild_uses();
        changed = true;
    }
    changed
}

fn eliminate_dead_bindings(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let selected = module.nodes.iter().enumerate().find_map(|(index, node)| {
            let id = CpsNodeId(index as u32);
            match node.as_ref()? {
                CpsNode::LetValue { result, next, .. }
                    if module.uses_of(CpsUseTarget::Value(*result)).count() == 0 =>
                {
                    Some((id, *next, Some(*result)))
                }
                CpsNode::LetPrim {
                    result, op, next, ..
                } if op.is_total() && module.uses_of(CpsUseTarget::Value(*result)).count() == 0 => {
                    Some((id, *next, Some(*result)))
                }
                CpsNode::LetFun { functions, body } if functions.is_empty() => {
                    Some((id, *body, None))
                }
                CpsNode::LetCont {
                    continuations,
                    body,
                } if continuations.is_empty() => Some((id, *body, None)),
                _ => None,
            }
        });
        let Some((node, replacement, value)) = selected else {
            break;
        };
        rewire_node(module, node, replacement);
        module.nodes[node.index()] = None;
        if let Some(value) = value {
            module.values[value.index()] = None;
        }
        module.rebuild_uses();
        changed = true;
    }
    changed
}

fn rewire_node(module: &mut CpsModule, from: CpsNodeId, to: CpsNodeId) {
    for function in module.functions.iter_mut().flatten() {
        if function.body == from {
            function.body = to;
        }
    }
    for continuation in module.continuations.iter_mut().flatten() {
        if continuation.body == from {
            continuation.body = to;
        }
    }
    for node in module.nodes.iter_mut().flatten() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => {
                if *next == from {
                    *next = to;
                }
            }
            CpsNode::LetFun { body, .. } | CpsNode::LetCont { body, .. } => {
                if *body == from {
                    *body = to;
                }
            }
            CpsNode::RecInit { ready, body, .. } => {
                if *ready == from {
                    *ready = to;
                }
                if *body == from {
                    *body = to;
                }
            }
            CpsNode::ApplyFun { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Foreign { .. }
            | CpsNode::Cell { .. }
            | CpsNode::Intrinsic { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Unreachable => {}
        }
    }
}

fn eliminate_dead_parameters(module: &mut CpsModule) -> bool {
    let mut continuation = None;
    for (index, definition) in module.continuations.iter().enumerate() {
        let Some(definition) = definition else {
            continue;
        };
        let id = CpsContId(index as u32);
        if module.nodes.iter().flatten().any(|node| {
            matches!(
                node,
                CpsNode::ApplyFun { return_to, .. }
                    | CpsNode::Foreign { return_to, .. }
                    | CpsNode::Cell { return_to, .. }
                    | CpsNode::Intrinsic { return_to, .. }
                    if *return_to == id
            )
        }) {
            continue;
        }
        let dead = definition
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| {
                (module.uses_of(CpsUseTarget::Value(*value)).count() == 0).then_some(index)
            })
            .collect::<BTreeSet<_>>();
        if !dead.is_empty() {
            continuation = Some((id, dead));
            break;
        }
    }
    if let Some((continuation, dead)) = continuation {
        let removed = remove_parameter_indices(
            &mut module.continuations[continuation.index()]
                .as_mut()
                .unwrap()
                .params,
            &dead,
        );
        for node in module.nodes.iter_mut().flatten() {
            match node {
                CpsNode::ApplyCont(edge) if edge.target == continuation => {
                    remove_parameter_indices(&mut edge.args, &dead);
                }
                CpsNode::Switch { cases, default, .. } => {
                    for edge in cases.values_mut().chain(default.iter_mut()) {
                        if edge.target == continuation {
                            remove_parameter_indices(&mut edge.args, &dead);
                        }
                    }
                }
                _ => {}
            }
        }
        for value in removed {
            module.values[value.index()] = None;
        }
        module.rebuild_uses();
        return true;
    }

    let escaping = module
        .nodes
        .iter()
        .flatten()
        .flat_map(atoms)
        .filter_map(|atom| match atom {
            CpsAtom::Fun(function) => Some(*function),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let mut function = None;
    for (index, definition) in module.functions.iter().enumerate() {
        let Some(definition) = definition else {
            continue;
        };
        let id = CpsFunId(index as u32);
        if escaping.contains(&id) {
            continue;
        }
        let dead = definition
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| {
                (module.uses_of(CpsUseTarget::Value(*value)).count() == 0).then_some(index)
            })
            .collect::<BTreeSet<_>>();
        if !dead.is_empty() {
            function = Some((id, dead));
            break;
        }
    }
    let Some((function, dead)) = function else {
        return false;
    };
    let removed = remove_parameter_indices(
        &mut module.functions[function.index()].as_mut().unwrap().params,
        &dead,
    );
    for node in module.nodes.iter_mut().flatten() {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            ..
        } = node
            && *callee == function
        {
            remove_parameter_indices(args, &dead);
        }
    }
    for value in removed {
        module.values[value.index()] = None;
    }
    module.rebuild_uses();
    true
}

fn remove_parameter_indices<T>(values: &mut Vec<T>, removed: &BTreeSet<usize>) -> Vec<T> {
    let mut index = 0;
    let mut removed_values = Vec::new();
    let mut retained = Vec::with_capacity(values.len() - removed.len());
    for value in std::mem::take(values) {
        if removed.contains(&index) {
            removed_values.push(value);
        } else {
            retained.push(value);
        }
        index += 1;
    }
    *values = retained;
    removed_values
}

fn evaluate(op: CpsPrimOp, args: &[CpsAtom]) -> Option<CpsLiteral> {
    let literals = args
        .iter()
        .map(|atom| match atom {
            CpsAtom::Literal(literal) => Some(literal),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let nat = |index: usize| match literals[index] {
        CpsLiteral::Nat(value) => Some(*value),
        _ => None,
    };
    let int = |index: usize| match literals[index] {
        CpsLiteral::Int(value) => Some(*value),
        _ => None,
    };
    let flt = |index: usize| match literals[index] {
        CpsLiteral::Flt(value) => Some(*value),
        _ => None,
    };
    let bln = |value: bool| Some(CpsLiteral::Nat(value as u32));
    let nat31 = |value: u64| (value < (1 << 31)).then_some(CpsLiteral::Nat(value as u32));
    let int31 = |value: i64| {
        ((-(1 << 30))..(1 << 30))
            .contains(&value)
            .then_some(CpsLiteral::Int(value as i32))
    };
    let wrap31s = |value: i32| value.wrapping_shl(1) >> 1;

    match op {
        CpsPrimOp::NatEql => bln(nat(0)? == nat(1)?),
        CpsPrimOp::NatNeq => bln(nat(0)? != nat(1)?),
        CpsPrimOp::NatAdd => nat31(nat(0)? as u64 + nat(1)? as u64),
        CpsPrimOp::NatSub => Some(CpsLiteral::Nat(nat(0)?.saturating_sub(nat(1)?))),
        CpsPrimOp::NatMul => nat31(nat(0)? as u64 * nat(1)? as u64),
        CpsPrimOp::NatLt => bln(nat(0)? < nat(1)?),
        CpsPrimOp::NatDiv => Some(CpsLiteral::Nat(
            nat(0)? / (nat(1)? != 0).then_some(nat(1)?)?,
        )),
        CpsPrimOp::NatRem => Some(CpsLiteral::Nat(
            nat(0)? % (nat(1)? != 0).then_some(nat(1)?)?,
        )),
        CpsPrimOp::NatGt => bln(nat(0)? > nat(1)?),
        CpsPrimOp::NatLte => bln(nat(0)? <= nat(1)?),
        CpsPrimOp::NatGte => bln(nat(0)? >= nat(1)?),
        CpsPrimOp::NatAnd => Some(CpsLiteral::Nat(nat(0)? & nat(1)?)),
        CpsPrimOp::NatOr => Some(CpsLiteral::Nat(nat(0)? | nat(1)?)),
        CpsPrimOp::NatXor => Some(CpsLiteral::Nat(nat(0)? ^ nat(1)?)),
        CpsPrimOp::NatShl => Some(CpsLiteral::Nat(nat(0)?.wrapping_shl(nat(1)?) & 0x7fff_ffff)),
        CpsPrimOp::NatShr => Some(CpsLiteral::Nat(nat(0)?.wrapping_shr(nat(1)?))),
        CpsPrimOp::NatRotl => nat31(nat(0)?.rotate_left(nat(1)?) as u64),
        CpsPrimOp::NatRotr => nat31(nat(0)?.rotate_right(nat(1)?) as u64),
        CpsPrimOp::NatClz => Some(CpsLiteral::Nat(nat(0)?.leading_zeros())),
        CpsPrimOp::NatCtz => Some(CpsLiteral::Nat(nat(0)?.trailing_zeros())),
        CpsPrimOp::NatPopcnt => Some(CpsLiteral::Nat(nat(0)?.count_ones())),
        CpsPrimOp::NatEqz => bln(nat(0)? == 0),
        CpsPrimOp::NatToInt => Some(CpsLiteral::Int(wrap31s(nat(0)? as i32))),
        CpsPrimOp::NatToFlt => Some(CpsLiteral::Flt(nat(0)? as f32)),
        CpsPrimOp::IntEql => bln(int(0)? == int(1)?),
        CpsPrimOp::IntNeq => bln(int(0)? != int(1)?),
        CpsPrimOp::IntAdd => int31(int(0)? as i64 + int(1)? as i64),
        CpsPrimOp::IntSub => int31(int(0)? as i64 - int(1)? as i64),
        CpsPrimOp::IntMul => int31(int(0)? as i64 * int(1)? as i64),
        CpsPrimOp::IntDiv => int31(int(0)? as i64 / (int(1)? != 0).then_some(int(1)?)? as i64),
        CpsPrimOp::IntRem => Some(CpsLiteral::Int(
            int(0)? % (int(1)? != 0).then_some(int(1)?)?,
        )),
        CpsPrimOp::IntLt => bln(int(0)? < int(1)?),
        CpsPrimOp::IntGt => bln(int(0)? > int(1)?),
        CpsPrimOp::IntLte => bln(int(0)? <= int(1)?),
        CpsPrimOp::IntGte => bln(int(0)? >= int(1)?),
        CpsPrimOp::IntAnd => Some(CpsLiteral::Int(wrap31s(int(0)? & int(1)?))),
        CpsPrimOp::IntOr => Some(CpsLiteral::Int(wrap31s(int(0)? | int(1)?))),
        CpsPrimOp::IntXor => Some(CpsLiteral::Int(wrap31s(int(0)? ^ int(1)?))),
        CpsPrimOp::IntShl => Some(CpsLiteral::Int(wrap31s(
            int(0)?.wrapping_shl(int(1)? as u32),
        ))),
        CpsPrimOp::IntShr => Some(CpsLiteral::Int(wrap31s(
            int(0)?.wrapping_shr(int(1)? as u32),
        ))),
        CpsPrimOp::IntRotl => int31((int(0)? as u32).rotate_left(int(1)? as u32) as i32 as i64),
        CpsPrimOp::IntRotr => int31((int(0)? as u32).rotate_right(int(1)? as u32) as i32 as i64),
        CpsPrimOp::IntClz => Some(CpsLiteral::Int((int(0)? as u32).leading_zeros() as i32)),
        CpsPrimOp::IntCtz => Some(CpsLiteral::Int((int(0)? as u32).trailing_zeros() as i32)),
        CpsPrimOp::IntPopcnt => Some(CpsLiteral::Int((int(0)? as u32).count_ones() as i32)),
        CpsPrimOp::IntEqz => bln(int(0)? == 0),
        CpsPrimOp::IntToNat => Some(CpsLiteral::Nat(int(0)? as u32 & 0x7fff_ffff)),
        CpsPrimOp::IntToFlt => Some(CpsLiteral::Flt(int(0)? as f32)),
        CpsPrimOp::FltAdd => Some(CpsLiteral::Flt(flt(0)? + flt(1)?)),
        CpsPrimOp::FltSub => Some(CpsLiteral::Flt(flt(0)? - flt(1)?)),
        CpsPrimOp::FltMul => Some(CpsLiteral::Flt(flt(0)? * flt(1)?)),
        CpsPrimOp::FltDiv => Some(CpsLiteral::Flt(flt(0)? / flt(1)?)),
        CpsPrimOp::FltRem => Some(CpsLiteral::Flt(flt(0)? % flt(1)?)),
        CpsPrimOp::FltEql => bln(flt(0)? == flt(1)?),
        CpsPrimOp::FltNeq => bln(flt(0)? != flt(1)?),
        CpsPrimOp::FltLt => bln(flt(0)? < flt(1)?),
        CpsPrimOp::FltGt => bln(flt(0)? > flt(1)?),
        CpsPrimOp::FltLte => bln(flt(0)? <= flt(1)?),
        CpsPrimOp::FltGte => bln(flt(0)? >= flt(1)?),
        CpsPrimOp::FltMin if !flt(0)?.is_nan() && !flt(1)?.is_nan() => {
            Some(CpsLiteral::Flt(flt(0)?.min(flt(1)?)))
        }
        CpsPrimOp::FltMax if !flt(0)?.is_nan() && !flt(1)?.is_nan() => {
            Some(CpsLiteral::Flt(flt(0)?.max(flt(1)?)))
        }
        CpsPrimOp::FltNeg => Some(CpsLiteral::Flt(-flt(0)?)),
        CpsPrimOp::FltAbs => Some(CpsLiteral::Flt(flt(0)?.abs())),
        CpsPrimOp::FltSqrt => Some(CpsLiteral::Flt(flt(0)?.sqrt())),
        CpsPrimOp::FltFloor => Some(CpsLiteral::Flt(flt(0)?.floor())),
        CpsPrimOp::FltCeil => Some(CpsLiteral::Flt(flt(0)?.ceil())),
        CpsPrimOp::FltTrunc => Some(CpsLiteral::Flt(flt(0)?.trunc())),
        CpsPrimOp::FltNearest => Some(CpsLiteral::Flt(flt(0)?.round_ties_even())),
        CpsPrimOp::FltCopysign => Some(CpsLiteral::Flt(flt(0)?.copysign(flt(1)?))),
        CpsPrimOp::FltToNat => {
            let value = flt(0)?;
            let truncated = value.trunc();
            (value.is_finite() && truncated > -1.0 && truncated < 2_147_483_648.0)
                .then_some(CpsLiteral::Nat(truncated as u32))
        }
        CpsPrimOp::FltToInt => {
            let value = flt(0)?;
            let truncated = value.trunc();
            (value.is_finite() && (-1_073_741_824.0..1_073_741_824.0).contains(&truncated))
                .then_some(CpsLiteral::Int(truncated as i32))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{
            atoms, eliminate_dead_bindings, eliminate_dead_parameters, evaluate,
            forward_continuations, inline_known_calls, inline_single_use_continuations,
            known_values, optimize,
        },
        crate::{
            CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsLiteral, CpsModule,
            CpsNode, CpsPrimOp,
        },
        std::collections::BTreeMap,
    };

    #[test]
    fn preserves_traps_and_folds_wasm_faithful_nat_add() {
        assert_eq!(
            evaluate(
                CpsPrimOp::NatAdd,
                &[
                    CpsAtom::Literal(CpsLiteral::Nat(20)),
                    CpsAtom::Literal(CpsLiteral::Nat(22)),
                ],
            ),
            Some(CpsLiteral::Nat(42))
        );
        assert_eq!(
            evaluate(
                CpsPrimOp::NatAdd,
                &[
                    CpsAtom::Literal(CpsLiteral::Nat(0x7fff_ffff)),
                    CpsAtom::Literal(CpsLiteral::Nat(1)),
                ],
            ),
            None
        );
        assert_eq!(
            evaluate(
                CpsPrimOp::NatDiv,
                &[
                    CpsAtom::Literal(CpsLiteral::Nat(1)),
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                ],
            ),
            None
        );
    }

    #[test]
    fn dead_binding_elimination_preserves_traps_and_drops_total_literals() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let return_cont = module.reserve_continuation();
        let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        let dead_total = module.add_value(Some("dead total".into()), false);
        let total_node = module.add_node(CpsNode::LetPrim {
            result: dead_total,
            op: CpsPrimOp::NatEql,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ],
            next: return_node,
        });
        let dead_trap = module.add_value(Some("dead trap".into()), false);
        let trap_node = module.add_node(CpsNode::LetPrim {
            result: dead_trap,
            op: CpsPrimOp::NatDiv,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(0)),
            ],
            next: total_node,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body: trap_node,
            },
        );
        module.set_entry(entry);

        assert!(eliminate_dead_bindings(&mut module));
        assert!(module.node(total_node).is_none());
        assert!(matches!(
            module.node(trap_node),
            Some(CpsNode::LetPrim {
                op: CpsPrimOp::NatDiv,
                next,
                ..
            }) if *next == return_node
        ));
        module.verify().unwrap();
    }

    #[test]
    fn dead_parameter_elimination_rewrites_known_calls() {
        let mut module = CpsModule::new();
        let main = module.reserve_function(Some("main".into()));
        let callee = module.reserve_function(Some("callee".into()));
        let kept = module.add_value(Some("kept".into()), false);
        let removed = module.add_value(Some("removed".into()), false);
        let callee_return = module.reserve_continuation();
        let callee_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: callee_return,
            args: vec![CpsAtom::Value(kept)],
        }));
        module.define_function(
            callee,
            CpsFunction {
                debug_name: Some("callee".into()),
                params: vec![kept, removed],
                return_cont: callee_return,
                body: callee_body,
            },
        );
        let main_return = module.reserve_continuation();
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ],
            return_to: main_return,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![callee],
            body: call,
        });
        module.define_function(
            main,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: main_return,
                body,
            },
        );
        module.set_entry(main);

        assert!(eliminate_dead_parameters(&mut module));
        assert_eq!(module.function(callee).unwrap().params, vec![kept]);
        assert!(matches!(
            module.node(call),
            Some(CpsNode::ApplyFun { args, .. })
                if args == &[CpsAtom::Literal(CpsLiteral::Nat(1))]
        ));
        module.verify().unwrap();
    }

    #[test]
    fn known_continuation_values_are_not_substituted_across_scopes() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let return_cont = module.reserve_continuation();
        let seed = module.add_value(Some("seed".into()), false);
        let forwarding = module.reserve_continuation();
        let forwarded = module.add_value(Some("forwarded".into()), false);
        let target = module.reserve_continuation();
        let target_param = module.add_value(Some("target".into()), false);
        let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(target_param)],
        }));
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: Some("target".into()),
                params: vec![target_param],
                body: target_body,
            },
        );
        let forwarding_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target,
            args: vec![CpsAtom::Value(forwarded)],
        }));
        module.define_continuation(
            forwarding,
            CpsContinuation {
                debug_name: Some("forwarding".into()),
                params: vec![forwarded],
                body: forwarding_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: forwarding,
            args: vec![CpsAtom::Value(seed)],
        }));
        let body = module.add_node(CpsNode::LetCont {
            continuations: vec![forwarding, target],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![seed],
                return_cont,
                body,
            },
        );
        module.set_entry(entry);

        optimize(&mut module);

        assert!(
            module
                .nodes()
                .iter()
                .flatten()
                .flat_map(atoms)
                .all(|atom| atom != &CpsAtom::Value(forwarded))
        );
        module.verify().unwrap();
    }

    #[test]
    fn known_value_analysis_leaves_local_continuation_parameters_to_beta_reduction() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(None);
        let return_cont = module.reserve_continuation();
        let continuation = module.reserve_continuation();
        let parameter = module.add_value(None, false);
        let continuation_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(parameter)],
        }));
        module.define_continuation(
            continuation,
            CpsContinuation {
                debug_name: None,
                params: vec![parameter],
                body: continuation_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: continuation,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
        }));
        let body = module.add_node(CpsNode::LetCont {
            continuations: vec![continuation],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: None,
                params: vec![],
                return_cont,
                body,
            },
        );
        module.set_entry(entry);
        module.verify().unwrap();

        assert!(!known_values(&module).contains_key(&parameter));
        assert!(inline_single_use_continuations(&mut module));
        assert!(matches!(
            module.node(call),
            Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
                if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
        ));
    }

    #[test]
    fn forwarding_composes_jump_arguments_instead_of_only_retargeting() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let return_cont = module.reserve_continuation();
        let target = module.reserve_continuation();
        let target_left = module.add_value(Some("target left".into()), false);
        let target_right = module.add_value(Some("target right".into()), false);
        let target_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(target_right)],
        }));
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: Some("target".into()),
                params: vec![target_left, target_right],
                body: target_body,
            },
        );
        let forwarding = module.reserve_continuation();
        let forwarded = module.add_value(Some("forwarded".into()), false);
        let forwarding_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target,
            args: vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Value(forwarded),
            ],
        }));
        module.define_continuation(
            forwarding,
            CpsContinuation {
                debug_name: Some("forwarding".into()),
                params: vec![forwarded],
                body: forwarding_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: forwarding,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
        }));
        let body = module.add_node(CpsNode::LetCont {
            continuations: vec![forwarding, target],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        module.set_entry(entry);

        assert!(forward_continuations(&mut module));
        assert!(matches!(
            module.node(call),
            Some(CpsNode::ApplyCont(CpsEdge { target: actual, args }))
                if *actual == target
                    && args == &[
                        CpsAtom::Literal(CpsLiteral::Nat(1)),
                        CpsAtom::Literal(CpsLiteral::Nat(7)),
                    ]
        ));
        module.verify().unwrap();
    }

    #[test]
    fn continuation_beta_rewrites_parameters_captured_by_nested_functions() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let return_cont = module.reserve_continuation();
        let continuation = module.reserve_continuation();
        let captured = module.add_value(Some("captured".into()), false);

        let nested = module.reserve_function(Some("nested".into()));
        let nested_return = module.reserve_continuation();
        let nested_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: nested_return,
            args: vec![CpsAtom::Value(captured)],
        }));
        module.define_function(
            nested,
            CpsFunction {
                debug_name: Some("nested".into()),
                params: vec![],
                return_cont: nested_return,
                body: nested_body,
            },
        );
        let return_nested = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Fun(nested)],
        }));
        let continuation_body = module.add_node(CpsNode::LetFun {
            functions: vec![nested],
            body: return_nested,
        });
        module.define_continuation(
            continuation,
            CpsContinuation {
                debug_name: Some("capture scope".into()),
                params: vec![captured],
                body: continuation_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: continuation,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(7))],
        }));
        let body = module.add_node(CpsNode::LetCont {
            continuations: vec![continuation],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        module.set_entry(entry);

        assert!(inline_single_use_continuations(&mut module));
        assert!(matches!(
            module.node(nested_body),
            Some(CpsNode::ApplyCont(CpsEdge { args, .. }))
                if args == &[CpsAtom::Literal(CpsLiteral::Nat(7))]
        ));
        module.verify().unwrap();
    }

    #[test]
    fn known_call_inlining_clones_recursive_local_continuations() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(None);
        let entry_return = module.reserve_continuation();
        let callee = module.reserve_function(None);
        let callee_return = module.reserve_continuation();
        let callee_param = module.add_value(None, false);
        let local_cont = module.reserve_continuation();
        let local_param = module.add_value(None, false);
        let local_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: local_cont,
            args: vec![CpsAtom::Value(local_param)],
        }));
        module.define_continuation(
            local_cont,
            CpsContinuation {
                debug_name: None,
                params: vec![local_param],
                body: local_body,
            },
        );
        let enter_local = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: local_cont,
            args: vec![CpsAtom::Value(callee_param)],
        }));
        let callee_body = module.add_node(CpsNode::LetCont {
            continuations: vec![local_cont],
            body: enter_local,
        });
        module.define_function(
            callee,
            CpsFunction {
                debug_name: None,
                params: vec![callee_param],
                return_cont: callee_return,
                body: callee_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            return_to: entry_return,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![callee],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: None,
                params: vec![],
                return_cont: entry_return,
                body,
            },
        );
        module.set_entry(entry);
        module.verify().unwrap();

        assert!(inline_known_calls(&mut module));
        assert!(matches!(
            module.node(call),
            Some(CpsNode::LetCont { continuations, .. }) if continuations != &[local_cont]
        ));
        module.verify().unwrap();
    }

    #[test]
    fn contifies_a_single_entry_tail_loop_and_bridges_switch_returns() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();
        let loop_function = module.reserve_function(Some("loop".into()));
        let loop_return = module.reserve_continuation();
        let loop_param = module.add_value(Some("loop argument".into()), false);
        let recur = module.reserve_continuation();
        let recur_param = module.add_value(Some("recur argument".into()), false);
        let recur_body = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_function),
            args: vec![CpsAtom::Value(recur_param)],
            return_to: loop_return,
        });
        module.define_continuation(
            recur,
            CpsContinuation {
                debug_name: Some("recur".into()),
                params: vec![recur_param],
                body: recur_body,
            },
        );
        let switch = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(loop_param),
            cases: BTreeMap::from([(
                0,
                CpsEdge {
                    target: loop_return,
                    args: vec![CpsAtom::Value(loop_param)],
                },
            )]),
            default: Some(CpsEdge {
                target: recur,
                args: vec![CpsAtom::Value(loop_param)],
            }),
        });
        let loop_body = module.add_node(CpsNode::LetCont {
            continuations: vec![recur],
            body: switch,
        });
        module.define_function(
            loop_function,
            CpsFunction {
                debug_name: Some("loop".into()),
                params: vec![loop_param],
                return_cont: loop_return,
                body: loop_body,
            },
        );
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_function),
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
            return_to: entry_return,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![loop_function],
            body: call,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: entry_return,
                body,
            },
        );
        module.set_entry(entry);

        optimize(&mut module);

        assert!(module.function(loop_function).is_none());
        let loop_cont = module
            .continuations()
            .iter()
            .flatten()
            .find(|continuation| continuation.debug_name.as_deref() == Some("loop"))
            .unwrap();
        assert_eq!(loop_cont.params, vec![loop_param]);
        let return_bridge = module
            .continuations()
            .iter()
            .flatten()
            .find(|continuation| continuation.debug_name.as_deref() == Some("contified return"))
            .unwrap();
        assert!(matches!(
            module.node(return_bridge.body),
            Some(CpsNode::ApplyCont(CpsEdge { target, .. })) if *target == entry_return
        ));
        let CpsNode::Switch { cases, .. } = module.node(switch).unwrap() else {
            panic!("loop switch changed shape")
        };
        assert_ne!(cases[&0].target, entry_return);
        module.verify().unwrap();
    }
}
