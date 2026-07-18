//! Deterministic high-CPS canonicalization and propagation.

use {
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral,
        CpsModule, CpsNode, CpsNodeId, CpsPrimOp, CpsValueExpr, CpsValueId, atoms, visit_atoms_mut,
    },
    std::collections::{BTreeMap, BTreeSet, VecDeque},
};

pub(super) const MULTI_SITE_INLINE_LIMIT: usize = 8;
pub(super) const BRANCH_SPECIALIZATION_GROWTH_LIMIT: usize = 24;
pub(super) const PURE_EVALUATION_STEP_LIMIT: usize = 10_000;
pub(super) const PURE_EVALUATION_DEPTH_LIMIT: usize = 256;
pub(super) const SCC_CLONE_LIMIT: usize = 64;
pub(super) const SCC_CLONE_NODE_LIMIT: usize = 256;
pub(super) const BRANCH_CLONE_LIMIT: usize = 64;

#[derive(Clone, PartialEq)]
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

    /// Lattice join for the SCC-invariant fixpoint, ordered
    /// `Unknown < Known(_) < Conflict`. Unlike `merge`, `Unknown` is the
    /// identity (not-yet-resolved), so a forwarded parameter still resolving to
    /// `Unknown` contributes nothing rather than forcing a conflict.
    fn join(&mut self, incoming: Knowledge) {
        *self = match (std::mem::replace(self, Knowledge::Unknown), incoming) {
            (Knowledge::Conflict, _) | (_, Knowledge::Conflict) => Knowledge::Conflict,
            (Knowledge::Unknown, other) | (other, Knowledge::Unknown) => other,
            (Knowledge::Known(current), Knowledge::Known(incoming)) => {
                if current == incoming {
                    Knowledge::Known(current)
                } else {
                    Knowledge::Conflict
                }
            }
        }
    }
}

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are
/// rebuilt at deterministic boundaries instead of being kept as shadow state.
pub(crate) fn optimize(module: &mut CpsModule) {
    module
        .verify()
        .expect("invalid high CPS before optimization");

    // The pure-evaluation limits await their ported parity passes (residual
    // optimization parity, deferred to the baseline comparison). Referenced here
    // until those land so they are not dead code.
    let _pending_limits = (PURE_EVALUATION_STEP_LIMIT, PURE_EVALUATION_DEPTH_LIMIT);

    let mut scc_clone_budget = SCC_CLONE_LIMIT;
    let mut branch_clone_budget = BRANCH_CLONE_LIMIT;
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
            | contify_calls(module)
            | specialize_scc_calls(module, &mut scc_clone_budget)
            | specialize_call_patterns(module, &mut branch_clone_budget)
            | dissolve_rec_init(module)
            | prune_unreachable(module);
        if !changed {
            break;
        }
    }

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
    sccs: SccAnalysis,
}

/// Function strongly-connected components of the known-callee call graph,
/// computed at an explicit phase boundary. `SccId` is a dense index into
/// `members`; each component lists its functions in `CpsFunId` order.
type SccId = usize;

#[derive(Default)]
struct SccAnalysis {
    component_of: BTreeMap<CpsFunId, SccId>,
    members: Vec<Vec<CpsFunId>>,
}

/// Deterministic iterative Tarjan over the known-callee call graph. Uses an
/// explicit frame stack rather than recursion so it stays within the default
/// test-thread stack on deep call graphs. Components are numbered in the order
/// their roots pop, and members are sorted, so the output is a pure function of
/// the graph.
fn analyze_sccs(call_graph: &BTreeMap<CpsFunId, BTreeSet<CpsFunId>>) -> SccAnalysis {
    let mut analysis = SccAnalysis::default();
    let mut index_of: BTreeMap<CpsFunId, u32> = BTreeMap::new();
    let mut lowlink: BTreeMap<CpsFunId, u32> = BTreeMap::new();
    let mut on_stack: BTreeSet<CpsFunId> = BTreeSet::new();
    let mut stack: Vec<CpsFunId> = Vec::new();
    let mut next_index: u32 = 0;

    let successors = |function: CpsFunId| -> Vec<CpsFunId> {
        call_graph
            .get(&function)
            .map(|edges| edges.iter().copied().collect())
            .unwrap_or_default()
    };

    for &root in call_graph.keys() {
        if index_of.contains_key(&root) {
            continue;
        }
        index_of.insert(root, next_index);
        lowlink.insert(root, next_index);
        next_index += 1;
        stack.push(root);
        on_stack.insert(root);
        let mut work: Vec<(CpsFunId, Vec<CpsFunId>, usize)> = vec![(root, successors(root), 0)];

        while let Some(&(node, _, _)) = work.last() {
            let position = work.last().unwrap().2;
            if position < work.last().unwrap().1.len() {
                let successor = work.last().unwrap().1[position];
                work.last_mut().unwrap().2 += 1;
                let visited = index_of.contains_key(&successor);
                if !visited {
                    index_of.insert(successor, next_index);
                    lowlink.insert(successor, next_index);
                    next_index += 1;
                    stack.push(successor);
                    on_stack.insert(successor);
                    let edges = successors(successor);
                    work.push((successor, edges, 0));
                } else if on_stack.contains(&successor) {
                    let reached = index_of[&successor];
                    let link = lowlink.get_mut(&node).unwrap();
                    *link = (*link).min(reached);
                }
            } else {
                work.pop();
                if lowlink[&node] == index_of[&node] {
                    let mut component = Vec::new();
                    loop {
                        let popped = stack.pop().unwrap();
                        on_stack.remove(&popped);
                        component.push(popped);
                        if popped == node {
                            break;
                        }
                    }
                    component.sort();
                    let id = analysis.members.len();
                    for &function in &component {
                        analysis.component_of.insert(function, id);
                    }
                    analysis.members.push(component);
                }
                if let Some(&(parent, _, _)) = work.last() {
                    let child = lowlink[&node];
                    let link = lowlink.get_mut(&parent).unwrap();
                    *link = (*link).min(child);
                }
            }
        }
    }
    analysis
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

    // A function is recursive exactly when it lies on a call cycle: it is in a
    // multi-member SCC, or it is a singleton SCC with a self-edge. Deriving the
    // set from the SCC phase keeps one source of truth for cyclicity.
    let sccs = analyze_sccs(&analysis.call_graph);
    for (&function, &component) in &sccs.component_of {
        let multi_member = sccs.members[component].len() > 1;
        let self_edge = analysis
            .call_graph
            .get(&function)
            .is_some_and(|edges| edges.contains(&function));
        if multi_member || self_edge {
            analysis.recursive.insert(function);
        }
    }
    analysis.sccs = sccs;
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

/// Contify a non-escaping function whose calls resolve to a single return
/// context into a local continuation, covering both the single-entry recursive
/// loop and the non-recursive join-point cases.
///
/// A function qualifies when it has exactly one external call site: any call
/// from a third function would make `external` longer than one, so the only
/// admissible calls are that single entry plus the function's own tail-recursive
/// self-calls. This excludes mutual recursion and multi-return-context callers
/// without a separate check. Common-dominator placement for genuinely
/// multi-site contification is deferred to the machine-CFG analysis.
fn contify_calls(module: &mut CpsModule) -> bool {
    let analysis = analyze_calls(module);
    let mut selected = None;

    for (index, function) in module.functions.iter().enumerate() {
        let Some(function) = function else { continue };
        let callee = CpsFunId(index as u32);
        if Some(callee) == module.entry || analysis.escaping.contains(&callee) {
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
    contify_call(module, callee, call);
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

fn contify_call(module: &mut CpsModule, callee: CpsFunId, call: CpsNodeId) {
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
    true
}

/// Dissolve a `RecInit` knot into an ordinary `LetFun` once optimization has
/// severed the function-to-value dependency. `RecInit` additionally binds its
/// computed values so escaping closures may forward-reference them and emits a
/// fallback shell for each escaping member that captures one; when no member
/// still captures a computed value, that binding and those shells are
/// unnecessary and the node is an ordinary recursive function group. The
/// stronger "captures nothing computed" test (rather than merely "escapes
/// nothing") also keeps every computed value in lexical scope after the rewrite.
fn dissolve_rec_init(module: &mut CpsModule) -> bool {
    let mut selected = None;
    for (index, node) in module.nodes.iter().enumerate() {
        let Some(CpsNode::RecInit {
            functions,
            values,
            body,
            ..
        }) = node
        else {
            continue;
        };
        let computed: BTreeSet<CpsValueId> = values.iter().copied().collect();
        let captures = functions
            .iter()
            .any(|function| !free_values(module, *function).is_disjoint(&computed));
        if !captures {
            selected = Some((CpsNodeId(index as u32), functions.clone(), *body));
            break;
        }
    }
    let Some((node, functions, body)) = selected else {
        return false;
    };
    module.nodes[node.index()] = Some(CpsNode::LetFun { functions, body });
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

    let analysis = analyze_calls(module);
    let recursive_functions = &analysis.recursive;

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

    // Recursive members are skipped above because a self-forwarded argument
    // pollutes the flat per-call join. Recover their provably-invariant known
    // parameters with a dedicated SCC fixpoint and fold them in.
    let invariant = scc_invariant_knowns(module, &analysis, &known);
    known.extend(invariant);

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

/// Compute the parameters of recursive SCC members that are provably a single
/// literal or function reference at every entry, so they can be substituted in
/// place and dropped as dead. This is a monotone constant-propagation fixpoint
/// over the whole known-callee call graph, restricted to literal/function atoms
/// with parameter forwarding, ordered `Unknown < Known < Conflict`.
///
/// Only members of an eligible SCC participate: the SCC must be recursive and
/// must contain no escaping member and not the program entry, because an
/// escaping or host-called function receives arguments this analysis cannot
/// observe. `known_literals` seeds resolution of caller values already known to
/// be constant.
fn scc_invariant_knowns(
    module: &CpsModule,
    analysis: &CallAnalysis,
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut params_of: BTreeMap<CpsFunId, Vec<CpsValueId>> = BTreeMap::new();
    for members in eligible_sccs(module, analysis) {
        for &function in &members {
            params_of.insert(function, module.function(function).unwrap().params.clone());
        }
    }
    if params_of.is_empty() {
        return BTreeMap::new();
    }

    let mut constraints: Vec<(CpsFunId, Vec<CpsAtom>)> = Vec::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            ..
        } = node
            && params_of.contains_key(callee)
        {
            constraints.push((*callee, args.clone()));
        }
    }

    let class = invariant_fixpoint(&params_of, &constraints, known_literals);
    useful_knowns(class)
}

/// The members of every SCC eligible for known-argument analysis: recursive, and
/// containing neither an escaping member nor the program entry, because those
/// receive arguments the analysis cannot observe.
fn eligible_sccs(module: &CpsModule, analysis: &CallAnalysis) -> Vec<Vec<CpsFunId>> {
    analysis
        .sccs
        .members
        .iter()
        .filter(|members| {
            let recursive = members.len() > 1
                || members.iter().any(|function| {
                    analysis
                        .call_graph
                        .get(function)
                        .is_some_and(|edges| edges.contains(function))
                });
            let observable = members.iter().all(|function| {
                !analysis.escaping.contains(function) && Some(*function) != module.entry
            });
            recursive && observable
        })
        .cloned()
        .collect()
}

/// Run the monotone `Unknown < Known < Conflict` join to a fixpoint over the
/// given parameter positions and call constraints.
fn invariant_fixpoint(
    params_of: &BTreeMap<CpsFunId, Vec<CpsValueId>>,
    constraints: &[(CpsFunId, Vec<CpsAtom>)],
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> BTreeMap<CpsValueId, Knowledge> {
    let mut class: BTreeMap<CpsValueId, Knowledge> = params_of
        .values()
        .flatten()
        .map(|&param| (param, Knowledge::Unknown))
        .collect();
    loop {
        let mut changed = false;
        for (callee, args) in constraints {
            let Some(params) = params_of.get(callee) else {
                continue;
            };
            for (index, arg) in args.iter().enumerate() {
                let Some(&param) = params.get(index) else {
                    continue;
                };
                let incoming = resolve_atom(arg, &class, known_literals);
                let mut updated = class[&param].clone();
                updated.join(incoming);
                if updated != class[&param] {
                    class.insert(param, updated);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    class
}

/// Extract the parameters resolved to a single literal or function reference.
fn useful_knowns(class: BTreeMap<CpsValueId, Knowledge>) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut result = BTreeMap::new();
    for (param, knowledge) in class {
        if let Knowledge::Known(atom @ (CpsAtom::Literal(_) | CpsAtom::Fun(_))) = knowledge {
            result.insert(param, atom);
        }
    }
    result
}

/// Specialize a recursive SCC for one external call context whose known
/// arguments the module-wide analysis cannot use because other callers disagree.
///
/// The SCC is cloned verbatim and the disagreeing call site (with any siblings
/// passing the same arguments) is repointed to the private copy. The clone then
/// has a single agreeing external caller, so the ordinary invariant-known
/// propagation folds those arguments in place on a later iteration while the
/// original stays polymorphic for its other callers. At most `SCC_CLONE_LIMIT`
/// clones are made per module and only SCCs within `SCC_CLONE_NODE_LIMIT` live
/// nodes are cloned. One clone is performed per call so the outer fixpoint stays
/// deterministic.
fn specialize_scc_calls(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let analysis = analyze_calls(module);
    let literals = literal_value_map(module);
    let global = scc_invariant_knowns(module, &analysis, &literals);

    for members in eligible_sccs(module, &analysis) {
        let member_set: BTreeSet<CpsFunId> = members.iter().copied().collect();
        let node_count: usize = members
            .iter()
            .map(|&m| function_nodes(module, m).len())
            .sum();
        if node_count > SCC_CLONE_NODE_LIMIT {
            continue;
        }
        let Some(intro) = introducing_letfun(module, &member_set) else {
            continue;
        };

        let params_of: BTreeMap<CpsFunId, Vec<CpsValueId>> = members
            .iter()
            .map(|&m| (m, module.function(m).unwrap().params.clone()))
            .collect();
        let mut internal: Vec<(CpsFunId, Vec<CpsAtom>)> = Vec::new();
        let mut external: Vec<(CpsNodeId, CpsFunId, Vec<CpsAtom>)> = Vec::new();
        for (&node_id, &owner) in &analysis.node_owners {
            let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                ..
            }) = module.node(node_id)
            else {
                continue;
            };
            if !member_set.contains(callee) {
                continue;
            }
            if member_set.contains(&owner) {
                internal.push((*callee, args.clone()));
            } else {
                external.push((node_id, *callee, args.clone()));
            }
        }

        // Find the first external context that unlocks a known the module-wide
        // analysis could not, in deterministic call-site order.
        let mut chosen: Option<(CpsFunId, Vec<CpsAtom>)> = None;
        for (_, callee, args) in &external {
            let mut constraints = internal.clone();
            constraints.push((*callee, args.clone()));
            let context = useful_knowns(invariant_fixpoint(&params_of, &constraints, &literals));
            if context
                .iter()
                .any(|(param, atom)| global.get(param) != Some(atom))
            {
                chosen = Some((*callee, args.clone()));
                break;
            }
        }
        let Some((entry, context_args)) = chosen else {
            continue;
        };

        let Some(clones) = clone_scc(module, &member_set) else {
            continue;
        };
        let clone_entry = clones[&entry];
        if let Some(CpsNode::LetFun { functions, .. }) = module.nodes[intro.index()].as_mut() {
            functions.extend(clones.values().copied());
        }
        for (node_id, callee, args) in &external {
            if *callee == entry
                && *args == context_args
                && let Some(CpsNode::ApplyFun { callee, .. }) =
                    module.nodes[node_id.index()].as_mut()
            {
                *callee = CpsCallee::Known(clone_entry);
            }
        }
        *budget -= 1;
        return true;
    }
    false
}

/// SpecConstr-style call-pattern specialization. When a known-callee call passes
/// a statically-known tagged tuple into a parameter the callee deconstructs,
/// clone the callee with that constructor rebuilt at its entry so the existing
/// aggregate-projection and known-switch simplifications collapse the
/// deconstruction on a later iteration. The constructor's dynamic fields are
/// threaded as fresh parameters (a worker/wrapper rebuild) and the clone's
/// recursive self-calls fall back to the general function, so it peels the one
/// matched level rather than assuming the recursion stays in pattern. Every call
/// sharing the `(callee, index, tag, arity)` pattern repoints to the single
/// clone, so equivalent sites specialize once. Bounded by
/// `BRANCH_SPECIALIZATION_GROWTH_LIMIT` cloned live nodes and the module-wide
/// clone-count `budget`.
fn specialize_call_patterns(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let constructors = tagged_tuple_values(module);
    if constructors.is_empty() {
        return false;
    }

    // The first specializable pattern in deterministic (node, then argument)
    // order: a known-callee call whose argument is a known tagged tuple that the
    // callee deconstructs, whose callee has a lexical `LetFun` owner and a
    // clonable body within the growth budget.
    let mut chosen: Option<(CpsFunId, usize, u32, usize)> = None;
    'search: for node in module.nodes.iter().flatten() {
        let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            ..
        } = node
        else {
            continue;
        };
        if Some(*callee) == module.entry {
            continue;
        }
        let params = &module.function(*callee).unwrap().params;
        for (index, arg) in args.iter().enumerate() {
            let CpsAtom::Value(value) = arg else { continue };
            let Some((tag, fields)) = constructors.get(value) else {
                continue;
            };
            let Some(&param) = params.get(index) else {
                continue;
            };
            if !deconstructs_param(module, *callee, param) {
                continue;
            }
            let member = BTreeSet::from([*callee]);
            if introducing_letfun(module, &member).is_none() {
                continue;
            }
            let body = function_nodes(module, *callee);
            let unclonable = body.iter().any(|&id| {
                matches!(
                    module.node(id),
                    Some(CpsNode::LetFun { .. } | CpsNode::RecInit { .. })
                )
            });
            if unclonable || body.len() + 1 > BRANCH_SPECIALIZATION_GROWTH_LIMIT {
                continue;
            }
            chosen = Some((*callee, index, *tag, fields.len()));
            break 'search;
        }
    }
    let Some((callee, index, tag, arity)) = chosen else {
        return false;
    };

    let member = BTreeSet::from([callee]);
    let intro = introducing_letfun(module, &member).unwrap();
    let Some(clones) = clone_scc(module, &member) else {
        return false;
    };
    let clone = clones[&callee];

    // Peel: the clone recurses into the general function, not itself, so a
    // recursive call that does not carry the matched constructor stays valid.
    for node_id in function_nodes(module, clone) {
        let node = module.nodes[node_id.index()].as_mut().unwrap();
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(target),
            ..
        } = node
            && *target == clone
        {
            *target = callee;
        }
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Fun(function) = atom
                && *function == clone
            {
                *atom = CpsAtom::Fun(callee);
            }
        });
    }

    // Rebuild the constructor at the clone entry, threading its dynamic fields as
    // fresh parameters in place of the specialized parameter.
    let clone_function = module.function(clone).unwrap();
    let mut params = clone_function.params.clone();
    let clone_body = clone_function.body;
    let old_param = params[index];
    let field_params: Vec<CpsValueId> = (1..arity)
        .map(|field| module.add_value(Some(format!("field#{field}")), false))
        .collect();
    let mut rebuilt = Vec::with_capacity(arity);
    rebuilt.push(CpsAtom::Literal(CpsLiteral::Nat(tag)));
    rebuilt.extend(field_params.iter().map(|&p| CpsAtom::Value(p)));
    let entry = module.add_node(CpsNode::LetValue {
        result: old_param,
        value: CpsValueExpr::Tuple(rebuilt),
        next: clone_body,
    });
    params.splice(index..=index, field_params);
    let clone_function = module.functions[clone.index()].as_mut().unwrap();
    clone_function.params = params;
    clone_function.body = entry;

    // Introduce the clone in the callee's lexical scope.
    if let Some(CpsNode::LetFun { functions, .. }) = module.nodes[intro.index()].as_mut() {
        functions.push(clone);
    }

    // Repoint every call sharing the pattern to the single clone, splicing each
    // site's own constructor fields in place of the tuple argument.
    for node_id in 0..module.nodes.len() {
        let Some(CpsNode::ApplyFun {
            callee: CpsCallee::Known(target),
            args,
            ..
        }) = module.node(CpsNodeId(node_id as u32))
        else {
            continue;
        };
        if *target != callee {
            continue;
        }
        let Some(CpsAtom::Value(value)) = args.get(index) else {
            continue;
        };
        let Some((site_tag, site_fields)) = constructors.get(value) else {
            continue;
        };
        if *site_tag != tag || site_fields.len() != arity {
            continue;
        }
        let spliced = site_fields[1..].to_vec();
        let Some(CpsNode::ApplyFun {
            callee: target,
            args,
            ..
        }) = module.nodes[node_id].as_mut()
        else {
            unreachable!()
        };
        *target = CpsCallee::Known(clone);
        args.splice(index..=index, spliced);
    }

    *budget -= 1;
    true
}

/// The `LetValue`-bound tagged tuples: values whose defining expression is a
/// tuple whose first field is a `Nat` literal tag. These are the constructor
/// call patterns branch specialization can bake into a callee.
fn tagged_tuple_values(module: &CpsModule) -> BTreeMap<CpsValueId, (u32, Vec<CpsAtom>)> {
    let mut result = BTreeMap::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::LetValue {
            result: value,
            value: CpsValueExpr::Tuple(fields),
            ..
        } = node
            && let Some(CpsAtom::Literal(CpsLiteral::Nat(tag))) = fields.first()
        {
            result.insert(*value, (*tag, fields.clone()));
        }
    }
    result
}

/// Whether `function` projects a field out of `param`, i.e. contains a `TplGet`
/// on it. This is the profitability gate: baking a known tuple into a parameter
/// only pays off when the body actually deconstructs it.
fn deconstructs_param(module: &CpsModule, function: CpsFunId, param: CpsValueId) -> bool {
    function_nodes(module, function).iter().any(|&id| {
        matches!(
            module.node(id),
            Some(CpsNode::LetPrim {
                op: CpsPrimOp::TplGet(_),
                args,
                ..
            }) if args.first() == Some(&CpsAtom::Value(param))
        )
    })
}

/// The literal results of `LetValue` bindings, used to resolve caller values
/// already known to be constant.
fn literal_value_map(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut literals = BTreeMap::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(literal),
            ..
        } = node
        {
            literals.insert(*result, CpsAtom::Literal(literal.clone()));
        }
    }
    literals
}

/// The single `LetFun` node introducing every member, or `None` if the members
/// are split across nodes or introduced by a `RecInit` knot. The clones are
/// added to this node so they share the members' lexical scope.
fn introducing_letfun(module: &CpsModule, members: &BTreeSet<CpsFunId>) -> Option<CpsNodeId> {
    for (index, node) in module.nodes.iter().enumerate() {
        if let Some(CpsNode::LetFun { functions, .. }) = node {
            let introduced: BTreeSet<CpsFunId> = functions.iter().copied().collect();
            if members.is_subset(&introduced) {
                return Some(CpsNodeId(index as u32));
            }
        }
    }
    None
}

/// Verbatim-clone every member of an SCC into fresh functions with fresh return
/// continuations, local continuations, owned values, and nodes. Internal
/// known-callee edges and return continuations are rewired to the clones while
/// free values, external callees, and external continuations are shared. Returns
/// the old-to-new function map, or `None` if a member body nests a function
/// definition, which this verbatim clone does not reproduce.
fn clone_scc(
    module: &mut CpsModule,
    members: &BTreeSet<CpsFunId>,
) -> Option<BTreeMap<CpsFunId, CpsFunId>> {
    let member_defs: BTreeMap<CpsFunId, CpsFunction> = members
        .iter()
        .map(|&m| (m, module.function(m).unwrap().clone()))
        .collect();

    let mut node_ids: Vec<CpsNodeId> = Vec::new();
    for &m in members {
        node_ids.extend(function_nodes(module, m));
    }
    let node_defs: BTreeMap<CpsNodeId, CpsNode> = node_ids
        .iter()
        .map(|&id| (id, module.node(id).unwrap().clone()))
        .collect();
    if node_defs
        .values()
        .any(|node| matches!(node, CpsNode::LetFun { .. } | CpsNode::RecInit { .. }))
    {
        return None;
    }

    let cont_ids: BTreeSet<CpsContId> = node_defs
        .values()
        .filter_map(|node| match node {
            CpsNode::LetCont { continuations, .. } => Some(continuations.clone()),
            _ => None,
        })
        .flatten()
        .collect();
    let cont_defs: BTreeMap<CpsContId, CpsContinuation> = cont_ids
        .iter()
        .map(|&id| (id, module.continuation(id).unwrap().clone()))
        .collect();

    // Mint fresh owned values: member params, let-bound results, local
    // continuation parameters. Values defined outside the SCC are shared.
    let mut values: BTreeMap<CpsValueId, CpsValueId> = BTreeMap::new();
    let mut owned: Vec<CpsValueId> = Vec::new();
    for def in member_defs.values() {
        owned.extend(def.params.iter().copied());
    }
    for node in node_defs.values() {
        if let CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } = node {
            owned.push(*result);
        }
    }
    for cont in cont_defs.values() {
        owned.extend(cont.params.iter().copied());
    }
    for old in owned {
        let definition = module.values[old.index()].as_ref().unwrap().clone();
        let fresh = module.add_value(definition.debug_name, definition.candidate);
        values.insert(old, fresh);
    }

    let mut conts: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for &id in cont_defs.keys() {
        conts.insert(id, module.reserve_continuation());
    }
    let mut functions: BTreeMap<CpsFunId, CpsFunId> = BTreeMap::new();
    let mut returns: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for (&m, def) in &member_defs {
        functions.insert(m, module.reserve_function(def.debug_name.clone()));
        returns.insert(def.return_cont, module.reserve_continuation());
    }
    let mut nodes: BTreeMap<CpsNodeId, CpsNodeId> = BTreeMap::new();
    for &id in node_defs.keys() {
        nodes.insert(id, module.reserve_node());
    }

    let map_value = |value: CpsValueId| values.get(&value).copied().unwrap_or(value);
    let map_atom = |atom: &CpsAtom| match atom {
        CpsAtom::Value(value) => CpsAtom::Value(map_value(*value)),
        CpsAtom::Fun(function) => {
            CpsAtom::Fun(functions.get(function).copied().unwrap_or(*function))
        }
        CpsAtom::Literal(literal) => CpsAtom::Literal(literal.clone()),
    };
    let map_cont = |target: CpsContId| {
        returns
            .get(&target)
            .copied()
            .or_else(|| conts.get(&target).copied())
            .unwrap_or(target)
    };
    let map_edge = |edge: &CpsEdge| CpsEdge {
        target: map_cont(edge.target),
        args: edge.args.iter().map(&map_atom).collect(),
    };
    let map_callee = |callee: &CpsCallee| match callee {
        CpsCallee::Known(function) => {
            CpsCallee::Known(functions.get(function).copied().unwrap_or(*function))
        }
        CpsCallee::Closure(value) => CpsCallee::Closure(map_value(*value)),
    };

    let mut cloned_nodes: Vec<(CpsNodeId, CpsNode)> = Vec::new();
    for (&old, node) in &node_defs {
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
                next: nodes[next],
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
                next: nodes[next],
            },
            CpsNode::LetCont {
                continuations: members,
                body,
            } => CpsNode::LetCont {
                continuations: members.iter().map(|id| conts[id]).collect(),
                body: nodes[body],
            },
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => CpsNode::ApplyFun {
                callee: map_callee(callee),
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
            CpsNode::LetFun { .. } | CpsNode::RecInit { .. } => return None,
        };
        cloned_nodes.push((nodes[&old], cloned));
    }

    let mut cloned_conts: Vec<(CpsContId, CpsContinuation)> = Vec::new();
    for (&old, cont) in &cont_defs {
        cloned_conts.push((
            conts[&old],
            CpsContinuation {
                debug_name: cont.debug_name.clone(),
                params: cont.params.iter().map(|&p| map_value(p)).collect(),
                body: nodes[&cont.body],
            },
        ));
    }

    let mut cloned_functions: Vec<(CpsFunId, CpsFunction)> = Vec::new();
    for (&m, def) in &member_defs {
        cloned_functions.push((
            functions[&m],
            CpsFunction {
                debug_name: def.debug_name.clone(),
                params: def.params.iter().map(|&p| map_value(p)).collect(),
                return_cont: returns[&def.return_cont],
                body: nodes[&def.body],
            },
        ));
    }

    for (id, node) in cloned_nodes {
        module.nodes[id.index()] = Some(node);
    }
    for (id, cont) in cloned_conts {
        module.continuations[id.index()] = Some(cont);
    }
    for (id, function) in cloned_functions {
        module.define_function(id, function);
    }
    Some(functions)
}

/// Resolve an argument atom to its lattice value: literals and function
/// references are known; a value is a forwarded SCC parameter (its current
/// class), a caller constant (`known_literals`), or otherwise an unobservable
/// runtime value that forces `Conflict`.
fn resolve_atom(
    atom: &CpsAtom,
    class: &BTreeMap<CpsValueId, Knowledge>,
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> Knowledge {
    match atom {
        CpsAtom::Literal(literal) => Knowledge::Known(CpsAtom::Literal(literal.clone())),
        CpsAtom::Fun(function) => Knowledge::Known(CpsAtom::Fun(*function)),
        CpsAtom::Value(value) => {
            if let Some(knowledge) = class.get(value) {
                knowledge.clone()
            } else if let Some(atom @ (CpsAtom::Literal(_) | CpsAtom::Fun(_))) =
                known_literals.get(value)
            {
                Knowledge::Known(atom.clone())
            } else {
                Knowledge::Conflict
            }
        }
    }
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

        // A closure callee holds its callee in a value, which `visit_atoms_mut`
        // does not reach. Remap it here: a known function devirtualizes the call,
        // and a forwarded value (e.g. a projected constructor field) keeps the
        // callee pointing at a live value rather than a deleted one.
        if let CpsNode::ApplyFun { callee, .. } = node
            && let CpsCallee::Closure(value) = *callee
        {
            match known.get(&value) {
                Some(CpsAtom::Fun(function)) => {
                    *callee = CpsCallee::Known(*function);
                    changed = true;
                }
                Some(CpsAtom::Value(replacement)) if *replacement != value => {
                    *callee = CpsCallee::Closure(*replacement);
                    changed = true;
                }
                _ => {}
            }
        }
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
            module.continuation(edge.target)?;
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
        changed = true;
    }
    changed
}

fn eliminate_dead_bindings(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let counts = module.value_use_counts();
        let selected = module.nodes.iter().enumerate().find_map(|(index, node)| {
            let id = CpsNodeId(index as u32);
            match node.as_ref()? {
                CpsNode::LetValue { result, next, .. }
                    if counts.get(result).copied().unwrap_or(0) == 0 =>
                {
                    Some((id, *next, Some(*result)))
                }
                CpsNode::LetPrim {
                    result, op, next, ..
                } if op.is_total() && counts.get(result).copied().unwrap_or(0) == 0 => {
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
    let counts = module.value_use_counts();
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
                (counts.get(value).copied().unwrap_or(0) == 0).then_some(index)
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
                (counts.get(value).copied().unwrap_or(0) == 0).then_some(index)
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
    true
}

fn remove_parameter_indices<T>(values: &mut Vec<T>, removed: &BTreeSet<usize>) -> Vec<T> {
    let mut removed_values = Vec::new();
    let mut retained = Vec::with_capacity(values.len() - removed.len());
    for (index, value) in std::mem::take(values).into_iter().enumerate() {
        if removed.contains(&index) {
            removed_values.push(value);
        } else {
            retained.push(value);
        }
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
            BRANCH_CLONE_LIMIT, BRANCH_SPECIALIZATION_GROWTH_LIMIT, SCC_CLONE_LIMIT, analyze_sccs,
            atoms, contify_calls, dissolve_rec_init, eliminate_dead_bindings,
            eliminate_dead_parameters, evaluate, forward_aggregate_projections,
            forward_continuations, function_nodes, inline_known_calls,
            inline_single_use_continuations, known_values, optimize, rewrite_atoms, simplify_nodes,
            specialize_call_patterns, specialize_scc_calls,
        },
        crate::{
            CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
            CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsPrimOp, CpsValueExpr, CpsValueId,
        },
        std::collections::{BTreeMap, BTreeSet},
    };

    fn call_graph(edges: &[(u32, &[u32])]) -> BTreeMap<CpsFunId, BTreeSet<CpsFunId>> {
        edges
            .iter()
            .map(|(function, successors)| {
                (
                    CpsFunId(*function),
                    successors.iter().map(|s| CpsFunId(*s)).collect(),
                )
            })
            .collect()
    }

    #[test]
    fn sccs_group_cycles_and_stay_deterministic() {
        // 0 <-> 1 form a cycle; 1 -> 2 and 2 -> 2 leaves 2 a self-looping
        // singleton; 3 is isolated.
        let graph = call_graph(&[(0, &[1]), (1, &[0, 2]), (2, &[2]), (3, &[])]);
        let sccs = analyze_sccs(&graph);

        let component = |function: u32| sccs.component_of[&CpsFunId(function)];
        assert_eq!(component(0), component(1));
        assert_ne!(component(0), component(2));
        assert_ne!(component(2), component(3));
        assert_eq!(sccs.members.len(), 3);
        assert_eq!(
            sccs.members[component(0)],
            vec![CpsFunId(0), CpsFunId(1)],
            "cycle members are reported in CpsFunId order"
        );
        assert_eq!(sccs.members[component(2)], vec![CpsFunId(2)]);

        let again = analyze_sccs(&graph);
        assert_eq!(sccs.component_of, again.component_of);
        assert_eq!(sccs.members, again.members);
    }

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

    #[test]
    fn scc_invariant_known_argument_propagates_into_recursive_member() {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();

        // A trivial helper used only as an invariant first-class argument.
        let helper = module.reserve_function(Some("helper".into()));
        let helper_return = module.reserve_continuation();
        let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: helper_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        module.define_function(
            helper,
            CpsFunction {
                debug_name: Some("helper".into()),
                params: vec![],
                return_cont: helper_return,
                body: helper_body,
            },
        );

        // loop(invariant, counter): the recursive call forwards `invariant`
        // unchanged and replaces `counter`, so `invariant` is loop-invariant and
        // `counter` is not.
        let loop_function = module.reserve_function(Some("loop".into()));
        let loop_return = module.reserve_continuation();
        let invariant = module.add_value(Some("invariant".into()), false);
        let counter = module.add_value(Some("counter".into()), false);
        let recur = module.reserve_continuation();
        let recur_param = module.add_value(Some("recur".into()), false);
        let recur_body = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_function),
            args: vec![CpsAtom::Value(invariant), CpsAtom::Value(recur_param)],
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
            scrutinee: CpsAtom::Value(counter),
            cases: BTreeMap::from([(
                0,
                CpsEdge {
                    target: loop_return,
                    args: vec![CpsAtom::Value(counter)],
                },
            )]),
            default: Some(CpsEdge {
                target: recur,
                args: vec![CpsAtom::Value(counter)],
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
                params: vec![invariant, counter],
                return_cont: loop_return,
                body: loop_body,
            },
        );

        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_function),
            args: vec![CpsAtom::Fun(helper), CpsAtom::Literal(CpsLiteral::Nat(3))],
            return_to: entry_return,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![loop_function, helper],
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
        module.verify().unwrap();

        let known = known_values(&module);
        assert_eq!(
            known.get(&invariant),
            Some(&CpsAtom::Fun(helper)),
            "the invariant recursive parameter is recognized as the known function"
        );
        assert!(
            !known.contains_key(&counter),
            "the varying recursive parameter stays unknown"
        );
    }

    struct PolymorphicLoop {
        module: CpsModule,
        call1: CpsNodeId,
        call2: CpsNodeId,
        loop_fn: CpsFunId,
    }

    /// Build `loop(op, n)` which indirectly calls `op(n)` and recurses forwarding
    /// `op`, called from `entry` as `loop(add, 3)` then `loop(second, 4)`. When
    /// `second` differs from `add` the two contexts disagree. `padding` prepends
    /// dead `LetPrim` nodes to `loop`'s body to inflate its node count.
    fn polymorphic_loop(second_is_mul: bool, padding: usize) -> PolymorphicLoop {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();

        let trivial = |module: &mut CpsModule, name: &str| {
            let function = module.reserve_function(Some(name.into()));
            let function_return = module.reserve_continuation();
            let param = module.add_value(Some(format!("{name} x")), false);
            let function_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
                target: function_return,
                args: vec![CpsAtom::Value(param)],
            }));
            module.define_function(
                function,
                CpsFunction {
                    debug_name: Some(name.into()),
                    params: vec![param],
                    return_cont: function_return,
                    body: function_body,
                },
            );
            function
        };
        let add = trivial(&mut module, "add");
        let mul = trivial(&mut module, "mul");

        let loop_fn = module.reserve_function(Some("loop".into()));
        let loop_return = module.reserve_continuation();
        let op = module.add_value(Some("op".into()), false);
        let n = module.add_value(Some("n".into()), false);
        let after = module.reserve_continuation();
        let after_r = module.add_value(Some("after r".into()), false);
        let after_body = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_fn),
            args: vec![CpsAtom::Value(op), CpsAtom::Value(after_r)],
            return_to: loop_return,
        });
        module.define_continuation(
            after,
            CpsContinuation {
                debug_name: Some("after".into()),
                params: vec![after_r],
                body: after_body,
            },
        );
        let recur = module.reserve_continuation();
        let recur_m = module.add_value(Some("recur m".into()), false);
        let recur_body = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Closure(op),
            args: vec![CpsAtom::Value(recur_m)],
            return_to: after,
        });
        module.define_continuation(
            recur,
            CpsContinuation {
                debug_name: Some("recur".into()),
                params: vec![recur_m],
                body: recur_body,
            },
        );
        let switch = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(n),
            cases: BTreeMap::from([(
                0,
                CpsEdge {
                    target: loop_return,
                    args: vec![CpsAtom::Value(n)],
                },
            )]),
            default: Some(CpsEdge {
                target: recur,
                args: vec![CpsAtom::Value(n)],
            }),
        });
        let scope = module.add_node(CpsNode::LetCont {
            continuations: vec![recur, after],
            body: switch,
        });
        let mut loop_body = scope;
        for _ in 0..padding {
            let dead = module.add_value(None, false);
            loop_body = module.add_node(CpsNode::LetPrim {
                result: dead,
                op: CpsPrimOp::NatAdd,
                args: vec![
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                ],
                next: loop_body,
            });
        }
        module.define_function(
            loop_fn,
            CpsFunction {
                debug_name: Some("loop".into()),
                params: vec![op, n],
                return_cont: loop_return,
                body: loop_body,
            },
        );

        let second = if second_is_mul { mul } else { add };
        let x1 = module.add_value(Some("x1".into()), false);
        let call2 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_fn),
            args: vec![CpsAtom::Fun(second), CpsAtom::Literal(CpsLiteral::Nat(4))],
            return_to: entry_return,
        });
        let k1 = module.reserve_continuation();
        module.define_continuation(
            k1,
            CpsContinuation {
                debug_name: Some("k1".into()),
                params: vec![x1],
                body: call2,
            },
        );
        let call1 = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(loop_fn),
            args: vec![CpsAtom::Fun(add), CpsAtom::Literal(CpsLiteral::Nat(3))],
            return_to: k1,
        });
        let outer = module.add_node(CpsNode::LetCont {
            continuations: vec![k1],
            body: call1,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![loop_fn, add, mul],
            body: outer,
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
        module.verify().unwrap();
        PolymorphicLoop {
            module,
            call1,
            call2,
            loop_fn,
        }
    }

    fn known_callee(module: &CpsModule, node: CpsNodeId) -> CpsFunId {
        match module.node(node).unwrap() {
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                ..
            } => *callee,
            _ => panic!("call site changed shape"),
        }
    }

    #[test]
    fn specializes_a_polymorphic_recursive_scc_per_call_context() {
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            loop_fn,
        } = polymorphic_loop(true, 0);

        let mut budget = SCC_CLONE_LIMIT;
        assert!(
            specialize_scc_calls(&mut module, &mut budget),
            "a disagreeing call context is specialized"
        );
        assert_eq!(budget, SCC_CLONE_LIMIT - 1, "one clone consumed the budget");
        module.verify().unwrap();

        let first = known_callee(&module, call1);
        let second = known_callee(&module, call2);
        assert_ne!(
            first, second,
            "the two contexts now call different functions"
        );
        assert!(
            first == loop_fn || second == loop_fn,
            "one context keeps the original polymorphic function"
        );
        assert!(
            first != loop_fn || second != loop_fn,
            "one context is repointed to a fresh clone"
        );
    }

    #[test]
    fn agreeing_call_contexts_are_not_specialized() {
        // Both sites pass `add`, so the module-wide analysis already knows the
        // argument and cloning would add nothing.
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            loop_fn,
        } = polymorphic_loop(false, 0);

        let mut budget = SCC_CLONE_LIMIT;
        assert!(
            !specialize_scc_calls(&mut module, &mut budget),
            "no clone is made when callers agree"
        );
        assert_eq!(budget, SCC_CLONE_LIMIT);
        assert_eq!(known_callee(&module, call1), loop_fn);
        assert_eq!(known_callee(&module, call2), loop_fn);
    }

    #[test]
    fn specialization_respects_the_clone_budget() {
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            loop_fn,
        } = polymorphic_loop(true, 0);

        let mut budget = 0;
        assert!(
            !specialize_scc_calls(&mut module, &mut budget),
            "an exhausted budget makes no clone"
        );
        assert_eq!(known_callee(&module, call1), loop_fn);
        assert_eq!(known_callee(&module, call2), loop_fn);
    }

    #[test]
    fn specialization_respects_the_node_budget() {
        // Inflate `loop` past SCC_CLONE_NODE_LIMIT live nodes.
        let PolymorphicLoop {
            mut module,
            call1,
            call2,
            loop_fn,
        } = polymorphic_loop(true, super::SCC_CLONE_NODE_LIMIT + 1);

        let mut budget = SCC_CLONE_LIMIT;
        assert!(
            !specialize_scc_calls(&mut module, &mut budget),
            "an oversized SCC is not cloned"
        );
        assert_eq!(budget, SCC_CLONE_LIMIT);
        assert_eq!(known_callee(&module, call1), loop_fn);
        assert_eq!(known_callee(&module, call2), loop_fn);
    }

    #[test]
    fn specialization_is_deterministic() {
        let run = || {
            let PolymorphicLoop {
                mut module,
                call1,
                call2,
                ..
            } = polymorphic_loop(true, 0);
            let mut budget = SCC_CLONE_LIMIT;
            specialize_scc_calls(&mut module, &mut budget);
            (
                known_callee(&module, call1).0,
                known_callee(&module, call2).0,
            )
        };
        assert_eq!(run(), run(), "specialization output is a pure function");
    }

    #[test]
    fn optimization_specializes_away_the_polymorphic_indirect_call() {
        // With each caller peeled into its own clone, invariant-known propagation
        // resolves every `op` to a direct callee, leaving no closure calls.
        let PolymorphicLoop { mut module, .. } = polymorphic_loop(true, 0);
        optimize(&mut module);
        module.verify().unwrap();
        assert!(
            module.nodes.iter().flatten().all(|node| !matches!(
                node,
                CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(_),
                    ..
                }
            )),
            "specialization turned every recursive indirect call into a direct call"
        );
    }

    // Build `helper(x) = x`, non-escaping, called from `entry` at one or two
    // external sites. Returns the module and the helper function.
    fn helper_called(two_sites: bool) -> (CpsModule, CpsFunId) {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();
        let helper = module.reserve_function(Some("helper".into()));
        let helper_return = module.reserve_continuation();
        let x = module.add_value(Some("x".into()), false);
        let helper_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: helper_return,
            args: vec![CpsAtom::Value(x)],
        }));
        module.define_function(
            helper,
            CpsFunction {
                debug_name: Some("helper".into()),
                params: vec![x],
                return_cont: helper_return,
                body: helper_body,
            },
        );

        let inner = if two_sites {
            let call2 = module.add_node(CpsNode::ApplyFun {
                callee: CpsCallee::Known(helper),
                args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
                return_to: entry_return,
            });
            let param = module.add_value(None, false);
            let bridge = module.reserve_continuation();
            module.define_continuation(
                bridge,
                CpsContinuation {
                    debug_name: None,
                    params: vec![param],
                    body: call2,
                },
            );
            let call1 = module.add_node(CpsNode::ApplyFun {
                callee: CpsCallee::Known(helper),
                args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
                return_to: bridge,
            });
            module.add_node(CpsNode::LetCont {
                continuations: vec![bridge],
                body: call1,
            })
        } else {
            module.add_node(CpsNode::ApplyFun {
                callee: CpsCallee::Known(helper),
                args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
                return_to: entry_return,
            })
        };
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![helper],
            body: inner,
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
        module.verify().unwrap();
        (module, helper)
    }

    #[test]
    fn contifies_a_nonrecursive_single_call_function() {
        let (mut module, helper) = helper_called(false);
        assert!(
            contify_calls(&mut module),
            "the single-call helper is contified"
        );
        assert!(
            module.function(helper).is_none(),
            "the contified function is replaced by a local continuation"
        );
        module.verify().unwrap();
    }

    #[test]
    fn does_not_contify_a_multi_site_function() {
        // Two return contexts: single-site placement cannot cover both, so this
        // is left for common-dominator contification in the machine CFG.
        let (mut module, helper) = helper_called(true);
        assert!(
            !contify_calls(&mut module),
            "a function with two call sites is not contified here"
        );
        assert!(module.function(helper).is_some());
    }

    // Build `main` whose body is a `RecInit` over `f` and computed value `v`.
    // `v` is produced by a `rec/v` continuation and returned at the ready point.
    // When `captures` is set, `f` forward-references `v` (a live mixed knot);
    // otherwise `f` is independent and the knot is already broken.
    fn rec_init_module(captures: bool) -> (CpsModule, CpsNodeId) {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();

        let f = module.reserve_function(Some("f".into()));
        let f_return = module.reserve_continuation();
        let a = module.add_value(Some("a".into()), false);
        let v = module.add_value(Some("v".into()), false);
        let f_result = if captures {
            CpsAtom::Value(v)
        } else {
            CpsAtom::Value(a)
        };
        let f_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: f_return,
            args: vec![f_result],
        }));
        module.define_function(
            f,
            CpsFunction {
                debug_name: Some("f".into()),
                params: vec![a],
                return_cont: f_return,
                body: f_body,
            },
        );

        let ready = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: entry_return,
            args: vec![CpsAtom::Value(v)],
        }));
        let rec_v = module.reserve_continuation();
        module.define_continuation(
            rec_v,
            CpsContinuation {
                debug_name: Some("rec/v".into()),
                params: vec![v],
                body: ready,
            },
        );
        let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: rec_v,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        let init_body = module.add_node(CpsNode::LetCont {
            continuations: vec![rec_v],
            body: enter,
        });
        let rec_init = module.add_node(CpsNode::RecInit {
            functions: vec![f],
            values: vec![v],
            ready,
            body: init_body,
        });
        module.define_function(
            entry,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont: entry_return,
                body: rec_init,
            },
        );
        module.set_entry(entry);
        module.verify().unwrap();
        (module, rec_init)
    }

    #[test]
    fn dissolves_a_broken_recursive_initializer() {
        let (mut module, rec_init) = rec_init_module(false);
        assert!(
            dissolve_rec_init(&mut module),
            "a knot no member still captures dissolves"
        );
        assert!(
            matches!(module.node(rec_init), Some(CpsNode::LetFun { .. })),
            "the initializer becomes an ordinary function group"
        );
        module.verify().unwrap();
    }

    #[test]
    fn retains_a_live_recursive_initializer() {
        let (mut module, rec_init) = rec_init_module(true);
        assert!(
            !dissolve_rec_init(&mut module),
            "a member still capturing a computed value keeps the fallback"
        );
        assert!(matches!(
            module.node(rec_init),
            Some(CpsNode::RecInit { .. })
        ));
    }

    // Build `main` calling a non-recursive `consume(t)` once per entry in
    // `sites`. `consume` projects the tag and a field out of its tuple parameter
    // and switches on the tag, so a known tagged tuple at a call site unlocks the
    // fold. Each site `i` passes the tuple `(sites[i], i)`. `padding` pads
    // `consume` with dead bindings to grow its live-node count. Returns the
    // module, the call node per site (in `sites` order), and `consume`.
    fn tagged_consumer(padding: usize, sites: &[u32]) -> (CpsModule, Vec<CpsNodeId>, CpsFunId) {
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();

        let consume = module.reserve_function(Some("consume".into()));
        let consume_return = module.reserve_continuation();
        let t = module.add_value(Some("t".into()), false);
        let tag = module.add_value(Some("tag".into()), false);
        let val = module.add_value(Some("val".into()), false);
        let switch = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(tag),
            cases: BTreeMap::from([
                (
                    0,
                    CpsEdge {
                        target: consume_return,
                        args: vec![CpsAtom::Value(val)],
                    },
                ),
                (
                    1,
                    CpsEdge {
                        target: consume_return,
                        args: vec![CpsAtom::Literal(CpsLiteral::Nat(999))],
                    },
                ),
            ]),
            default: Some(CpsEdge {
                target: consume_return,
                args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
            }),
        });
        let project_val = module.add_node(CpsNode::LetPrim {
            result: val,
            op: CpsPrimOp::TplGet(1),
            args: vec![CpsAtom::Value(t)],
            next: switch,
        });
        let mut consume_body = module.add_node(CpsNode::LetPrim {
            result: tag,
            op: CpsPrimOp::TplGet(0),
            args: vec![CpsAtom::Value(t)],
            next: project_val,
        });
        for _ in 0..padding {
            let dead = module.add_value(None, false);
            consume_body = module.add_node(CpsNode::LetPrim {
                result: dead,
                op: CpsPrimOp::NatAdd,
                args: vec![
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                ],
                next: consume_body,
            });
        }
        module.define_function(
            consume,
            CpsFunction {
                debug_name: Some("consume".into()),
                params: vec![t],
                return_cont: consume_return,
                body: consume_body,
            },
        );

        // Build the call chain forward so the search visits `sites[0]` first.
        // Each site's return continuation is introduced by its own `LetCont`, and
        // returning from site `i` runs site `i + 1`.
        let count = sites.len();
        let results: Vec<CpsValueId> = (0..count)
            .map(|i| module.add_value(Some(format!("r{i}")), false))
            .collect();
        let ctors: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
        let calls: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
        let scopes: Vec<CpsNodeId> = (0..count).map(|_| module.reserve_node()).collect();
        let conts: Vec<CpsContId> = (0..count).map(|_| module.reserve_continuation()).collect();
        let tail = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: entry_return,
            args: vec![match results.last() {
                Some(&last) => CpsAtom::Value(last),
                None => CpsAtom::Literal(CpsLiteral::Nat(0)),
            }],
        }));
        for i in 0..count {
            let value = module.add_value(Some(format!("v{i}")), false);
            module.define_node(
                ctors[i],
                CpsNode::LetValue {
                    result: value,
                    value: CpsValueExpr::Tuple(vec![
                        CpsAtom::Literal(CpsLiteral::Nat(sites[i])),
                        CpsAtom::Literal(CpsLiteral::Nat(i as u32)),
                    ]),
                    next: calls[i],
                },
            );
            module.define_node(
                calls[i],
                CpsNode::ApplyFun {
                    callee: CpsCallee::Known(consume),
                    args: vec![CpsAtom::Value(value)],
                    return_to: conts[i],
                },
            );
            module.define_node(
                scopes[i],
                CpsNode::LetCont {
                    continuations: vec![conts[i]],
                    body: ctors[i],
                },
            );
            let next = if i + 1 < count { scopes[i + 1] } else { tail };
            module.define_continuation(
                conts[i],
                CpsContinuation {
                    debug_name: Some(format!("k{i}")),
                    params: vec![results[i]],
                    body: next,
                },
            );
        }
        let first = scopes.first().copied().unwrap_or(tail);
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![consume],
            body: first,
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
        module.verify().unwrap();
        (module, calls, consume)
    }

    #[test]
    fn rewrite_atoms_remaps_and_devirtualizes_a_closure_callee() {
        // The closure callee holds its target in a value that `visit_atoms_mut`
        // never reaches. A forwarded value must follow (else the callee dangles
        // when the original value is deleted), and a known function devirtualizes.
        let mut module = CpsModule::new();
        let ret = module.reserve_continuation();
        let old = module.add_value(Some("old".into()), false);
        let new = module.add_value(Some("new".into()), false);
        let target = module.reserve_function(Some("target".into()));

        let value_call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Closure(old),
            args: vec![],
            return_to: ret,
        });
        assert!(rewrite_atoms(
            &mut module,
            &BTreeMap::from([(old, CpsAtom::Value(new))]),
        ));
        assert!(
            matches!(module.node(value_call), Some(CpsNode::ApplyFun { callee: CpsCallee::Closure(v), .. }) if *v == new),
            "a forwarded value keeps the closure callee pointing at a live value"
        );

        let fun_call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Closure(new),
            args: vec![],
            return_to: ret,
        });
        assert!(rewrite_atoms(
            &mut module,
            &BTreeMap::from([(new, CpsAtom::Fun(target))]),
        ));
        assert!(
            matches!(module.node(fun_call), Some(CpsNode::ApplyFun { callee: CpsCallee::Known(f), .. }) if *f == target),
            "a known function devirtualizes the closure call"
        );
    }

    fn has_switch(module: &CpsModule, function: CpsFunId) -> bool {
        function_nodes(module, function)
            .iter()
            .any(|&id| matches!(module.node(id), Some(CpsNode::Switch { .. })))
    }

    #[test]
    fn specializes_a_constructor_argument_and_collapses_the_switch() {
        let (mut module, calls, consume) = tagged_consumer(0, &[0]);
        let mut budget = BRANCH_CLONE_LIMIT;
        assert!(
            specialize_call_patterns(&mut module, &mut budget),
            "a known tagged-tuple argument is specialized"
        );
        assert_eq!(
            budget,
            BRANCH_CLONE_LIMIT - 1,
            "one clone consumed the budget"
        );
        module.verify().unwrap();

        let clone = known_callee(&module, calls[0]);
        assert_ne!(clone, consume, "the call is repointed to a fresh clone");
        assert!(
            has_switch(&module, consume),
            "the general function keeps its switch"
        );

        // The rebuilt constructor lets the existing folds collapse the switch.
        while forward_aggregate_projections(&mut module) | simplify_nodes(&mut module) {}
        assert!(
            !has_switch(&module, clone),
            "projection and known-switch folding collapse the clone's dispatch"
        );
        module.verify().unwrap();
    }

    #[test]
    fn equivalent_constructor_sites_share_one_clone() {
        // Two tag-0 sites match one pattern; a tag-1 site is a different pattern.
        let (mut module, calls, consume) = tagged_consumer(0, &[0, 0, 1]);
        let before = module.functions().iter().flatten().count();
        let mut budget = BRANCH_CLONE_LIMIT;
        assert!(specialize_call_patterns(&mut module, &mut budget));
        module.verify().unwrap();

        let clone = known_callee(&module, calls[0]);
        assert_ne!(clone, consume);
        assert_eq!(
            known_callee(&module, calls[1]),
            clone,
            "an equivalent site reuses the one clone"
        );
        assert_eq!(
            known_callee(&module, calls[2]),
            consume,
            "a non-matching pattern keeps the original function"
        );
        let after = module.functions().iter().flatten().count();
        assert_eq!(after, before + 1, "exactly one clone is created");
    }

    #[test]
    fn call_pattern_specialization_respects_the_growth_budget() {
        // Inflate `consume` past BRANCH_SPECIALIZATION_GROWTH_LIMIT live nodes.
        let (mut module, calls, consume) =
            tagged_consumer(BRANCH_SPECIALIZATION_GROWTH_LIMIT + 1, &[0]);
        let mut budget = BRANCH_CLONE_LIMIT;
        assert!(
            !specialize_call_patterns(&mut module, &mut budget),
            "an oversized callee is not specialized"
        );
        assert_eq!(budget, BRANCH_CLONE_LIMIT);
        assert_eq!(known_callee(&module, calls[0]), consume);
    }

    #[test]
    fn call_pattern_specialization_respects_the_clone_budget() {
        let (mut module, calls, consume) = tagged_consumer(0, &[0]);
        let mut budget = 0;
        assert!(
            !specialize_call_patterns(&mut module, &mut budget),
            "an exhausted budget makes no clone"
        );
        assert_eq!(known_callee(&module, calls[0]), consume);
    }

    #[test]
    fn call_pattern_specialization_is_deterministic() {
        let run = || {
            let (mut module, calls, _) = tagged_consumer(0, &[0, 0]);
            let mut budget = BRANCH_CLONE_LIMIT;
            specialize_call_patterns(&mut module, &mut budget);
            (
                known_callee(&module, calls[0]).0,
                known_callee(&module, calls[1]).0,
            )
        };
        assert_eq!(run(), run(), "specialization output is a pure function");
    }

    #[test]
    fn optimization_eliminates_a_constructor_dispatch() {
        // A multi-site, oversized-for-inlining consumer: only specialization can
        // resolve the tagged dispatch, and folding then removes every switch.
        let (mut module, _, _) = tagged_consumer(8, &[0, 0]);
        optimize(&mut module);
        module.verify().unwrap();
        assert!(
            module
                .nodes()
                .iter()
                .flatten()
                .all(|node| !matches!(node, CpsNode::Switch { .. })),
            "specialization and folding leave no residual tag dispatch"
        );
    }

    #[test]
    fn specialization_peels_a_recursive_callee_into_the_general_function() {
        // consume(t): leaf returns the field; node recurses on the child.
        let mut module = CpsModule::new();
        let entry = module.reserve_function(Some("main".into()));
        let entry_return = module.reserve_continuation();

        let consume = module.reserve_function(Some("consume".into()));
        let consume_return = module.reserve_continuation();
        let t = module.add_value(Some("t".into()), false);
        let tag = module.add_value(Some("tag".into()), false);
        let child = module.add_value(Some("child".into()), false);
        let leaf = module.reserve_continuation();
        let node = module.reserve_continuation();
        let leaf_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: consume_return,
            args: vec![CpsAtom::Value(child)],
        }));
        module.define_continuation(
            leaf,
            CpsContinuation {
                debug_name: Some("leaf".into()),
                params: vec![],
                body: leaf_body,
            },
        );
        let node_body = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(consume),
            args: vec![CpsAtom::Value(child)],
            return_to: consume_return,
        });
        module.define_continuation(
            node,
            CpsContinuation {
                debug_name: Some("node".into()),
                params: vec![],
                body: node_body,
            },
        );
        let switch = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(tag),
            cases: BTreeMap::from([(
                0,
                CpsEdge {
                    target: leaf,
                    args: vec![],
                },
            )]),
            default: Some(CpsEdge {
                target: node,
                args: vec![],
            }),
        });
        let scope = module.add_node(CpsNode::LetCont {
            continuations: vec![leaf, node],
            body: switch,
        });
        let project_child = module.add_node(CpsNode::LetPrim {
            result: child,
            op: CpsPrimOp::TplGet(1),
            args: vec![CpsAtom::Value(t)],
            next: scope,
        });
        let project_tag = module.add_node(CpsNode::LetPrim {
            result: tag,
            op: CpsPrimOp::TplGet(0),
            args: vec![CpsAtom::Value(t)],
            next: project_child,
        });
        module.define_function(
            consume,
            CpsFunction {
                debug_name: Some("consume".into()),
                params: vec![t],
                return_cont: consume_return,
                body: project_tag,
            },
        );

        let root = module.add_value(Some("root".into()), false);
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(consume),
            args: vec![CpsAtom::Value(root)],
            return_to: entry_return,
        });
        let ctor = module.add_node(CpsNode::LetValue {
            result: root,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(0)),
                CpsAtom::Literal(CpsLiteral::Nat(5)),
            ]),
            next: call,
        });
        let body = module.add_node(CpsNode::LetFun {
            functions: vec![consume],
            body: ctor,
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
        module.verify().unwrap();

        let mut budget = BRANCH_CLONE_LIMIT;
        assert!(specialize_call_patterns(&mut module, &mut budget));
        module.verify().unwrap();

        let clone = known_callee(&module, call);
        assert_ne!(clone, consume);
        let recursive_target =
            function_nodes(&module, clone)
                .into_iter()
                .find_map(|id| match module.node(id) {
                    Some(CpsNode::ApplyFun {
                        callee: CpsCallee::Known(target),
                        ..
                    }) => Some(*target),
                    _ => None,
                });
        assert_eq!(
            recursive_target,
            Some(consume),
            "the clone peels one level and recurses into the general function"
        );
    }
}
