use {
    super::specialize::{Knowledge, merge_inputs, record_known_literals, scc_invariant_knowns},
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Default)]
pub(super) struct CallAnalysis {
    pub(super) call_sites: BTreeMap<CpsFunId, Vec<CpsNodeId>>,
    pub(super) call_graph: BTreeMap<CpsFunId, BTreeSet<CpsFunId>>,
    pub(super) node_owners: BTreeMap<CpsNodeId, CpsFunId>,
    pub(super) escaping: BTreeSet<CpsFunId>,
    pub(super) recursive: BTreeSet<CpsFunId>,
    pub(super) sccs: SccAnalysis,
}
/// Function strongly-connected components of the known-callee call graph,
/// computed at an explicit phase boundary. `SccId` is a dense index into
/// `members`; each component lists its functions in `CpsFunId` order.
pub(super) type SccId = usize;
#[derive(Default)]
pub(super) struct SccAnalysis {
    pub(super) component_of: BTreeMap<CpsFunId, SccId>,
    pub(super) members: Vec<Vec<CpsFunId>>,
}
/// Deterministic iterative Tarjan over the known-callee call graph. Uses an
/// explicit frame stack rather than recursion so it stays within the default
/// test-thread stack on deep call graphs. Components are numbered in the order
/// their roots pop, and members are sorted, so the output is a pure function of
/// the graph.
pub(super) fn analyze_sccs(call_graph: &BTreeMap<CpsFunId, BTreeSet<CpsFunId>>) -> SccAnalysis {
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
pub(super) fn analyze_calls(module: &CpsModule) -> CallAnalysis {
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
pub(super) fn function_nodes(module: &CpsModule, function: CpsFunId) -> Vec<CpsNodeId> {
    nodes_from(module, module.function(function).unwrap().body)
}
pub(super) fn nodes_from(module: &CpsModule, body: CpsNodeId) -> Vec<CpsNodeId> {
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
                    // Tolerate a tombstoned continuation: an inline sweep can leave a
                    // `LetCont` transiently referencing an inlined-away continuation
                    // until its sweep-ending prune. That continuation's body is dead,
                    // so skipping it is correct.
                    if let Some(continuation) = module.continuation(*continuation) {
                        work.push(continuation.body);
                    }
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
pub(super) fn owned_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
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
pub(super) fn free_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
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
pub(super) fn available_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let mut available = owned_values(module, function);
    available.extend(free_values(module, function));
    available
}
pub(super) fn known_values(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
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
/// Resolve an argument atom to its lattice value: literals and function
/// references are known; a value is a forwarded SCC parameter (its current
/// class), a caller constant (`known_literals`), or otherwise an unobservable
/// runtime value that forces `Conflict`.
pub(super) fn resolve_atom(
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
