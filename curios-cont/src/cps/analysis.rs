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
/// Function strongly-connected components of the known-callee call graph, computed at an explicit phase boundary. `SccId` is a dense index into `members`; each component lists its functions in `CpsFunId` order.
pub(super) type SccId = usize;
#[derive(Default)]
pub(super) struct SccAnalysis {
    pub(super) component_of: BTreeMap<CpsFunId, SccId>,
    pub(super) members: Vec<Vec<CpsFunId>>,
}
/// Deterministic iterative Tarjan over the known-callee call graph. Uses an explicit frame stack rather than recursion so it stays within the default test-thread stack on deep call graphs. Components are numbered in the order their roots pop, and members are sorted, so the output is a pure function of the graph.
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
    for (function, _) in module.functions.iter_live() {
        analysis.call_sites.entry(function).or_default();
        analysis.call_graph.entry(function).or_default();
    }

    for owner in module.functions.live_ids().collect::<Vec<_>>() {
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

    // A function is recursive exactly when it lies on a call cycle: it is in a multi-member SCC, or it is a singleton SCC with a self-edge. Deriving the set from the SCC phase keeps one source of truth for cyclicity.
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
/// Every node in `function`'s own body, stopping at each nested function's boundary — see [`free_values`] for which callers that suits and which it does not.
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
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => work.push(*next),
            CpsNode::LetFun { body, .. } => work.push(*body),
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for continuation in continuations.iter().rev() {
                    // Tolerate a tombstoned continuation: an inline sweep can leave a `LetCont` transiently referencing an inlined-away continuation until its sweep-ending prune. That continuation's body is dead, so skipping it is correct.
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
/// The values `function` binds, and the values it mentions — from one walk of its body.
///
/// The two are collected together because the pair *is* the content of both callers below: free is `used \ owned` and available is `owned ∪ used`. Computing them apart cost three walks to answer a question one walk holds, and made `available_values` derive `owned` twice — once directly and once inside the `free_values` it called.
fn owned_and_used(
    module: &CpsModule,
    function: CpsFunId,
) -> (BTreeSet<CpsValueId>, BTreeSet<CpsValueId>) {
    let mut owned = module
        .function(function)
        .unwrap()
        .params
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let mut used = BTreeSet::new();

    for node_id in function_nodes(module, function) {
        let node = module.node(node_id).unwrap();

        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
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
            // A closure callee is a value the body reads, and it is the one such read that is not an operand atom.
            CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
                ..
            } => {
                used.insert(*value);
            }
            _ => {}
        }

        for atom in atoms(node) {
            if let CpsAtom::Value(value) = atom {
                used.insert(*value);
            }
        }
    }

    (owned, used)
}

/// The values `function` mentions without binding — what lowering must carry into it.
///
/// **The walk stops at a nested function.** [`function_nodes`] enters a `LetFun`'s body and not its members, so a value referenced only inside a function defined *within* this one is not reported here. That is correct for a caller asking what to carry into a call or a closure — a nested function's own captures are answered when the sweep reaches that function, which is the shape `represent`'s `offers` sweep over every live function relies on. It is wrong for a caller about to *remove* a binding, which must cover the whole region that loses it — a pass once asked this question for that purpose and dropped a binding a nested function still referenced.
pub(super) fn free_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let (owned, used) = owned_and_used(module, function);
    used.difference(&owned).copied().collect()
}

/// Everything in scope in `function`: what it binds, plus what it inherits.
///
/// Stated as `owned ∪ used` rather than `owned ∪ free`, which is the same set — `free` is `used \ owned`, so the old spelling subtracted `owned` only to add it straight back.
pub(super) fn available_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
    let (mut available, used) = owned_and_used(module, function);
    available.extend(used);
    available
}

pub(super) fn known_values(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut known = BTreeMap::new();

    for (_, node) in module.nodes.iter_live() {
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
    for (function, definition) in module.functions.iter_live() {
        function_inputs.insert(function, vec![Knowledge::Unknown; definition.params.len()]);
    }

    for (_, node) in module.nodes.iter_live() {
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

    // Recursive members are skipped above because a self-forwarded argument pollutes the flat per-call join. Recover their provably-invariant known parameters with a dedicated SCC fixpoint and fold them in.
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
/// Resolve an argument atom to its lattice value: literals and function references are known; a value is a forwarded SCC parameter (its current class), a caller constant (`known_literals`), or otherwise an unobservable runtime value that forces `Conflict`.
pub(super) fn resolve_atom(
    atom: &CpsAtom,
    class: &BTreeMap<CpsValueId, Knowledge>,
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> Knowledge {
    match atom {
        CpsAtom::Literal(literal) => Knowledge::Known(CpsAtom::Literal(literal.clone())),
        CpsAtom::Fun(function) => Knowledge::Known(CpsAtom::Fun(*function)),
        // A filler is unobservable rather than known: propagating it would substitute "no value" into a position that reads one.
        CpsAtom::Filler => Knowledge::Conflict,
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
