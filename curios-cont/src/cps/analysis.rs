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
    /// On a cycle of the call graph closed under definition — a callee inherits the calls of every function nested within it — which is the inliner's question: see `analyze_calls`.
    pub(super) recursive: BTreeSet<CpsFunId>,
    pub(super) sccs: SccAnalysis,
    /// The functions each function's body may name: its own `LetFun` group, every group enclosing it, and every group bound before it along the chain from the entry — the scope `verify_lexical_scopes` walks, recorded per function so a pass that forwards a function reference into a body can ask whether that body may name it. Where the walk does not reach a live function, the entry is the owner-chain subset instead, which refuses more than the rule does and never less.
    pub(super) lexical_scope: BTreeMap<CpsFunId, BTreeSet<CpsFunId>>,
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

    // The function references each body names, collected beside `escaping` off the same atoms, and which bodies hold a closure callee at all. The recursion verdict below needs both; the body-only `call_graph` must carry neither, since a reference is not a call site.
    let mut named_in: BTreeMap<CpsFunId, BTreeSet<CpsFunId>> = BTreeMap::new();
    let mut applies_a_closure: BTreeSet<CpsFunId> = BTreeSet::new();
    for owner in module.functions.live_ids().collect::<Vec<_>>() {
        for node_id in function_nodes(module, owner) {
            analysis.node_owners.insert(node_id, owner);
            let node = module.node(node_id).unwrap();
            match node {
                CpsNode::ApplyFun {
                    callee: CpsCallee::Known(callee),
                    ..
                } => {
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
                CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(_),
                    ..
                } => {
                    applies_a_closure.insert(owner);
                }
                _ => {}
            }
            for atom in atoms(node) {
                if let CpsAtom::Fun(function) = atom {
                    analysis.escaping.insert(*function);
                    named_in.entry(owner).or_default().insert(*function);
                }
            }
        }
    }

    // A function is recursive, for the inliner, when it lies on a cycle of what an inline *copies*. A callee's extent carries every function defined lexically within it (`copied_extent`), so a call one of those makes is reproduced by each copy as surely as a call the callee's own body makes — and a copy that reaches back to the caller is a call the next sweep meets again, with nothing but the size limits to end it. That is how a by-need knot's forcing function, its initializer and the closure the initializer builds — three small functions, none calling itself, each reaching the next through a call or a definition — inlined one another without bound. So the verdict is taken over the call graph closed under definition: an owner inherits the calls of every function nested within it, transitively.
    //
    // A body that *names* a function can reach it the same way, and this is the second half of the closure rather than a separate rule. `map_atom` carries a `CpsAtom::Fun` through a copy unchanged, and substituting one into a closure callee is what turns it into a known call, so a reference such a body reproduces is a call it reproduces. Without that edge two functions handing each other's reference back and forth are a knot the verdict cannot see: `a(p) = p(b)` and `b(q) = q(a)`, called as `a(b)`, oscillate with period four under the sweep — each round devirtualizes to the other, neither ever closes a cycle of `Known` callees, the node count never moves, and so no size limit ever applies.
    //
    // The edge is conditional on the naming body *applying* a closure, because that is precisely what the rewrite needs: `map_callee` turns a reference into a call only where a substituted parameter stood in a `CpsCallee::Closure`. A body that merely hands a reference onward — an initializer returning the closure it built, which is the shape beside this one — cannot devirtualize anything, and reading its reference as a call would make the closure it defines recursive on a cycle no inline can travel.
    //
    // The body-only graph and its components stay what specialization and contification read, since to them a nested closure is a function of its own, and folding it into its definer's component would let an escaping closure disqualify a component it merely sits in. That is why the reference edges are seeded into `closed` alone and never into `call_graph`.
    let mut nested_in: BTreeMap<CpsFunId, Vec<CpsFunId>> = BTreeMap::new();
    for (&node_id, &owner) in &analysis.node_owners {
        if let Some(CpsNode::LetFun { functions, .. }) = module.node(node_id) {
            nested_in
                .entry(owner)
                .or_default()
                .extend(functions.iter().copied());
        }
    }
    let mut closed = analysis.call_graph.clone();
    for (owner, referenced) in &named_in {
        if applies_a_closure.contains(owner) {
            closed
                .entry(*owner)
                .or_default()
                .extend(referenced.iter().copied());
        }
    }
    // Nesting is a forest, so this settles in as many rounds as it is deep.
    let mut changed = true;
    while changed {
        changed = false;
        for (owner, nested) in &nested_in {
            for function in nested {
                let Some(edges) = closed.get(function).cloned() else {
                    continue;
                };
                let own = closed.entry(*owner).or_default();
                let before = own.len();
                own.extend(edges);
                changed |= own.len() != before;
            }
        }
    }
    let closed_sccs = analyze_sccs(&closed);
    for (&function, &component) in &closed_sccs.component_of {
        let multi_member = closed_sccs.members[component].len() > 1;
        let self_edge = closed
            .get(&function)
            .is_some_and(|edges| edges.contains(&function));
        if multi_member || self_edge {
            analysis.recursive.insert(function);
        }
    }
    analysis.sccs = analyze_sccs(&analysis.call_graph);

    // Each function's lexical scope, taken from the walk `verify_lexical_scopes` performs rather than restated here.
    //
    // It used to be read off the `LetFun` nodes alone — a member's own group, then everything its owner's group holds, up to the entry — and that misses the third of the three things `scope_step` admits: a group bound *earlier along the same body chain*. Every group the lowering emits is a singleton, so what it missed was precisely a body's siblings, which is nearly every function reference a program makes; the filter reading this then refused to forward any of them.
    analysis.lexical_scope = module.lexical_scopes();

    // A function the walk did not reach keeps the owner-chain answer, which is a subset of what it may name. This runs mid-round, where the module is transiently unscoped by design, so a walk from the entry can miss a live function; and the set is read to *refuse* a forward, so answering with less than the truth costs an optimization where answering with more would forward a reference the body cannot legally name.
    let mut group_of: BTreeMap<CpsFunId, (CpsFunId, Vec<CpsFunId>)> = BTreeMap::new();
    for (&node_id, &owner) in &analysis.node_owners {
        if let Some(CpsNode::LetFun { functions, .. }) = module.node(node_id) {
            for &member in functions {
                group_of.insert(member, (owner, functions.clone()));
            }
        }
    }
    for function in module.functions.live_ids() {
        if analysis.lexical_scope.contains_key(&function) {
            continue;
        }
        let mut scope = BTreeSet::new();
        let mut visited = BTreeSet::new();
        let mut current = function;
        while visited.insert(current) {
            match group_of.get(&current) {
                Some((owner, members)) => {
                    scope.extend(members.iter().copied());
                    current = *owner;
                }
                None => {
                    scope.insert(current);
                    break;
                }
            }
        }
        analysis.lexical_scope.insert(function, scope);
    }
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
            | CpsNode::Panic(_)
            | CpsNode::Unreachable => {}
        }
    }
    found.into_iter().collect()
}
/// The values `function` mentions without binding — what lowering must carry into it.
///
/// **The walk stops at a nested function.** [`function_nodes`] enters a `LetFun`'s body and not its members, so a value referenced only inside a function defined *within* this one is not reported here. That is correct for a caller asking what to carry into a call or a closure — a nested function's own captures are answered when the sweep reaches that function, which is the shape `represent`'s `offers` sweep over every live function relies on. It is wrong for a caller about to *remove* a binding, which must cover the whole region that loses it — a pass once asked this question for that purpose and dropped a binding a nested function still referenced.
///
/// What this is *not* for is admitting a call's inline or contification: a site names its callee, so the callee's `LetFun` encloses the site and every value reported here is bound before it — in scope at the site by construction. Two passes once checked it against the values the owner's body happened to mention, and refused a move whenever the owner did not name the captured binding itself.
pub(super) fn free_values(module: &CpsModule, function: CpsFunId) -> BTreeSet<CpsValueId> {
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

    used.difference(&owned).copied().collect()
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

    // A continuation parameter is known the same way a function's is: every transfer hands it the same literal. Edges carry their arguments; an operation delivering into its `return_to` hands a runtime value, which is the `None` that forces `Conflict`. A join point whose every jump passes one tag — the clone `specialize_jump_patterns` or `split_parameters` leaves once the other tag's jumps are gone — keeps a switch on that parameter and the arm it can never take, and a read in that dead arm of a value minted in the live arm's vocabulary is exactly what `verify_rows` refuses once a later pass substitutes the construction into it.
    let mut continuation_inputs = BTreeMap::<CpsContId, Vec<Knowledge>>::new();
    for (continuation, definition) in module.continuations.iter_live() {
        continuation_inputs.insert(
            continuation,
            vec![Knowledge::Unknown; definition.params.len()],
        );
    }
    for (_, node) in module.nodes.iter_live() {
        let mut edge = |edge: &CpsEdge| {
            if let Some(inputs) = continuation_inputs.get_mut(&edge.target) {
                merge_inputs(inputs, Some(&edge.args));
            }
        };
        match node {
            CpsNode::ApplyCont(target) => edge(target),
            CpsNode::Switch { cases, default, .. } => {
                cases.values().chain(default.iter()).for_each(edge);
            }
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                if let Some(inputs) = continuation_inputs.get_mut(return_to) {
                    merge_inputs(inputs, None);
                }
            }
            CpsNode::LetValue { .. }
            | CpsNode::LetIntrinsic { .. }
            | CpsNode::LetFun { .. }
            | CpsNode::LetCont { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Panic(_)
            | CpsNode::Unreachable => {}
        }
    }
    for (continuation, inputs) in continuation_inputs {
        let params = &module.continuation(continuation).unwrap().params;
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
