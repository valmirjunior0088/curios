use {
    super::*,
    super::{
        analysis::{CallAnalysis, analyze_calls, function_nodes, nodes_from, resolve_atom},
        clone::{copied_extent, copy_bodies},
        inline::continuation_transfers,
        optimize::{BRANCH_SPECIALIZATION_GROWTH_LIMIT, SCC_CLONE_NODE_LIMIT},
    },
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Clone, PartialEq)]
pub(super) enum Knowledge {
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

/// Ordered `Unknown < Known(_) < Conflict`.
///
/// Note how this differs from [`Knowledge::merge`], which is *not* this join and must not be confused with it: `merge` reads its `None` as "a caller I cannot observe" and lets that force a `Conflict`, whereas here `Unknown` is the identity, so a forwarded parameter still resolving to `Unknown` contributes nothing rather than poisoning the result. The two disagree on exactly one case — `merge` leaves `(Unknown, None)` at `Unknown` — and that case is why the observation step and the fixpoint cannot share one operation.
impl Lattice for Knowledge {
    fn bottom() -> Self {
        Knowledge::Unknown
    }

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
/// Compute the parameters of recursive SCC members that are provably a single literal or function reference at every entry, so they can be substituted in place and dropped as dead. This is a monotone constant-propagation fixpoint over the whole known-callee call graph, restricted to literal/function atoms with parameter forwarding, ordered `Unknown < Known < Conflict`.
///
/// Only members of an eligible SCC participate: the SCC must be recursive and must contain no escaping member and not the program entry, because an escaping or host-called function receives arguments this analysis cannot observe. `known_literals` seeds resolution of caller values already known to be constant.
pub(super) fn scc_invariant_knowns(
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
    for (_, node) in module.nodes.iter_live() {
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
/// The members of every SCC eligible for known-argument analysis: recursive, and containing neither an escaping member nor the program entry, because those receive arguments the analysis cannot observe.
pub(super) fn eligible_sccs(module: &CpsModule, analysis: &CallAnalysis) -> Vec<Vec<CpsFunId>> {
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
/// Run the monotone `Unknown < Known < Conflict` join to a fixpoint over the given parameter positions and call constraints.
pub(super) fn invariant_fixpoint(
    params_of: &BTreeMap<CpsFunId, Vec<CpsValueId>>,
    constraints: &[(CpsFunId, Vec<CpsAtom>)],
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> BTreeMap<CpsValueId, Knowledge> {
    Solver::solve(params_of.values().flatten().copied(), |solver| {
        for (callee, args) in constraints {
            let Some(params) = params_of.get(callee) else {
                continue;
            };
            for (index, arg) in args.iter().enumerate() {
                let Some(&param) = params.get(index) else {
                    continue;
                };
                let incoming = resolve_atom(arg, solver.facts(), known_literals);
                solver.join(param, incoming);
            }
        }
    })
}
/// Extract the parameters resolved to a single literal or function reference.
pub(super) fn useful_knowns(
    class: BTreeMap<CpsValueId, Knowledge>,
) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut result = BTreeMap::new();
    for (param, knowledge) in class {
        if let Knowledge::Known(atom @ (CpsAtom::Literal(_) | CpsAtom::Fun(_))) = knowledge {
            result.insert(param, atom);
        }
    }
    result
}
/// Specialize a recursive SCC for one external call context whose known arguments the module-wide analysis cannot use because other callers disagree.
///
/// The SCC is cloned verbatim and the disagreeing call site (with any siblings passing the same arguments) is repointed to the private copy. The clone then has a single agreeing external caller, so the ordinary invariant-known propagation folds those arguments in place on a later iteration while the original stays polymorphic for its other callers. At most `SCC_CLONE_LIMIT` clones are made per module and only SCCs within `SCC_CLONE_NODE_LIMIT` live nodes are cloned. One clone is performed per call so the outer fixpoint stays deterministic.
pub(super) fn specialize_scc_calls(module: &mut CpsModule, budget: &mut usize) -> bool {
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

        // Find the first external context that unlocks a known the module-wide analysis could not, in deterministic call-site order.
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

        let clones = clone_scc(module, &member_set);
        let clone_entry = clones[&entry];
        // Only the requested members are bound here. A copy of a *nested* definition is introduced by the copied `LetFun` inside its own member's body, so binding it out here as well would bind it twice.
        if let Some(CpsNode::LetFun { functions, .. }) = module.nodes.get_mut(intro) {
            functions.extend(member_set.iter().filter_map(|m| clones.get(m).copied()));
        }
        for (node_id, callee, args) in &external {
            if *callee == entry
                && *args == context_args
                && let Some(CpsNode::ApplyFun { callee, .. }) = module.nodes.get_mut(*node_id)
            {
                *callee = CpsCallee::Known(clone_entry);
            }
        }
        *budget -= 1;
        return true;
    }
    false
}
/// SpecConstr-style call-pattern specialization. When a known-callee call passes a statically-known tagged tuple into a parameter the callee deconstructs, clone the callee with that constructor rebuilt at its entry so the existing aggregate-projection and known-switch simplifications collapse the deconstruction on a later iteration. The constructor's dynamic fields are threaded as fresh parameters (a worker/wrapper rebuild) and the clone's recursive self-calls fall back to the general function, so it peels the one matched level rather than assuming the recursion stays in pattern. Every call sharing the `(callee, index, tag, arity)` pattern repoints to the single clone, so equivalent sites specialize once. Bounded by `BRANCH_SPECIALIZATION_GROWTH_LIMIT` cloned live nodes and the module-wide clone-count `budget`.
pub(super) fn specialize_call_patterns(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let constructors = tagged_tuple_values(module);
    if constructors.is_empty() {
        return false;
    }

    // The first specializable pattern in deterministic (node, then argument) order: a known-callee call whose argument is a known tagged tuple that the callee deconstructs, whose callee has a lexical `LetFun` owner and a clonable body within the growth budget.
    let mut chosen: Option<(CpsFunId, usize, u32, usize)> = None;
    'search: for (_, node) in module.nodes.iter_live() {
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
            // The extent rather than the body: a definition nested in the callee is copied along with it, and counting only the outer body would price the clone at a fraction of what it duplicates.
            let (extent, _) = copied_extent(module, function_nodes(module, *callee));
            if extent.len() + 1 > BRANCH_SPECIALIZATION_GROWTH_LIMIT {
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
    let clones = clone_scc(module, &member);
    let clone = clones[&callee];

    // Peel: the clone recurses into the general function, not itself, so a recursive call that does not carry the matched constructor stays valid.
    //
    // Over every copied function rather than the entry alone. The entry's parameters are about to be respliced, so a call to it that kept the old argument list would not merely be unpeeled — it would carry the wrong arity, and a definition copied from inside the entry's own body is exactly where such a call can hide.
    let peeled = clones
        .values()
        .flat_map(|&id| function_nodes(module, id))
        .collect::<Vec<_>>();
    for node_id in peeled {
        let node = module.nodes.get_mut(node_id).unwrap();
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

    // Rebuild the constructor at the clone entry, threading its dynamic fields as fresh parameters in place of the specialized parameter.
    let clone_function = module.function(clone).unwrap();
    let mut params = clone_function.params.clone();
    let clone_body = clone_function.body;
    let old_param = params[index];
    let field_params: Vec<CpsValueId> = (1..arity)
        .map(|field| module.add_value(Some(format!("field#{field}"))))
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
    let clone_function = module.functions.get_mut(clone).unwrap();
    clone_function.params = params;
    clone_function.body = entry;

    // Introduce the clone in the callee's lexical scope.
    if let Some(CpsNode::LetFun { functions, .. }) = module.nodes.get_mut(intro) {
        functions.push(clone);
    }

    // Repoint every call sharing the pattern to the single clone, splicing each site's own constructor fields in place of the tuple argument.
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
        }) = module.nodes.get_mut(CpsNodeId(node_id as u32))
        else {
            unreachable!()
        };
        *target = CpsCallee::Known(clone);
        args.splice(index..=index, spliced);
    }

    *budget -= 1;
    true
}
/// The `LetValue`-bound tagged tuples: values whose defining expression is a tuple whose first field is a `Nat` literal tag. These are the constructor call patterns branch specialization can bake into a callee.
pub(super) fn tagged_tuple_values(module: &CpsModule) -> BTreeMap<CpsValueId, (u32, Vec<CpsAtom>)> {
    let mut result = BTreeMap::new();
    for (_, node) in module.nodes.iter_live() {
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
/// Whether `function` projects a field out of `param`, i.e. contains a `TupleGet` on it. This is the profitability gate: baking a known tuple into a parameter only pays off when the body actually deconstructs it.
pub(super) fn deconstructs_param(
    module: &CpsModule,
    function: CpsFunId,
    param: CpsValueId,
) -> bool {
    function_nodes(module, function).iter().any(|&id| {
        matches!(
            module.node(id),
            Some(CpsNode::LetIntrinsic {
                op: CpsIntrinsic::TupleGet(_),
                args,
                ..
            }) if args.first() == Some(&CpsAtom::Value(param))
        )
    })
}
/// The literal results of `LetValue` bindings, used to resolve caller values already known to be constant.
pub(super) fn literal_value_map(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut literals = BTreeMap::new();
    for (_, node) in module.nodes.iter_live() {
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
/// The single `LetFun` node introducing every member, or `None` if the members are split across nodes or introduced by a `RecInit` knot. The clones are added to this node so they share the members' lexical scope.
pub(super) fn introducing_letfun(
    module: &CpsModule,
    members: &BTreeSet<CpsFunId>,
) -> Option<CpsNodeId> {
    for (id, node) in module.nodes.iter_live() {
        if let CpsNode::LetFun { functions, .. } = node {
            let introduced: BTreeSet<CpsFunId> = functions.iter().copied().collect();
            if members.is_subset(&introduced) {
                return Some(id);
            }
        }
    }
    None
}
/// Copy every member of an SCC, and every definition nested inside them, into fresh functions with fresh return continuations, local continuations, owned values, and nodes. Internal known-callee edges and return continuations are rewired to the copies while free values, external callees, and external continuations are shared.
pub(super) fn clone_scc(
    module: &mut CpsModule,
    members: &BTreeSet<CpsFunId>,
) -> BTreeMap<CpsFunId, CpsFunId> {
    copy_bodies(module, members, &BTreeSet::new()).functions
}
/// SpecConstr for continuation joins — the join-point analogue of [`specialize_call_patterns`]. When an edge jumps a statically-known tagged tuple into a multi-transfer continuation that deconstructs it, clone the continuation with the constructor rebuilt at its entry and its dynamic fields threaded as parameters, so the existing aggregate-projection and known-switch simplifications collapse the deconstruction — and usually the allocation and the branch — on a later iteration. Every edge sharing the `(target, index, tag, arity)` pattern repoints to the single clone. Single-transfer joins are excluded: `inline_single_use_continuations` already collapses them outright. Bounded by `BRANCH_SPECIALIZATION_GROWTH_LIMIT` cloned live nodes and the module-wide clone-count `budget`.
pub(super) fn specialize_jump_patterns(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let constructors = tagged_tuple_values(module);
    if constructors.is_empty() {
        return false;
    }
    let transfers = continuation_transfers(module);

    // The first specializable pattern in deterministic (node, then edge, then argument) order.
    let mut chosen: Option<(CpsContId, usize, u32, usize)> = None;
    'search: for (_, node) in module.nodes.iter_live() {
        let edges: Vec<&CpsEdge> = match node {
            CpsNode::ApplyCont(edge) => vec![edge],
            CpsNode::Switch { cases, default, .. } => {
                cases.values().chain(default.iter()).collect()
            }
            _ => continue,
        };
        for edge in edges {
            let Some(continuation) = module.continuation(edge.target) else {
                continue;
            };
            if transfers
                .get(&edge.target)
                .is_none_or(|sites| sites.len() < 2)
            {
                continue;
            }
            for (index, arg) in edge.args.iter().enumerate() {
                let CpsAtom::Value(value) = arg else { continue };
                let Some((tag, fields)) = constructors.get(value) else {
                    continue;
                };
                let Some(&param) = continuation.params.get(index) else {
                    continue;
                };
                if !continuation_projects(module, edge.target, param) {
                    continue;
                }
                if introducing_letcont(module, edge.target).is_none() {
                    continue;
                }
                // The extent rather than the body, for the reason the call specializer counts one: a definition nested in the join is copied with it.
                let (extent, _) = copied_extent(module, nodes_from(module, continuation.body));
                if extent.len() + 1 > BRANCH_SPECIALIZATION_GROWTH_LIMIT {
                    continue;
                }
                chosen = Some((edge.target, index, *tag, fields.len()));
                break 'search;
            }
        }
    }
    let Some((target, index, tag, arity)) = chosen else {
        return false;
    };

    let intro = introducing_letcont(module, target).unwrap();
    let clone = clone_continuation(module, target);

    // Rebuild the constructor at the clone entry, threading its dynamic fields as fresh parameters in place of the specialized parameter.
    let clone_definition = module.continuation(clone).unwrap();
    let mut params = clone_definition.params.clone();
    let clone_body = clone_definition.body;
    let old_param = params[index];
    let field_params: Vec<CpsValueId> = (1..arity)
        .map(|field| module.add_value(Some(format!("field#{field}"))))
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
    let clone_definition = module.continuations.get_mut(clone).unwrap();
    clone_definition.params = params;
    clone_definition.body = entry;

    // Introduce the clone beside the original, so it shares the original's lexical scope.
    if let Some(CpsNode::LetCont { continuations, .. }) = module.nodes.get_mut(intro) {
        continuations.push(clone);
    }

    // Repoint every edge sharing the pattern, splicing each edge's own constructor fields in place of the tuple argument.
    let repoint =
        |edge: &mut CpsEdge, constructors: &BTreeMap<CpsValueId, (u32, Vec<CpsAtom>)>| -> bool {
            if edge.target != target {
                return false;
            }
            let Some(CpsAtom::Value(value)) = edge.args.get(index) else {
                return false;
            };
            let Some((site_tag, site_fields)) = constructors.get(value) else {
                return false;
            };
            if *site_tag != tag || site_fields.len() != arity {
                return false;
            }
            let spliced = site_fields[1..].to_vec();
            edge.target = clone;
            edge.args.splice(index..=index, spliced);
            true
        };
    for node_index in 0..module.nodes.len() {
        let node_id = CpsNodeId(node_index as u32);
        let Some(node) = module.nodes.get_mut(node_id) else {
            continue;
        };
        match node {
            CpsNode::ApplyCont(edge) => {
                repoint(edge, &constructors);
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    repoint(edge, &constructors);
                }
            }
            _ => {}
        }
    }

    *budget -= 1;
    true
}
/// Whether `continuation`'s body projects a field out of `param` — the profitability gate matching [`deconstructs_param`], over a continuation body.
pub(super) fn continuation_projects(
    module: &CpsModule,
    continuation: CpsContId,
    param: CpsValueId,
) -> bool {
    nodes_from(module, module.continuation(continuation).unwrap().body)
        .iter()
        .any(|&id| {
            matches!(
                module.node(id),
                Some(CpsNode::LetIntrinsic {
                    op: CpsIntrinsic::TupleGet(_),
                    args,
                    ..
                }) if args.first() == Some(&CpsAtom::Value(param))
            )
        })
}
/// The `LetCont` node introducing `continuation`. Every live local continuation has exactly one (the verifier's lexical-binding check), so `None` only means the module is mid-rewrite.
pub(super) fn introducing_letcont(
    module: &CpsModule,
    continuation: CpsContId,
) -> Option<CpsNodeId> {
    module.nodes.iter_live().find_map(|(id, node)| {
        matches!(
            node,
            CpsNode::LetCont { continuations, .. } if continuations.contains(&continuation)
        )
        .then_some(id)
    })
}
/// Copy one continuation's body subtree, and every definition nested inside it, into a fresh continuation with fresh parameters, owned values, nested continuations, and nodes. External values, functions, and continuations — including the owning function's return — are shared.
pub(super) fn clone_continuation(module: &mut CpsModule, target: CpsContId) -> CpsContId {
    copy_bodies(module, &BTreeSet::new(), &BTreeSet::from([target])).continuations[&target]
}
pub(super) fn merge_inputs(inputs: &mut [Knowledge], args: Option<&[CpsAtom]>) {
    for (index, input) in inputs.iter_mut().enumerate() {
        input.merge(args.and_then(|args| args.get(index)));
    }
}
pub(super) fn record_known_literals(
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
