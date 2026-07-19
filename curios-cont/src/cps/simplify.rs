use {
    super::*,
    super::{analysis::free_values, evaluate::evaluate},
    std::collections::{BTreeMap, BTreeSet},
};

/// Dissolve a `RecInit` knot into an ordinary `LetFun` once optimization has
/// severed the function-to-value dependency. `RecInit` additionally binds its
/// computed values so escaping closures may forward-reference them and emits a
/// fallback shell for each escaping member that captures one; when no member
/// still captures a computed value, that binding and those shells are
/// unnecessary and the node is an ordinary recursive function group. The
/// stronger "captures nothing computed" test (rather than merely "escapes
/// nothing") also keeps every computed value in lexical scope after the rewrite.
pub(super) fn dissolve_rec_init(module: &mut CpsModule) -> bool {
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
pub(super) fn rewrite_atoms(module: &mut CpsModule, known: &BTreeMap<CpsValueId, CpsAtom>) -> bool {
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
pub(super) fn forward_continuations(module: &mut CpsModule) -> bool {
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
pub(super) fn thread_edge(
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
pub(super) fn retarget(
    target: &mut CpsContId,
    resolve: &impl Fn(CpsContId) -> CpsContId,
    changed: &mut bool,
) {
    let replacement = resolve(*target);
    if replacement != *target {
        *target = replacement;
        *changed = true;
    }
}
pub(super) fn simplify_nodes(module: &mut CpsModule) -> bool {
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
pub(super) fn forward_aggregate_projections(module: &mut CpsModule) -> bool {
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
pub(super) fn eliminate_dead_bindings(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Remove dead bindings in sweeps: count value uses once, collect every binding
    // the snapshot proves dead, and splice them all out in a single chain-resolving
    // pass rather than recomputing the counts and rewiring the whole module for one
    // removal at a time. Removing a binding only ever lowers another value's use
    // count, so a value dead in the snapshot stays dead; a binding that a removal
    // newly exposes is collected by the next sweep.
    loop {
        let counts = module.value_use_counts();
        let mut redirect = BTreeMap::<CpsNodeId, CpsNodeId>::new();
        let mut dead_values = Vec::<CpsValueId>::new();
        for (index, node) in module.nodes.iter().enumerate() {
            let id = CpsNodeId(index as u32);
            let removal = match node.as_ref() {
                Some(CpsNode::LetValue { result, next, .. })
                    if counts.get(result).copied().unwrap_or(0) == 0 =>
                {
                    Some((*next, Some(*result)))
                }
                Some(CpsNode::LetPrim {
                    result, op, next, ..
                }) if op.is_total() && counts.get(result).copied().unwrap_or(0) == 0 => {
                    Some((*next, Some(*result)))
                }
                Some(CpsNode::LetFun { functions, body }) if functions.is_empty() => {
                    Some((*body, None))
                }
                Some(CpsNode::LetCont {
                    continuations,
                    body,
                }) if continuations.is_empty() => Some((*body, None)),
                _ => None,
            };
            if let Some((successor, value)) = removal {
                redirect.insert(id, successor);
                if let Some(value) = value {
                    dead_values.push(value);
                }
            }
        }
        if redirect.is_empty() {
            break;
        }
        splice_dead_nodes(module, &redirect);
        for &node in redirect.keys() {
            module.nodes[node.index()] = None;
        }
        for value in dead_values {
            module.values[value.index()] = None;
        }
        changed = true;
    }
    changed
}

/// Redirect every control edge that targets a spliced-out node to the first
/// surviving node in its chain. `redirect` maps each removed node to its immediate
/// successor; following the chain skips runs of consecutive removed nodes, so the
/// result is the same as rewiring one node at a time.
fn splice_dead_nodes(module: &mut CpsModule, redirect: &BTreeMap<CpsNodeId, CpsNodeId>) {
    for function in module.functions.iter_mut().flatten() {
        function.body = resolve_redirect(redirect, function.body);
    }
    for continuation in module.continuations.iter_mut().flatten() {
        continuation.body = resolve_redirect(redirect, continuation.body);
    }
    for node in module.nodes.iter_mut().flatten() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => {
                *next = resolve_redirect(redirect, *next);
            }
            CpsNode::LetFun { body, .. } | CpsNode::LetCont { body, .. } => {
                *body = resolve_redirect(redirect, *body);
            }
            CpsNode::RecInit { ready, body, .. } => {
                *ready = resolve_redirect(redirect, *ready);
                *body = resolve_redirect(redirect, *body);
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

fn resolve_redirect(redirect: &BTreeMap<CpsNodeId, CpsNodeId>, mut id: CpsNodeId) -> CpsNodeId {
    while let Some(&next) = redirect.get(&id) {
        id = next;
    }
    id
}
pub(super) fn rewire_node(module: &mut CpsModule, from: CpsNodeId, to: CpsNodeId) {
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
pub(super) fn eliminate_dead_parameters(module: &mut CpsModule) -> bool {
    let counts = module.value_use_counts();
    // Precompute the continuations used as a return target in one pass, rather than
    // rescanning every node for each continuation.
    let return_targets = module
        .nodes
        .iter()
        .flatten()
        .filter_map(|node| match node {
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => Some(*return_to),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let mut continuation = None;
    for (index, definition) in module.continuations.iter().enumerate() {
        let Some(definition) = definition else {
            continue;
        };
        let id = CpsContId(index as u32);
        if return_targets.contains(&id) {
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
pub(super) fn remove_parameter_indices<T>(
    values: &mut Vec<T>,
    removed: &BTreeSet<usize>,
) -> Vec<T> {
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
