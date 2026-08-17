use {
    super::*,
    super::{analysis::free_values, evaluate::evaluate},
    std::collections::{BTreeMap, BTreeSet},
};

/// Dissolve a `RecInit` knot into an ordinary `LetFun` once optimization has severed the function-to-value dependency. `RecInit` additionally binds its computed values so escaping closures may forward-reference them and emits a fallback shell for each escaping member that captures one; when no member still captures a computed value, that binding and those shells are unnecessary and the node is an ordinary recursive function group. The stronger "captures nothing computed" test (rather than merely "escapes nothing") also keeps every computed value in lexical scope after the rewrite.
pub(super) fn dissolve_rec_init(module: &mut CpsModule) -> bool {
    let mut selected = None;
    for (id, node) in module.nodes.iter_live() {
        let CpsNode::RecInit {
            functions,
            values,
            body,
            ..
        } = node
        else {
            continue;
        };
        let computed: BTreeSet<CpsValueId> = values.iter().copied().collect();
        let captures = functions
            .iter()
            .any(|function| !free_values(module, *function).is_disjoint(&computed));
        if !captures {
            selected = Some((id, functions.clone(), *body));
            break;
        }
    }
    let Some((node, functions, body)) = selected else {
        return false;
    };
    module.nodes.set(node, CpsNode::LetFun { functions, body });
    true
}
pub(super) fn rewrite_atoms(module: &mut CpsModule, known: &BTreeMap<CpsValueId, CpsAtom>) -> bool {
    let mut changed = false;
    for (_, node) in module.nodes.iter_live_mut() {
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Value(value) = atom
                && let Some(replacement) = known.get(value)
                && atom != replacement
            {
                *atom = replacement.clone();
                changed = true;
            }
        });

        // A closure callee holds its callee in a value, which `visit_atoms_mut` does not reach. Remap it here: a known function devirtualizes the call, and a forwarded value (e.g. a projected constructor field) keeps the callee pointing at a live value rather than a deleted one.
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
        .iter_live()
        .filter_map(|(id, continuation)| {
            let CpsNode::ApplyCont(edge) = module.node(continuation.body)? else {
                return None;
            };
            module.continuation(edge.target)?;
            Some((id, (continuation.params.clone(), edge.clone())))
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
    for (_, node) in module.nodes.iter_live_mut() {
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
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetIntrinsic {
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
/// The two rewrite shapes an identity law produces: forward the surviving operand, or pin the absorbed result as a literal.
enum IdentityFold {
    Operand(CpsAtom),
    Literal(CpsLiteral),
}

/// Match one `Nat`/`Int` identity or absorption law on a binary intrinsic with a literal neutral or absorbing operand: `x + 0`, `x - 0`, `x * 1`, `x * 0`, `x / 1`, `x % 1`, `x & 0`, `x | 0`, `x ^ 0`, and shifts or rotates by zero.
///
/// Trap discipline: `nat_add`/`nat_mul` wrap and `nat_sub` is monus, so the only runtime trap of the `MayTrap` members is the backend's i31 range check on the result. Every fold here returns either an operand that is already a live in-range value or a literal inside the envelope, and a `/ 1` or `% 1` divisor can never be the trapping zero, so no trap is added or dropped. `Flt` deliberately has no laws here: `x + 0.0` is not the identity on `-0.0`.
fn identity_fold(op: CpsIntrinsicOp, args: &[CpsAtom]) -> Option<IdentityFold> {
    let [left, right] = args else { return None };
    let nat = |atom: &CpsAtom| match atom {
        CpsAtom::Literal(CpsLiteral::Nat(value)) => Some(*value),
        _ => None,
    };
    let int = |atom: &CpsAtom| match atom {
        CpsAtom::Literal(CpsLiteral::Int(value)) => Some(*value),
        _ => None,
    };
    let operand = |atom: &CpsAtom| Some(IdentityFold::Operand(atom.clone()));

    match op {
        CpsIntrinsicOp::NatAdd | CpsIntrinsicOp::NatOr | CpsIntrinsicOp::NatXor => {
            if nat(right) == Some(0) {
                operand(left)
            } else if nat(left) == Some(0) {
                operand(right)
            } else {
                None
            }
        }
        CpsIntrinsicOp::IntAdd | CpsIntrinsicOp::IntOr | CpsIntrinsicOp::IntXor => {
            if int(right) == Some(0) {
                operand(left)
            } else if int(left) == Some(0) {
                operand(right)
            } else {
                None
            }
        }
        CpsIntrinsicOp::NatSub
        | CpsIntrinsicOp::NatShl
        | CpsIntrinsicOp::NatShr
        | CpsIntrinsicOp::NatRotl
        | CpsIntrinsicOp::NatRotr => (nat(right) == Some(0)).then(|| operand(left)).flatten(),
        CpsIntrinsicOp::IntSub
        | CpsIntrinsicOp::IntShl
        | CpsIntrinsicOp::IntShr
        | CpsIntrinsicOp::IntRotl
        | CpsIntrinsicOp::IntRotr => (int(right) == Some(0)).then(|| operand(left)).flatten(),
        CpsIntrinsicOp::NatMul => {
            if nat(right) == Some(1) {
                operand(left)
            } else if nat(left) == Some(1) {
                operand(right)
            } else if nat(right) == Some(0) || nat(left) == Some(0) {
                Some(IdentityFold::Literal(CpsLiteral::Nat(0)))
            } else {
                None
            }
        }
        CpsIntrinsicOp::IntMul => {
            if int(right) == Some(1) {
                operand(left)
            } else if int(left) == Some(1) {
                operand(right)
            } else if int(right) == Some(0) || int(left) == Some(0) {
                Some(IdentityFold::Literal(CpsLiteral::Int(0)))
            } else {
                None
            }
        }
        CpsIntrinsicOp::NatDiv => (nat(right) == Some(1)).then(|| operand(left)).flatten(),
        CpsIntrinsicOp::IntDiv => (int(right) == Some(1)).then(|| operand(left)).flatten(),
        CpsIntrinsicOp::NatRem => {
            (nat(right) == Some(1)).then_some(IdentityFold::Literal(CpsLiteral::Nat(0)))
        }
        CpsIntrinsicOp::IntRem => {
            (int(right) == Some(1)).then_some(IdentityFold::Literal(CpsLiteral::Int(0)))
        }
        CpsIntrinsicOp::NatAnd => (nat(right) == Some(0) || nat(left) == Some(0))
            .then_some(IdentityFold::Literal(CpsLiteral::Nat(0))),
        CpsIntrinsicOp::IntAnd => (int(right) == Some(0) || int(left) == Some(0))
            .then_some(IdentityFold::Literal(CpsLiteral::Int(0))),
        _ => None,
    }
}

/// Fold intrinsic identity and absorption laws with one literal operand, which all-literal folding (`evaluate`) cannot reach. An operand fold forwards the surviving value and deletes the binding; an absorption fold pins the result as a literal in place.
pub(super) fn fold_intrinsic_identities(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let selected = module.nodes.iter_live().find_map(|(id, node)| {
            let CpsNode::LetIntrinsic {
                result,
                op,
                args,
                next,
            } = node
            else {
                return None;
            };
            let folded = identity_fold(*op, args)?;
            Some((id, *result, *next, folded))
        });
        let Some((node, result, next, folded)) = selected else {
            break;
        };

        match folded {
            IdentityFold::Operand(replacement) => {
                rewrite_atoms(module, &BTreeMap::from([(result, replacement)]));
                rewire_node(module, node, next);
                module.nodes.remove(node);
                module.values.remove(result);
            }
            IdentityFold::Literal(literal) => {
                module.nodes.set(
                    node,
                    CpsNode::LetValue {
                        result,
                        value: CpsValueExpr::Literal(literal),
                        next,
                    },
                );
            }
        }
        changed = true;
    }
    changed
}
pub(super) fn forward_aggregate_projections(module: &mut CpsModule) -> bool {
    let mut changed = false;
    loop {
        let aggregates = module
            .nodes
            .slots()
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
        let selected = module.nodes.iter_live().find_map(|(id, node)| {
            let CpsNode::LetIntrinsic {
                result,
                op: CpsIntrinsicOp::TplGet(field),
                args,
                next,
            } = node
            else {
                return None;
            };
            let [CpsAtom::Value(tuple)] = args.as_slice() else {
                return None;
            };
            let replacement = aggregates.get(tuple)?.get(*field)?.clone();
            Some((id, *result, *next, replacement))
        });
        let Some((node, result, next, replacement)) = selected else {
            break;
        };

        rewrite_atoms(module, &BTreeMap::from([(result, replacement)]));
        rewire_node(module, node, next);
        module.nodes.remove(node);
        module.values.remove(result);
        changed = true;
    }
    changed
}
pub(super) fn eliminate_dead_bindings(module: &mut CpsModule) -> bool {
    let mut changed = false;
    // Remove dead bindings in sweeps: count value uses once, collect every binding the snapshot proves dead, and splice them all out in a single chain-resolving pass rather than recomputing the counts and rewiring the whole module for one removal at a time. Removing a binding only ever lowers another value's use count, so a value dead in the snapshot stays dead; a binding that a removal newly exposes is collected by the next sweep.
    loop {
        let counts = module.value_use_counts();
        let mut redirect = BTreeMap::<CpsNodeId, CpsNodeId>::new();
        let mut dead_values = Vec::<CpsValueId>::new();
        for (id, node) in module.nodes.iter_live() {
            let removal = match node {
                CpsNode::LetValue { result, next, .. }
                    if counts.get(result).copied().unwrap_or(0) == 0 =>
                {
                    Some((*next, Some(*result)))
                }
                CpsNode::LetIntrinsic {
                    result, op, next, ..
                } if op.is_total() && counts.get(result).copied().unwrap_or(0) == 0 => {
                    Some((*next, Some(*result)))
                }
                CpsNode::LetFun { functions, body } if functions.is_empty() => Some((*body, None)),
                CpsNode::LetCont {
                    continuations,
                    body,
                } if continuations.is_empty() => Some((*body, None)),
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
            module.nodes.remove(node);
        }
        for value in dead_values {
            module.values.remove(value);
        }
        changed = true;
    }
    changed
}

/// Redirect every control edge that targets a spliced-out node to the first surviving node in its chain. `redirect` maps each removed node to its immediate successor; following the chain skips runs of consecutive removed nodes, so the result is the same as rewiring one node at a time.
fn splice_dead_nodes(module: &mut CpsModule, redirect: &BTreeMap<CpsNodeId, CpsNodeId>) {
    for (_, function) in module.functions.iter_live_mut() {
        function.body = resolve_redirect(redirect, function.body);
    }
    for (_, continuation) in module.continuations.iter_live_mut() {
        continuation.body = resolve_redirect(redirect, continuation.body);
    }
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
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
    for (_, function) in module.functions.iter_live_mut() {
        if function.body == from {
            function.body = to;
        }
    }
    for (_, continuation) in module.continuations.iter_live_mut() {
        if continuation.body == from {
            continuation.body = to;
        }
    }
    for (_, node) in module.nodes.iter_live_mut() {
        match node {
            CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
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
/// Drop one entity's unread parameters, and the arguments every caller passes into them.
///
/// Deadness is read from [`super::demand`]'s lattice rather than from a use count, which is the same question asked at the bottom point of a richer order — the one whose `Projected` point a return protocol needs. The lattice defers an argument's demand to the receiving parameter, so `Unused` here reaches further than a zero use count: a value threaded only into parameters nobody reads is dead however many edges carry it, and this pass deleting such a chain is the code motion the strengthening was scheduled to cause. The deletion stays well-formed because a parameter is always removed together with the argument every incoming edge passes into it, so no occurrence survives its binding.
pub(super) fn eliminate_dead_parameters(module: &mut CpsModule) -> bool {
    let demands = demands(module);
    let dead_value = |value: &CpsValueId| demand_of(&demands, *value) == Demand::Unused;
    // Precompute the continuations used as a return target in one pass, rather than rescanning every node for each continuation.
    let return_targets = module
        .nodes
        .slots()
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
    for (id, definition) in module.continuations.iter_live() {
        if return_targets.contains(&id) {
            continue;
        }
        let dead = definition
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| dead_value(value).then_some(index))
            .collect::<BTreeSet<_>>();
        if !dead.is_empty() {
            continuation = Some((id, dead));
            break;
        }
    }
    if let Some((continuation, dead)) = continuation {
        let removed = remove_parameter_indices(
            &mut module.continuations.get_mut(continuation).unwrap().params,
            &dead,
        );
        module.remove_params_from_record(continuation, &dead);
        for (_, node) in module.nodes.iter_live_mut() {
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
            module.values.remove(value);
        }
        return true;
    }

    let escaping = module
        .nodes
        .slots()
        .iter()
        .flatten()
        .flat_map(atoms)
        .filter_map(|atom| match atom {
            CpsAtom::Fun(function) => Some(*function),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let mut function = None;
    for (id, definition) in module.functions.iter_live() {
        if escaping.contains(&id) {
            continue;
        }
        let dead = definition
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, value)| dead_value(value).then_some(index))
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
        &mut module.functions.get_mut(function).unwrap().params,
        &dead,
    );
    for (_, node) in module.nodes.iter_live_mut() {
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
        module.values.remove(value);
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
