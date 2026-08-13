//! Absorbing, into a function, the application its returned closure always receives.
//!
//! A monadic carrier makes an action *be* a closure, so a step allocates one and calls it indirectly. Where every use of a returned closure is an immediate application, the callee can take the argument instead and the closure is never built — and this needs no knowledge of *which* closure comes back, which is what makes it smaller than dispatching on the possibilities.

use {
    super::analysis::{analyze_calls, function_nodes, nodes_from},
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

/// A function whose returned closure every caller applies, and the arity it is applied at.
pub(super) fn uncurryable(module: &CpsModule) -> BTreeMap<CpsFunId, usize> {
    let calls = analyze_calls(module);
    let demands = demands(module);
    let mut arity = BTreeMap::<CpsFunId, Option<usize>>::new();

    for owner in module.functions.live_ids().collect::<Vec<_>>() {
        let sentinel = module.function(owner).unwrap().return_cont;
        for node_id in function_nodes(module, owner) {
            let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                return_to,
                ..
            }) = module.node(node_id)
            else {
                continue;
            };
            if *return_to == sentinel {
                continue;
            }
            let entry = arity.entry(*callee).or_insert(Some(usize::MAX));
            let Some(resume) = module.continuation(*return_to) else {
                *entry = None;
                continue;
            };
            let [result] = resume.params.as_slice() else {
                *entry = None;
                continue;
            };
            // The application has to be the *only* use, and at one arity — which is exactly what the lattice point says.
            let Demand::Applied(width) = demand_of(&demands, *result) else {
                *entry = None;
                continue;
            };
            // And its arguments must already exist where the call is, not be computed inside the continuation that receives the closure — otherwise moving the application above the call moves a computation with it.
            let bound = values_bound_in(module, resume.body);
            let escapes_scope = applied_arguments(module, resume.body, *result)
                .iter()
                .any(|atom| matches!(atom, CpsAtom::Value(value) if bound.contains(value)));
            if escapes_scope {
                *entry = None;
                continue;
            }
            *entry = match *entry {
                Some(usize::MAX) => Some(width),
                Some(seen) if seen == width => Some(width),
                _ => None,
            };
        }
    }

    arity
        .into_iter()
        .filter_map(|(function, width)| {
            let width = width.filter(|width| *width != usize::MAX)?;
            let admissible = !calls.escaping.contains(&function)
                && module.entry() != Some(function)
                && returns_functions(module, function);
            admissible.then_some((function, width))
        })
        .collect()
}

/// Whether every edge `function` returns on carries a function reference — what the rewrite turns into a tail call.
///
/// Vacuously true for a function with no return edge at all, which returns only by tail-forwarding. Such a member has nothing of its own to rewrite and takes its whole answer from the class it forwards into.
fn returns_functions(module: &CpsModule, function: CpsFunId) -> bool {
    let sentinel = module.function(function).unwrap().return_cont;
    for node_id in function_nodes(module, function) {
        let edges: Vec<&CpsEdge> = match module.node(node_id).unwrap() {
            CpsNode::ApplyCont(edge) => vec![edge],
            CpsNode::Switch { cases, default, .. } => {
                cases.values().chain(default.as_ref()).collect()
            }
            _ => continue,
        };
        for edge in edges.into_iter().filter(|edge| edge.target == sentinel) {
            if !matches!(edge.args.as_slice(), [CpsAtom::Fun(_)]) {
                return false;
            }
        }
    }
    true
}

/// The values a continuation's own body binds — what the call site above it cannot see.
fn values_bound_in(module: &CpsModule, body: CpsNodeId) -> BTreeSet<CpsValueId> {
    let mut bound = BTreeSet::new();
    for node_id in nodes_from(module, body) {
        match module.node(node_id).unwrap() {
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
                bound.insert(*result);
            }
            CpsNode::LetCont { continuations, .. } => {
                for continuation in continuations {
                    if let Some(definition) = module.continuation(*continuation) {
                        bound.extend(definition.params.iter().copied());
                    }
                }
            }
            _ => {}
        }
    }
    bound
}

/// The arguments the application of `callee` passes, wherever it occurs beneath `body`.
fn applied_arguments(module: &CpsModule, body: CpsNodeId, callee: CpsValueId) -> Vec<CpsAtom> {
    let mut found = Vec::new();
    for node_id in nodes_from(module, body) {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Closure(value),
            args,
            ..
        } = module.node(node_id).unwrap()
            && *value == callee
        {
            found.extend(args.iter().cloned());
        }
    }
    found
}

/// Absorb the application, for every class [`uncurryable`] admits.
///
/// Each member takes the applied arguments as extra parameters; each return edge `jump k[Fun(g)]` becomes the tail call `apply Known(g)` on them; each call site passes them; and the application that used to consume the returned closure becomes a jump carrying the result the callee now hands back directly. The continuation that received the closure is *kept* rather than bypassed — it may bind the very continuation the application resumes into, and skipping it would unbind that.
pub(super) fn uncurry_returns(module: &mut CpsModule) -> bool {
    let widths = uncurryable(module);
    if widths.is_empty() {
        return false;
    }
    let classes = tail_classes(module);
    let Some((members, width)) = classes.into_iter().find_map(|members| {
        let width = widths.get(members.first()?).copied()?;
        members
            .iter()
            .all(|member| widths.get(member) == Some(&width))
            .then_some((members, width))
    }) else {
        return false;
    };

    // Mint the parameters first: every member takes the same ones positionally, and a tail call between members forwards its own.
    let mut extra = BTreeMap::new();
    for &member in &members {
        let params = (0..width)
            .map(|index| module.add_value(Some(format!("applied/{}/{index}", member.index()))))
            .collect::<Vec<_>>();
        module.functions.get_mut(member).unwrap().params.extend(&params);
        extra.insert(member, params);
    }

    for &member in &members {
        let sentinel = module.function(member).unwrap().return_cont;
        let carried = extra[&member]
            .iter()
            .copied()
            .map(CpsAtom::Value)
            .collect::<Vec<_>>();
        for node_id in function_nodes(module, member) {
            // A return edge hands back a function this class no longer builds a closure for; calling it here is what absorbs the application.
            // A member that returns by tail-calling another member forwards what it was given, or the callee would gain a parameter nothing passes.
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Known(onward),
                args,
                return_to,
            } = module.node(node_id).unwrap()
                && *return_to == sentinel
                && members.contains(onward)
            {
                let (onward, mut args) = (*onward, args.clone());
                args.extend(carried.iter().cloned());
                module.nodes.set(
                    node_id,
                    CpsNode::ApplyFun {
                        callee: CpsCallee::Known(onward),
                        args,
                        return_to: sentinel,
                    },
                );
                continue;
            }
            if let CpsNode::ApplyCont(edge) = module.node(node_id).unwrap()
                && edge.target == sentinel
                && let [CpsAtom::Fun(returned)] = edge.args.as_slice()
            {
                let returned = *returned;
                module.nodes.set(
                    node_id,
                    CpsNode::ApplyFun {
                        callee: CpsCallee::Known(returned),
                        args: carried.clone(),
                        return_to: sentinel,
                    },
                );
            }
        }
    }

    // Every call site: pass what the application passed, and turn that application into a jump carrying the result.
    for node_id in module.nodes.live_ids().collect::<Vec<_>>() {
        let Some(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            return_to,
        }) = module.node(node_id)
        else {
            continue;
        };
        if !members.contains(callee) {
            continue;
        }
        let (callee, mut args, return_to) = (*callee, args.clone(), *return_to);
        let Some(resume) = module.continuation(return_to) else {
            continue;
        };
        let [result] = resume.params.as_slice() else {
            continue;
        };
        let (result, body) = (*result, resume.body);
        let applications = application_sites(module, body, result);
        let Some(passed) = applications.first().map(|(_, passed)| passed.clone()) else {
            continue;
        };
        // A common arity is not a common argument list: two sites passing different values cannot both be answered by one call.
        if applications.iter().any(|(_, other)| *other != passed) {
            continue;
        }
        for (site, _) in applications {
            let CpsNode::ApplyFun { return_to: after, .. } = *module.node(site).unwrap() else {
                unreachable!("an application site is an application")
            };
            module.nodes.set(
                site,
                CpsNode::ApplyCont(CpsEdge {
                    target: after,
                    args: vec![CpsAtom::Value(result)],
                }),
            );
        }
        args.extend(passed);
        module.nodes.set(
            node_id,
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                return_to,
            },
        );
    }
    true
}

/// Where `callee` is applied beneath `body`, and with what.
fn application_sites(
    module: &CpsModule,
    body: CpsNodeId,
    callee: CpsValueId,
) -> Vec<(CpsNodeId, Vec<CpsAtom>)> {
    nodes_from(module, body)
        .into_iter()
        .filter_map(|node_id| match module.node(node_id).unwrap() {
            CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
                args,
                ..
            } if *value == callee => Some((node_id, args.clone())),
            _ => None,
        })
        .collect()
}

/// The undirected connected components of the tail-call graph, which a shared return obliges to decide together.
fn tail_classes(module: &CpsModule) -> Vec<Vec<CpsFunId>> {
    let mut edges = BTreeMap::<CpsFunId, BTreeSet<CpsFunId>>::new();
    for owner in module.functions.live_ids().collect::<Vec<_>>() {
        let sentinel = module.function(owner).unwrap().return_cont;
        edges.entry(owner).or_default();
        for node_id in function_nodes(module, owner) {
            if let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                return_to,
                ..
            }) = module.node(node_id)
                && *return_to == sentinel
            {
                edges.entry(owner).or_default().insert(*callee);
                edges.entry(*callee).or_default().insert(owner);
            }
        }
    }
    let mut seen = BTreeSet::new();
    let mut classes = Vec::new();
    for &root in edges.keys() {
        if !seen.insert(root) {
            continue;
        }
        let (mut members, mut work) = (vec![root], vec![root]);
        while let Some(function) = work.pop() {
            for &next in edges.get(&function).into_iter().flatten() {
                if seen.insert(next) {
                    members.push(next);
                    work.push(next);
                }
            }
        }
        members.sort();
        classes.push(members);
    }
    classes
}
