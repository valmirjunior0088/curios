//! Absorbing, into a function, the application its returned closure always receives.
//!
//! A monadic carrier makes an action *be* a closure, so a step allocates one and calls it indirectly. Where every use of a returned closure is an immediate application, the callee can take the argument instead and the closure is never built — and this needs no knowledge of *which* closure comes back, which is what makes it smaller than dispatching on the possibilities.

#[cfg(test)]
mod tests;

use {
    super::analysis::{CallAnalysis, analyze_calls, function_nodes, nodes_from},
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

/// A function whose returned closure every caller applies, and the arity it is applied at.
///
/// Absence means one of two different things, and [`uncurry_returns`] needs them apart: a function with non-tail callers that do something else with what it returns is *inadmissible*, while one reached only by a class-mate's tail call is merely *unobserved* — it has no callers of its own to disagree, and takes its width from the class. [`rewritable`] is the half of admissibility that does not depend on having been observed.
pub(super) fn uncurryable(module: &CpsModule) -> BTreeMap<CpsFunId, usize> {
    let calls = analyze_calls(module);
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
            // The application has to be the *only* use, and at one arity. The demand lattice stopped answering that when it went interprocedural: `Applied` there may now be earned by a forwarded application in a later continuation, while this transform moves the application it finds *here* — so the sole-local-application fact is recomputed syntactically where it is spent.
            let Some(width) = locally_applied_at(module, resume.body, *result) else {
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
            // A width of zero is a different transform wearing this one's clothes. There is no argument to absorb, so the closure is a *thunk* and the rewrite only decides when it runs — which for an `Io` description is the one thing its meaning rests on.
            let width = width.filter(|width| *width != usize::MAX && *width > 0)?;
            rewritable(module, &calls, function).then_some((function, width))
        })
        .collect()
}

/// Whether `function` could be rewritten at all, on grounds that have nothing to do with what its callers do.
///
/// Separated out because it has to be asked of every member of a class, including the ones no caller observes. Those take their width from the class rather than from a site of their own, and none of the three facts below is implied by that width: an escaping member reaches its callers through the arity-keyed `clsr/{arity}` supertype, which the rewrite changes; the entry is called by the host; and a member whose return edges do not all carry a function reference has an edge the rewrite cannot turn into a call, which would leave it handing back a raw value where its class-mates hand back a called one.
fn rewritable(module: &CpsModule, calls: &CallAnalysis, function: CpsFunId) -> bool {
    !calls.escaping.contains(&function)
        && module.entry() != Some(function)
        && returns_functions(module, function)
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

/// The one arity every use of `value` beneath `body` applies it at, or `None` when any use does anything else — including appearing as an ordinary operand, which would dangle once the callee returns the answer instead of the closure.
fn locally_applied_at(module: &CpsModule, body: CpsNodeId, value: CpsValueId) -> Option<usize> {
    let mut width = None;
    for node_id in nodes_from(module, body) {
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if matches!(atom, CpsAtom::Value(used) if *used == value) {
                return None;
            }
        }
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Closure(callee),
            args,
            ..
        } = node
            && *callee == value
        {
            match width {
                None => width = Some(args.len()),
                Some(seen) if seen == args.len() => {}
                Some(_) => return None,
            }
        }
    }
    width
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

/// How a call site resumes once the callee hands back the answer instead of the closure that produced it.
enum Resume {
    /// Resume where the *application* did, bypassing the continuation that received the closure.
    ///
    /// Admissible only when that continuation's body is the application and nothing else, which is what makes the bypass total rather than a skipped computation — and which also proves the target is in scope at the call: with no `LetCont` of its own to introduce it, the target was already bound where the receiving continuation was.
    Retarget(CpsContId),
    /// Keep that continuation, and turn the application inside it into a jump carrying what the callee now returns directly.
    ///
    /// Always well-formed, because nothing moves. It costs one live frame per call, which matters only in a loop — and in a loop the application is itself in tail position, so its target is the caller's own sentinel and [`Resume::Retarget`] takes the site instead.
    Jump {
        site: CpsNodeId,
        result: CpsValueId,
        after: CpsContId,
    },
}

/// Absorb the application, for every class [`uncurryable`] admits.
///
/// Each member takes the applied arguments as extra parameters; each return edge `jump k[Fun(g)]` becomes the tail call `apply Known(g)` on them; and each call site passes them and resumes by whichever [`Resume`] form its shape admits.
pub(super) fn uncurry_returns(module: &mut CpsModule) -> bool {
    let widths = uncurryable(module);
    if widths.is_empty() {
        return false;
    }
    let calls = analyze_calls(module);
    // A class that cannot be planned is declined, not fatal: aborting here would leave every later class untried, and one unrewritable site would silently disable the whole pass.
    let Some((members, width, plan)) = tail_classes(module).into_iter().find_map(|members| {
        // Tail-forwarding makes one return stream of the whole class, so a width observed anywhere in it is the width of all of it — and a member with no caller of its own has nothing to disagree with. What every member must satisfy is [`rewritable`], which its width says nothing about.
        let mut observed = members.iter().filter_map(|member| widths.get(member));
        let width = *observed.next()?;
        observed.all(|other| *other == width).then_some(())?;
        members
            .iter()
            .all(|member| rewritable(module, &calls, *member))
            .then_some(())?;
        let plan = plan_class(module, &members)?;
        Some((members, width, plan))
    }) else {
        return false;
    };

    let mut extra = BTreeMap::new();
    for &member in &members {
        let params = (0..width)
            .map(|index| module.add_value(Some(format!("applied/{}/{index}", member.index()))))
            .collect::<Vec<_>>();
        module
            .functions
            .get_mut(member)
            .unwrap()
            .params
            .extend(&params);
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
            // A member that returns by tail-calling another forwards what it was given, or the callee gains a parameter nothing passes.
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
            // A return edge hands back a function this class no longer builds a closure for; calling it here is what absorbs the application.
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

    for (node_id, passed, resume) in plan {
        let Some(CpsNode::ApplyFun {
            callee,
            args,
            return_to,
        }) = module.node(node_id)
        else {
            unreachable!("a planned site is a call")
        };
        let (callee, mut args, return_to) = (callee.clone(), args.clone(), *return_to);
        args.extend(passed);
        let return_to = match resume {
            Resume::Retarget(after) => after,
            Resume::Jump {
                site,
                result,
                after,
            } => {
                module.nodes.set(
                    site,
                    CpsNode::ApplyCont(CpsEdge {
                        target: after,
                        args: vec![CpsAtom::Value(result)],
                    }),
                );
                return_to
            }
        };
        module.nodes.set(
            node_id,
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            },
        );
    }
    true
}

/// Every call site the rewrite must change, or `None` if any of them cannot be.
///
/// Parameters are added to a whole class at once, so a site discovered later to be unrewritable would leave a callee expecting an argument nobody passes. The transform has to be decided before it is begun.
fn plan_class(
    module: &CpsModule,
    members: &[CpsFunId],
) -> Option<Vec<(CpsNodeId, Vec<CpsAtom>, Resume)>> {
    let mut plan = Vec::new();
    for node_id in module.nodes.live_ids().collect::<Vec<_>>() {
        let Some(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            return_to,
            ..
        }) = module.node(node_id)
        else {
            continue;
        };
        if !members.contains(callee) {
            continue;
        }
        // A tail call between members forwards the caller's own parameters, and is rewritten with the member rather than here.
        let Some(resume) = module.continuation(*return_to) else {
            continue;
        };
        let [result] = resume.params.as_slice() else {
            return None;
        };
        let applications = application_sites(module, resume.body, *result);
        let [(site, passed)] = applications.as_slice() else {
            return None;
        };
        let CpsNode::ApplyFun {
            return_to: after, ..
        } = *module.node(*site).unwrap()
        else {
            unreachable!("an application site is an application")
        };
        if !reached_directly(module, resume.body, *site) {
            return None;
        }
        // Both forms are correct rewrites of this site, and the cheap one is available wherever losing it would cost: a call whose stack depth matters is in tail position, so the application that follows it is too, and a continuation holding nothing but that application is what [`Resume::Retarget`] asks for.
        plan.push(match resume.body == *site {
            true => (node_id, passed.clone(), Resume::Retarget(after)),
            false => (
                node_id,
                passed.clone(),
                Resume::Jump {
                    site: *site,
                    result: *result,
                    after,
                },
            ),
        });
    }
    Some(plan)
}

/// Whether `site` is reached from `body` by binding continuations and doing nothing else.
///
/// [`Resume::Jump`] leaves the application where it stands but has the callee perform it before returning, so whatever the receiving continuation evaluates *ahead* of the application would move behind it. A `LetCont` introduces names without evaluating anything, which is why the shape that motivates the form — a join point bound for the application's own result — is admissible where a preceding `let` is not. It also rules out an application reached through a branch or a loop: one syntactic site inside a loop is many forcings, and the rewrite would leave one.
fn reached_directly(module: &CpsModule, body: CpsNodeId, site: CpsNodeId) -> bool {
    let mut node = body;
    loop {
        if node == site {
            return true;
        }
        match module.node(node) {
            Some(CpsNode::LetCont { body, .. }) => node = *body,
            _ => return false,
        }
    }
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
