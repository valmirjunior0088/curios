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

/// Each function some caller observes, with the width every one of its call sites is admitted at — or `None` where a site refuses.
///
/// The three answers are three different things, and [`uncurry_returns`] needs them apart: `Some(width)` is a function whose every non-tail caller applies what it returns, at that one width; `None` is one with a caller that does something else, which no class-mate's width may overrule; and absence is a function reached only by a class-mate's tail call — *unobserved*, with no caller of its own to disagree, taking its width from the class. Both halves of a verdict are [`admit_site`]'s, per site, and [`rewritable`]'s, per function; nothing here judges a site a second way.
pub(super) fn uncurryable(module: &CpsModule) -> BTreeMap<CpsFunId, Option<usize>> {
    let calls = analyze_calls(module);
    let mut verdicts = BTreeMap::<CpsFunId, Option<usize>>::new();

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
            // A tail call forwards the caller's own return, and is a class edge rather than an observation.
            if *return_to == sentinel {
                continue;
            }
            let observed = admit_site(module, *return_to).map(|site| site.passed.len());
            let verdict = verdicts.entry(*callee).or_insert(observed);
            *verdict = match (*verdict, observed) {
                (Some(seen), Some(width)) if seen == width => Some(width),
                _ => None,
            };
        }
    }

    for (function, verdict) in &mut verdicts {
        if !rewritable(module, &calls, *function) {
            *verdict = None;
        }
    }
    verdicts
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

/// One call site the rewrite can absorb: what the callee takes over, and how the site resumes without the closure.
struct Site {
    passed: Vec<CpsAtom>,
    resume: Resume,
}

/// Judge one call site by the continuation it resumes at: the application the callee would absorb, or `None` where the site cannot be rewritten.
///
/// **Every condition on a site is here, and nowhere else.** Admission ([`uncurryable`]) and planning ([`plan_class`]) each walked the resume with a list of their own, and each list had a clause the other lacked — the plan counted application sites and never asked whether the closure was also kept, so a member the admission had refused was planned on its class-mate's width and rewritten, and the tuple that had held the closure held the applied answer. One judgment consumed twice cannot disagree with itself.
///
/// The conditions: the resume receives the one value the tuple protocol delivers; that value's only use is a single application, which [`sole_application`] establishes over the whole region, nested functions included; the application's arguments exist where the call is rather than being bound inside the resume, or moving the application above the call would move a computation with it; and the application is reached from the resume's head through `LetCont`s alone, which is what [`Resume`] needs. A width of zero is refused because it is a different transform wearing this one's clothes: with no argument to absorb, the closure is a *thunk* and the rewrite would only decide when it runs — which for an `Io` description is the one thing its meaning rests on.
fn admit_site(module: &CpsModule, return_to: CpsContId) -> Option<Site> {
    let resume = module.continuation(return_to)?;
    let [result] = resume.params.as_slice() else {
        return None;
    };
    let (site, passed) = sole_application(module, resume.body, *result)?;
    if passed.is_empty() {
        return None;
    }
    let bound = values_bound_in(module, resume.body);
    if passed
        .iter()
        .any(|atom| matches!(atom, CpsAtom::Value(value) if bound.contains(value)))
    {
        return None;
    }
    if !reached_directly(module, resume.body, site) {
        return None;
    }
    let CpsNode::ApplyFun {
        return_to: after, ..
    } = *module.node(site).unwrap()
    else {
        unreachable!("an application site is an application")
    };
    // Both forms are correct rewrites of this site, and the cheap one is available wherever losing it would cost: a call whose stack depth matters is in tail position, so the application that follows it is too, and a continuation holding nothing but that application is what [`Resume::Retarget`] asks for.
    let resume = if resume.body == site {
        Resume::Retarget(after)
    } else {
        Resume::Jump {
            site,
            result: *result,
            after,
        }
    };
    Some(Site { passed, resume })
}

/// The one application of `value` beneath `body`, with its arguments, when that application is the value's only use — or `None` when any use does anything else: a second application, an ordinary operand, which would dangle once the callee returns the answer instead of the closure, or any mention inside a function defined beneath `body`, which captures the closure and outlives the site.
///
/// The capture rule is what [`free_values`](super::analysis::free_values) warns a pass about to remove a binding of: [`nodes_from`] enters a `LetFun`'s continuation and not its members, so a lambda defined below the application that applied the closure again was invisible here. The site was admitted on its one visible application, the callee absorbed it, and the lambda went on applying what was by then the applied answer — a trap where the program printed a number.
fn sole_application(
    module: &CpsModule,
    body: CpsNodeId,
    value: CpsValueId,
) -> Option<(CpsNodeId, Vec<CpsAtom>)> {
    let mut found = None;
    for node_id in nodes_from(module, body) {
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if matches!(atom, CpsAtom::Value(used) if *used == value) {
                return None;
            }
        }
        if let CpsNode::LetFun { functions, .. } = node
            && functions
                .iter()
                .any(|function| mentioned_in(module, *function, value))
        {
            return None;
        }
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Closure(callee),
            args,
            ..
        } = node
            && *callee == value
        {
            if found.is_some() {
                return None;
            }
            found = Some((node_id, args.clone()));
        }
    }
    found
}

/// Whether `function` mentions `value` anywhere in its region — as an operand, as a closure callee, or inside a function it defines in turn.
fn mentioned_in(module: &CpsModule, function: CpsFunId, value: CpsValueId) -> bool {
    region_nodes(module, module.function(function).unwrap().body)
        .into_iter()
        .any(|node_id| match module.node(node_id).unwrap() {
            CpsNode::ApplyFun {
                callee: CpsCallee::Closure(callee),
                ..
            } if *callee == value => true,
            node => atoms(node)
                .into_iter()
                .any(|atom| matches!(atom, CpsAtom::Value(used) if *used == value)),
        })
}

/// Every node beneath `body`, the bodies of the functions defined beneath it included, transitively — the whole region a site's rewrite reaches, where [`nodes_from`] stops at a nested function.
fn region_nodes(module: &CpsModule, body: CpsNodeId) -> Vec<CpsNodeId> {
    let mut nodes = BTreeSet::new();
    let mut work = vec![body];
    while let Some(body) = work.pop() {
        for node_id in nodes_from(module, body) {
            if nodes.insert(node_id)
                && let CpsNode::LetFun { functions, .. } = module.node(node_id).unwrap()
            {
                work.extend(
                    functions
                        .iter()
                        .filter_map(|function| module.function(*function))
                        .map(|function| function.body),
                );
            }
        }
    }
    nodes.into_iter().collect()
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

/// Absorb the application, for the first class every one of whose sites [`admit_site`] admits.
///
/// Each member takes the applied arguments as extra parameters; each return edge `jump k[Fun(g)]` becomes the tail call `apply Known(g)` on them; and each call site passes them and resumes by whichever [`Resume`] form its shape admits.
pub(super) fn uncurry_returns(module: &mut CpsModule) -> bool {
    let verdicts = uncurryable(module);
    if !verdicts.values().any(Option::is_some) {
        return false;
    }
    let calls = analyze_calls(module);
    // A class that cannot be planned is declined, not fatal: aborting here would leave every later class untried, and one unrewritable site would silently disable the whole pass.
    let Some((members, width, plan)) = tail_classes(module).into_iter().find_map(|members| {
        // Tail-forwarding makes one return stream of the whole class, so a width observed anywhere in it is the width of all of it, and a member with no caller of its own has nothing to disagree with — while a member whose caller refused is refused whatever its class-mates observed. What every member must satisfy is [`rewritable`], which its width says nothing about.
        let mut widths = members.iter().filter_map(|member| verdicts.get(member));
        let width = (*widths.next()?)?;
        widths.all(|other| *other == Some(width)).then_some(())?;
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

    for (node_id, Site { passed, resume }) in plan {
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

/// Every call site the rewrite must change, each with what [`admit_site`] admitted at it, or `None` if any of them refuses.
///
/// Parameters are added to a whole class at once, so a site discovered later to be unrewritable would leave a callee expecting an argument nobody passes. The transform has to be decided before it is begun — and it is decided by the same judgment that observed the widths, so nothing a site refused can be planned on its class-mates' account.
fn plan_class(module: &CpsModule, members: &[CpsFunId]) -> Option<Vec<(CpsNodeId, Site)>> {
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
        if module.continuation(*return_to).is_none() {
            continue;
        }
        plan.push((node_id, admit_site(module, *return_to)?));
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
