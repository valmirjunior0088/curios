//! Absorbing, into a function, the application its returned closure always receives.
//!
//! A monadic carrier makes an action *be* a closure, so a step allocates one and calls it indirectly. Where every use of a returned closure is an immediate application, the callee can take the argument instead and the closure is never built — and this needs no knowledge of *which* closure comes back, which is what makes it smaller than dispatching on the possibilities.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, CallAnalysis, Callee, ContinuationId, Edge, FunctionId, Module, Node, NodeId,
        ValueId, analyze_calls, atoms, continuation_entries, function_nodes, nodes_from,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Each function some caller observes, with the width every one of its call sites is admitted at — or `None` where a site refuses.
///
/// The three answers are three different things, and [`uncurry_returns`] needs them apart: `Some(width)` is a function whose every non-tail caller applies what it returns, at that one width; `None` is one with a caller that does something else, which no class-mate's width may overrule; and absence is a function reached only by a class-mate's tail call — *unobserved*, with no caller of its own to disagree, taking its width from the class. Both halves of a verdict are [`admit_site`]'s, per site, and [`rewritable`]'s, per function; nothing here judges a site a second way.
fn uncurryable(
    module: &Module,
    calls: &CallAnalysis,
    observations: &BTreeMap<ContinuationId, Option<Site>>,
) -> BTreeMap<FunctionId, Option<usize>> {
    let mut verdicts = BTreeMap::<FunctionId, Option<usize>>::new();

    for owner in module.function_ids().collect::<Vec<_>>() {
        let sentinel = module.function(owner).unwrap().return_cont;
        for node_id in function_nodes(module, owner) {
            let Some(Node::ApplyFun {
                callee: Callee::Known(callee),
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
            let observed = observations
                .get(return_to)
                .and_then(Option::as_ref)
                .map(|site| site.passed.len());
            let verdict = verdicts.entry(*callee).or_insert(observed);
            *verdict = match (*verdict, observed) {
                (Some(seen), Some(width)) if seen == width => Some(width),
                _ => None,
            };
        }
    }

    for (function, verdict) in &mut verdicts {
        if !rewritable(module, calls, *function) {
            *verdict = None;
        }
    }
    verdicts
}

/// Whether `function` could be rewritten at all, on grounds that have nothing to do with what its callers do.
///
/// Separated out because it has to be asked of every member of a class, including the ones no caller observes. Those take their width from the class rather than from a site of their own, and none of the three facts below is implied by that width: an escaping member reaches its callers through the arity-keyed `clsr/{arity}` supertype, which the rewrite changes; the entry is called by the host; and a member whose return edges do not all carry a function reference has an edge the rewrite cannot turn into a call, which would leave it handing back a raw value where its class-mates hand back a called one.
fn rewritable(module: &Module, calls: &CallAnalysis, function: FunctionId) -> bool {
    !calls.escaping.contains(&function)
        && module.entry() != Some(function)
        && returns_functions(module, function)
}

/// Whether every edge `function` returns on carries a function reference — what the rewrite turns into a tail call.
///
/// Vacuously true for a function with no return edge at all, which returns only by tail-forwarding. Such a member has nothing of its own to rewrite and takes its whole answer from the class it forwards into.
fn returns_functions(module: &Module, function: FunctionId) -> bool {
    let sentinel = module.function(function).unwrap().return_cont;
    for node_id in function_nodes(module, function) {
        let edges: Vec<&Edge> = match module.node(node_id).unwrap() {
            Node::ApplyCont(edge) => vec![edge],
            Node::Switch { cases, default, .. } => cases.values().chain(default.as_ref()).collect(),
            _ => continue,
        };
        for edge in edges.into_iter().filter(|edge| edge.target == sentinel) {
            if !matches!(edge.args.as_slice(), [Atom::Fun(_)]) {
                return false;
            }
        }
    }
    true
}

/// One call site the rewrite can absorb: what the callee takes over, and how the site resumes without the closure.
#[derive(Clone)]
struct Site {
    passed: Vec<Atom>,
    resume: Resume,
    /// The known functions whose calls enter the continuation a [`Resume::Jump`] rewrites — this site's callee among them — and none for a [`Resume::Retarget`], which leaves that continuation alone. Whether each is rewritten with the site is a fact about the class, which [`plan_class`] holds them to.
    callers: BTreeSet<FunctionId>,
}

/// Judge one call site by the continuation it resumes at: the application the callee would absorb, or `None` where the site cannot be rewritten.
///
/// **Every condition on a site is here, and nowhere else.** Admission ([`uncurryable`]) and planning ([`plan_class`]) both consume this judgment rather than each walking the resume with a list of its own, where each list could lack a clause the other has — a plan counting application sites without asking whether the closure is also kept would rewrite a member admission refused, on its class-mate's width, and the tuple holding the closure would hold the applied answer. One judgment consumed twice cannot disagree with itself.
///
/// The conditions: the resume receives the one value the tuple protocol delivers; that value's only use is a single application, which [`sole_application`] establishes over the whole region, nested functions included; the application's arguments exist where the call is rather than being bound inside the resume, or moving the application above the call would move a computation with it; the application is reached from the resume's head through `LetCont`s alone, which is what [`Resume`] needs; and where the resume is kept and rewritten, every entry into it is a call, whose callers the site records. The one condition that is not the site's own — that each of those callers is rewritten with it — is the class's, and [`plan_class`] checks it against what this records rather than judging the resume again. A width of zero is refused because it is a different transform wearing this one's clothes: with no argument to absorb, the closure is a *thunk* and the rewrite would only decide when it runs — which for an `Io` description is the one thing its meaning rests on.
fn admit_site(
    module: &Module,
    entries: &BTreeMap<ContinuationId, Vec<Option<FunctionId>>>,
    return_to: ContinuationId,
) -> Option<Site> {
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
        .any(|atom| matches!(atom, Atom::Value(value) if bound.contains(value)))
    {
        return None;
    }
    if !reached_directly(module, resume.body, site) {
        return None;
    }
    let Node::ApplyFun {
        return_to: after, ..
    } = *module.node(site).unwrap()
    else {
        unreachable!("an application site is an application")
    };
    // Both forms are correct rewrites of this site, and the cheap one is available wherever losing it would cost: a call whose stack depth matters is in tail position, so the application that follows it is too, and a continuation holding nothing but that application is what [`Resume::Retarget`] asks for.
    let (resume, callers) = if resume.body == site {
        (Resume::Retarget(after), BTreeSet::new())
    } else {
        // Keeping the resume rewrites it for everything that enters it, so everything that enters it has to be a call this transform can reach: a jump carrying some other closure, a call through one, an operation's result would each arrive at the rewritten body still unapplied.
        let callers = entries
            .get(&return_to)?
            .iter()
            .copied()
            .collect::<Option<BTreeSet<_>>>()?;
        let resume = Resume::Jump {
            site,
            result: *result,
            after,
        };
        (resume, callers)
    };
    Some(Site {
        passed,
        resume,
        callers,
    })
}

/// The one application of `value` beneath `body`, with its arguments, when that application is the value's only use — or `None` when any use does anything else: a second application, an ordinary operand, which would dangle once the callee returns the answer instead of the closure, or any mention inside a function defined beneath `body`, which captures the closure and outlives the site.
///
/// The capture rule is what [`function_nodes`] warns a pass about to remove a binding of: [`nodes_from`] enters a `LetFun`'s continuation and not its members, so a lambda defined below the application that applies the closure again would be invisible to it: the site would be admitted on its one visible application, the callee would absorb it, and the lambda would go on applying what is by then the applied answer — a trap where the program prints a number.
fn sole_application(module: &Module, body: NodeId, value: ValueId) -> Option<(NodeId, Vec<Atom>)> {
    let mut found = None;
    for node_id in nodes_from(module, body) {
        let node = module.node(node_id).unwrap();
        for atom in atoms(node) {
            if matches!(atom, Atom::Value(used) if *used == value) {
                return None;
            }
        }
        if let Node::LetFun { functions, .. } = node
            && functions
                .iter()
                .any(|function| mentioned_in(module, *function, value))
        {
            return None;
        }
        if let Node::ApplyFun {
            callee: Callee::Closure(callee),
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
fn mentioned_in(module: &Module, function: FunctionId, value: ValueId) -> bool {
    region_nodes(module, module.function(function).unwrap().body)
        .into_iter()
        .any(|node_id| match module.node(node_id).unwrap() {
            Node::ApplyFun {
                callee: Callee::Closure(callee),
                ..
            } if *callee == value => true,
            node => atoms(node)
                .into_iter()
                .any(|atom| matches!(atom, Atom::Value(used) if *used == value)),
        })
}

/// Every node beneath `body`, the bodies of the functions defined beneath it included, transitively — the whole region a site's rewrite reaches, where [`nodes_from`] stops at a nested function.
fn region_nodes(module: &Module, body: NodeId) -> Vec<NodeId> {
    let mut nodes = BTreeSet::new();
    let mut work = vec![body];
    while let Some(body) = work.pop() {
        for node_id in nodes_from(module, body) {
            if nodes.insert(node_id)
                && let Node::LetFun { functions, .. } = module.node(node_id).unwrap()
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
fn values_bound_in(module: &Module, body: NodeId) -> BTreeSet<ValueId> {
    let mut bound = BTreeSet::new();
    for node_id in nodes_from(module, body) {
        match module.node(node_id).unwrap() {
            Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. } => {
                bound.insert(*result);
            }
            Node::LetCont { continuations, .. } => {
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
#[derive(Clone, Copy)]
enum Resume {
    /// Resume where the *application* did, bypassing the continuation that received the closure.
    ///
    /// Admissible only when that continuation's body is the application and nothing else, which is what makes the bypass total rather than a skipped computation — and which also proves the target is in scope at the call: with no `LetCont` of its own to introduce it, the target was already bound where the receiving continuation was.
    Retarget(ContinuationId),
    /// Keep that continuation, and turn the application inside it into a jump carrying what the callee now returns directly.
    ///
    /// Nothing moves, but the continuation is every entry's rather than this call's, so from then on it reads an answer wherever it is entered from: it is well-formed exactly when every entry is a call to a member of the class being rewritten, which [`admit_site`] and [`plan_class`] establish between them. It costs one live frame per call, which matters only in a loop — and in a loop the application is itself in tail position, so its target is the caller's own sentinel and [`Resume::Retarget`] takes the site instead.
    Jump {
        site: NodeId,
        result: ValueId,
        after: ContinuationId,
    },
}

/// A complete class judgment, including the bodies, signatures and resumes whose assumptions must survive earlier rewrites.
struct ClassPlan {
    members: Vec<FunctionId>,
    width: usize,
    sites: Vec<(NodeId, Site)>,
    reads: BTreeSet<NodeId>,
    functions: BTreeSet<FunctionId>,
}

/// Absorb every independent tail class admitted by one call/entry snapshot. A class whose body, caller or resume an earlier rewrite touches waits for the next round; in particular a retargeted caller can introduce a tail edge joining previously separate classes.
pub(crate) fn uncurry_returns(module: &mut Module) -> bool {
    let calls = analyze_calls(module);
    let entries = continuation_entries(module);
    let observed = calls
        .call_sites
        .values()
        .flatten()
        .filter_map(|id| match module.node(*id).unwrap() {
            Node::ApplyFun { return_to, .. } => Some(*return_to),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let observations = observed
        .into_iter()
        .map(|resume| (resume, admit_site(module, &entries, resume)))
        .collect::<BTreeMap<_, _>>();
    let verdicts = uncurryable(module, &calls, &observations);
    let mut plans = Vec::new();
    for members in tail_classes(module) {
        let mut widths = members.iter().filter_map(|member| verdicts.get(member));
        let Some(Some(width)) = widths.next().copied() else {
            continue;
        };
        if !widths.all(|other| *other == Some(width))
            || !members
                .iter()
                .all(|member| rewritable(module, &calls, *member))
        {
            continue;
        }
        let Some(sites) = plan_class(module, &calls, &observations, &members) else {
            continue;
        };
        let mut reads = members
            .iter()
            .flat_map(|member| function_nodes(module, *member))
            .collect::<BTreeSet<_>>();
        let mut functions = members.iter().copied().collect::<BTreeSet<_>>();
        for &id in &reads {
            for atom in atoms(module.node(id).unwrap()) {
                if let Atom::Fun(function) = atom {
                    functions.insert(*function);
                }
            }
        }
        for (id, _) in &sites {
            reads.insert(*id);
            let Node::ApplyFun { return_to, .. } = *module.node(*id).unwrap() else {
                unreachable!()
            };
            if let Some(resume) = module.continuation(return_to) {
                reads.extend(region_nodes(module, resume.body));
            }
        }
        plans.push(ClassPlan {
            members,
            width,
            sites,
            reads,
            functions,
        });
    }
    let mut touched = BTreeSet::new();
    let mut changed_functions = BTreeSet::new();
    let mut changed = false;
    for plan in plans {
        if !plan.reads.is_disjoint(&touched) || !plan.functions.is_disjoint(&changed_functions) {
            continue;
        }
        let mutations = plan
            .members
            .iter()
            .flat_map(|member| function_nodes(module, *member))
            .chain(plan.sites.iter().flat_map(|(id, site)| {
                let mut nodes = vec![*id];
                if let Resume::Jump { site, .. } = site.resume {
                    nodes.push(site);
                }
                nodes
            }))
            .collect::<BTreeSet<_>>();
        changed_functions.extend(plan.members.iter().copied());
        apply_class(module, &plan.members, plan.width, plan.sites);
        touched.extend(mutations);
        changed = true;
    }
    changed
}

fn apply_class(
    module: &mut Module,
    members: &[FunctionId],
    width: usize,
    plan: Vec<(NodeId, Site)>,
) {
    let mut extra = BTreeMap::new();
    for &member in members {
        let params = (0..width)
            .map(|index| module.add_value(Some(format!("applied/{}/{index}", member.index()))))
            .collect::<Vec<_>>();
        module.function_mut(member).unwrap().params.extend(&params);
        extra.insert(member, params);
    }

    for &member in members {
        let sentinel = module.function(member).unwrap().return_cont;
        let carried = extra[&member]
            .iter()
            .copied()
            .map(Atom::Value)
            .collect::<Vec<_>>();
        for node_id in function_nodes(module, member) {
            // A member that returns by tail-calling another forwards what it was given, or the callee gains a parameter nothing passes.
            if let Node::ApplyFun {
                callee: Callee::Known(onward),
                args,
                return_to,
            } = module.node(node_id).unwrap()
                && *return_to == sentinel
                && members.contains(onward)
            {
                let (onward, mut args) = (*onward, args.clone());
                args.extend(carried.iter().cloned());
                module.set_node(
                    node_id,
                    Node::ApplyFun {
                        callee: Callee::Known(onward),
                        args,
                        return_to: sentinel,
                    },
                );
                continue;
            }
            // A return edge hands back a function this class no longer builds a closure for; calling it here is what absorbs the application.
            if let Node::ApplyCont(edge) = module.node(node_id).unwrap()
                && edge.target == sentinel
                && let [Atom::Fun(returned)] = edge.args.as_slice()
            {
                let returned = *returned;
                // A function of another arity than the application every caller makes is handed back where no run arrives — see [`Module::applies`].
                let absorbed = match module.applies(returned, carried.len()) {
                    true => Node::ApplyFun {
                        callee: Callee::Known(returned),
                        args: carried.clone(),
                        return_to: sentinel,
                    },
                    false => Node::Unreachable,
                };
                module.set_node(node_id, absorbed);
            }
        }
    }

    for (node_id, Site { passed, resume, .. }) in plan {
        let Some(Node::ApplyFun {
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
                module.set_node(
                    site,
                    Node::ApplyCont(Edge {
                        target: after,
                        args: vec![Atom::Value(result)],
                    }),
                );
                return_to
            }
        };
        module.set_node(
            node_id,
            Node::ApplyFun {
                callee,
                args,
                return_to,
            },
        );
    }
}

/// Every call site the rewrite must change, each with what [`admit_site`] admitted at it, or `None` if any of them refuses.
///
/// Parameters are added to a whole class at once, so a site discovered later to be unrewritable would leave a callee expecting an argument nobody passes. The transform has to be decided before it is begun — and it is decided by the same judgment that observed the widths, so nothing a site refused can be planned on its class-mates' account.
fn plan_class(
    module: &Module,
    calls: &CallAnalysis,
    observations: &BTreeMap<ContinuationId, Option<Site>>,
    members: &[FunctionId],
) -> Option<Vec<(NodeId, Site)>> {
    let mut plan = Vec::new();
    for node_id in members
        .iter()
        .flat_map(|member| calls.call_sites[member].iter())
        .copied()
    {
        let Some(Node::ApplyFun {
            callee: Callee::Known(callee),
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
        let site = observations.get(return_to)?.clone()?;
        // A kept resume is rewritten for every call entering it, so a call to anything outside the class would still hand it a closure.
        if !site.callers.iter().all(|caller| members.contains(caller)) {
            return None;
        }
        plan.push((node_id, site));
    }
    Some(plan)
}

/// Whether `site` is reached from `body` by binding continuations and doing nothing else.
///
/// [`Resume::Jump`] leaves the application where it stands but has the callee perform it before returning, so whatever the receiving continuation evaluates *ahead* of the application would move behind it. A `LetCont` introduces names without evaluating anything, which is why the shape that motivates the form — a join point bound for the application's own result — is admissible where a preceding `let` is not. It also rules out an application reached through a branch or a loop: one syntactic site inside a loop is many forcings, and the rewrite would leave one.
fn reached_directly(module: &Module, body: NodeId, site: NodeId) -> bool {
    let mut node = body;
    loop {
        if node == site {
            return true;
        }
        match module.node(node) {
            Some(Node::LetCont { body, .. }) => node = *body,
            _ => return false,
        }
    }
}

/// The undirected connected components of the tail-call graph, which a shared return obliges to decide together.
fn tail_classes(module: &Module) -> Vec<Vec<FunctionId>> {
    let mut edges = BTreeMap::<FunctionId, BTreeSet<FunctionId>>::new();
    for owner in module.function_ids().collect::<Vec<_>>() {
        let sentinel = module.function(owner).unwrap().return_cont;
        edges.entry(owner).or_default();
        for node_id in function_nodes(module, owner) {
            if let Some(Node::ApplyFun {
                callee: Callee::Known(callee),
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
