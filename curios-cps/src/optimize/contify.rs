use {
    super::{SCC_CLONE_NODE_LIMIT, copy_bodies, introducing_letfun},
    crate::{
        CallAnalysis, Callee, Continuation, CopyPlan, Edge, FunctionId, Module, Node, NodeId,
        analyze_calls, function_nodes,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Contify every non-escaping function whose calls resolve to a single return context into a local continuation, covering both the single-entry recursive loop and the non-recursive join-point cases.
///
/// A function qualifies when it has exactly one external call site: any call from a third function would make `external` longer than one, so the only admissible calls are that single entry plus the function's own tail-recursive self-calls. This excludes mutual recursion and multi-return-context callers without a separate check, and a function with several external call sites stays a function.
///
/// The callee's captures need no check. Its one external site names it, so the `LetFun` binding it encloses that site, and every value the callee mentions without binding is bound before that `LetFun` — in scope at the site by the rule `verify_lexical_scopes` walks. A guard refusing the move unless the owner's own body *mentions* each such value would test the owner's text rather than its scope: a loop capturing an outer binding would stay a function call whenever its caller happens not to name the binding itself.
///
/// One call analysis serves every candidate, because a contification leaves the others admissible. It replaces one call and moves one body: no other function's sites change, nothing new escapes, and the one fact that does move — the owner of a site that sat inside the contified body is now that body's new owner — preserves the reachability condition that reads it: the new owner called the contified function directly, so reaching it would have meant reaching the function, which was refused. What does go stale — which function the snapshot says owns a site — nothing below reads. One contification per call would set a `Toml/decode` compile's round count, as `fixpoint_pass_measurements` shows.
pub fn contify_calls(module: &mut Module) -> bool {
    let analysis = analyze_calls(module);
    let mut admitted = Vec::new();

    for (callee, function) in module.live_functions() {
        if Some(callee) == module.entry()
            || analysis.escaping.contains(&callee)
            || module.is_out_of_line(callee)
        {
            continue;
        }

        let sites = &analysis.call_sites[&callee];
        let external = sites
            .iter()
            .copied()
            .filter(|site| analysis.node_owners[site] != callee)
            .collect::<Vec<_>>();
        if external.len() != 1 {
            continue;
        }
        if function_reaches(
            &analysis.call_graph,
            callee,
            analysis.node_owners[&external[0]],
        ) {
            continue;
        }

        let mut compatible = true;
        for &site in sites {
            let Node::ApplyFun { return_to, .. } = module.node(site).unwrap() else {
                unreachable!()
            };
            if analysis.node_owners[&site] == callee && *return_to != function.return_cont {
                compatible = false;
                break;
            }
        }
        if compatible {
            admitted.push((callee, external[0]));
        }
    }

    let mut changed = false;
    for (callee, call) in admitted {
        contify_call(module, callee, call);
        changed = true;
    }
    changed
}

/// The one node contification adds beside the copied body: its initial transfer. The call node itself is reused.
const CONTIFICATION_NODE_OVERHEAD: usize = 1;

/// One admitted loop-entry copy whose sole external caller can receive its body as a continuation.
///
/// The copy's complete extent must contain no call back to the root from a nested function: that call would follow the copy and give it another external entry. Ordinary self-tail calls belong to the root itself and become backedges. Preparation mints nothing and reads the loop's extent as the module stands; application copies and contifies immediately, with no graph mutation between them that could invalidate admission.
struct LoopEntryPlan {
    function: FunctionId,
    call: NodeId,
    intro: NodeId,
    copy: CopyPlan,
}

impl LoopEntryPlan {
    /// The plan that gives `callee`'s external call `entry` a copy, where the loop admits one: the first external call keeps the loop itself, so the entries a plan serves count from one.
    fn prepare(
        module: &Module,
        analysis: &CallAnalysis,
        callee: FunctionId,
        entry: usize,
    ) -> Option<Self> {
        let alone = !analysis.call_graph[&callee]
            .iter()
            .any(|&next| next != callee && function_reaches(&analysis.call_graph, next, callee));
        if Some(callee) == module.entry()
            || analysis.escaping.contains(&callee)
            || module.is_out_of_line(callee)
            || !alone
        {
            return None;
        }
        let function = module.function(callee).unwrap();
        let (internal, external): (Vec<_>, Vec<_>) = analysis.call_sites[&callee]
            .iter()
            .copied()
            .partition(|site| analysis.node_owners[site] == callee);
        let tail = internal.iter().all(|&site| {
            matches!(module.node(site), Some(Node::ApplyFun { return_to, .. }) if *return_to == function.return_cont)
        });
        let reentered = external
            .iter()
            .any(|site| function_reaches(&analysis.call_graph, callee, analysis.node_owners[site]));
        if internal.is_empty() || external.len() < 2 || !tail || reentered {
            return None;
        }
        let body_limit = SCC_CLONE_NODE_LIMIT.checked_sub(CONTIFICATION_NODE_OVERHEAD)?;
        let copy = CopyPlan::functions_with_limit(module, BTreeSet::from([callee]), body_limit)?;
        if external.iter().any(|site| copy.nodes().contains(site)) {
            return None;
        }
        Some(Self {
            function: callee,
            call: *external.get(entry)?,
            intro: introducing_letfun(module, &BTreeSet::from([callee]))?,
            copy,
        })
    }

    fn apply(self, module: &mut Module) {
        let copy = copy_bodies(module, self.copy).functions[&self.function];
        let Node::LetFun { functions, .. } = module.node_mut(self.intro).unwrap() else {
            unreachable!()
        };
        functions.push(copy);
        let Node::ApplyFun { callee, .. } = module.node_mut(self.call).unwrap() else {
            unreachable!()
        };
        *callee = Callee::Known(copy);
        #[cfg(debug_assertions)]
        {
            let analysis = analyze_calls(module);
            let external = analysis.call_sites[&copy]
                .iter()
                .copied()
                .filter(|site| analysis.node_owners[site] != copy)
                .collect::<Vec<_>>();
            debug_assert_eq!(external, vec![self.call]);
        }
        contify_call(module, copy, self.call);
        debug_assert!(module.function(copy).is_none());
    }
}

/// Give each additional caller of a small self-tail loop a contified copy.
///
/// An admitted plan bounds the complete copy and contification scaffolding, and proves the copy has one external entry. Copying and contification are one rewrite, so no intermediate function waits for a later round to make the expansion useful. Every loop's extra entries are taken in function and site order, each a copy charged to the module's entry-copy budget, against the one call analysis the pass begins with.
///
/// One analysis serves every plan, because a copy leaves the others admissible. It mints the nodes of one loop inside the function that called it and retargets that one call: no site the snapshot names changes its callee or its owner, a loop keeps its first external call and so stays, and what a plan reads of the graph, whether its loop stands alone and whether a caller is reached from it, is read off edges the rewrite only removes or shortens, the caller calling directly what it reached through the loop. A loop copied into another's body grows that body, which the other's plan reads as it stands. What the snapshot misses is the call a copied body makes to another loop, one more entry for that loop, which the next round's analysis finds. One copy a call would cost a module a round of every pass for each, as `fixpoint_pass_measurements` shows.
pub fn separate_loop_entries(module: &mut Module, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let analysis = analyze_calls(module);
    let loops = module
        .live_functions()
        .map(|(callee, _)| callee)
        .collect::<Vec<_>>();
    let mut changed = false;
    for callee in loops {
        let mut entry = 1;
        while *budget > 0
            && let Some(plan) = LoopEntryPlan::prepare(module, &analysis, callee, entry)
        {
            plan.apply(module);
            *budget -= 1;
            changed = true;
            entry += 1;
        }
    }
    changed
}

pub fn function_reaches(
    graph: &BTreeMap<FunctionId, BTreeSet<FunctionId>>,
    start: FunctionId,
    target: FunctionId,
) -> bool {
    let mut visited = BTreeSet::new();
    let mut work = graph[&start].iter().copied().collect::<Vec<_>>();
    while let Some(function) = work.pop() {
        if function == target {
            return true;
        }
        if visited.insert(function)
            && let Some(next) = graph.get(&function)
        {
            work.extend(next.iter().copied());
        }
    }
    false
}
pub fn contify_call(module: &mut Module, callee: FunctionId, call: NodeId) {
    let function = module.function(callee).unwrap().clone();
    let Node::ApplyFun {
        callee: Callee::Known(found),
        args,
        return_to,
    } = module.node(call).unwrap().clone()
    else {
        unreachable!()
    };
    assert_eq!(found, callee);

    let loop_cont = module.reserve_continuation();
    // Every transfer to the callee's return goes where the call returned, a switch's edge as a jump does: the machine lowering gives an edge to a function's return a block of its own, so the call's return may be the caller's.
    for node_id in function_nodes(module, callee) {
        let node = module.node_mut(node_id).unwrap();
        match node {
            Node::ApplyFun {
                callee: Callee::Known(target),
                args,
                return_to: target_return,
            } if *target == callee => {
                debug_assert_eq!(*target_return, function.return_cont);
                *node = Node::ApplyCont(Edge {
                    target: loop_cont,
                    args: std::mem::take(args),
                });
            }
            Node::ApplyFun {
                return_to: target, ..
            }
            | Node::Foreign {
                return_to: target, ..
            }
            | Node::Cell {
                return_to: target, ..
            }
            | Node::Channel {
                return_to: target, ..
            }
            | Node::Intrinsic {
                return_to: target, ..
            } if *target == function.return_cont => *target = return_to,
            Node::ApplyCont(edge) if edge.target == function.return_cont => {
                edge.target = return_to;
            }
            Node::Switch { cases, default, .. } => {
                for edge in cases.values_mut().chain(default.iter_mut()) {
                    if edge.target == function.return_cont {
                        edge.target = return_to;
                    }
                }
            }
            _ => {}
        }
    }

    let initial = module.reserve_node();
    module.define_node(
        initial,
        Node::ApplyCont(Edge {
            target: loop_cont,
            args,
        }),
    );
    module.define_continuation(
        loop_cont,
        Continuation {
            debug_name: function.debug_name,
            params: function.params,
            body: function.body,
        },
    );
    module.set_node(
        call,
        Node::LetCont {
            continuations: vec![loop_cont],
            body: initial,
        },
    );
    module.remove_function(callee);
    for (_, node) in module.live_nodes_mut() {
        if let Node::LetFun { functions, .. } = node {
            functions.retain(|function| *function != callee);
        }
    }
}

#[cfg(test)]
mod tests;
