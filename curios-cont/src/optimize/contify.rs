use {
    super::{SCC_CLONE_NODE_LIMIT, clone_scc, introducing_letfun},
    crate::{
        Atom, Callee, Continuation, Edge, FunctionId, Module, Node, NodeId, analyze_calls,
        function_nodes,
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
pub(crate) fn contify_calls(module: &mut Module) -> bool {
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

/// Give one more external call site of a loop a copy of its own, so that each copy has the single entry [`contify_calls`] asks for.
///
/// A loop here is a function that recurses through itself alone and only by tail calls, that nothing holds as a value, and whose body is within `SCC_CLONE_NODE_LIMIT` nodes: what contification would turn into a continuation if one site called it. Inlining makes a second site where it copies a caller that must also stay — a bind's continuation called on the path that answers at once and handed to `carry` on the path that suspends — and the loop would then stay a function, its scalars in no register. Copying it once per extra site costs its body again, which the node limit bounds, and leaves every copy contifiable where it is called. One copy is made per call, so the fixpoint stays deterministic, and at most `ENTRY_CLONE_LIMIT` across the module.
pub(crate) fn separate_loop_entries(module: &mut Module, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let analysis = analyze_calls(module);

    let chosen = module.live_functions().find_map(|(callee, function)| {
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

        let (internal, external): (Vec<_>, Vec<_>) = analysis.call_sites[&callee]
            .iter()
            .copied()
            .partition(|site| analysis.node_owners[site] == callee);
        let tail = internal.iter().all(|&site| {
            matches!(
                module.node(site),
                Some(Node::ApplyFun { return_to, .. }) if *return_to == function.return_cont
            )
        });
        let reentered = external
            .iter()
            .any(|site| function_reaches(&analysis.call_graph, callee, analysis.node_owners[site]));
        if internal.is_empty()
            || external.len() < 2
            || !tail
            || reentered
            || function_nodes(module, callee).len() > SCC_CLONE_NODE_LIMIT
        {
            return None;
        }

        introducing_letfun(module, &BTreeSet::from([callee]))
            .map(|intro| (callee, intro, external[1]))
    });
    let Some((callee, intro, site)) = chosen else {
        return false;
    };

    let copy = clone_scc(module, &BTreeSet::from([callee]))[&callee];
    if let Some(Node::LetFun { functions, .. }) = module.node_mut(intro) {
        functions.push(copy);
    }
    if let Some(Node::ApplyFun { callee, .. }) = module.node_mut(site) {
        *callee = Callee::Known(copy);
    }
    *budget -= 1;
    true
}

pub(crate) fn function_reaches(
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
pub(crate) fn contify_call(module: &mut Module, callee: FunctionId, call: NodeId) {
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
    let return_bridge = module.reserve_continuation();
    let return_value = module.add_value(Some("contified return".into()));
    let return_body = module.reserve_node();
    let loop_scope = module.reserve_node();
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
                        edge.target = return_bridge;
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
    module.define_node(
        return_body,
        Node::ApplyCont(Edge {
            target: return_to,
            args: vec![Atom::Value(return_value)],
        }),
    );
    module.define_continuation(
        return_bridge,
        Continuation {
            debug_name: Some("contified return".into()),
            params: vec![return_value],
            body: return_body,
        },
    );
    module.define_node(
        loop_scope,
        Node::LetCont {
            continuations: vec![return_bridge],
            body: function.body,
        },
    );
    module.define_continuation(
        loop_cont,
        Continuation {
            debug_name: function.debug_name,
            params: function.params,
            body: loop_scope,
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
