use {
    super::analysis::{analyze_calls, function_nodes},
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

/// Contify every non-escaping function whose calls resolve to a single return context into a local continuation, covering both the single-entry recursive loop and the non-recursive join-point cases.
///
/// A function qualifies when it has exactly one external call site: any call from a third function would make `external` longer than one, so the only admissible calls are that single entry plus the function's own tail-recursive self-calls. This excludes mutual recursion and multi-return-context callers without a separate check. Common-dominator placement for genuinely multi-site contification is deferred to the machine-CFG analysis.
///
/// The callee's captures need no check. Its one external site names it, so the `LetFun` binding it encloses that site, and every value the callee mentions without binding is bound before that `LetFun` — in scope at the site by the rule `verify_lexical_scopes` walks. A guard once refused the move unless the owner's own body *mentioned* each such value, which is a fact about the owner's text rather than its scope: a loop capturing an outer binding stayed a function call whenever its caller happened not to name the binding itself.
///
/// One call analysis serves every candidate, because a contification leaves the others admissible. It replaces one call and moves one body: no other function's sites change, nothing new escapes, and the one fact that does move — the owner of a site that sat inside the contified body is now that body's new owner — preserves the reachability condition that reads it: the new owner called the contified function directly, so reaching it would have meant reaching the function, which was refused. What does go stale — which function the snapshot says owns a site — nothing below reads. It was one contification per call, which `fixpoint_pass_measurements` found setting a `Toml/decode` compile's round count once the two passes ahead of it stopped.
pub(super) fn contify_calls(module: &mut CpsModule) -> bool {
    let analysis = analyze_calls(module);
    let mut admitted = Vec::new();

    for (callee, function) in module.functions.iter_live() {
        if Some(callee) == module.entry || analysis.escaping.contains(&callee) {
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
            let CpsNode::ApplyFun { return_to, .. } = module.node(site).unwrap() else {
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

pub(super) fn function_reaches(
    graph: &BTreeMap<CpsFunId, BTreeSet<CpsFunId>>,
    start: CpsFunId,
    target: CpsFunId,
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
pub(super) fn contify_call(module: &mut CpsModule, callee: CpsFunId, call: CpsNodeId) {
    let function = module.function(callee).unwrap().clone();
    let CpsNode::ApplyFun {
        callee: CpsCallee::Known(found),
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
        let node = module.nodes.get_mut(node_id).unwrap();
        match node {
            CpsNode::ApplyFun {
                callee: CpsCallee::Known(target),
                args,
                return_to: target_return,
            } if *target == callee => {
                debug_assert_eq!(*target_return, function.return_cont);
                *node = CpsNode::ApplyCont(CpsEdge {
                    target: loop_cont,
                    args: std::mem::take(args),
                });
            }
            CpsNode::ApplyFun {
                return_to: target, ..
            }
            | CpsNode::Foreign {
                return_to: target, ..
            }
            | CpsNode::Cell {
                return_to: target, ..
            }
            | CpsNode::Intrinsic {
                return_to: target, ..
            } if *target == function.return_cont => *target = return_to,
            CpsNode::ApplyCont(edge) if edge.target == function.return_cont => {
                edge.target = return_to;
            }
            CpsNode::Switch { cases, default, .. } => {
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
    module.nodes.define(
        initial,
        CpsNode::ApplyCont(CpsEdge {
            target: loop_cont,
            args,
        }),
    );
    module.nodes.define(
        return_body,
        CpsNode::ApplyCont(CpsEdge {
            target: return_to,
            args: vec![CpsAtom::Value(return_value)],
        }),
    );
    module.continuations.define(
        return_bridge,
        CpsContinuation {
            debug_name: Some("contified return".into()),
            params: vec![return_value],
            body: return_body,
        },
    );
    module.nodes.define(
        loop_scope,
        CpsNode::LetCont {
            continuations: vec![return_bridge],
            body: function.body,
        },
    );
    module.continuations.define(
        loop_cont,
        CpsContinuation {
            debug_name: function.debug_name,
            params: function.params,
            body: loop_scope,
        },
    );
    module.nodes.set(
        call,
        CpsNode::LetCont {
            continuations: vec![loop_cont],
            body: initial,
        },
    );
    module.functions.remove(callee);
    for (_, node) in module.nodes.iter_live_mut() {
        if let CpsNode::LetFun { functions, .. } = node {
            functions.retain(|function| *function != callee);
        }
    }
}
