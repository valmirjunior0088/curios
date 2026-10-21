//! What an arm of a switch learns of its scrutinee, spent on the window guards it decides.

use {
    crate::{
        Atom, ContinuationId, Intrinsic, Literal, Module, Node, NodeId, ValueId,
        continuation_transfers, visit_atoms_mut,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Answer every window guard its arm decides, in one sweep.
///
/// A suffix's guard, `Window/extent(k, n ∸ k, n)`, traps only where `k > n`, and the arm a switch on `n` takes past its cases `0` to `k - 1` is entered only where `n ≥ k`; inside that arm the guard answers its count, so its uses read the count and the guard goes. An arm learns what its switch decided where the switch is the one transfer into it, and what it learns holds of everything lexically inside it, since a continuation is in scope there alone and the scrutinee is bound before the switch.
pub(crate) fn answer_window_guards(module: &mut Module) -> bool {
    let differences = module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::NatSub,
                args,
                ..
            } => Some((*result, args.clone())),
            _ => None,
        })
        .collect::<BTreeMap<_, _>>();
    let enclosing = enclosing_continuations(module);
    let transfers = continuation_transfers(module);

    // Each answered guard: its node, the count its uses read instead, and the node that follows it.
    let mut answered = Vec::<(NodeId, ValueId, Atom, NodeId)>::new();
    for (id, node) in module.live_nodes() {
        let Node::LetIntrinsic {
            result,
            op: Intrinsic::WindowExtent,
            args,
            next,
        } = node
        else {
            continue;
        };
        let [start, Atom::Value(count), Atom::Value(length)] = args.as_slice() else {
            continue;
        };
        let Atom::Literal(Literal::Nat(start_value)) = start else {
            continue;
        };
        let Ok(start_value) = u32::try_from(start_value) else {
            continue;
        };
        let is_suffix = differences.get(count).is_some_and(|difference| {
            difference.as_slice() == [Atom::Value(*length), start.clone()]
        });
        if !is_suffix {
            continue;
        }

        let mut arm = enclosing.parent_of_node.get(&id).copied();
        while let Some(continuation) = arm {
            if decides_at_least(module, &transfers, continuation, *length, start_value) {
                answered.push((id, *result, Atom::Value(*count), *next));
                break;
            }
            arm = enclosing.parent_of_continuation.get(&continuation).copied();
        }
    }

    let substitutions = answered
        .iter()
        .map(|(_, result, count, _)| (*result, count.clone()))
        .collect::<BTreeMap<_, _>>();
    for (_, node) in module.live_nodes_mut() {
        visit_atoms_mut(node, &mut |atom| {
            if let Atom::Value(value) = atom
                && let Some(count) = substitutions.get(value)
            {
                *atom = count.clone();
            }
        });
    }
    // A guard's one structural parent links to the guard's slot, so the node after it moves into that slot and nothing else needs rewiring. A guard followed by another is moved after it, so a run of guards collapses from its end.
    let nexts = answered
        .iter()
        .map(|&(guard, _, _, next)| (guard, next))
        .collect::<BTreeMap<_, _>>();
    let mut moved = BTreeSet::new();
    for &(first, ..) in &answered {
        let mut run = vec![first];
        while let Some(&next) = nexts.get(run.last().unwrap())
            && nexts.contains_key(&next)
            && !moved.contains(&next)
        {
            run.push(next);
        }
        for guard in run.into_iter().rev() {
            if moved.insert(guard) {
                let follower = module
                    .remove_node(nexts[&guard])
                    .expect("an answered guard's next node is live");
                module.set_node(guard, follower);
            }
        }
    }
    for (_, result, ..) in &answered {
        module.remove_value(*result);
    }

    !answered.is_empty()
}

/// Whether `continuation` is entered only past a switch on `scrutinee` that has a case for each of `0` to `at_least - 1` and enters it by its default.
fn decides_at_least(
    module: &Module,
    transfers: &BTreeMap<ContinuationId, Vec<NodeId>>,
    continuation: ContinuationId,
    scrutinee: ValueId,
    at_least: u32,
) -> bool {
    let Some([switch]) = transfers.get(&continuation).map(Vec::as_slice) else {
        return false;
    };
    let Some(Node::Switch {
        scrutinee: Atom::Value(decided),
        cases,
        default: Some(default),
    }) = module.node(*switch)
    else {
        return false;
    };
    *decided == scrutinee
        && default.target == continuation
        && cases.values().all(|case| case.target != continuation)
        && (0..at_least).all(|case| cases.contains_key(&case))
}

/// The continuation lexically enclosing each node and each continuation, innermost first; a node of a function's body outside every continuation has none.
struct Enclosing {
    parent_of_node: BTreeMap<NodeId, ContinuationId>,
    parent_of_continuation: BTreeMap<ContinuationId, ContinuationId>,
}

fn enclosing_continuations(module: &Module) -> Enclosing {
    let mut enclosing = Enclosing {
        parent_of_node: BTreeMap::new(),
        parent_of_continuation: BTreeMap::new(),
    };
    for (_, function) in module.live_functions() {
        let mut work = vec![(function.body, None::<ContinuationId>)];
        while let Some((id, within)) = work.pop() {
            if let Some(continuation) = within {
                enclosing.parent_of_node.insert(id, continuation);
            }
            match module.node(id) {
                Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => {
                    work.push((*next, within));
                }
                Some(Node::LetFun { body, .. }) => work.push((*body, within)),
                Some(Node::LetCont {
                    continuations,
                    body,
                }) => {
                    work.push((*body, within));
                    for &continuation in continuations {
                        if let Some(within) = within {
                            enclosing
                                .parent_of_continuation
                                .insert(continuation, within);
                        }
                        if let Some(definition) = module.continuation(continuation) {
                            work.push((definition.body, Some(continuation)));
                        }
                    }
                }
                Some(
                    Node::ApplyFun { .. }
                    | Node::ApplyCont(_)
                    | Node::Switch { .. }
                    | Node::Foreign { .. }
                    | Node::Cell { .. }
                    | Node::Channel { .. }
                    | Node::Intrinsic { .. }
                    | Node::Halt { .. }
                    | Node::Panic(_)
                    | Node::Unreachable,
                )
                | None => {}
            }
        }
    }
    enclosing
}

#[cfg(test)]
mod tests;
