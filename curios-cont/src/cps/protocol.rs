//! How each function hands its result back, and which functions must decide that together.
//!
//! A function returns a variant by allocating the tuple its callers immediately take apart. Delivering the tuple's leading fields as several results removes that allocation. [`return_protocols`] decides where that is admissible and how wide, and [`split_returns`] performs it; they are one module because every condition the decision rests on is one the rewrite would otherwise have to restate, and a rewrite that restated any of them could disagree with the decision.
//!
//! **The width comes from the demand, not from the constructors.** It was first settled as a fixed tag beside one payload, on the ground that every surviving *constructor* carried at most one field. That measured the wrong side of the interface: what a protocol has to cover is the slots the *call sites* read, and the two components that pass every other test in `programs/parse_digits.crs` read three slots and five. A fixed pair selects neither, so the width is the demanded slot count and is a per-component number.
//!
//! **A function must not escape, for two independent reasons.** Its call sites have to be visible, and they are exactly the `Known` ones only when the function never becomes a value. And an escaping function additionally acquires a retained-ABI closure wrapper, which reaches it by a tail call at the shared `clsr/{arity}` type — a type this work deliberately does not re-key, so the wrapper would disagree with anything wider.
//!
//! **The shape a resume rebuilds in is part of the decision, not something the rewrite reads off the callee.** A class's return edges agree on one construction vocabulary — a row, or a structural tuple — and the reads below every resume are in that vocabulary, so the rebuild must be too. The rewrite used to recover it per *function*, from that function's own return edges; a member that returns only by tail-calling a class-mate has none, so its callers rebuilt a `Tuple` for a class that hands back a row, and the first `RowGet` below cast the wrong final type. Exactly the failure co-location was supposed to prevent, and what prevents it is the decision's return type being complete: `ReturnProtocol::Fields` carries the [`ReturnShape`] alongside the width, both joined over the class, and a class whose edges disagree is declined — the direction that only ever costs an allocation. [`CpsModule::verify`] holds the same line from below, refusing a read whose operand was minted in the other vocabulary.
//!
//! **Tail calls make the decision an equivalence class rather than a per-function one.** A call whose return continuation is its function's return sentinel lowers to `return_call`, which requires the callee's results to match the caller's exactly. Agreement is symmetric, so the classes are the *undirected* connected components of the tail-call graph: a function is decided together with everything it tail-calls and with everything that tail-calls it, however the edges point.
//!
//! **Four tail positions fix a function's results to something already settled**, and pin their whole component to the tuple: a tail call through a closure, which returns at the closure type's single result; a tail foreign call, a tail cell operation, and a tail `ListMap`, each of which falls through to the return with the results that operation produces. The module entry is pinned for the same kind of reason — the host calls it and is not rewritten with the module — and pinning it explicitly is load-bearing rather than belt-and-braces, because having no call sites of its own would otherwise leave it to inherit a component's decision through a tail call.
//!
//! **Every return edge must carry a construction the rewrite can read fields off.** A returned value that is merely some other value — `/std/Str/fold` returns a projection of its accumulator — has no statically known field count, so delivering *n* slots from it would mean projecting indices that need not exist at runtime. This is the condition that keeps a generic combinator out, and it is why a component containing one cannot be split however uniform its callers look.
//!
//! **A construction shorter than the width fills the rest with [`CpsAtom::Filler`]**, because a function's results are non-nullable and there is no empty value to pass. A caller reading slot *i* does so through a projection that already had to be reachable only where the tuple has that field, since the same projection on the same value is what runs today — but the filler is still *passed*, so it is written as "no value" rather than as a zero at a guessed carrier, and the emitter picks the zero once the destination's carrier is known. A return slot lands boxed either way; `fields.rs` is where the distinction became a trap.
//!
//! **A resume continuation is only widened when every entry to it is a call inside the same class.** A continuation has one arity, so one shared between a split call and anything else could not serve both — and since a split callee's every call site *must* be rewritten, one unwidenable site pins the whole class rather than being skipped.
//!
//! **Below two slots the decision is not taken.** A one-slot protocol hands back a single value, which is what a return edge carries before any of this — so nothing on the edge records that the class was already decided, and the next round would decide it again. The width bound is what makes [`split_returns`] idempotent, and idempotence here is a termination property rather than a tidiness one.

use {
    super::analysis::{analyze_calls, function_nodes},
    super::*,
    std::collections::{BTreeMap, BTreeSet},
};

/// How a function hands its result back to a caller.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ReturnProtocol {
    /// One value: the tuple as the callee built it. What every function speaks until this says otherwise.
    Tuple,
    /// The tuple's leading fields, delivered as that many results, rebuilt at the vocabulary the class's return edges agree on.
    Fields(usize, ReturnShape),
}

/// The construction vocabulary a class's return edges agree on — what a resume must rebuild in, so the reads below it stay exact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ReturnShape {
    Tuple,
    Row(CpsRowId),
}

/// What every live function's return protocol may be, total over the module's functions.
pub(super) fn return_protocols(module: &CpsModule) -> BTreeMap<CpsFunId, ReturnProtocol> {
    let calls = analyze_calls(module);
    let demands = demands(module);
    let constructions = constructions(module);
    let mut tail_calls = BTreeMap::<CpsFunId, BTreeSet<CpsFunId>>::new();
    let mut pinned = BTreeSet::<CpsFunId>::new();
    let mut demanded = BTreeMap::<CpsFunId, Demand>::new();
    let mut resumes = BTreeMap::<CpsFunId, BTreeSet<CpsContId>>::new();
    let entries = entries(module);

    for owner in module.functions.live_ids().collect::<Vec<_>>() {
        let sentinel = module.function(owner).unwrap().return_cont;
        tail_calls.entry(owner).or_default();
        if module.entry() == Some(owner) {
            pinned.insert(owner);
        }

        for node_id in function_nodes(module, owner) {
            let node = module.node(node_id).unwrap();
            for edge in return_edges(node, sentinel) {
                // A return edge whose value is not a construction leaves the rewrite nothing to read, and pinning the component is the only answer that stays correct at runtime.
                let visible = matches!(
                    edge.args.as_slice(),
                    [CpsAtom::Value(value)] if constructions.contains_key(value)
                );
                if !visible {
                    pinned.insert(owner);
                }
            }

            match node {
                CpsNode::ApplyFun {
                    callee: CpsCallee::Known(callee),
                    return_to,
                    ..
                } if *return_to == sentinel => {
                    tail_calls.entry(owner).or_default().insert(*callee);
                    tail_calls.entry(*callee).or_default().insert(owner);
                }
                CpsNode::ApplyFun {
                    callee: CpsCallee::Known(callee),
                    return_to,
                    ..
                } => {
                    let result = module.continuation(*return_to).unwrap().params.as_slice();
                    // A resume carrying anything but the single value the tuple protocol delivers is a call this analysis cannot read, and reading it as a whole-value use is the direction that only ever excludes.
                    let demand = match result {
                        [result] => demand_of(&demands, *result),
                        _ => Demand::Opaque,
                    };
                    demanded
                        .entry(*callee)
                        .or_insert_with(Demand::bottom)
                        .join(demand);
                    resumes.entry(*callee).or_default().insert(*return_to);
                }
                CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(_),
                    return_to,
                    ..
                }
                | CpsNode::Foreign { return_to, .. }
                | CpsNode::Cell { return_to, .. }
                | CpsNode::Intrinsic { return_to, .. }
                    if *return_to == sentinel =>
                {
                    pinned.insert(owner);
                }
                _ => {}
            }
        }
    }

    let mut protocols = BTreeMap::new();
    for members in components(&tail_calls) {
        // The vocabulary is a fact of the *class*, exactly as the width is: a member that returns only by tail call carries no edge of its own, and its callers must still rebuild what the class actually hands back.
        let mut shape = None;
        let mut agreed = true;
        for &function in &members {
            let sentinel = module.function(function).unwrap().return_cont;
            for node_id in function_nodes(module, function) {
                for edge in return_edges(module.node(node_id).unwrap(), sentinel) {
                    let [CpsAtom::Value(value)] = edge.args.as_slice() else {
                        continue;
                    };
                    let Some((_, row)) = constructions.get(value) else {
                        continue;
                    };
                    let seen = row.map_or(ReturnShape::Tuple, ReturnShape::Row);
                    agreed &= shape.is_none_or(|prior| prior == seen);
                    shape = Some(seen);
                }
            }
        }
        let class = members.iter().copied().collect::<BTreeSet<_>>();
        let admissible = members
            .iter()
            .all(|function| !calls.escaping.contains(function) && !pinned.contains(function))
            && members
                .iter()
                .filter_map(|function| resumes.get(function))
                .flatten()
                .all(|resume| {
                    entries.get(resume).is_some_and(|sources| {
                        sources
                            .iter()
                            .all(|source| source.is_some_and(|from| class.contains(&from)))
                    })
                });
        let mut demand = Demand::bottom();
        for function in &members {
            demand.join(
                demanded
                    .get(function)
                    .cloned()
                    .unwrap_or_else(Demand::bottom),
            );
        }
        let protocol = match (admissible && agreed).then_some(demand).zip(shape) {
            // The slots are delivered in order from zero, so covering the demanded indices means covering everything up to the last of them, read or not.
            Some((Demand::Projected(indices), shape)) => match indices.last() {
                Some(&last) if last >= 1 => ReturnProtocol::Fields(last + 1, shape),
                _ => ReturnProtocol::Tuple,
            },
            _ => ReturnProtocol::Tuple,
        };
        protocols.extend(members.into_iter().map(|function| (function, protocol)));
    }
    protocols
}

/// Deliver the leading fields of a returned construction as several results, wherever [`return_protocols`] finds a class every caller takes apart.
///
/// Both sides are stated as local edits and the existing chain finishes the job. A return edge names the construction's fields instead of the construction, leaving that construction unread and so removable. A resume continuation takes the fields as parameters and rebuilds the tuple at its head, leaving the projections below to be forwarded through a construction that is now visible to them — which is precisely what they could not do while it was built inside the callee. Nothing here splices a node out of a chain, and nothing here deletes: the tuple that survives at each end is dead, and dead is what the following passes already handle.
pub(super) fn split_returns(module: &mut CpsModule) -> bool {
    let widths = return_protocols(module)
        .into_iter()
        .filter_map(|(function, protocol)| match protocol {
            ReturnProtocol::Fields(width, shape) => Some((function, (width, shape))),
            ReturnProtocol::Tuple => None,
        })
        .collect::<BTreeMap<_, _>>();
    if widths.is_empty() {
        return false;
    }
    let constructions = constructions(module);

    let mut returning = Vec::new();
    for (&function, &(width, _)) in &widths {
        let sentinel = module.function(function).unwrap().return_cont;
        for node_id in function_nodes(module, function) {
            if !return_edges(module.node(node_id).unwrap(), sentinel).is_empty() {
                returning.push((node_id, sentinel, width));
            }
        }
    }
    for (node_id, sentinel, width) in returning {
        let mut node = module.node(node_id).unwrap().clone();
        for edge in return_edges_mut(&mut node, sentinel) {
            edge.args = split_fields(&constructions, &edge.args, width);
        }
        module.nodes.set(node_id, node);
    }

    let mut resuming = BTreeMap::new();
    for (_, node) in module.nodes.iter_live() {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            return_to,
            ..
        } = node
            && let Some(&(width, shape)) = widths.get(callee)
        {
            resuming.insert(*return_to, (width, shape));
        }
    }
    for (resume, (width, shape)) in resuming {
        // A tail call names its function's sentinel, which is bodyless and has no parameters to widen — the caller's own return edges carry the class's width already.
        let Some(definition) = module.continuation(resume) else {
            continue;
        };
        let [held] = definition.params.as_slice() else {
            continue;
        };
        let (held, body) = (*held, definition.body);
        let params = (0..width)
            .map(|index| module.add_value(Some(format!("resume/{}/{index}", resume.index()))))
            .collect::<Vec<_>>();
        let mut atoms: Vec<CpsAtom> = params.iter().copied().map(CpsAtom::Value).collect();
        let rebuilt = module.add_node(CpsNode::LetValue {
            result: held,
            value: match shape {
                ReturnShape::Row(row) => {
                    // The protocol carries only the slots the demand asked for, so the rebuild fills the row's remaining width rather than widening the interface — a narrower interface is the whole point of splitting, and the slots past the demand are by construction unread.
                    atoms.resize(module.row(row).width(), CpsAtom::Filler);
                    CpsValueExpr::Row(row, atoms)
                }
                ReturnShape::Tuple => CpsValueExpr::Tuple(atoms),
            },
            next: body,
        });
        let definition = module.continuations.get_mut(resume).unwrap();
        definition.params = params;
        definition.body = rebuilt;
    }
    true
}

/// The first `width` fields of the construction `args` names, filled out where the constructor is shorter than the class's width.
fn split_fields(
    constructions: &BTreeMap<CpsValueId, (Vec<CpsAtom>, Option<CpsRowId>)>,
    args: &[CpsAtom],
    width: usize,
) -> Vec<CpsAtom> {
    let [CpsAtom::Value(value)] = args else {
        return args.to_vec();
    };
    let Some((fields, _)) = constructions.get(value) else {
        return args.to_vec();
    };
    (0..width)
        .map(|index| fields.get(index).cloned().unwrap_or(CpsAtom::Filler))
        .collect()
}

/// What transfers into each continuation: the known function whose call resumes there, or `None` for an entry that is anything else.
fn entries(module: &CpsModule) -> BTreeMap<CpsContId, Vec<Option<CpsFunId>>> {
    let mut output = BTreeMap::<CpsContId, Vec<Option<CpsFunId>>>::new();
    for (_, node) in module.nodes.iter_live() {
        match node {
            CpsNode::ApplyFun {
                callee, return_to, ..
            } => {
                let from = match callee {
                    CpsCallee::Known(callee) => Some(*callee),
                    CpsCallee::Closure(_) => None,
                };
                output.entry(*return_to).or_default().push(from);
            }
            CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                output.entry(*return_to).or_default().push(None);
            }
            CpsNode::ApplyCont(edge) => output.entry(edge.target).or_default().push(None),
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.as_ref()) {
                    output.entry(edge.target).or_default().push(None);
                }
            }
            _ => {}
        }
    }
    output
}

/// The edges of `node` that transfer to `sentinel`, to be rewritten in place.
fn return_edges_mut(node: &mut CpsNode, sentinel: CpsContId) -> Vec<&mut CpsEdge> {
    let edges: Vec<&mut CpsEdge> = match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => {
            cases.values_mut().chain(default.as_mut()).collect()
        }
        _ => vec![],
    };
    edges
        .into_iter()
        .filter(|edge| edge.target == sentinel)
        .collect()
}

/// The edges of `node` that transfer to `sentinel` — a function's returns, whether it jumps to one or switches into several.
fn return_edges(node: &CpsNode, sentinel: CpsContId) -> Vec<&CpsEdge> {
    let edges: Vec<&CpsEdge> = match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => cases.values().chain(default.as_ref()).collect(),
        _ => vec![],
    };
    edges
        .into_iter()
        .filter(|edge| edge.target == sentinel)
        .collect()
}

/// The fields of every aggregate built in the module, by the value the construction binds, with the row a variant construction belongs to. A variant is visible here for the same reason a tuple is — a return edge naming one is a class this rewrite can split — and the row rides along so the resume rebuilds in the vocabulary the reads below it use.
fn constructions(module: &CpsModule) -> BTreeMap<CpsValueId, (Vec<CpsAtom>, Option<CpsRowId>)> {
    let mut output = BTreeMap::new();
    for (_, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Tuple(fields),
                ..
            } => {
                output.insert(*result, (fields.clone(), None));
            }
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Row(row, fields),
                ..
            } => {
                output.insert(*result, (fields.clone(), Some(*row)));
            }
            _ => {}
        }
    }
    output
}

/// The connected components of an undirected graph given as its adjacency, each listed in `CpsFunId` order.
fn components(edges: &BTreeMap<CpsFunId, BTreeSet<CpsFunId>>) -> Vec<Vec<CpsFunId>> {
    let mut seen = BTreeSet::new();
    let mut output = Vec::new();

    for &root in edges.keys() {
        if !seen.insert(root) {
            continue;
        }
        let mut members = vec![root];
        let mut work = vec![root];
        while let Some(function) = work.pop() {
            for &next in edges.get(&function).into_iter().flatten() {
                if seen.insert(next) {
                    members.push(next);
                    work.push(next);
                }
            }
        }
        members.sort();
        output.push(members);
    }
    output
}

#[cfg(test)]
mod tests;
