//! Continuation scalar replacement: a tuple that travels a join point as one aggregate parameter becomes that many field parameters, and the record in [`FieldGroup`] is what makes the change a fact of the program.
//!
//! Admission composes the two halves the value-lifetime specification keeps separate: the backward half says every use of the parameter is a projection or an eligible transfer (`demands`, `Projected`), and the forward half says every flow reaching it is a construction of one exact arity or an alias of one (`origins`, `Exact`). Loop backedges are the central case rather than an exclusion — an edge carrying the join's own parameter reads as the constructions that entered it, which is precisely what the forward fixpoint establishes.
//!
//! The rewrite is three local edits, and the existing chain finishes the job, exactly as `split_returns` works: the parameter list is spliced and the group recorded; the continuation's head rebuilds the aggregate from the new field parameters and every occurrence of the old parameter is redirected to that rebuild; and every incoming edge projects its argument into fields above the jump. Projection forwarding then collapses the inserted reads through the constructions they see, dead-binding elimination removes the constructions nothing reads any more — and the head rebuild survives exactly where a whole-value use survives, which makes it the materialization boundary the cost contract prescribes rather than a leak.
//!
//! Resume continuations are excluded: their parameter list is the call interface the return protocol owns. Splitting inside an already-recorded group is declined — one aggregate is exposed one level per round at fresh joins, and the growth ceiling is what stops recursive structures from flattening without end.

#[cfg(test)]
mod tests;

use {
    super::{
        CpsAtom, CpsContId, CpsEdge, CpsIntrinsicOp, CpsModule, CpsNode, CpsUseTarget,
        CpsValueExpr, CpsValueId, Demand, FieldGroup, Origin, demand_of, demands,
        optimize::PARAM_SPLIT_GROWTH_LIMIT, origins, simplify::rewire_node,
    },
    std::collections::BTreeSet,
};

/// One admitted split: which parameter of which continuation, and the exact arity every flow agrees on.
struct Split {
    continuation: CpsContId,
    position: usize,
    param: CpsValueId,
    arity: usize,
}

/// The first admissible split in deterministic order, if any.
fn admit(module: &CpsModule) -> Option<Split> {
    let demands = demands(module);
    let origins = origins(module);

    // A resume's parameters are the call interface the return protocol owns, whatever their demand says.
    let resumes = module
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

    for (continuation, definition) in module.continuations.iter_live() {
        if resumes.contains(&continuation) {
            continue;
        }
        let recorded = module.field_groups().get(&continuation);
        for (position, &param) in definition.params.iter().enumerate() {
            if recorded.is_some_and(|groups| {
                groups
                    .iter()
                    .any(|group| position >= group.start && position < group.start + group.width)
            }) {
                continue;
            }
            let Origin::Exact(arity) = origins.get(&param).copied().unwrap_or(Origin::Opaque)
            else {
                continue;
            };
            let Demand::Projected(read) = demand_of(&demands, param) else {
                continue;
            };
            if arity == 0 || read.last().is_some_and(|&last| last >= arity) {
                continue;
            }
            if definition.params.len() - 1 + arity > PARAM_SPLIT_GROWTH_LIMIT {
                continue;
            }
            return Some(Split {
                continuation,
                position,
                param,
                arity,
            });
        }
    }
    None
}

/// Split one admissible continuation parameter into its fields, record the group, and leave the cleanup to the chain. One split per invocation keeps the pass deterministic and lets the optimizer's own fixpoint drive region-wide convergence; termination is independent of the round limit because every split consumes an unrecorded aggregate parameter and the growth ceiling bounds how many parameters any continuation can accrue.
pub(super) fn split_parameters(module: &mut CpsModule) -> bool {
    let Some(split) = admit(module) else {
        return false;
    };

    // The field parameters, and the group that records them.
    let fields = (0..split.arity)
        .map(|index| {
            module.add_value(Some(format!(
                "field/{}/{index}",
                split.continuation.index()
            )))
        })
        .collect::<Vec<_>>();
    let definition = module
        .continuations
        .get_mut(split.continuation)
        .expect("admitted continuation is live");
    definition
        .params
        .splice(split.position..=split.position, fields.iter().copied());
    let body = definition.body;
    module.record_field_group(
        split.continuation,
        FieldGroup {
            start: split.position,
            width: split.arity,
        },
    );
    // Existing groups after the spliced position widen their offsets by the net parameter growth.
    let groups = module
        .field_groups
        .get_mut(&split.continuation)
        .expect("the group was just recorded");
    for group in groups.iter_mut() {
        if group.start > split.position {
            group.start += split.arity - 1;
        }
    }
    groups.sort_by_key(|group| group.start);

    // The head rebuild: the aggregate reconstructed from its fields, standing in for the old parameter everywhere. It survives exactly where a whole-value use survives, and is the materialization the cost contract allows at such a boundary.
    let rebuilt = module.add_value(Some(format!("rebuilt/{}", split.continuation.index())));
    let head = module.add_node(CpsNode::LetValue {
        result: rebuilt,
        value: CpsValueExpr::Tuple(fields.iter().copied().map(CpsAtom::Value).collect()),
        next: body,
    });
    module
        .continuations
        .get_mut(split.continuation)
        .expect("admitted continuation is live")
        .body = head;
    module.replace_atom(CpsUseTarget::Value(split.param), CpsAtom::Value(rebuilt));
    module.values.remove(split.param);

    // Every incoming edge projects its argument into fields above the jump; forwarding collapses the reads through visible constructions on the next rounds.
    let carriers = module
        .nodes
        .iter_live()
        .filter(|(_, node)| {
            edges_of(node)
                .iter()
                .any(|edge| edge.target == split.continuation)
        })
        .map(|(id, _)| id)
        .collect::<Vec<_>>();
    for carrier in carriers {
        let mut node = module.node(carrier).expect("carrier is live").clone();
        let mut chain = Vec::new();
        for edge in edges_of_mut(&mut node) {
            if edge.target != split.continuation {
                continue;
            }
            let CpsAtom::Value(source) = &edge.args[split.position] else {
                unreachable!("an exact origin admits only value arguments");
            };
            let source = *source;
            let projections = (0..split.arity)
                .map(|index| module.add_value(Some(format!("field/{}/{index}", carrier.index()))))
                .collect::<Vec<_>>();
            for (index, &projection) in projections.iter().enumerate() {
                chain.push((projection, index, source));
            }
            edge.args.splice(
                split.position..=split.position,
                projections.iter().copied().map(CpsAtom::Value),
            );
        }
        let ids = chain
            .iter()
            .map(|_| module.reserve_node())
            .collect::<Vec<_>>();
        if let Some(&first) = ids.first() {
            rewire_node(module, carrier, first);
        }
        for (offset, (projection, index, source)) in chain.into_iter().enumerate() {
            let next = ids.get(offset + 1).copied().unwrap_or(carrier);
            module.define_node(
                ids[offset],
                CpsNode::LetIntrinsic {
                    result: projection,
                    op: CpsIntrinsicOp::TplGet(index),
                    args: vec![CpsAtom::Value(source)],
                    next,
                },
            );
        }
        module.nodes.set(carrier, node);
    }

    true
}

fn edges_of(node: &CpsNode) -> Vec<&CpsEdge> {
    match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => cases.values().chain(default.as_ref()).collect(),
        _ => vec![],
    }
}

fn edges_of_mut(node: &mut CpsNode) -> Vec<&mut CpsEdge> {
    match node {
        CpsNode::ApplyCont(edge) => vec![edge],
        CpsNode::Switch { cases, default, .. } => {
            cases.values_mut().chain(default.as_mut()).collect()
        }
        _ => vec![],
    }
}
