//! Snapshot-based join specialization: equivalent entries share a peeled copy, and dependent joins wait for fresh facts.

use {
    super::tagged_row_values,
    crate::{
        Atom, BRANCH_SPECIALIZATION_GROWTH_LIMIT, ContinuationId, CopyBudget, CopyPlan,
        CopyPriority, Edge, Intrinsic, Literal, Module, Node, NodeId, RowId, ValueExpr,
        collect_control_targets, continuation_transfers, copy_bodies, nodes_from,
    },
    curios_num::Natural,
    curios_utilities::ArenaId,
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Pattern {
    index: usize,
    tag: u32,
    width: usize,
    row: RowId,
}

impl Pattern {
    fn matches(&self, edge: &Edge, constructors: &Constructors) -> bool {
        let Some(Atom::Value(value)) = edge.args.get(self.index) else {
            return false;
        };
        constructors.get(value).is_some_and(|(tag, fields, row)| {
            *tag == self.tag && fields.len() == self.width && *row == self.row
        })
    }
}

type Constructors = BTreeMap<crate::ValueId, (u32, Vec<Atom>, RowId)>;

struct Candidate {
    target: ContinuationId,
    pattern: Pattern,
    intro: NodeId,
    extent: BTreeSet<NodeId>,
    carriers: BTreeSet<NodeId>,
    priority: CopyPriority,
}

fn edges(node: &Node) -> Vec<&Edge> {
    match node {
        Node::ApplyCont(edge) => vec![edge],
        Node::Switch { cases, default, .. } => cases.values().chain(default.iter()).collect(),
        _ => vec![],
    }
}

/// Apply independent patterns admitted by one snapshot, in sharing-per-copy order. The complete extent plus the head rebuild is charged against both the individual ceiling and the module's non-refundable allowance.
///
/// One pattern per generic join lets ordinary cleanup reconsider its remaining transfers. An earlier rewrite invalidates a candidate when it changes the copied source or introduces transfers into the join; compatible lexical-anchor appends remain admissible. Newly minted entries wait for the next snapshot. Seeded backedges retain the generic join.
pub(crate) fn specialize_jump_patterns(module: &mut Module, budget: &mut CopyBudget) -> bool {
    if !budget.admits(1) {
        return false;
    }
    let constructors = tagged_row_values(module);
    if constructors.is_empty() {
        return false;
    }
    let transfers = continuation_transfers(module);
    let intros = module
        .live_nodes()
        .flat_map(|(id, node)| match node {
            Node::LetCont { continuations, .. } => {
                continuations.iter().map(|target| (*target, id)).collect()
            }
            _ => Vec::new(),
        })
        .collect::<BTreeMap<_, _>>();
    let mut roots = BTreeMap::new();
    for (target, definition) in module.live_continuations() {
        let Some(&intro) = intros.get(&target) else {
            continue;
        };
        if transfers.get(&target).is_none_or(|sites| sites.len() < 2) {
            continue;
        }
        let extent = CopyPlan::continuation(module, target).nodes;
        if extent.len() + 1 > BRANCH_SPECIALIZATION_GROWTH_LIMIT {
            continue;
        }
        let projected = nodes_from(module, definition.body)
            .into_iter()
            .filter_map(|id| match module.node(id).unwrap() {
                Node::LetIntrinsic {
                    op: Intrinsic::RowGet(..),
                    args,
                    ..
                } => match args.first() {
                    Some(Atom::Value(value)) => Some(*value),
                    _ => None,
                },
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        roots.insert(target, (intro, extent, projected));
    }
    let mut grouped = BTreeMap::<(ContinuationId, Pattern), BTreeSet<NodeId>>::new();
    for (id, node) in module.live_nodes() {
        for edge in edges(node) {
            let Some((_, _, projected)) = roots.get(&edge.target) else {
                continue;
            };
            let Some(definition) = module.continuation(edge.target) else {
                continue;
            };
            for (index, arg) in edge.args.iter().enumerate() {
                let Atom::Value(value) = arg else { continue };
                let Some((tag, fields, row)) = constructors.get(value) else {
                    continue;
                };
                let Some(param) = definition.params.get(index) else {
                    continue;
                };
                if !projected.contains(param) {
                    continue;
                }
                grouped
                    .entry((
                        edge.target,
                        Pattern {
                            index,
                            tag: *tag,
                            width: fields.len(),
                            row: *row,
                        },
                    ))
                    .or_default()
                    .insert(id);
            }
        }
    }
    let mut candidates = Vec::new();
    for ((target, pattern), carriers) in grouped {
        let (intro, extent, _) = &roots[&target];
        let nodes = extent.len() + 1;
        if nodes > BRANCH_SPECIALIZATION_GROWTH_LIMIT || !budget.admits(nodes) {
            continue;
        }
        candidates.push(Candidate {
            target,
            pattern,
            intro: *intro,
            extent: extent.clone(),
            priority: CopyPriority {
                sites: carriers.len(),
                nodes,
                first: carriers.first().unwrap().index(),
            },
            carriers,
        });
    }
    candidates.sort_by_key(|candidate| candidate.priority);
    let mut changed = false;
    let mut touched = BTreeSet::new();
    let mut changed_entries = BTreeSet::new();
    let mut selected = BTreeSet::new();
    for candidate in candidates {
        if selected.contains(&candidate.target)
            || changed_entries.contains(&candidate.target)
            || !candidate.extent.is_disjoint(&touched)
            || !budget.admits(candidate.priority.nodes)
            || !matches!(module.node(candidate.intro), Some(Node::LetCont { continuations, .. }) if continuations.contains(&candidate.target))
        {
            continue;
        }
        let live = transfers[&candidate.target]
            .iter()
            .filter(|id| {
                let mut targets = BTreeSet::new();
                collect_control_targets(module.node(**id).unwrap(), &mut targets);
                targets.contains(&candidate.target)
            })
            .count();
        if live < 2 {
            continue;
        }
        let carriers = candidate
            .carriers
            .iter()
            .copied()
            .filter(|id| {
                edges(module.node(*id).unwrap()).iter().any(|edge| {
                    edge.target == candidate.target
                        && candidate.pattern.matches(edge, &constructors)
                })
            })
            .collect::<Vec<_>>();
        if carriers.is_empty() {
            continue;
        }
        let plan = CopyPlan::continuation(module, candidate.target);
        if plan.nodes != candidate.extent {
            continue;
        }
        let first_node = module.nodes().len();
        let clone = copy_bodies(module, plan).continuations[&candidate.target];
        let definition = module.continuation(clone).unwrap();
        let mut params = definition.params.clone();
        let old_param = params[candidate.pattern.index];
        let body = definition.body;
        let fields = (1..candidate.pattern.width)
            .map(|index| module.add_value(Some(format!("field#{index}"))))
            .collect::<Vec<_>>();
        let mut rebuilt = vec![Atom::Literal(Literal::Nat(Natural::from(
            candidate.pattern.tag,
        )))];
        rebuilt.extend(fields.iter().copied().map(Atom::Value));
        let head = module.add_node(Node::LetValue {
            result: old_param,
            value: ValueExpr::Row(candidate.pattern.row, rebuilt),
            next: body,
        });
        params.splice(candidate.pattern.index..=candidate.pattern.index, fields);
        let definition = module.continuation_mut(clone).unwrap();
        definition.params = params;
        definition.body = head;
        let Node::LetCont { continuations, .. } = module.node_mut(candidate.intro).unwrap() else {
            unreachable!()
        };
        continuations.push(clone);
        touched.insert(candidate.intro);
        for id in carriers {
            let node = module.node_mut(id).unwrap();
            let repoint = |edge: &mut Edge| {
                if edge.target == candidate.target && candidate.pattern.matches(edge, &constructors)
                {
                    let Atom::Value(value) = edge.args[candidate.pattern.index] else {
                        unreachable!()
                    };
                    let fields = &constructors[&value].1;
                    edge.target = clone;
                    edge.args.splice(
                        candidate.pattern.index..=candidate.pattern.index,
                        fields[1..].iter().cloned(),
                    );
                }
            };
            match node {
                Node::ApplyCont(edge) => repoint(edge),
                Node::Switch { cases, default, .. } => {
                    for edge in cases.values_mut().chain(default.iter_mut()) {
                        repoint(edge);
                    }
                }
                _ => unreachable!(),
            }
            touched.insert(id);
        }
        for index in first_node..module.nodes().len() {
            collect_control_targets(
                module.node(NodeId::from_index(index)).unwrap(),
                &mut changed_entries,
            );
        }
        selected.insert(candidate.target);
        budget.charge(candidate.priority.nodes);
        changed = true;
    }
    changed
}

#[cfg(test)]
mod tests;
