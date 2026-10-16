//! Call-pattern specialization uses one constructor/caller snapshot and gives equivalent sites a shared peeled worker.

use {
    super::{ConstructorPattern, tagged_row_values},
    crate::{
        Atom, BRANCH_SPECIALIZATION_GROWTH_LIMIT, Callee, CopyBudget, CopyPlan, CopyPriority,
        FunctionId, Intrinsic, Literal, Module, Node, NodeId, ValueExpr, ValueId, analyze_calls,
        copy_bodies, function_nodes, visit_atoms_mut,
    },
    curios_num::Natural,
    curios_utilities::ArenaId,
    std::collections::{BTreeMap, BTreeSet},
};

struct Candidate {
    callee: FunctionId,
    pattern: ConstructorPattern,
    intro: NodeId,
    extent: BTreeSet<NodeId>,
    sites: Vec<NodeId>,
    priority: CopyPriority,
}

/// Apply independent known-constructor contexts from one snapshot, selecting one context per generic callee so cleanup can reconsider its remaining callers. The complete extent and reconstruction node are charged only after application. New calls or changed source bodies defer dependent callees.
pub(crate) fn specialize_call_patterns(module: &mut Module, budget: &mut CopyBudget) -> bool {
    if !budget.admits(1) {
        return false;
    }
    let constructors = tagged_row_values(module);
    if constructors.is_empty() {
        return false;
    }
    let calls = analyze_calls(module);
    let intros = module
        .live_nodes()
        .flat_map(|(id, node)| match node {
            Node::LetFun { functions, .. } => {
                functions.iter().map(|function| (*function, id)).collect()
            }
            _ => Vec::new(),
        })
        .collect::<BTreeMap<_, _>>();
    let mut roots = BTreeMap::new();
    for (&callee, sites) in &calls.call_sites {
        if sites.is_empty() || Some(callee) == module.entry() || module.is_out_of_line(callee) {
            continue;
        }
        let Some(&intro) = intros.get(&callee) else {
            continue;
        };
        let Some(plan) = CopyPlan::functions_with_limit(
            module,
            BTreeSet::from([callee]),
            BRANCH_SPECIALIZATION_GROWTH_LIMIT - 1,
        ) else {
            continue;
        };
        let extent = plan.nodes;
        let projected = function_nodes(module, callee)
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
        roots.insert(callee, (intro, extent, projected));
    }
    let mut grouped = BTreeMap::<(FunctionId, ConstructorPattern), Vec<NodeId>>::new();
    for (&callee, (_, _, projected)) in &roots {
        let params = &module.function(callee).unwrap().params;
        for &id in &calls.call_sites[&callee] {
            let Node::ApplyFun { args, .. } = module.node(id).unwrap() else {
                unreachable!()
            };
            for (index, arg) in args.iter().enumerate() {
                let Atom::Value(value) = arg else { continue };
                let Some((tag, fields, row)) = constructors.get(value) else {
                    continue;
                };
                if !params
                    .get(index)
                    .is_some_and(|param| projected.contains(param))
                {
                    continue;
                }
                grouped
                    .entry((
                        callee,
                        ConstructorPattern {
                            index,
                            tag: *tag,
                            width: fields.len(),
                            row: *row,
                        },
                    ))
                    .or_default()
                    .push(id);
            }
        }
    }
    let mut candidates = grouped
        .into_iter()
        .filter_map(|((callee, pattern), mut sites)| {
            sites.sort_unstable();
            let (intro, extent, _) = &roots[&callee];
            let nodes = extent.len() + 1;
            budget.admits(nodes).then(|| Candidate {
                callee,
                pattern,
                intro: *intro,
                extent: extent.clone(),
                priority: CopyPriority {
                    sites: sites.len(),
                    nodes,
                    first: sites[0].index(),
                },
                sites,
            })
        })
        .collect::<Vec<_>>();
    candidates.sort_by_key(|candidate| candidate.priority);
    let mut touched = BTreeSet::new();
    let mut changed_callers = BTreeSet::new();
    let mut selected = BTreeSet::new();
    let mut changed = false;
    for candidate in candidates {
        if selected.contains(&candidate.callee)
            || changed_callers.contains(&candidate.callee)
            || !candidate.extent.is_disjoint(&touched)
            || !budget.admits(candidate.priority.nodes)
            || !matches!(module.node(candidate.intro), Some(Node::LetFun { functions, .. }) if functions.contains(&candidate.callee))
        {
            continue;
        }
        let sites = candidate.sites.iter().copied().filter(|id| {
            matches!(module.node(*id), Some(Node::ApplyFun { callee: Callee::Known(target), args, .. }) if *target == candidate.callee && candidate.pattern.fields(args, &constructors).is_some())
        }).collect::<Vec<_>>();
        if sites.is_empty() {
            continue;
        }
        let plan = CopyPlan::functions(module, BTreeSet::from([candidate.callee]));
        if plan.nodes != candidate.extent {
            continue;
        }
        let first_node = module.nodes().len();
        let clone = apply_copy(
            module,
            candidate.callee,
            candidate.intro,
            candidate.pattern,
            plan,
        );
        touched.insert(candidate.intro);
        for id in sites {
            let Node::ApplyFun { callee, args, .. } = module.node_mut(id).unwrap() else {
                unreachable!()
            };
            let fields = candidate.pattern.fields(args, &constructors).unwrap();
            let passed = fields[1..].to_vec();
            *callee = Callee::Known(clone);
            args.splice(candidate.pattern.index..=candidate.pattern.index, passed);
            touched.insert(id);
        }
        for index in first_node..module.nodes().len() {
            if let Node::ApplyFun {
                callee: Callee::Known(target),
                ..
            } = module.node(NodeId::from_index(index)).unwrap()
            {
                changed_callers.insert(*target);
            }
        }
        selected.insert(candidate.callee);
        budget.charge(candidate.priority.nodes);
        changed = true;
    }
    changed
}

fn apply_copy(
    module: &mut Module,
    callee: FunctionId,
    intro: NodeId,
    pattern: ConstructorPattern,
    plan: CopyPlan,
) -> FunctionId {
    let clones = copy_bodies(module, plan).functions;
    let clone = clones[&callee];

    // Peel: the clone recurses into the general function, not itself, so a recursive call that does not carry the matched constructor stays valid.
    //
    // Over every copied function rather than the entry alone. The entry's parameters are about to be respliced, so a call to it that kept the old argument list would not merely be unpeeled — it would carry the wrong arity, and a definition copied from inside the entry's own body is exactly where such a call can hide.
    let peeled = clones
        .values()
        .flat_map(|&id| function_nodes(module, id))
        .collect::<Vec<_>>();
    for node_id in peeled {
        let node = module.node_mut(node_id).unwrap();
        if let Node::ApplyFun {
            callee: Callee::Known(target),
            ..
        } = node
            && *target == clone
        {
            *target = callee;
        }
        visit_atoms_mut(node, &mut |atom| {
            if let Atom::Fun(function) = atom
                && *function == clone
            {
                *atom = Atom::Fun(callee);
            }
        });
    }

    // Rebuild the constructor at the clone entry, threading its dynamic fields as fresh parameters in place of the specialized parameter.
    let clone_function = module.function(clone).unwrap();
    let mut params = clone_function.params.clone();
    let clone_body = clone_function.body;
    let old_param = params[pattern.index];
    let field_params: Vec<ValueId> = (1..pattern.width)
        .map(|field| module.add_value(Some(format!("field#{field}"))))
        .collect();
    let mut rebuilt = Vec::with_capacity(pattern.width);
    rebuilt.push(Atom::Literal(Literal::Nat(Natural::from(pattern.tag))));
    rebuilt.extend(field_params.iter().map(|&p| Atom::Value(p)));
    let entry = module.add_node(Node::LetValue {
        result: old_param,
        value: ValueExpr::Row(pattern.row, rebuilt),
        next: clone_body,
    });
    params.splice(pattern.index..=pattern.index, field_params);
    let clone_function = module.function_mut(clone).unwrap();
    clone_function.params = params;
    clone_function.body = entry;

    // Introduce the clone in the callee's lexical scope.
    if let Some(Node::LetFun { functions, .. }) = module.node_mut(intro) {
        functions.push(clone);
    }

    clone
}

#[cfg(test)]
mod tests;
