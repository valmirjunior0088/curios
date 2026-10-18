//! Recursive specialization shares one call graph and rechecks remaining contexts within each component.

use {
    super::{
        eligible_sccs, function_introductions, invariant_fixpoint, literal_value_map,
        scc_invariant_knowns, useful_knowns,
    },
    crate::{
        Atom, CallAnalysis, Callee, CopyBudget, CopyPlan, CopyPriority, FunctionId, Literal,
        Module, Node, NodeId, SCC_CLONE_NODE_LIMIT, ValueId, analyze_calls, copy_bodies,
    },
    curios_utilities::ArenaId,
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        hash::{Hash, Hasher},
        mem::discriminant,
    },
};

/// Context equality is the existing argument equality, including bitwise floating identity. Hashing is used only for lookup; the encounter vector and explicit priority determine traversal.
#[derive(Clone, PartialEq)]
struct Context {
    entry: FunctionId,
    args: Vec<Atom>,
}

impl Eq for Context {}

impl Hash for Context {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.entry.hash(state);
        self.args.len().hash(state);
        for atom in &self.args {
            discriminant(atom).hash(state);
            match atom {
                Atom::Value(value) => value.hash(state),
                Atom::Fun(function) => function.hash(state),
                Atom::Literal(literal) => {
                    discriminant(literal).hash(state);
                    match literal {
                        Literal::Nat(value) => value.hash(state),
                        Literal::Int(value) => value.hash(state),
                        Literal::Flt(value) => value.hash(state),
                        Literal::Bytes(value) => value.hash(state),
                    }
                }
                Atom::Filler => {}
            }
        }
    }
}

struct Group {
    context: Context,
    sites: Vec<NodeId>,
}

struct Component {
    members: BTreeSet<FunctionId>,
    params: BTreeMap<FunctionId, Vec<ValueId>>,
    owners: BTreeMap<ValueId, FunctionId>,
    internal: Vec<(FunctionId, Vec<Atom>)>,
    external: Vec<NodeId>,
    groups: Vec<Group>,
    intro: NodeId,
    extent: BTreeSet<NodeId>,
    baseline: BTreeMap<ValueId, Atom>,
    disabled: bool,
}

impl Component {
    fn knowns(
        &self,
        constraints: &[(FunctionId, Vec<Atom>)],
        seeds: &BTreeMap<ValueId, Atom>,
        analysis: &CallAnalysis,
    ) -> BTreeMap<ValueId, Atom> {
        useful_knowns(invariant_fixpoint(&self.params, constraints, seeds))
            .into_iter()
            .filter(|(param, atom)| match atom {
                Atom::Fun(function) => analysis
                    .lexical_scope
                    .get(&self.owners[param])
                    .is_some_and(|scope| scope.contains(function)),
                _ => true,
            })
            .collect()
    }

    fn remaining(&self, module: &Module) -> Vec<(FunctionId, Vec<Atom>)> {
        let mut constraints = self.internal.clone();
        constraints.extend(
            self.external
                .iter()
                .filter_map(|id| match module.node(*id).unwrap() {
                    Node::ApplyFun {
                        callee: Callee::Known(callee),
                        args,
                        ..
                    } if self.members.contains(callee) => Some((*callee, args.clone())),
                    _ => None,
                }),
        );
        constraints
    }
}

struct Candidate {
    component: usize,
    group: usize,
    priority: CopyPriority,
}

/// Apply independent SCC contexts under a lifetime copied-node allowance. Remaining callers are re-solved locally after each copy; newly established generic facts stop that component for cleanup. A changed source or newly copied incoming call defers dependent components, without rebuilding the module-wide call analysis.
pub(crate) fn specialize_scc_calls(module: &mut Module, budget: &mut CopyBudget) -> bool {
    if !budget.admits(1) {
        return false;
    }
    let analysis = analyze_calls(module);
    let mut seeds = literal_value_map(module);
    let global = scc_invariant_knowns(module, &analysis, &seeds);
    seeds.extend(global);
    let intros = function_introductions(module);
    let mut components = Vec::new();
    let mut candidates = Vec::new();
    for members in eligible_sccs(module, &analysis) {
        let members = members.into_iter().collect::<BTreeSet<_>>();
        let Some(copy) =
            CopyPlan::functions_with_limit(module, members.clone(), SCC_CLONE_NODE_LIMIT)
        else {
            continue;
        };
        if !budget.admits(copy.nodes.len()) {
            continue;
        }
        let Some(&intro) = intros.get(members.first().unwrap()) else {
            continue;
        };
        if members
            .iter()
            .any(|member| intros.get(member) != Some(&intro))
        {
            continue;
        }
        let params = members
            .iter()
            .map(|member| (*member, module.function(*member).unwrap().params.clone()))
            .collect::<BTreeMap<_, _>>();
        let owners = params
            .iter()
            .flat_map(|(owner, params)| params.iter().map(|param| (*param, *owner)))
            .collect();
        let mut internal = Vec::new();
        let mut external = Vec::new();
        let mut groups: Vec<Group> = Vec::new();
        let mut indices = HashMap::new();
        for &member in &members {
            for &id in &analysis.call_sites[&member] {
                let Node::ApplyFun { args, .. } = module.node(id).unwrap() else {
                    unreachable!()
                };
                if members.contains(&analysis.node_owners[&id]) {
                    internal.push((member, args.clone()));
                } else {
                    external.push(id);
                    let context = Context {
                        entry: member,
                        args: args.clone(),
                    };
                    let index = *indices.entry(context.clone()).or_insert_with(|| {
                        let index = groups.len();
                        groups.push(Group {
                            context,
                            sites: Vec::new(),
                        });
                        index
                    });
                    groups[index].sites.push(id);
                }
            }
        }
        external.sort_unstable();
        let mut component = Component {
            members,
            params,
            owners,
            internal,
            external,
            groups,
            intro,
            extent: copy.nodes,
            baseline: BTreeMap::new(),
            disabled: false,
        };
        component.baseline = component.knowns(&component.remaining(module), &seeds, &analysis);
        for (index, group) in component.groups.iter().enumerate() {
            let mut constraints = component.internal.clone();
            constraints.push((group.context.entry, group.context.args.clone()));
            let context = component.knowns(&constraints, &seeds, &analysis);
            if context
                .iter()
                .any(|(param, atom)| component.baseline.get(param) != Some(atom))
            {
                candidates.push(Candidate {
                    component: components.len(),
                    group: index,
                    priority: CopyPriority {
                        sites: group.sites.len(),
                        nodes: component.extent.len(),
                        first: group.sites.iter().map(|id| id.index()).min().unwrap(),
                    },
                });
            }
        }
        components.push(component);
    }
    candidates.sort_by_key(|candidate| candidate.priority);
    let mut touched = BTreeSet::new();
    let mut changed_callers = BTreeSet::new();
    let mut changed = false;
    for candidate in candidates {
        let component = &mut components[candidate.component];
        if component.disabled
            || !component.extent.is_disjoint(&touched)
            || !component.members.is_disjoint(&changed_callers)
            || !budget.admits(candidate.priority.nodes)
            || !matches!(module.node(component.intro), Some(Node::LetFun { functions, .. }) if component.members.iter().all(|member| functions.contains(member)))
        {
            continue;
        }
        let group = &component.groups[candidate.group];
        let sites = group.sites.iter().copied().filter(|id| {
            matches!(module.node(*id), Some(Node::ApplyFun { callee: Callee::Known(callee), args, .. }) if *callee == group.context.entry && *args == group.context.args)
        }).collect::<Vec<_>>();
        if sites.is_empty() {
            continue;
        }
        let Some(plan) =
            CopyPlan::functions_with_limit(module, component.members.clone(), SCC_CLONE_NODE_LIMIT)
        else {
            continue;
        };
        if plan.nodes != component.extent {
            continue;
        }
        let first_node = module.nodes().len();
        let copies = copy_bodies(module, plan).functions;
        let clone = copies[&group.context.entry];
        let Node::LetFun { functions, .. } = module.node_mut(component.intro).unwrap() else {
            unreachable!()
        };
        functions.extend(component.members.iter().map(|member| copies[member]));
        touched.insert(component.intro);
        for id in sites {
            let Node::ApplyFun { callee, .. } = module.node_mut(id).unwrap() else {
                unreachable!()
            };
            *callee = Callee::Known(clone);
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
        budget.charge(candidate.priority.nodes);
        changed = true;
        component.disabled = !component.extent.is_disjoint(&touched)
            || component.knowns(&component.remaining(module), &seeds, &analysis)
                != component.baseline;
    }
    changed
}

#[cfg(test)]
mod tests;
