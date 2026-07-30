use {
    super::*,
    super::{
        analysis::{CallAnalysis, analyze_calls, function_nodes, resolve_atom},
        optimize::{BRANCH_SPECIALIZATION_GROWTH_LIMIT, SCC_CLONE_NODE_LIMIT},
    },
    std::collections::{BTreeMap, BTreeSet},
};

#[derive(Clone, PartialEq)]
pub(super) enum Knowledge {
    Unknown,
    Known(CpsAtom),
    Conflict,
}
impl Knowledge {
    fn merge(&mut self, incoming: Option<&CpsAtom>) {
        match (&*self, incoming) {
            (Self::Conflict, _) | (Self::Unknown, None) => {}
            (Self::Unknown, Some(atom)) => *self = Self::Known(atom.clone()),
            (Self::Known(_), None) => *self = Self::Conflict,
            (Self::Known(current), Some(incoming)) if current == incoming => {}
            (Self::Known(_), Some(_)) => *self = Self::Conflict,
        }
    }

    /// Lattice join for the SCC-invariant fixpoint, ordered `Unknown < Known(_) < Conflict`. Unlike `merge`, `Unknown` is the identity (not-yet-resolved), so a forwarded parameter still resolving to `Unknown` contributes nothing rather than forcing a conflict.
    fn join(&mut self, incoming: Knowledge) {
        *self = match (std::mem::replace(self, Knowledge::Unknown), incoming) {
            (Knowledge::Conflict, _) | (_, Knowledge::Conflict) => Knowledge::Conflict,
            (Knowledge::Unknown, other) | (other, Knowledge::Unknown) => other,
            (Knowledge::Known(current), Knowledge::Known(incoming)) => {
                if current == incoming {
                    Knowledge::Known(current)
                } else {
                    Knowledge::Conflict
                }
            }
        }
    }
}
/// Compute the parameters of recursive SCC members that are provably a single literal or function reference at every entry, so they can be substituted in place and dropped as dead. This is a monotone constant-propagation fixpoint over the whole known-callee call graph, restricted to literal/function atoms with parameter forwarding, ordered `Unknown < Known < Conflict`.
///
/// Only members of an eligible SCC participate: the SCC must be recursive and must contain no escaping member and not the program entry, because an escaping or host-called function receives arguments this analysis cannot observe. `known_literals` seeds resolution of caller values already known to be constant.
pub(super) fn scc_invariant_knowns(
    module: &CpsModule,
    analysis: &CallAnalysis,
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut params_of: BTreeMap<CpsFunId, Vec<CpsValueId>> = BTreeMap::new();
    for members in eligible_sccs(module, analysis) {
        for &function in &members {
            params_of.insert(function, module.function(function).unwrap().params.clone());
        }
    }
    if params_of.is_empty() {
        return BTreeMap::new();
    }

    let mut constraints: Vec<(CpsFunId, Vec<CpsAtom>)> = Vec::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            ..
        } = node
            && params_of.contains_key(callee)
        {
            constraints.push((*callee, args.clone()));
        }
    }

    let class = invariant_fixpoint(&params_of, &constraints, known_literals);
    useful_knowns(class)
}
/// The members of every SCC eligible for known-argument analysis: recursive, and containing neither an escaping member nor the program entry, because those receive arguments the analysis cannot observe.
pub(super) fn eligible_sccs(module: &CpsModule, analysis: &CallAnalysis) -> Vec<Vec<CpsFunId>> {
    analysis
        .sccs
        .members
        .iter()
        .filter(|members| {
            let recursive = members.len() > 1
                || members.iter().any(|function| {
                    analysis
                        .call_graph
                        .get(function)
                        .is_some_and(|edges| edges.contains(function))
                });
            let observable = members.iter().all(|function| {
                !analysis.escaping.contains(function) && Some(*function) != module.entry
            });
            recursive && observable
        })
        .cloned()
        .collect()
}
/// Run the monotone `Unknown < Known < Conflict` join to a fixpoint over the given parameter positions and call constraints.
pub(super) fn invariant_fixpoint(
    params_of: &BTreeMap<CpsFunId, Vec<CpsValueId>>,
    constraints: &[(CpsFunId, Vec<CpsAtom>)],
    known_literals: &BTreeMap<CpsValueId, CpsAtom>,
) -> BTreeMap<CpsValueId, Knowledge> {
    let mut class: BTreeMap<CpsValueId, Knowledge> = params_of
        .values()
        .flatten()
        .map(|&param| (param, Knowledge::Unknown))
        .collect();
    loop {
        let mut changed = false;
        for (callee, args) in constraints {
            let Some(params) = params_of.get(callee) else {
                continue;
            };
            for (index, arg) in args.iter().enumerate() {
                let Some(&param) = params.get(index) else {
                    continue;
                };
                let incoming = resolve_atom(arg, &class, known_literals);
                let mut updated = class[&param].clone();
                updated.join(incoming);
                if updated != class[&param] {
                    class.insert(param, updated);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    class
}
/// Extract the parameters resolved to a single literal or function reference.
pub(super) fn useful_knowns(
    class: BTreeMap<CpsValueId, Knowledge>,
) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut result = BTreeMap::new();
    for (param, knowledge) in class {
        if let Knowledge::Known(atom @ (CpsAtom::Literal(_) | CpsAtom::Fun(_))) = knowledge {
            result.insert(param, atom);
        }
    }
    result
}
/// Specialize a recursive SCC for one external call context whose known arguments the module-wide analysis cannot use because other callers disagree.
///
/// The SCC is cloned verbatim and the disagreeing call site (with any siblings passing the same arguments) is repointed to the private copy. The clone then has a single agreeing external caller, so the ordinary invariant-known propagation folds those arguments in place on a later iteration while the original stays polymorphic for its other callers. At most `SCC_CLONE_LIMIT` clones are made per module and only SCCs within `SCC_CLONE_NODE_LIMIT` live nodes are cloned. One clone is performed per call so the outer fixpoint stays deterministic.
pub(super) fn specialize_scc_calls(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let analysis = analyze_calls(module);
    let literals = literal_value_map(module);
    let global = scc_invariant_knowns(module, &analysis, &literals);

    for members in eligible_sccs(module, &analysis) {
        let member_set: BTreeSet<CpsFunId> = members.iter().copied().collect();
        let node_count: usize = members
            .iter()
            .map(|&m| function_nodes(module, m).len())
            .sum();
        if node_count > SCC_CLONE_NODE_LIMIT {
            continue;
        }
        let Some(intro) = introducing_letfun(module, &member_set) else {
            continue;
        };

        let params_of: BTreeMap<CpsFunId, Vec<CpsValueId>> = members
            .iter()
            .map(|&m| (m, module.function(m).unwrap().params.clone()))
            .collect();
        let mut internal: Vec<(CpsFunId, Vec<CpsAtom>)> = Vec::new();
        let mut external: Vec<(CpsNodeId, CpsFunId, Vec<CpsAtom>)> = Vec::new();
        for (&node_id, &owner) in &analysis.node_owners {
            let Some(CpsNode::ApplyFun {
                callee: CpsCallee::Known(callee),
                args,
                ..
            }) = module.node(node_id)
            else {
                continue;
            };
            if !member_set.contains(callee) {
                continue;
            }
            if member_set.contains(&owner) {
                internal.push((*callee, args.clone()));
            } else {
                external.push((node_id, *callee, args.clone()));
            }
        }

        // Find the first external context that unlocks a known the module-wide analysis could not, in deterministic call-site order.
        let mut chosen: Option<(CpsFunId, Vec<CpsAtom>)> = None;
        for (_, callee, args) in &external {
            let mut constraints = internal.clone();
            constraints.push((*callee, args.clone()));
            let context = useful_knowns(invariant_fixpoint(&params_of, &constraints, &literals));
            if context
                .iter()
                .any(|(param, atom)| global.get(param) != Some(atom))
            {
                chosen = Some((*callee, args.clone()));
                break;
            }
        }
        let Some((entry, context_args)) = chosen else {
            continue;
        };

        let Some(clones) = clone_scc(module, &member_set) else {
            continue;
        };
        let clone_entry = clones[&entry];
        if let Some(CpsNode::LetFun { functions, .. }) = module.nodes[intro.index()].as_mut() {
            functions.extend(clones.values().copied());
        }
        for (node_id, callee, args) in &external {
            if *callee == entry
                && *args == context_args
                && let Some(CpsNode::ApplyFun { callee, .. }) =
                    module.nodes[node_id.index()].as_mut()
            {
                *callee = CpsCallee::Known(clone_entry);
            }
        }
        *budget -= 1;
        return true;
    }
    false
}
/// SpecConstr-style call-pattern specialization. When a known-callee call passes a statically-known tagged tuple into a parameter the callee deconstructs, clone the callee with that constructor rebuilt at its entry so the existing aggregate-projection and known-switch simplifications collapse the deconstruction on a later iteration. The constructor's dynamic fields are threaded as fresh parameters (a worker/wrapper rebuild) and the clone's recursive self-calls fall back to the general function, so it peels the one matched level rather than assuming the recursion stays in pattern. Every call sharing the `(callee, index, tag, arity)` pattern repoints to the single clone, so equivalent sites specialize once. Bounded by `BRANCH_SPECIALIZATION_GROWTH_LIMIT` cloned live nodes and the module-wide clone-count `budget`.
pub(super) fn specialize_call_patterns(module: &mut CpsModule, budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    let constructors = tagged_tuple_values(module);
    if constructors.is_empty() {
        return false;
    }

    // The first specializable pattern in deterministic (node, then argument) order: a known-callee call whose argument is a known tagged tuple that the callee deconstructs, whose callee has a lexical `LetFun` owner and a clonable body within the growth budget.
    let mut chosen: Option<(CpsFunId, usize, u32, usize)> = None;
    'search: for node in module.nodes.iter().flatten() {
        let CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args,
            ..
        } = node
        else {
            continue;
        };
        if Some(*callee) == module.entry {
            continue;
        }
        let params = &module.function(*callee).unwrap().params;
        for (index, arg) in args.iter().enumerate() {
            let CpsAtom::Value(value) = arg else { continue };
            let Some((tag, fields)) = constructors.get(value) else {
                continue;
            };
            let Some(&param) = params.get(index) else {
                continue;
            };
            if !deconstructs_param(module, *callee, param) {
                continue;
            }
            let member = BTreeSet::from([*callee]);
            if introducing_letfun(module, &member).is_none() {
                continue;
            }
            let body = function_nodes(module, *callee);
            let unclonable = body.iter().any(|&id| {
                matches!(
                    module.node(id),
                    Some(CpsNode::LetFun { .. } | CpsNode::RecInit { .. })
                )
            });
            if unclonable || body.len() + 1 > BRANCH_SPECIALIZATION_GROWTH_LIMIT {
                continue;
            }
            chosen = Some((*callee, index, *tag, fields.len()));
            break 'search;
        }
    }
    let Some((callee, index, tag, arity)) = chosen else {
        return false;
    };

    let member = BTreeSet::from([callee]);
    let intro = introducing_letfun(module, &member).unwrap();
    let Some(clones) = clone_scc(module, &member) else {
        return false;
    };
    let clone = clones[&callee];

    // Peel: the clone recurses into the general function, not itself, so a recursive call that does not carry the matched constructor stays valid.
    for node_id in function_nodes(module, clone) {
        let node = module.nodes[node_id.index()].as_mut().unwrap();
        if let CpsNode::ApplyFun {
            callee: CpsCallee::Known(target),
            ..
        } = node
            && *target == clone
        {
            *target = callee;
        }
        visit_atoms_mut(node, &mut |atom| {
            if let CpsAtom::Fun(function) = atom
                && *function == clone
            {
                *atom = CpsAtom::Fun(callee);
            }
        });
    }

    // Rebuild the constructor at the clone entry, threading its dynamic fields as fresh parameters in place of the specialized parameter.
    let clone_function = module.function(clone).unwrap();
    let mut params = clone_function.params.clone();
    let clone_body = clone_function.body;
    let old_param = params[index];
    let field_params: Vec<CpsValueId> = (1..arity)
        .map(|field| module.add_value(Some(format!("field#{field}"))))
        .collect();
    let mut rebuilt = Vec::with_capacity(arity);
    rebuilt.push(CpsAtom::Literal(CpsLiteral::Nat(tag)));
    rebuilt.extend(field_params.iter().map(|&p| CpsAtom::Value(p)));
    let entry = module.add_node(CpsNode::LetValue {
        result: old_param,
        value: CpsValueExpr::Tuple(rebuilt),
        next: clone_body,
    });
    params.splice(index..=index, field_params);
    let clone_function = module.functions[clone.index()].as_mut().unwrap();
    clone_function.params = params;
    clone_function.body = entry;

    // Introduce the clone in the callee's lexical scope.
    if let Some(CpsNode::LetFun { functions, .. }) = module.nodes[intro.index()].as_mut() {
        functions.push(clone);
    }

    // Repoint every call sharing the pattern to the single clone, splicing each site's own constructor fields in place of the tuple argument.
    for node_id in 0..module.nodes.len() {
        let Some(CpsNode::ApplyFun {
            callee: CpsCallee::Known(target),
            args,
            ..
        }) = module.node(CpsNodeId(node_id as u32))
        else {
            continue;
        };
        if *target != callee {
            continue;
        }
        let Some(CpsAtom::Value(value)) = args.get(index) else {
            continue;
        };
        let Some((site_tag, site_fields)) = constructors.get(value) else {
            continue;
        };
        if *site_tag != tag || site_fields.len() != arity {
            continue;
        }
        let spliced = site_fields[1..].to_vec();
        let Some(CpsNode::ApplyFun {
            callee: target,
            args,
            ..
        }) = module.nodes[node_id].as_mut()
        else {
            unreachable!()
        };
        *target = CpsCallee::Known(clone);
        args.splice(index..=index, spliced);
    }

    *budget -= 1;
    true
}
/// The `LetValue`-bound tagged tuples: values whose defining expression is a tuple whose first field is a `Nat` literal tag. These are the constructor call patterns branch specialization can bake into a callee.
pub(super) fn tagged_tuple_values(module: &CpsModule) -> BTreeMap<CpsValueId, (u32, Vec<CpsAtom>)> {
    let mut result = BTreeMap::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::LetValue {
            result: value,
            value: CpsValueExpr::Tuple(fields),
            ..
        } = node
            && let Some(CpsAtom::Literal(CpsLiteral::Nat(tag))) = fields.first()
        {
            result.insert(*value, (*tag, fields.clone()));
        }
    }
    result
}
/// Whether `function` projects a field out of `param`, i.e. contains a `TplGet` on it. This is the profitability gate: baking a known tuple into a parameter only pays off when the body actually deconstructs it.
pub(super) fn deconstructs_param(
    module: &CpsModule,
    function: CpsFunId,
    param: CpsValueId,
) -> bool {
    function_nodes(module, function).iter().any(|&id| {
        matches!(
            module.node(id),
            Some(CpsNode::LetPrim {
                op: CpsPrimOp::TplGet(_),
                args,
                ..
            }) if args.first() == Some(&CpsAtom::Value(param))
        )
    })
}
/// The literal results of `LetValue` bindings, used to resolve caller values already known to be constant.
pub(super) fn literal_value_map(module: &CpsModule) -> BTreeMap<CpsValueId, CpsAtom> {
    let mut literals = BTreeMap::new();
    for node in module.nodes.iter().flatten() {
        if let CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(literal),
            ..
        } = node
        {
            literals.insert(*result, CpsAtom::Literal(literal.clone()));
        }
    }
    literals
}
/// The single `LetFun` node introducing every member, or `None` if the members are split across nodes or introduced by a `RecInit` knot. The clones are added to this node so they share the members' lexical scope.
pub(super) fn introducing_letfun(
    module: &CpsModule,
    members: &BTreeSet<CpsFunId>,
) -> Option<CpsNodeId> {
    for (index, node) in module.nodes.iter().enumerate() {
        if let Some(CpsNode::LetFun { functions, .. }) = node {
            let introduced: BTreeSet<CpsFunId> = functions.iter().copied().collect();
            if members.is_subset(&introduced) {
                return Some(CpsNodeId(index as u32));
            }
        }
    }
    None
}
/// Verbatim-clone every member of an SCC into fresh functions with fresh return continuations, local continuations, owned values, and nodes. Internal known-callee edges and return continuations are rewired to the clones while free values, external callees, and external continuations are shared. Returns the old-to-new function map, or `None` if a member body nests a function definition, which this verbatim clone does not reproduce.
pub(super) fn clone_scc(
    module: &mut CpsModule,
    members: &BTreeSet<CpsFunId>,
) -> Option<BTreeMap<CpsFunId, CpsFunId>> {
    let member_defs: BTreeMap<CpsFunId, CpsFunction> = members
        .iter()
        .map(|&m| (m, module.function(m).unwrap().clone()))
        .collect();

    let mut node_ids: Vec<CpsNodeId> = Vec::new();
    for &m in members {
        node_ids.extend(function_nodes(module, m));
    }
    let node_defs: BTreeMap<CpsNodeId, CpsNode> = node_ids
        .iter()
        .map(|&id| (id, module.node(id).unwrap().clone()))
        .collect();
    if node_defs
        .values()
        .any(|node| matches!(node, CpsNode::LetFun { .. } | CpsNode::RecInit { .. }))
    {
        return None;
    }

    let cont_ids: BTreeSet<CpsContId> = node_defs
        .values()
        .filter_map(|node| match node {
            CpsNode::LetCont { continuations, .. } => Some(continuations.clone()),
            _ => None,
        })
        .flatten()
        .collect();
    let cont_defs: BTreeMap<CpsContId, CpsContinuation> = cont_ids
        .iter()
        .map(|&id| (id, module.continuation(id).unwrap().clone()))
        .collect();

    // Mint fresh owned values: member params, let-bound results, local continuation parameters. Values defined outside the SCC are shared.
    let mut values: BTreeMap<CpsValueId, CpsValueId> = BTreeMap::new();
    let mut owned: Vec<CpsValueId> = Vec::new();
    for def in member_defs.values() {
        owned.extend(def.params.iter().copied());
    }
    for node in node_defs.values() {
        if let CpsNode::LetValue { result, .. } | CpsNode::LetPrim { result, .. } = node {
            owned.push(*result);
        }
    }
    for cont in cont_defs.values() {
        owned.extend(cont.params.iter().copied());
    }
    for old in owned {
        let definition = module.values[old.index()].as_ref().unwrap().clone();
        let fresh = module.add_value(definition.debug_name);
        values.insert(old, fresh);
    }

    let mut conts: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for &id in cont_defs.keys() {
        conts.insert(id, module.reserve_continuation());
    }
    let mut functions: BTreeMap<CpsFunId, CpsFunId> = BTreeMap::new();
    let mut returns: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for (&m, def) in &member_defs {
        functions.insert(m, module.reserve_function());
        returns.insert(def.return_cont, module.reserve_continuation());
    }
    let mut nodes: BTreeMap<CpsNodeId, CpsNodeId> = BTreeMap::new();
    for &id in node_defs.keys() {
        nodes.insert(id, module.reserve_node());
    }

    let map_value = |value: CpsValueId| values.get(&value).copied().unwrap_or(value);
    let map_atom = |atom: &CpsAtom| match atom {
        CpsAtom::Value(value) => CpsAtom::Value(map_value(*value)),
        CpsAtom::Fun(function) => {
            CpsAtom::Fun(functions.get(function).copied().unwrap_or(*function))
        }
        CpsAtom::Literal(literal) => CpsAtom::Literal(literal.clone()),
    };
    let map_cont = |target: CpsContId| {
        returns
            .get(&target)
            .copied()
            .or_else(|| conts.get(&target).copied())
            .unwrap_or(target)
    };
    let map_edge = |edge: &CpsEdge| CpsEdge {
        target: map_cont(edge.target),
        args: edge.args.iter().map(&map_atom).collect(),
    };
    let map_callee = |callee: &CpsCallee| match callee {
        CpsCallee::Known(function) => {
            CpsCallee::Known(functions.get(function).copied().unwrap_or(*function))
        }
        CpsCallee::Closure(value) => CpsCallee::Closure(map_value(*value)),
    };

    let mut cloned_nodes: Vec<(CpsNodeId, CpsNode)> = Vec::new();
    for (&old, node) in &node_defs {
        let cloned = match node {
            CpsNode::LetValue {
                result,
                value,
                next,
            } => CpsNode::LetValue {
                result: map_value(*result),
                value: match value {
                    CpsValueExpr::Literal(literal) => CpsValueExpr::Literal(literal.clone()),
                    CpsValueExpr::List(atoms) => {
                        CpsValueExpr::List(atoms.iter().map(&map_atom).collect())
                    }
                    CpsValueExpr::Tuple(atoms) => {
                        CpsValueExpr::Tuple(atoms.iter().map(&map_atom).collect())
                    }
                },
                next: nodes[next],
            },
            CpsNode::LetPrim {
                result,
                op,
                args,
                next,
            } => CpsNode::LetPrim {
                result: map_value(*result),
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                next: nodes[next],
            },
            CpsNode::LetCont {
                continuations: members,
                body,
            } => CpsNode::LetCont {
                continuations: members.iter().map(|id| conts[id]).collect(),
                body: nodes[body],
            },
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => CpsNode::ApplyFun {
                callee: map_callee(callee),
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::ApplyCont(edge) => CpsNode::ApplyCont(map_edge(edge)),
            CpsNode::Switch {
                scrutinee,
                cases,
                default,
            } => CpsNode::Switch {
                scrutinee: map_atom(scrutinee),
                cases: cases
                    .iter()
                    .map(|(tag, edge)| (*tag, map_edge(edge)))
                    .collect(),
                default: default.as_ref().map(map_edge),
            },
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => CpsNode::Foreign {
                function: function.clone(),
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => CpsNode::Cell {
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Intrinsic {
                op,
                args,
                return_to,
            } => CpsNode::Intrinsic {
                op: *op,
                args: args.iter().map(&map_atom).collect(),
                return_to: map_cont(*return_to),
            },
            CpsNode::Exit { value } => CpsNode::Exit {
                value: value.as_ref().map(&map_atom),
            },
            CpsNode::Unreachable => CpsNode::Unreachable,
            CpsNode::LetFun { .. } | CpsNode::RecInit { .. } => return None,
        };
        cloned_nodes.push((nodes[&old], cloned));
    }

    let mut cloned_conts: Vec<(CpsContId, CpsContinuation)> = Vec::new();
    for (&old, cont) in &cont_defs {
        cloned_conts.push((
            conts[&old],
            CpsContinuation {
                debug_name: cont.debug_name.clone(),
                params: cont.params.iter().map(|&p| map_value(p)).collect(),
                body: nodes[&cont.body],
            },
        ));
    }

    let mut cloned_functions: Vec<(CpsFunId, CpsFunction)> = Vec::new();
    for (&m, def) in &member_defs {
        cloned_functions.push((
            functions[&m],
            CpsFunction {
                debug_name: def.debug_name.clone(),
                params: def.params.iter().map(|&p| map_value(p)).collect(),
                return_cont: returns[&def.return_cont],
                body: nodes[&def.body],
            },
        ));
    }

    for (id, node) in cloned_nodes {
        module.nodes[id.index()] = Some(node);
    }
    for (id, cont) in cloned_conts {
        module.continuations[id.index()] = Some(cont);
    }
    for (id, function) in cloned_functions {
        module.define_function(id, function);
    }
    Some(functions)
}
pub(super) fn merge_inputs(inputs: &mut [Knowledge], args: Option<&[CpsAtom]>) {
    for (index, input) in inputs.iter_mut().enumerate() {
        input.merge(args.and_then(|args| args.get(index)));
    }
}
pub(super) fn record_known_literals(
    params: &[CpsValueId],
    inputs: &[Knowledge],
    known: &mut BTreeMap<CpsValueId, CpsAtom>,
) {
    for (&param, input) in params.iter().zip(inputs) {
        if let Knowledge::Known(CpsAtom::Literal(literal)) = input {
            known.insert(param, CpsAtom::Literal(literal.clone()));
        }
    }
}
