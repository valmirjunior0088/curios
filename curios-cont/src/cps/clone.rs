//! Copying one body into another set of identities.
//!
//! Three callers copy a subtree and differ only in what they map it onto: the SCC specializer mints a fresh identity for everything it owns, the join specializer does the same for one continuation's subtree, and the inliner binds the callee's parameters to the call's argument atoms and splices the body's root onto the call node itself. Those differences are the [`Mapping`]; the walk below is what all three were writing out identically.
//!
//! **Two of the three share the whole copy, not just the walk.** Cloning a function set and cloning a continuation's subtree ask the same question of nesting and answer it the same way, so [`copy_bodies`] serves both and each caller is left with only its seed and what it does with the result. The inliner keeps its own, because its mapping is genuinely different: parameters bind to the call's argument atoms rather than to fresh values, and the body's root is spliced onto the call node instead of being minted.

use {
    super::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsModule,
        CpsNode, CpsNodeId, CpsValueExpr, CpsValueId,
        analysis::{function_nodes, nodes_from},
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What one copy renames its original onto.
///
/// `value` answers for a binding site and `atom` for a use, which are not the same question: a use may map to a literal or a function reference where a binding can only ever map to another value.
pub(super) struct Mapping<'a> {
    pub(super) value: &'a dyn Fn(CpsValueId) -> CpsValueId,
    pub(super) atom: &'a dyn Fn(&CpsAtom) -> CpsAtom,
    pub(super) cont: &'a dyn Fn(CpsContId) -> CpsContId,
    pub(super) callee: &'a dyn Fn(&CpsCallee) -> CpsCallee,
    pub(super) function: &'a dyn Fn(CpsFunId) -> CpsFunId,
    pub(super) node: &'a dyn Fn(CpsNodeId) -> CpsNodeId,
}

impl Mapping<'_> {
    fn edge(&self, edge: &CpsEdge) -> CpsEdge {
        CpsEdge {
            target: (self.cont)(edge.target),
            args: edge.args.iter().map(self.atom).collect(),
        }
    }
}

/// One node, rewritten onto the identities `map` names.
pub(super) fn clone_node(node: &CpsNode, map: &Mapping<'_>) -> CpsNode {
    match node {
        CpsNode::LetValue {
            result,
            value,
            next,
        } => CpsNode::LetValue {
            result: (map.value)(*result),
            value: match value {
                CpsValueExpr::Literal(literal) => CpsValueExpr::Literal(literal.clone()),
                CpsValueExpr::List(atoms) => {
                    CpsValueExpr::List(atoms.iter().map(map.atom).collect())
                }
                CpsValueExpr::Tuple(atoms) => {
                    CpsValueExpr::Tuple(atoms.iter().map(map.atom).collect())
                }
                CpsValueExpr::Variant(family, atoms) => {
                    CpsValueExpr::Variant(*family, atoms.iter().map(map.atom).collect())
                }
            },
            next: (map.node)(*next),
        },
        CpsNode::LetIntrinsic {
            result,
            op,
            args,
            next,
        } => CpsNode::LetIntrinsic {
            result: (map.value)(*result),
            op: *op,
            args: args.iter().map(map.atom).collect(),
            next: (map.node)(*next),
        },
        CpsNode::LetCont {
            continuations,
            body,
        } => CpsNode::LetCont {
            continuations: continuations.iter().map(|id| (map.cont)(*id)).collect(),
            body: (map.node)(*body),
        },
        CpsNode::ApplyFun {
            callee,
            args,
            return_to,
        } => CpsNode::ApplyFun {
            callee: (map.callee)(callee),
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        CpsNode::ApplyCont(edge) => CpsNode::ApplyCont(map.edge(edge)),
        CpsNode::Switch {
            scrutinee,
            cases,
            default,
        } => CpsNode::Switch {
            scrutinee: (map.atom)(scrutinee),
            cases: cases
                .iter()
                .map(|(tag, edge)| (*tag, map.edge(edge)))
                .collect(),
            default: default.as_ref().map(|edge| map.edge(edge)),
        },
        CpsNode::Foreign {
            function,
            args,
            return_to,
        } => CpsNode::Foreign {
            function: function.clone(),
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        CpsNode::Cell {
            op,
            args,
            return_to,
        } => CpsNode::Cell {
            op: *op,
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        CpsNode::Intrinsic {
            op,
            args,
            return_to,
        } => CpsNode::Intrinsic {
            op: *op,
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        CpsNode::Exit { value } => CpsNode::Exit {
            value: value.as_ref().map(map.atom),
        },
        CpsNode::Unreachable => CpsNode::Unreachable,
        CpsNode::LetFun { functions, body } => CpsNode::LetFun {
            functions: functions.iter().map(|id| (map.function)(*id)).collect(),
            body: (map.node)(*body),
        },
        CpsNode::RecInit {
            functions,
            values,
            ready,
            body,
        } => CpsNode::RecInit {
            functions: functions.iter().map(|id| (map.function)(*id)).collect(),
            values: values.iter().map(|id| (map.value)(*id)).collect(),
            ready: (map.node)(*ready),
            body: (map.node)(*body),
        },
    }
}

/// Everything a copy of `roots` has to reproduce: the nodes, and the functions defined lexically within them.
///
/// Closed transitively, because a nested body may nest further. A nested definition cannot be left shared and cannot be copied separately: its body may read values bound in the body being copied, so it has to be renamed by the same mapping — which is what makes the extent one question rather than each caller's own.
pub(super) fn copied_extent(
    module: &CpsModule,
    roots: impl IntoIterator<Item = CpsNodeId>,
) -> (BTreeSet<CpsNodeId>, BTreeSet<CpsFunId>) {
    let mut nodes: BTreeSet<CpsNodeId> = roots.into_iter().collect();
    let mut functions = BTreeSet::new();
    let mut pending: Vec<CpsNodeId> = nodes.iter().copied().collect();

    while let Some(node_id) = pending.pop() {
        let nested = match module.node(node_id) {
            Some(CpsNode::LetFun { functions, .. } | CpsNode::RecInit { functions, .. }) => {
                functions.clone()
            }
            _ => continue,
        };
        for function in nested {
            if !functions.insert(function) {
                continue;
            }
            for inner in function_nodes(module, function) {
                if nodes.insert(inner) {
                    pending.push(inner);
                }
            }
        }
    }
    (nodes, functions)
}

/// What one call to [`copy_bodies`] minted.
pub(super) struct Copies {
    pub(super) functions: BTreeMap<CpsFunId, CpsFunId>,
    pub(super) continuations: BTreeMap<CpsContId, CpsContId>,
}

/// Copy `functions` and `continuations` into fresh identities, together with everything nested inside them.
///
/// Values, continuations, nodes and function identities owned by the copied extent are minted afresh and rewired to each other; anything defined outside it is shared, which is what makes the copy a copy rather than a second module. A seeded continuation is copied as itself, so its own parameters are renamed — a caller wanting the original's arity preserved reads the fresh id out of [`Copies`].
pub(super) fn copy_bodies(
    module: &mut CpsModule,
    functions: &BTreeSet<CpsFunId>,
    continuations: &BTreeSet<CpsContId>,
) -> Copies {
    let roots = functions
        .iter()
        .flat_map(|&id| function_nodes(module, id))
        .chain(
            continuations
                .iter()
                .flat_map(|&id| nodes_from(module, module.continuation(id).unwrap().body)),
        )
        .collect::<Vec<_>>();
    let (node_ids, nested) = copied_extent(module, roots);
    let members: BTreeSet<CpsFunId> = functions.union(&nested).copied().collect();

    let member_defs: BTreeMap<CpsFunId, CpsFunction> = members
        .iter()
        .map(|&id| (id, module.function(id).unwrap().clone()))
        .collect();
    let node_defs: BTreeMap<CpsNodeId, CpsNode> = node_ids
        .iter()
        .map(|&id| (id, module.node(id).unwrap().clone()))
        .collect();
    let cont_ids: BTreeSet<CpsContId> = node_defs
        .values()
        .filter_map(|node| match node {
            CpsNode::LetCont { continuations, .. } => Some(continuations.clone()),
            _ => None,
        })
        .flatten()
        .chain(continuations.iter().copied())
        .collect();
    let cont_defs: BTreeMap<CpsContId, CpsContinuation> = cont_ids
        .iter()
        .map(|&id| (id, module.continuation(id).unwrap().clone()))
        .collect();

    // Mint fresh owned values: member parameters, let-bound results, the values a `RecInit` knot binds, and continuation parameters. Values defined outside the extent are shared.
    let mut owned: Vec<CpsValueId> = Vec::new();
    for def in member_defs.values() {
        owned.extend(def.params.iter().copied());
    }
    for node in node_defs.values() {
        match node {
            CpsNode::LetValue { result, .. } | CpsNode::LetIntrinsic { result, .. } => {
                owned.push(*result)
            }
            CpsNode::RecInit { values, .. } => owned.extend(values.iter().copied()),
            _ => {}
        }
    }
    for cont in cont_defs.values() {
        owned.extend(cont.params.iter().copied());
    }
    let mut values: BTreeMap<CpsValueId, CpsValueId> = BTreeMap::new();
    for old in owned {
        let definition = module.values.get(old).unwrap().clone();
        let fresh = module.add_value(definition.debug_name);
        values.insert(old, fresh);
    }

    let mut conts: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for &id in cont_defs.keys() {
        conts.insert(id, module.reserve_continuation());
    }
    let mut minted: BTreeMap<CpsFunId, CpsFunId> = BTreeMap::new();
    let mut returns: BTreeMap<CpsContId, CpsContId> = BTreeMap::new();
    for (&id, def) in &member_defs {
        minted.insert(id, module.reserve_function());
        returns.insert(def.return_cont, module.reserve_continuation());
    }
    let mut nodes: BTreeMap<CpsNodeId, CpsNodeId> = BTreeMap::new();
    for &id in node_defs.keys() {
        nodes.insert(id, module.reserve_node());
    }

    let map_value = |value: CpsValueId| values.get(&value).copied().unwrap_or(value);
    let map_function = |function: CpsFunId| minted.get(&function).copied().unwrap_or(function);
    let map_atom = |atom: &CpsAtom| match atom {
        CpsAtom::Value(value) => CpsAtom::Value(map_value(*value)),
        CpsAtom::Fun(function) => CpsAtom::Fun(map_function(*function)),
        CpsAtom::Literal(literal) => CpsAtom::Literal(literal.clone()),
        CpsAtom::Filler => CpsAtom::Filler,
    };
    // A seeded continuation is copied as a *peeled* entry: it gets a fresh identity, but a transfer to it from inside the copy keeps naming the original. A join copy exists to specialize one entry, and a back-edge that followed the copy would re-enter parameters its caller is about to resplice. A seeded *function* is the opposite and maps internally, because an SCC copy has to be a self-contained recursive unit for the arguments it was specialized on to survive the recursion.
    let map_cont = |id: CpsContId| {
        returns
            .get(&id)
            .copied()
            .or_else(|| (!continuations.contains(&id)).then(|| conts.get(&id).copied())?)
            .unwrap_or(id)
    };
    let map_callee = |callee: &CpsCallee| match callee {
        CpsCallee::Known(function) => CpsCallee::Known(map_function(*function)),
        CpsCallee::Closure(value) => CpsCallee::Closure(map_value(*value)),
    };
    let map = Mapping {
        value: &map_value,
        atom: &map_atom,
        cont: &map_cont,
        callee: &map_callee,
        function: &map_function,
        node: &|id| nodes[&id],
    };

    let cloned_nodes: Vec<(CpsNodeId, CpsNode)> = node_defs
        .iter()
        .map(|(&old, node)| (nodes[&old], clone_node(node, &map)))
        .collect();
    let cloned_conts: Vec<(CpsContId, CpsContinuation)> = cont_defs
        .iter()
        .map(|(&old, cont)| {
            (
                conts[&old],
                CpsContinuation {
                    debug_name: cont.debug_name.clone(),
                    params: cont.params.iter().map(|&p| map_value(p)).collect(),
                    body: nodes[&cont.body],
                },
            )
        })
        .collect();
    let cloned_functions: Vec<(CpsFunId, CpsFunction)> = member_defs
        .iter()
        .map(|(&id, def)| {
            (
                minted[&id],
                CpsFunction {
                    debug_name: def.debug_name.clone(),
                    params: def.params.iter().map(|&p| map_value(p)).collect(),
                    return_cont: returns[&def.return_cont],
                    body: nodes[&def.body],
                },
            )
        })
        .collect();

    for (id, node) in cloned_nodes {
        module.nodes.define(id, node);
    }
    for (id, cont) in cloned_conts {
        module.continuations.define(id, cont);
    }
    for (id, function) in cloned_functions {
        module.define_function(id, function);
    }
    Copies {
        functions: minted,
        continuations: conts,
    }
}

#[cfg(test)]
mod tests;
