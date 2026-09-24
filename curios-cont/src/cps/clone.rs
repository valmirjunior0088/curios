//! Copying one body into another set of identities.
//!
//! Three callers copy a subtree and differ only in what they map it onto: the SCC specializer mints a fresh identity for everything it owns, the join specializer does the same for one continuation's subtree, and the inliner binds the callee's parameters to the call's argument atoms and splices the body's root onto the call node itself. Those differences are the [`Mapping`]; the walk below is what all three were writing out identically.
//!
//! **Two of the three share the whole copy, not just the walk.** Cloning a function set and cloning a continuation's subtree ask the same question of nesting and answer it the same way, so [`copy_bodies`] serves both and each caller is left with only its seed and what it does with the result. The inliner keeps its own, because its mapping is genuinely different: parameters bind to the call's argument atoms rather than to fresh values, and the body's root is spliced onto the call node instead of being minted.

use {
    super::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Module, Node,
        NodeId, ValueExpr, ValueId,
        analysis::{function_nodes, nodes_from},
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What one copy renames its original onto.
///
/// `value` answers for a binding site and `atom` for a use, which are not the same question: a use may map to a literal or a function reference where a binding can only ever map to another value.
pub(super) struct Mapping<'a> {
    pub(super) value: &'a dyn Fn(ValueId) -> ValueId,
    pub(super) atom: &'a dyn Fn(&Atom) -> Atom,
    pub(super) cont: &'a dyn Fn(ContinuationId) -> ContinuationId,
    pub(super) callee: &'a dyn Fn(&Callee) -> Callee,
    pub(super) function: &'a dyn Fn(FunctionId) -> FunctionId,
    pub(super) node: &'a dyn Fn(NodeId) -> NodeId,
}

impl Mapping<'_> {
    fn edge(&self, edge: &Edge) -> Edge {
        Edge {
            target: (self.cont)(edge.target),
            args: edge.args.iter().map(self.atom).collect(),
        }
    }
}

/// One node, rewritten onto the identities `map` names.
pub(super) fn clone_node(node: &Node, map: &Mapping<'_>) -> Node {
    match node {
        Node::LetValue {
            result,
            value,
            next,
        } => Node::LetValue {
            result: (map.value)(*result),
            value: match value {
                ValueExpr::Literal(literal) => ValueExpr::Literal(literal.clone()),
                ValueExpr::List(atoms) => ValueExpr::List(atoms.iter().map(map.atom).collect()),
                ValueExpr::Tuple(atoms) => ValueExpr::Tuple(atoms.iter().map(map.atom).collect()),
                ValueExpr::Row(row, atoms) => {
                    ValueExpr::Row(*row, atoms.iter().map(map.atom).collect())
                }
            },
            next: (map.node)(*next),
        },
        Node::LetIntrinsic {
            result,
            op,
            args,
            next,
        } => Node::LetIntrinsic {
            result: (map.value)(*result),
            op: *op,
            args: args.iter().map(map.atom).collect(),
            next: (map.node)(*next),
        },
        Node::LetCont {
            continuations,
            body,
        } => Node::LetCont {
            continuations: continuations.iter().map(|id| (map.cont)(*id)).collect(),
            body: (map.node)(*body),
        },
        Node::ApplyFun {
            callee,
            args,
            return_to,
        } => Node::ApplyFun {
            callee: (map.callee)(callee),
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        Node::ApplyCont(edge) => Node::ApplyCont(map.edge(edge)),
        Node::Switch {
            scrutinee,
            cases,
            default,
        } => Node::Switch {
            scrutinee: (map.atom)(scrutinee),
            cases: cases
                .iter()
                .map(|(tag, edge)| (*tag, map.edge(edge)))
                .collect(),
            default: default.as_ref().map(|edge| map.edge(edge)),
        },
        Node::Foreign {
            function,
            args,
            return_to,
        } => Node::Foreign {
            function: function.clone(),
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        Node::Cell {
            op,
            args,
            return_to,
        } => Node::Cell {
            op: *op,
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        Node::Channel {
            op,
            args,
            return_to,
        } => Node::Channel {
            op: *op,
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        Node::Intrinsic {
            op,
            args,
            return_to,
        } => Node::Intrinsic {
            op: *op,
            args: args.iter().map(map.atom).collect(),
            return_to: (map.cont)(*return_to),
        },
        Node::Exit { value } => Node::Exit {
            value: value.as_ref().map(map.atom),
        },
        Node::Panic(panic) => Node::Panic(*panic),
        Node::Unreachable => Node::Unreachable,
        Node::LetFun { functions, body } => Node::LetFun {
            functions: functions.iter().map(|id| (map.function)(*id)).collect(),
            body: (map.node)(*body),
        },
    }
}

/// Everything a copy of `roots` has to reproduce: the nodes, and the functions defined lexically within them.
///
/// Closed transitively, because a nested body may nest further. A nested definition cannot be left shared and cannot be copied separately: its body may read values bound in the body being copied, so it has to be renamed by the same mapping — which is what makes the extent one question rather than each caller's own.
pub(super) fn copied_extent(
    module: &Module,
    roots: impl IntoIterator<Item = NodeId>,
) -> (BTreeSet<NodeId>, BTreeSet<FunctionId>) {
    let mut nodes: BTreeSet<NodeId> = roots.into_iter().collect();
    let mut functions = BTreeSet::new();
    let mut pending: Vec<NodeId> = nodes.iter().copied().collect();

    while let Some(node_id) = pending.pop() {
        let nested = match module.node(node_id) {
            Some(Node::LetFun { functions, .. }) => functions.clone(),
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
    pub(super) functions: BTreeMap<FunctionId, FunctionId>,
    pub(super) continuations: BTreeMap<ContinuationId, ContinuationId>,
}

/// Copy `functions` and `continuations` into fresh identities, together with everything nested inside them.
///
/// Values, continuations, nodes and function identities owned by the copied extent are minted afresh and rewired to each other; anything defined outside it is shared, which is what makes the copy a copy rather than a second module. A seeded continuation is copied as itself, so its own parameters are renamed — a caller wanting the original's arity preserved reads the fresh id out of [`Copies`].
pub(super) fn copy_bodies(
    module: &mut Module,
    functions: &BTreeSet<FunctionId>,
    continuations: &BTreeSet<ContinuationId>,
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
    let members: BTreeSet<FunctionId> = functions.union(&nested).copied().collect();

    let member_defs: BTreeMap<FunctionId, Function> = members
        .iter()
        .map(|&id| (id, module.function(id).unwrap().clone()))
        .collect();
    let node_defs: BTreeMap<NodeId, Node> = node_ids
        .iter()
        .map(|&id| (id, module.node(id).unwrap().clone()))
        .collect();
    let cont_ids: BTreeSet<ContinuationId> = node_defs
        .values()
        .filter_map(|node| match node {
            Node::LetCont { continuations, .. } => Some(continuations.clone()),
            _ => None,
        })
        .flatten()
        .chain(continuations.iter().copied())
        .collect();
    let cont_defs: BTreeMap<ContinuationId, Continuation> = cont_ids
        .iter()
        .map(|&id| (id, module.continuation(id).unwrap().clone()))
        .collect();

    // Mint fresh owned values: member parameters, let-bound results, and continuation parameters. Values defined outside the extent are shared.
    let mut owned: Vec<ValueId> = Vec::new();
    for def in member_defs.values() {
        owned.extend(def.params.iter().copied());
    }
    for node in node_defs.values() {
        match node {
            Node::LetValue { result, .. } | Node::LetIntrinsic { result, .. } => {
                owned.push(*result)
            }
            _ => {}
        }
    }
    for cont in cont_defs.values() {
        owned.extend(cont.params.iter().copied());
    }
    let mut values: BTreeMap<ValueId, ValueId> = BTreeMap::new();
    for old in owned {
        let definition = module.values.get(old).unwrap().clone();
        let fresh = module.add_value(definition.debug_name);
        values.insert(old, fresh);
    }

    let mut conts: BTreeMap<ContinuationId, ContinuationId> = BTreeMap::new();
    for &id in cont_defs.keys() {
        conts.insert(id, module.reserve_continuation());
    }
    let mut minted: BTreeMap<FunctionId, FunctionId> = BTreeMap::new();
    let mut returns: BTreeMap<ContinuationId, ContinuationId> = BTreeMap::new();
    for (&id, def) in &member_defs {
        minted.insert(id, module.reserve_function());
        returns.insert(def.return_cont, module.reserve_continuation());
    }
    let mut nodes: BTreeMap<NodeId, NodeId> = BTreeMap::new();
    for &id in node_defs.keys() {
        nodes.insert(id, module.reserve_node());
    }

    let map_value = |value: ValueId| values.get(&value).copied().unwrap_or(value);
    let map_function = |function: FunctionId| minted.get(&function).copied().unwrap_or(function);
    let map_atom = |atom: &Atom| match atom {
        Atom::Value(value) => Atom::Value(map_value(*value)),
        Atom::Fun(function) => Atom::Fun(map_function(*function)),
        Atom::Literal(literal) => Atom::Literal(literal.clone()),
        Atom::Filler => Atom::Filler,
    };
    // A seeded continuation is copied as a *peeled* entry: it gets a fresh identity, but a transfer to it from inside the copy keeps naming the original. A join copy exists to specialize one entry, and a back-edge that followed the copy would re-enter parameters its caller is about to resplice. A seeded *function* is the opposite and maps internally, because an SCC copy has to be a self-contained recursive unit for the arguments it was specialized on to survive the recursion.
    let map_cont = |id: ContinuationId| {
        returns
            .get(&id)
            .copied()
            .or_else(|| (!continuations.contains(&id)).then(|| conts.get(&id).copied())?)
            .unwrap_or(id)
    };
    let map_callee = |callee: &Callee| match callee {
        Callee::Known(function) => Callee::Known(map_function(*function)),
        Callee::Closure(value) => Callee::Closure(map_value(*value)),
    };
    let map = Mapping {
        value: &map_value,
        atom: &map_atom,
        cont: &map_cont,
        callee: &map_callee,
        function: &map_function,
        node: &|id| nodes[&id],
    };

    let cloned_nodes: Vec<(NodeId, Node)> = node_defs
        .iter()
        .map(|(&old, node)| (nodes[&old], clone_node(node, &map)))
        .collect();
    let cloned_conts: Vec<(ContinuationId, Continuation)> = cont_defs
        .iter()
        .map(|(&old, cont)| {
            (
                conts[&old],
                Continuation {
                    debug_name: cont.debug_name.clone(),
                    params: cont.params.iter().map(|&p| map_value(p)).collect(),
                    body: nodes[&cont.body],
                },
            )
        })
        .collect();
    let cloned_functions: Vec<(FunctionId, Function)> = member_defs
        .iter()
        .map(|(&id, def)| {
            (
                minted[&id],
                Function {
                    debug_name: def.debug_name.clone(),
                    params: def.params.iter().map(|&p| map_value(p)).collect(),
                    return_cont: returns[&def.return_cont],
                    body: nodes[&def.body],
                    droppable: def.droppable,
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
