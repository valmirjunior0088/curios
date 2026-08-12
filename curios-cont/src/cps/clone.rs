//! Copying one body into another set of identities.
//!
//! Three callers copy a subtree and differ only in what they map it onto: the SCC specializer mints a fresh identity for everything it owns, the join specializer does the same for one continuation's subtree, and the inliner binds the callee's parameters to the call's argument atoms and splices the body's root onto the call node itself. Those differences are the [`Mapping`]; the walk below is what all three were writing out identically.
//!
//! **The walk is the whole of the sharing, deliberately.** Each caller still decides its own member set, mints its own identities, and defines its own results, because those are where they genuinely differ — and a single entry point taking a description of all three would be a larger thing to read than the three call sites it replaced.

use {
    super::{
        CpsAtom, CpsCallee, CpsContId, CpsEdge, CpsFunId, CpsModule, CpsNode, CpsNodeId,
        CpsValueExpr, CpsValueId, analysis::function_nodes,
    },
    std::collections::BTreeSet,
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
