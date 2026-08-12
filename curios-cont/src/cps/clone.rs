//! Copying one body into another set of identities.
//!
//! Three callers copy a subtree and differ only in what they map it onto: the SCC specializer mints a fresh identity for everything it owns, the join specializer does the same for one continuation's subtree, and the inliner binds the callee's parameters to the call's argument atoms and splices the body's root onto the call node itself. Those differences are the [`Mapping`]; the walk below is what all three were writing out identically.
//!
//! **The walk is the whole of the sharing, deliberately.** Each caller still decides its own member set, mints its own identities, and defines its own results, because those are where they genuinely differ — and a single entry point taking a description of all three would be a larger thing to read than the three call sites it replaced.

use super::{CpsAtom, CpsCallee, CpsContId, CpsEdge, CpsNode, CpsNodeId, CpsValueExpr, CpsValueId};

/// What one copy renames its original onto.
///
/// `value` answers for a binding site and `atom` for a use, which are not the same question: a use may map to a literal or a function reference where a binding can only ever map to another value.
pub(super) struct Mapping<'a> {
    pub(super) value: &'a dyn Fn(CpsValueId) -> CpsValueId,
    pub(super) atom: &'a dyn Fn(&CpsAtom) -> CpsAtom,
    pub(super) cont: &'a dyn Fn(CpsContId) -> CpsContId,
    pub(super) callee: &'a dyn Fn(&CpsCallee) -> CpsCallee,
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
        // Every caller rejects a body nesting a function definition before it mints anything, because a copy that reproduced neither the definition nor its identities would be silently wrong and bailing here would leak what was already minted.
        CpsNode::LetFun { .. } | CpsNode::RecInit { .. } => unreachable!(),
    }
}
