//! What a value's uses ask of it, and how much of it they read.
//!
//! Dead-parameter elimination asks only whether a value is used at all, which a use count answers. A return protocol needs more: whether *every* use projects a field, so a constructor could be delivered as its fields rather than as a heap tuple. Those are two points of one order, so they are computed once here rather than by two walks that could drift apart.
//!
//! **The fact is interprocedural.** A value passed to a known call or jumped along an edge asks exactly what the receiving parameter's own uses ask, so an argument's demand defers to that parameter and the round becomes a genuine fixpoint under the shared solver. Two transfers deliberately keep the syntactic reading: an argument to a closure call crosses an indirection this walk does not resolve, and a value on an edge into a bodyless return sentinel is consumed by the caller's resume, whose linkage belongs to the return protocol rather than to this lattice.

use {
    super::{
        CpsAtom, CpsCallee, CpsEdge, CpsIntrinsic, CpsModule, CpsNode, CpsValueId, Lattice, Solver,
        atoms,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// How a value's uses consume it, ordered `Unused < Projected(_) | Applied(_) < Opaque`.
///
/// `Projected` carries the field indices read and `Applied` the arity called, so a use set that never reads the whole value stays distinguishable from one that does. The two refinements sit beside each other rather than in a chain: a value read as a tuple and a value invoked as a function are both narrower than opaque and neither is narrower than the other, so mixing them joins to `Opaque`. Absence from the map is **not** `Unused` — see [`demand_of`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Demand {
    /// No use at all. The point dead-parameter elimination reads.
    Unused,
    /// Every use so far projects a field, at these indices.
    Projected(BTreeSet<usize>),
    /// Every use so far calls it, at this arity. Disagreeing arities join to `Opaque`, because a function reached at two arities cannot have either absorbed into it.
    Applied(usize),
    /// Every use so far reads it as a sequence — an element, a length, a window base, an equality side, or a settle — never growing or escaping it. The point the list flattening reads: a construction whose demand is `Indexed` will pay its gather anyway, so building it flat is never asymptotically worse.
    Indexed,
    /// Some use consumes the value whole.
    Opaque,
}

impl Lattice for Demand {
    fn bottom() -> Self {
        Demand::Unused
    }

    fn join(&mut self, incoming: Self) {
        *self = match (std::mem::replace(self, Demand::Unused), incoming) {
            (Demand::Opaque, _) | (_, Demand::Opaque) => Demand::Opaque,
            (Demand::Unused, other) | (other, Demand::Unused) => other,
            (Demand::Projected(mut left), Demand::Projected(right)) => {
                left.extend(right);
                Demand::Projected(left)
            }
            (Demand::Applied(left), Demand::Applied(right)) if left == right => {
                Demand::Applied(left)
            }
            (Demand::Indexed, Demand::Indexed) => Demand::Indexed,
            // A projection, a call, or an indexing read in disagreement — including two calls at different arities: nothing narrower than opaque describes both.
            (
                Demand::Applied(_) | Demand::Projected(_) | Demand::Indexed,
                Demand::Applied(_) | Demand::Projected(_) | Demand::Indexed,
            ) => Demand::Opaque,
        }
    }
}

/// The demand on `value`, reading absence as `Opaque`.
///
/// The conservative direction here is the opposite of the representation client's, which forces the *top* for an unseeded value because it cannot be held in a register. Here the top is what keeps a value alive: answering `Unused` for one the walk never reached would delete a live parameter.
pub(crate) fn demand_of(demands: &BTreeMap<CpsValueId, Demand>, value: CpsValueId) -> Demand {
    demands.get(&value).cloned().unwrap_or(Demand::Opaque)
}

/// Join each of an edge's arguments with the demand established so far on the parameter receiving it. A target without a definition is a return sentinel, whose arguments stay opaque — see the module documentation.
fn defer_edge(module: &CpsModule, solver: &mut Solver<Demand>, edge: &CpsEdge) {
    let params = module
        .continuation(edge.target)
        .map(|continuation| continuation.params.as_slice());
    for (position, atom) in edge.args.iter().enumerate() {
        if let CpsAtom::Value(value) = atom {
            let deferred = params
                .and_then(|params| params.get(position))
                .and_then(|param| solver.facts().get(param).cloned())
                .unwrap_or(Demand::Opaque);
            solver.join(*value, deferred);
        }
    }
}

/// What every value's uses ask of it.
pub(crate) fn demands(module: &CpsModule) -> BTreeMap<CpsValueId, Demand> {
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        for (_, node) in module.nodes.iter_live() {
            match node {
                // A projection reads one field and nothing else — the only use that does not consume the whole value. It is taken before the general fallback below, which would otherwise report `Opaque` for the same operand and erase the refinement.
                CpsNode::LetIntrinsic {
                    op: CpsIntrinsic::TupleGet(index) | CpsIntrinsic::RowGet(_, index),
                    args,
                    ..
                } if matches!(args.as_slice(), [CpsAtom::Value(_)]) => {
                    if let [CpsAtom::Value(value)] = args.as_slice() {
                        solver.join(*value, Demand::Projected(BTreeSet::from([*index])));
                    }
                }

                // A sequence read consumes only elements, lengths, or windows of its carrier operand: the carrier's demand stays `Indexed`, while every other operand — an index, a count, an appended element — is consumed whole. `ListSettle` joins the reads because settling is exactly what an `Indexed` construction would have done to itself. The growth forms — concat, append, chunk, flat — are deliberately absent: their carrier operands are consumed into a new value, which is the escape the lattice point exists to exclude.
                CpsNode::LetIntrinsic {
                    op:
                        CpsIntrinsic::BinLen(_)
                        | CpsIntrinsic::BinEql(_)
                        | CpsIntrinsic::BinGet(_)
                        | CpsIntrinsic::BinSlice(_)
                        | CpsIntrinsic::BinRest(_)
                        | CpsIntrinsic::ListLen
                        | CpsIntrinsic::ListGet
                        | CpsIntrinsic::ListSlice
                        | CpsIntrinsic::ListRest
                        | CpsIntrinsic::ListSettle,
                    args,
                    ..
                } => {
                    let carriers = match args.len() {
                        // Equality reads both sides.
                        2 if matches!(
                            node,
                            CpsNode::LetIntrinsic {
                                op: CpsIntrinsic::BinEql(_),
                                ..
                            }
                        ) =>
                        {
                            2
                        }
                        _ => 1,
                    };
                    for (position, atom) in args.iter().enumerate() {
                        if let CpsAtom::Value(value) = atom {
                            let demand = match position < carriers {
                                true => Demand::Indexed,
                                false => Demand::Opaque,
                            };
                            solver.join(*value, demand);
                        }
                    }
                }

                // The deferral: a known call's argument asks what the receiving parameter's uses ask. A parameter is a seeded live value, so an absent fact can only mean a malformed call, and opaque is the reading that only ever excludes.
                CpsNode::ApplyFun {
                    callee: CpsCallee::Known(callee),
                    args,
                    ..
                } => {
                    let params = module
                        .function(*callee)
                        .map(|function| function.params.as_slice());
                    for (position, atom) in args.iter().enumerate() {
                        if let CpsAtom::Value(value) = atom {
                            let deferred = params
                                .and_then(|params| params.get(position))
                                .and_then(|param| solver.facts().get(param).cloned())
                                .unwrap_or(Demand::Opaque);
                            solver.join(*value, deferred);
                        }
                    }
                }

                // A closure callee is a *use of the whole* value but not an opaque one: the arity is what a caller would have to pass if the application moved into whatever produced it. Its arguments stay opaque — the callee is not resolved here, so no parameter exists to defer to.
                CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(closure),
                    args,
                    ..
                } => {
                    for atom in args {
                        if let CpsAtom::Value(value) = atom {
                            solver.join(*value, Demand::Opaque);
                        }
                    }
                    solver.join(*closure, Demand::Applied(args.len()));
                }

                CpsNode::ApplyCont(edge) => defer_edge(module, solver, edge),

                CpsNode::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    if let CpsAtom::Value(value) = scrutinee {
                        solver.join(*value, Demand::Opaque);
                    }
                    for edge in cases.values().chain(default.as_ref()) {
                        defer_edge(module, solver, edge);
                    }
                }

                _ => {
                    for atom in atoms(node) {
                        if let CpsAtom::Value(value) = atom {
                            solver.join(*value, Demand::Opaque);
                        }
                    }
                }
            }
        }
    })
}

#[cfg(test)]
mod tests;
