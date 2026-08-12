//! What a value's uses ask of it, and how much of it they read.
//!
//! Dead-parameter elimination asks only whether a value is used at all, which a use count answers. A return protocol needs more: whether *every* use projects a field, so a constructor could be delivered as its fields rather than as a heap tuple. Those are two points of one order, so they are computed once here rather than by two walks that could drift apart.
//!
//! **The fact is deliberately syntactic today.** A value passed as an argument is consumed, whatever the callee does with it. Deferring instead to the callee's own parameter is the interprocedural strengthening — the thing that would make this a fixpoint rather than a scan — and it belongs to its own change, because it finds values dead that a use count cannot and therefore moves emitted code. The round below is written so that strengthening is a change of one rule rather than of the shape.

use {
    super::{
        CpsAtom, CpsCallee, CpsIntrinsicOp, CpsModule, CpsNode, CpsValueId, Lattice, Solver, atoms,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// How a value's uses consume it, ordered `Unused < Projected(_) < Opaque`.
///
/// `Projected` carries the field indices read, so a use set that never reads the whole value stays distinguishable from one that does. Absence from the map is **not** `Unused` — see [`demand_of`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Demand {
    /// No use at all. The point dead-parameter elimination reads.
    Unused,
    /// Every use so far projects a field, at these indices.
    Projected(BTreeSet<usize>),
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
        }
    }
}

/// The demand on `value`, reading absence as `Opaque`.
///
/// The conservative direction here is the opposite of the representation client's, which forces the *top* for an unseeded value because it cannot be held in a register. Here the top is what keeps a value alive: answering `Unused` for one the walk never reached would delete a live parameter.
pub(crate) fn demand_of(demands: &BTreeMap<CpsValueId, Demand>, value: CpsValueId) -> Demand {
    demands.get(&value).cloned().unwrap_or(Demand::Opaque)
}

/// What every value's uses ask of it.
pub(crate) fn demands(module: &CpsModule) -> BTreeMap<CpsValueId, Demand> {
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        for (_, node) in module.nodes.iter_live() {
            // A projection reads one field and nothing else — the only use that does not consume the whole value. It is taken before the general walk below, which would otherwise report `Opaque` for the same operand and erase the refinement.
            if let CpsNode::LetIntrinsic {
                op: CpsIntrinsicOp::TplGet(index),
                args,
                ..
            } = node
                && let [CpsAtom::Value(value)] = args.as_slice()
            {
                solver.join(*value, Demand::Projected(BTreeSet::from([*index])));
                continue;
            }

            for atom in atoms(node) {
                if let CpsAtom::Value(value) = atom {
                    solver.join(*value, Demand::Opaque);
                }
            }

            // A closure callee lives in a value `atoms` does not reach, and calling through it consumes the whole of it.
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
                ..
            } = node
            {
                solver.join(*value, Demand::Opaque);
            }
        }
    })
}

#[cfg(test)]
mod tests;
