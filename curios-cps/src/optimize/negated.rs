//! A comparison's negation folded into the comparison that is true exactly where it is false.
//!
//! `a != b` elaborates to the negation of `a == b`, `xor(a == b, true)`, as `Bool/not` of any comparison does, and reaches here as the comparison's `0`/`1` word handed to `NatXor` beside `1`. On a total order the negation of a comparison is a comparison of its own — `==`'s is `!=`, and `a < b`'s is `b <= a` — so the two intrinsics are one, which the host computes in one instruction. A `Flt` comparison is left as it is: against a NaN, `a < b` and `b <= a` are both false.
//!
//! The comparison's operands are in scope wherever its answer is: a binding's lexical scope is its dominance region, the premise [`dedupe_intrinsics`](super::dedupe_intrinsics) rests on, and the answer is read only below it. So the folded comparison reads at the negation what the comparison read where it stood, and the comparison is left for the passes that drop what nothing reads.

#[cfg(test)]
mod tests;

use {
    crate::{Atom, Intrinsic, Literal, Module, Node, ValueId},
    curios_num::Natural,
    std::collections::BTreeMap,
};

/// Fold every negation of a comparison on a total order into the comparison that negates it, answering whether any was folded.
pub fn fold_negated_comparisons(module: &mut Module) -> bool {
    let negations = module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::LetIntrinsic {
                result, op, args, ..
            } => Some((*result, negation(*op, args)?)),
            _ => None,
        })
        .collect::<BTreeMap<ValueId, (Intrinsic, Vec<Atom>)>>();
    let one = Atom::Literal(Literal::Nat(Natural::from(1u32)));
    let folds = module
        .live_nodes()
        .filter_map(|(id, node)| match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::NatXor,
                args,
                next,
            } => {
                let negated = match args.as_slice() {
                    [Atom::Value(value), other] | [other, Atom::Value(value)] if *other == one => {
                        value
                    }
                    _ => return None,
                };
                let (op, args) = negations.get(negated)?.clone();
                Some((
                    id,
                    Node::LetIntrinsic {
                        result: *result,
                        op,
                        args,
                        next: *next,
                    },
                ))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let folded = !folds.is_empty();
    for (id, node) in folds {
        module.set_node(id, node);
    }
    folded
}

/// The comparison true exactly where `op` over `args` is false, with the operands it reads: `None` for an intrinsic that is no comparison on a total order.
fn negation(op: Intrinsic, args: &[Atom]) -> Option<(Intrinsic, Vec<Atom>)> {
    let [left, right] = args else {
        return None;
    };
    let (op, swapped) = match op {
        Intrinsic::NatEql => (Intrinsic::NatNeq, false),
        Intrinsic::NatNeq => (Intrinsic::NatEql, false),
        Intrinsic::NatLt => (Intrinsic::NatLe, true),
        Intrinsic::NatLe => (Intrinsic::NatLt, true),
        Intrinsic::IntEql => (Intrinsic::IntNeq, false),
        Intrinsic::IntNeq => (Intrinsic::IntEql, false),
        Intrinsic::IntLt => (Intrinsic::IntLe, true),
        Intrinsic::IntLe => (Intrinsic::IntLt, true),
        _ => return None,
    };
    Some(match swapped {
        true => (op, vec![right.clone(), left.clone()]),
        false => (op, vec![left.clone(), right.clone()]),
    })
}
