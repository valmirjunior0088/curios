//! Scoped common-subexpression elimination over deterministic intrinsics.
//!
//! In this CPS a binding's lexical scope is its dominance region: every use of a `LetIntrinsic` result sits inside the binder's subtree, and any jump into a `LetCont` member comes from within that subtree, after the bindings above it executed. Walking each function's node tree with a scoped table of `(op, operands)` therefore finds exactly the duplicates whose dominating occurrence already ran, which is what makes reusing its result sound even for `MayTrap` ops (see [`CpsIntrinsic::cse_eligible`]). The table never crosses a nested function definition: sharing across one would grow its closure environment, a size tradeoff this pass refuses.

use {
    super::simplify::{rewire_node, rewrite_atoms},
    super::*,
    curios_utilities::{Grain, PackedBin},
    std::collections::{BTreeMap, BTreeSet},
};

/// An operand under a total order, with `Flt` by bit pattern and packed data by the value's own, so commutative normalization can sort and the scope table can key deterministically.
///
/// A `Bin` holds the [`PackedBin`], which carries its logical length, rather than a re-derived byte string that does not. Packing alone underdetermines a bit-grain value — `b[1]` and `b[1, 0]` pack identically — so a key built from packed bytes collided them, and two `BinEql`s against those two literals deduped into one.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum AtomKey {
    Value(u32),
    Fun(u32),
    Nat(u32),
    Int(i32),
    Flt(u32),
    Bin(Grain, PackedBin),
    Filler,
}

fn atom_key(atom: &CpsAtom) -> AtomKey {
    match atom {
        CpsAtom::Value(value) => AtomKey::Value(value.index() as u32),
        CpsAtom::Fun(function) => AtomKey::Fun(function.index() as u32),
        CpsAtom::Literal(CpsLiteral::Nat(value)) => AtomKey::Nat(*value),
        CpsAtom::Literal(CpsLiteral::Int(value)) => AtomKey::Int(*value),
        CpsAtom::Literal(CpsLiteral::Flt(value)) => AtomKey::Flt(value.to_bits()),
        CpsAtom::Literal(CpsLiteral::Bin(grain, value)) => AtomKey::Bin(*grain, value.clone()),
        CpsAtom::Filler => AtomKey::Filler,
    }
}

fn intrinsic_key(op: CpsIntrinsic, args: &[CpsAtom]) -> (CpsIntrinsic, Vec<AtomKey>) {
    let mut keys = args.iter().map(atom_key).collect::<Vec<_>>();
    if op.is_commutative() {
        keys.sort();
    }
    (op, keys)
}

/// One scope-walk work item: visit a node under the current table, or retract what a binder inserted once its subtree is done. The LIFO order makes retraction happen exactly between a `LetCont`'s sibling subtrees, which is what keeps one sibling's bindings invisible to the next.
///
/// A retraction names the *node*, not the key it inserted. The walk never mutates the module — every rewrite is batched after the last function is walked — so the node still carries the operator and operands that produced its key, and recomputing costs one `intrinsic_key`. Carrying the key instead parked an owned `Vec<AtomKey>` here for the whole extent of the binder's subtree, and an `AtomKey::Bin` holds a packed literal's entire byte string, so a chain of literal-bearing intrinsics held one copy of each literal per pending retraction.
enum Task {
    Visit(CpsNodeId),
    Retract(CpsNodeId),
}

/// Remove every `LetIntrinsic` whose `(op, operands)` already has a binding in scope, rewriting its uses to the dominating result. One walk collects every duplicate — a duplicate of a duplicate resolves to the first binder because only the first occupies the table — and the rewrites apply as a batch afterwards.
pub(super) fn dedupe_intrinsics(module: &mut CpsModule) -> bool {
    debug_assert_single_owner(module);

    let mut substitutions = BTreeMap::new();
    let mut duplicates = Vec::new();
    let functions = module.functions.live_ids().collect::<Vec<_>>();
    for function in functions {
        let mut table = BTreeMap::<(CpsIntrinsic, Vec<AtomKey>), CpsValueId>::new();
        let mut work = vec![Task::Visit(module.function(function).unwrap().body)];
        let mut visited = BTreeSet::new();
        while let Some(task) = work.pop() {
            let node_id = match task {
                Task::Visit(node_id) => node_id,
                Task::Retract(node) => {
                    let CpsNode::LetIntrinsic { op, args, .. } = module
                        .node(node)
                        .expect("a retraction names a node this walk visited")
                    else {
                        unreachable!("a retraction is scheduled only for a LetIntrinsic")
                    };

                    table.remove(&intrinsic_key(*op, args));
                    continue;
                }
            };
            if !visited.insert(node_id) {
                continue;
            }
            match module.node(node_id).unwrap() {
                CpsNode::LetIntrinsic {
                    result,
                    op,
                    args,
                    next,
                } => {
                    if op.cse_eligible() {
                        let key = intrinsic_key(*op, args);
                        if let Some(&existing) = table.get(&key) {
                            substitutions.insert(*result, CpsAtom::Value(existing));
                            duplicates.push((node_id, *result, *next));
                        } else {
                            table.insert(key, *result);
                            work.push(Task::Retract(node_id));
                        }
                    }
                    work.push(Task::Visit(*next));
                }
                CpsNode::LetValue { next, .. } => work.push(Task::Visit(*next)),
                CpsNode::LetFun { body, .. } => work.push(Task::Visit(*body)),
                CpsNode::LetCont {
                    continuations,
                    body,
                } => {
                    for continuation in continuations.iter().rev() {
                        if let Some(continuation) = module.continuation(*continuation) {
                            work.push(Task::Visit(continuation.body));
                        }
                    }
                    work.push(Task::Visit(*body));
                }
                CpsNode::ApplyFun { .. }
                | CpsNode::ApplyCont(_)
                | CpsNode::Switch { .. }
                | CpsNode::Foreign { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }
    }

    if duplicates.is_empty() {
        return false;
    }
    rewrite_atoms(module, &substitutions);
    for (node, result, next) in duplicates {
        rewire_node(module, node, next);
        module.nodes.remove(node);
        module.values.remove(result);
    }
    true
}

/// The scope-is-dominance argument requires the node graph to be a tree: every live node owned by exactly one of a function body, a continuation body, or a predecessor's `next`/`body` link. No pass creates sharing today; this assertion is where that assumption fails loudly if one starts to.
fn debug_assert_single_owner(module: &CpsModule) {
    if cfg!(debug_assertions) {
        let mut counts = BTreeMap::<CpsNodeId, usize>::new();
        for (_, function) in module.functions.iter_live() {
            *counts.entry(function.body).or_insert(0) += 1;
        }
        for (_, continuation) in module.continuations.iter_live() {
            *counts.entry(continuation.body).or_insert(0) += 1;
        }
        for (_, node) in module.nodes.iter_live() {
            match node {
                CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                    *counts.entry(*next).or_insert(0) += 1;
                }
                CpsNode::LetFun { body, .. } | CpsNode::LetCont { body, .. } => {
                    *counts.entry(*body).or_insert(0) += 1;
                }
                _ => {}
            }
        }
        for (id, _) in module.nodes.iter_live() {
            assert_eq!(
                counts.get(&id).copied().unwrap_or(0),
                1,
                "{id} must have exactly one owning link for scoped CSE"
            );
        }
    }
}
