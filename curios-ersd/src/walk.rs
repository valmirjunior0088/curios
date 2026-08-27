//! Structural traversal intrinsics shared by the derived analyses (and any later pass that walks the graph without caring which form it visits).
//!
//! Each accessor enumerates one structural dimension of a right-hand side — its atom operands, its owned sub-blocks, the values its sub-blocks bind — in deterministic evaluation order. Consumers drive their own explicit worklists over these, keeping every whole-module walk iterative.

use {
    super::{Atom, BlockId, Module, Rhs, Statement, Terminator, ValueId},
    std::collections::BTreeSet,
};

impl Rhs {
    /// Every atom operand this right-hand side evaluates or references, in evaluation order — including callees and scrutinees.
    pub fn operands(&self) -> Vec<Atom> {
        match self {
            Self::Alias(atom) => vec![*atom],
            Self::Apply { callee, arguments } => {
                let mut atoms = Vec::with_capacity(arguments.len() + 1);
                atoms.push(*callee);
                atoms.extend(arguments);
                atoms
            }
            Self::Operation { operands, .. }
            | Self::Sequence { operands, .. }
            | Self::Cell { operands, .. }
            | Self::Foreign { operands, .. }
            | Self::Intrinsic { operands, .. } => operands.clone(),
            Self::Product { fields, .. } | Self::Construct { fields, .. } => fields.clone(),
            Self::Project { product, .. } => vec![*product],
            Self::MatchVariant { scrutinee, .. }
            | Self::SwitchBool { scrutinee, .. }
            | Self::SwitchNat { scrutinee, .. }
            | Self::FoldNat { scrutinee, .. }
            | Self::FoldSequence { scrutinee, .. }
            | Self::UnconsSequence { scrutinee, .. } => vec![*scrutinee],
        }
    }

    /// Every block this right-hand side owns, in evaluation order.
    pub fn sub_blocks(&self) -> Vec<BlockId> {
        match self {
            Self::Alias(_)
            | Self::Apply { .. }
            | Self::Operation { .. }
            | Self::Sequence { .. }
            | Self::Product { .. }
            | Self::Construct { .. }
            | Self::Project { .. }
            | Self::Cell { .. }
            | Self::Foreign { .. }
            | Self::Intrinsic { .. } => Vec::new(),
            Self::MatchVariant { arms, default, .. } => {
                arms.iter().map(|arm| arm.block).chain(*default).collect()
            }
            Self::SwitchBool {
                if_false, if_true, ..
            } => vec![*if_false, *if_true],
            Self::SwitchNat { cases, default, .. } => cases
                .iter()
                .map(|case| case.block)
                .chain([*default])
                .collect(),
            Self::FoldNat { zero, step, .. } => vec![*zero, step.block],
            Self::FoldSequence { empty, step, .. } => vec![*empty, step.block],
            Self::UnconsSequence { empty, cons, .. } => vec![*empty, cons.block],
        }
    }

    /// Every value the sub-blocks of this right-hand side bind — arm payload binders and fold step binders.
    pub fn binders(&self) -> Vec<ValueId> {
        match self {
            Self::MatchVariant { arms, .. } => {
                arms.iter().flat_map(|arm| arm.bindings.clone()).collect()
            }
            Self::FoldNat { step, .. } => vec![step.predecessor, step.hypothesis],
            Self::FoldSequence { step, .. } => {
                vec![step.element, step.suffix, step.accumulator]
            }
            Self::UnconsSequence { cons, .. } => vec![cons.element, cons.suffix],
            _ => Vec::new(),
        }
    }
}

impl Terminator {
    /// The atom this terminator yields, when it yields one.
    pub fn atom(&self) -> Option<Atom> {
        match self {
            Self::Return(atom) | Self::Exit(atom) => Some(*atom),
            Self::Unreachable => None,
        }
    }
}

/// Every control block reachable from `root` — the block itself and the sub-blocks of its statements, transitively — without entering a nested function's body. Iterative, deterministic order.
///
/// A recursive group's *value* members are reached, its *function* members are not, and the asymmetry is the same one the sentence above draws. A computed member's `init` is an inline block of this function, run where the group is entered; a function member has a body of its own, which a caller walks by following the group's functions. Omitting an init block makes it invisible to every consumer here — and to a deep copy that decides what it owns by this walk, invisible reads as *outward*, so the copy keeps pointing at the original's block and the two come to own it jointly.
pub(crate) fn control_blocks(module: &Module, root: BlockId) -> Vec<BlockId> {
    let mut blocks = Vec::new();
    let mut seen = BTreeSet::new();
    let mut work = vec![root];
    while let Some(block) = work.pop() {
        if !seen.insert(block) {
            continue;
        }
        blocks.push(block);
        let Some(definition) = module.block(block) else {
            continue;
        };
        for &statement in &definition.statements {
            match module.statement(statement) {
                Some(Statement::Let { rhs, .. }) => work.extend(rhs.sub_blocks()),
                Some(Statement::Rec { group }) => {
                    if let Some(group) = module.rec_group(*group) {
                        work.extend(group.values.iter().map(|member| member.init));
                    }
                }
                Some(Statement::Functions { .. }) | None => {}
            }
        }
    }
    blocks
}

#[cfg(test)]
mod tests;
