//! Rewriting every identity a node holds, through old→new maps.
//!
//! Two passes need this and they need it for opposite reasons. Deep-copying a region mints fresh identities for what it duplicates and rewrites references into them, leaving anything outside the region alone. Compaction moves *every* live identity at once and must rewrite all of them. One traversal serves both because the shape being walked is the same; only the maps differ.
//!
//! **The lookup is deliberately lenient** — an identity absent from a map passes through unchanged. That is what lets a copier hand over partial maps covering only its region. It also means a compaction whose maps are incomplete rewrites nothing and reports nothing, so compaction verifies the module afterwards rather than trusting the walk.

use {
    crate::{
        Atom, Block, BlockId, FoldNatStep, FoldSequenceStep, Function, FunctionId, NatCase,
        RecGroup, RecGroupId, RecValue, Rhs, Statement, StatementId, Terminator,
        UnconsSequenceStep, ValueId, VariantArm,
    },
    std::collections::BTreeMap,
};

/// Look an identity up in a remap expected to contain it, keeping it unchanged when it does not (an identity outside the copied region).
pub(crate) fn lookup<I: Ord + Copy>(map: &BTreeMap<I, I>, id: I) -> I {
    map.get(&id).copied().unwrap_or(id)
}

pub(crate) struct Remap<'a> {
    pub(crate) values: &'a BTreeMap<ValueId, ValueId>,
    pub(crate) blocks: &'a BTreeMap<BlockId, BlockId>,
    pub(crate) functions: &'a BTreeMap<FunctionId, FunctionId>,
    pub(crate) rec_groups: &'a BTreeMap<RecGroupId, RecGroupId>,
    /// Statement identities, which a copier never renumbers and compaction always does.
    pub(crate) statements: &'a BTreeMap<StatementId, StatementId>,
    pub(crate) substitution: &'a BTreeMap<ValueId, Atom>,
    /// When set, a `Function` atom that would remap to `from` (the copy of the region's root) is kept as `to` (the original) instead. Structural bindings are never redirected.
    pub(crate) redirect: Option<(FunctionId, FunctionId)>,
}

impl Remap<'_> {
    pub(crate) fn atom(&self, atom: Atom) -> Atom {
        match atom {
            Atom::Value(value) => {
                if let Some(&fresh) = self.values.get(&value) {
                    Atom::Value(fresh)
                } else if let Some(&replacement) = self.substitution.get(&value) {
                    replacement
                } else {
                    Atom::Value(value)
                }
            }
            Atom::Function(function) => {
                let mapped = lookup(self.functions, function);
                let mapped = match self.redirect {
                    Some((from, to)) if mapped == from => to,
                    _ => mapped,
                };
                Atom::Function(mapped)
            }
            other => other,
        }
    }

    pub(crate) fn value(&self, value: ValueId) -> ValueId {
        lookup(self.values, value)
    }

    pub(crate) fn block(&self, block: BlockId) -> BlockId {
        lookup(self.blocks, block)
    }

    pub(crate) fn statement(&self, statement: &Statement) -> Statement {
        match statement {
            Statement::Let { result, rhs } => Statement::Let {
                result: self.value(*result),
                rhs: self.rhs(rhs),
            },
            Statement::Functions { functions } => Statement::Functions {
                functions: functions
                    .iter()
                    .map(|&function| lookup(self.functions, function))
                    .collect(),
            },
            Statement::Rec { group } => Statement::Rec {
                group: lookup(self.rec_groups, *group),
            },
        }
    }

    pub(crate) fn terminator(&self, terminator: &Terminator) -> Terminator {
        match terminator {
            Terminator::Return(atom) => Terminator::Return(self.atom(*atom)),
            Terminator::Exit(atom) => Terminator::Exit(self.atom(*atom)),
            Terminator::Unreachable => Terminator::Unreachable,
        }
    }

    pub(crate) fn rhs(&self, rhs: &Rhs) -> Rhs {
        let atoms = |operands: &[Atom]| operands.iter().map(|&atom| self.atom(atom)).collect();
        match rhs {
            Rhs::Alias(atom) => Rhs::Alias(self.atom(*atom)),
            Rhs::Apply { callee, arguments } => Rhs::Apply {
                callee: self.atom(*callee),
                arguments: atoms(arguments),
            },
            Rhs::Operation {
                operation,
                operands,
            } => Rhs::Operation {
                operation: *operation,
                operands: atoms(operands),
            },
            Rhs::Sequence {
                operation,
                operands,
            } => Rhs::Sequence {
                operation: *operation,
                operands: atoms(operands),
            },
            Rhs::Product { schema, fields } => Rhs::Product {
                schema: *schema,
                fields: atoms(fields),
            },
            Rhs::Construct {
                constructor,
                fields,
            } => Rhs::Construct {
                constructor: *constructor,
                fields: atoms(fields),
            },
            Rhs::Project {
                schema,
                product,
                field,
            } => Rhs::Project {
                schema: *schema,
                product: self.atom(*product),
                field: *field,
            },
            Rhs::MatchVariant {
                family,
                scrutinee,
                arms,
                default,
            } => Rhs::MatchVariant {
                family: *family,
                scrutinee: self.atom(*scrutinee),
                arms: arms
                    .iter()
                    .map(|arm| VariantArm {
                        constructor: arm.constructor,
                        bindings: arm.bindings.iter().map(|&b| self.value(b)).collect(),
                        block: self.block(arm.block),
                    })
                    .collect(),
                default: default.map(|block| self.block(block)),
            },
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => Rhs::SwitchBool {
                scrutinee: self.atom(*scrutinee),
                if_false: self.block(*if_false),
                if_true: self.block(*if_true),
            },
            Rhs::SwitchNat {
                scrutinee,
                cases,
                default,
            } => Rhs::SwitchNat {
                scrutinee: self.atom(*scrutinee),
                cases: cases
                    .iter()
                    .map(|case| NatCase {
                        key: case.key,
                        block: self.block(case.block),
                    })
                    .collect(),
                default: self.block(*default),
            },
            Rhs::FoldNat {
                scrutinee,
                zero,
                step,
            } => Rhs::FoldNat {
                scrutinee: self.atom(*scrutinee),
                zero: self.block(*zero),
                step: FoldNatStep {
                    predecessor: self.value(step.predecessor),
                    hypothesis: self.value(step.hypothesis),
                    block: self.block(step.block),
                },
            },
            Rhs::FoldSequence {
                grain,
                scrutinee,
                empty,
                step,
            } => Rhs::FoldSequence {
                grain: *grain,
                scrutinee: self.atom(*scrutinee),
                empty: self.block(*empty),
                step: FoldSequenceStep {
                    element: self.value(step.element),
                    suffix: self.value(step.suffix),
                    accumulator: self.value(step.accumulator),
                    block: self.block(step.block),
                },
            },
            Rhs::UnconsSequence {
                grain,
                scrutinee,
                empty,
                cons,
            } => Rhs::UnconsSequence {
                grain: *grain,
                scrutinee: self.atom(*scrutinee),
                empty: self.block(*empty),
                cons: UnconsSequenceStep {
                    element: self.value(cons.element),
                    suffix: self.value(cons.suffix),
                    block: self.block(cons.block),
                },
            },
            Rhs::Cell {
                operation,
                operands,
            } => Rhs::Cell {
                operation: *operation,
                operands: atoms(operands),
            },
            Rhs::Foreign { foreign, operands } => Rhs::Foreign {
                foreign: *foreign,
                operands: atoms(operands),
            },
            Rhs::Intrinsic {
                intrinsic,
                operands,
            } => Rhs::Intrinsic {
                intrinsic: *intrinsic,
                operands: atoms(operands),
            },
        }
    }

    pub(crate) fn statement_id(&self, statement: StatementId) -> StatementId {
        lookup(self.statements, statement)
    }

    /// A block's own contents — the statement list a copier rebuilds itself, and the terminator.
    pub(crate) fn block_body(&self, block: &Block) -> Block {
        Block {
            statements: block
                .statements
                .iter()
                .map(|&statement| self.statement_id(statement))
                .collect(),
            terminator: self.terminator(&block.terminator),
        }
    }

    pub(crate) fn function(&self, function: &Function) -> Function {
        Function {
            debug_name: function.debug_name.clone(),
            params: function.params.iter().map(|&p| self.value(p)).collect(),
            body: self.block(function.body),
            description: function.description,
        }
    }

    pub(crate) fn rec_group(&self, group: &RecGroup) -> RecGroup {
        RecGroup {
            functions: group
                .functions
                .iter()
                .map(|&function| lookup(self.functions, function))
                .collect(),
            values: group
                .values
                .iter()
                .map(|member| RecValue {
                    value: self.value(member.value),
                    init: self.block(member.init),
                })
                .collect(),
        }
    }
}
