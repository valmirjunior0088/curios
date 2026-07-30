//! Deep-copying a function region into fresh arena identities.
//!
//! Both partial-evaluation drivers reproduce a function's body under fresh identities: closure reification copies a lambda with its resolved captures wired in, and the spine specializer mints a specialized copy with one parameter bound to a literal. A copy is not a structural clone: it mints a fresh identity for every block, statement, value, function, and recursive group the region owns, and rewrites every reference through the old→new maps. References that leave the region — top-level identities and constants — are rewritten by the caller's substitution or kept verbatim.

use {
    super::super::super::walk::control_blocks,
    crate::{
        Atom, Block, BlockId, FoldNatStep, FoldSequenceStep, Function, FunctionId, Module, NatCase,
        RecGroup, RecGroupId, RecValue, Rhs, Statement, StatementId, Terminator, ValueId,
        VariantArm,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Deep-copy `source` and everything it lexically owns into fresh identities, rewriting each free value named in `substitution` to its replacement atom. With `self_reference` given, a `Function(source)` reference in the copy is kept pointing there rather than at the copy — the spine specializer passes the original target so the copy's self-recursion stays generic for its fold to re-specialize. Returns the fresh identity of the copied `source`, or `None` if the region references a tombstoned entity.
pub(super) fn deep_copy_function(
    module: &mut Module,
    source: FunctionId,
    substitution: &BTreeMap<ValueId, Atom>,
    self_reference: Option<FunctionId>,
) -> Option<FunctionId> {
    let region = gather_region(module, source)?;

    // Snapshot every structure before any allocation mutates the arena.
    let functions = clone_all(&region.functions, |id| module.function(id).cloned())?;
    let blocks = clone_all(&region.blocks, |id| module.block(id).cloned())?;
    let statements = clone_all(&region.statements, |id| module.statement(id).cloned())?;
    let rec_groups = clone_all(&region.rec_groups, |id| module.rec_group(id).cloned())?;

    let values: BTreeMap<ValueId, ValueId> = region
        .values
        .iter()
        .map(|&value| (value, module.add_value(None)))
        .collect();
    let block_ids: BTreeMap<BlockId, BlockId> = region
        .blocks
        .iter()
        .map(|&block| (block, module.reserve_block()))
        .collect();
    let function_ids: BTreeMap<FunctionId, FunctionId> = region
        .functions
        .iter()
        .map(|&function| (function, module.reserve_function()))
        .collect();

    let mut rec_group_ids = BTreeMap::new();
    for (old, group) in &rec_groups {
        let remapped = RecGroup {
            functions: group
                .functions
                .iter()
                .map(|&function| lookup(&function_ids, function))
                .collect(),
            values: group
                .values
                .iter()
                .map(|member| RecValue {
                    value: lookup(&values, member.value),
                    init: lookup(&block_ids, member.init),
                })
                .collect(),
        };
        rec_group_ids.insert(*old, module.add_rec_group(remapped));
    }

    let redirect = self_reference.map(|original| (lookup(&function_ids, source), original));

    let remap = Remap {
        values: &values,
        blocks: &block_ids,
        functions: &function_ids,
        rec_groups: &rec_group_ids,
        substitution,
        redirect,
    };

    let mut statement_ids = BTreeMap::new();
    for (old, statement) in &statements {
        let id = module.add_statement(remap.statement(statement));
        statement_ids.insert(*old, id);
    }
    for (old, block) in &blocks {
        let remapped = Block {
            statements: block
                .statements
                .iter()
                .map(|statement| lookup(&statement_ids, *statement))
                .collect(),
            terminator: remap.terminator(&block.terminator),
        };
        module.define_block(lookup(&block_ids, *old), remapped);
    }
    for (old, function) in &functions {
        let remapped = Function {
            debug_name: function.debug_name.clone(),
            params: function
                .params
                .iter()
                .map(|&p| lookup(&values, p))
                .collect(),
            body: lookup(&block_ids, function.body),
        };
        module.define_function(lookup(&function_ids, *old), remapped);
    }

    Some(lookup(&function_ids, source))
}

/// The identities a function region owns, in deterministic order: the function and every function nested in it, and all of their control blocks, statements, bound values, and recursive groups. Iterative.
struct Region {
    functions: Vec<FunctionId>,
    blocks: Vec<BlockId>,
    statements: Vec<StatementId>,
    values: Vec<ValueId>,
    rec_groups: Vec<RecGroupId>,
}

fn gather_region(module: &Module, source: FunctionId) -> Option<Region> {
    let mut functions = Vec::new();
    let mut seen_functions = BTreeSet::new();
    let mut blocks = BTreeSet::new();
    let mut statements = Vec::new();
    let mut values = Vec::new();
    let mut rec_groups = Vec::new();
    let mut seen_rec_groups = BTreeSet::new();

    let mut work = vec![source];
    while let Some(function_id) = work.pop() {
        if !seen_functions.insert(function_id) {
            continue;
        }
        functions.push(function_id);
        let function = module.function(function_id)?;
        values.extend(&function.params);
        for block in control_blocks(module, function.body) {
            if !blocks.insert(block) {
                continue;
            }
            let Some(definition) = module.block(block) else {
                continue;
            };
            for &statement in &definition.statements {
                statements.push(statement);
                match module.statement(statement) {
                    Some(Statement::Let { result, rhs }) => {
                        values.push(*result);
                        values.extend(rhs.binders());
                    }
                    Some(Statement::Functions { functions }) => work.extend(functions),
                    Some(Statement::Rec { group }) => {
                        if seen_rec_groups.insert(*group) {
                            rec_groups.push(*group);
                        }
                        if let Some(group) = module.rec_group(*group) {
                            work.extend(&group.functions);
                            values.extend(group.values.iter().map(|member| member.value));
                        }
                    }
                    None => {}
                }
            }
        }
    }

    Some(Region {
        functions,
        blocks: blocks.into_iter().collect(),
        statements,
        values,
        rec_groups,
    })
}

fn clone_all<I: Copy, T>(ids: &[I], read: impl Fn(I) -> Option<T>) -> Option<Vec<(I, T)>> {
    ids.iter().map(|&id| Some((id, read(id)?))).collect()
}

/// Look an identity up in a remap expected to contain it, keeping it unchanged when it does not (an identity outside the copied region).
fn lookup<I: Ord + Copy>(map: &BTreeMap<I, I>, id: I) -> I {
    map.get(&id).copied().unwrap_or(id)
}

struct Remap<'a> {
    values: &'a BTreeMap<ValueId, ValueId>,
    blocks: &'a BTreeMap<BlockId, BlockId>,
    functions: &'a BTreeMap<FunctionId, FunctionId>,
    rec_groups: &'a BTreeMap<RecGroupId, RecGroupId>,
    substitution: &'a BTreeMap<ValueId, Atom>,
    /// When set, a `Function` atom that would remap to `from` (the copy of the region's root) is kept as `to` (the original) instead. Structural bindings are never redirected.
    redirect: Option<(FunctionId, FunctionId)>,
}

impl Remap<'_> {
    fn atom(&self, atom: Atom) -> Atom {
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

    fn value(&self, value: ValueId) -> ValueId {
        lookup(self.values, value)
    }

    fn block(&self, block: BlockId) -> BlockId {
        lookup(self.blocks, block)
    }

    fn statement(&self, statement: &Statement) -> Statement {
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

    fn terminator(&self, terminator: &Terminator) -> Terminator {
        match terminator {
            Terminator::Return(atom) => Terminator::Return(self.atom(*atom)),
            Terminator::Exit(atom) => Terminator::Exit(self.atom(*atom)),
            Terminator::Unreachable => Terminator::Unreachable,
        }
    }

    fn rhs(&self, rhs: &Rhs) -> Rhs {
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
}
