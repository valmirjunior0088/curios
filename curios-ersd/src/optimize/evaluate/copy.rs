//! Deep-copying a function region into fresh arena identities.
//!
//! Both partial-evaluation drivers reproduce a function's body under fresh identities: closure reification copies a lambda with its resolved captures wired in, and the spine specializer mints a specialized copy with one parameter bound to a literal. A copy is not a structural clone: it mints a fresh identity for every block, statement, value, function, and recursive group the region owns, and rewrites every reference through the old→new maps. References that leave the region — top-level identities and constants — are rewritten by the caller's substitution or kept verbatim.

use {
    crate::{
        Atom, Block, BlockId, Function, FunctionId, Module, RecGroup, RecGroupId, RecValue,
        Statement, StatementId, ValueId,
        remap::{Remap, lookup},
        walk::control_blocks,
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

    // A copier mints its own statements, so it renumbers none: an empty map leaves every statement identity untouched.
    let statement_remap = BTreeMap::new();
    let remap = Remap {
        values: &values,
        blocks: &block_ids,
        functions: &function_ids,
        rec_groups: &rec_group_ids,
        statements: &statement_remap,
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
