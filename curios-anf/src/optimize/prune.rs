//! Reachability pruning — the shrink-before-lower pass.
//!
//! A linked program's item chain carries every item of its own and what its items and its entry reach of its scope; lowering them and running CPS's whole-module fixpoint over them is this stage's dominant cost, and CPS cannot recover the loss: item granularity dissolves into the entry's code at CPS conversion. A top-level item is kept where it is **reachable** — it binds something the entry block (or a kept item) transitively references — and dropped otherwise, whatever its evaluation may do: a program computes a top-level value the first time it reads it ([`lower_to_cps`](crate::lower_to_cps)), so one nothing reads it never computes.
//!
//! Function and value reachability are mutually dependent, so both close together over the item reference graph, seeded by the entry block's free references. Kept items retain their original order (definition before use), everything owned only by dropped items is tombstoned, and the pruned module re-verifies. Deterministic: identity order everywhere.

#[cfg(test)]
mod tests;

use {
    crate::{
        Atom, BlockId, FunctionId, Module, RecGroupId, Statement, StatementId, Terminator, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// The entities one item's whole subtree binds and references — nested function bodies included. Free references (`used − bound`) drive reachability; the bound and owned sets are what to keep when it survives.
#[derive(Default)]
struct Subtree {
    used_functions: BTreeSet<FunctionId>,
    used_values: BTreeSet<ValueId>,
    bound_functions: BTreeSet<FunctionId>,
    bound_values: BTreeSet<ValueId>,
    blocks: BTreeSet<BlockId>,
    statements: BTreeSet<StatementId>,
    rec_groups: BTreeSet<RecGroupId>,
}

impl Subtree {
    fn free_functions(&self) -> impl Iterator<Item = FunctionId> + '_ {
        self.used_functions
            .difference(&self.bound_functions)
            .copied()
    }

    fn free_values(&self) -> impl Iterator<Item = ValueId> + '_ {
        self.used_values.difference(&self.bound_values).copied()
    }
}

/// Drop the items the program does not reach, a recursive group with its web among them.
pub fn prune_unreachable(module: &mut Module) {
    curios_profile::profile!("prune_unreachable");
    let Some(entry) = module.entry() else { return };
    let items = module.items().to_vec();

    // Each item's subtree is disjoint from the others', so one walk apiece maps every bound entity to exactly one item.
    let subtrees: Vec<Subtree> = items
        .iter()
        .map(|&item| walk_subtree(module, vec![item], Vec::new()))
        .collect();
    let entry_subtree = walk_subtree(module, Vec::new(), vec![entry]);

    let mut binder_function = BTreeMap::<FunctionId, usize>::new();
    let mut binder_value = BTreeMap::<ValueId, usize>::new();
    for (index, subtree) in subtrees.iter().enumerate() {
        for &function in &subtree.bound_functions {
            binder_function.entry(function).or_insert(index);
        }
        for &value in &subtree.bound_values {
            binder_value.entry(value).or_insert(index);
        }
    }

    // The items binding what `subtree` references and does not bind itself.
    let binders = |subtree: &Subtree| {
        let functions = subtree
            .free_functions()
            .filter_map(|function| binder_function.get(&function));
        let values = subtree
            .free_values()
            .filter_map(|value| binder_value.get(&value));

        functions.chain(values).copied().collect::<Vec<_>>()
    };
    // What `roots` reach over the item reference graph, themselves among it.
    let closed = |roots: Vec<usize>| {
        let mut kept = BTreeSet::<usize>::new();
        let mut work = roots;
        while let Some(index) = work.pop() {
            if kept.insert(index) {
                work.extend(binders(&subtrees[index]));
            }
        }

        kept
    };
    curios_profile::sample!("prune::items", items.len());

    let kept = closed(binders(&entry_subtree));
    curios_profile::sample!("prune::kept", kept.len());

    // Retain the kept items in order and everything they (or the entry block's own region) own; tombstone the rest.
    let mut keep_functions = entry_subtree.bound_functions.clone();
    keep_functions.extend(entry_subtree.used_functions.iter());
    let mut keep_values = entry_subtree.bound_values.clone();
    let mut keep_blocks = entry_subtree.blocks.clone();
    keep_blocks.insert(entry);
    let mut keep_statements = entry_subtree.statements.clone();
    let mut keep_rec_groups = entry_subtree.rec_groups.clone();

    let mut retained = Vec::<StatementId>::new();
    for (index, &item) in items.iter().enumerate() {
        if !kept.contains(&index) {
            continue;
        }
        retained.push(item);
        keep_statements.insert(item);
        let subtree = &subtrees[index];
        keep_functions.extend(&subtree.bound_functions);
        keep_values.extend(&subtree.bound_values);
        keep_blocks.extend(&subtree.blocks);
        keep_statements.extend(&subtree.statements);
        keep_rec_groups.extend(&subtree.rec_groups);
    }

    module.set_items(retained);
    module.retain_functions(&keep_functions);
    module.retain_blocks(&keep_blocks);
    module.retain_statements(&keep_statements);
    module.retain_values(&keep_values);
    module.retain_rec_groups(&keep_rec_groups);

    module
        .verify()
        .expect("the reachability prune preserves a verifiable module");
}

/// Walk one region's whole subtree — statements, control sub-blocks, nested function bodies, recursive-group initializers — collecting what it binds, owns, and references. Iterative.
fn walk_subtree(module: &Module, statements: Vec<StatementId>, blocks: Vec<BlockId>) -> Subtree {
    let mut subtree = Subtree::default();
    let mut statements = statements;
    let mut blocks = blocks;
    let mut functions: Vec<FunctionId> = Vec::new();

    let use_atom = |subtree: &mut Subtree, atom: Atom| match atom {
        Atom::Value(value) => {
            subtree.used_values.insert(value);
        }
        Atom::Function(function) => {
            subtree.used_functions.insert(function);
        }
        Atom::Constant(_) => {}
    };

    loop {
        if let Some(statement) = statements.pop() {
            if !subtree.statements.insert(statement) {
                continue;
            }
            match module.statement(statement) {
                Some(Statement::Let { result, rhs }) => {
                    subtree.bound_values.insert(*result);
                    subtree.bound_values.extend(rhs.binders());
                    for atom in rhs.operands() {
                        use_atom(&mut subtree, atom);
                    }
                    blocks.extend(rhs.sub_blocks());
                }
                Some(Statement::Functions { functions: members }) => {
                    subtree.bound_functions.extend(members.iter().copied());
                    functions.extend(members.iter().copied());
                }
                Some(Statement::Rec { group }) => {
                    subtree.rec_groups.insert(*group);
                    if let Some(group) = module.rec_group(*group) {
                        subtree
                            .bound_functions
                            .extend(group.functions.iter().copied());
                        functions.extend(group.functions.iter().copied());
                        for member in &group.values {
                            subtree.bound_values.insert(member.value);
                            blocks.push(member.init);
                        }
                    }
                }
                None => {}
            }
            continue;
        }
        if let Some(function) = functions.pop() {
            if let Some(function) = module.function(function) {
                subtree.bound_values.extend(function.params.iter().copied());
                blocks.push(function.body);
            }
            continue;
        }
        let Some(block) = blocks.pop() else { break };
        if !subtree.blocks.insert(block) {
            continue;
        }
        if let Some(block) = module.block(block) {
            statements.extend(block.statements.iter().copied());
            match &block.terminator {
                Terminator::Return(atom) => use_atom(&mut subtree, *atom),
                Terminator::Halt { operands, .. } => {
                    for &atom in operands {
                        use_atom(&mut subtree, atom);
                    }
                }
                Terminator::Unreachable => {}
            }
        }
    }
    subtree
}
