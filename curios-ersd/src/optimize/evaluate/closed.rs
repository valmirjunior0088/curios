//! The closed-term evaluation driver.
//!
//! A two-phase plan/apply split: interpretation borrows the module immutably (a closure names a module function) while installing a replacement mutates it. The plan phase finds every closed candidate call and records its freestanding result; the apply phase materializes each result, splices the construction statements ahead of the candidate, rewrites the candidate to an alias or a residual, and re-verifies.

#[cfg(test)]
mod tests;

use {
    super::{
        budget::{PASS_REIFY_BUDGET, ReifyBudget},
        interpret::{Evaluator, Outcome, Residual},
        reify::{ReifyScope, reify, reify_all, reify_check, reify_check_all},
        value::Value,
    },
    crate::{
        Analysis, Atom, BlockId, ForeignId, FunctionId, Module, Rhs, Statement, StatementId,
        ValueId, walk::control_blocks,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Where a candidate statement lives: inside a block, or in the module's top-level item list.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Owner {
    Block(BlockId),
    Items,
}

struct Planned {
    statement: StatementId,
    result: ValueId,
    owner: Owner,
    kind: Kind,
}

enum Kind {
    /// The candidate evaluated to a value; the statement becomes an alias.
    Value(Value),
    /// A tail-position host call.
    Foreign(ForeignId, Vec<Value>),
    /// A tail call to a module function whose body performs an effect.
    Call(FunctionId, Vec<Value>),
}

/// Fold every closed call the interpreter can finish, module-wide. Returns whether anything was installed — a curried chain folds one application per round, so the driver iterates until quiescent.
pub(crate) fn evaluate_closed_terms(module: &mut Module) -> bool {
    curios_profile::profile!("evaluate_closed_terms");
    let analysis = Analysis::analyze(module);
    let owners = index_owners(module);
    let planned = plan(module, &analysis, &owners);
    apply(module, planned)
}

fn plan(
    module: &Module,
    analysis: &Analysis,
    owners: &BTreeMap<StatementId, Owner>,
) -> Vec<Planned> {
    // Every top-level function a reified closure names was bound — by dominance-order erasure — before the statement that first uses the folded call, so it stays in lexical scope even for a candidate nested in a match arm.
    let mut evaluator = Evaluator::new(module, analysis);
    let mut planned = Vec::new();
    for (index, slot) in module.statements().iter().enumerate() {
        let Some(Statement::Let {
            result,
            rhs: Rhs::Apply { callee, arguments },
        }) = slot
        else {
            continue;
        };
        if !evaluator.is_closed_atom(*callee)
            || !arguments.iter().all(|&atom| evaluator.is_closed_atom(atom))
        {
            continue;
        }
        let statement = StatementId(index as u32);
        let Some(&owner) = owners.get(&statement) else {
            continue;
        };
        let kind = match evaluator.evaluate(*callee, arguments) {
            Outcome::Done(value) => Kind::Value(value),
            Outcome::Stuck(Residual::Foreign(foreign, operands)) => {
                Kind::Foreign(foreign, operands)
            }
            Outcome::Stuck(Residual::Call(function, operands)) => Kind::Call(function, operands),
            Outcome::Bail(_) => continue,
        };
        planned.push(Planned {
            statement,
            result: *result,
            owner,
            kind,
        });
    }
    planned
}

/// Reification runs first, over every plan, and only *appends* to the arena; only once every plan is reified are the candidates rewritten and the materialized statements spliced in. The order matters: a reified closure deep-copies a source function and must read the original module, not one where an earlier plan's rewrite left an alias whose definition is not yet spliced into its block.
fn apply(module: &mut Module, planned: Vec<Planned>) -> bool {
    if planned.is_empty() {
        return false;
    }

    let mut rewrites = Vec::<(StatementId, ValueId, Rhs)>::new();
    let mut spliced_before = BTreeMap::<StatementId, Vec<StatementId>>::new();
    let mut touched = BTreeSet::<Owner>::new();
    // Every replacement draws on one pool, so a pass cannot multiply the module however many candidates it found — see `PASS_REIFY_BUDGET`.
    let mut reify_pool = PASS_REIFY_BUDGET;
    // Stable for the whole pass: reification only appends, and the item list is rebuilt after this loop.
    let mut scope = ReifyScope::new();
    // Which item hosts each block, so a candidate inside one binds its group ahead of that item. Stable for the same reason.
    let block_items = index_block_items(module);
    // Where each item stands, so a shared copy is reused only by a candidate its splice point precedes. Stable for the same reason.
    let item_positions: BTreeMap<StatementId, usize> = module
        .items()
        .iter()
        .enumerate()
        .map(|(position, &statement)| (statement, position))
        .collect();

    for plan in planned {
        if reify_pool == 0 {
            break;
        }
        // Dry-run first: a plan that cannot fully materialize is skipped before anything is emitted, so nothing is ever stranded.
        {
            let mut probe = ReifyBudget::within(reify_pool);
            let ok = match &plan.kind {
                Kind::Value(value) => reify_check(module, value, &mut probe, &mut scope).is_ok(),
                Kind::Foreign(_, values) | Kind::Call(_, values) => {
                    reify_check_all(module, values, &mut probe, &mut scope).is_ok()
                }
            };
            if !ok {
                continue;
            }
        }
        let mut spliced = Vec::new();
        let mut budget = ReifyBudget::within(reify_pool);
        // The probe above shares nothing, so it charges at least what this run will; the memos only ever remove copies from under a gate that already fit without them.
        //
        // **Where the group is bound, which is not where the candidate stands.** Every statement a replacement emits is closed by construction — interned constants, functions [`ReifyScope::outward_ok`] proved item-bound, and earlier statements of the same group — so the group can be bound ahead of the *item* enclosing the candidate instead of inside the candidate's own block. Item bindings are ambient for everything after them, so that puts it in scope at the candidate and at every candidate after it, and gives a block-owned candidate an item position to share from.
        //
        // Binding it in the block instead is what made the same grammar cost `n² + 2` copies where the identical applications written at item level cost `n + 2`: a group bound inside a block that need not dominate anything contributed to no other replacement, so every definition re-materialized the whole chain below it.
        //
        // A block the entry expression owns has no item ahead of it. Such a candidate keeps the block-local splice and shares only within itself, which is what every block-owned candidate used to do.
        let (splice_before, splice_owner, position) = match plan.owner {
            Owner::Items => (
                plan.statement,
                Owner::Items,
                item_positions.get(&plan.statement).copied(),
            ),
            Owner::Block(block) => match block_items.get(&block) {
                Some(&item) => (item, Owner::Items, item_positions.get(&item).copied()),
                None => (plan.statement, plan.owner, None),
            },
        };
        scope.begin_replacement(position);
        let rhs = match plan.kind {
            Kind::Value(value) => {
                match reify(module, &value, &mut budget, &mut spliced, &mut scope) {
                    Ok(atom) => Rhs::Alias(atom),
                    Err(_) => continue,
                }
            }
            Kind::Foreign(foreign, values) => {
                match reify_all(module, &values, &mut budget, &mut spliced, &mut scope) {
                    Ok(operands) => Rhs::Foreign { foreign, operands },
                    Err(_) => continue,
                }
            }
            Kind::Call(function, values) => {
                match reify_all(module, &values, &mut budget, &mut spliced, &mut scope) {
                    Ok(arguments) => {
                        let callee = Atom::Function(function);
                        // A residual identical to the original call would churn forever; leave it untouched.
                        if is_same_call(module, plan.statement, callee, &arguments) {
                            continue;
                        }
                        Rhs::Apply { callee, arguments }
                    }
                    Err(_) => continue,
                }
            }
        };
        reify_pool = reify_pool.saturating_sub(budget.spent());
        rewrites.push((plan.statement, plan.result, rhs));
        if !spliced.is_empty() {
            // Appended rather than inserted: every candidate inside one item binds its group ahead of that same item, in the order the plans were applied — which is the order a later group's reuse of an earlier group's copy depends on.
            spliced_before
                .entry(splice_before)
                .or_default()
                .extend(spliced);
            touched.insert(splice_owner);
        }
    }

    let installed = !rewrites.is_empty();
    for (statement, result, rhs) in rewrites {
        module.set_statement(statement, Statement::Let { result, rhs });
    }

    for owner in touched {
        let statements = match owner {
            Owner::Block(block) => module
                .block(block)
                .map(|block| block.statements.clone())
                .unwrap_or_default(),
            Owner::Items => module.items().to_vec(),
        };
        let mut rebuilt = Vec::with_capacity(statements.len());
        for statement in statements {
            if let Some(spliced) = spliced_before.get(&statement) {
                rebuilt.extend(spliced.iter().copied());
            }
            rebuilt.push(statement);
        }
        match owner {
            Owner::Block(block) => module.set_block_statements(block, rebuilt),
            Owner::Items => module.set_items(rebuilt),
        }
    }

    module
        .verify()
        .expect("closed-term evaluation preserves a verifiable module");
    installed
}

fn is_same_call(module: &Module, statement: StatementId, callee: Atom, arguments: &[Atom]) -> bool {
    matches!(
        module.statement(statement),
        Some(Statement::Let {
            rhs: Rhs::Apply {
                callee: original_callee,
                arguments: original_arguments,
            },
            ..
        }) if *original_callee == callee && original_arguments.as_slice() == arguments
    )
}

/// Map every block to the top-level item whose region owns it.
///
/// **What lets a candidate inside a block bind its group where later candidates can see it.** [`Module::verify`] treats the top level as a virtual block — the items in order, then the entry block, with each item's bindings ambient for everything after it — so a group bound ahead of the item *enclosing* a candidate is in scope at that candidate and at every candidate after it. Bound inside the candidate's own block, the same group is in scope for nothing else at all, and that difference is the whole of the quadratic this pass used to produce.
///
/// A `Rec` group's computed members initialize in blocks the group owns, so they map to the item beside the function bodies. A block the *entry expression* owns maps to nothing: there is no item ahead of it, and a candidate there keeps the block-local splice.
///
/// Iterative rather than recursive over the two worklists, because module nesting is what erasure generated rather than what anyone wrote, and the verifier next door already reaches for `recurse` at this shape.
fn index_block_items(module: &Module) -> BTreeMap<BlockId, StatementId> {
    let mut hosts = BTreeMap::new();

    for &item in module.items() {
        let mut functions = Vec::new();
        let mut blocks = Vec::new();
        match module.statement(item) {
            Some(Statement::Functions { functions: bound }) => functions.extend(bound),
            Some(Statement::Rec { group }) => {
                let Some(group) = module.rec_group(*group) else {
                    continue;
                };
                functions.extend(&group.functions);
                blocks.extend(group.values.iter().map(|member| member.init));
            }
            _ => continue,
        }

        let mut seen_functions = BTreeSet::new();
        let mut seen_blocks = BTreeSet::new();
        while !functions.is_empty() || !blocks.is_empty() {
            while let Some(function) = functions.pop() {
                if !seen_functions.insert(function) {
                    continue;
                }
                if let Some(definition) = module.function(function) {
                    blocks.push(definition.body);
                }
            }
            while let Some(root) = blocks.pop() {
                for block in control_blocks(module, root) {
                    if !seen_blocks.insert(block) {
                        continue;
                    }
                    hosts.insert(block, item);
                    let Some(definition) = module.block(block) else {
                        continue;
                    };
                    for &statement in &definition.statements {
                        match module.statement(statement) {
                            Some(Statement::Functions { functions: bound }) => {
                                functions.extend(bound)
                            }
                            Some(Statement::Rec { group }) => {
                                if let Some(group) = module.rec_group(*group) {
                                    functions.extend(&group.functions);
                                    blocks.extend(group.values.iter().map(|member| member.init));
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    hosts
}

/// Map each statement to its owner — its block, or the top-level item list.
fn index_owners(module: &Module) -> BTreeMap<StatementId, Owner> {
    let mut owners = BTreeMap::new();
    for (index, slot) in module.blocks().iter().enumerate() {
        if let Some(block) = slot {
            let id = BlockId(index as u32);
            for &statement in &block.statements {
                owners.insert(statement, Owner::Block(id));
            }
        }
    }
    for &item in module.items() {
        owners.insert(item, Owner::Items);
    }
    owners
}
