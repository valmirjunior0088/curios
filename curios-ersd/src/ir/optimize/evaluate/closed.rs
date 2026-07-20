//! The closed-term evaluation driver.
//!
//! A two-phase plan/apply split: interpretation borrows the module immutably
//! (a closure names a module function) while installing a replacement mutates
//! it. The plan phase finds every closed candidate call and records its
//! freestanding result; the apply phase materializes each result, splices the
//! construction statements ahead of the candidate, rewrites the candidate to
//! an alias or a residual, and re-verifies.

use {
    super::{
        budget::ReifyBudget,
        interpret::{Evaluator, Outcome, Residual},
        reify::{reify, reify_all},
        value::Value,
    },
    crate::{
        Analysis, BlockId, ErasedAtom, ErasedModule, ForeignId, FunctionId, Rhs, Statement,
        StatementId, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// Where a candidate statement lives: inside a block, or in the module's
/// top-level item list.
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

/// Fold every closed call the interpreter can finish, module-wide.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn evaluate_closed_terms(module: &mut ErasedModule) {
    let analysis = Analysis::analyze(module);
    let owners = index_owners(module);
    let planned = plan(module, &analysis, &owners);
    apply(module, planned);
}

fn plan(
    module: &ErasedModule,
    analysis: &Analysis,
    owners: &BTreeMap<StatementId, Owner>,
) -> Vec<Planned> {
    // Every top-level function a reified closure names was bound — by
    // dominance-order erasure — before the statement that first uses the
    // folded call, so it stays in lexical scope even for a candidate nested
    // in a match arm.
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

/// Reification runs first, over every plan, and only *appends* to the arena;
/// only once every plan is reified are the candidates rewritten and the
/// materialized statements spliced in. The order matters: a reified closure
/// deep-copies a source function and must read the original module, not one
/// where an earlier plan's rewrite left an alias whose definition is not yet
/// spliced into its block.
fn apply(module: &mut ErasedModule, planned: Vec<Planned>) {
    if planned.is_empty() {
        return;
    }

    let mut rewrites = Vec::<(StatementId, ValueId, Rhs)>::new();
    let mut spliced_before = BTreeMap::<StatementId, Vec<StatementId>>::new();
    let mut touched = BTreeSet::<Owner>::new();

    for plan in planned {
        let mut spliced = Vec::new();
        let mut budget = ReifyBudget::new();
        let rhs = match plan.kind {
            Kind::Value(value) => match reify(module, &value, &mut budget, &mut spliced) {
                Ok(atom) => Rhs::Alias(atom),
                Err(_) => continue,
            },
            Kind::Foreign(foreign, values) => {
                match reify_all(module, &values, &mut budget, &mut spliced) {
                    Ok(operands) => Rhs::Foreign { foreign, operands },
                    Err(_) => continue,
                }
            }
            Kind::Call(function, values) => {
                match reify_all(module, &values, &mut budget, &mut spliced) {
                    Ok(arguments) => {
                        let callee = ErasedAtom::Function(function);
                        // A residual identical to the original call would
                        // churn forever; leave it untouched.
                        if is_same_call(module, plan.statement, callee, &arguments) {
                            continue;
                        }
                        Rhs::Apply { callee, arguments }
                    }
                    Err(_) => continue,
                }
            }
        };
        rewrites.push((plan.statement, plan.result, rhs));
        if !spliced.is_empty() {
            spliced_before.insert(plan.statement, spliced);
            touched.insert(plan.owner);
        }
    }

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
}

fn is_same_call(
    module: &ErasedModule,
    statement: StatementId,
    callee: ErasedAtom,
    arguments: &[ErasedAtom],
) -> bool {
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

/// Map each statement to its owner — its block, or the top-level item list.
fn index_owners(module: &ErasedModule) -> BTreeMap<StatementId, Owner> {
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
