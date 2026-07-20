//! Materializing a runtime value back into arena statements.
//!
//! A leaf value interns as a [`Constant`] and needs no statement; a list,
//! product, or constructor materializes its already-reified fields into a
//! `Let` binding the corresponding construction right-hand side, appended to
//! `out` in dependency order for the caller to splice ahead of the candidate.
//! A closure result materializes as a deep copy of its function with its
//! reified captures wired in, bound by a `Functions` statement — what makes
//! the runtime-args `Fmt` collapse reachable.

use {
    super::{
        budget::ReifyBudget,
        copy::deep_copy_function,
        value::{Bail, Closure, Value},
    },
    crate::{
        Atom, Constant, FunctionId, Module, Rhs, SequenceOp, Statement, StatementId,
        walk::control_blocks,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// Check that `value` can fully materialize — the same budget charges and
/// closure gates as [`reify`], with no module mutation — so a failed
/// reification never strands half-emitted statements in the arena.
pub(super) fn reify_check(
    module: &Module,
    value: &Value,
    budget: &mut ReifyBudget,
) -> Result<(), Bail> {
    budget.node()?;
    if let Some(constant) = value.as_constant() {
        if let Constant::Bin(grain, value) = &constant {
            budget.payload(value.len(*grain))?;
        }
        return Ok(());
    }
    match value {
        Value::Lst(elements) => {
            budget.payload(elements.len())?;
            for element in elements.iter() {
                reify_check(module, element, budget)?;
            }
            Ok(())
        }
        Value::Product(_, fields) | Value::Construct(_, fields) => {
            for field in fields.iter() {
                reify_check(module, field, budget)?;
            }
            Ok(())
        }
        Value::Closure(closure) => {
            if !outward_functions_item_bound(module, closure.function) {
                return Err(Bail::Unsupported);
            }
            for (_, held) in closure.env.borrow().iter() {
                reify_check(module, held, budget)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// [`reify_check`] over a list of values.
pub(super) fn reify_check_all(
    module: &Module,
    values: &[Value],
    budget: &mut ReifyBudget,
) -> Result<(), Bail> {
    for value in values {
        reify_check(module, value, budget)?;
    }
    Ok(())
}

/// Materialize `value` into `module`, appending construction statements to
/// `out` in dependency order, and return the atom naming the result. The
/// caller has already run [`reify_check`], so failure cannot strand emitted
/// statements.
pub(super) fn reify(
    module: &mut Module,
    value: &Value,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<Atom, Bail> {
    budget.node()?;

    if let Some(constant) = value.as_constant() {
        if let Constant::Bin(grain, value) = &constant {
            budget.payload(value.len(*grain))?;
        }
        return Ok(Atom::Constant(module.intern_constant(constant)));
    }

    match value {
        Value::Lst(elements) => {
            budget.payload(elements.len())?;
            let operands = reify_all(module, elements, budget, out)?;
            Ok(emit(
                module,
                out,
                Rhs::Sequence {
                    operation: SequenceOp::LstBuild,
                    operands,
                },
            ))
        }
        Value::Product(schema, fields) => {
            let fields = reify_all(module, fields, budget, out)?;
            Ok(emit(
                module,
                out,
                Rhs::Product {
                    schema: *schema,
                    fields,
                },
            ))
        }
        Value::Construct(constructor, fields) => {
            let fields = reify_all(module, fields, budget, out)?;
            Ok(emit(
                module,
                out,
                Rhs::Construct {
                    constructor: *constructor,
                    fields,
                },
            ))
        }
        Value::Closure(closure) => reify_closure(module, closure, budget, out),
        Value::Unit
        | Value::Bool(_)
        | Value::Nat(_)
        | Value::Byte(_)
        | Value::Int(_)
        | Value::Flt(_)
        | Value::Io(_)
        | Value::Bin(..) => unreachable!("leaf values are reified through `as_constant`"),
    }
}

/// Materialize a closure: reify each captured value to an atom (nesting
/// captured closures), then deep-copy the closure's function with those atoms
/// substituted for its free values, introduced by a `Functions` statement. A
/// free value the captures do not cover is a top-level identity kept
/// verbatim.
fn reify_closure(
    module: &mut Module,
    closure: &Rc<Closure>,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<Atom, Bail> {
    // The copy keeps outward function references verbatim; every one must be
    // item-bound to stay in scope at an arbitrary splice site — a reference
    // to a *locally* bound function outside the copied region declines.
    if !outward_functions_item_bound(module, closure.function) {
        return Err(Bail::Unsupported);
    }
    let captures = closure.env.borrow().clone();
    let mut substitution = BTreeMap::new();
    for (value, held) in &captures {
        substitution.insert(*value, reify(module, held, budget, out)?);
    }
    let function = deep_copy_function(module, closure.function, &substitution, None)
        .ok_or(Bail::Unsupported)?;
    out.push(module.add_statement(Statement::Functions {
        functions: vec![function],
    }));
    Ok(Atom::Function(function))
}

/// Reify each value in order, collecting the atoms that name them.
pub(super) fn reify_all(
    module: &mut Module,
    values: &[Value],
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<Vec<Atom>, Bail> {
    let mut atoms = Vec::with_capacity(values.len());
    for value in values {
        atoms.push(reify(module, value, budget, out)?);
    }
    Ok(atoms)
}

/// Whether every function the region rooted at `root` references outside
/// itself is bound by a top-level item.
fn outward_functions_item_bound(module: &Module, root: FunctionId) -> bool {
    let mut item_bound = BTreeSet::<FunctionId>::new();
    for &item in module.items() {
        match module.statement(item) {
            Some(Statement::Functions { functions }) => item_bound.extend(functions.iter()),
            Some(Statement::Rec { group }) => {
                if let Some(group) = module.rec_group(*group) {
                    item_bound.extend(group.functions.iter());
                }
            }
            _ => {}
        }
    }

    let mut region = BTreeSet::new();
    let mut outward = BTreeSet::new();
    let mut work = vec![root];
    while let Some(function) = work.pop() {
        if !region.insert(function) {
            continue;
        }
        let Some(definition) = module.function(function) else {
            return false;
        };
        for block in control_blocks(module, definition.body) {
            let Some(block) = module.block(block) else {
                continue;
            };
            for &statement in &block.statements {
                match module.statement(statement) {
                    Some(Statement::Let { rhs, .. }) => {
                        for atom in rhs.operands() {
                            if let Atom::Function(referenced) = atom {
                                outward.insert(referenced);
                            }
                        }
                    }
                    Some(Statement::Functions { functions }) => work.extend(functions),
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            work.extend(&group.functions);
                        }
                    }
                    None => {}
                }
            }
            if let Some(Atom::Function(referenced)) = block.terminator.atom() {
                outward.insert(referenced);
            }
        }
    }
    outward
        .into_iter()
        .all(|function| region.contains(&function) || item_bound.contains(&function))
}

fn emit(module: &mut Module, out: &mut Vec<StatementId>, rhs: Rhs) -> Atom {
    let result = module.add_value(None);
    out.push(module.add_statement(Statement::Let { result, rhs }));
    Atom::Value(result)
}
