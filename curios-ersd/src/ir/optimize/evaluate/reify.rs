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
    crate::{Constant, ErasedAtom, ErasedModule, Rhs, SequenceOp, Statement, StatementId},
    std::{collections::BTreeMap, rc::Rc},
};

/// Materialize `value` into `module`, appending construction statements to
/// `out` in dependency order, and return the atom naming the result.
pub(super) fn reify(
    module: &mut ErasedModule,
    value: &Value,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<ErasedAtom, Bail> {
    budget.node()?;

    if let Some(constant) = value.as_constant() {
        if let Constant::Bin(grain, value) = &constant {
            budget.payload(value.len(*grain))?;
        }
        return Ok(ErasedAtom::Constant(module.intern_constant(constant)));
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
        | Value::Bln(_)
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
    module: &mut ErasedModule,
    closure: &Rc<Closure>,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<ErasedAtom, Bail> {
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
    Ok(ErasedAtom::Function(function))
}

/// Reify each value in order, collecting the atoms that name them.
pub(super) fn reify_all(
    module: &mut ErasedModule,
    values: &[Value],
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
) -> Result<Vec<ErasedAtom>, Bail> {
    let mut atoms = Vec::with_capacity(values.len());
    for value in values {
        atoms.push(reify(module, value, budget, out)?);
    }
    Ok(atoms)
}

fn emit(module: &mut ErasedModule, out: &mut Vec<StatementId>, rhs: Rhs) -> ErasedAtom {
    let result = module.add_value(None);
    out.push(module.add_statement(Statement::Let { result, rhs }));
    ErasedAtom::Value(result)
}
