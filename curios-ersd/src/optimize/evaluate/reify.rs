//! Materializing a runtime value back into arena statements.
//!
//! A leaf value interns as a [`Constant`] and needs no statement; a list, product, or constructor materializes its already-reified fields into a `Let` binding the corresponding construction right-hand side, appended to `out` in dependency order for the caller to splice ahead of the candidate. A closure result materializes as a deep copy of its function with its reified captures wired in, bound by a `Functions` statement — what makes the runtime-args `Fmt` collapse reachable.

use {
    super::{
        budget::ReifyBudget,
        copy::{copy_weight, deep_copy_function},
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

/// Check that `value` can fully materialize — the same budget charges and closure gates as [`reify`], with no module mutation — so a failed reification never strands half-emitted statements in the arena.
pub(super) fn reify_check(
    module: &Module,
    value: &Value,
    budget: &mut ReifyBudget,
    scope: &mut ReifyScope,
) -> Result<(), Bail> {
    budget.node()?;
    if let Some(constant) = value.as_constant() {
        if let Constant::Bin(grain, value) = &constant {
            budget.payload(value.len(*grain))?;
        }
        return Ok(());
    }
    match value {
        Value::List(elements) => {
            budget.payload(elements.len())?;
            for element in elements.iter() {
                reify_check(module, element, budget, scope)?;
            }
            Ok(())
        }
        Value::Product(_, fields) | Value::Construct(_, fields) => {
            for field in fields.iter() {
                reify_check(module, field, budget, scope)?;
            }
            Ok(())
        }
        Value::Closure(closure) => {
            if !scope.outward_ok(module, closure.function) {
                return Err(Bail::Unsupported);
            }
            // A closure over nothing needs no specialized copy — see `reify_closure`.
            if closure.env.borrow().is_empty() && scope.is_item_bound(module, closure.function) {
                return Ok(());
            }
            budget.bulk(
                scope
                    .weigh(module, closure.function)
                    .ok_or(Bail::Unsupported)?,
            )?;
            for (_, held) in closure.env.borrow().iter() {
                reify_check(module, held, budget, scope)?;
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
    scope: &mut ReifyScope,
) -> Result<(), Bail> {
    for value in values {
        reify_check(module, value, budget, scope)?;
    }
    Ok(())
}

/// Materialize `value` into `module`, appending construction statements to `out` in dependency order, and return the atom naming the result. The caller has already run [`reify_check`], so failure cannot strand emitted statements.
pub(super) fn reify(
    module: &mut Module,
    value: &Value,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
    scope: &mut ReifyScope,
) -> Result<Atom, Bail> {
    budget.node()?;

    if let Some(constant) = value.as_constant() {
        if let Constant::Bin(grain, value) = &constant {
            budget.payload(value.len(*grain))?;
        }
        return Ok(Atom::Constant(module.intern_constant(constant)));
    }

    match value {
        Value::List(elements) => {
            budget.payload(elements.len())?;
            let operands = reify_all(module, elements, budget, out, scope)?;
            Ok(emit(
                module,
                out,
                Rhs::Sequence {
                    operation: SequenceOp::ListBuild,
                    operands,
                },
            ))
        }
        Value::Product(schema, fields) => {
            let fields = reify_all(module, fields, budget, out, scope)?;
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
            let fields = reify_all(module, fields, budget, out, scope)?;
            Ok(emit(
                module,
                out,
                Rhs::Construct {
                    constructor: *constructor,
                    fields,
                },
            ))
        }
        Value::Closure(closure) => reify_closure(module, closure, budget, out, scope),
        Value::Unit
        | Value::Bool(_)
        | Value::Nat(_)
        | Value::Byte(_)
        | Value::Int(_)
        | Value::Flt(_)
        | Value::Handle(_)
        | Value::Bin(..) => unreachable!("leaf values are reified through `as_constant`"),
    }
}

/// Materialize a closure: reify each captured value to an atom (nesting captured closures), then deep-copy the closure's function with those atoms substituted for its free values, introduced by a `Functions` statement. A free value the captures do not cover is a top-level identity kept verbatim.
fn reify_closure(
    module: &mut Module,
    closure: &Rc<Closure>,
    budget: &mut ReifyBudget,
    out: &mut Vec<StatementId>,
    scope: &mut ReifyScope,
) -> Result<Atom, Bail> {
    // The copy keeps outward function references verbatim; every one must be item-bound to stay in scope at an arbitrary splice site — a reference to a *locally* bound function outside the copied region declines.
    if !scope.outward_ok(module, closure.function) {
        return Err(Bail::Unsupported);
    }
    // A closure capturing nothing has an empty substitution, so the "specialized" copy would be byte-identical to a function the module already holds. Name the original instead -- provided it is item-bound, so it is in scope wherever this reification is spliced. Measured on a combinator-heavy prelude: 4,850 of 10,024 closure reifications in one round took this path.
    let captures = closure.env.borrow().clone();
    if captures.is_empty() && scope.is_item_bound(module, closure.function) {
        return Ok(Atom::Function(closure.function));
    }
    budget.bulk(
        scope
            .weigh(module, closure.function)
            .ok_or(Bail::Unsupported)?,
    )?;
    let mut substitution = BTreeMap::new();
    for (value, held) in &captures {
        substitution.insert(*value, reify(module, held, budget, out, scope)?);
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
    scope: &mut ReifyScope,
) -> Result<Vec<Atom>, Bail> {
    let mut atoms = Vec::with_capacity(values.len());
    for value in values {
        atoms.push(reify(module, value, budget, out, scope)?);
    }
    Ok(atoms)
}

/// Whether every function the region rooted at `root` references outside itself is bound by a top-level item.
/// What a reification pass may compute once and reuse for every replacement in it.
///
/// Both facts are stable while a pass reifies -- it only appends, and the item list is rebuilt afterwards -- and both were previously recomputed per closure: the item-bound set walked every item, and the copy weight walked the whole region, each on the probe *and* again on the real run. On a combinator-heavy module that is thousands of full walks per round.
pub(super) struct ReifyScope {
    item_bound: Option<BTreeSet<FunctionId>>,
    weights: BTreeMap<FunctionId, Option<usize>>,
    outward: BTreeMap<FunctionId, bool>,
}

impl ReifyScope {
    /// Every fact here is computed on first use, not on construction. A caller that reifies no closure -- a literal constructor spine, typically -- must not pay for the item walk at all, which is what an eager scope charged it.
    pub(super) fn new() -> Self {
        Self {
            item_bound: None,
            weights: BTreeMap::new(),
            outward: BTreeMap::new(),
        }
    }

    fn item_bound(&mut self, module: &Module) -> &BTreeSet<FunctionId> {
        self.item_bound
            .get_or_insert_with(|| item_bound_functions(module))
    }

    pub(super) fn is_item_bound(&mut self, module: &Module, function: FunctionId) -> bool {
        self.item_bound(module).contains(&function)
    }

    /// Whether every function reachable from `function`'s region is in scope at an arbitrary splice site.
    ///
    /// Walks the whole region, and ran on the probe *and* again on the real run for every closure -- the last un-hoisted region walk in this path. Stable for a pass on the same argument as the others: reification only appends, and statements are rewritten in the tail.
    pub(super) fn outward_ok(&mut self, module: &Module, function: FunctionId) -> bool {
        if let Some(&known) = self.outward.get(&function) {
            return known;
        }
        let item_bound = self
            .item_bound
            .get_or_insert_with(|| item_bound_functions(module));
        let ok = outward_functions_item_bound(module, function, item_bound);
        self.outward.insert(function, ok);
        ok
    }

    /// What deep-copying `function` would materialize, computed once per pass.
    pub(super) fn weigh(&mut self, module: &Module, function: FunctionId) -> Option<usize> {
        if let Some(&weight) = self.weights.get(&function) {
            return weight;
        }
        let weight = copy_weight(module, function);
        self.weights.insert(function, weight);
        weight
    }
}

/// Every function bound at item level.
///
/// Computed once for a reification pass and handed to each check, because the item list does not change while a pass reifies: rebuilding it per closure walked the whole item list ten thousand times a round on a combinator-heavy module, which is a second cost with the same shape as the copying itself.
pub(super) fn item_bound_functions(module: &Module) -> BTreeSet<FunctionId> {
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
    item_bound
}

fn outward_functions_item_bound(
    module: &Module,
    root: FunctionId,
    item_bound: &BTreeSet<FunctionId>,
) -> bool {
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
