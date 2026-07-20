//! Specialization of self-recursive functions on literal constructor spines.
//!
//! The closed-term evaluator leaves one shape it cannot finish: a
//! self-recursive function applied to a **constant** inductive value with
//! *runtime* companion arguments — `Fmt/go_with` over a parsed format-string
//! spine, with a runtime `finish` and accumulator. Cont never runs closed
//! recursion to a constant, so this residual would ship the whole combinator
//! web unless the arena unrolls it.
//!
//! This pass mints one specialized function per `(target, position, spine)`:
//! a verbatim deep copy with the baked parameter dropped and instead bound
//! locally to the reified spine — *binding, not substitution* — then folded:
//! a match on the now-known spine takes its arm, a known projection reads its
//! field, and a self-recursive call whose spine argument folds to a strictly
//! smaller literal is rewritten to the next specialization. The chain is
//! finite because every mint keys on a strict subterm of its requester's
//! spine; the budgets are belt and braces. A recursive call whose spine does
//! not fold falls back to the generic target — always correct, never
//! required to fire.

use {
    super::{budget::ReifyBudget, copy::deep_copy_function, reify::reify, value::Value},
    crate::{
        Analysis, Atom, BlockId, Function, FunctionId, Module, Rhs, Statement, StatementId,
        Terminator, ValueId, walk::control_blocks,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt::Write,
        rc::Rc,
    },
};

/// Module-wide cap on minted specializations. A format spine of `k`
/// directives mints about `2k + 1`.
const MAX_SPECIALIZATIONS: usize = 64;

/// A literal larger than this many nodes is not a specialization key.
const MAX_SPINE_NODES: usize = 256;

/// Specialize every eligible literal-spine call site, module-wide.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn specialize_literal_spines(module: &mut Module) {
    let analysis = Analysis::analyze(module);
    let targets = find_targets(module, &analysis);
    if targets.is_empty() {
        return;
    }

    let def_index = definition_index(module);
    let sites = plan_sites(module, &def_index, &targets);
    if sites.is_empty() {
        return;
    }

    let mut minter = Minter::new();
    let mut rewrites = Vec::<(StatementId, ValueId, FunctionId, usize)>::new();
    for site in sites {
        if let Some(spec) = minter.request(module, site.target, site.position, &site.spine) {
            rewrites.push((site.statement, site.result, spec, site.position));
        }
    }

    // Taking a match arm orphans the untaken arms' values; they stay live
    // until the following prune tombstones them, so verification is deferred
    // to the end of the driver.
    apply(module, &minter, rewrites);
}

struct Target {
    params: usize,
}

struct Site {
    statement: StatementId,
    result: ValueId,
    target: FunctionId,
    position: usize,
    spine: Value,
}

/// The specializable targets: directly self-recursive functions bound by a
/// one-member `Functions` item, so the target and everything it names stay in
/// scope where its specializations install. The function may sit in a larger
/// recursive component — the fold only re-specializes its *direct* self
/// calls, and the copy redirects them to the generic original.
fn find_targets(module: &Module, analysis: &Analysis) -> BTreeMap<FunctionId, Target> {
    let mut targets = BTreeMap::new();
    for &item in module.items() {
        let Some(Statement::Functions { functions }) = module.statement(item) else {
            continue;
        };
        let [function] = functions.as_slice() else {
            continue;
        };
        let is_self_recursive = analysis.references(*function).contains(function);
        if is_self_recursive && let Some(definition) = module.function(*function) {
            targets.insert(
                *function,
                Target {
                    params: definition.params.len(),
                },
            );
        }
    }
    targets
}

/// Every value's defining `Let` statement, for structural literal resolution.
fn definition_index(module: &Module) -> BTreeMap<ValueId, StatementId> {
    let mut index = BTreeMap::new();
    for (position, slot) in module.statements().iter().enumerate() {
        if let Some(Statement::Let { result, .. }) = slot {
            index.insert(*result, StatementId(position as u32));
        }
    }
    index
}

fn plan_sites(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    targets: &BTreeMap<FunctionId, Target>,
) -> Vec<Site> {
    let mut sites = Vec::new();
    for (position, slot) in module.statements().iter().enumerate() {
        let Some(Statement::Let {
            result,
            rhs: Rhs::Apply { callee, arguments },
        }) = slot
        else {
            continue;
        };
        let Atom::Function(target) = callee else {
            continue;
        };
        let Some(info) = targets.get(target) else {
            continue;
        };
        if arguments.len() != info.params {
            continue;
        }
        if let Some((argument_position, spine)) = spine_argument(module, def_index, arguments) {
            sites.push(Site {
                statement: StatementId(position as u32),
                result: *result,
                target: *target,
                position: argument_position,
                spine,
            });
        }
    }
    sites
}

/// The first argument that resolves to a constructor-shaped literal spine.
fn spine_argument(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    arguments: &[Atom],
) -> Option<(usize, Value)> {
    arguments.iter().enumerate().find_map(|(position, &atom)| {
        let mut budget = MAX_SPINE_NODES;
        match resolve_literal(module, def_index, atom, &mut budget) {
            Some(spine @ Value::Construct(..)) => Some((position, spine)),
            _ => None,
        }
    })
}

/// The compile-time literal an atom names, following aliases: a constant, or
/// a construction over further literals. Bounded, so a deep or cyclic chain
/// terminates.
fn resolve_literal(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    atom: Atom,
    budget: &mut usize,
) -> Option<Value> {
    if *budget == 0 {
        return None;
    }
    *budget -= 1;
    match atom {
        Atom::Constant(constant) => Some(Value::from_constant(module.constant(constant)?)),
        Atom::Value(value) => {
            let Some(Statement::Let { rhs, .. }) = module.statement(*def_index.get(&value)?) else {
                return None;
            };
            match rhs {
                Rhs::Alias(inner) => resolve_literal(module, def_index, *inner, budget),
                Rhs::Construct {
                    constructor,
                    fields,
                } => Some(Value::Construct(
                    *constructor,
                    Rc::new(resolve_fields(module, def_index, fields, budget)?),
                )),
                Rhs::Product { schema, fields } => Some(Value::Product(
                    *schema,
                    Rc::new(resolve_fields(module, def_index, fields, budget)?),
                )),
                _ => None,
            }
        }
        Atom::Function(_) => None,
    }
}

fn resolve_fields(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    fields: &[Atom],
    budget: &mut usize,
) -> Option<Vec<Value>> {
    fields
        .iter()
        .map(|&field| resolve_literal(module, def_index, field, budget))
        .collect()
}

/// A deterministic structural key for a literal spine.
fn spine_key(value: &Value, out: &mut String) {
    match value {
        Value::Unit => out.push('u'),
        Value::Bool(bit) => {
            let _ = write!(out, "b{bit}");
        }
        Value::Nat(number) => {
            let _ = write!(out, "n{number}");
        }
        Value::Byte(byte) => {
            let _ = write!(out, "x{byte}");
        }
        Value::Int(number) => {
            let _ = write!(out, "i{number}");
        }
        Value::Flt(number) => {
            let _ = write!(out, "f{:08x}", number.to_f32().to_bits());
        }
        Value::Io(token) => {
            let _ = write!(out, "o{token}");
        }
        Value::Bin(grain, packed) => {
            let _ = write!(out, "d{grain:?}:{:?}", packed.to_packed_bytes());
        }
        Value::Lst(elements) => {
            out.push('[');
            for element in elements.iter() {
                spine_key(element, out);
                out.push(',');
            }
            out.push(']');
        }
        Value::Product(schema, fields) => {
            let _ = write!(out, "P{}(", schema.index());
            for field in fields.iter() {
                spine_key(field, out);
                out.push(',');
            }
            out.push(')');
        }
        Value::Construct(constructor, fields) => {
            let _ = write!(out, "K{}(", constructor.index());
            for field in fields.iter() {
                spine_key(field, out);
                out.push(',');
            }
            out.push(')');
        }
        Value::Closure(_) => out.push_str("<closure>"),
    }
}

/// Mints and memoizes specializations, remembering each against its target
/// for installation.
struct Minter {
    memo: BTreeMap<String, FunctionId>,
    minted: BTreeMap<FunctionId, Vec<FunctionId>>,
    budget: usize,
}

impl Minter {
    fn new() -> Self {
        Self {
            memo: BTreeMap::new(),
            minted: BTreeMap::new(),
            budget: MAX_SPECIALIZATIONS,
        }
    }

    /// The specialization of `target` at `position` for `spine`, minted (and
    /// recursively folded) on first use.
    fn request(
        &mut self,
        module: &mut Module,
        target: FunctionId,
        position: usize,
        spine: &Value,
    ) -> Option<FunctionId> {
        let mut key = String::new();
        let _ = write!(key, "{}:{}:", target.index(), position);
        spine_key(spine, &mut key);
        if let Some(&spec) = self.memo.get(&key) {
            return Some(spec);
        }
        if self.budget == 0 {
            return None;
        }
        self.budget -= 1;

        // Materialize the spine locally, deep-copy the target, drop the
        // baked parameter and bind it to the spine ahead of the copied body.
        // Dry-run first so a declined mint strands nothing.
        {
            let mut probe = ReifyBudget::new();
            super::reify::reify_check(module, spine, &mut probe).ok()?;
        }
        let mut prelude = Vec::new();
        let mut reify_budget = ReifyBudget::new();
        let spine_atom = reify(module, spine, &mut reify_budget, &mut prelude).ok()?;

        let spec = deep_copy_function(module, target, &BTreeMap::new(), Some(target))?;
        let definition = module.function(spec)?.clone();
        let baked = *definition.params.get(position)?;
        let mut params = definition.params.clone();
        params.remove(position);
        prelude.push(module.add_statement(Statement::Let {
            result: baked,
            rhs: Rhs::Alias(spine_atom),
        }));
        prelude.extend(module.block(definition.body)?.statements.iter().copied());
        module.set_block_statements(definition.body, prelude);
        module.set_function(
            spec,
            Function {
                debug_name: definition.debug_name,
                params,
                body: definition.body,
            },
        );

        self.memo.insert(key, spec);
        self.minted.entry(target).or_default().push(spec);

        self.fold_known(module, spec, target);
        Some(spec)
    }

    /// Fold what the baked spine decides in a minted body: take known match
    /// arms, read known projections, and re-specialize self-recursive calls
    /// over strictly smaller spines. Iterates to a fixed point.
    fn fold_known(&mut self, module: &mut Module, spec: FunctionId, target: FunctionId) {
        loop {
            let def_index = definition_index(module);
            let mut progressed = false;
            for block in region_blocks(module, spec) {
                if self.reduce_block(module, &def_index, target, block) {
                    progressed = true;
                    break;
                }
            }
            if !progressed {
                break;
            }
        }
    }

    /// Apply the first available reduction in `block`.
    fn reduce_block(
        &mut self,
        module: &mut Module,
        def_index: &BTreeMap<ValueId, StatementId>,
        target: FunctionId,
        block: BlockId,
    ) -> bool {
        let Some(definition) = module.block(block) else {
            return false;
        };
        let statements = definition.statements.clone();
        for (index, &statement) in statements.iter().enumerate() {
            let Some(Statement::Let { result, rhs }) = module.statement(statement) else {
                continue;
            };
            let result = *result;
            match rhs {
                Rhs::MatchVariant {
                    scrutinee,
                    arms,
                    default,
                    ..
                } => {
                    let mut budget = MAX_SPINE_NODES;
                    let Some((constructor, payload)) =
                        resolve_construct(module, def_index, *scrutinee, &mut budget)
                    else {
                        continue;
                    };
                    let (bindings, arm) =
                        match arms.iter().find(|arm| arm.constructor == constructor) {
                            Some(arm) => (arm.bindings.clone(), arm.block),
                            None => match default {
                                Some(block) => (Vec::new(), *block),
                                None => continue,
                            },
                        };
                    if bindings.len() == payload.len()
                        && take_block(module, block, index, result, &bindings, &payload, arm)
                    {
                        return true;
                    }
                }
                Rhs::Project { product, field, .. } => {
                    let mut budget = MAX_SPINE_NODES;
                    if let Some(fields) = resolve_product(module, def_index, *product, &mut budget)
                        && let Some(atom) = fields.get(*field as usize).copied()
                    {
                        module.set_statement(
                            statement,
                            Statement::Let {
                                result,
                                rhs: Rhs::Alias(atom),
                            },
                        );
                        return true;
                    }
                }
                Rhs::Apply {
                    callee: Atom::Function(callee),
                    arguments,
                } if *callee == target => {
                    let arguments = arguments.clone();
                    if let Some((position, spine)) = spine_argument(module, def_index, &arguments)
                        && let Some(spec) = self.request(module, target, position, &spine)
                    {
                        let mut rewritten = arguments;
                        rewritten.remove(position);
                        module.set_statement(
                            statement,
                            Statement::Let {
                                result,
                                rhs: Rhs::Apply {
                                    callee: Atom::Function(spec),
                                    arguments: rewritten,
                                },
                            },
                        );
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }
}

fn apply(
    module: &mut Module,
    minter: &Minter,
    rewrites: Vec<(StatementId, ValueId, FunctionId, usize)>,
) {
    for (statement, result, spec, position) in rewrites {
        let Some(Statement::Let {
            rhs: Rhs::Apply { arguments, .. },
            ..
        }) = module.statement(statement)
        else {
            continue;
        };
        let mut arguments = arguments.clone();
        if position >= arguments.len() {
            continue;
        }
        arguments.remove(position);
        module.set_statement(
            statement,
            Statement::Let {
                result,
                rhs: Rhs::Apply {
                    callee: Atom::Function(spec),
                    arguments,
                },
            },
        );
    }

    // Insert each target's specialization group right after its binding item.
    let installed = &minter.minted;
    let items = module.items().to_vec();
    let mut rebuilt = Vec::with_capacity(items.len() + installed.len());
    for item in items {
        rebuilt.push(item);
        if let Some(target) = binds_target(module, item, installed)
            && let Some(functions) = installed.get(&target)
        {
            rebuilt.push(module.add_statement(Statement::Functions {
                functions: functions.clone(),
            }));
        }
    }
    module.set_items(rebuilt);
}

/// The target a one-member `Functions` item binds, when it has minted
/// specializations.
fn binds_target(
    module: &Module,
    statement: StatementId,
    installed: &BTreeMap<FunctionId, Vec<FunctionId>>,
) -> Option<FunctionId> {
    match module.statement(statement) {
        Some(Statement::Functions { functions }) => match functions.as_slice() {
            [function] if installed.contains_key(function) => Some(*function),
            _ => None,
        },
        _ => None,
    }
}

fn resolve_construct(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    atom: Atom,
    budget: &mut usize,
) -> Option<(crate::ConstructorId, Vec<Atom>)> {
    let mut current = atom;
    loop {
        if *budget == 0 {
            return None;
        }
        *budget -= 1;
        let Atom::Value(value) = current else {
            return None;
        };
        match module.statement(*def_index.get(&value)?) {
            Some(Statement::Let {
                rhs:
                    Rhs::Construct {
                        constructor,
                        fields,
                    },
                ..
            }) => return Some((*constructor, fields.clone())),
            Some(Statement::Let {
                rhs: Rhs::Alias(inner),
                ..
            }) => current = *inner,
            _ => return None,
        }
    }
}

fn resolve_product(
    module: &Module,
    def_index: &BTreeMap<ValueId, StatementId>,
    atom: Atom,
    budget: &mut usize,
) -> Option<Vec<Atom>> {
    let mut current = atom;
    loop {
        if *budget == 0 {
            return None;
        }
        *budget -= 1;
        let Atom::Value(value) = current else {
            return None;
        };
        match module.statement(*def_index.get(&value)?) {
            Some(Statement::Let {
                rhs: Rhs::Product { fields, .. },
                ..
            }) => return Some(fields.clone()),
            Some(Statement::Let {
                rhs: Rhs::Alias(inner),
                ..
            }) => current = *inner,
            _ => return None,
        }
    }
}

/// Inline a decided branch block in place of the eliminating statement: bind
/// the payload to the arm's binders, splice the arm's statements, and alias
/// the result to the arm's returned atom. Declines an arm that exits or
/// traps, leaving the eliminator for Cont.
fn take_block(
    module: &mut Module,
    block: BlockId,
    index: usize,
    result: ValueId,
    bindings: &[ValueId],
    payload: &[Atom],
    arm: BlockId,
) -> bool {
    let Some(arm_definition) = module.block(arm) else {
        return false;
    };
    let Terminator::Return(returned) = arm_definition.terminator else {
        return false;
    };
    let arm_statements = arm_definition.statements.clone();

    let mut inlined = Vec::new();
    for (&binder, &atom) in bindings.iter().zip(payload) {
        inlined.push(module.add_statement(Statement::Let {
            result: binder,
            rhs: Rhs::Alias(atom),
        }));
    }
    inlined.extend(arm_statements);
    inlined.push(module.add_statement(Statement::Let {
        result,
        rhs: Rhs::Alias(returned),
    }));

    let Some(definition) = module.block(block) else {
        return false;
    };
    let mut rebuilt = Vec::with_capacity(definition.statements.len() + inlined.len());
    for (position, &statement) in definition.statements.iter().enumerate() {
        if position == index {
            rebuilt.extend(inlined.iter().copied());
        } else {
            rebuilt.push(statement);
        }
    }
    module.set_block_statements(block, rebuilt);
    true
}

/// Every block a function region owns, descending through nested functions —
/// unlike `control_blocks`, which stops at a nested function body.
fn region_blocks(module: &Module, root: FunctionId) -> Vec<BlockId> {
    let mut work = vec![root];
    let mut seen_functions = BTreeSet::new();
    let mut seen_blocks = BTreeSet::new();
    let mut blocks = Vec::new();
    while let Some(function) = work.pop() {
        if !seen_functions.insert(function) {
            continue;
        }
        let Some(definition) = module.function(function) else {
            continue;
        };
        for block in control_blocks(module, definition.body) {
            if !seen_blocks.insert(block) {
                continue;
            }
            blocks.push(block);
            let Some(definition) = module.block(block) else {
                continue;
            };
            for &statement in &definition.statements {
                match module.statement(statement) {
                    Some(Statement::Functions { functions }) => work.extend(functions),
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            work.extend(&group.functions);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    blocks
}
