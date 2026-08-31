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
        Atom, BlockId, Constant, FunctionId, Module, Rhs, SequenceOp, Statement, StatementId,
        ValueId, walk::control_blocks,
    },
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
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
            if scope.escapes_knot(module, closure.function) {
                return Err(Bail::Unsupported);
            }
            // Captures first, then the copy, in `reify_closure`'s order. The real run pays a subset of these charges, so charging the same amounts *in the same sequence* is what makes "the probe fits" imply "the real run fits". The order is the real run's, not this one's: a memo key cannot be formed until the captures have atoms.
            for (_, held) in closure.env.borrow().iter() {
                reify_check(module, held, budget, scope)?;
            }
            budget.bulk(
                scope
                    .weigh(module, closure.function)
                    .ok_or(Bail::Unsupported)?,
            )?;
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
///
/// The copy is memoized on the substitution it would apply, because the substitution *is* the copy: two closures over one function with equal captures deep-copy to functions differing only in their fresh identities. A combinator grammar reaches the same specialization over and over — one folded TOML document materialized `Parse/fail(\"bare carriage return\")` 462 times, and 51.5% of that module's 8,818 emitted functions were exact twins — so the memo is the difference between copying a parser tree and naming it.
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
    // A knot's own function stays in its initializer — see `ReifyScope::escapes_knot`.
    if scope.escapes_knot(module, closure.function) {
        return Err(Bail::Unsupported);
    }
    let mut substitution = BTreeMap::new();
    for (value, held) in &captures {
        substitution.insert(*value, reify(module, held, budget, out, scope)?);
    }
    let specialization = (
        closure.function,
        substitution
            .iter()
            .map(|(&value, &atom)| (value, atom))
            .collect::<Vec<_>>(),
    );
    if let Some(materialized) = scope.reusable(&specialization) {
        return Ok(materialized);
    }
    budget.bulk(
        scope
            .weigh(module, closure.function)
            .ok_or(Bail::Unsupported)?,
    )?;
    let function = deep_copy_function(module, closure.function, &substitution, None)
        .ok_or(Bail::Unsupported)?;
    out.push(module.add_statement(Statement::Functions {
        functions: vec![function],
    }));
    let materialized = Atom::Function(function);
    scope.record(specialization, materialized);
    Ok(materialized)
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
/// One closure copy's identity: the source function and the captures already reduced to atoms. Equal keys deep-copy to functions that differ only in their fresh identities.
type Specialization = (FunctionId, Vec<(ValueId, Atom)>);

/// What a reification pass may compute once, and what only one replacement in it may reuse.
///
/// The first three facts are stable while a pass reifies -- it only appends, and the item list is rebuilt afterwards -- and all three were previously recomputed per closure: the item-bound set walked every item, and the copy weight walked the whole region, each on the probe *and* again on the real run. On a combinator-heavy module that is thousands of full walks per round.
///
/// The two memos are the exception. Reifying a closure deep-copies its whole region, and the same specialization is reached along two independent axes: within one replacement, because a combinator names a sub-parser twice (`alt(a, a)`), and across replacements, because many definitions name one shared sub-parser. Measured on a generated grammar, the first axis costs `2^depth` copies without [`ReifyScope::local`] and one per level with it; the second costs five functions per referencing definition without [`ReifyScope::shared`]. Only the first is unconditionally safe to reuse, which is what [`ReifyScope::reusable`] arbitrates.
pub(super) struct ReifyScope {
    item_bound: Option<BTreeSet<FunctionId>>,
    weights: BTreeMap<FunctionId, Option<usize>>,
    outward: BTreeMap<FunctionId, bool>,
    local: HashMap<Specialization, Atom>,
    shared: HashMap<Specialization, (Atom, usize)>,
    position: Option<usize>,
    /// Every function bound within a knot member's initializer — in its blocks, or in the body of a function so bound, transitively — mapped to that initializer's root block. See [`ReifyScope::escapes_knot`].
    knot_inits: Option<BTreeMap<FunctionId, BlockId>>,
    /// The blocks an initializer's own region spans, computed once per initializer asked about.
    init_regions: BTreeMap<BlockId, BTreeSet<BlockId>>,
    /// The block the current candidate stands in, or `None` at item level.
    owner: Option<BlockId>,
}

impl ReifyScope {
    /// Every fact here is computed on first use, not on construction. A caller that reifies no closure -- a literal constructor spine, typically -- must not pay for the item walk at all, which is what an eager scope charged it.
    pub(super) fn new() -> Self {
        Self {
            item_bound: None,
            weights: BTreeMap::new(),
            outward: BTreeMap::new(),
            local: HashMap::new(),
            shared: HashMap::new(),
            position: None,
            knot_inits: None,
            init_regions: BTreeMap::new(),
            owner: None,
        }
    }

    /// Drop the previous replacement's copies before reifying the next one's value.
    ///
    /// The memo is scoped to one replacement because a copy is bound where it is spliced: the `Functions` statement goes immediately before *its* candidate, so the function it binds is in scope for that candidate and for nothing that does not follow it. Carrying an atom into the next replacement would name a function bound in a block that need not dominate it. The pass-stable facts above are untouched. `owner` is the block the candidate stands in, which [`ReifyScope::escapes_knot`] reads.
    pub(super) fn begin_replacement(&mut self, position: Option<usize>, owner: Option<BlockId>) {
        self.local.clear();
        self.position = position;
        self.owner = owner;
    }

    /// Whether copying `function` for the current candidate would carry it out of the knot initializer it is bound in.
    ///
    /// A recursive value's dictionary of methods is built by its initializer, so the closures it holds are functions bound *inside* that initializer, and a call that projects one — `Show/show(dict)` — is a closed candidate whose result is such a closure. Materialized as a copy where the candidate stands, the copy carries the same candidate, so the next round folds it again into another copy: the recursive dispatch is unrolled one level per round, linearly for two members that resolve through each other and exponentially once a member also reaches itself, and only the round limit ends it. A copy taken *within* the initializer's own blocks is the fold a parser knot's construction relies on and stays; one taken outside them — at item level, or inside any function's body, the knot's own included — is the unrolling, and is declined so the candidate stays the call it was.
    pub(super) fn escapes_knot(&mut self, module: &Module, function: FunctionId) -> bool {
        let knot_inits = self
            .knot_inits
            .get_or_insert_with(|| knot_bound_functions(module));
        let Some(&init) = knot_inits.get(&function) else {
            return false;
        };
        let Some(owner) = self.owner else {
            return true;
        };
        let region = self
            .init_regions
            .entry(init)
            .or_insert_with(|| control_blocks(module, init).into_iter().collect());
        !region.contains(&owner)
    }

    /// The copy already materialized for this specialization, if one is in scope here.
    ///
    /// A copy made for *this* replacement is always in scope: it is spliced immediately before the candidate that is about to use it. A copy made for an earlier replacement is in scope only when both are item-level and this candidate is not earlier in the item list, because [`Module::verify`] binds an item's functions where the item stands rather than making them ambient — so a reuse from above would name a function bound below it.
    fn reusable(&self, specialization: &Specialization) -> Option<Atom> {
        if let Some(&atom) = self.local.get(specialization) {
            return Some(atom);
        }
        // Not reusable from here when this candidate has no item position, or stands before the splice point.
        let &(atom, defined) = self.shared.get(specialization)?;
        (self.position? >= defined).then_some(atom)
    }

    /// Withdraw everything this replacement contributed to the module-wide memo.
    ///
    /// **For a group that turned out not to be bindable at item level.** The position is chosen before reification, because [`ReifyScope::record`] needs it; whether the group it produces is *closed* at item level can only be read off the group afterwards. A replacement that loses that bet keeps its copies — they are spliced into its own block, exactly as before — but must take back the claim that a later candidate can name them, which is what `local` holds the keys for.
    pub(super) fn withdraw_replacement(&mut self) {
        for specialization in self.local.keys() {
            self.shared.remove(specialization);
        }
    }

    /// Record a copy for reuse: within this replacement always, and module-wide when the candidate is item-level.
    fn record(&mut self, specialization: Specialization, atom: Atom) {
        if let Some(position) = self.position {
            self.shared.insert(specialization.clone(), (atom, position));
        }
        self.local.insert(specialization, atom);
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

/// Every function bound within a knot member's initializer, mapped to that initializer's root block: the functions a `Functions` statement in the initializer's blocks binds, and — since a function bound there may bind further ones in its own body — everything bound within those, transitively.
fn knot_bound_functions(module: &Module) -> BTreeMap<FunctionId, BlockId> {
    let mut bound = BTreeMap::new();
    for &item in module.items() {
        let Some(Statement::Rec { group }) = module.statement(item) else {
            continue;
        };
        let Some(group) = module.rec_group(*group) else {
            continue;
        };
        for member in &group.values {
            let mut roots = vec![member.init];
            while let Some(root) = roots.pop() {
                for block in control_blocks(module, root) {
                    let Some(definition) = module.block(block) else {
                        continue;
                    };
                    for &statement in &definition.statements {
                        let Some(Statement::Functions { functions }) = module.statement(statement)
                        else {
                            continue;
                        };
                        for &function in functions {
                            if bound.insert(function, member.init).is_none()
                                && let Some(function) = module.function(function)
                            {
                                roots.push(function.body);
                            }
                        }
                    }
                }
            }
        }
    }
    bound
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

/// The functions and values a region references from outside itself.
///
/// [`outward_functions_item_bound`] answers the same question about functions alone and is what gates a reification at all; this answers it about both, and is what decides whether an emitted group can be *bound at item level*. The two are separate because a free value is ordinarily covered by the closure's captures and substituted away, so the reification gate has no business refusing one — while a copy that came out still naming a value bound in some block cannot be lifted out of it.
///
/// `None` when the region reaches a tombstoned entity, which the caller treats as unsafe.
pub(super) fn free_references(
    module: &Module,
    root: FunctionId,
) -> Option<(BTreeSet<FunctionId>, BTreeSet<ValueId>)> {
    let mut region = BTreeSet::new();
    let mut bound = BTreeSet::new();
    let mut functions = BTreeSet::new();
    let mut values = BTreeSet::new();
    let mut work = vec![root];

    while let Some(function) = work.pop() {
        if !region.insert(function) {
            continue;
        }
        let definition = module.function(function)?;
        bound.extend(&definition.params);
        for block in control_blocks(module, definition.body) {
            let Some(block) = module.block(block) else {
                continue;
            };
            for &statement in &block.statements {
                match module.statement(statement) {
                    Some(Statement::Let { result, rhs }) => {
                        bound.insert(*result);
                        bound.extend(rhs.binders());
                        for atom in rhs.operands() {
                            match atom {
                                Atom::Function(referenced) => {
                                    functions.insert(referenced);
                                }
                                Atom::Value(referenced) => {
                                    values.insert(referenced);
                                }
                                Atom::Constant(_) => {}
                            }
                        }
                    }
                    Some(Statement::Functions {
                        functions: bound_here,
                    }) => work.extend(bound_here),
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            work.extend(&group.functions);
                            bound.extend(group.values.iter().map(|member| member.value));
                        }
                    }
                    None => {}
                }
            }
            match block.terminator.atom() {
                Some(Atom::Function(referenced)) => {
                    functions.insert(referenced);
                }
                Some(Atom::Value(referenced)) => {
                    values.insert(referenced);
                }
                _ => {}
            }
        }
    }

    functions.retain(|function| !region.contains(function));
    values.retain(|value| !bound.contains(value));
    Some((functions, values))
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
