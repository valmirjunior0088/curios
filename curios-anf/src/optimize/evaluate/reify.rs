//! Materializing a runtime value back into arena statements.
//!
//! A leaf value interns as a [`Constant`] and needs no statement; a list, product, or constructor materializes its already-reified fields into a `Let` binding the corresponding construction right-hand side, appended to `out` in dependency order for the caller to splice ahead of the candidate. A closure result materializes as a deep copy of its function with its reified captures wired in, bound by a `Functions` statement — what makes the runtime-args `Fmt` collapse reachable.

#[cfg(test)]
mod tests;

use {
    super::{Bail, Closure, ReifyBudget, Value, deep_copy_function},
    crate::{
        Atom, BlockId, Constant, FunctionId, Module, Rhs, SequenceOp, Statement, StatementId,
        ValueId, control_blocks,
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
        if let Constant::Bytes(value) = &constant {
            budget.payload(value.len())?;
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
            if !scope.free_values_settled(module, closure) {
                return Err(Bail::Unsupported);
            }
            // A closure over nothing needs no specialized copy — see `reify_closure`.
            if closure.env.borrow().is_empty() && scope.is_item_bound(module, closure.function) {
                return Ok(());
            }
            if scope.escapes_knot(module, closure.function)
                || scope.reaches_candidate(module, closure.function)
            {
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
        if let Constant::Bytes(value) = &constant {
            budget.payload(value.len())?;
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
        | Value::Bytes(_) => unreachable!("leaf values are reified through `as_constant`"),
    }
}

/// Materialize a closure: reify each captured value to an atom (nesting captured closures), then deep-copy the closure's function with those atoms substituted for its free values, introduced by a `Functions` statement. A free value the captures do not cover is a top-level identity kept verbatim.
///
/// The copy is memoized on the substitution it would apply, because the substitution *is* the copy: two closures over one function with equal captures deep-copy to functions differing only in their fresh identities. A combinator grammar reaches the same specialization over and over, so the memo is the difference between copying a parser tree and naming it.
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
    // And it keeps an uncovered free *value* verbatim on the same reasoning, which holds only when that value is item-bound — see `ReifyScope::free_values_settled`.
    if !scope.free_values_settled(module, closure) {
        return Err(Bail::Unsupported);
    }
    // A closure capturing nothing has an empty substitution, so the "specialized" copy would be byte-identical to a function the module already holds. Name the original instead -- provided it is item-bound, so it is in scope wherever this reification is spliced. On a combinator-heavy prelude about half of a round's closure reifications take this path.
    let captures = closure.env.borrow().clone();
    if captures.is_empty() && scope.is_item_bound(module, closure.function) {
        return Ok(Atom::Function(closure.function));
    }
    // A knot's own function stays in its initializer, and a loop's continuation stays the call it was — see `ReifyScope::escapes_knot` and `ReifyScope::reaches_candidate`.
    if scope.escapes_knot(module, closure.function)
        || scope.reaches_candidate(module, closure.function)
    {
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

/// What a region names and does not bind: its free functions, then its free values ([`RegionFacts`]).
type Free = (BTreeSet<FunctionId>, BTreeSet<ValueId>);

/// One closure copy's identity: the source function and the captures already reduced to atoms. Equal keys deep-copy to functions that differ only in their fresh identities.
type Specialization = (FunctionId, Vec<(ValueId, Atom)>);

/// What a reification pass may compute once, and what only one replacement in it may reuse.
///
/// The first three facts are stable while a pass reifies -- it only appends, and the item list is rebuilt afterwards -- so each is computed once per pass rather than per closure, where the item-bound set would walk every item and the copy weight the whole region, each on the probe *and* again on the real run: thousands of full walks per round on a combinator-heavy module.
///
/// The two memos are the exception. Reifying a closure deep-copies its whole region, and the same specialization is reached along two independent axes: within one replacement, because a combinator names a sub-parser twice (`alt(a, a)`), and across replacements, because many definitions name one shared sub-parser. Without [`ReifyScope::local`] the first axis costs `2^depth` copies where it costs one per level with it, and without [`ReifyScope::shared`] the second costs a copy per referencing definition. Only the first is unconditionally safe to reuse, which is what [`ReifyScope::reusable`] arbitrates.
pub(super) struct ReifyScope {
    item_bound: Option<BTreeSet<FunctionId>>,
    /// The values top-level items bind, computed on first use like [`ReifyScope::item_bound`]. See [`ReifyScope::free_values_settled`].
    item_values: Option<BTreeSet<ValueId>>,
    regions: Regions,
    local: HashMap<Specialization, Atom>,
    shared: HashMap<Specialization, (Atom, usize)>,
    position: Option<usize>,
    /// Every function bound within a knot member's initializer — in its blocks, or in the body of a function so bound, transitively — mapped to that initializer's root block. See [`ReifyScope::escapes_knot`].
    knot_inits: Option<BTreeMap<FunctionId, BlockId>>,
    /// The blocks an initializer's own region spans, computed once per initializer asked about.
    init_regions: BTreeMap<BlockId, BTreeSet<BlockId>>,
    /// The block the current candidate stands in, or `None` at item level.
    owner: Option<BlockId>,
    /// The function whose call the current candidate folds, where its replacement holds a description. See [`ReifyScope::reaches_candidate`].
    candidate: Option<FunctionId>,
    /// Whether each function's region reaches each candidate's function, computed once per pair a pass asks about.
    reaches: BTreeMap<(FunctionId, FunctionId), bool>,
}

impl ReifyScope {
    /// Every fact here is computed on first use, not on construction. A caller that reifies no closure -- a literal constructor spine, typically -- must not pay for the item walk at all, which an eager scope would charge it.
    pub(super) fn new() -> Self {
        Self {
            item_bound: None,
            item_values: None,
            regions: Regions::default(),
            local: HashMap::new(),
            shared: HashMap::new(),
            position: None,
            knot_inits: None,
            init_regions: BTreeMap::new(),
            owner: None,
            candidate: None,
            reaches: BTreeMap::new(),
        }
    }

    /// Drop the previous replacement's copies before reifying the next one's value.
    ///
    /// The memo is scoped to one replacement because a copy is bound where it is spliced: the `Functions` statement goes immediately before *its* candidate, so the function it binds is in scope for that candidate and for nothing that does not follow it. Carrying an atom into the next replacement would name a function bound in a block that need not dominate it. The pass-stable facts above are untouched. `owner` is the block the candidate stands in, which [`ReifyScope::escapes_knot`] reads, and `candidate` the function whose call it folds where its replacement holds a description, which [`ReifyScope::reaches_candidate`] reads.
    pub(super) fn begin_replacement(
        &mut self,
        position: Option<usize>,
        owner: Option<BlockId>,
        candidate: Option<FunctionId>,
    ) {
        self.local.clear();
        self.position = position;
        self.owner = owner;
        self.candidate = candidate;
    }

    /// Whether copying `function` for the current candidate would copy a call back to the function the candidate calls.
    ///
    /// A description that loops — a write resent until the stream has taken it all, a read retried until bytes arrive — calls its own function again from its bind's continuation, a closure the description captures. Folding a closed call of such a function and copying the description it answers copies that call too, closed, so the next round folds it into another copy: the loop is unrolled a level a round, for as many rounds as the evaluator runs, and the stage after deletes the copies. A copy whose region reaches the candidate's function, directly or through the functions it calls, is therefore declined, and the loop stays the call it was. A copy that never comes back to it still folds: the description a constant `print` answers reaches the write loop without being a call of `print`.
    pub(super) fn reaches_candidate(&mut self, module: &Module, function: FunctionId) -> bool {
        let Some(candidate) = self.candidate else {
            return false;
        };
        if let Some(&known) = self.reaches.get(&(candidate, function)) {
            return known;
        }

        let mut seen = BTreeSet::new();
        let mut work = vec![function];
        let mut reached = false;
        while let Some(next) = work.pop() {
            if !seen.insert(next) {
                continue;
            }
            // A region holding a tombstone says nothing of where it leads, so it is taken to lead back.
            let Some((functions, _)) = self.free(module, next) else {
                reached = true;
                break;
            };
            if functions.contains(&candidate) {
                reached = true;
                break;
            }
            work.extend(functions.iter().copied());
        }

        self.reaches.insert((candidate, function), reached);
        reached
    }

    /// The functions and values `function`'s region names and does not bind, `None` where it reaches a tombstoned entity.
    pub(super) fn free(&mut self, module: &Module, function: FunctionId) -> Option<&Free> {
        self.regions.of(module, function).map(|facts| &facts.free)
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
    /// **For a group that turned out not to be bindable at item level.** The position is chosen before reification, because [`ReifyScope::record`] needs it; whether the group it produces is *closed* at item level can only be read off the group afterwards. A replacement that loses that bet keeps its copies — they are spliced into its own block — but must take back the claim that a later candidate can name them, which is what `local` holds the keys for.
    pub(super) fn withdraw_replacement(&mut self) {
        self.shared
            .retain(|specialization, _| !self.local.contains_key(specialization));
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

    /// Whether every value the copy would still name after substitution is one it can name from anywhere.
    ///
    /// **The assumption [`reify_closure`] states, checked.** A free value the captures do not cover is kept verbatim on the reasoning that it is a top-level identity — and when it is not, the copy leaves the block that bound it still naming it. The source module verifies, so such a value is in scope *there*; the copy is spliced at the group's splice point and may be reused from any later candidate, and neither has to be inside that block. `outward_ok` asks this question about functions and deliberately not about values, because a free value is ordinarily substituted away; this asks it about the ones that are not.
    pub(super) fn free_values_settled(&mut self, module: &Module, closure: &Closure) -> bool {
        let Some((_, free)) = self.free(module, closure.function) else {
            return false;
        };
        if free.is_empty() {
            return true;
        }
        let free = free.clone();
        let covered: BTreeSet<ValueId> = closure.env.borrow().iter().map(|(id, _)| *id).collect();
        let item_values = self
            .item_values
            .get_or_insert_with(|| item_bound_values(module));
        free.iter()
            .all(|value| covered.contains(value) || item_values.contains(value))
    }

    /// Whether every function `function`'s region names outside itself is bound by a top-level item, and so in scope at an arbitrary splice site.
    pub(super) fn outward_ok(&mut self, module: &Module, function: FunctionId) -> bool {
        let item_bound = self
            .item_bound
            .get_or_insert_with(|| item_bound_functions(module));
        self.regions
            .of(module, function)
            .is_some_and(|facts| facts.free.0.iter().all(|named| item_bound.contains(named)))
    }

    /// What deep-copying `function` would materialize.
    pub(super) fn weigh(&mut self, module: &Module, function: FunctionId) -> Option<usize> {
        self.regions.of(module, function).map(|facts| facts.weight)
    }
}

/// What a function's region — the function and every function bound in its blocks, transitively — names and does not bind, and what copying it materializes.
struct RegionFacts {
    free: Free,
    /// What [`deep_copy_function`] adds for the region, in arena entries: its functions, blocks, statements, values and recursive groups.
    ///
    /// A closure reifies by copying its whole function region, so charging one node for it would let a single replacement add a parser body's worth of statements for the price of one. The caps only bound what they can see, and this is what makes a closure cost what it actually builds.
    weight: usize,
}

/// Each region's facts, computed once per pass from the facts of the regions bound in it.
///
/// **A region is a tree.** A function is bound once and a value defined once ([`Module::verify`]), and a region's code names only what is in its lexical scope, so what a region names from outside is what its function's own blocks name and what the regions bound there name, less the function, the functions its blocks bind and the values they define. Walked whole for each function instead, the regions of a nesting — an `Io` block's continuations, each bound inside the one before — cost the square of its depth.
///
/// Stable for a pass: reification only appends, and statements are rewritten after it. A region reaching a tombstoned entity, or a function bound inside its own region, has no facts, which every reader takes as unsafe.
#[derive(Default)]
struct Regions(BTreeMap<FunctionId, Option<RegionFacts>>);

impl Regions {
    fn of(&mut self, module: &Module, function: FunctionId) -> Option<&RegionFacts> {
        // Post-order without native recursion: a nesting is as deep as the program wrote it.
        let mut work = vec![(function, None)];
        let mut entered = BTreeSet::new();
        while let Some((next, share)) = work.pop() {
            match share {
                None => {
                    if self.0.contains_key(&next) || !entered.insert(next) {
                        continue;
                    }
                    let Some(share) = Share::of(module, next) else {
                        self.0.insert(next, None);
                        continue;
                    };
                    let pending = share
                        .bound
                        .iter()
                        .filter(|child| !self.0.contains_key(child) && !entered.contains(child))
                        .map(|&child| (child, None))
                        .collect::<Vec<_>>();
                    work.push((next, Some(share)));
                    work.extend(pending);
                }
                Some(share) => {
                    let facts = share.facts(next, &self.0);
                    self.0.insert(next, facts);
                }
            }
        }
        self.0.get(&function)?.as_ref()
    }
}

/// What one function's own blocks contribute to its region, before the regions bound in them.
struct Share {
    /// The functions those blocks bind, whose regions the function's region holds.
    bound: Vec<FunctionId>,
    named: Free,
    /// The function's parameters and the values its blocks define.
    defined: BTreeSet<ValueId>,
    weight: usize,
}

impl Share {
    /// `None` when `function` is a tombstone.
    fn of(module: &Module, function: FunctionId) -> Option<Share> {
        let definition = module.function(function)?;
        let mut share = Share {
            bound: Vec::new(),
            named: Free::default(),
            defined: definition.params.iter().copied().collect(),
            weight: 1 + definition.params.len(),
        };
        let mut groups = BTreeSet::new();
        for block in control_blocks(module, definition.body) {
            share.weight += 1;
            let Some(block) = module.block(block) else {
                continue;
            };
            share.weight += block.statements.len();
            for &statement in &block.statements {
                match module.statement(statement) {
                    Some(Statement::Let { result, rhs }) => {
                        let binders = rhs.binders();
                        share.weight += 1 + binders.len();
                        share.defined.insert(*result);
                        share.defined.extend(binders);
                        share.name(rhs.operands());
                    }
                    Some(Statement::Functions { functions }) => share.bound.extend(functions),
                    Some(Statement::Rec { group }) => {
                        groups.insert(*group);
                        if let Some(group) = module.rec_group(*group) {
                            share.weight += group.values.len();
                            share.bound.extend(&group.functions);
                            share
                                .defined
                                .extend(group.values.iter().map(|member| member.value));
                        }
                    }
                    None => {}
                }
            }
            share.name(block.terminator.atoms());
        }
        share.weight += groups.len();
        Some(share)
    }

    fn name(&mut self, atoms: Vec<Atom>) {
        for atom in atoms {
            match atom {
                Atom::Function(named) => {
                    self.named.0.insert(named);
                }
                Atom::Value(named) => {
                    self.named.1.insert(named);
                }
                Atom::Constant(_) => {}
            }
        }
    }

    /// `function`'s region facts, from this share and the facts of every region bound in it: `None` where one of those has none, or has not been computed because it encloses `function`.
    fn facts(
        self,
        function: FunctionId,
        regions: &BTreeMap<FunctionId, Option<RegionFacts>>,
    ) -> Option<RegionFacts> {
        let Share {
            bound,
            named: (mut functions, mut values),
            defined,
            mut weight,
        } = self;
        for child in &bound {
            let facts = regions.get(child)?.as_ref()?;
            functions.extend(&facts.free.0);
            values.extend(&facts.free.1);
            weight += facts.weight;
        }
        functions.remove(&function);
        for child in &bound {
            functions.remove(child);
        }
        values.retain(|value| !defined.contains(value));
        Some(RegionFacts {
            free: (functions, values),
            weight,
        })
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

/// Every value bound at item level: an item's result and the binders of its right-hand side, and a recursive group's computed members.
///
/// Computed once for a reification pass and handed to each check, because the item list does not change while a pass reifies: rebuilding it per closure would walk the whole item list ten thousand times a round on a combinator-heavy module, a second cost with the same shape as the copying itself.
pub(super) fn item_bound_values(module: &Module) -> BTreeSet<ValueId> {
    let mut bound = BTreeSet::<ValueId>::new();
    for &item in module.items() {
        match module.statement(item) {
            Some(Statement::Let { result, rhs }) => {
                bound.insert(*result);
                bound.extend(rhs.binders());
            }
            Some(Statement::Rec { group }) => {
                if let Some(group) = module.rec_group(*group) {
                    bound.extend(group.values.iter().map(|member| member.value));
                }
            }
            _ => {}
        }
    }
    bound
}

/// The functions top-level items bind.
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

fn emit(module: &mut Module, out: &mut Vec<StatementId>, rhs: Rhs) -> Atom {
    let result = module.add_value(None);
    out.push(module.add_statement(Statement::Let { result, rhs }));
    Atom::Value(result)
}
