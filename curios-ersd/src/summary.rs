//! Interprocedural effect summaries — the fixed point over the oracle's leaves.
//!
//! Where [`Semantics`] reports one operation's node-local behavior, a [`Summary`] composes those leaves over the reference graph and the block structure, giving the total behavior of *evaluating* a function body, a right-hand side, or a statement — including every function it calls, the callback an intrinsic runs, and the sub-blocks its control forms evaluate. This is the fact pruning consumes to decide whether an eager top-level item must be kept for effect even when its result is unused.
//!
//! Composition is conservative: an unknown callee or callback contributes the lattice top, and a function in a recursive component may diverge unless its definition was proved total above Core, which [`Function::total`](super::Function::total) carries down — this phase proves no termination of its own and re-derives none. Dormancy is structural: constructing a function contributes nothing — its summary is composed only where a call or callback invokes it. Every traversal is iterative and identity-ordered, so a deep region cannot overflow the native stack and the result never depends on hash order.

use {
    super::{
        Analysis, Atom, BlockId, FunctionId, Intrinsic, LocalBehavior, Module, ProductId, Rhs,
        Semantics, Statement, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A per-function effect summary snapshot over one module state.
#[derive(Debug, Clone, Default)]
pub struct Summary {
    functions: BTreeMap<FunctionId, LocalBehavior>,
    /// The computation-free rebindings between a callee spelled as a value and the function it names, so a call through one is judged as that function rather than as an unknown — see [`resolve_callee`].
    rebindings: Rebindings,
}

/// What stands between a value and the atom it is, without computing anything: an [`Rhs::Alias`], and an [`Rhs::Project`] of a product the module constructs, whose field the [`Rhs::Product`] already holds.
#[derive(Debug, Clone, Default)]
struct Rebindings {
    aliases: BTreeMap<ValueId, Atom>,
    products: BTreeMap<ValueId, (ProductId, Vec<Atom>)>,
    projections: BTreeMap<ValueId, (ProductId, Atom, u32)>,
}

impl Summary {
    /// Compute the summary to a fixed point over a verified module and its analysis.
    pub fn analyze(module: &Module, analysis: &Analysis) -> Self {
        let rebindings = rebindings(module);
        // Seed: a recursive component's members may diverge *unless the definition they erased from was proved total*; everything else starts pure. The seed persists because updates join the previous summary in (the lattice only grows).
        let mut current = BTreeMap::<FunctionId, LocalBehavior>::new();
        for id in module.function_ids() {
            // Recursion is why this stage cannot tell termination on its own, and the verdict is why it does not have to: it is the certifier's, decided above Core and carried down (see `Function::total`). Reading it here is what keeps a total recursive definition from being called divergent by everything that consults a summary.
            let recursive = analysis
                .component_of(id)
                .is_some_and(|component| analysis.is_recursive(component))
                && !module.function(id).is_some_and(|function| function.total);
            let seed = if recursive {
                LocalBehavior {
                    observable: super::ObservableBehavior::none().with_divergence(),
                    ..LocalBehavior::pure()
                }
            } else {
                LocalBehavior::pure()
            };
            current.insert(id, seed);
        }

        // Monotone fixed point over a finite lattice.
        loop {
            let mut changed = false;
            let mut next = current.clone();
            for id in module.function_ids() {
                let function = module.function(id).expect("live function");
                let composed = region_behavior(module, vec![function.body], &current, &rebindings);
                let updated = current[&id].join(composed);
                if updated != current[&id] {
                    changed = true;
                    next.insert(id, updated);
                }
            }
            current = next;
            if !changed {
                break;
            }
        }

        Self {
            functions: current,
            rebindings,
        }
    }

    /// The total behavior of evaluating a right-hand side: its own operation, its callee or callback, and every sub-block it evaluates.
    pub fn rhs_behavior(&self, module: &Module, rhs: &Rhs) -> LocalBehavior {
        Semantics::local_behavior(rhs)
            .join(call_behavior(rhs, &self.functions, &self.rebindings))
            .join(region_behavior(
                module,
                rhs.sub_blocks(),
                &self.functions,
                &self.rebindings,
            ))
    }

    /// The total behavior of executing a statement: a `Let` evaluates its right-hand side; binding functions performs nothing (dormancy), and so does binding a recursive group, whose computed members are forced by need — an initializer runs when something reads its member, and the verifier holds it to performing no effect, so what it can contribute then is a trap or divergence the language does not owe a program that never forces it.
    pub fn statement_behavior(&self, module: &Module, statement: &Statement) -> LocalBehavior {
        match statement {
            Statement::Let { rhs, .. } => self.rhs_behavior(module, rhs),
            Statement::Functions { .. } | Statement::Rec { .. } => LocalBehavior::pure(),
        }
    }
}

/// The joined behavior of every block reachable from `seeds` through control flow — never through a nested function body, whose behavior is composed only at its calls, nor a nested group's initializers, which are forced by need.
fn region_behavior(
    module: &Module,
    seeds: Vec<BlockId>,
    summaries: &BTreeMap<FunctionId, LocalBehavior>,
    rebindings: &Rebindings,
) -> LocalBehavior {
    let mut behavior = LocalBehavior::pure();
    let mut seen = BTreeSet::new();
    let mut work = seeds;
    while let Some(id) = work.pop() {
        if !seen.insert(id) {
            continue;
        }
        let Some(block) = module.block(id) else {
            continue;
        };
        for &statement in &block.statements {
            match module.statement(statement) {
                Some(Statement::Let { rhs, .. }) => {
                    behavior = behavior
                        .join(Semantics::local_behavior(rhs))
                        .join(call_behavior(rhs, summaries, rebindings));
                    work.extend(rhs.sub_blocks());
                }
                Some(Statement::Rec { .. } | Statement::Functions { .. }) | None => {}
            }
        }
        behavior.observable = behavior
            .observable
            .join(Semantics::terminator(&block.terminator));
    }
    behavior
}

/// What a right-hand side inherits from the function it calls or the callback it runs: a known function contributes its summary; an unknown callee or callback the conservative top.
fn call_behavior(
    rhs: &Rhs,
    summaries: &BTreeMap<FunctionId, LocalBehavior>,
    rebindings: &Rebindings,
) -> LocalBehavior {
    match rhs {
        Rhs::Apply { callee, .. } => callee_behavior(*callee, summaries, rebindings),
        Rhs::Intrinsic {
            intrinsic: Intrinsic::ListMap,
            operands,
        } => operands
            .get(1)
            .map_or_else(LocalBehavior::unknown, |&mapper| {
                callee_behavior(mapper, summaries, rebindings)
            }),
        _ => LocalBehavior::pure(),
    }
}

fn callee_behavior(
    atom: Atom,
    summaries: &BTreeMap<FunctionId, LocalBehavior>,
    rebindings: &Rebindings,
) -> LocalBehavior {
    match resolve_callee(atom, rebindings) {
        // The seed covers every live function and a verified module references only live functions, so a miss is a broken snapshot discipline, never a program property.
        Atom::Function(function) => summaries
            .get(&function)
            .copied()
            .expect("a live callee has a summary"),
        _ => LocalBehavior::unknown(),
    }
}

/// Follow a callee atom through the [`Rebindings`] that stand between it and the function it names.
///
/// **An alias is computation-free, so a call through one is a call to what it rebinds** — and reading it as an unknown callee costs the whole conservative top. That is not a missed refinement but a retention bug with a price: a top-level `apply` whose callee is an alias — `/std/Json/decode/decode`'s, of `/std/Parse/bind` — would be judged observable and kept, and through it the recursive parser group and the entire `Json`/`Parse` web, in *every* program, `/std/print("hi")` included.
///
/// **So is a projection of a product the module constructs**: the field it reads is an atom the construction already holds, so a call through `dict.bind` is a call to whatever `dict`'s construction put there. It is the shape a concept method's call erases to — the method projected off the witness, applied — and so the other way the same web would be kept. A projection whose product is not one the walk can see constructed resolves to nothing, and its callee stays unknown.
///
/// The walk is bounded by the maps, not by trust: a verified module's rebindings form a dag, and the visited set is what makes that an assumption this function does not have to make. It is iterative, as every traversal here is: the projections still to apply wait on a stack, innermost last, for the product the walk reaches next.
fn resolve_callee(atom: Atom, rebindings: &Rebindings) -> Atom {
    let mut current = atom;
    let mut pending = Vec::<(ProductId, u32)>::new();
    let mut seen = BTreeSet::new();
    while let Atom::Value(value) = current {
        if !seen.insert(value) {
            break;
        }
        if let Some(&next) = rebindings.aliases.get(&value) {
            current = next;
        } else if let Some(&(schema, product, field)) = rebindings.projections.get(&value) {
            pending.push((schema, field));
            current = product;
        } else if let Some(&(schema, field)) = pending.last()
            && let Some((built, fields)) = rebindings.products.get(&value)
            && *built == schema
            && let Some(&next) = fields.get(field as usize)
        {
            pending.pop();
            current = next;
        } else {
            break;
        }
    }
    match pending.is_empty() {
        true => current,
        false => atom,
    }
}

/// Every [`Rebindings`] entry, module-wide. Scanned from the statement arena rather than walked per region, because a callee's alias or dictionary is frequently bound at top level while the call sits inside a nested body.
fn rebindings(module: &Module) -> Rebindings {
    let mut rebindings = Rebindings::default();
    for statement in module.statements().iter().flatten() {
        let Statement::Let { result, rhs } = statement else {
            continue;
        };
        match rhs {
            Rhs::Alias(atom) => {
                rebindings.aliases.insert(*result, *atom);
            }
            Rhs::Product { schema, fields } => {
                rebindings
                    .products
                    .insert(*result, (*schema, fields.clone()));
            }
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                rebindings
                    .projections
                    .insert(*result, (*schema, *product, *field));
            }
            _ => {}
        }
    }
    rebindings
}

#[cfg(test)]
mod tests;
