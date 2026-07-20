//! Interprocedural effect summaries — the fixed point over the oracle's
//! leaves.
//!
//! Where [`Semantics`] reports one operation's node-local behavior, a
//! [`Summary`] composes those leaves over the reference graph and the block
//! structure, giving the total behavior of *evaluating* a function body, a
//! right-hand side, or a statement — including every function it calls, the
//! callback an intrinsic runs, and the sub-blocks its control forms evaluate.
//! This is the fact pruning consumes to decide whether an eager top-level
//! item must be kept for effect even when its result is unused.
//!
//! Composition is conservative: an unknown callee or callback contributes the
//! lattice top, and a function in a recursive component may diverge unless
//! proven otherwise (which this phase does not attempt). Dormancy is
//! structural: constructing a function contributes nothing — its summary is
//! composed only where a call or callback invokes it. Every traversal is
//! iterative and identity-ordered, so a deep region cannot overflow the
//! native stack and the result never depends on hash order.

use {
    super::{
        Analysis, Atom, BlockId, FunctionId, Intrinsic, LocalBehavior, Module, Rhs, Semantics,
        Statement,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A per-function effect summary snapshot over one module state.
#[derive(Debug, Clone, Default)]
pub struct Summary {
    functions: BTreeMap<FunctionId, LocalBehavior>,
}

impl Summary {
    /// Compute the summary to a fixed point over a verified module and its
    /// analysis.
    pub fn analyze(module: &Module, analysis: &Analysis) -> Self {
        // Seed: a recursive component's members may diverge; everything else
        // starts pure. The seed persists because updates join the previous
        // summary in (the lattice only grows).
        let mut current = BTreeMap::<FunctionId, LocalBehavior>::new();
        for id in module.function_ids() {
            let recursive = analysis
                .component_of(id)
                .is_some_and(|component| analysis.is_recursive(component));
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
                let composed = region_behavior(module, vec![function.body], &current);
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

        Self { functions: current }
    }

    /// The total behavior of invoking a function's body.
    pub fn function(&self, id: FunctionId) -> LocalBehavior {
        self.functions.get(&id).copied().unwrap_or_default()
    }

    /// The total behavior of evaluating a right-hand side: its own operation,
    /// its callee or callback, and every sub-block it evaluates.
    pub fn rhs_behavior(&self, module: &Module, rhs: &Rhs) -> LocalBehavior {
        Semantics::local_behavior(rhs)
            .join(call_behavior(rhs, &self.functions))
            .join(region_blocks(module, rhs.sub_blocks(), &self.functions))
    }

    /// The total behavior of executing a statement: a `Let` evaluates its
    /// right-hand side; binding functions performs nothing (dormancy); a
    /// recursive group eagerly evaluates its computed initializers.
    pub fn statement_behavior(&self, module: &Module, statement: &Statement) -> LocalBehavior {
        match statement {
            Statement::Let { rhs, .. } => self.rhs_behavior(module, rhs),
            Statement::Functions { .. } => LocalBehavior::pure(),
            Statement::Rec { group } => {
                let inits = module
                    .rec_group(*group)
                    .map(|group| group.values.iter().map(|member| member.init).collect())
                    .unwrap_or_default();
                region_blocks(module, inits, &self.functions)
            }
        }
    }
}

fn region_blocks(
    module: &Module,
    seeds: Vec<BlockId>,
    summaries: &BTreeMap<FunctionId, LocalBehavior>,
) -> LocalBehavior {
    region_behavior(module, seeds, summaries)
}

/// The joined behavior of every block reachable from `seeds` through control
/// flow and eager initializers — never through a nested function body, whose
/// behavior is composed only at its calls.
fn region_behavior(
    module: &Module,
    seeds: Vec<BlockId>,
    summaries: &BTreeMap<FunctionId, LocalBehavior>,
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
                        .join(call_behavior(rhs, summaries));
                    work.extend(rhs.sub_blocks());
                }
                Some(Statement::Rec { group }) => {
                    if let Some(group) = module.rec_group(*group) {
                        work.extend(group.values.iter().map(|member| member.init));
                    }
                }
                Some(Statement::Functions { .. }) | None => {}
            }
        }
        behavior.observable = behavior
            .observable
            .join(Semantics::terminator(&block.terminator));
    }
    behavior
}

/// What a right-hand side inherits from the function it calls or the callback
/// it runs: a known function contributes its summary; an unknown callee or
/// callback the conservative top.
fn call_behavior(rhs: &Rhs, summaries: &BTreeMap<FunctionId, LocalBehavior>) -> LocalBehavior {
    match rhs {
        Rhs::Apply { callee, .. } => callee_behavior(*callee, summaries),
        Rhs::Intrinsic {
            intrinsic: Intrinsic::LstMap,
            operands,
        } => operands
            .first()
            .map_or_else(LocalBehavior::unknown, |&mapper| {
                callee_behavior(mapper, summaries)
            }),
        _ => LocalBehavior::pure(),
    }
}

fn callee_behavior(atom: Atom, summaries: &BTreeMap<FunctionId, LocalBehavior>) -> LocalBehavior {
    match atom {
        Atom::Function(function) => summaries.get(&function).copied().unwrap_or_default(),
        _ => LocalBehavior::unknown(),
    }
}
