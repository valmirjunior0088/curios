//! Interprocedural effect summaries — the fixed point over the oracle's leaves.
//!
//! Where [`Semantics`] reports one operation's node-local behavior, a [`Summary`] composes those leaves over the reference graph and the block structure, giving the total behavior of *evaluating* a function body, a right-hand side, or a statement — including every function it calls, the callback an intrinsic runs, and the sub-blocks its control forms evaluate. This is the fact pruning consumes to decide whether an eager top-level item must be kept for effect even when its result is unused.
//!
//! **A function's summary names what it applies of what it was handed.** A function applying a parameter or a value it captures does what that value does, which its body cannot tell; so its summary is what its body does apart from those applications, beside the values it applies, and a call of it composes the summary with what the call hands: the argument where a parameter is applied, and the captured value, in scope at the call, where one is. A fold handed a pure stepper is then pure at its call, though the fold alone applies an unknown.
//!
//! Composition is conservative: an unknown callee or callback contributes the lattice top, and a function in a recursive component may diverge unless its definition was proved total above Core, which [`Function::total`](super::Function::total) carries down — this phase proves no termination of its own and re-derives none. Dormancy is structural: constructing a function contributes nothing — its summary is composed only where a call or callback invokes it. Every traversal is iterative and identity-ordered, so a deep region cannot overflow the native stack and the result never depends on hash order.

use {
    super::{
        Allocation, Analysis, Atom, BlockId, FunctionId, Intrinsic, LocalBehavior, Module,
        ObservableBehavior, Rebindings, Rhs, Semantics, Statement, ValueId,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A per-function effect summary snapshot over one module state.
#[derive(Debug, Clone, Default)]
pub struct Summary {
    functions: BTreeMap<FunctionId, Applying>,
    /// The computation-free rebindings between a callee spelled as a value and the function it names, so a call through one is judged as that function rather than as an unknown — see [`Rebindings::resolve`].
    rebindings: Rebindings,
}

/// What evaluating a function's body does apart from applying what it was handed, and which of its parameters and captured values it applies.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Applying {
    behavior: LocalBehavior,
    applied: BTreeSet<ValueId>,
}

impl Summary {
    /// Compute the summary to a fixed point over a verified module and its analysis.
    pub fn analyze(module: &Module, analysis: &Analysis) -> Self {
        let rebindings = Rebindings::analyze(module);
        // Seed: a recursive component's members may diverge *unless the definition they erased from was proved total*; everything else starts pure. The seed persists because updates join the previous summary in (the lattice only grows).
        let mut current = BTreeMap::<FunctionId, Applying>::new();
        let mut handed = BTreeMap::<FunctionId, BTreeSet<ValueId>>::new();
        for id in module.function_ids() {
            let function = module.function(id).expect("live function");
            // Recursion is why this stage cannot tell termination on its own, and the verdict is why it does not have to: it is the certifier's, decided above Core and carried down (see `Function::total`). Reading it here is what keeps a total recursive definition from being called divergent by everything that consults a summary.
            let recursive = analysis
                .component_of(id)
                .is_some_and(|component| analysis.is_recursive(component))
                && !function.total;
            let seed = if recursive {
                LocalBehavior {
                    observable: ObservableBehavior::none().with_divergence(),
                    ..LocalBehavior::pure()
                }
            } else {
                LocalBehavior::pure()
            };
            current.insert(
                id,
                Applying {
                    behavior: seed,
                    applied: BTreeSet::new(),
                },
            );
            handed.insert(
                id,
                function
                    .params
                    .iter()
                    .chain(analysis.free_values(id))
                    .copied()
                    .collect(),
            );
        }

        // Monotone fixed point over a finite lattice: behaviors join, and the applied sets only grow within what each function was handed.
        loop {
            let mut changed = false;
            let mut next = current.clone();
            for id in module.function_ids() {
                let function = module.function(id).expect("live function");
                let mut context = Context::Within {
                    handed: &handed[&id],
                    applied: BTreeSet::new(),
                };
                let composed = Composer {
                    module,
                    summaries: &current,
                    rebindings: &rebindings,
                }
                .region(vec![function.body], &mut context);
                let Context::Within { applied, .. } = context else {
                    unreachable!("a function's body is composed within it");
                };
                let previous = &current[&id];
                let updated = Applying {
                    behavior: previous.behavior.join(composed),
                    applied: previous.applied.union(&applied).copied().collect(),
                };
                if updated != *previous {
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

    /// The total behavior of evaluating a right-hand side: its own operation, its callee or callback with what the call hands it, and every sub-block it evaluates. A value applied that names no function is an unknown, since nothing here says what was handed for it.
    pub fn rhs_behavior(&self, module: &Module, rhs: &Rhs) -> LocalBehavior {
        let composer = self.composer(module);
        let mut context = Context::Unknown;
        Semantics::local_behavior(rhs)
            .join(composer.call(rhs, &mut context))
            .join(composer.region(rhs.sub_blocks(), &mut context))
    }

    /// The total behavior of executing a statement: a `Let` evaluates its right-hand side; binding functions performs nothing (dormancy), and so does binding a recursive group, whose computed members are forced by need — an initializer runs when something reads its member, and the verifier holds it to performing no effect, so what it can contribute then is a trap or divergence the language does not owe a program that never forces it.
    pub fn statement_behavior(&self, module: &Module, statement: &Statement) -> LocalBehavior {
        match statement {
            Statement::Let { rhs, .. } => self.rhs_behavior(module, rhs),
            Statement::Functions { .. } | Statement::Rec { .. } => LocalBehavior::pure(),
        }
    }

    fn composer<'a>(&'a self, module: &'a Module) -> Composer<'a> {
        Composer {
            module,
            summaries: &self.functions,
            rebindings: &self.rebindings,
        }
    }
}

/// Where an application of a value is judged: inside a function, where one it was handed is recorded as applied, or with nothing known of what any value holds.
enum Context<'a> {
    Within {
        handed: &'a BTreeSet<ValueId>,
        applied: BTreeSet<ValueId>,
    },
    Unknown,
}

impl Context<'_> {
    /// An application of `value`, which names no function: recorded where it was handed, and the top otherwise.
    fn apply(&mut self, value: ValueId) -> LocalBehavior {
        match self {
            Self::Within { handed, applied } if handed.contains(&value) => {
                applied.insert(value);
                LocalBehavior::pure()
            }
            Self::Within { .. } | Self::Unknown => LocalBehavior::unknown(),
        }
    }
}

/// The walk composing behaviors over one snapshot of the per-function summaries.
struct Composer<'a> {
    module: &'a Module,
    summaries: &'a BTreeMap<FunctionId, Applying>,
    rebindings: &'a Rebindings,
}

impl Composer<'_> {
    /// The joined behavior of every block reachable from `seeds` through control flow — never through a nested function body, whose behavior is composed only at its calls, nor a nested group's initializers, which are forced by need.
    fn region(&self, seeds: Vec<BlockId>, context: &mut Context<'_>) -> LocalBehavior {
        let mut behavior = LocalBehavior::pure();
        let mut seen = BTreeSet::new();
        let mut work = seeds;
        while let Some(id) = work.pop() {
            if !seen.insert(id) {
                continue;
            }
            let Some(block) = self.module.block(id) else {
                continue;
            };
            for &statement in &block.statements {
                match self.module.statement(statement) {
                    Some(Statement::Let { rhs, .. }) => {
                        behavior = behavior
                            .join(Semantics::local_behavior(rhs))
                            .join(self.call(rhs, context));
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

    /// What a right-hand side inherits from the function it calls or the callback it runs.
    fn call(&self, rhs: &Rhs, context: &mut Context<'_>) -> LocalBehavior {
        match rhs {
            Rhs::Apply { callee, arguments } => {
                self.apply(*callee, Some(arguments), context, &mut BTreeSet::new())
            }
            Rhs::Intrinsic {
                intrinsic: Intrinsic::ListMap,
                operands,
            } => operands
                .get(1)
                .map_or_else(LocalBehavior::unknown, |&mapper| {
                    self.apply(mapper, None, context, &mut BTreeSet::new())
                }),
            _ => LocalBehavior::pure(),
        }
    }

    /// Applying `atom` to `arguments`, or to what nothing here knows where they are `None`: a known function contributes its summary and what applying each value it applies does — the argument handed for a parameter, the captured value itself, in scope here, for a capture. A function already being applied in this chain contributes nothing more, its behavior being joined already.
    fn apply(
        &self,
        atom: Atom,
        arguments: Option<&[Atom]>,
        context: &mut Context<'_>,
        chain: &mut BTreeSet<FunctionId>,
    ) -> LocalBehavior {
        match self.rebindings.resolve(atom) {
            Atom::Function(function) => {
                // The seed covers every live function and a verified module references only live functions, so a miss is a broken snapshot discipline, never a program property.
                let summary = self
                    .summaries
                    .get(&function)
                    .expect("a live callee has a summary");
                if arguments.is_none() && !chain.insert(function) {
                    return LocalBehavior::pure();
                }
                let params = &self
                    .module
                    .function(function)
                    .expect("a live callee is defined")
                    .params;
                summary
                    .applied
                    .iter()
                    .fold(summary.behavior, |behavior, &value| {
                        let handed = match params.iter().position(|&param| param == value) {
                            Some(position) => match arguments {
                                Some(arguments) => arguments
                                    .get(position)
                                    .map_or_else(LocalBehavior::unknown, |&argument| {
                                        self.apply(argument, None, context, chain)
                                    }),
                                None => LocalBehavior::unknown(),
                            },
                            None => self.apply(Atom::Value(value), None, context, chain),
                        };
                        behavior.join(handed)
                    })
            }
            Atom::Value(value) => context.apply(value),
            Atom::Constant(_) => LocalBehavior::unknown(),
        }
    }
}

#[cfg(test)]
mod tests;

/// Which source functions a dead call of may simply not happen, and on what each call must hand them: the carried termination verdict met with this stage's effect summary. A function is droppable, given the values it applies of what it was handed, where it was proved total and does nothing observable besides those applications — a call of it may then go where it hands a function droppable on its own for each of them; `None` is never.
///
/// **Both halves are needed and neither is this stage's to invent.** Termination is decided once, above Core, by the certifier's walk, and arrives on [`Function::total`](crate::Function::total). Freedom from effects is the interprocedural [`Summary`], which is ANF's and dissolves at this boundary — item granularity and per-function behavior become the entry's initialization code. So the conjunction is computed from the two where both are in hand, by the lowering into CPS, whose functions carry only it, and by the rebase, which leaves a recursion it may drop to the placement that reads the same fact below.
///
/// A mutable allocation is excluded because a program can tell one from another of equal contents; an immutable one is not, which is the case this exists for — a list built and never read is a list that need not be built.
pub fn droppable_functions(source: &Module) -> BTreeMap<FunctionId, Option<BTreeSet<ValueId>>> {
    let summary = Summary::analyze(source, &Analysis::analyze(source));
    summary
        .functions
        .iter()
        .map(|(&id, Applying { behavior, applied })| {
            let observable = behavior.observable;
            let droppable = source.function(id).is_some_and(|function| function.total)
                && !observable.host_effect
                && !observable.state_read
                && !observable.state_write
                && !observable.may_exit
                && !observable.may_diverge
                && behavior.operational.allocation != Allocation::Mutable;
            (id, droppable.then(|| applied.clone()))
        })
        .collect()
}
