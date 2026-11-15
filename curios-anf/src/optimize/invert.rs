//! A structural recursion inverted into its fold: a recursion that peels one parameter and calls itself once, on what the peel left, with every other parameter as it came, computes the fold of that peel's case split, and becomes it — a `Nat` recursion `FoldNat`, a sequence's `FoldSequence` — which the lowering into CPS makes a loop. Why, and what it rejects, is [`curios-anf`'s README](../../README.md#a-structural-recursion-is-its-fold).
//!
//! ```text
//!   rec f(n, a) = match n | 0 => z(a) | _ => let k = n - 1; s(k, a, f(k, a)) end
//!   ⟶  f(n, a)   = fold n | 0 => z(a) | k + 1; r => s(k, a, r) end
//! ```
//!
//! **The envelope, structural in ANF.** The function's body is one dispatch it returns: a `SwitchNat` on a parameter whose one case is `0`, or an `UnconsSequence` on a parameter. Among the step arm's own statements is a self-call handed, in the peeled parameter's place, the predecessor the arm computes ahead of it (`n - 1`) or the suffix the peel binds, and every other parameter in its own place, or anything where the function never reads the parameter, as an erased type or proof is handed on as `unit`. It is the only reference the function's region makes to itself, every run of the arm reads its answer, and the dispatch and the predecessor are the only reads of the peeled parameter, which the fold leaves at its outermost value. Outside the envelope the pass is a no-op, never a miscompile.
//!
//! **Every run reads the answer.** A call whose answer only some runs of the arm read is one the lowering places where it is read ([`curios-cps`'s README](../../../curios-cps/README.md#a-droppable-computation-runs-where-it-is-read)), so the recursion stops at the first level that does not read it, as `x == y && eql(xs, ys)` stops at the first unequal pair; its fold would run every level. So the arm's answer is read, and so is what the definition of a value read reads, chased across the arm's statements and its arms alike ([`Demand`]); the call's answer must be among it.
//!
//! **What runs in another order.** The recursion runs every level's statements ahead of its call from the outermost level in, then the base, then every level's statements after its call from the innermost out; the fold runs the base, then each level whole from the innermost out. The base and what follows each call keep their order, and the statements ahead of a call, which run later than they did, are held to perform nothing observable ([`Summary`]). Nothing runs earlier than a value it could depend on: the base reads only what the function was handed, and a statement ahead of the call only its own level's, so a proof erased from either still holds where it runs.

#[cfg(test)]
mod tests;

use {
    crate::{
        Analysis, Atom, BlockId, Constant, FoldNatStep, FoldSequenceStep, FunctionId, Module,
        Operation, Rhs, Semantics, Statement, StatementId, Summary, Terminator, ValueId,
        control_blocks, self_references,
    },
    curios_common::recurse,
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

/// Invert every recognized recursion into its fold, module-wide.
pub fn invert_structural_recursion(module: &mut Module) {
    curios_profile::profile!("invert_structural_recursion");
    // Targets: functions bound alone by a `Functions` statement, anywhere. An inversion rewrites its own function's body and nothing else, so one snapshot serves every target: what it still says of a function already inverted, that it recurses, only makes a later target's reading more cautious.
    let targets: Vec<FunctionId> = module
        .statements()
        .iter()
        .filter_map(|slot| match slot {
            Some(Statement::Functions { functions }) => match functions.as_slice() {
                [function] => Some(*function),
                _ => None,
            },
            _ => None,
        })
        .collect();

    // Read where a recursion has the envelope's shape, and only then.
    let mut facts = None;
    for function in targets {
        if let Some(inversion) = Inversion::of(module, &mut facts, function) {
            inversion.apply(module);
        }
    }
}

/// A recursion recognized as its fold: the dispatch its body returns, the fold that takes its place, and the step arm's statements less the call and the predecessor, which the fold binds.
struct Inversion {
    dispatch: StatementId,
    result: ValueId,
    fold: Rhs,
    step: BlockId,
    statements: Vec<StatementId>,
}

impl Inversion {
    /// The inversion of `function` where it is in the envelope: its shape read off its body, then what the snapshot says of the peeled parameter's reads and of the statements ahead of the call.
    fn of(
        module: &Module,
        facts: &mut Option<(Analysis, Summary)>,
        function: FunctionId,
    ) -> Option<Self> {
        let definition = module.function(function)?;
        let params = &definition.params;
        let body = module.block(definition.body)?;
        let (&[dispatch], Terminator::Return(returned)) =
            (body.statements.as_slice(), &body.terminator)
        else {
            return None;
        };
        let Some(Statement::Let { result, rhs }) = module.statement(dispatch) else {
            return None;
        };
        if *returned != Atom::Value(*result) {
            return None;
        }

        // The peel: the parameter, the base arm, the step arm, and what a sequence's binds.
        let (parameter, base, step, sequence) = match rhs {
            Rhs::SwitchNat {
                scrutinee: Atom::Value(parameter),
                cases,
                default,
            } => match cases.as_slice() {
                [case] if case.key == Natural::zero() => (*parameter, case.block, *default, None),
                _ => return None,
            },
            Rhs::UnconsSequence {
                grain,
                scrutinee: Atom::Value(parameter),
                empty,
                cons,
            } => (
                *parameter,
                *empty,
                cons.block,
                Some((*grain, cons.element, cons.suffix)),
            ),
            _ => return None,
        };
        let peeled = params.iter().position(|param| *param == parameter)?;

        // The call: a self-call among the step arm's own statements, every argument but the peeled one the parameter in its place, or one the function never reads, as an erased type or proof is handed on as `unit`.
        let arm = module.block(step)?;
        let (position, hypothesis, tail) =
            arm.statements
                .iter()
                .enumerate()
                .find_map(|(position, &statement)| match module.statement(statement) {
                    Some(Statement::Let {
                        result,
                        rhs:
                            Rhs::Apply {
                                callee: Atom::Function(callee),
                                arguments,
                            },
                    }) if *callee == function && arguments.len() == params.len() => {
                        Some((position, *result, arguments))
                    }
                    _ => None,
                })?;
        let changed: Vec<ValueId> = tail
            .iter()
            .zip(params)
            .enumerate()
            .filter(|(index, (argument, param))| {
                *index != peeled && **argument != Atom::Value(**param)
            })
            .map(|(_, (_, param))| *param)
            .collect();
        let Atom::Value(tail) = tail[peeled] else {
            return None;
        };
        let call = arm.statements[position];
        let ahead = &arm.statements[..position];
        let Read::Values(read) = Demand::of(module, step).block(step) else {
            return None;
        };
        if !read.contains(&hypothesis) {
            return None;
        }

        // What the peel left: `n - 1`, computed ahead of the call, or the suffix the peel binds.
        let (fold, predecessor) = match sequence {
            None => {
                let predecessor = ahead.iter().copied().find(|&statement| {
                    matches!(
                        module.statement(statement),
                        Some(Statement::Let {
                            result,
                            rhs: Rhs::Operation {
                                operation: Operation::NatSub,
                                operands,
                            },
                        }) if *result == tail
                            && matches!(
                                operands.as_slice(),
                                [Atom::Value(minuend), Atom::Constant(one)]
                                    if *minuend == parameter
                                        && module.constant(*one) == Some(&Constant::Nat(Natural::one()))
                            )
                    )
                })?;
                let fold = Rhs::FoldNat {
                    scrutinee: Atom::Value(parameter),
                    zero: base,
                    step: FoldNatStep {
                        predecessor: tail,
                        hypothesis,
                        block: step,
                    },
                };
                (fold, Some(predecessor))
            }
            Some((grain, element, suffix)) => {
                if tail != suffix {
                    return None;
                }
                let fold = Rhs::FoldSequence {
                    grain,
                    scrutinee: Atom::Value(parameter),
                    empty: base,
                    step: FoldSequenceStep {
                        element,
                        suffix,
                        accumulator: hypothesis,
                        block: step,
                    },
                };
                (fold, None)
            }
        };

        if self_references(module, function) != 1 {
            return None;
        }
        let (analysis, summary) = facts.get_or_insert_with(|| {
            let analysis = Analysis::analyze(module);
            let summary = Summary::analyze(module, &analysis);
            (analysis, summary)
        });
        // Each level read its own value of the peeled parameter, where the fold's arms would read the outermost one.
        if analysis.value_uses(parameter) != 1 + usize::from(predecessor.is_some()) {
            return None;
        }
        // A parameter the call hands on changed is one no level reads.
        if changed.iter().any(|&param| analysis.value_uses(param) != 0) {
            return None;
        }
        // What runs later than it did performs nothing a program could tell the order of.
        let quiet = ahead
            .iter()
            .filter(|&&statement| Some(statement) != predecessor)
            .all(|&statement| {
                module.statement(statement).is_some_and(|statement| {
                    !summary
                        .statement_behavior(module, statement)
                        .observable
                        .is_effectful()
                })
            });
        if !quiet {
            return None;
        }

        let statements = arm
            .statements
            .iter()
            .copied()
            .filter(|&statement| statement != call && Some(statement) != predecessor)
            .collect();
        Some(Self {
            dispatch,
            result: *result,
            fold,
            step,
            statements,
        })
    }

    /// Put the fold in the dispatch's place and take the call and the predecessor out of the step arm, whose results the fold now binds. The statements taken out own nothing else, and the prune after the optimizer's passes drops them.
    fn apply(self, module: &mut Module) {
        module.set_block_statements(self.step, self.statements);
        module.set_statement(
            self.dispatch,
            Statement::Let {
                result: self.result,
                rhs: self.fold,
            },
        );
    }
}

/// What every run of a step arm reads, chased through the definitions of what it reads: a value, and what its definition reads; of an operation its first operand can decide, that first alone ([`Semantics::decided_by_first`]); of a dispatch, its scrutinee and what every one of its arms reads; of a call of a function the arm binds, what its body reads, as a guarded match's fallthrough is read where its arms call it.
struct Demand<'m> {
    module: &'m Module,
    /// The right-hand side binding each value of the arm's region: its blocks, and the bodies of the functions it binds.
    definitions: BTreeMap<ValueId, &'m Rhs>,
    /// The functions the arm's region binds.
    bound: BTreeSet<FunctionId>,
    /// The functions whose bodies are being read, a recursion among them reading nothing more.
    entered: BTreeSet<FunctionId>,
    read: BTreeMap<ValueId, Read>,
}

/// What every run of a computation reads.
#[derive(Clone)]
enum Read {
    /// No run returns: each traps or exits, which ends it whatever it read.
    All,
    Values(BTreeSet<ValueId>),
}

impl Read {
    /// What a run of both reads.
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Values(mut first), Self::Values(second)) => {
                first.extend(second);
                Self::Values(first)
            }
            _ => Self::All,
        }
    }

    /// What a run of either reads.
    fn meet(self, other: Self) -> Self {
        match (self, other) {
            (Self::All, other) | (other, Self::All) => other,
            (Self::Values(first), Self::Values(second)) => {
                Self::Values(first.intersection(&second).copied().collect())
            }
        }
    }
}

impl<'m> Demand<'m> {
    /// The demand of the arm `step`.
    fn of(module: &'m Module, step: BlockId) -> Self {
        let mut definitions = BTreeMap::new();
        let mut bound = BTreeSet::new();
        let mut regions = vec![step];
        while let Some(region) = regions.pop() {
            for block in control_blocks(module, region) {
                let Some(block) = module.block(block) else {
                    continue;
                };
                for &statement in &block.statements {
                    match module.statement(statement) {
                        Some(Statement::Let { result, rhs }) => {
                            definitions.insert(*result, rhs);
                        }
                        Some(Statement::Functions { functions }) => {
                            for &function in functions {
                                if bound.insert(function)
                                    && let Some(definition) = module.function(function)
                                {
                                    regions.push(definition.body);
                                }
                            }
                        }
                        Some(Statement::Rec { .. }) | None => {}
                    }
                }
            }
        }
        Self {
            module,
            definitions,
            bound,
            entered: BTreeSet::new(),
            read: BTreeMap::new(),
        }
    }

    /// What every run of `block` reads: what it returns.
    fn block(&mut self, block: BlockId) -> Read {
        match self.module.block(block).map(|block| &block.terminator) {
            Some(Terminator::Return(Atom::Value(value))) => self.value(*value),
            Some(Terminator::Return(_)) => Read::Values(BTreeSet::new()),
            Some(Terminator::Halt { .. } | Terminator::Unreachable) | None => Read::All,
        }
    }

    /// What reading `value` reads: itself, and what its definition reads.
    fn value(&mut self, value: ValueId) -> Read {
        if let Some(read) = self.read.get(&value) {
            return read.clone();
        }
        let itself = Read::Values(BTreeSet::from([value]));
        let read = match self.definitions.get(&value).copied() {
            Some(rhs) => itself.join(recurse(|| self.rhs(rhs))),
            None => itself,
        };
        self.read.insert(value, read.clone());
        read
    }

    fn rhs(&mut self, rhs: &'m Rhs) -> Read {
        let operands = match rhs {
            Rhs::Operation {
                operation,
                operands,
            } if Semantics::decided_by_first(*operation).is_some() => {
                operands.iter().take(1).copied().collect()
            }
            _ => rhs.operands(),
        };
        let mut read = Read::Values(BTreeSet::new());
        for operand in operands {
            if let Atom::Value(operand) = operand {
                read = read.join(self.value(operand));
            }
        }
        match rhs {
            Rhs::Apply {
                callee: Atom::Function(callee),
                ..
            } if self.bound.contains(callee) && !self.entered.contains(callee) => {
                let Some(function) = self.module.function(*callee) else {
                    return read;
                };
                self.entered.insert(*callee);
                let body = self.block(function.body);
                self.entered.remove(callee);
                read.join(body)
            }
            _ => {
                let arms = rhs.sub_blocks();
                if arms.is_empty() {
                    return read;
                }
                let every = arms
                    .into_iter()
                    .fold(Read::All, |every, arm| every.meet(self.block(arm)));
                read.join(every)
            }
        }
    }
}
