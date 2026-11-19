//! A structural recursion inverted into a loop: a recursion that peels one parameter and calls itself once, on what the peel left, with every other parameter as it came, computes its base and then each level in turn from the innermost out, and becomes a worker that does so — a `Nat` recursion's counting its predecessor up from zero, a sequence's counting its indices down from its length and reading each level's element — whose call of itself is its tail, which the CPS optimizer makes a loop. Why, and what it rejects, is [`curios-anf`'s README](../../README.md#a-structural-recursion-is-a-loop).
//!
//! ```text
//!   rec f(n, a) = match n | 0 => z(a) | _ => let k = n - 1; s(k, a, f(k, a)) end
//!   ⟶  f(n, a)   = let rec go(k, r) = match k < n | true => go(k + 1, s(k, a, r)) | false => r end;
//!                  go(0, z(a))
//! ```
//!
//! **The envelope, structural in ANF.** The function's body is one dispatch it returns: a `SwitchNat` on a parameter whose one case is `0`, or an `UnconsSequence` on a parameter. Among the step arm's own statements is a self-call handed, in the peeled parameter's place, the predecessor the arm computes ahead of it (`n - 1`) or the suffix the peel binds, and every other parameter in its own place, or anything where the function never reads the parameter, as an erased type or proof is handed on as `unit`. It is the only reference the function's region makes to itself, every run of the arm reads its answer, and the dispatch and the predecessor are the only reads of the peeled parameter, which the loop leaves at its outermost value. Outside the envelope the pass is a no-op, never a miscompile.
//!
//! **Every run reads the answer.** A call whose answer only some runs of the arm read is one the lowering places where it is read ([`curios-cps`'s README](../../../curios-cps/README.md#a-droppable-computation-runs-where-it-is-read)), so the recursion stops at the first level that does not read it, as `x == y && eql(xs, ys)` stops at the first unequal pair; its loop would run every level. So the arm's answer is read, and so is what the definition of a value read reads, chased across the arm's statements and its arms alike ([`Demand`]); the call's answer must be among it.
//!
//! **What runs in another order.** The recursion runs every level's statements ahead of its call from the outermost level in, then the base, then every level's statements after its call from the innermost out; the loop runs the base, then each level whole from the innermost out. The base and what follows each call keep their order, and the statements ahead of a call, which run later than they did, are held to perform nothing observable ([`Summary`]). Nothing runs earlier than a value it could depend on: the base reads only what the function was handed, and a statement ahead of the call only its own level's, so a proof erased from either still holds where it runs.

#[cfg(test)]
mod tests;

use {
    crate::{
        Analysis, Atom, Block, BlockId, Constant, Function, FunctionId, Module, NatCase, Operation,
        Rhs, Semantics, SequenceGrain, SequenceOp, Statement, StatementId, Summary, Terminator,
        ValueId, control_blocks, self_references,
    },
    curios_common::recurse,
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

/// Invert every recognized recursion into a loop, module-wide.
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

/// A recursion recognized as a loop: the function, the dispatch its body returns and what it binds, the peeled parameter, the base and step arms, the step arm's statements less the call and the predecessor, and the values a turn of the loop is handed where the recursion bound them — the answer so far where the call's answer stood, and what the peel left.
struct Inversion {
    function: FunctionId,
    dispatch: StatementId,
    result: ValueId,
    parameter: ValueId,
    base: BlockId,
    step: BlockId,
    statements: Vec<StatementId>,
    hypothesis: ValueId,
    peel: Peel,
}

/// What a turn of the loop is handed of the peeled parameter.
enum Peel {
    /// A `Nat`'s predecessor: the loop counts it up from zero to the parameter.
    Nat { predecessor: ValueId },
    /// A sequence's element: the loop counts the indices down from its length, reading each.
    Sequence {
        grain: SequenceGrain,
        element: ValueId,
    },
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

        // The base is computed ahead of the loop, so it must hand back a value.
        if !matches!(module.block(base)?.terminator, Terminator::Return(_)) {
            return None;
        }

        // What the peel left: `n - 1`, computed ahead of the call, or the suffix the peel binds, which no turn may read beyond the call it is handed to: the loop reads each element by its index and builds no suffix.
        let (peel, predecessor, suffix) = match sequence {
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
                (Peel::Nat { predecessor: tail }, Some(predecessor), None)
            }
            Some((grain, element, suffix)) => {
                if tail != suffix {
                    return None;
                }
                (Peel::Sequence { grain, element }, None, Some(suffix))
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
        // Each level read its own value of the peeled parameter, where the loop's turns would read the outermost one.
        if analysis.value_uses(parameter) != 1 + usize::from(predecessor.is_some()) {
            return None;
        }
        // A parameter the call hands on changed is one no level reads.
        if changed.iter().any(|&param| analysis.value_uses(param) != 0) {
            return None;
        }
        // The call is the one reader of the suffix.
        if suffix.is_some_and(|suffix| analysis.value_uses(suffix) != 1) {
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
            function,
            dispatch,
            result: *result,
            parameter,
            base,
            step,
            statements,
            hypothesis,
            peel,
        })
    }

    /// Put a loop in the recursion's place: the base computed first, as the innermost level computes it, then a worker taking the levels in turn from the innermost out, each turn the step arm handed the level's peel and the answer so far, its own answer handed on in its tail. A `Nat`'s worker counts the predecessor up from zero to the parameter; a sequence's counts the indices down from its length and reads each level's element. The worker is what the recursion was ([`crate::Function::role`]), and its termination is the recursion's: inverting a recursion moves where its levels run, never whether they end.
    fn apply(self, module: &mut Module) {
        let Self {
            function,
            dispatch,
            result,
            parameter,
            base,
            step,
            statements,
            hypothesis,
            peel,
        } = self;
        let definition = module
            .function(function)
            .cloned()
            .expect("an inverted function is live");
        let body = module
            .block(definition.body)
            .cloned()
            .expect("an inverted function has a body");
        let base_block = module.block(base).cloned().expect("the base arm is live");
        let Terminator::Return(start_answer) = base_block.terminator else {
            unreachable!("the base hands back a value, as the recognizer required");
        };
        let step_block = module.block(step).cloned().expect("the step arm is live");
        let one = Atom::Constant(module.intern_constant(Constant::Nat(Natural::one())));
        let worker = module.reserve_function();

        // What a turn starts from, what it reads of the peel, and the count it hands the next turn.
        let (counter, start, setup, mut turn, next) = match peel {
            Peel::Nat { predecessor } => {
                let zero = Atom::Constant(module.intern_constant(Constant::Nat(Natural::zero())));
                let (raised, raising) = module.let_value(
                    None,
                    Rhs::Operation {
                        operation: Operation::NatAdd,
                        operands: vec![Atom::Value(predecessor), one],
                    },
                );
                (predecessor, zero, Vec::new(), vec![raising], raised)
            }
            Peel::Sequence { grain, element } => {
                let (len, get) = match grain {
                    SequenceGrain::Bytes => (SequenceOp::BytesLen, SequenceOp::BytesGet),
                    SequenceGrain::List => (SequenceOp::ListLen, SequenceOp::ListGet),
                };
                let (length, measured) = module.let_value(
                    None,
                    Rhs::Sequence {
                        operation: len,
                        operands: vec![Atom::Value(parameter)],
                    },
                );
                let index = module.add_value(Some("i".into()));
                let (below, lowering) = module.let_value(
                    None,
                    Rhs::Operation {
                        operation: Operation::NatSub,
                        operands: vec![Atom::Value(index), one],
                    },
                );
                // The element the peel bound, now read by its index: the peel's binder stated a carrier, and the read names its own.
                let read = module.add_statement(Statement::Let {
                    result: element,
                    rhs: Rhs::Sequence {
                        operation: get,
                        operands: vec![Atom::Value(parameter), Atom::Value(below)],
                    },
                });
                (
                    index,
                    Atom::Value(length),
                    vec![measured],
                    vec![lowering, read],
                    below,
                )
            }
        };
        turn.extend(statements);
        // A turn hands its answer on to the next, unless the step arm ends otherwise.
        let terminator = match step_block.terminator {
            Terminator::Return(answer) => {
                let (handed, handing) = module.let_value(
                    None,
                    Rhs::Apply {
                        callee: Atom::Function(worker),
                        arguments: vec![Atom::Value(next), answer],
                    },
                );
                turn.push(handing);
                Terminator::Return(Atom::Value(handed))
            }
            terminator => terminator,
        };
        module.set_block_statements(step, turn);
        module.set_block_terminator(step, terminator);

        // Done once the count is: the answer so far is the recursion's.
        let done = module.add_block(Block {
            statements: Vec::new(),
            terminator: Terminator::Return(Atom::Value(hypothesis)),
        });
        let (test, switched) = match peel {
            Peel::Nat { .. } => {
                let (more, comparing) = module.let_value(
                    None,
                    Rhs::Operation {
                        operation: Operation::NatLt,
                        operands: vec![Atom::Value(counter), Atom::Value(parameter)],
                    },
                );
                let (switched, switching) = module.let_value(
                    None,
                    Rhs::SwitchBool {
                        scrutinee: Atom::Value(more),
                        if_false: done,
                        if_true: step,
                    },
                );
                (vec![comparing, switching], switched)
            }
            Peel::Sequence { .. } => {
                let (switched, switching) = module.let_value(
                    None,
                    Rhs::SwitchNat {
                        scrutinee: Atom::Value(counter),
                        cases: vec![NatCase {
                            key: Natural::zero(),
                            block: done,
                        }],
                        default: step,
                    },
                );
                (vec![switching], switched)
            }
        };
        let worker_body = module.add_block(Block {
            statements: test,
            terminator: Terminator::Return(Atom::Value(switched)),
        });
        module.define_function(
            worker,
            Function {
                lineage: definition.lineage,
                debug_name: definition
                    .debug_name
                    .as_ref()
                    .map(|name| format!("{name}@loop")),
                params: vec![counter, hypothesis],
                body: worker_body,
                role: definition.role,
                total: definition.total,
            },
        );

        // The function: the base, the worker, and the loop entered at the start with the base's answer.
        let binding = module.add_functions(vec![worker]);
        module.set_statement(
            dispatch,
            Statement::Let {
                result,
                rhs: Rhs::Apply {
                    callee: Atom::Function(worker),
                    arguments: vec![start, start_answer],
                },
            },
        );
        module.set_block_statements(base, Vec::new());
        let mut statements = base_block.statements;
        statements.push(binding);
        statements.extend(setup);
        statements.push(dispatch);
        debug_assert_eq!(body.statements, vec![dispatch]);
        module.set_block_statements(definition.body, statements);
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
