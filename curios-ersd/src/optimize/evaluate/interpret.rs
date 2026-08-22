//! The fueled big-step, call-by-value interpreter over the direct-style ANF.
//!
//! The interpreter walks blocks — statements then a terminator — so tail position, the only place an effect may residualize, is the statement whose result the block returns. ANF hoists every effect into a `Let`, so a `Foreign` feeding a block's `Return` is a tail-position effect and residualizes to a single host call — exactly the `Handle/write(<bytes>)` the `Fmt` collapse produces.
//!
//! Closedness and scope: a candidate call is evaluated from an empty frame; every atom it transitively reads must resolve to a constant, a module function, or a top-level item value (a CAF forced on demand and memoized), never a runtime binder. A function reference closes on demand against the current frame. Scalar and packed-binary operations fold through [`Semantics`] — the single source of truth under the numeric law — while list operations, closures, and `ListMap` are interpreted directly.

use {
    super::{
        budget::Budget,
        value::{Bail, Closure, Value},
    },
    crate::{
        Analysis, Atom, BlockId, Constant, FoldNatStep, FoldOutcome, FoldSequenceStep, ForeignId,
        FunctionId, Intrinsic, Module, Operation, Rhs, Semantics, SequenceGrain, SequenceOp,
        Statement, Terminator, UnconsSequenceStep, ValueId, VariantArm,
    },
    curios_utilities::{Grain, PackedBin},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// A lexical frame: the values in scope, innermost last. Identities are unique per module, so lookup scans from the back and a scope pops by truncation.
struct Frame {
    values: Vec<(ValueId, Value)>,
}

impl Frame {
    fn new() -> Self {
        Self { values: Vec::new() }
    }

    fn lookup(&self, value: ValueId) -> Option<Value> {
        self.values
            .iter()
            .rev()
            .find(|(bound, _)| *bound == value)
            .map(|(_, held)| held.clone())
    }

    fn push(&mut self, value: ValueId, held: Value) {
        self.values.push((value, held));
    }

    fn mark(&self) -> usize {
        self.values.len()
    }

    fn restore(&mut self, mark: usize) {
        self.values.truncate(mark);
    }
}

/// How one evaluation ended.
pub(super) enum Outcome {
    Done(Value),
    /// A tail-position effect reached with fully-evaluated inputs.
    Stuck(Residual),
    Bail(Bail),
}

/// A residual installed in place of a folded candidate: a tail-position host call, or a tail call to a module function whose body performs an effect it cannot residualize itself.
pub(super) enum Residual {
    Foreign(ForeignId, Vec<Value>),
    Call(FunctionId, Vec<Value>),
}

/// Where a forced CAF's value comes from: a top-level item `Let` right-hand side, or an eager recursive-group member's initializer block.
enum CafSource<'m> {
    Rhs(&'m Rhs),
    Block(BlockId),
}

/// The interpreter over a borrowed module and its analysis snapshot.
pub(super) struct Evaluator<'m> {
    module: &'m Module,
    analysis: &'m Analysis,
    /// Every `Let`-defined value, module-wide, forced on demand. Forcing from an empty frame naturally declines anything not transitively closed (a parameter or binder leaks as an unknown), so this is exactly the "local CAF" generalization that lets curried chains rooted in entry-block locals fold.
    definitions: BTreeMap<ValueId, &'m Rhs>,
    rec_caf: BTreeMap<ValueId, BlockId>,
    /// Functions bound by top-level items — the only callees a residual call may name (a locally bound function is out of scope at an arbitrary candidate site).
    item_functions: BTreeSet<FunctionId>,
    /// Memoized CAF values; `None` poisons a value whose forcing failed hard.
    cache: BTreeMap<ValueId, Option<Value>>,
    /// Values currently being forced — a repeat is a value-recursive cycle.
    forcing: BTreeSet<ValueId>,
    budget: Budget,
}

impl<'m> Evaluator<'m> {
    /// Build an interpreter over a module and its analysis. The top-level value bindings are the module's item chain.
    pub(super) fn new(module: &'m Module, analysis: &'m Analysis) -> Self {
        let mut definitions = BTreeMap::new();
        let mut rec_caf = BTreeMap::new();
        let mut item_functions = BTreeSet::new();
        for slot in module.statements().iter() {
            if let Some(Statement::Let { result, rhs }) = slot {
                definitions.insert(*result, rhs);
            }
        }
        for &item in module.items() {
            match module.statement(item) {
                Some(Statement::Rec { group }) => {
                    if let Some(group) = module.rec_group(*group) {
                        for member in &group.values {
                            rec_caf.insert(member.value, member.init);
                        }
                        item_functions.extend(group.functions.iter().copied());
                    }
                }
                Some(Statement::Functions { functions }) => {
                    item_functions.extend(functions.iter().copied());
                }
                _ => {}
            }
        }
        Self {
            module,
            analysis,
            definitions,
            rec_caf,
            item_functions,
            cache: BTreeMap::new(),
            forcing: BTreeSet::new(),
            budget: Budget::new(),
        }
    }

    /// Whether an atom names a closed value — a constant, a module function, or a top-level item value — so a call over such atoms is a candidate.
    pub(super) fn is_closed_atom(&self, atom: Atom) -> bool {
        match atom {
            Atom::Constant(_) | Atom::Function(_) => true,
            Atom::Value(value) => {
                self.definitions.contains_key(&value) || self.rec_caf.contains_key(&value)
            }
        }
    }

    /// Evaluate one closed candidate call from a fresh budget frame.
    pub(super) fn evaluate(&mut self, callee: Atom, arguments: &[Atom]) -> Outcome {
        self.budget.restart();
        let mut frame = Frame::new();
        self.eval_apply(callee, arguments, &mut frame, true)
    }

    fn eval_atom(&mut self, atom: Atom, frame: &Frame) -> Result<Value, Bail> {
        match atom {
            Atom::Constant(id) => match self.module.constant(id) {
                Some(constant) => Ok(Value::from_constant(constant)),
                None => Err(Bail::Unknown),
            },
            Atom::Function(function) => self.close(function, frame),
            Atom::Value(value) => match frame.lookup(value) {
                Some(held) => Ok(held),
                None => self.force_toplevel(value),
            },
        }
    }

    /// Close a function over its free values against the current frame. A free value that is a top-level CAF resolves on demand at use; one that is neither in scope nor top-level declines the fold.
    fn close(&self, function: FunctionId, frame: &Frame) -> Result<Value, Bail> {
        let mut captured = Vec::new();
        for &free in self.analysis.free_values(function) {
            match frame.lookup(free) {
                Some(held) => captured.push((free, held)),
                None if self.definitions.contains_key(&free)
                    || self.rec_caf.contains_key(&free) => {}
                None => return Err(Bail::Unknown),
            }
        }
        Ok(Value::Closure(Rc::new(Closure {
            function,
            env: RefCell::new(captured),
        })))
    }

    /// The memoized value of a top-level CAF. A failed forcing poisons the value with a hard bail — never `Effect`, so an effectful CAF is never residualized as a call — except on fuel exhaustion, which leaves it uncached for a fresher attempt.
    fn force_toplevel(&mut self, value: ValueId) -> Result<Value, Bail> {
        if let Some(cached) = self.cache.get(&value) {
            return cached.clone().ok_or(Bail::Unknown);
        }
        let source = if let Some(rhs) = self.definitions.get(&value).copied() {
            CafSource::Rhs(rhs)
        } else if let Some(&block) = self.rec_caf.get(&value) {
            CafSource::Block(block)
        } else {
            return Err(Bail::Unknown);
        };
        if !self.forcing.insert(value) {
            return Err(Bail::Cycle);
        }
        let outcome = match source {
            CafSource::Rhs(rhs) => self.eval_rhs(rhs, &mut Frame::new(), false),
            CafSource::Block(block) => self.eval_block(block, &mut Frame::new(), false),
        };
        let result = match outcome {
            Outcome::Done(held) => Ok(held),
            Outcome::Bail(Bail::Fuel) => {
                self.forcing.remove(&value);
                return Err(Bail::Fuel);
            }
            Outcome::Stuck(_) | Outcome::Bail(_) => Err(Bail::Unknown),
        };
        self.forcing.remove(&value);
        self.cache.insert(value, result.clone().ok());
        result
    }

    /// Evaluate a block in non-tail position, where the result must be a value: a `Stuck` cannot arise (an effect outside the tail bails).
    fn value_of_block(&mut self, block: BlockId, frame: &mut Frame) -> Result<Value, Bail> {
        match self.eval_block(block, frame, false) {
            Outcome::Done(held) => Ok(held),
            Outcome::Stuck(_) => unreachable!("non-tail evaluation never residualizes"),
            Outcome::Bail(bail) => Err(bail),
        }
    }

    /// Evaluate a block: statements in order, then the terminator. In tail position the last statement — when it defines the returned value — is evaluated in tail position, so a `Foreign` there residualizes.
    fn eval_block(&mut self, block: BlockId, frame: &mut Frame, tail: bool) -> Outcome {
        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }
        let module = self.module;
        let Some(block) = module.block(block) else {
            return Outcome::Bail(Bail::Unknown);
        };
        let returned = match &block.terminator {
            Terminator::Return(atom) => *atom,
            // An exit is an effect the candidate binding cannot become; the tail-call boundary converts it into a residual call instead.
            Terminator::Exit(_) => return Outcome::Bail(Bail::Effect),
            Terminator::Unreachable => return Outcome::Bail(Bail::Trap),
        };

        let mark = frame.mark();
        let last = block.statements.len().checked_sub(1);
        for (index, &statement) in block.statements.iter().enumerate() {
            match module.statement(statement) {
                Some(Statement::Let { result, rhs }) => {
                    let is_tail = tail && Some(index) == last && returned == Atom::Value(*result);
                    match self.eval_rhs(rhs, frame, is_tail) {
                        Outcome::Done(held) => {
                            if is_tail {
                                frame.restore(mark);
                                return Outcome::Done(held);
                            }
                            frame.push(*result, held);
                        }
                        outcome @ (Outcome::Stuck(_) | Outcome::Bail(_)) => {
                            frame.restore(mark);
                            return outcome;
                        }
                    }
                }
                // Function groups resolve their members on demand at each reference; introducing them binds no value here.
                Some(Statement::Functions { .. }) => {}
                // A local recursive group with computed members would need the knot's cells modeled here; decline. A top-level one is forced on demand through `force_toplevel`, which is the same by-need semantics the lowering gives every knot. A function-only group binds on demand.
                Some(Statement::Rec { group })
                    if module
                        .rec_group(*group)
                        .is_some_and(|group| !group.values.is_empty()) =>
                {
                    frame.restore(mark);
                    return Outcome::Bail(Bail::Unsupported);
                }
                Some(Statement::Rec { .. }) => {}
                None => {}
            }
        }

        let result = self.eval_atom(returned, frame);
        frame.restore(mark);
        match result {
            Ok(held) => Outcome::Done(held),
            Err(bail) => Outcome::Bail(bail),
        }
    }

    fn eval_rhs(&mut self, rhs: &Rhs, frame: &mut Frame, tail: bool) -> Outcome {
        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }
        match rhs {
            Rhs::Alias(atom) => outcome(self.eval_atom(*atom, frame)),
            Rhs::Apply { callee, arguments } => self.eval_apply(*callee, arguments, frame, tail),
            Rhs::Operation {
                operation,
                operands,
            } => self.eval_operation(*operation, operands, frame),
            Rhs::Sequence {
                operation,
                operands,
            } => self.eval_sequence(*operation, operands, frame),
            Rhs::Product { schema, fields } => match self.eval_operands(fields, frame) {
                Ok(fields) => Outcome::Done(Value::Product(*schema, Rc::new(fields))),
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::Construct {
                constructor,
                fields,
            } => match self.eval_operands(fields, frame) {
                Ok(fields) => Outcome::Done(Value::Construct(*constructor, Rc::new(fields))),
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::Project { product, field, .. } => match self.eval_atom(*product, frame) {
                Ok(Value::Product(_, fields)) => match fields.get(*field as usize) {
                    Some(held) => Outcome::Done(held.clone()),
                    None => Outcome::Bail(Bail::Unsupported),
                },
                Ok(_) => Outcome::Bail(Bail::Unsupported),
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::MatchVariant {
                scrutinee,
                arms,
                default,
                ..
            } => self.eval_match(*scrutinee, arms, *default, frame, tail),
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => match self
                .eval_atom(*scrutinee, frame)
                .and_then(|held| held.bool_())
            {
                Ok(chosen) => {
                    self.eval_block(if chosen { *if_true } else { *if_false }, frame, tail)
                }
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::SwitchNat {
                scrutinee,
                cases,
                default,
            } => match self
                .eval_atom(*scrutinee, frame)
                .and_then(|held| held.nat())
            {
                Ok(key) => {
                    let block = cases
                        .iter()
                        .find(|case| case.key == key)
                        .map_or(*default, |case| case.block);
                    self.eval_block(block, frame, tail)
                }
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::FoldNat {
                scrutinee,
                zero,
                step,
            } => self.eval_fold_nat(*scrutinee, *zero, step, frame),
            Rhs::FoldSequence {
                grain,
                scrutinee,
                empty,
                step,
            } => self.eval_fold_sequence(*grain, *scrutinee, *empty, step, frame),
            Rhs::UnconsSequence {
                grain,
                scrutinee,
                empty,
                cons,
            } => self.eval_uncons_sequence(*grain, *scrutinee, *empty, cons, frame),
            // A host call is an effect: it residualizes in tail position with evaluated operands, and bails everywhere else.
            Rhs::Foreign { foreign, operands } => match self.eval_operands(operands, frame) {
                Ok(operands) if tail => Outcome::Stuck(Residual::Foreign(*foreign, operands)),
                Ok(_) => Outcome::Bail(Bail::Effect),
                Err(bail) => Outcome::Bail(bail),
            },
            // A cell operation's identity is its program point: never residualized, never folded.
            Rhs::Cell { .. } => Outcome::Bail(Bail::Effect),
            Rhs::Intrinsic {
                intrinsic: Intrinsic::ListMap,
                operands,
            } => self.eval_map(operands, frame),
        }
    }

    fn eval_apply(
        &mut self,
        callee: Atom,
        arguments: &[Atom],
        frame: &mut Frame,
        tail: bool,
    ) -> Outcome {
        let head = match self.eval_atom(callee, frame) {
            Ok(head) => head,
            Err(bail) => return Outcome::Bail(bail),
        };
        let args = match self.eval_operands(arguments, frame) {
            Ok(args) => args,
            Err(bail) => return Outcome::Bail(bail),
        };
        let Value::Closure(closure) = head else {
            return Outcome::Bail(Bail::Unsupported);
        };

        match self.enter(&closure, args.clone(), tail) {
            // Boundary conversion: the callee performs an effect it cannot residualize itself, but this call is the candidate's tail and names an *item-bound* function — in scope at every candidate site — so the call with its evaluated arguments is the residual, rerunning the callee in full at this point. A locally bound callee would be out of scope where the residual installs.
            Outcome::Bail(Bail::Effect) if tail => match callee {
                Atom::Function(function) if self.item_functions.contains(&function) => {
                    Outcome::Stuck(Residual::Call(function, args))
                }
                _ => Outcome::Bail(Bail::Effect),
            },
            other => other,
        }
    }

    fn enter(&mut self, closure: &Closure, args: Vec<Value>, tail: bool) -> Outcome {
        let Some(function) = self.module.function(closure.function) else {
            return Outcome::Bail(Bail::Unknown);
        };
        if function.params.len() != args.len() {
            return Outcome::Bail(Bail::Arity);
        }
        if let Err(bail) = self.budget.enter() {
            return Outcome::Bail(bail);
        }
        let mut frame = Frame {
            values: closure.env.borrow().clone(),
        };
        for (&param, arg) in function.params.iter().zip(args) {
            frame.push(param, arg);
        }
        let result = self.eval_block(function.body, &mut frame, tail);
        self.budget.leave();
        result
    }

    fn eval_operation(
        &mut self,
        operation: Operation,
        operands: &[Atom],
        frame: &mut Frame,
    ) -> Outcome {
        let operands = match self.eval_operands(operands, frame) {
            Ok(operands) => operands,
            Err(bail) => return Outcome::Bail(bail),
        };
        let Some(constants) = leaves(&operands) else {
            return Outcome::Bail(Bail::Unsupported);
        };
        fold(Semantics::fold_operation(operation, &constants))
    }

    fn eval_sequence(
        &mut self,
        operation: SequenceOp,
        operands: &[Atom],
        frame: &mut Frame,
    ) -> Outcome {
        let operands = match self.eval_operands(operands, frame) {
            Ok(operands) => operands,
            Err(bail) => return Outcome::Bail(bail),
        };
        use SequenceOp::*;
        match operation {
            ListLen | ListGet | ListSlice | ListAppend | ListConcat | ListBuild => {
                outcome(interpret_list(operation, &operands))
            }
            _ => {
                let Some(constants) = leaves(&operands) else {
                    return Outcome::Bail(Bail::Unsupported);
                };
                fold(Semantics::fold_sequence(operation, &constants))
            }
        }
    }

    fn eval_match(
        &mut self,
        scrutinee: Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        frame: &mut Frame,
        tail: bool,
    ) -> Outcome {
        let (constructor, payload) = match self.eval_atom(scrutinee, frame) {
            Ok(Value::Construct(constructor, payload)) => (constructor, payload),
            Ok(_) => return Outcome::Bail(Bail::Unsupported),
            Err(bail) => return Outcome::Bail(bail),
        };
        if let Some(arm) = arms.iter().find(|arm| arm.constructor == constructor) {
            if arm.bindings.len() != payload.len() {
                return Outcome::Bail(Bail::Arity);
            }
            let mark = frame.mark();
            for (&binder, held) in arm.bindings.iter().zip(payload.iter()) {
                frame.push(binder, held.clone());
            }
            let result = self.eval_block(arm.block, frame, tail);
            frame.restore(mark);
            result
        } else if let Some(default) = default {
            self.eval_block(default, frame, tail)
        } else {
            Outcome::Bail(Bail::Unsupported)
        }
    }

    fn eval_fold_nat(
        &mut self,
        scrutinee: Atom,
        zero: BlockId,
        step: &FoldNatStep,
        frame: &mut Frame,
    ) -> Outcome {
        let bound = match self.eval_atom(scrutinee, frame).and_then(|held| held.nat()) {
            Ok(bound) => bound,
            Err(bail) => return Outcome::Bail(bail),
        };
        let mut accumulator = match self.value_of_block(zero, frame) {
            Ok(held) => held,
            Err(bail) => return Outcome::Bail(bail),
        };
        for predecessor in 0..bound {
            if let Err(bail) = self.budget.charge() {
                return Outcome::Bail(bail);
            }
            let mark = frame.mark();
            frame.push(step.predecessor, Value::Nat(predecessor));
            frame.push(step.hypothesis, accumulator);
            let result = self.value_of_block(step.block, frame);
            frame.restore(mark);
            match result {
                Ok(held) => accumulator = held,
                Err(bail) => return Outcome::Bail(bail),
            }
        }
        Outcome::Done(accumulator)
    }

    fn eval_fold_sequence(
        &mut self,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        step: &FoldSequenceStep,
        frame: &mut Frame,
    ) -> Outcome {
        let sequence = match self.eval_atom(scrutinee, frame) {
            Ok(sequence) => sequence,
            Err(bail) => return Outcome::Bail(bail),
        };
        let elements = match fold_elements(grain, &sequence) {
            Ok(elements) => elements,
            Err(bail) => return Outcome::Bail(bail),
        };
        let mut accumulator = match self.value_of_block(empty, frame) {
            Ok(held) => held,
            Err(bail) => return Outcome::Bail(bail),
        };
        // A right fold in erasure order: the last element folds first, seeing the empty suffix; each earlier element sees the suffix after it.
        for cut in (0..elements.len()).rev() {
            if let Err(bail) = self.budget.charge() {
                return Outcome::Bail(bail);
            }
            let element = elements[cut].clone();
            let suffix = suffix_view(grain, &elements[cut + 1..]);
            let mark = frame.mark();
            frame.push(step.element, element);
            frame.push(step.suffix, suffix);
            frame.push(step.accumulator, accumulator);
            let result = self.value_of_block(step.block, frame);
            frame.restore(mark);
            match result {
                Ok(held) => accumulator = held,
                Err(bail) => return Outcome::Bail(bail),
            }
        }
        Outcome::Done(accumulator)
    }

    /// One peel, evaluated: the empty block on an empty sequence, else the cons block over the head and the suffix after it. The non-looping sibling of [`Interpreter::eval_fold_sequence`], sharing its element view and its suffix view so the two cannot disagree about what a peel exposes.
    fn eval_uncons_sequence(
        &mut self,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: &UnconsSequenceStep,
        frame: &mut Frame,
    ) -> Outcome {
        let sequence = match self.eval_atom(scrutinee, frame) {
            Ok(sequence) => sequence,
            Err(bail) => return Outcome::Bail(bail),
        };
        let elements = match fold_elements(grain, &sequence) {
            Ok(elements) => elements,
            Err(bail) => return Outcome::Bail(bail),
        };

        let Some((element, rest)) = elements.split_first() else {
            return match self.value_of_block(empty, frame) {
                Ok(held) => Outcome::Done(held),
                Err(bail) => Outcome::Bail(bail),
            };
        };

        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }

        let mark = frame.mark();
        frame.push(cons.element, element.clone());
        frame.push(cons.suffix, suffix_view(grain, rest));
        let result = self.value_of_block(cons.block, frame);
        frame.restore(mark);

        match result {
            Ok(held) => Outcome::Done(held),
            Err(bail) => Outcome::Bail(bail),
        }
    }

    fn eval_map(&mut self, operands: &[Atom], frame: &mut Frame) -> Outcome {
        // The intrinsic's operand order is the list first, then the mapper.
        let [source, mapper] = operands else {
            return Outcome::Bail(Bail::Arity);
        };
        let elements = match self.eval_atom(*source, frame) {
            Ok(Value::List(elements)) => elements,
            Ok(_) => return Outcome::Bail(Bail::Unsupported),
            Err(bail) => return Outcome::Bail(bail),
        };
        let mapper = match self.eval_atom(*mapper, frame) {
            Ok(Value::Closure(closure)) => closure,
            Ok(_) => return Outcome::Bail(Bail::Unsupported),
            Err(bail) => return Outcome::Bail(bail),
        };
        let mut mapped = Vec::with_capacity(elements.len());
        for element in elements.iter() {
            if let Err(bail) = self.budget.charge() {
                return Outcome::Bail(bail);
            }
            match self.enter(&mapper, vec![element.clone()], false) {
                Outcome::Done(held) => mapped.push(held),
                Outcome::Stuck(_) => unreachable!("non-tail evaluation never residualizes"),
                Outcome::Bail(bail) => return Outcome::Bail(bail),
            }
        }
        Outcome::Done(Value::List(Rc::new(mapped)))
    }

    fn eval_operands(&mut self, atoms: &[Atom], frame: &mut Frame) -> Result<Vec<Value>, Bail> {
        let mut values = Vec::with_capacity(atoms.len());
        for &atom in atoms {
            values.push(self.eval_atom(atom, frame)?);
        }
        Ok(values)
    }
}

fn outcome(result: Result<Value, Bail>) -> Outcome {
    match result {
        Ok(held) => Outcome::Done(held),
        Err(bail) => Outcome::Bail(bail),
    }
}

/// A value folds, a known trap bails so the runtime trap survives, and a lack of knowledge bails.
fn fold(outcome: FoldOutcome) -> Outcome {
    match outcome {
        FoldOutcome::Value(constant) => Outcome::Done(Value::from_constant(&constant)),
        FoldOutcome::WouldTrap(_) => Outcome::Bail(Bail::Trap),
        FoldOutcome::Unknown => Outcome::Bail(Bail::Unsupported),
    }
}

fn leaves(values: &[Value]) -> Option<Vec<Constant>> {
    values.iter().map(Value::as_constant).collect()
}

/// The elements of a sequence value as the fold binds them: a list's own elements, or a packed binary's grains — byte grain as `Byte`, bit grain as `Bool`.
fn fold_elements(grain: SequenceGrain, sequence: &Value) -> Result<Vec<Value>, Bail> {
    match (grain, sequence) {
        (SequenceGrain::List, Value::List(elements)) => Ok(elements.as_ref().clone()),
        (SequenceGrain::Bin(expected), Value::Bin(found, packed)) if *found == expected => {
            let length = packed.len(expected);
            let mut elements = Vec::with_capacity(length);
            for index in 0..length {
                let element = match expected {
                    Grain::X => packed.byte(index).map(Value::Byte),
                    Grain::B => packed.bit(index).map(Value::Bool),
                };
                match element {
                    Some(value) => elements.push(value),
                    None => return Err(Bail::Unsupported),
                }
            }
            Ok(elements)
        }
        _ => Err(Bail::Unsupported),
    }
}

fn interpret_list(operation: SequenceOp, operands: &[Value]) -> Result<Value, Bail> {
    use SequenceOp::*;
    let list = |index: usize| match operands.get(index) {
        Some(Value::List(elements)) => Ok(elements.clone()),
        _ => Err(Bail::Unsupported),
    };
    let index = |position: usize| match operands.get(position) {
        Some(Value::Nat(value)) => Ok(*value as usize),
        _ => Err(Bail::Unsupported),
    };
    match operation {
        ListBuild => Ok(Value::List(Rc::new(operands.to_vec()))),
        ListLen => Ok(Value::Nat(list(0)?.len() as u32)),
        ListGet => match list(0)?.get(index(1)?) {
            Some(element) => Ok(element.clone()),
            None => Err(Bail::Trap),
        },
        // A window is `(start, length)`, so there is no reversed range left to reject — only one that runs past the end.
        ListSlice => {
            let elements = list(0)?;
            let (start, count) = (index(1)?, index(2)?);
            match start
                .checked_add(count)
                .filter(|end| *end <= elements.len())
            {
                Some(end) => Ok(Value::List(Rc::new(elements[start..end].to_vec()))),
                None => Err(Bail::Trap),
            }
        }
        ListAppend => {
            let mut elements = list(0)?.as_ref().clone();
            match operands.get(1) {
                Some(element) => elements.push(element.clone()),
                None => return Err(Bail::Arity),
            }
            Ok(Value::List(Rc::new(elements)))
        }
        ListConcat => {
            let mut elements = Vec::new();
            for position in 0..operands.len() {
                elements.extend(list(position)?.iter().cloned());
            }
            Ok(Value::List(Rc::new(elements)))
        }
        BinLen(_) | BinGet(_) | BinSlice(_) | BinAppend(_) | BinConcat(_) | BinEql(_) => {
            unreachable!("packed-binary operations fold through the semantic contract")
        }
    }
}

/// The suffix a fold's step binder sees: the remaining list, or the remaining grains rebuilt into a binary.
fn suffix_view(grain: SequenceGrain, remainder: &[Value]) -> Value {
    match grain {
        SequenceGrain::List => Value::List(Rc::new(remainder.to_vec())),
        SequenceGrain::Bin(grain) => {
            let mut packed = PackedBin::from_bytes(Vec::new());
            for value in remainder {
                packed = match (grain, value) {
                    (Grain::X, Value::Byte(byte)) => packed
                        .append_byte(*byte)
                        .expect("a byte-built binary stays byte-aligned"),
                    (Grain::B, Value::Bool(bit)) => packed.append_bit(*bit),
                    _ => unreachable!("fold elements match their sequence grain"),
                };
            }
            Value::Bin(grain, Rc::new(packed))
        }
    }
}
