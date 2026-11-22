//! The fueled big-step, call-by-value interpreter over the direct-style ANF.
//!
//! The interpreter walks blocks — statements then a terminator — so tail position, the only place an effect may residualize, is the statement whose result the block returns. ANF hoists every effect into a `Let`, so a `Foreign` feeding a block's `Return` is a tail-position effect and residualizes to a single host call — exactly the `Handle/write(<bytes>)` the `Fmt` collapse produces.
//!
//! **A call in its function body's tail takes its caller's place.** Such a call is handed back as a jump rather than entered, and the loop running the body runs the callee next in the same host frame, so a walk of any length costs one call's depth — the run-time meaning of a tail call, which is what makes a recursion written as one a loop. Only a call that waits on its answer takes a frame, and only such calls meet the depth cap; the steps every body walks are charged as ever.
//!
//! Closedness and scope: a candidate call is evaluated from an empty frame; every atom it transitively reads must resolve to a constant, a module function, or a top-level item value (a CAF forced on demand and memoized), never a runtime binder. A function reference closes on demand against the current frame. Scalar and packed-binary operations fold through [`Semantics`] — the single source of truth under the numeric law — while list operations, closures, and `ListMap` are interpreted directly.

use {
    super::{Bail, Budget, Closure, ListWindow, MAX_FOLD_BITS, MAX_FOLD_ELEMENTS, Value},
    crate::{
        Atom, BlockId, Constant, FoldOutcome, ForeignId, FreeValues, FunctionId, Intrinsic, Keys,
        Module, Operation, Rhs, Semantics, SequenceGrain, SequenceOp, Statement,
        SuspensionOperation, Terminator, UnconsSequenceStep, ValueId, VariantArm,
    },
    curios_num::Natural,
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
pub enum Outcome {
    Done(Value),
    /// A tail-position effect reached with fully-evaluated inputs.
    Stuck(Residual),
    Bail(Bail),
    /// A call in its function body's tail, handed to the loop running that body ([`Evaluator::enter`]) to take its caller's place, as a tail call does. It never leaves the function whose body made it, so no evaluation ends on one.
    Jump(Jump),
}

/// A call a function body hands on in its tail: what it calls, what with, and — where its callee is a function an item binds — that function, which an effect the call cannot leave for run time is left as a call of.
pub struct Jump {
    closure: Rc<Closure>,
    arguments: Vec<Value>,
    item: Option<FunctionId>,
}

/// Where an evaluation stands: in the candidate's tail, where an effect may be left for run time, and in its function body's tail, where a call takes the caller's place. A statement whose value its block does not return stands in neither.
#[derive(Clone, Copy)]
struct Position {
    candidate: bool,
    body: bool,
}

impl Position {
    /// Neither tail: a value something after it reads.
    const INNER: Self = Self {
        candidate: false,
        body: false,
    };
}

/// A residual installed in place of a folded candidate: a tail-position host call, or a tail call to a module function whose body performs an effect it cannot residualize itself.
pub enum Residual {
    Foreign(ForeignId, Vec<Value>),
    Call(FunctionId, Vec<Value>),
}

/// Where a forced CAF's value comes from: a top-level item `Let` right-hand side, or an eager recursive-group member's initializer block.
enum CafSource<'m> {
    Rhs(&'m Rhs),
    Block(BlockId),
}

/// The interpreter over a borrowed module.
pub struct Evaluator<'m> {
    module: &'m Module,
    /// What a function closes over, derived the first time it is closed rather than from a whole-module snapshot, which would be retaken every round while the module grows under reification; the handful actually closed are what this walks.
    free_values: FreeValues<'m>,
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
    /// Build an interpreter over a module. The top-level value bindings are the module's item chain.
    pub fn new(module: &'m Module) -> Self {
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
            free_values: FreeValues::new(module),
            definitions,
            rec_caf,
            item_functions,
            cache: BTreeMap::new(),
            forcing: BTreeSet::new(),
            budget: Budget::new(),
        }
    }

    /// Whether an atom names a closed value — a constant, a module function, or a top-level item value — so a call over such atoms is a candidate.
    pub fn is_closed_atom(&self, atom: Atom) -> bool {
        match atom {
            Atom::Constant(_) | Atom::Function(_) => true,
            Atom::Value(value) => {
                self.definitions.contains_key(&value) || self.rec_caf.contains_key(&value)
            }
        }
    }

    /// Evaluate one closed candidate call from a fresh budget frame.
    pub fn evaluate(&mut self, callee: Atom, arguments: &[Atom]) -> Outcome {
        self.budget.restart();
        let mut frame = Frame::new();
        // The candidate's own call stands in its tail, and in no body: nothing is running to hand it on to.
        let at = Position {
            candidate: true,
            body: false,
        };
        self.eval_apply(callee, arguments, &mut frame, at)
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
        for &free in self.free_values.of(function).iter() {
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
            CafSource::Rhs(rhs) => self.eval_rhs(rhs, &mut Frame::new(), Position::INNER),
            CafSource::Block(block) => self.eval_block(block, &mut Frame::new(), Position::INNER),
        };
        let result = match outcome {
            Outcome::Done(held) => Ok(held),
            Outcome::Bail(Bail::Fuel) => {
                self.forcing.remove(&value);
                return Err(Bail::Fuel);
            }
            Outcome::Stuck(_) | Outcome::Bail(_) => Err(Bail::Unknown),
            Outcome::Jump(_) => unreachable!("a value forced in no body's tail makes no jump"),
        };
        self.forcing.remove(&value);
        self.cache.insert(value, result.clone().ok());
        result
    }

    /// Evaluate a block: statements in order, then the terminator. The last statement — when it defines the returned value — stands where the block does, so a `Foreign` there residualizes in the candidate's tail and a call there is a jump in its body's.
    fn eval_block(&mut self, block: BlockId, frame: &mut Frame, at: Position) -> Outcome {
        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }
        let module = self.module;
        let Some(block) = module.block(block) else {
            return Outcome::Bail(Bail::Unknown);
        };
        let returned = match &block.terminator {
            Terminator::Return(atom) => *atom,
            // A diverging host call is an effect the candidate binding cannot become; the tail-call boundary converts it into a residual call instead.
            Terminator::Halt { .. } => return Outcome::Bail(Bail::Effect),
            Terminator::Unreachable => return Outcome::Bail(Bail::Trap),
        };

        let mark = frame.mark();
        let last = block.statements.len().checked_sub(1);
        for (index, &statement) in block.statements.iter().enumerate() {
            match module.statement(statement) {
                Some(Statement::Let { result, rhs }) => {
                    let returns = Some(index) == last && returned == Atom::Value(*result);
                    let inner = match returns {
                        true => at,
                        false => Position::INNER,
                    };
                    match self.eval_rhs(rhs, frame, inner) {
                        Outcome::Done(held) => {
                            if returns {
                                frame.restore(mark);
                                return Outcome::Done(held);
                            }
                            frame.push(*result, held);
                        }
                        outcome @ (Outcome::Stuck(_) | Outcome::Bail(_) | Outcome::Jump(_)) => {
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

    fn eval_rhs(&mut self, rhs: &Rhs, frame: &mut Frame, at: Position) -> Outcome {
        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }
        match rhs {
            Rhs::Alias(atom) => outcome(self.eval_atom(*atom, frame)),
            Rhs::Apply { callee, arguments } => self.eval_apply(*callee, arguments, frame, at),
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
            } => self.eval_match(*scrutinee, arms, *default, frame, at),
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => match self
                .eval_atom(*scrutinee, frame)
                .and_then(|held| held.bool_())
            {
                Ok(chosen) => self.eval_block(if chosen { *if_true } else { *if_false }, frame, at),
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::Switch {
                scrutinee,
                keys,
                default,
            } => match (self.eval_atom(*scrutinee, frame), keys) {
                (Ok(Value::Nat(key)), Keys::Nat(cases)) => {
                    let block = cases
                        .iter()
                        .find(|case| case.key == key)
                        .map_or(*default, |case| case.block);
                    self.eval_block(block, frame, at)
                }
                (Ok(Value::Int(key)), Keys::Int(cases)) => {
                    let block = cases
                        .iter()
                        .find(|case| case.key == key)
                        .map_or(*default, |case| case.block);
                    self.eval_block(block, frame, at)
                }
                (Ok(_), _) => Outcome::Bail(Bail::Unsupported),
                (Err(bail), _) => Outcome::Bail(bail),
            },
            Rhs::UnconsSequence {
                grain,
                scrutinee,
                empty,
                cons,
            } => self.eval_uncons_sequence(*grain, *scrutinee, *empty, cons, frame, at),
            // A host call is an effect: it residualizes in tail position with evaluated operands, and bails everywhere else.
            Rhs::Foreign { foreign, operands } => match self.eval_operands(operands, frame) {
                Ok(operands) if at.candidate => {
                    Outcome::Stuck(Residual::Foreign(*foreign, operands))
                }
                Ok(_) => Outcome::Bail(Bail::Effect),
                Err(bail) => Outcome::Bail(bail),
            },
            // A cell operation's identity is its program point: never residualized, never folded.
            Rhs::Cell { .. } | Rhs::Box { .. } => Outcome::Bail(Bail::Effect),
            // Building a suspension is a wait, which only run time performs; so no value held here is one, and every test answers no.
            Rhs::Suspension {
                operation: SuspensionOperation::Is,
                operands,
            } => match self.eval_operands(operands, frame) {
                Ok(_) => Outcome::Done(Value::Bool(false)),
                Err(bail) => Outcome::Bail(bail),
            },
            Rhs::Suspension { .. } => Outcome::Bail(Bail::Effect),
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
        at: Position,
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
        // A residual call names an *item-bound* function — in scope at every candidate site — and never a locally bound one, which would be out of scope where the residual installs.
        let item = match callee {
            Atom::Function(function) if self.item_functions.contains(&function) => Some(function),
            _ => None,
        };

        if at.body {
            return Outcome::Jump(Jump {
                closure,
                arguments: args,
                item,
            });
        }
        match self.enter(&closure, args.clone(), at.candidate) {
            // Boundary conversion: the callee performs an effect it cannot residualize itself, but this call is the candidate's tail and names an item-bound function, so the call with its evaluated arguments is the residual, rerunning the callee in full at this point.
            Outcome::Bail(Bail::Effect) if at.candidate => match item {
                Some(function) => Outcome::Stuck(Residual::Call(function, args)),
                None => Outcome::Bail(Bail::Effect),
            },
            other => other,
        }
    }

    /// Run `closure` on `args`, at the depth of one call however many calls its body hands on in its tail: each takes its caller's place in this loop rather than a frame of its own (`MAX_CALL_DEPTH`), while every block each body walks still charges the step budget. In the candidate's tail, an effect a function the loop reached cannot leave for run time is left as a call of the last item-bound function a jump reached, with what it was handed — the site the boundary conversion in [`Evaluator::eval_apply`] would have been at had each jump been entered.
    fn enter(&mut self, closure: &Closure, args: Vec<Value>, candidate: bool) -> Outcome {
        if let Err(bail) = self.budget.enter() {
            return Outcome::Bail(bail);
        }
        let module = self.module;
        let mut function_id = closure.function;
        let mut env = closure.env.borrow().clone();
        let mut args = args;
        let mut reached: Option<(FunctionId, Vec<Value>)> = None;
        let outcome = loop {
            let Some(function) = module.function(function_id) else {
                break Outcome::Bail(Bail::Unknown);
            };
            if function.params.len() != args.len() {
                break Outcome::Bail(Bail::Arity);
            }
            let mut frame = Frame { values: env };
            for (&param, arg) in function.params.iter().zip(args) {
                frame.push(param, arg);
            }
            let at = Position {
                candidate,
                body: true,
            };
            match self.eval_block(function.body, &mut frame, at) {
                Outcome::Jump(jump) => {
                    if let Some(item) = jump.item {
                        reached = Some((item, jump.arguments.clone()));
                    }
                    function_id = jump.closure.function;
                    env = jump.closure.env.borrow().clone();
                    args = jump.arguments;
                }
                Outcome::Bail(Bail::Effect) if candidate => {
                    break match reached {
                        Some((function, args)) => Outcome::Stuck(Residual::Call(function, args)),
                        None => Outcome::Bail(Bail::Effect),
                    };
                }
                other => break other,
            }
        };
        self.budget.leave();
        outcome
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
        fold(Semantics::fold_operation(
            operation,
            &constants,
            MAX_FOLD_BITS,
        ))
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
        match operation {
            SequenceOp::ListLen
            | SequenceOp::ListGet
            | SequenceOp::ListSlice
            | SequenceOp::ListSet
            | SequenceOp::ListAppend
            | SequenceOp::ListConcat
            | SequenceOp::ListBuild => outcome(interpret_list(operation, &operands)),
            _ => {
                let Some(constants) = leaves(&operands) else {
                    return Outcome::Bail(Bail::Unsupported);
                };
                fold(Semantics::fold_sequence(
                    operation,
                    &constants,
                    MAX_FOLD_BITS,
                ))
            }
        }
    }

    fn eval_match(
        &mut self,
        scrutinee: Atom,
        arms: &[VariantArm],
        default: Option<BlockId>,
        frame: &mut Frame,
        at: Position,
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
            let result = self.eval_block(arm.block, frame, at);
            frame.restore(mark);
            result
        } else if let Some(default) = default {
            self.eval_block(default, frame, at)
        } else {
            Outcome::Bail(Bail::Unsupported)
        }
    }

    /// One peel, evaluated: the empty block on an empty sequence, else the cons block over the head and the suffix after it.
    fn eval_uncons_sequence(
        &mut self,
        grain: SequenceGrain,
        scrutinee: Atom,
        empty: BlockId,
        cons: &UnconsSequenceStep,
        frame: &mut Frame,
        at: Position,
    ) -> Outcome {
        let sequence = match self.eval_atom(scrutinee, frame) {
            Ok(sequence) => sequence,
            Err(bail) => return Outcome::Bail(bail),
        };
        let peeled = match peel(grain, &sequence) {
            Ok(peeled) => peeled,
            Err(bail) => return Outcome::Bail(bail),
        };

        // Each arm stands where the peel does, as a dispatch's arms do, so a walk over a sequence that hands its suffix on in its tail is a loop here as it is at run time.
        let Some((element, suffix)) = peeled else {
            return self.eval_block(empty, frame, at);
        };

        if let Err(bail) = self.budget.charge() {
            return Outcome::Bail(bail);
        }

        let mark = frame.mark();
        frame.push(cons.element, element);
        frame.push(cons.suffix, suffix);
        let result = self.eval_block(cons.block, frame, at);
        frame.restore(mark);
        result
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
                Outcome::Jump(_) => unreachable!("an entered call makes no jump of its own"),
                Outcome::Bail(bail) => return Outcome::Bail(bail),
            }
        }
        Outcome::Done(Value::List(ListWindow::new(mapped)))
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

/// One peel of a sequence: its head and the suffix after it, or `None` where it is empty. A `Bytes` peels a `Byte`.
///
/// Read off the original value the way the door's `Emitter::emit_peel` reads it at runtime — the head by index and the suffix as a window sharing the allocation — so a peel costs the same whether three elements or a hundred thousand follow it. Rebuilding each suffix element by element would make a recursive byte walk quadratic.
fn peel(grain: SequenceGrain, sequence: &Value) -> Result<Option<(Value, Value)>, Bail> {
    match (grain, sequence) {
        (SequenceGrain::List, Value::List(elements)) => {
            let Some(element) = elements.first() else {
                return Ok(None);
            };
            let suffix = elements
                .window(1, elements.len() - 1)
                .expect("the suffix after an element is within its list");
            Ok(Some((element.clone(), Value::List(suffix))))
        }
        (SequenceGrain::Bytes, Value::Bytes(packed)) => {
            let length = packed.len();
            if length == 0 {
                return Ok(None);
            }
            let element = packed.byte(0).map(Value::Byte);
            let suffix = packed.slice(1, length);
            match (element, suffix) {
                (Some(element), Some(suffix)) => Ok(Some((element, Value::Bytes(Rc::new(suffix))))),
                _ => Err(Bail::Unsupported),
            }
        }
        _ => Err(Bail::Unsupported),
    }
}

/// Decline a list of `length` elements past [`MAX_FOLD_ELEMENTS`], before it is built.
fn list_within(length: Option<usize>) -> Result<(), Bail> {
    match length.is_some_and(|length| length <= MAX_FOLD_ELEMENTS) {
        true => Ok(()),
        false => Err(Bail::Unsupported),
    }
}

fn interpret_list(operation: SequenceOp, operands: &[Value]) -> Result<Value, Bail> {
    let list = |index: usize| match operands.get(index) {
        Some(Value::List(elements)) => Ok(elements.clone()),
        _ => Err(Bail::Unsupported),
    };
    let index = |position: usize| match operands.get(position) {
        Some(Value::Nat(value)) => usize::try_from(value).ok().ok_or(Bail::Unsupported),
        _ => Err(Bail::Unsupported),
    };
    match operation {
        SequenceOp::ListBuild => Ok(Value::List(ListWindow::new(operands.to_vec()))),
        SequenceOp::ListLen => Ok(Value::Nat(Natural::from(list(0)?.len()))),
        SequenceOp::ListGet => match list(0)?.get(index(1)?) {
            Some(element) => Ok(element.clone()),
            None => Err(Bail::Trap),
        },
        // A window is `(start, length)`, so there is no reversed range left to reject — only one that runs past the end.
        SequenceOp::ListSlice => match list(0)?.window(index(1)?, index(2)?) {
            Some(window) => Ok(Value::List(window)),
            None => Err(Bail::Trap),
        },
        // An index past the end has no element to write, as it has none to read.
        SequenceOp::ListSet => {
            let at = index(1)?;
            let mut elements = list(0)?.to_vec();
            match (elements.get_mut(at), operands.get(2)) {
                (Some(slot), Some(element)) => *slot = element.clone(),
                (None, Some(_)) => return Err(Bail::Trap),
                (_, None) => return Err(Bail::Arity),
            }
            Ok(Value::List(ListWindow::new(elements)))
        }
        SequenceOp::ListAppend => {
            let list = list(0)?;
            list_within(list.len().checked_add(1))?;
            let mut elements = list.to_vec();
            match operands.get(1) {
                Some(element) => elements.push(element.clone()),
                None => return Err(Bail::Arity),
            }
            Ok(Value::List(ListWindow::new(elements)))
        }
        SequenceOp::ListConcat => {
            let lists = (0..operands.len())
                .map(list)
                .collect::<Result<Vec<_>, _>>()?;
            list_within(
                lists
                    .iter()
                    .try_fold(0usize, |length, list| length.checked_add(list.len())),
            )?;
            let mut elements = Vec::new();
            for list in &lists {
                elements.extend(list.iter().cloned());
            }
            Ok(Value::List(ListWindow::new(elements)))
        }
        SequenceOp::BytesLen
        | SequenceOp::BytesGet
        | SequenceOp::BytesSlice
        | SequenceOp::BytesAppend
        | SequenceOp::BytesConcat
        | SequenceOp::BytesEql
        | SequenceOp::BytesReplicate
        | SequenceOp::BytesAnd
        | SequenceOp::BytesOr
        | SequenceOp::BytesXor => {
            unreachable!("packed-binary operations fold through the semantic contract")
        }
    }
}
