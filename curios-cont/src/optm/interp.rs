//! Interpreter for the partial evaluator: runs a pure body against a frame of
//! [`Snapshot`] values, sharing leaf semantics with `constant_folding` via
//! `scalar_eval` (the frame is the [`EvalEnv`]) so the trap and host-boundary set is
//! identical between compile-time folding and compile-time interpretation.

use {
    super::*,
    std::{
        cell::RefCell,
        collections::{HashMap, HashSet},
        rc::Rc,
    },
};

/// Per top-level call site the interpreter is allowed to take at most this many
/// tail transitions before bailing — bounds runtime and rules out pathological
/// non-terminating pure callees. Picked to cover the OPTM.md §2 target
/// (`parse_fmt("{} is {}", …)` ≈ ~hundreds of steps) with headroom; a runaway
/// aborts as `Outcome::GaveUp`, leaving the original call intact.
const STEP_BUDGET: usize = 10_000;

/// Cap on how deep `Direct`/`Indirect` calls can recurse during interpretation.
/// Bounds the host Rust stack independently of `STEP_BUDGET` — a deeply but
/// finitely recursive pure callee that fits the budget would still blow the
/// stack without this. Picked well below the default 2 MB test-thread stack:
/// each `run_body` frame is on the order of a few hundred bytes (a `Frame`
/// `HashMap` plus locals), so 256 leaves a wide safety margin.
const MAX_CALL_DEPTH: usize = 256;

/// Static handles the interpreter needs across every recursive call. The
/// funcs/clsrs maps are snapshots taken before the rewriter mutates anything,
/// so the interpreter can read freely while bodies are being edited in place.
pub(crate) struct Ctx<'a> {
    pub funcs: &'a HashMap<FuncName, Func>,
    pub clsrs: &'a HashMap<ClsrName, Clsr>,
    pub pure_funcs: &'a HashSet<FuncName>,
    pub pure_clsrs: &'a HashSet<ClsrName>,
}

/// The interpreter's runtime value. Distinct from `Data` because closure
/// captures must be **resolved** at construction time: a `Data::Clsr`'s
/// captures are `Vec<ValueName>` bound in the creator's frame, but a recursive
/// callee invocation pops out of that frame, so the names are no longer in
/// scope. Resolving the captures into a `Snapshot::Clsr` makes the closure
/// value self-contained — at the cost of a `RefCell` for the recursion case
/// (a `Prealloc::Clsr` placeholder whose same-named `Pure(Data::Clsr)` fill
/// captures the very name being filled, so the recursive reference must
/// observe the resolved captures *after* the fill).
#[derive(Clone)]
pub(crate) enum Snapshot {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Rc<Vec<u8>>),
    Lst(Rc<Vec<Snapshot>>),
    Tpl(Rc<Vec<Snapshot>>),
    Clsr(ClsrName, Rc<RefCell<Vec<Snapshot>>>),
}

pub(crate) type Frame = HashMap<ValueName, Snapshot>;

/// The frame is the interpreter's [`EvalEnv`]: scalar lookups read snapshots, and an
/// aggregate's elements are themselves snapshots — so the shared evaluator's
/// projections and `Lst` builders work directly on runtime values, with no
/// projection of the frame into a name-keyed literal map.
impl EvalEnv for Frame {
    type Elem = Snapshot;

    fn nat(&self, name: &ValueName) -> Option<u32> {
        match self.get(name)? {
            Snapshot::Nat(value) => Some(*value),
            _ => None,
        }
    }

    fn int(&self, name: &ValueName) -> Option<i32> {
        match self.get(name)? {
            Snapshot::Int(value) => Some(*value),
            _ => None,
        }
    }

    fn flt(&self, name: &ValueName) -> Option<f32> {
        match self.get(name)? {
            Snapshot::Flt(value) => Some(*value),
            _ => None,
        }
    }

    fn bin(&self, name: &ValueName) -> Option<&[u8]> {
        match self.get(name)? {
            Snapshot::Bin(bytes) => Some(bytes),
            _ => None,
        }
    }

    fn lst(&self, name: &ValueName) -> Option<&[Snapshot]> {
        match self.get(name)? {
            Snapshot::Lst(elems) => Some(elems),
            _ => None,
        }
    }

    fn tpl(&self, name: &ValueName) -> Option<&[Snapshot]> {
        match self.get(name)? {
            Snapshot::Tpl(elems) => Some(elems),
            _ => None,
        }
    }

    fn elem(&self, name: &ValueName) -> Option<Snapshot> {
        self.get(name).cloned()
    }

    fn scalar(&self, elem: &Snapshot) -> Option<Scalar> {
        match elem {
            Snapshot::Nat(value) => Some(Scalar::Nat(*value)),
            Snapshot::Int(value) => Some(Scalar::Int(*value)),
            Snapshot::Flt(value) => Some(Scalar::Flt(*value)),
            _ => None,
        }
    }
}

pub(crate) enum Outcome {
    Returned(Snapshot),
    GaveUp,
}

pub(crate) struct Interp<'a> {
    ctx: &'a Ctx<'a>,
    /// Tail transitions remaining across the whole top-level interpretation —
    /// shared between the iterative loop and every recursive `run_body`.
    budget: usize,
    /// Calls remaining before the Rust stack would blow. Bracketed in
    /// `run_body` so it tracks real `Direct`/`Indirect` nesting.
    call_depth: usize,
}

impl<'a> Interp<'a> {
    pub(crate) fn new(ctx: &'a Ctx<'a>) -> Self {
        Self {
            ctx,
            budget: STEP_BUDGET,
            call_depth: MAX_CALL_DEPTH,
        }
    }

    /// Interpret `region` as the body of a callee. The within-body Jump/Match/
    /// Call-resume traffic is dispatched **iteratively** in a single loop — only
    /// `Direct`/`Indirect` calls recurse into Rust frames (one per actual
    /// function invocation), so the host stack tracks logical call depth rather
    /// than total tail transitions.
    pub(crate) fn run_body(
        &mut self,
        region: &Region,
        body_resume: &BlockName,
        mut frame: Frame,
    ) -> Outcome {
        if self.call_depth == 0 {
            return Outcome::GaveUp;
        }
        self.call_depth -= 1;
        let result = self.run_body_inner(region, body_resume, &mut frame);
        self.call_depth += 1;
        result
    }

    fn run_body_inner(
        &mut self,
        region: &Region,
        body_resume: &BlockName,
        frame: &mut Frame,
    ) -> Outcome {
        let mut blocks: HashMap<BlockName, &Block> = HashMap::new();
        index_blocks(region, &mut blocks);

        let mut current: &Region = region;
        loop {
            if let Err(outcome) = self.bind_preallocs_and_values(current, frame) {
                return outcome;
            }

            if self.budget == 0 {
                return Outcome::GaveUp;
            }
            self.budget -= 1;

            // `dispatch_tail` either yields the next (target, args) to thread
            // into a block lookup, or aborts the whole interpretation.
            let next = match self.dispatch_tail(&current.tail, frame) {
                Ok(next) => next,
                Err(outcome) => return outcome,
            };

            // Resolve the next region within the same body, returning out of
            // it when control reaches the resume sentinel.
            let (target, args) = next;
            if &target == body_resume {
                return match args.into_iter().next() {
                    Some(snap) => Outcome::Returned(snap),
                    None => Outcome::GaveUp,
                };
            }
            let Some(block) = blocks.get(&target) else {
                return Outcome::GaveUp;
            };
            if block.params.len() != args.len() {
                return Outcome::GaveUp;
            }
            for (param, arg) in block.params.iter().zip(args) {
                frame.insert(param.clone(), arg);
            }
            current = &block.region;
        }
    }

    /// Bind preallocs (placeholder snapshots) and walk values into the frame.
    /// Returns `Err(GaveUp)` on the first irreducible value.
    fn bind_preallocs_and_values(
        &mut self,
        region: &Region,
        frame: &mut Frame,
    ) -> Result<(), Outcome> {
        // A prealloc backs a recursive closure: the shell is the placeholder whose
        // same-named `Pure(Data::Clsr)` fill mutates the `RefCell` in place, so a self-
        // referential capture observes the resolved content after the fill. Only closures
        // are prealloc'd (cyclic tuples/arrays are rejected upstream in `to_cont`).
        for (name, clsr) in &region.preallocs {
            frame.insert(
                name.clone(),
                Snapshot::Clsr(clsr.clone(), Rc::new(RefCell::new(Vec::new()))),
            );
        }

        for (name, value) in &region.values {
            // The `Pure(Data::Clsr)` fill is the one case that mutates a
            // placeholder rather than replacing the binding — anything else is
            // a straight `eval_value` then `insert`.
            if let Value::Pure(Data::Clsr(c, capture_names)) = value {
                let Some(captures) = resolve_names(capture_names, frame) else {
                    return Err(Outcome::GaveUp);
                };
                if let Some(Snapshot::Clsr(existing, slot)) = frame.get(name) {
                    debug_assert_eq!(existing, c);
                    let mut content = slot.borrow_mut();
                    content.clear();
                    content.extend(captures);
                } else {
                    frame.insert(
                        name.clone(),
                        Snapshot::Clsr(c.clone(), Rc::new(RefCell::new(captures))),
                    );
                }
                continue;
            }

            match self.eval_value(value, frame) {
                Some(snap) => {
                    frame.insert(name.clone(), snap);
                }
                None => return Err(Outcome::GaveUp),
            }
        }
        Ok(())
    }

    /// Evaluate a non-Clsr `Value` against the frame, returning `None` when an
    /// operand is unresolved or a primitive would trap.
    fn eval_value(&mut self, value: &Value, frame: &Frame) -> Option<Snapshot> {
        match value {
            Value::Alias(source) => frame.get(source).cloned(),
            Value::Pure(data) => materialise_data(data, frame),
            // Delegate to `scalar_eval` — the same wasm-faithful arithmetic and
            // aggregate logic the constant folder uses, so traps line up. The
            // frame is the environment directly: an aggregate's elements are
            // snapshots, so projections and `Lst` builders work on any element,
            // not just scalars.
            Value::Eval(code) => Some(match simplify(code, frame)? {
                Evaluated::Scalar(scalar) => scalar.snapshot(),
                Evaluated::Lst(elems) => Snapshot::Lst(Rc::new(elems)),
                Evaluated::Elem(snap) => snap,
            }),
        }
    }

    /// Reduce a region's tail to the next `(block_target, args)` to continue
    /// at, or abort. `Direct`/`Indirect` calls recurse into `run_body` here —
    /// every other tail just produces a within-body jump for the outer loop.
    fn dispatch_tail(
        &mut self,
        tail: &Tail,
        frame: &Frame,
    ) -> Result<(BlockName, Vec<Snapshot>), Outcome> {
        match tail {
            Tail::Jump(JumpTarget { target, params }) => {
                let args = resolve_names(params, frame).ok_or(Outcome::GaveUp)?;
                Ok((target.clone(), args))
            }
            Tail::Match(MatchTarget {
                operand,
                cases,
                default,
            }) => {
                let tag = match frame.get(operand) {
                    Some(Snapshot::Nat(t)) => *t,
                    _ => return Err(Outcome::GaveUp),
                };
                let jump = cases
                    .get(&tag)
                    .or(default.as_ref())
                    .ok_or(Outcome::GaveUp)?;
                let args = resolve_names(&jump.params, frame).ok_or(Outcome::GaveUp)?;
                Ok((jump.target.clone(), args))
            }
            Tail::Call(CallTarget::Direct {
                target,
                params,
                resume,
            }) => {
                if !self.ctx.pure_funcs.contains(target) {
                    return Err(Outcome::GaveUp);
                }
                let args = resolve_names(params, frame).ok_or(Outcome::GaveUp)?;
                let callee = self.ctx.funcs.get(target).ok_or(Outcome::GaveUp)?;
                let callee_frame = seed_frame(&callee.params, args).ok_or(Outcome::GaveUp)?;
                let result = self.run_body(&callee.region, &callee.resume, callee_frame);
                let Outcome::Returned(snap) = result else {
                    return Err(Outcome::GaveUp);
                };
                Ok((resume.clone(), vec![snap]))
            }
            Tail::Call(CallTarget::Indirect {
                target,
                params,
                resume,
            }) => {
                let (clsr_name, captures_rc) = match frame.get(target) {
                    Some(Snapshot::Clsr(name, captures)) => (name.clone(), captures.clone()),
                    _ => return Err(Outcome::GaveUp),
                };
                if !self.ctx.pure_clsrs.contains(&clsr_name) {
                    return Err(Outcome::GaveUp);
                }
                let args = resolve_names(params, frame).ok_or(Outcome::GaveUp)?;
                let callee = self.ctx.clsrs.get(&clsr_name).ok_or(Outcome::GaveUp)?;
                let captures = captures_rc.borrow();
                if callee.fields.len() != captures.len() || callee.params.len() != args.len() {
                    return Err(Outcome::GaveUp);
                }
                let mut callee_frame: Frame = HashMap::new();
                for (field, cap) in callee.fields.iter().zip(captures.iter()) {
                    callee_frame.insert(field.name.clone(), cap.clone());
                }
                drop(captures);
                for (param, arg) in callee.params.iter().zip(args) {
                    callee_frame.insert(param.name.clone(), arg);
                }
                let result = self.run_body(&callee.region, &callee.resume, callee_frame);
                let Outcome::Returned(snap) = result else {
                    return Err(Outcome::GaveUp);
                };
                Ok((resume.clone(), vec![snap]))
            }
            Tail::Host(_) | Tail::Cell(_) | Tail::Unreachable => Err(Outcome::GaveUp),
        }
    }
}

fn index_blocks<'a>(region: &'a Region, index: &mut HashMap<BlockName, &'a Block>) {
    for (name, block) in &region.blocks {
        index.insert(name.clone(), block);
        index_blocks(&block.region, index);
    }
}

fn resolve_names(names: &[ValueName], frame: &Frame) -> Option<Vec<Snapshot>> {
    names.iter().map(|n| frame.get(n).cloned()).collect()
}

pub(crate) fn seed_frame(params: &[Argument], args: Vec<Snapshot>) -> Option<Frame> {
    (params.len() == args.len()).then(|| {
        params
            .iter()
            .map(|p| p.name.clone())
            .zip(args)
            .collect::<Frame>()
    })
}

/// Build a `Snapshot` from a `Data` literal by resolving any name references it
/// carries (aggregates, closure captures) against the frame. Scalars and `Bin`
/// are owned outright.
pub(crate) fn materialise_data(data: &Data, frame: &Frame) -> Option<Snapshot> {
    Some(match data {
        Data::Nat(n) => Snapshot::Nat(*n),
        Data::Int(i) => Snapshot::Int(*i),
        Data::Flt(f) => Snapshot::Flt(*f),
        Data::Bin(bytes) => Snapshot::Bin(Rc::new(bytes.clone())),
        Data::Lst(elems) => Snapshot::Lst(Rc::new(resolve_names(elems, frame)?)),
        Data::Tpl(elems) => Snapshot::Tpl(Rc::new(resolve_names(elems, frame)?)),
        Data::Clsr(c, captures) => Snapshot::Clsr(
            c.clone(),
            Rc::new(RefCell::new(resolve_names(captures, frame)?)),
        ),
    })
}
