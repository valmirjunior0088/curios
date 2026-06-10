//! CPS-level partial evaluator for pure-callee call sites.
//!
//! `constant_folding` reduces `Value::Eval(code)` against literals in the *same*
//! region; `inline_calls` is single-call-site, so it cannot reach a recursive
//! callee — which is what the parser combinator in `examples/crs_printf.rs`
//! collapses to after early DCE and inlining. This pass closes that gap: when a
//! `Direct` call's target is **pure** (no `Io`/`*ToStr`, no `Indirect` calls, no
//! impure callees, transitively) and every argument is a literal, the call is
//! interpreted at compile time and replaced by the materialised result plus a
//! `Jump` to the original resume.
//!
//! The structure mirrors the spec in OPTM.md §2:
//!
//! 1. A **purity classifier** computes the fixed-point set of pure funcs/clsrs.
//! 2. An **interpreter** runs a pure body against a frame of `Snapshot` values,
//!    sharing leaf semantics with `constant_folding` via `scalar_eval` so the
//!    trap and host-boundary set is identical between the two passes.
//! 3. A **rewriter** finds eligible `Direct`/`Indirect` call sites in every
//!    region tree, runs the interpreter, and materialises the result back into
//!    the host region as fresh `Pure(Data)` bindings plus a `Jump` to the
//!    original resume continuation.

use {
    super::*,
    crate::Entropy,
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

/// Rewrite every `Direct` call to a pure target with literal arguments into the
/// materialised result of compile-time interpretation, plus a `Jump` to the
/// original resume continuation. Indirect calls through a known pure-closure
/// value are handled symmetrically.
pub fn evaluate_pure_calls(module: &mut Module) {
    let (pure_funcs, pure_clsrs) = purity(module);

    // Snapshot the callable population so the interpreter can keep reading bodies
    // while the rewriter mutates host tails in place. Cheap (one `clone` per
    // func/clsr) compared to the alternative of plumbing a separate index across
    // every mutable borrow on the rewrite path.
    let funcs: HashMap<FuncName, Func> = module
        .funcs()
        .iter()
        .map(|(n, f)| (n.clone(), f.clone()))
        .collect();
    let clsrs: HashMap<ClsrName, Clsr> = module
        .clsrs()
        .iter()
        .map(|(n, c)| (n.clone(), c.clone()))
        .collect();

    let ctx = Ctx {
        funcs: &funcs,
        clsrs: &clsrs,
        pure_funcs: &pure_funcs,
        pure_clsrs: &pure_clsrs,
    };

    // One counter for the whole pass: materialised result names must be unique
    // module-wide, because two rewritten call sites can share a function body and
    // every downstream pass keys its flat per-body maps by value name.
    let counter = Entropy::<usize>::new();

    for (_, func) in module.funcs_mut() {
        rewrite_region(&mut func.region, &ctx, &counter);
    }
    for (_, clsr) in module.clsrs_mut() {
        rewrite_region(&mut clsr.region, &ctx, &counter);
    }
}

// --- Purity classifier ------------------------------------------------------

/// What a single region tree reveals about its enclosing func or clsr.
///
/// The fields are split so the worklist below can read each in isolation:
/// `has_host_tail` and `has_indirect_call` are *direct* reasons (terminate the
/// classifier in one step), while `func_calls` and `clsr_refs` are *edges* the
/// fixed point propagates impurity along.
#[derive(Default)]
struct BodyScan {
    /// `FuncName`s reached via a `Direct` call. Propagation edge.
    func_calls: HashSet<FuncName>,
    /// `ClsrName`s constructed (`Pure(Data::Clsr)`) or preallocated. Propagation edge.
    clsr_refs: HashSet<ClsrName>,
    /// A `Tail::Call(Indirect)` was found — the static classifier cannot follow
    /// the call, so it conservatively pins the enclosing impure. The interpreter
    /// still resolves indirect calls dynamically when the target is a known
    /// `Pure(Data::Clsr)` and that closure itself is pure.
    has_indirect_call: bool,
    /// A `Tail::Host(_)` was found — the impure boundary lives at the tail
    /// position, so any region tree whose tails include a host primitive is
    /// classified impure. (`Code`-level ops are now all pure: arithmetic and
    /// conversions are deterministic and folded by `scalar_eval::eval`; trap
    /// conditions are operand-dependent and handled by it returning `None`.)
    has_host_tail: bool,
}

fn scan_body(region: &Region) -> BodyScan {
    let mut scan = BodyScan::default();
    scan_region(region, &mut scan);
    scan
}

fn scan_region(region: &Region, scan: &mut BodyScan) {
    for (_, prealloc) in &region.preallocs {
        if let Prealloc::Clsr(c) = prealloc {
            scan.clsr_refs.insert(c.clone());
        }
    }

    for (_, value) in &region.values {
        match value {
            Value::Pure(Data::Clsr(c, _)) => {
                scan.clsr_refs.insert(c.clone());
            }
            Value::Pure(_) | Value::Alias(_) | Value::Eval(_) => {}
        }
    }

    match &region.tail {
        Tail::Call(CallTarget::Direct { target, .. }) => {
            scan.func_calls.insert(target.clone());
        }
        Tail::Call(CallTarget::Indirect { .. }) => {
            scan.has_indirect_call = true;
        }
        Tail::Host(_) => {
            scan.has_host_tail = true;
        }
        Tail::Jump(_) | Tail::Match(_) | Tail::Unreachable => {}
    }

    for (_, block) in &region.blocks {
        scan_region(&block.region, scan);
    }
}

/// The pure `FuncName`s and `ClsrName`s — every body that has no host code, no
/// `Indirect` call, no `Direct` call to an impure target, and constructs no
/// impure closure (transitively). One [`classify`] fixed point serves both sets.
fn purity(module: &Module) -> (HashSet<FuncName>, HashSet<ClsrName>) {
    let (impure_funcs, impure_clsrs) = classify(module);

    let pure_funcs = module
        .funcs()
        .iter()
        .filter(|(name, _)| !impure_funcs.contains(name))
        .map(|(name, _)| name.clone())
        .collect();

    let pure_clsrs = module
        .clsrs()
        .iter()
        .filter(|(name, _)| !impure_clsrs.contains(name))
        .map(|(name, _)| name.clone())
        .collect();

    (pure_funcs, pure_clsrs)
}

/// Compute the impure sets jointly. Funcs and clsrs share one fixed point
/// because their impurity cross-propagates: a func calling an impure clsr
/// (after specialisation, every closure call site is `Indirect` → handled by
/// `has_indirect_call`) or *constructing* an impure clsr is itself impure, and
/// vice versa.
///
/// Mirrors the worklist pattern in `dead_code_elimination::dce_module` with
/// the sign flipped: start with the *directly* impure seeds (host code or
/// indirect call), then iterate, adding any item that calls an impure func or
/// references an impure clsr. Stabilises in O(funcs + clsrs) iterations.
fn classify(module: &Module) -> (HashSet<FuncName>, HashSet<ClsrName>) {
    let func_scans: HashMap<FuncName, BodyScan> = module
        .funcs()
        .iter()
        .map(|(name, func)| (name.clone(), scan_body(&func.region)))
        .collect();
    let clsr_scans: HashMap<ClsrName, BodyScan> = module
        .clsrs()
        .iter()
        .map(|(name, clsr)| (name.clone(), scan_body(&clsr.region)))
        .collect();

    let mut impure_funcs: HashSet<FuncName> = func_scans
        .iter()
        .filter(|(_, scan)| scan.has_host_tail || scan.has_indirect_call)
        .map(|(name, _)| name.clone())
        .collect();
    let mut impure_clsrs: HashSet<ClsrName> = clsr_scans
        .iter()
        .filter(|(_, scan)| scan.has_host_tail || scan.has_indirect_call)
        .map(|(name, _)| name.clone())
        .collect();

    loop {
        let mut changed = false;

        for (name, scan) in &func_scans {
            if impure_funcs.contains(name) {
                continue;
            }
            if propagates(scan, &impure_funcs, &impure_clsrs) {
                impure_funcs.insert(name.clone());
                changed = true;
            }
        }

        for (name, scan) in &clsr_scans {
            if impure_clsrs.contains(name) {
                continue;
            }
            if propagates(scan, &impure_funcs, &impure_clsrs) {
                impure_clsrs.insert(name.clone());
                changed = true;
            }
        }

        if !changed {
            return (impure_funcs, impure_clsrs);
        }
    }
}

/// Whether the scanned body's edges reach an already-impure func or clsr.
fn propagates(
    scan: &BodyScan,
    impure_funcs: &HashSet<FuncName>,
    impure_clsrs: &HashSet<ClsrName>,
) -> bool {
    scan.func_calls.iter().any(|t| impure_funcs.contains(t))
        || scan.clsr_refs.iter().any(|c| impure_clsrs.contains(c))
}

// --- Interpreter ------------------------------------------------------------

/// Static handles the interpreter needs across every recursive call. The
/// funcs/clsrs maps are snapshots taken before the rewriter mutates anything,
/// so the interpreter can read freely while bodies are being edited in place.
struct Ctx<'a> {
    funcs: &'a HashMap<FuncName, Func>,
    clsrs: &'a HashMap<ClsrName, Clsr>,
    pure_funcs: &'a HashSet<FuncName>,
    pure_clsrs: &'a HashSet<ClsrName>,
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
enum Snapshot {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Rc<Vec<u8>>),
    Arr(Rc<Vec<Snapshot>>),
    Tpl(Rc<Vec<Snapshot>>),
    Clsr(ClsrName, Rc<RefCell<Vec<Snapshot>>>),
}

type Frame = HashMap<ValueName, Snapshot>;

enum Outcome {
    Returned(Snapshot),
    GaveUp,
}

struct Interp<'a> {
    ctx: &'a Ctx<'a>,
    /// Tail transitions remaining across the whole top-level interpretation —
    /// shared between the iterative loop and every recursive `run_body`.
    budget: usize,
    /// Calls remaining before the Rust stack would blow. Bracketed in
    /// `run_body` so it tracks real `Direct`/`Indirect` nesting.
    call_depth: usize,
}

impl<'a> Interp<'a> {
    fn new(ctx: &'a Ctx<'a>) -> Self {
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
    fn run_body(&mut self, region: &Region, body_resume: &BlockName, mut frame: Frame) -> Outcome {
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
        // Preallocs back recursive data: a `Prealloc::Clsr` is the placeholder
        // whose same-named `Pure(Data::Clsr)` fill mutates the `RefCell` in
        // place so a self-referential capture observes the resolved content
        // after the fill. `Tpl`/`Arr` preallocs back cyclic *data* values; the
        // placeholder stays empty, so a cyclic field access OOBs and the
        // interpreter gives up — correct for non-evaluable cyclic data.
        for (name, prealloc) in &region.preallocs {
            let placeholder = match prealloc {
                Prealloc::Clsr(c) => Snapshot::Clsr(c.clone(), Rc::new(RefCell::new(Vec::new()))),
                Prealloc::Tpl(_) => Snapshot::Tpl(Rc::new(Vec::new())),
                Prealloc::Arr(_) => Snapshot::Arr(Rc::new(Vec::new())),
            };
            frame.insert(name.clone(), placeholder);
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
            Value::Eval(code) => {
                // Delegate to `scalar_eval` — the same wasm-faithful arithmetic
                // and aggregate logic the constant folder uses, so traps line up.
                let lits = lits_from_frame(frame);
                let replacement = simplify(code, &lits)?;
                match replacement {
                    Value::Pure(data) => materialise_data(&data, frame),
                    Value::Alias(source) => frame.get(&source).cloned(),
                    Value::Eval(_) => None,
                }
            }
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
            Tail::Host(_) | Tail::Unreachable => Err(Outcome::GaveUp),
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

fn seed_frame(params: &[Argument], args: Vec<Snapshot>) -> Option<Frame> {
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
fn materialise_data(data: &Data, frame: &Frame) -> Option<Snapshot> {
    Some(match data {
        Data::Nat(n) => Snapshot::Nat(*n),
        Data::Int(i) => Snapshot::Int(*i),
        Data::Flt(f) => Snapshot::Flt(*f),
        Data::Bin(bytes) => Snapshot::Bin(Rc::new(bytes.clone())),
        Data::Arr(elems) => Snapshot::Arr(Rc::new(resolve_names(elems, frame)?)),
        Data::Tpl(elems) => Snapshot::Tpl(Rc::new(resolve_names(elems, frame)?)),
        Data::Clsr(c, captures) => Snapshot::Clsr(
            c.clone(),
            Rc::new(RefCell::new(resolve_names(captures, frame)?)),
        ),
    })
}

/// Project the frame down to a `scalar_eval::Lits` (scalars + `Bin` + aggregate
/// element-name lists), so `simplify` can reuse its existing literal-lookup
/// machinery without learning about `Snapshot`. Closures are deliberately
/// omitted — no `Code` operation reads a closure's structure.
fn lits_from_frame(frame: &Frame) -> Lits {
    let mut lits = Lits::new();
    // Mint synthetic names for aggregate elements that scalar_eval needs to look
    // up by name. We give each element a per-frame-unique synthetic name and
    // record a corresponding scalar/Bin entry; non-scalar elements are left out
    // and `simplify` will bail when it can't find them, which is the right
    // semantics (a folded `TplGet` over a tuple of non-literals stays unfolded).
    let synth = Entropy::<usize>::new();
    for (name, snap) in frame {
        match snap {
            Snapshot::Nat(n) => {
                lits.insert(name.clone(), Data::Nat(*n));
            }
            Snapshot::Int(i) => {
                lits.insert(name.clone(), Data::Int(*i));
            }
            Snapshot::Flt(f) => {
                lits.insert(name.clone(), Data::Flt(*f));
            }
            Snapshot::Bin(bytes) => {
                lits.insert(name.clone(), Data::Bin((**bytes).clone()));
            }
            Snapshot::Tpl(elems) => {
                let elem_names: Vec<ValueName> = elems
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let elem_name = ValueName::from(format!("{name}@elt{i}#{}", synth.fresh()));
                        if let Some(d) = scalar_data(elem) {
                            lits.insert(elem_name.clone(), d);
                        }
                        elem_name
                    })
                    .collect();
                lits.insert(name.clone(), Data::Tpl(elem_names));
            }
            Snapshot::Arr(elems) => {
                let elem_names: Vec<ValueName> = elems
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let elem_name = ValueName::from(format!("{name}@elt{i}#{}", synth.fresh()));
                        if let Some(d) = scalar_data(elem) {
                            lits.insert(elem_name.clone(), d);
                        }
                        elem_name
                    })
                    .collect();
                lits.insert(name.clone(), Data::Arr(elem_names));
            }
            Snapshot::Clsr(_, _) => {}
        }
    }
    lits
}

fn scalar_data(snap: &Snapshot) -> Option<Data> {
    Some(match snap {
        Snapshot::Nat(n) => Data::Nat(*n),
        Snapshot::Int(i) => Data::Int(*i),
        Snapshot::Flt(f) => Data::Flt(*f),
        Snapshot::Bin(bytes) => Data::Bin((**bytes).clone()),
        _ => return None,
    })
}

// --- Rewriter ---------------------------------------------------------------

/// Walk `region` and every nested block, attempting to rewrite the tail of each
/// one whose tail is a literal-argument call to a pure target.
fn rewrite_region(region: &mut Region, ctx: &Ctx<'_>, counter: &Entropy) {
    if let Some(rewrite) = try_evaluate_tail(region, ctx, counter) {
        region.values.extend(rewrite.new_values);
        region.tail = rewrite.new_tail;
    }
    for (_, block) in &mut region.blocks {
        rewrite_region(&mut block.region, ctx, counter);
    }
}

/// The replacement for a rewritten tail: any number of fresh `Pure(_)` bindings
/// (the materialised result, deepest-first) plus the new `Jump` tail.
struct Rewrite {
    new_values: Vec<(ValueName, Value)>,
    new_tail: Tail,
}

/// If the host region's tail is a Direct/Indirect call whose target is pure
/// and every argument is a known literal, interpret the call to a single
/// `Snapshot` and materialise that as a list of fresh bindings plus a jump to
/// the original resume.
fn try_evaluate_tail(region: &Region, ctx: &Ctx<'_>, counter: &Entropy) -> Option<Rewrite> {
    let lits = literals(region);

    let (callee_outcome, resume) = match &region.tail {
        Tail::Call(CallTarget::Direct {
            target,
            params,
            resume,
        }) => {
            if !ctx.pure_funcs.contains(target) {
                return None;
            }
            let callee = ctx.funcs.get(target)?;
            let arg_data = collect_literal_args(params, &lits)?;
            let snaps = arg_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            let callee_frame = seed_frame(&callee.params, snaps)?;
            let mut interp = Interp::new(ctx);
            (
                interp.run_body(&callee.region, &callee.resume, callee_frame),
                resume.clone(),
            )
        }
        Tail::Call(CallTarget::Indirect {
            target,
            params,
            resume,
        }) => {
            // The closure value must itself be a literal in this region.
            let Data::Clsr(clsr_name, capture_names) = lits.get(target)? else {
                return None;
            };
            if !ctx.pure_clsrs.contains(clsr_name) {
                return None;
            }
            let callee = ctx.clsrs.get(clsr_name)?;
            let cap_data = collect_literal_args(capture_names, &lits)?;
            let arg_data = collect_literal_args(params, &lits)?;
            let captures = cap_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            let args = arg_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            if callee.fields.len() != captures.len() || callee.params.len() != args.len() {
                return None;
            }
            let mut callee_frame: Frame = HashMap::new();
            for (field, cap) in callee.fields.iter().zip(captures) {
                callee_frame.insert(field.name.clone(), cap);
            }
            for (param, arg) in callee.params.iter().zip(args) {
                callee_frame.insert(param.name.clone(), arg);
            }
            let mut interp = Interp::new(ctx);
            (
                interp.run_body(&callee.region, &callee.resume, callee_frame),
                resume.clone(),
            )
        }
        _ => return None,
    };

    let Outcome::Returned(snap) = callee_outcome else {
        return None;
    };

    // Materialise the result snapshot. Fresh names use the pass-wide counter
    // suffixed `@eval#N`: the suffix keeps them clear of the host region's `vN`,
    // and the shared counter keeps two rewrites in one body from colliding.
    let mut new_values = Vec::<(ValueName, Value)>::new();
    let mut visited = HashSet::<*const ()>::new();
    let top = materialise_snapshot(&snap, counter, &mut new_values, &mut visited)?;

    Some(Rewrite {
        new_values,
        new_tail: Tail::Jump(JumpTarget {
            target: resume,
            params: vec![top],
        }),
    })
}

/// Collect the literal `Data` behind each name in `params`. Returns `None` if
/// any name is not a literal (the call isn't a candidate).
fn collect_literal_args(params: &[ValueName], lits: &Lits) -> Option<Vec<Data>> {
    params.iter().map(|p| lits.get(p).cloned()).collect()
}

/// Materialise a `Snapshot` into a chain of fresh `Pure(_)` bindings, emitted
/// deepest-first so each element is named before any aggregate that references
/// it. Returns the top-level binding's name (the value the new `Jump` carries).
///
/// Cycles abort: if the same aggregate `Rc` is visited twice on one path, no
/// finite IR representation exists, so the rewrite is discarded.
fn materialise_snapshot(
    snap: &Snapshot,
    counter: &Entropy,
    out: &mut Vec<(ValueName, Value)>,
    visited: &mut HashSet<*const ()>,
) -> Option<ValueName> {
    let data = match snap {
        Snapshot::Nat(n) => Data::Nat(*n),
        Snapshot::Int(i) => Data::Int(*i),
        Snapshot::Flt(f) => Data::Flt(*f),
        Snapshot::Bin(bytes) => Data::Bin((**bytes).clone()),
        Snapshot::Arr(elems) => {
            let key = Rc::as_ptr(elems) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let names: Option<Vec<ValueName>> = elems
                .iter()
                .map(|e| materialise_snapshot(e, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Arr(names?)
        }
        Snapshot::Tpl(elems) => {
            let key = Rc::as_ptr(elems) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let names: Option<Vec<ValueName>> = elems
                .iter()
                .map(|e| materialise_snapshot(e, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Tpl(names?)
        }
        Snapshot::Clsr(c, captures) => {
            let key = Rc::as_ptr(captures) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let cap_snaps = captures.borrow();
            let names: Option<Vec<ValueName>> = cap_snaps
                .iter()
                .map(|cap| materialise_snapshot(cap, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Clsr(c.clone(), names?)
        }
    };

    let name = ValueName::from(format!("v@eval#{}", counter.fresh()));
    out.push((name.clone(), Value::Pure(data)));
    Some(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn b(name: &str) -> BlockName {
        BlockName::from(name)
    }

    fn jump(target: &str, params: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(target),
            params,
        })
    }

    fn io_print(value: ValueName, resume: &str) -> Tail {
        Tail::Host(HostTarget::IoPrint {
            value,
            resume: b(resume),
        })
    }

    fn region(values: Vec<(ValueName, Value)>, tail: Tail) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks: vec![],
            tail,
        }
    }

    fn func(params: Vec<ValueName>, resume: &str, region: Region) -> Func {
        Func {
            params: params.into_iter().map(Into::into).collect(),
            resume: b(resume),
            region,
        }
    }

    fn clsr(fields: Vec<ValueName>, params: Vec<ValueName>, resume: &str, region: Region) -> Clsr {
        Clsr {
            fields: fields.into_iter().map(Into::into).collect(),
            params: params.into_iter().map(Into::into).collect(),
            resume: b(resume),
            region,
        }
    }

    #[test]
    fn purely_arithmetic_func_is_pure() {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                region(
                    vec![
                        (v("v0"), Value::Pure(Data::Nat(1))),
                        (v("v1"), Value::Eval(Code::NatAdd(v("p"), v("v0")))),
                    ],
                    jump("r", vec![v("v1")]),
                ),
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn host_tail_demotes_to_impure() {
        // A body whose tail is `Tail::Host(_)` is impure regardless of its
        // values. The `*ToStr` ops are no longer impure (see
        // `pure_to_str_code_is_pure`); `Io.print`/`Io.read` are the real
        // impurity boundary and they live at tail position.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(vec![v("p")], "r", region(vec![], io_print(v("p"), "r"))),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn pure_to_str_code_is_pure() {
        // `Code::NatToStr` is deterministic and folded by `scalar_eval`. A
        // body that only does conversions is now classified pure — the
        // interpreter materialises the converted string at compile time.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                region(
                    vec![(v("v0"), Value::Eval(Code::NatToStr(v("p"))))],
                    jump("r", vec![v("v0")]),
                ),
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn indirect_call_demotes_to_impure() {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Indirect {
                        target: v("p"),
                        params: vec![],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn impurity_propagates_through_direct_calls() {
        // `caller` calls `host`, which contains IoPrint; `caller` must also be impure.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("host"),
            func(vec![v("p")], "r", region(vec![], io_print(v("p"), "r"))),
        );
        module.add_func(
            FuncName::from("caller"),
            func(
                vec![v("p")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("host"),
                        params: vec![v("p")],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("host")));
        assert!(!pure.contains(&FuncName::from("caller")));
    }

    #[test]
    fn impurity_propagates_through_closure_construction() {
        // A pure-looking func that *constructs* an impure closure is itself impure.
        let mut module = Module::new();
        module.add_clsr(
            ClsrName::from("c"),
            clsr(
                vec![],
                vec![v("p")],
                "r",
                region(vec![], io_print(v("p"), "r")),
            ),
        );
        module.add_func(
            FuncName::from("builder"),
            func(
                vec![],
                "r",
                region(
                    vec![(
                        v("v0"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![])),
                    )],
                    jump("r", vec![v("v0")]),
                ),
            ),
        );

        let (pure_funcs, pure_clsrs) = purity(&module);
        assert!(!pure_clsrs.contains(&ClsrName::from("c")));
        assert!(!pure_funcs.contains(&FuncName::from("builder")));
    }

    // --- Rewriter tests ----------------------------------------------------

    fn module_with_main_calling(callee: &str, args: Vec<(ValueName, Data)>) -> Module {
        let arg_names: Vec<ValueName> = args.iter().map(|(n, _)| n.clone()).collect();
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: args
                        .into_iter()
                        .map(|(name, data)| (name, Value::Pure(data)))
                        .collect(),
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rm", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from(callee),
                        params: arg_names,
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module
    }

    fn main_region(module: &Module) -> &Region {
        &module
            .funcs()
            .iter()
            .find(|(n, _)| n == &FuncName::from("main"))
            .expect("main present")
            .1
            .region
    }

    fn find_pure(values: &[(ValueName, Value)], data: &Data) -> bool {
        values.iter().any(|(_, v)| match (v, data) {
            (Value::Pure(Data::Nat(a)), Data::Nat(b)) => a == b,
            (Value::Pure(Data::Int(a)), Data::Int(b)) => a == b,
            (Value::Pure(Data::Tpl(a)), Data::Tpl(b)) => a.len() == b.len(),
            _ => false,
        })
    }

    #[test]
    fn evaluates_pure_direct_call_with_literal_arg() {
        // f(n) = n + 1; main calls f(41) → 42, residual is Pure(Nat(42)) + Jump(cont).
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(41))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("one"), Value::Pure(Data::Nat(1))),
                        (v("sum"), Value::Eval(Code::NatAdd(v("n"), v("one")))),
                    ],
                    jump("rf", vec![v("sum")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        assert!(
            find_pure(&main.values, &Data::Nat(42)),
            "expected Pure(Nat(42)) in main, got {:?}",
            main.values,
        );
        match &main.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("cont"));
                assert_eq!(target.params.len(), 1);
            }
            other => panic!("expected Jump to cont, got {other:?}"),
        }
    }

    #[test]
    fn evaluates_recursive_direct_call() {
        // sum(n) = if n == 0 then 0 else n + sum(n - 1); main calls sum(3) → 6.
        let sum_region = Region {
            preallocs: vec![],
            values: vec![(v("zero"), Value::Pure(Data::Nat(0)))],
            blocks: vec![
                (
                    b("rec"),
                    Block {
                        params: vec![],
                        region: region(
                            vec![
                                (v("one"), Value::Pure(Data::Nat(1))),
                                (v("m"), Value::Eval(Code::NatSub(v("n"), v("one")))),
                            ],
                            Tail::Call(CallTarget::Direct {
                                target: FuncName::from("sum"),
                                params: vec![v("m")],
                                resume: b("k"),
                            }),
                        ),
                    },
                ),
                (
                    b("k"),
                    Block {
                        params: vec![v("rec_r")],
                        region: region(
                            vec![(v("total"), Value::Eval(Code::NatAdd(v("n"), v("rec_r"))))],
                            jump("rsum", vec![v("total")]),
                        ),
                    },
                ),
            ],
            tail: Tail::Match(MatchTarget {
                operand: v("n"),
                cases: [(
                    0u32,
                    JumpTarget {
                        target: b("rsum"),
                        params: vec![v("zero")],
                    },
                )]
                .into_iter()
                .collect(),
                default: Some(JumpTarget {
                    target: b("rec"),
                    params: vec![],
                }),
            }),
        };
        let mut module = module_with_main_calling("sum", vec![(v("three"), Data::Nat(3))]);
        module.add_func(
            FuncName::from("sum"),
            Func {
                params: vec![v("n").into()],
                resume: b("rsum"),
                region: sum_region,
            },
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        assert!(
            find_pure(&main.values, &Data::Nat(6)),
            "expected Pure(Nat(6)) in main, got {:?}",
            main.values,
        );
        assert!(
            matches!(&main.tail, Tail::Jump(_)),
            "expected Jump in main, got {:?}",
            main.tail,
        );
    }

    #[test]
    fn evaluates_aggregate_construction_into_tpl() {
        // pair(a, b) = (a, b); main calls pair(3, 4). Residual: a Tpl with two
        // scalar literals plus a Jump.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![
                        (v("three"), Value::Pure(Data::Nat(3))),
                        (v("four"), Value::Pure(Data::Nat(4))),
                    ],
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rm", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("pair"),
                        params: vec![v("three"), v("four")],
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module.add_func(
            FuncName::from("pair"),
            func(
                vec![v("a"), v("b")],
                "rp",
                region(
                    vec![(v("t"), Value::Pure(Data::Tpl(vec![v("a"), v("b")])))],
                    jump("rp", vec![v("t")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        // The materialised tuple has length 2.
        assert!(
            find_pure(&main.values, &Data::Tpl(vec![v("_a"), v("_b")])),
            "expected materialised Tpl(2) in main, got {:?}",
            main.values,
        );
        // Two fresh scalar bindings (3 and 4) for the tuple's elements.
        assert!(find_pure(&main.values, &Data::Nat(3)));
        assert!(find_pure(&main.values, &Data::Nat(4)));
        assert!(matches!(&main.tail, Tail::Jump(_)));
    }

    #[test]
    fn leaves_impure_call_intact() {
        // f does IoPrint; calls to it must not be folded — the trap/effect
        // observability would be lost.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(1))]);
        module.add_func(
            FuncName::from("f"),
            func(vec![v("n")], "rf", region(vec![], io_print(v("n"), "rf"))),
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn leaves_budget_overrun_intact() {
        // f(n) = f(n) — pure but non-terminating. The budget is consumed and
        // the original call survives.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(1))]);
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![v("n").into()],
                resume: b("rf"),
                region: Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![(
                        b("k"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rf", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("f"),
                        params: vec![v("n")],
                        resume: b("k"),
                    }),
                },
            },
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn leaves_trap_call_intact() {
        // f(n) = n / 0 — pure but traps. `scalar_eval::eval` returns None on
        // the divide-by-zero, the interpreter aborts, the original call stays.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(5))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("zero"), Value::Pure(Data::Nat(0))),
                        (v("q"), Value::Eval(Code::NatDiv(v("n"), v("zero")))),
                    ],
                    jump("rf", vec![v("q")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn folds_to_str_call_at_compile_time() {
        // f(n) = Nat::to_str(n) — now pure under the new classification (see
        // `pure_to_str_code_is_pure`). With a literal argument, partial eval
        // folds the call to a `Pure(Data::Bin("7"))` plus a jump to the
        // original resume; the runtime conversion disappears.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(7))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![(v("s"), Value::Eval(Code::NatToStr(v("n"))))],
                    jump("rf", vec![v("s")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let region = main_region(&module);
        assert!(
            matches!(&region.tail, Tail::Jump(_)),
            "expected the Direct call to fold to a Jump, got {:?}",
            region.tail,
        );
        assert!(
            region
                .values
                .iter()
                .any(|(_, v)| matches!(v, Value::Pure(Data::Bin(b)) if b == b"7")),
            "expected a Pure(Bin(\"7\")) binding from the folded NatToStr, got {:?}",
            region.values,
        );
    }

    #[test]
    fn two_rewrites_in_one_body_mint_distinct_names() {
        // main's root tail calls f(41), and the continuation block's tail calls
        // f(41) again. Both sites are rewritten in one pass; the materialised
        // bindings land in two regions of the *same* body, so their names must
        // differ — every downstream pass keys flat per-body maps by value name.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(v("a"), Value::Pure(Data::Nat(41)))],
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: Region {
                                preallocs: vec![],
                                values: vec![(v("a2"), Value::Pure(Data::Nat(41)))],
                                blocks: vec![(
                                    b("cont2"),
                                    Block {
                                        params: vec![v("res2")],
                                        region: region(vec![], jump("rm", vec![v("res2")])),
                                    },
                                )],
                                tail: Tail::Call(CallTarget::Direct {
                                    target: FuncName::from("f"),
                                    params: vec![v("a2")],
                                    resume: b("cont2"),
                                }),
                            },
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("f"),
                        params: vec![v("a")],
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("one"), Value::Pure(Data::Nat(1))),
                        (v("sum"), Value::Eval(Code::NatAdd(v("n"), v("one")))),
                    ],
                    jump("rf", vec![v("sum")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        // Both call sites folded to jumps.
        let main = main_region(&module);
        assert!(matches!(&main.tail, Tail::Jump(_)));
        let (_, cont) = &main.blocks[0];
        assert!(matches!(&cont.region.tail, Tail::Jump(_)));

        // No value name is bound twice anywhere in the body.
        fn collect_names(region: &Region, names: &mut Vec<ValueName>) {
            names.extend(region.values.iter().map(|(n, _)| n.clone()));
            for (_, block) in &region.blocks {
                collect_names(&block.region, names);
            }
        }
        let mut names = Vec::new();
        collect_names(main, &mut names);
        let unique: HashSet<&ValueName> = names.iter().collect();
        assert_eq!(
            unique.len(),
            names.len(),
            "duplicate value names in one body: {names:?}",
        );
    }

    // --- Classifier tests --------------------------------------------------

    #[test]
    fn mutually_recursive_pure_funcs_stay_pure() {
        // `even`/`odd` call each other only on the pure arithmetic path.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("even"),
            func(
                vec![v("n")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("odd"),
                        params: vec![v("n")],
                        resume: b("r"),
                    }),
                },
            ),
        );
        module.add_func(
            FuncName::from("odd"),
            func(
                vec![v("n")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("even"),
                        params: vec![v("n")],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("even")));
        assert!(pure.contains(&FuncName::from("odd")));
    }
}
