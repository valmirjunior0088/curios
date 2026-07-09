//! Compile-time evaluation of closed subterms.
//!
//! A dependently-typed program routinely computes data the type checker already
//! evaluated — `std/Fmt`'s `parse(s)` runs a full parser combinator over a
//! format-string *literal* to produce the `Fmt` the term then folds over. After
//! erasure that computation is ordinary first-order evaluation, so this pass
//! runs it here: every **closed** `Apply` (its free names all resolve to module
//! items) is handed to a fueled big-step call-by-value interpreter, and a
//! successful run replaces the call with its reified result. Cont's
//! `evaluate_pure_calls` cannot reach these — its purity classifier statically
//! rejects any body with an indirect call, and combinator code is built from
//! records of closures — while an interpreter simply follows the closures.
//!
//! Three result shapes are installed:
//!
//! - **data** — scalars, bytestrings, lists, tuples, tags — as literal terms;
//! - **closures** — the `Func`'s body copied verbatim, its resolved local
//!   captures bound around it as `Let`s (`Fmt/print(lit)` becomes the curried
//!   hole-filling closure with the parsed spine baked in);
//! - **an effect residual** — when evaluation reaches a host primitive, or a
//!   call to an item whose body performs an effect, in *tail position of the
//!   candidate* with fully-evaluated inputs, the primitive (or the call, with
//!   its arguments reified) is installed instead: `Fmt/print(lit)(42)("x")`
//!   collapses to `#/std/Io/print("42 …")`. Compile-time evaluation never
//!   *performs* an effect, so the residual runs it at the same program point
//!   with the same values — the pure prefix is simply baked into its operands.
//!
//! Everything else **bails** and leaves the term untouched: fuel or call-depth
//! exhaustion, a would-trap operation (the runtime trap must survive — the
//! discipline and the 31-bit arithmetic below mirror
//! `curios-cont/src/optimize/scalar_eval.rs`, the wasm-faithful table shared by
//! cont's folding and interpretation), an effect anywhere but the candidate's
//! tail, a `Cell` primitive (never residualized: a cell op's identity is its
//! program point), an unresolvable name, or an oversized reification.
//!
//! The pass is two-phase because evaluation borrows the module's syntax
//! (closure values point straight at their `Func` nodes): a *plan* phase walks
//! `&Module` and records `(location, replacement)` pairs — every replacement a
//! freshly built term — and an *apply* phase installs them through
//! [`rewrite::term_at_mut`]. A successful candidate is not descended into, so
//! recorded paths never overlap.

use {
    super::{
        Loc, children, clone_args, deep_copy, members, refresh_touched, root_mut, term_at_mut,
    },
    crate::{
        Apply, Atom, Func, HostPrim, Item, Let, Module, NatMatch, Prim, PurePrim, Subterm, Term,
        Tuple,
    },
    std::{
        cell::RefCell,
        collections::{HashMap, HashSet},
        rc::Rc,
        sync::Arc,
    },
};

/// Per-candidate step budget. A format-string parse over a short literal runs
/// hundreds of steps; the UTF-8 revalidation of its segments a few thousand.
const STEP_BUDGET: usize = 50_000;

/// Shared step pool for the whole pass — the hard compile-time ceiling, spent
/// across every candidate (and the memoized item forcings they trigger).
const PASS_BUDGET: usize = 500_000;

/// Call-nesting cap: each interpreted call recurses into a host Rust frame.
const MAX_CALL_DEPTH: usize = 256;

/// Caps on one replacement term: total nodes, and total `Bin` bytes plus `Lst`
/// elements. Reification past either declines the candidate.
const MAX_REIFY_NODES: usize = 2_048;
const MAX_REIFY_BYTES: usize = 65_536;

/// Fold every closed `Apply` the interpreter can finish, module-wide.
pub(crate) fn evaluate_closed_terms(module: &mut Module) {
    let rewrites = plan(module);
    apply(module, rewrites);
}

struct Rewrite {
    loc: Loc,
    path: Vec<usize>,
    replacement: Term,
}

fn plan(module: &Module) -> Vec<Rewrite> {
    // `into_cont` requires a definition to precede its uses, and item order is
    // also the eager-init order. Evaluation follows calls *forward* through
    // the item list, so a fold inside item `i` can bake in a direct reference
    // to a later item that was only reached through calls before — such a
    // replacement is declined rather than reordering the module.
    let owner: HashMap<&str, usize> = module
        .items
        .iter()
        .enumerate()
        .flat_map(|(index, item)| item.names().map(move |name| (name, index)))
        .collect();

    let mut evaluator = Evaluator::new(module);
    let mut rewrites = Vec::new();

    for (index, item) in module.items.iter().enumerate() {
        for (member, term) in members(item).into_iter().enumerate() {
            for (path, replacement) in scan_root(&mut evaluator, term) {
                let backward_only = replacement
                    .free_names()
                    .iter()
                    .all(|name| owner.get(name.as_str()).is_some_and(|&at| at <= index));
                if !backward_only {
                    continue;
                }
                rewrites.push(Rewrite {
                    loc: Loc::Item {
                        item: index,
                        member,
                    },
                    path,
                    replacement,
                });
            }
        }
    }
    for (path, replacement) in scan_root(&mut evaluator, &module.body) {
        rewrites.push(Rewrite {
            loc: Loc::Body,
            path,
            replacement,
        });
    }

    rewrites
}

fn apply(module: &mut Module, rewrites: Vec<Rewrite>) {
    let mut touched: HashSet<(usize, usize)> = HashSet::new();
    let mut touched_body = false;

    for rewrite in rewrites {
        match rewrite.loc {
            Loc::Item { item, member } => {
                touched.insert((item, member));
            }
            Loc::Body => touched_body = true,
        }
        let root = root_mut(module, rewrite.loc);
        *term_at_mut(root, &rewrite.path) = rewrite.replacement;
    }

    // A replacement can introduce free names (a reified closure's global
    // captures, a residual call's item name) *under* enclosing closures whose
    // capture lists predate the rewrite. `into_cont` threads every global
    // through the enclosing captures, and `free_names`/prune read them — so
    // every rewritten root's captures are recomputed. No names are introduced
    // with flags: fresh global captures default to non-candidate.
    refresh_touched(module, &touched, touched_body, &HashMap::new());
}

fn scan_root<'m>(evaluator: &mut Evaluator<'m>, root: &'m Term) -> Vec<(Vec<usize>, Term)> {
    let mut found = Vec::new();
    scan(evaluator, root, &mut Vec::new(), &mut found);
    found
}

/// Try every closed `Apply`, outermost-first; a successful fold is recorded and
/// not descended into, a failed one is searched for foldable sub-candidates.
fn scan<'m>(
    evaluator: &mut Evaluator<'m>,
    term: &'m Term,
    path: &mut Vec<usize>,
    found: &mut Vec<(Vec<usize>, Term)>,
) {
    if matches!(term.as_subterm(), Subterm::Apply(_))
        && evaluator.is_closed(term)
        && let Some(replacement) = evaluator.attempt(term)
    {
        found.push((path.clone(), replacement));
        return;
    }

    for (index, child) in children(term).into_iter().enumerate() {
        path.push(index);
        scan(evaluator, child, path, found);
        path.pop();
    }
}

// --- Values -------------------------------------------------------------------

/// A runtime value. Aggregates are `Rc`-shared (environments clone freely);
/// a closure points at its `Func` syntax and carries its resolved **local**
/// captures — module-item captures stay unresolved and are looked up globally
/// at use, which is what lets a `rec` group's members reference each other
/// without forcing.
#[derive(Clone)]
enum Value<'m> {
    Erased,
    Nat(u32),
    Int(i32),
    Flt(f32),
    Io(u32),
    Tag(usize),
    Bin(Rc<Vec<u8>>),
    Lst(Rc<Vec<Value<'m>>>),
    Tuple(Rc<Vec<Value<'m>>>),
    Clsr(Rc<ClsrVal<'m>>),
}

impl<'m> Value<'m> {
    fn nat(&self) -> Result<u32, Bail> {
        match self {
            Value::Nat(value) => Ok(*value),
            _ => Err(Bail::Unsupported),
        }
    }

    fn int(&self) -> Result<i32, Bail> {
        match self {
            Value::Int(value) => Ok(*value),
            _ => Err(Bail::Unsupported),
        }
    }

    fn flt(&self) -> Result<f32, Bail> {
        match self {
            Value::Flt(value) => Ok(*value),
            _ => Err(Bail::Unsupported),
        }
    }

    fn bin(&self) -> Result<&Rc<Vec<u8>>, Bail> {
        match self {
            Value::Bin(bytes) => Ok(bytes),
            _ => Err(Bail::Unsupported),
        }
    }

    fn lst(&self) -> Result<&Rc<Vec<Value<'m>>>, Bail> {
        match self {
            Value::Lst(elements) => Ok(elements),
            _ => Err(Bail::Unsupported),
        }
    }
}

/// The `RefCell` exists for local `rec`: members are bound as empty closures
/// first, then each capture env is backpatched to see its siblings.
struct ClsrVal<'m> {
    func: &'m Func,
    env: RefCell<Vec<(String, Value<'m>)>>,
}

type Env<'m> = Vec<(String, Value<'m>)>;

fn lookup_local<'m>(env: &Env<'m>, name: &str) -> Option<Value<'m>> {
    env.iter()
        .rev()
        .find(|(bound, _)| bound == name)
        .map(|(_, value)| value.clone())
}

/// How one evaluation ended: a value, a residual term (evaluation reached an
/// effect in the candidate's tail — the residual *is* the candidate's
/// replacement), or a bail leaving the candidate untouched.
enum Outcome<'m> {
    Done(Value<'m>),
    Stuck(Term),
    Bail(Bail),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bail {
    /// An effectful primitive outside the candidate's tail. The only
    /// *convertible* bail: a tail call to a module item whose body bails this
    /// way is residualized as the call itself, arguments reified.
    Effect,
    Fuel,
    Depth,
    /// The operation would trap at runtime on these operands; folding would
    /// erase the trap.
    Trap,
    Unknown,
    Arity,
    TooBig,
    Cycle,
    Unsupported,
}

// --- The interpreter ------------------------------------------------------------

struct Evaluator<'m> {
    /// Every declared item name to its body (`rec` members individually).
    items: HashMap<&'m str, &'m Term>,
    /// Memoized item values; `None` poisons a name whose forcing failed.
    cache: HashMap<&'m str, Option<Value<'m>>>,
    /// Names currently being forced — a cycle means a value-recursive CAF.
    forcing: HashSet<&'m str>,
    pool: usize,
    steps: usize,
    depth: usize,
}

impl<'m> Evaluator<'m> {
    fn new(module: &'m Module) -> Self {
        let mut items = HashMap::new();
        for item in &module.items {
            match item {
                Item::Let { name, body } => {
                    items.insert(name.as_str(), body);
                }
                Item::Rec {
                    names,
                    items: members,
                } => {
                    for (name, member) in names.iter().zip(members) {
                        items.insert(name.as_str(), member);
                    }
                }
            }
        }
        Self {
            items,
            cache: HashMap::new(),
            forcing: HashSet::new(),
            pool: PASS_BUDGET,
            steps: 0,
            depth: 0,
        }
    }

    fn is_closed(&self, term: &Term) -> bool {
        term.free_names()
            .iter()
            .all(|name| self.items.contains_key(name.as_str()))
    }

    /// Evaluate one candidate from a fresh frame. `Done` reifies; `Stuck` is
    /// installed as-is unless it prints identically to the original (a residual
    /// from an earlier fold re-scanned — replacing it would churn forever).
    fn attempt(&mut self, term: &'m Term) -> Option<Term> {
        self.steps = 0;
        self.depth = 0;

        match self.eval(term, &mut Vec::new(), true) {
            Outcome::Done(value) => {
                let mut budget = ReifyBudget::new();
                self.reify(&value, &mut budget, &mut Vec::new()).ok()
            }
            Outcome::Stuck(residual) => {
                (format!("{residual}") != format!("{term}")).then_some(residual)
            }
            Outcome::Bail(_) => None,
        }
    }

    fn charge(&mut self) -> Result<(), Bail> {
        if self.pool == 0 || self.steps >= STEP_BUDGET {
            return Err(Bail::Fuel);
        }
        self.pool -= 1;
        self.steps += 1;
        Ok(())
    }

    /// Evaluate in a non-tail position: the result must be a value — a `Stuck`
    /// cannot arise there (effects outside the tail bail), which is what makes
    /// discarding the surrounding bindings of a `Stuck` sound.
    fn value_of(&mut self, term: &'m Term, env: &mut Env<'m>) -> Result<Value<'m>, Bail> {
        match self.eval(term, env, false) {
            Outcome::Done(value) => Ok(value),
            Outcome::Stuck(_) => unreachable!("non-tail evaluation never residualizes"),
            Outcome::Bail(bail) => Err(bail),
        }
    }

    /// Big-step call-by-value evaluation. `tail` tracks whether `term`'s result
    /// is the candidate's result — the only positions where an effect may
    /// residualize instead of bailing.
    fn eval(&mut self, term: &'m Term, env: &mut Env<'m>, tail: bool) -> Outcome<'m> {
        if let Err(bail) = self.charge() {
            return Outcome::Bail(bail);
        }

        match term.as_subterm() {
            Subterm::Erased => Outcome::Done(Value::Erased),
            // Reaching `Unreachable` means the runtime would trap here.
            Subterm::Unreachable => Outcome::Bail(Bail::Unsupported),
            Subterm::Atom(atom) => Outcome::Done(Value::Tag(atom.index)),
            Subterm::Name(name) => {
                if let Some(value) = lookup_local(env, name.as_str()) {
                    return Outcome::Done(value);
                }
                match self.item_value(name.as_str()) {
                    Ok(value) => Outcome::Done(value),
                    Err(bail) => Outcome::Bail(bail),
                }
            }
            Subterm::Func(func) => match self.close(func, env) {
                Ok(value) => Outcome::Done(value),
                Err(bail) => Outcome::Bail(bail),
            },
            Subterm::Tuple(tuple) => {
                let mut fields = Vec::with_capacity(tuple.fields.len());
                for field in &tuple.fields {
                    match self.value_of(field, env) {
                        Ok(value) => fields.push(value),
                        Err(bail) => return Outcome::Bail(bail),
                    }
                }
                Outcome::Done(Value::Tuple(Rc::new(fields)))
            }
            Subterm::Proj(proj) => {
                let head = match self.value_of(&proj.head, env) {
                    Ok(value) => value,
                    Err(bail) => return Outcome::Bail(bail),
                };
                match head {
                    Value::Tuple(fields) => match fields.get(proj.index) {
                        Some(field) => Outcome::Done(field.clone()),
                        None => Outcome::Bail(Bail::Unsupported),
                    },
                    _ => Outcome::Bail(Bail::Unsupported),
                }
            }
            Subterm::Match(m) => {
                let head = match self.value_of(&m.head, env) {
                    Ok(value) => value,
                    Err(bail) => return Outcome::Bail(bail),
                };
                let Value::Tag(tag) = head else {
                    return Outcome::Bail(Bail::Unsupported);
                };
                match m.cases.get(&tag).or(m.default.as_ref()) {
                    Some(case) => self.eval(case, env, tail),
                    None => Outcome::Bail(Bail::Unsupported),
                }
            }
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                cases,
                default,
            }) => {
                let head = match self.value_of(head, env) {
                    Ok(value) => value,
                    Err(bail) => return Outcome::Bail(bail),
                };
                let Value::Nat(scrutinee) = head else {
                    return Outcome::Bail(Bail::Unsupported);
                };
                self.eval(cases.get(&scrutinee).unwrap_or(default), env, tail)
            }
            Subterm::NatMatch(NatMatch::Induction {
                head,
                zero_case,
                pred,
                ih,
                succ_case,
            }) => {
                let head = match self.value_of(head, env) {
                    Ok(value) => value,
                    Err(bail) => return Outcome::Bail(bail),
                };
                let Value::Nat(bound) = head else {
                    return Outcome::Bail(Bail::Unsupported);
                };
                let mut accumulator = match self.value_of(zero_case, env) {
                    Ok(value) => value,
                    Err(bail) => return Outcome::Bail(bail),
                };
                for step in 0..bound {
                    if let Err(bail) = self.charge() {
                        return Outcome::Bail(bail);
                    }
                    let scope = env.len();
                    env.push((pred.clone(), Value::Nat(step)));
                    env.push((ih.clone(), accumulator));
                    let result = self.value_of(succ_case, env);
                    env.truncate(scope);
                    match result {
                        Ok(value) => accumulator = value,
                        Err(bail) => return Outcome::Bail(bail),
                    }
                }
                Outcome::Done(accumulator)
            }
            // A `let` chain recurses along its `.tail` spine; peel it into a
            // loop so a long straight-line sequence of local bindings costs
            // one iteration instead of one Rust stack frame. Each additional
            // binding beyond the first (already charged by this `eval` call's
            // own entry) is charged explicitly, matching the fuel cost of the
            // recursive form it replaces.
            Subterm::Let(binding) => {
                let mut let_ = binding;
                let mut pushed = 0usize;

                let result = loop {
                    let mut bail = None;

                    for (name, body) in &let_.bindings {
                        // The first binding of the whole chain is charged by
                        // this `eval` call's own entry; every one beyond it is
                        // charged explicitly, matching the recursive form's fuel.
                        if pushed > 0 {
                            if let Err(b) = self.charge() {
                                bail = Some(b);
                                break;
                            }
                        }

                        let bound = match self.value_of(body, env) {
                            Ok(value) => value,
                            Err(b) => {
                                bail = Some(b);
                                break;
                            }
                        };
                        env.push((name.clone(), bound));
                        pushed += 1;
                    }

                    if let Some(bail) = bail {
                        break Outcome::Bail(bail);
                    }

                    match let_.tail.as_subterm() {
                        Subterm::Let(next) => let_ = next,
                        _ => break self.eval(&let_.tail, env, tail),
                    }
                };

                env.truncate(env.len() - pushed);
                result
            }
            Subterm::Rec(rec) => {
                let scope = env.len();
                let mut closures = Vec::with_capacity(rec.items.len());
                for (name, member) in rec.names.iter().zip(&rec.items) {
                    let Subterm::Func(func) = member.as_subterm() else {
                        env.truncate(scope);
                        return Outcome::Bail(Bail::Unsupported);
                    };
                    let closure = Rc::new(ClsrVal {
                        func,
                        env: RefCell::new(Vec::new()),
                    });
                    env.push((name.clone(), Value::Clsr(Rc::clone(&closure))));
                    closures.push(closure);
                }
                for closure in &closures {
                    match self.capture_env(closure.func, env) {
                        Ok(captured) => *closure.env.borrow_mut() = captured,
                        Err(bail) => {
                            env.truncate(scope);
                            return Outcome::Bail(bail);
                        }
                    }
                }
                let result = self.eval(&rec.tail, env, tail);
                env.truncate(scope);
                result
            }
            Subterm::Apply(apply) => self.eval_apply(apply, env, tail),
            Subterm::Prim(prim) => self.eval_prim(prim, env, tail),
        }
    }

    fn close(&mut self, func: &'m Func, env: &Env<'m>) -> Result<Value<'m>, Bail> {
        Ok(Value::Clsr(Rc::new(ClsrVal {
            func,
            env: RefCell::new(self.capture_env(func, env)?),
        })))
    }

    /// Resolve a `Func`'s captures against the local environment. A capture
    /// naming a module item is *skipped* — it resolves globally at use.
    fn capture_env(&mut self, func: &'m Func, env: &Env<'m>) -> Result<Env<'m>, Bail> {
        let mut captured = Vec::new();
        for capture in &func.captures {
            if let Some(value) = lookup_local(env, &capture.name) {
                captured.push((capture.name.clone(), value));
            } else if !self.items.contains_key(capture.name.as_str()) {
                return Err(Bail::Unknown);
            }
        }
        Ok(captured)
    }

    /// The memoized value of a module item. A `Func` body is a closure without
    /// forcing; anything else (a CAF) is evaluated once. A failed forcing
    /// poisons the name with a *hard* bail — never `Effect`, so a failed CAF is
    /// never residualized as a call — except on fuel exhaustion, which leaves
    /// the name uncached for a later, fresher attempt.
    fn item_value(&mut self, name: &'m str) -> Result<Value<'m>, Bail> {
        if let Some(cached) = self.cache.get(name) {
            return cached.clone().ok_or(Bail::Unknown);
        }
        let Some(&term) = self.items.get(name) else {
            return Err(Bail::Unknown);
        };
        if !self.forcing.insert(name) {
            return Err(Bail::Cycle);
        }

        let result = match term.as_subterm() {
            // An item-level `Func` closes over module items only.
            Subterm::Func(func) => Ok(Value::Clsr(Rc::new(ClsrVal {
                func,
                env: RefCell::new(Vec::new()),
            }))),
            _ => match self.eval(term, &mut Vec::new(), false) {
                Outcome::Done(value) => Ok(value),
                Outcome::Bail(Bail::Fuel) => {
                    self.forcing.remove(name);
                    return Err(Bail::Fuel);
                }
                Outcome::Stuck(_) | Outcome::Bail(_) => Err(Bail::Unknown),
            },
        };

        self.forcing.remove(name);
        self.cache.insert(name, result.clone().ok());
        result
    }

    fn eval_apply(&mut self, apply: &'m Apply, env: &mut Env<'m>, tail: bool) -> Outcome<'m> {
        let head = match self.value_of(&apply.head, env) {
            Ok(value) => value,
            Err(bail) => return Outcome::Bail(bail),
        };
        let mut args = Vec::with_capacity(apply.params.len());
        for param in &apply.params {
            match self.value_of(param, env) {
                Ok(value) => args.push(value),
                Err(bail) => return Outcome::Bail(bail),
            }
        }
        let Value::Clsr(closure) = head else {
            return Outcome::Bail(Bail::Unsupported);
        };

        match self.enter(&closure, args.clone(), tail) {
            // Boundary conversion: the callee's body performs an effect it
            // cannot residualize itself, but this call *is* the candidate's
            // tail and names a module item — so the call, with its already
            // evaluated arguments reified, is an exact residual: the callee
            // reruns in full at the same program point with the same inputs.
            Outcome::Bail(Bail::Effect) if tail => {
                let Subterm::Name(name) = apply.head.as_subterm() else {
                    return Outcome::Bail(Bail::Effect);
                };
                if !self.items.contains_key(name.as_str()) {
                    return Outcome::Bail(Bail::Effect);
                }
                let mut budget = ReifyBudget::new();
                let mut params = Vec::with_capacity(args.len());
                for arg in &args {
                    match self.reify(arg, &mut budget, &mut Vec::new()) {
                        Ok(reified) => params.push(reified),
                        Err(_) => return Outcome::Bail(Bail::Effect),
                    }
                }
                Outcome::Stuck(
                    Subterm::Apply(Apply {
                        head: Subterm::Name(name.clone()).into(),
                        params,
                    })
                    .into(),
                )
            }
            other => other,
        }
    }

    fn enter(&mut self, closure: &ClsrVal<'m>, args: Vec<Value<'m>>, tail: bool) -> Outcome<'m> {
        if closure.func.params.len() != args.len() {
            return Outcome::Bail(Bail::Arity);
        }
        if self.depth >= MAX_CALL_DEPTH {
            return Outcome::Bail(Bail::Depth);
        }
        self.depth += 1;

        let mut frame: Env<'m> = closure.env.borrow().clone();
        frame.extend(
            closure
                .func
                .params
                .iter()
                .map(|param| param.name.clone())
                .zip(args),
        );
        let result = self.eval(&closure.func.body, &mut frame, tail);

        self.depth -= 1;
        result
    }

    fn eval_prim(&mut self, prim: &'m Prim, env: &mut Env<'m>, tail: bool) -> Outcome<'m> {
        match prim {
            // A cell op's identity is its program point: never residualized.
            Prim::Cell(_) => Outcome::Bail(Bail::Effect),
            Prim::Host(host) => {
                let mut operands = Vec::new();
                for operand in prim.operands() {
                    match self.value_of(operand, env) {
                        Ok(value) => operands.push(value),
                        Err(bail) => return Outcome::Bail(bail),
                    }
                }
                if !tail {
                    return Outcome::Bail(Bail::Effect);
                }
                let mut budget = ReifyBudget::new();
                let mut reified = Vec::with_capacity(operands.len());
                for operand in &operands {
                    match self.reify(operand, &mut budget, &mut Vec::new()) {
                        Ok(term) => reified.push(term),
                        Err(_) => return Outcome::Bail(Bail::Effect),
                    }
                }
                let rebuilt = match host {
                    HostPrim::Foreign(function, _) => {
                        HostPrim::Foreign(Arc::clone(function), reified)
                    }
                    HostPrim::IoExit(_) => {
                        let code = reified.pop().expect("IoExit has one operand");
                        HostPrim::IoExit(code)
                    }
                };
                Outcome::Stuck(Subterm::Prim(Prim::Host(rebuilt)).into())
            }
            Prim::Pure(pure) => {
                // Literals and the closure-applying map first; everything else
                // evaluates its operands positionally and folds.
                match pure {
                    PurePrim::Nat(value) => return Outcome::Done(Value::Nat(*value)),
                    PurePrim::Int(value) => return Outcome::Done(Value::Int(*value)),
                    PurePrim::Flt(value) => return Outcome::Done(Value::Flt(*value)),
                    PurePrim::Io(token) => return Outcome::Done(Value::Io(*token)),
                    PurePrim::Bin(bytes) => {
                        return Outcome::Done(Value::Bin(Rc::new(bytes.clone())));
                    }
                    PurePrim::LstMap(source, mapper) => {
                        return self.eval_map(source, mapper, env);
                    }
                    _ => {}
                }

                let mut operands = Vec::new();
                for operand in prim.operands() {
                    match self.value_of(operand, env) {
                        Ok(value) => operands.push(value),
                        Err(bail) => return Outcome::Bail(bail),
                    }
                }
                match apply_pure(pure, &operands) {
                    Ok(value) => Outcome::Done(value),
                    Err(bail) => Outcome::Bail(bail),
                }
            }
        }
    }

    fn eval_map(&mut self, source: &'m Term, mapper: &'m Term, env: &mut Env<'m>) -> Outcome<'m> {
        let source = match self.value_of(source, env) {
            Ok(value) => value,
            Err(bail) => return Outcome::Bail(bail),
        };
        let mapper = match self.value_of(mapper, env) {
            Ok(value) => value,
            Err(bail) => return Outcome::Bail(bail),
        };
        let (Value::Lst(elements), Value::Clsr(closure)) = (source, mapper) else {
            return Outcome::Bail(Bail::Unsupported);
        };

        let mut mapped = Vec::with_capacity(elements.len());
        for element in elements.iter() {
            if let Err(bail) = self.charge() {
                return Outcome::Bail(bail);
            }
            match self.enter(&closure, vec![element.clone()], false) {
                Outcome::Done(value) => mapped.push(value),
                Outcome::Stuck(_) => unreachable!("non-tail evaluation never residualizes"),
                Outcome::Bail(bail) => return Outcome::Bail(bail),
            }
        }
        Outcome::Done(Value::Lst(Rc::new(mapped)))
    }

    // --- Reification ------------------------------------------------------------

    /// A value as a freshly built term. Data is a literal; a closure is its
    /// `Func` syntax copied verbatim with `Let`s binding the resolved local
    /// captures around it (module-item captures stay free — they are globally
    /// valid names). The budget bounds nodes and payload bytes; `visiting`
    /// breaks closure-value cycles (a local `rec` member escaping as a result).
    fn reify(
        &self,
        value: &Value<'m>,
        budget: &mut ReifyBudget,
        visiting: &mut Vec<*const Func>,
    ) -> Result<Term, Bail> {
        budget.node()?;
        Ok(match value {
            Value::Erased => Subterm::Erased.into(),
            Value::Nat(value) => Subterm::Prim(Prim::Pure(PurePrim::Nat(*value))).into(),
            Value::Int(value) => Subterm::Prim(Prim::Pure(PurePrim::Int(*value))).into(),
            Value::Flt(value) => Subterm::Prim(Prim::Pure(PurePrim::Flt(*value))).into(),
            Value::Io(token) => Subterm::Prim(Prim::Pure(PurePrim::Io(*token))).into(),
            Value::Tag(index) => Subterm::Atom(Atom { index: *index }).into(),
            Value::Bin(bytes) => {
                budget.payload(bytes.len())?;
                Subterm::Prim(Prim::Pure(PurePrim::Bin(bytes.as_ref().clone()))).into()
            }
            Value::Lst(elements) => {
                budget.payload(elements.len())?;
                let mut reified = Vec::with_capacity(elements.len());
                for element in elements.iter() {
                    reified.push(self.reify(element, budget, visiting)?);
                }
                Subterm::Prim(Prim::Pure(PurePrim::Lst(reified))).into()
            }
            Value::Tuple(fields) => {
                let mut reified = Vec::with_capacity(fields.len());
                for field in fields.iter() {
                    reified.push(self.reify(field, budget, visiting)?);
                }
                Subterm::Tuple(Tuple { fields: reified }).into()
            }
            Value::Clsr(closure) => {
                let key = closure.func as *const Func;
                if visiting.contains(&key) {
                    return Err(Bail::Cycle);
                }
                visiting.push(key);

                budget.nodes(term_size(&closure.func.body))?;
                let result: Term = Subterm::Func(Func {
                    captures: clone_args(&closure.func.captures),
                    params: clone_args(&closure.func.params),
                    body: deep_copy(&closure.func.body),
                })
                .into();

                // Reify the captured environment as one grouped `Let` in env
                // order — `env[0]` is the outermost binding, and each later
                // capture may reference the ones before it.
                let mut bindings = Vec::with_capacity(closure.env.borrow().len());
                for (name, captured) in closure.env.borrow().iter() {
                    let body = self.reify(captured, budget, visiting)?;
                    bindings.push((name.clone(), body));
                }

                let result = match bindings.is_empty() {
                    true => result,
                    false => Subterm::Let(Let {
                        bindings,
                        tail: result,
                    })
                    .into(),
                };

                visiting.pop();
                result
            }
        })
    }
}

fn term_size(term: &Term) -> usize {
    1 + children(term).into_iter().map(term_size).sum::<usize>()
}

struct ReifyBudget {
    nodes: usize,
    payload: usize,
}

impl ReifyBudget {
    fn new() -> Self {
        Self {
            nodes: MAX_REIFY_NODES,
            payload: MAX_REIFY_BYTES,
        }
    }

    fn node(&mut self) -> Result<(), Bail> {
        self.nodes(1)
    }

    fn nodes(&mut self, count: usize) -> Result<(), Bail> {
        if self.nodes < count {
            return Err(Bail::TooBig);
        }
        self.nodes -= count;
        Ok(())
    }

    fn payload(&mut self, count: usize) -> Result<(), Bail> {
        if self.payload < count {
            return Err(Bail::TooBig);
        }
        self.payload -= count;
        Ok(())
    }
}

// --- Pure-primitive semantics ---------------------------------------------------
//
// A row-by-row transcription of `curios-cont/src/optimize/scalar_eval.rs` for
// ersd's primitive set: 31-bit `Nat`/`Int` carriers, monus subtraction,
// value-dependent traps (`Err(Bail::Trap)` — the fold is declined so the
// runtime trap survives), NaN-refusing float min/max, and truncating
// float-to-integer casts. Any semantic change there must land here too.

fn apply_pure<'m>(prim: &PurePrim, operands: &[Value<'m>]) -> Result<Value<'m>, Bail> {
    use PurePrim::*;

    let nat = |index: usize| operands[index].nat();
    let int = |index: usize| operands[index].int();
    let flt = |index: usize| operands[index].flt();
    let bin = |index: usize| operands[index].bin();
    let lst = |index: usize| operands[index].lst();
    let bln = |value: bool| Ok(Value::Nat(value as u32));

    match prim {
        // Nat — 31-bit unsigned; add/mul trap on overflow, sub is monus,
        // div/rem trap on a zero divisor.
        NatAdd(..) => fits31u(nat(0)? as u64 + nat(1)? as u64).map(Value::Nat),
        NatSub(..) => Ok(Value::Nat(nat(0)?.saturating_sub(nat(1)?))),
        NatMul(..) => fits31u(nat(0)? as u64 * nat(1)? as u64).map(Value::Nat),
        NatDiv(..) => Ok(Value::Nat(nat(0)? / nonzero_u(nat(1)?)?)),
        NatRem(..) => Ok(Value::Nat(nat(0)? % nonzero_u(nat(1)?)?)),
        NatAnd(..) => Ok(Value::Nat(nat(0)? & nat(1)?)),
        NatOr(..) => Ok(Value::Nat(nat(0)? | nat(1)?)),
        NatXor(..) => Ok(Value::Nat(nat(0)? ^ nat(1)?)),
        NatShl(..) => Ok(Value::Nat(nat(0)?.wrapping_shl(nat(1)?) & 0x7FFF_FFFF)),
        NatShr(..) => Ok(Value::Nat(nat(0)?.wrapping_shr(nat(1)?))),
        NatEql(..) => bln(nat(0)? == nat(1)?),
        NatNeq(..) => bln(nat(0)? != nat(1)?),
        NatLt(..) => bln(nat(0)? < nat(1)?),
        NatGt(..) => bln(nat(0)? > nat(1)?),
        NatLte(..) => bln(nat(0)? <= nat(1)?),
        NatGte(..) => bln(nat(0)? >= nat(1)?),

        // Int — 31-bit signed; arithmetic traps on overflow, div/rem on zero.
        IntAdd(..) => fits31s(int(0)? as i64 + int(1)? as i64).map(Value::Int),
        IntSub(..) => fits31s(int(0)? as i64 - int(1)? as i64).map(Value::Int),
        IntMul(..) => fits31s(int(0)? as i64 * int(1)? as i64).map(Value::Int),
        IntDiv(..) => fits31s(int(0)? as i64 / nonzero_s(int(1)?)? as i64).map(Value::Int),
        IntRem(..) => Ok(Value::Int(int(0)? % nonzero_s(int(1)?)?)),
        IntAnd(..) => Ok(Value::Int(wrap31s(int(0)? & int(1)?))),
        IntOr(..) => Ok(Value::Int(wrap31s(int(0)? | int(1)?))),
        IntXor(..) => Ok(Value::Int(wrap31s(int(0)? ^ int(1)?))),
        IntShl(..) => Ok(Value::Int(wrap31s(int(0)?.wrapping_shl(int(1)? as u32)))),
        IntShr(..) => Ok(Value::Int(wrap31s(int(0)?.wrapping_shr(int(1)? as u32)))),
        IntEql(..) => bln(int(0)? == int(1)?),
        IntNeq(..) => bln(int(0)? != int(1)?),
        IntLt(..) => bln(int(0)? < int(1)?),
        IntGt(..) => bln(int(0)? > int(1)?),
        IntLte(..) => bln(int(0)? <= int(1)?),
        IntGte(..) => bln(int(0)? >= int(1)?),

        // Flt — f32, total; min/max fold only when neither operand is NaN (the
        // one case wasm and Rust diverge on the value); nearest is half-to-even.
        FltAdd(..) => Ok(Value::Flt(flt(0)? + flt(1)?)),
        FltSub(..) => Ok(Value::Flt(flt(0)? - flt(1)?)),
        FltMul(..) => Ok(Value::Flt(flt(0)? * flt(1)?)),
        FltDiv(..) => Ok(Value::Flt(flt(0)? / flt(1)?)),
        FltRem(..) => Ok(Value::Flt(flt(0)? % flt(1)?)),
        FltNeg(..) => Ok(Value::Flt(-flt(0)?)),
        FltAbs(..) => Ok(Value::Flt(flt(0)?.abs())),
        FltSqrt(..) => Ok(Value::Flt(flt(0)?.sqrt())),
        FltFloor(..) => Ok(Value::Flt(flt(0)?.floor())),
        FltCeil(..) => Ok(Value::Flt(flt(0)?.ceil())),
        FltTrunc(..) => Ok(Value::Flt(flt(0)?.trunc())),
        FltNearest(..) => Ok(Value::Flt(flt(0)?.round_ties_even())),
        FltMin(..) => flt_minmax(flt(0)?, flt(1)?, f32::min),
        FltMax(..) => flt_minmax(flt(0)?, flt(1)?, f32::max),
        FltEql(..) => bln(flt(0)? == flt(1)?),
        FltNeq(..) => bln(flt(0)? != flt(1)?),
        FltLt(..) => bln(flt(0)? < flt(1)?),
        FltGt(..) => bln(flt(0)? > flt(1)?),
        FltLte(..) => bln(flt(0)? <= flt(1)?),
        FltGte(..) => bln(flt(0)? >= flt(1)?),

        // Conversions — the int↔int casts reinterpret the 31-bit payload; the
        // float→integer casts truncate toward zero and trap-check the range.
        NatToInt(..) => Ok(Value::Int(wrap31s(nat(0)? as i32))),
        NatToFlt(..) => Ok(Value::Flt(nat(0)? as f32)),
        IntToNat(..) => Ok(Value::Nat(int(0)? as u32 & 0x7FFF_FFFF)),
        IntToFlt(..) => Ok(Value::Flt(int(0)? as f32)),
        FltToNat(..) => flt_to_nat(flt(0)?),
        FltToInt(..) => flt_to_int(flt(0)?),
        FltToLeBin(..) => Ok(Value::Bin(Rc::new(flt(0)?.to_le_bytes().to_vec()))),

        // Bin — bounds-checked reads trap; append truncates the byte like the
        // backend; concat and equality are total.
        BinLen(..) => fits31u(bin(0)?.len() as u64).map(Value::Nat),
        BinEql(..) => bln(bin(0)?.as_slice() == bin(1)?.as_slice()),
        BinGet(..) => match bin(0)?.get(nat(1)? as usize) {
            Some(byte) => Ok(Value::Nat(*byte as u32)),
            None => Err(Bail::Trap),
        },
        BinSlice(..) => {
            let bytes = bin(0)?;
            let (start, end) = (nat(1)? as usize, nat(2)? as usize);
            if start <= end && end <= bytes.len() {
                Ok(Value::Bin(Rc::new(bytes[start..end].to_vec())))
            } else {
                Err(Bail::Trap)
            }
        }
        BinAppend(..) => {
            let mut bytes = bin(0)?.as_ref().clone();
            bytes.push(nat(1)? as u8);
            Ok(Value::Bin(Rc::new(bytes)))
        }
        BinConcat(..) => {
            let mut bytes = Vec::new();
            for index in 0..operands.len() {
                bytes.extend_from_slice(bin(index)?);
            }
            Ok(Value::Bin(Rc::new(bytes)))
        }

        // Lst — same discipline over elements.
        Lst(..) => Ok(Value::Lst(Rc::new(operands.to_vec()))),
        LstLen(..) => fits31u(lst(0)?.len() as u64).map(Value::Nat),
        LstGet(..) => match lst(0)?.get(nat(1)? as usize) {
            Some(element) => Ok(element.clone()),
            None => Err(Bail::Trap),
        },
        LstSlice(..) => {
            let elements = lst(0)?;
            let (start, end) = (nat(1)? as usize, nat(2)? as usize);
            if start <= end && end <= elements.len() {
                Ok(Value::Lst(Rc::new(elements[start..end].to_vec())))
            } else {
                Err(Bail::Trap)
            }
        }
        LstAppend(..) => {
            let mut elements = lst(0)?.as_ref().clone();
            elements.push(operands[1].clone());
            Ok(Value::Lst(Rc::new(elements)))
        }
        LstConcat(..) => {
            let mut elements = Vec::new();
            for index in 0..operands.len() {
                elements.extend_from_slice(lst(index)?);
            }
            Ok(Value::Lst(Rc::new(elements)))
        }

        IoEql(..) => match (&operands[0], &operands[1]) {
            (Value::Io(left), Value::Io(right)) => bln(left == right),
            _ => Err(Bail::Unsupported),
        },

        // Literals and `LstMap` are handled before operand evaluation.
        Nat(..) | Int(..) | Flt(..) | Bin(..) | Io(..) | LstMap(..) => {
            unreachable!("handled in eval_prim")
        }
    }
}

fn flt_minmax<'m>(left: f32, right: f32, op: fn(f32, f32) -> f32) -> Result<Value<'m>, Bail> {
    if left.is_nan() || right.is_nan() {
        return Err(Bail::Unsupported);
    }
    Ok(Value::Flt(op(left, right)))
}

fn flt_to_nat<'m>(value: f32) -> Result<Value<'m>, Bail> {
    let truncated = value.trunc();
    if value.is_finite() && truncated > -1.0 && truncated < 2_147_483_648.0 {
        Ok(Value::Nat(truncated as u32))
    } else {
        Err(Bail::Trap)
    }
}

fn flt_to_int<'m>(value: f32) -> Result<Value<'m>, Bail> {
    let truncated = value.trunc();
    if value.is_finite() && (-1_073_741_824.0..1_073_741_824.0).contains(&truncated) {
        Ok(Value::Int(truncated as i32))
    } else {
        Err(Bail::Trap)
    }
}

fn wrap31s(value: i32) -> i32 {
    value.wrapping_shl(1) >> 1
}

fn fits31u(value: u64) -> Result<u32, Bail> {
    if value < (1 << 31) {
        Ok(value as u32)
    } else {
        Err(Bail::Trap)
    }
}

fn fits31s(value: i64) -> Result<i32, Bail> {
    if ((-(1 << 30))..(1 << 30)).contains(&value) {
        Ok(value as i32)
    } else {
        Err(Bail::Trap)
    }
}

fn nonzero_u(divisor: u32) -> Result<u32, Bail> {
    if divisor == 0 {
        Err(Bail::Trap)
    } else {
        Ok(divisor)
    }
}

fn nonzero_s(divisor: i32) -> Result<i32, Bail> {
    if divisor == 0 {
        Err(Bail::Trap)
    } else {
        Ok(divisor)
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{Argument, Atom, CellPrim, Match, Name},
    };

    fn nat(value: u32) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::Nat(value))).into()
    }

    fn atom(index: usize) -> Term {
        Subterm::Atom(Atom { index }).into()
    }

    // `dispatch(x) = match x | @0 => 100 | _ => 999 end` — a sparse constructor
    // switch with a single enumerated arm and a catch-all default.
    fn dispatch() -> Item {
        item(
            "dispatch",
            func(
                vec![],
                vec!["x"],
                Subterm::Match(Match {
                    head: name("x"),
                    cases: [(0usize, nat(100))].into_iter().collect(),
                    default: Some(nat(999)),
                })
                .into(),
            ),
        )
    }

    fn flt(value: f32) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::Flt(value))).into()
    }

    fn bin(bytes: Vec<u8>) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::Bin(bytes))).into()
    }

    fn name(named: &str) -> Term {
        Subterm::Name(Name::from(named)).into()
    }

    fn apply(head: Term, params: Vec<Term>) -> Term {
        Subterm::Apply(Apply { head, params }).into()
    }

    fn func(captures: Vec<&str>, params: Vec<&str>, body: Term) -> Term {
        Subterm::Func(Func {
            captures: captures.into_iter().map(Argument::from).collect(),
            params: params.into_iter().map(Argument::from).collect(),
            body,
        })
        .into()
    }

    fn let_(bound: &str, body: Term, tail: Term) -> Term {
        Subterm::Let(Let {
            bindings: vec![(bound.to_owned(), body)],
            tail,
        })
        .into()
    }

    fn prim(pure: PurePrim) -> Term {
        Subterm::Prim(Prim::Pure(pure)).into()
    }

    fn item(named: &str, body: Term) -> Item {
        Item::Let {
            name: named.to_owned(),
            body,
        }
    }

    /// `double(n) = n + n` — the workhorse pure callee.
    fn double() -> Item {
        item(
            "double",
            func(
                vec![],
                vec!["n"],
                prim(PurePrim::NatAdd(name("n"), name("n"))),
            ),
        )
    }

    /// `w(b) = let r = Io.exit(b); r` — an effect in non-tail position, so the
    /// callee itself can never residualize; only a call boundary can.
    fn effectful_callee() -> Item {
        item(
            "w",
            func(
                vec![],
                vec!["b"],
                let_(
                    "r",
                    Subterm::Prim(Prim::Host(HostPrim::IoExit(name("b")))).into(),
                    name("r"),
                ),
            ),
        )
    }

    fn run(mut module: Module) -> String {
        evaluate_closed_terms(&mut module);
        format!("{module}")
    }

    fn unchanged(mut module: Module) {
        let before = format!("{module}");
        evaluate_closed_terms(&mut module);
        assert_eq!(format!("{module}"), before, "expected the module untouched");
    }

    #[test]
    fn folds_a_closed_pure_call() {
        let module = Module {
            items: vec![double()],
            body: apply(name("double"), vec![nat(21)]),
        };
        let printed = run(module);
        assert!(printed.contains("42n"), "{printed}");
        assert!(!printed.contains("#double(21n)"), "{printed}");
    }

    #[test]
    fn folds_through_curried_closures() {
        // k(a) = (b) => a; k(1)(2) = 1.
        let module = Module {
            items: vec![item(
                "k",
                func(vec![], vec!["a"], func(vec!["a"], vec!["b"], name("a"))),
            )],
            body: apply(apply(name("k"), vec![nat(1)]), vec![nat(2)]),
        };
        let printed = run(module);
        assert!(!printed.contains("#k("), "{printed}");
        assert!(
            printed.ends_with("1n") || printed.contains("1n\n"),
            "{printed}"
        );
    }

    #[test]
    fn folds_a_caf_item_body() {
        let module = Module {
            items: vec![
                double(),
                item("table", apply(name("double"), vec![nat(21)])),
            ],
            body: name("table"),
        };
        let printed = run(module);
        assert!(printed.contains("42n"), "{printed}");
        assert!(!printed.contains("#double(21n)"), "{printed}");
    }

    #[test]
    fn folds_sparse_match_absent_tag_to_default() {
        // `dispatch(@1)` — tag 1 is not enumerated, so the catch-all wins. The
        // module body (last line) is the folded result; the retained `dispatch`
        // item still spells both literals, so assert on the tail, not `contains`.
        let module = Module {
            items: vec![dispatch()],
            body: apply(name("dispatch"), vec![atom(1)]),
        };
        let printed = run(module);
        assert!(
            !printed.contains("#dispatch("),
            "call not folded:\n{printed}"
        );
        assert!(printed.trim_end().ends_with("999n"), "{printed}");
    }

    #[test]
    fn folds_sparse_match_present_tag_to_its_arm() {
        // `dispatch(@0)` — the enumerated arm wins over the default.
        let module = Module {
            items: vec![dispatch()],
            body: apply(name("dispatch"), vec![atom(0)]),
        };
        let printed = run(module);
        assert!(
            !printed.contains("#dispatch("),
            "call not folded:\n{printed}"
        );
        assert!(printed.trim_end().ends_with("100n"), "{printed}");
    }

    #[test]
    fn folds_nat_induction() {
        // fact(n) via Nat.fold: 5! = 120.
        let module = Module {
            items: vec![item(
                "fact",
                func(
                    vec![],
                    vec!["n"],
                    Subterm::NatMatch(NatMatch::Induction {
                        head: name("n"),
                        zero_case: nat(1),
                        pred: "p".to_owned(),
                        ih: "ih".to_owned(),
                        succ_case: prim(PurePrim::NatMul(
                            prim(PurePrim::NatAdd(name("p"), nat(1))),
                            name("ih"),
                        )),
                    })
                    .into(),
                ),
            )],
            body: apply(name("fact"), vec![nat(5)]),
        };
        let printed = run(module);
        assert!(printed.contains("120n"), "{printed}");
    }

    #[test]
    fn folds_lst_map() {
        let module = Module {
            items: vec![item(
                "inc",
                func(vec![], vec!["e"], prim(PurePrim::NatAdd(name("e"), nat(1)))),
            )],
            body: apply(
                name("use"),
                vec![prim(PurePrim::LstMap(
                    prim(PurePrim::Lst(vec![nat(1), nat(2)])),
                    name("inc"),
                ))],
            ),
        };
        // Wrap in an identity call so the candidate is an `Apply`.
        let module = Module {
            items: {
                let mut items = module.items;
                items.push(item("use", func(vec![], vec!["l"], name("l"))));
                items
            },
            body: module.body,
        };
        let printed = run(module);
        assert!(printed.contains("[2n, 3n]"), "{printed}");
    }

    #[test]
    fn reifies_constructor_data() {
        // mk(n) = (@0, n, "hi") — a flat erased constructor tuple.
        let module = Module {
            items: vec![item(
                "mk",
                func(
                    vec![],
                    vec!["n"],
                    Subterm::Tuple(Tuple {
                        fields: vec![
                            Subterm::Atom(Atom { index: 0 }).into(),
                            name("n"),
                            bin(b"hi".to_vec()),
                        ],
                    })
                    .into(),
                ),
            )],
            body: apply(name("mk"), vec![nat(2)]),
        };
        let printed = run(module);
        assert!(printed.contains("(@0, 2n, \\68\\69)"), "{printed}");
    }

    #[test]
    fn reifies_a_closure_with_let_bound_captures() {
        // adder(n) = (m) => n + m; adder(4) is a closure capturing n = 4.
        let module = Module {
            items: vec![item(
                "adder",
                func(
                    vec![],
                    vec!["n"],
                    func(
                        vec!["n"],
                        vec!["m"],
                        prim(PurePrim::NatAdd(name("n"), name("m"))),
                    ),
                ),
            )],
            body: apply(name("adder"), vec![nat(4)]),
        };
        let printed = run(module);
        assert!(printed.contains("4n"), "{printed}");
        assert!(printed.contains("{#n} (#m) =>"), "{printed}");
        assert!(!printed.contains("#adder(4n)"), "{printed}");
    }

    #[test]
    fn residualizes_a_tail_host_prim() {
        let module = Module {
            items: vec![item(
                "boom",
                func(
                    vec![],
                    vec!["c"],
                    Subterm::Prim(Prim::Host(HostPrim::IoExit(name("c")))).into(),
                ),
            )],
            body: apply(name("boom"), vec![prim(PurePrim::NatAdd(nat(1), nat(2)))]),
        };
        let printed = run(module);
        assert!(printed.contains("Io.exit 3n"), "{printed}");
        assert!(!printed.contains("#boom("), "{printed}");
    }

    #[test]
    fn residualizes_at_a_tainted_call_boundary() {
        // p(x) = w(x + 1): entering w bails on its non-tail effect, and the
        // tail call to the item `w` becomes the residual — with the pure
        // prefix folded into its argument.
        let module = Module {
            items: vec![
                effectful_callee(),
                item(
                    "p",
                    func(
                        vec![],
                        vec!["x"],
                        apply(name("w"), vec![prim(PurePrim::NatAdd(name("x"), nat(1)))]),
                    ),
                ),
            ],
            body: apply(name("p"), vec![nat(4)]),
        };
        let printed = run(module);
        assert!(printed.contains("#w(5n)"), "{printed}");
        assert!(!printed.contains("#p(4n)"), "{printed}");
    }

    #[test]
    fn keeps_an_existing_residual_stable() {
        // `#w(5n)` is already residual-shaped: re-evaluating it produces the
        // identical term, which must be kept, not churned.
        let module = Module {
            items: vec![effectful_callee()],
            body: apply(name("w"), vec![nat(5)]),
        };
        unchanged(module);
    }

    #[test]
    fn never_residualizes_a_cell_prim() {
        // cget(x) = Cell.get x — an effect whose identity is its program
        // point; the boundary residual reproduces the call, so the module is
        // stable, and no `Cell.get` literal ever replaces the body.
        let module = Module {
            items: vec![item(
                "cget",
                func(
                    vec![],
                    vec!["x"],
                    Subterm::Prim(Prim::Cell(CellPrim::Get(name("x")))).into(),
                ),
            )],
            body: apply(name("cget"), vec![nat(0)]),
        };
        unchanged(module);
    }

    #[test]
    fn leaves_open_subterms_and_folds_closed_ones_under_a_binder() {
        let module = Module {
            items: vec![double()],
            body: func(
                vec![],
                vec!["x"],
                Subterm::Tuple(Tuple {
                    fields: vec![
                        apply(name("double"), vec![name("x")]),
                        apply(name("double"), vec![nat(3)]),
                    ],
                })
                .into(),
            ),
        };
        let printed = run(module);
        assert!(printed.contains("#double(#x)"), "{printed}");
        assert!(printed.contains("6n"), "{printed}");
        assert!(!printed.contains("#double(3n)"), "{printed}");
    }

    #[test]
    fn bails_on_fuel_exhaustion() {
        let module = Module {
            items: vec![Item::Rec {
                names: vec!["loop".to_owned()],
                items: vec![func(
                    vec!["loop"],
                    vec!["n"],
                    apply(name("loop"), vec![name("n")]),
                )],
            }],
            body: apply(name("loop"), vec![nat(0)]),
        };
        unchanged(module);
    }

    #[test]
    fn bails_on_every_would_trap_row() {
        // One row per value-dependent trap in the scalar_eval table (restricted
        // to ersd's primitive set): folding any of these would erase the trap.
        let rows: Vec<Term> = vec![
            prim(PurePrim::NatAdd(nat(2_147_483_647), nat(1))),
            prim(PurePrim::NatMul(nat(1 << 16), nat(1 << 16))),
            prim(PurePrim::NatDiv(nat(1), nat(0))),
            prim(PurePrim::NatRem(nat(1), nat(0))),
            prim(PurePrim::IntAdd(
                Subterm::Prim(Prim::Pure(PurePrim::Int(1_073_741_823))).into(),
                Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
            )),
            prim(PurePrim::IntDiv(
                Subterm::Prim(Prim::Pure(PurePrim::Int(1))).into(),
                Subterm::Prim(Prim::Pure(PurePrim::Int(0))).into(),
            )),
            prim(PurePrim::BinGet(bin(b"ab".to_vec()), nat(5))),
            prim(PurePrim::BinSlice(bin(b"ab".to_vec()), nat(1), nat(9))),
            prim(PurePrim::LstGet(prim(PurePrim::Lst(vec![nat(1)])), nat(3))),
            prim(PurePrim::FltToNat(flt(1e10))),
            prim(PurePrim::FltToInt(flt(f32::NAN))),
            prim(PurePrim::FltMin(flt(f32::NAN), flt(1.0))),
        ];
        for row in rows {
            let module = Module {
                items: vec![item("t", func(vec![], vec![], row))],
                body: apply(name("t"), vec![]),
            };
            unchanged(module);
        }
    }

    #[test]
    fn declines_an_oversized_reification() {
        let module = Module {
            items: vec![item(
                "big",
                func(
                    vec![],
                    vec![],
                    prim(PurePrim::BinConcat(vec![
                        bin(vec![0u8; 40_000]),
                        bin(vec![0u8; 40_000]),
                    ])),
                ),
            )],
            body: apply(name("big"), vec![]),
        };
        unchanged(module);
    }

    #[test]
    fn never_forces_an_effectful_caf_into_a_residual() {
        // `init` is an effectful CAF; `user()` forces it. The forcing fails
        // hard (never converted at the call boundary), so nothing changes.
        let module = Module {
            items: vec![
                item(
                    "init",
                    Subterm::Prim(Prim::Cell(CellPrim::New(nat(0)))).into(),
                ),
                item("user", func(vec![], vec![], name("init"))),
            ],
            body: apply(name("user"), vec![]),
        };
        unchanged(module);
    }
}
