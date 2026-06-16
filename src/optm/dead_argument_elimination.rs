use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// Dead argument elimination: drop parameters a body never *really* uses,
/// along with the matching arguments at every coordinated site.
///
/// After type erasure, erased type arguments survive as unit values that are
/// passed but never used. This pass removes them, finishing what erasure starts.
/// Three argument kinds can be removed soundly, because each is positional and
/// its reference sites are findable by name:
///
/// - **Function parameters.** A `Func` is referenced only by `Direct` calls, so a
///   dead parameter can be dropped from the definition and from the argument
///   list of every `Direct` call to that name.
/// - **Block parameters.** A block is jumped to only from within its own body
///   (`Jump` tails and `Match` edges), so a dead parameter can be dropped from
///   the block and from every edge targeting it. Blocks named as a call's
///   `resume` are pinned: their parameter is bound by the call result, whose
///   arity is fixed.
/// - **Closure captures.** A closure's captured `fields` live in its own env
///   struct — *not* in the indirect dispatch signature — and are written only by
///   `Pure(Data::Clsr(c, captures))`. A field unused in the body can be dropped
///   from the closure and from every such construction of it.
///
/// Closure *parameters* are deliberately left alone: indirect dispatch types are
/// keyed by arity, so a parameter slot could only be dropped uniformly across all
/// closures and indirect call sites of that arity. Closure lifting and inlining
/// already turn most type-carrying applications into `Direct` calls, where the
/// former closure parameters become function parameters and are reachable here.
///
/// # Cycle-aware liveness
///
/// "Dead" is not "never occurs": an argument that is only *fed back around a
/// recursion* — passed at its own position in a direct self-call, or around a
/// converted loop's header block — sustains itself without ever doing work.
/// The devirtualized self-closure a recursive function threads through its own
/// loop is the motivating case: nothing calls it anymore, it is merely passed.
///
/// Deadness is therefore an optimistic fixed point over *positions* (function
/// parameters and block parameters): every position starts dead, an occurrence
/// in a value operand, match scrutinee, indirect call, host argument, or
/// resume-sentinel jump is a *real* use that marks its binding position live,
/// and an occurrence as the `j`-th argument of a direct call or jump marks its
/// binding position live only once the receiving position `j` is live. The
/// entrypoint's parameters and call-resume block parameters seed the live set.
/// Pass-through cycles — self-feeding loops included — never get marked, and
/// are trimmed away.
///
/// The pass still runs to an outer fixed point so removals interleave with the
/// capture rule: dropping a dead capture can erase a parameter's last real use
/// for the next round, and vice versa. The trailing dead-code pass then sweeps
/// the now-unreferenced argument bindings.
pub fn eliminate_dead_arguments(module: &mut Module) {
    loop {
        let dropped_params = drop_dead_positions(module);
        let dropped_fields = drop_dead_clsr_fields(module);

        if !(dropped_params || dropped_fields) {
            return;
        }
    }
}

// --- Function and block parameters ------------------------------------------

/// A trimmable parameter position: a function's, or a block's within one body
/// (blocks are body-local, so block positions carry the body's index in the
/// funcs-then-clsrs enumeration).
#[derive(Clone, PartialEq, Eq, Hash)]
enum Position {
    Func(FuncName, usize),
    Block(usize, BlockName, usize),
}

/// The optimistic liveness solver: positions start dead; [`Liveness::mark`]
/// makes one live, and [`Liveness::edge`] defers a source position on a target
/// position's liveness — fired immediately if the target is already live,
/// recorded otherwise and discharged by [`Liveness::solve`].
#[derive(Default)]
struct Liveness {
    live: HashSet<Position>,
    deps: HashMap<Position, Vec<Position>>,
    worklist: Vec<Position>,
}

impl Liveness {
    fn mark(&mut self, position: Position) {
        if self.live.insert(position.clone()) {
            self.worklist.push(position);
        }
    }

    fn edge(&mut self, when: Position, then: Position) {
        if self.live.contains(&when) {
            self.mark(then);
        } else {
            self.deps.entry(when).or_default().push(then);
        }
    }

    fn solve(&mut self) {
        while let Some(position) = self.worklist.pop() {
            if let Some(dependents) = self.deps.remove(&position) {
                for dependent in dependents {
                    self.mark(dependent);
                }
            }
        }
    }
}

/// One body in the funcs-then-clsrs enumeration: its region, resume sentinel,
/// and — for functions — the name and parameters that join the position lattice.
struct Body<'a> {
    func: Option<(&'a FuncName, &'a [Argument])>,
    region: &'a Region,
    resume: &'a BlockName,
}

fn bodies(module: &Module) -> Vec<Body<'_>> {
    let funcs = module.funcs().iter().map(|(name, func)| Body {
        func: Some((name, &func.params)),
        region: &func.region,
        resume: &func.resume,
    });
    let clsrs = module.clsrs().iter().map(|(_, clsr)| Body {
        func: None,
        region: &clsr.region,
        resume: &clsr.resume,
    });

    funcs.chain(clsrs).collect()
}

fn drop_dead_positions(module: &mut Module) -> bool {
    let (func_drops, block_drops) = dead_parameter_positions(module);

    if func_drops.is_empty() && block_drops.iter().all(|drops| drops.is_empty()) {
        return false;
    }

    // Trim function signatures and every `Direct` call to them.
    for (name, func) in module.funcs_mut() {
        if let Some(dead) = func_drops.get(name) {
            retain_kept(&mut func.params, dead);
        }
    }
    for (_, func) in module.funcs_mut() {
        trim_direct_args(&mut func.region, &func_drops);
    }
    for (_, clsr) in module.clsrs_mut() {
        trim_direct_args(&mut clsr.region, &func_drops);
    }

    // Trim block signatures and every jump/match edge to them, per body, in
    // the same funcs-then-clsrs order the drops were computed in.
    let funcs = module.funcs_mut().len();
    for (index, (_, func)) in module.funcs_mut().iter_mut().enumerate() {
        trim_block_args(&mut func.region, &block_drops[index]);
    }
    for (index, (_, clsr)) in module.clsrs_mut().iter_mut().enumerate() {
        trim_block_args(&mut clsr.region, &block_drops[funcs + index]);
    }

    true
}

/// Solve the liveness fixed point and return the dead positions: per-function
/// parameter drops and per-body block-parameter drops (indexed like [`bodies`]).
#[allow(clippy::type_complexity)]
fn dead_parameter_positions(
    module: &Module,
) -> (
    HashMap<FuncName, HashSet<usize>>,
    Vec<HashMap<BlockName, HashSet<usize>>>,
) {
    let bodies = bodies(module);
    let mut liveness = Liveness::default();

    // The entrypoint's signature is the host's entry contract.
    if let Some(entry) = module.entry()
        && let Some(params) = bodies
            .iter()
            .find_map(|body| body.func.filter(|(name, _)| *name == entry))
            .map(|(_, params)| params)
    {
        for index in 0..params.len() {
            liveness.mark(Position::Func(entry.clone(), index));
        }
    }

    let mut all_blocks: Vec<HashMap<BlockName, usize>> = Vec::with_capacity(bodies.len());

    for (index, body) in bodies.iter().enumerate() {
        // Binders: every name bound at a trimmable position in this body.
        let mut binders = HashMap::new();
        if let Some((name, params)) = body.func {
            for (position, param) in params.iter().enumerate() {
                binders.insert(param.name.clone(), Position::Func(name.clone(), position));
            }
        }
        let mut arities = HashMap::new();
        collect_block_binders(body.region, index, &mut binders, &mut arities);

        // Call-resume blocks are pinned: their parameter is the call result.
        pin_resume_blocks(body.region, index, &arities, &mut liveness);

        constrain_region(body.region, index, body.resume, &binders, &mut liveness);

        all_blocks.push(arities);
    }

    liveness.solve();

    let mut func_drops: HashMap<FuncName, HashSet<usize>> = HashMap::new();
    for body in &bodies {
        if let Some((name, params)) = body.func {
            let dead: HashSet<usize> = (0..params.len())
                .filter(|&position| {
                    !liveness
                        .live
                        .contains(&Position::Func(name.clone(), position))
                })
                .collect();
            if !dead.is_empty() {
                func_drops.insert(name.clone(), dead);
            }
        }
    }

    let block_drops = all_blocks
        .iter()
        .enumerate()
        .map(|(index, arities)| {
            let mut drops: HashMap<BlockName, HashSet<usize>> = HashMap::new();
            for (block, &arity) in arities {
                let dead: HashSet<usize> = (0..arity)
                    .filter(|&position| {
                        !liveness
                            .live
                            .contains(&Position::Block(index, block.clone(), position))
                    })
                    .collect();
                if !dead.is_empty() {
                    drops.insert(block.clone(), dead);
                }
            }
            drops
        })
        .collect();

    (func_drops, block_drops)
}

fn collect_block_binders(
    region: &Region,
    body: usize,
    binders: &mut HashMap<ValueName, Position>,
    arities: &mut HashMap<BlockName, usize>,
) {
    for (name, block) in &region.blocks {
        arities.insert(name.clone(), block.params.len());
        for (position, param) in block.params.iter().enumerate() {
            binders.insert(param.clone(), Position::Block(body, name.clone(), position));
        }
        collect_block_binders(&block.region, body, binders, arities);
    }
}

/// Mark every parameter of every block named as a `Call`/`Host` resume live:
/// it is bound by the call's result, whose arity is not ours to change.
fn pin_resume_blocks(
    region: &Region,
    body: usize,
    arities: &HashMap<BlockName, usize>,
    liveness: &mut Liveness,
) {
    let resume = match &region.tail {
        Tail::Call(CallTarget::Direct { resume, .. })
        | Tail::Call(CallTarget::Indirect { resume, .. }) => Some(resume),
        Tail::Host(host) => Some(host.resume()),
        _ => None,
    };

    if let Some(resume) = resume
        && let Some(&arity) = arities.get(resume)
    {
        for position in 0..arity {
            liveness.mark(Position::Block(body, resume.clone(), position));
        }
    }

    for (_, block) in &region.blocks {
        pin_resume_blocks(&block.region, body, arities, liveness);
    }
}

/// Seeds a real use for every operand the value walker reports, when the name
/// is bound at a trimmable position.
struct Seeder<'a, 'b> {
    binders: &'a HashMap<ValueName, Position>,
    liveness: &'b mut Liveness,
}

impl Sink for Seeder<'_, '_> {
    fn value_use(&mut self, name: &ValueName) {
        if let Some(position) = self.binders.get(name) {
            self.liveness.mark(position.clone());
        }
    }
}

/// Generate the constraints for one region: value operands are real uses;
/// direct-call and jump arguments are deferred on the receiving position;
/// everything else a tail consumes (scrutinees, indirect calls, host args,
/// returns through the resume sentinel) is a real use.
fn constrain_region(
    region: &Region,
    body: usize,
    sentinel: &BlockName,
    binders: &HashMap<ValueName, Position>,
    liveness: &mut Liveness,
) {
    for (_, value) in &region.values {
        walk_value_uses(value, &mut Seeder { binders, liveness });
    }

    let seed = |name: &ValueName, liveness: &mut Liveness| {
        if let Some(position) = binders.get(name) {
            liveness.mark(position.clone());
        }
    };

    match &region.tail {
        Tail::Jump(jump) => {
            constrain_jump(jump, body, sentinel, binders, liveness);
        }
        Tail::Match(target) => {
            seed(&target.operand, liveness);
            for jump in target.cases.values() {
                constrain_jump(jump, body, sentinel, binders, liveness);
            }
            if let Some(jump) = &target.default {
                constrain_jump(jump, body, sentinel, binders, liveness);
            }
        }
        Tail::Call(CallTarget::Direct { target, params, .. }) => {
            for (position, arg) in params.iter().enumerate() {
                if let Some(source) = binders.get(arg) {
                    liveness.edge(Position::Func(target.clone(), position), source.clone());
                }
            }
        }
        Tail::Call(CallTarget::Indirect { target, params, .. }) => {
            seed(target, liveness);
            for arg in params {
                seed(arg, liveness);
            }
        }
        Tail::Host(HostTarget::IoRead { handle, count, .. }) => {
            seed(handle, liveness);
            seed(count, liveness);
        }
        Tail::Host(HostTarget::IoWrite { handle, bytes, .. }) => {
            seed(handle, liveness);
            seed(bytes, liveness);
        }
        Tail::Host(HostTarget::IoOpen { path, mode, .. }) => {
            seed(path, liveness);
            seed(mode, liveness);
        }
        Tail::Host(HostTarget::IoListen { host, port, .. }) => {
            seed(host, liveness);
            seed(port, liveness);
        }
        Tail::Host(HostTarget::IoAccept { handle, .. }) => seed(handle, liveness),
        Tail::Host(HostTarget::IoConnect {
            host,
            port,
            connect_timeout,
            read_timeout,
            write_timeout,
            ..
        }) => {
            seed(host, liveness);
            seed(port, liveness);
            seed(connect_timeout, liveness);
            seed(read_timeout, liveness);
            seed(write_timeout, liveness);
        }
        Tail::Host(HostTarget::IoClose { handle, .. }) => seed(handle, liveness),
        Tail::Host(HostTarget::IoClockWall { .. })
        | Tail::Host(HostTarget::IoClockMono { .. })
        | Tail::Host(HostTarget::IoArgs { .. }) => {}
        Tail::Host(HostTarget::IoRandom { count, .. }) => seed(count, liveness),
        Tail::Host(HostTarget::IoEnv { name, .. }) => seed(name, liveness),
        Tail::Host(HostTarget::IoExit { code, .. }) => seed(code, liveness),
        Tail::Unreachable => {}
    }

    for (_, block) in &region.blocks {
        constrain_region(&block.region, body, sentinel, binders, liveness);
    }
}

/// A jump to the resume sentinel returns the value out of the body — a real
/// use. Any other jump defers each argument on the target block's position.
fn constrain_jump(
    jump: &JumpTarget,
    body: usize,
    sentinel: &BlockName,
    binders: &HashMap<ValueName, Position>,
    liveness: &mut Liveness,
) {
    for (position, arg) in jump.params.iter().enumerate() {
        let Some(source) = binders.get(arg) else {
            continue;
        };

        if &jump.target == sentinel {
            liveness.mark(source.clone());
        } else {
            liveness.edge(
                Position::Block(body, jump.target.clone(), position),
                source.clone(),
            );
        }
    }
}

/// Trim the argument list of every `Direct` call whose target lost parameters.
fn trim_direct_args(region: &mut Region, drops: &HashMap<FuncName, HashSet<usize>>) {
    if let Tail::Call(CallTarget::Direct { target, params, .. }) = &mut region.tail
        && let Some(dead) = drops.get(target)
    {
        retain_kept(params, dead);
    }

    for (_, block) in &mut region.blocks {
        trim_direct_args(&mut block.region, drops);
    }
}

/// Trim the parameter list of every block that lost positions, and the argument
/// list of every `Jump`/`Match` edge targeting one.
fn trim_block_args(region: &mut Region, drops: &HashMap<BlockName, HashSet<usize>>) {
    if drops.is_empty() {
        return;
    }

    match &mut region.tail {
        Tail::Jump(jump) => trim_jump_args(jump, drops),
        Tail::Match(target) => {
            for jump in target.cases.values_mut() {
                trim_jump_args(jump, drops);
            }
            if let Some(jump) = &mut target.default {
                trim_jump_args(jump, drops);
            }
        }
        _ => {}
    }

    for (name, block) in &mut region.blocks {
        if let Some(dead) = drops.get(name) {
            retain_kept(&mut block.params, dead);
        }
        trim_block_args(&mut block.region, drops);
    }
}

fn trim_jump_args(jump: &mut JumpTarget, drops: &HashMap<BlockName, HashSet<usize>>) {
    if let Some(dead) = drops.get(&jump.target) {
        retain_kept(&mut jump.params, dead);
    }
}

// --- Closure captures -------------------------------------------------------

fn drop_dead_clsr_fields(module: &mut Module) -> bool {
    let drops = dead_clsr_fields(module);

    if drops.is_empty() {
        return false;
    }

    for (name, clsr) in module.clsrs_mut() {
        if let Some(dead) = drops.get(name) {
            retain_kept(&mut clsr.fields, dead);
        }
    }

    for (_, data) in module.consts_mut() {
        trim_clsr_data(data, &drops);
    }
    for (_, func) in module.funcs_mut() {
        trim_clsr_captures(&mut func.region, &drops);
    }
    for (_, clsr) in module.clsrs_mut() {
        trim_clsr_captures(&mut clsr.region, &drops);
    }

    true
}

/// The unused captured-field positions of every closure.
fn dead_clsr_fields(module: &Module) -> HashMap<ClsrName, HashSet<usize>> {
    let mut drops = HashMap::new();

    for (name, clsr) in module.clsrs() {
        let used = harvest::value_uses(&clsr.region);
        let dead = dead_positions(&clsr.fields, &used);

        if !dead.is_empty() {
            drops.insert(name.clone(), dead);
        }
    }

    drops
}

/// Trim the capture list of every `Data::Clsr` construction of a trimmed closure.
fn trim_clsr_captures(region: &mut Region, drops: &HashMap<ClsrName, HashSet<usize>>) {
    for (_, value) in &mut region.values {
        if let Value::Pure(data) = value {
            trim_clsr_data(data, drops);
        }
    }

    for (_, block) in &mut region.blocks {
        trim_clsr_captures(&mut block.region, drops);
    }
}

fn trim_clsr_data(data: &mut Data, drops: &HashMap<ClsrName, HashSet<usize>>) {
    if let Data::Clsr(clsr, captures) = data
        && let Some(dead) = drops.get(clsr)
    {
        retain_kept(captures, dead);
    }
}

// --- Shared helpers ---------------------------------------------------------

/// The positions of `args` whose name does not appear in `used`.
fn dead_positions(args: &[Argument], used: &HashSet<ValueName>) -> HashSet<usize> {
    args.iter()
        .enumerate()
        .filter(|(_, arg)| !used.contains(&arg.name))
        .map(|(index, _)| index)
        .collect()
}

/// Drop the `dead` positions from `items`, keeping the rest in order.
fn retain_kept<T>(items: &mut Vec<T>, dead: &HashSet<usize>) {
    let mut index = 0;
    items.retain(|_| {
        let keep = !dead.contains(&index);
        index += 1;
        keep
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
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
            resume: BlockName::from(resume),
            region,
        }
    }

    fn ret(resume: &str, value: ValueName) -> Tail {
        Tail::Jump(JumpTarget {
            target: BlockName::from(resume),
            params: vec![value],
        })
    }

    fn direct(target: &str, args: Vec<ValueName>, resume: &str) -> Tail {
        Tail::Call(CallTarget::Direct {
            target: FuncName::from(target),
            params: args,
            resume: BlockName::from(resume),
        })
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> &'a Func {
        module
            .funcs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, func)| func)
            .expect("function present")
    }

    fn clsr_named<'a>(module: &'a Module, name: &str) -> &'a Clsr {
        module
            .clsrs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, clsr)| clsr)
            .expect("closure present")
    }

    #[test]
    fn drops_unused_param_and_trims_its_direct_calls() {
        // f(a, b) = b; the unused `a` and the matching call argument are dropped.
        let f = func(
            vec![v("a"), v("b")],
            "rf",
            region(vec![], ret("rf", v("b"))),
        );

        let caller = func(
            vec![],
            "rm",
            region(
                vec![
                    (v("x"), Value::Pure(Data::Nat(1))),
                    (v("y"), Value::Pure(Data::Nat(2))),
                ],
                direct("f", vec![v("x"), v("y")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), caller);
        module.add_func(FuncName::from("f"), f);

        eliminate_dead_arguments(&mut module);

        assert_eq!(func_named(&module, "f").params, vec![v("b")]);
        match &func_named(&module, "main").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert_eq!(params, &vec![v("y")]),
            other => panic!("expected direct call, got {other:?}"),
        }
    }

    #[test]
    fn keeps_used_params() {
        let f = func(vec![v("a")], "rf", region(vec![], ret("rf", v("a"))));
        let caller = func(
            vec![],
            "rm",
            region(
                vec![(v("x"), Value::Pure(Data::Nat(1)))],
                direct("f", vec![v("x")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), caller);
        module.add_func(FuncName::from("f"), f);

        eliminate_dead_arguments(&mut module);

        assert_eq!(func_named(&module, "f").params, vec![v("a")]);
    }

    #[test]
    fn never_trims_the_entry_signature() {
        // The entry keeps a genuinely unused parameter: the host calls it.
        let main = func(
            vec![v("unused")],
            "rm",
            region(vec![(v("r"), Value::Pure(Data::Nat(0)))], ret("rm", v("r"))),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.set_entry(FuncName::from("main"));

        eliminate_dead_arguments(&mut module);

        assert_eq!(func_named(&module, "main").params, vec![v("unused")]);
    }

    #[test]
    fn drops_unused_capture_and_trims_its_construction() {
        // c captures [f0, f1] but its body uses only f1.
        let c = Clsr {
            fields: vec![v("f0").into(), v("f1").into()],
            params: vec![],
            resume: BlockName::from("rc"),
            region: region(vec![], ret("rc", v("f1"))),
        };

        let builder = func(
            vec![],
            "rm",
            region(
                vec![
                    (v("p"), Value::Pure(Data::Nat(0))),
                    (v("q"), Value::Pure(Data::Nat(1))),
                    (
                        v("clo"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("p"), v("q")])),
                    ),
                ],
                ret("rm", v("clo")),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), builder);
        module.add_clsr(ClsrName::from("c"), c);

        eliminate_dead_arguments(&mut module);

        assert_eq!(clsr_named(&module, "c").fields, vec![v("f1")]);
        match &func_named(&module, "main").region.values[2].1 {
            Value::Pure(Data::Clsr(_, captures)) => assert_eq!(captures, &vec![v("q")]),
            other => panic!("expected closure construction, got {other:?}"),
        }
    }

    fn block(params: Vec<ValueName>, region: Region) -> Block {
        Block { params, region }
    }

    #[test]
    fn drops_param_only_fed_through_a_direct_self_call() {
        // f(a, b): a is really used (NatAdd); b's only occurrence is feeding
        // its own position in the self-call — a self-sustaining cycle, dead.
        let k1 = block(vec![], region(vec![], ret("rf", v("x"))));
        let k2 = block(
            vec![],
            region(vec![], direct("f", vec![v("a"), v("b")], "rf")),
        );
        let f = Func {
            params: vec![v("a").into(), v("b").into()],
            resume: BlockName::from("rf"),
            region: Region {
                preallocs: vec![],
                values: vec![(v("x"), Value::Eval(Code::NatAdd(v("a"), v("a"))))],
                blocks: vec![(BlockName::from("k1"), k1), (BlockName::from("k2"), k2)],
                tail: Tail::Match(MatchTarget {
                    operand: v("x"),
                    cases: [
                        (
                            0,
                            JumpTarget {
                                target: BlockName::from("k1"),
                                params: vec![],
                            },
                        ),
                        (
                            1,
                            JumpTarget {
                                target: BlockName::from("k2"),
                                params: vec![],
                            },
                        ),
                    ]
                    .into(),
                    default: None,
                }),
            },
        };
        let main = func(
            vec![],
            "rm",
            region(
                vec![
                    (v("p"), Value::Pure(Data::Nat(1))),
                    (v("q"), Value::Pure(Data::Nat(2))),
                ],
                direct("f", vec![v("p"), v("q")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        eliminate_dead_arguments(&mut module);

        assert_eq!(func_named(&module, "f").params, vec![v("a")]);
        match &func_named(&module, "main").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert_eq!(params, &vec![v("p")]),
            other => panic!("expected trimmed call in main, got {other:?}"),
        }
        // The self-call inside k2 is trimmed too.
        match &func_named(&module, "f").region.blocks[1].1.region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert_eq!(params, &vec![v("a")]),
            other => panic!("expected trimmed self-call, got {other:?}"),
        }
    }

    #[test]
    fn drops_param_only_fed_around_a_loop_block() {
        // The shape tail_recursion emits: f(p, q) jumps into header h[keep,
        // dead]; `keep` is really used, `dead` only rides the backward Match
        // edge. Both the block position and the function position feeding it
        // are dead; the Jump and Match edges are trimmed coordinately.
        let header = block(
            vec![v("keep"), v("dead")],
            Region {
                preallocs: vec![],
                values: vec![(v("y"), Value::Eval(Code::NatAdd(v("keep"), v("keep"))))],
                blocks: vec![],
                tail: Tail::Match(MatchTarget {
                    operand: v("y"),
                    cases: [
                        (
                            0,
                            JumpTarget {
                                target: BlockName::from("rf"),
                                params: vec![v("y")],
                            },
                        ),
                        (
                            1,
                            JumpTarget {
                                target: BlockName::from("h"),
                                params: vec![v("keep"), v("dead")],
                            },
                        ),
                    ]
                    .into(),
                    default: None,
                }),
            },
        );
        let f = Func {
            params: vec![v("p").into(), v("q").into()],
            resume: BlockName::from("rf"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(BlockName::from("h"), header)],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("h"),
                    params: vec![v("p"), v("q")],
                }),
            },
        };
        let main = func(
            vec![],
            "rm",
            region(
                vec![
                    (v("s"), Value::Pure(Data::Nat(1))),
                    (v("t"), Value::Pure(Data::Nat(2))),
                ],
                direct("f", vec![v("s"), v("t")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        eliminate_dead_arguments(&mut module);

        let f = func_named(&module, "f");

        // The function position cascades dead through the dead block position.
        assert_eq!(f.params, vec![v("p")]);

        // Entry jump, header signature, and backward Match edge all trimmed.
        match &f.region.tail {
            Tail::Jump(jump) => assert_eq!(jump.params, vec![v("p")]),
            other => panic!("expected entry jump, got {other:?}"),
        }
        let (_, header) = &f.region.blocks[0];
        assert_eq!(header.params, vec![v("keep")]);
        match &header.region.tail {
            Tail::Match(target) => {
                assert_eq!(target.cases[&1].params, vec![v("keep")]);
                // The sentinel edge returns a value — untouched.
                assert_eq!(target.cases[&0].params, vec![v("y")]);
            }
            other => panic!("expected match, got {other:?}"),
        }

        // The caller's argument is trimmed with the function position.
        match &func_named(&module, "main").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert_eq!(params, &vec![v("s")]),
            other => panic!("expected trimmed call, got {other:?}"),
        }
    }

    #[test]
    fn drops_cross_function_feed_cycle() {
        // f(x) only forwards x to g; g(y) only forwards y back to f. Neither
        // ever really uses the value — the whole cycle is dead on both sides.
        let f = func(
            vec![v("x")],
            "rf",
            region(vec![], direct("g", vec![v("x")], "rf")),
        );
        let g = func(
            vec![v("y")],
            "rg",
            region(vec![], direct("f", vec![v("y")], "rg")),
        );
        let main = func(
            vec![],
            "rm",
            region(
                vec![(v("a"), Value::Pure(Data::Nat(1)))],
                direct("f", vec![v("a")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_func(FuncName::from("g"), g);

        eliminate_dead_arguments(&mut module);

        assert!(func_named(&module, "f").params.is_empty());
        assert!(func_named(&module, "g").params.is_empty());
        match &func_named(&module, "main").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert!(params.is_empty()),
            other => panic!("expected trimmed call, got {other:?}"),
        }
    }

    #[test]
    fn pins_call_resume_block_params() {
        // Block k receives g's call result in `res` but never uses it. Its
        // parameter is bound by the call's fixed one-value arity, so it must
        // survive even though it is dead by liveness.
        let k = block(
            vec![v("res")],
            region(vec![(v("z"), Value::Pure(Data::Nat(0)))], ret("rf", v("z"))),
        );
        let f = Func {
            params: vec![],
            resume: BlockName::from("rf"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(BlockName::from("k"), k)],
                tail: direct("g", vec![], "k"),
            },
        };
        let g = func(
            vec![],
            "rg",
            region(vec![(v("r"), Value::Pure(Data::Nat(7)))], ret("rg", v("r"))),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);
        module.add_func(FuncName::from("g"), g);

        eliminate_dead_arguments(&mut module);

        let (_, k) = &func_named(&module, "f").region.blocks[0];
        assert_eq!(k.params, vec![v("res")], "resume params are pinned");
    }

    #[test]
    fn cascades_through_a_wrapper_to_a_fixed_point() {
        // f ignores its parameter; g(x) only forwards x to f; main calls g.
        // Round 1 drops f's parameter, which empties g(x) -> g forwards nothing,
        // so round 2 drops g's parameter too.
        let f = func(
            vec![v("p")],
            "rf",
            region(vec![(v("r"), Value::Pure(Data::Nat(0)))], ret("rf", v("r"))),
        );
        let g = func(
            vec![v("x")],
            "rg",
            region(vec![], direct("f", vec![v("x")], "rg")),
        );
        let main = func(
            vec![],
            "rm",
            region(
                vec![(v("a"), Value::Pure(Data::Nat(1)))],
                direct("g", vec![v("a")], "rm"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("g"), g);
        module.add_func(FuncName::from("f"), f);

        eliminate_dead_arguments(&mut module);

        assert!(func_named(&module, "f").params.is_empty());
        assert!(func_named(&module, "g").params.is_empty());

        match &func_named(&module, "g").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert!(params.is_empty()),
            other => panic!("expected direct call, got {other:?}"),
        }
        match &func_named(&module, "main").region.tail {
            Tail::Call(CallTarget::Direct { params, .. }) => assert!(params.is_empty()),
            other => panic!("expected direct call, got {other:?}"),
        }
    }
}
