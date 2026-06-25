use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// Decided-dispatch threading: thread a `Jump` through the tail it already
/// decides — a `Match` whose arm the edge picks, or an `Indirect` call whose
/// callee the edge resolves to a known closure.
///
/// The shape this targets is the join point a constructor-then-eliminate chain
/// leaves behind once inlining has brought producer and consumer into one body
/// (the `Result` re-wrap in every `std/Parse` combinator is the canonical case):
///
/// ```text
/// a0: t = Pure(Nat(0)); r = Pure(Tpl([t, p])); Jump(J, [r])   -- tag known: 0
/// a1: t'= Pure(Nat(1)); r'= Pure(Tpl([t',m])); Jump(J, [r'])  -- tag known: 1
/// J(x): tag = Eval(TplGet(x, 0)); Match(tag, {0 → s0, 1 → s1})
/// ```
///
/// `J` has two predecessors, so [`jump_threading`](super::jump_threading)
/// refuses it; `x` is a block parameter, so [`constant_folding`]'s [`Lits`]
/// never binds it and the match cannot be decided in place. Yet on each edge
/// the outcome is fully determined.
///
/// This pass resolves it *per edge*: when a `Jump`'s arguments, substituted for
/// the target block's parameters, let the existing folding machinery decide the
/// target's `Match`, the jump is replaced by a clone of the target specialized
/// to that edge — its prelude, the decided arm's subtree, and a tail that jumps
/// straight to the selected arm. The original block stays for the edges that do
/// not decide; once all of them thread, it goes unreferenced and
/// [`dead_code_elimination`](super::dead_code_elimination) reclaims it.
///
/// The pass itself only moves control-flow edges. The payoff lands in the
/// settle round that follows it in the pipeline: copy propagation collapses the
/// `param = Alias(arg)` bindings, projection forwarding resolves the payload
/// reads against the now-visible constructor, the constructor allocation goes
/// unreferenced, and DCE deletes it.
///
/// ## Deciding an edge
///
/// The check reuses the folding machinery wholesale: a clone of the body-wide
/// [`Lits`] is extended with `param ↦ lits[arg]` for each literal-bound
/// argument, the target's value prelude is run through [`simplify`] to chase
/// projections and aliases (a `TplGet` of a known tuple yields its tag), and
/// [`decide_match`] is asked for the taken arm. No decision, no clone — the
/// pass never duplicates code it cannot immediately collapse.
///
/// ## Cloning and freshening
///
/// The spliced clone is freshened with [`mangle::thread_suffix`], renaming only
/// the names *bound inside* the target block (parameters, binders, and block
/// names — [`harvest::bound_values`]/[`harvest::bound_blocks`]). Free names —
/// enclosing-scope values, module consts, and outside block targets — keep
/// resolving as they did from the original block, which is sound because the
/// edge's region necessarily sits within the scope the target block closes
/// over (it could name the block in its tail). The unselected arms are pruned
/// from the clone before splicing, so transient growth per edge is the target's
/// prelude plus the one taken arm.
///
/// Every prelude value is kept, in order, ahead of the decided jump — including
/// the match operand computation — so any value-dependent trap the original
/// path could raise is preserved exactly.
///
/// ## Termination
///
/// A target that can reach itself through jump edges (a loop) is never
/// threaded: threading such a block would be loop unrolling, unbounded when the
/// deciding bindings repeat. With cyclic targets excluded, every splice
/// consumes an edge into a candidate and adds edges only to blocks strictly
/// *downstream* of it in the (acyclic) candidate-reachability order, so chains
/// of decided joins — `or` nested in `bind` — thread through in successive
/// iterations and the fixed point exists.
///
/// ## Indirect-call devirtualization
///
/// The same per-edge specialization devirtualizes an indirect call. The residue
/// a closure-returning `match` leaves once its arms are lifted is a shared join
/// whose tail is `%f(args)` with `%f` a *block parameter* — one of several known
/// closures, depending on the edge, so [`closure_lifting`](super::closure_lifting)
/// (which only devirtualizes a callee that traces to a single known
/// `Data::Clsr`) cannot resolve it:
///
/// ```text
/// a0: Jump(J, [clsr#0])   -- callee known: closure #0
/// a1: Jump(J, [clsr#1])   -- callee known: closure #1
/// J(f): Call(Indirect { target: f, .. })
/// ```
///
/// When an edge's argument for `f` is a known `Data::Clsr`, the jump is replaced
/// by a clone of `J` whose callee parameter aliases that one closure — monomorphic
/// per edge. The clone keeps its indirect tail; the settle round's copy
/// propagation collapses the alias and the following `lift_closures` rewrites the
/// now-single-callee call to a `Direct` one, which single-site inlining then
/// fuses. The pass itself only moves the edge, exactly as in the match case.
///
/// ## What is left alone
///
/// - **Match-arm edges** — an arm's `JumpTarget` could be threaded the same
///   way, but the lowerer emits arms with no parameters, so no deciding
///   argument ever flows along one today; only `Jump` tails are considered.
/// - **Dynamic callees** — an indirect call whose callee is computed from
///   runtime data (no `Data::Clsr` flows along the edge) stays indirect: no
///   decision, no clone.
/// - **Host and call *resumes*** — the resume edge is the call protocol, not a
///   jump.
/// - **Loops** — see termination above.
pub fn thread_decided_dispatch(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        thread_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        thread_tree(&mut clsr.region);
    }
}

/// Thread one edge at a time, re-harvesting literals and candidates after each
/// splice (a spliced clone can expose the next decidable edge of a chain).
fn thread_tree(region: &mut Region) {
    let mut counter = 0;

    loop {
        let lits = literals(region);
        let candidates = deciding_blocks(region);

        if candidates.is_empty() || !thread_first(region, &lits, &candidates, &mut counter) {
            break;
        }
    }
}

// --- Candidate selection ------------------------------------------------------

/// Every block in the tree that a known-tag edge could thread through: its
/// region's tail is a `Match`, and it cannot reach itself through jump edges
/// (threading a loop block would be unbounded unrolling). Cloned up front so
/// the splice can borrow the host region mutably.
fn deciding_blocks(region: &Region) -> HashMap<BlockName, Block> {
    let graph = jump_graph(region);
    let mut out = HashMap::new();
    collect_deciding(region, &graph, &mut out);
    out
}

fn collect_deciding(region: &Region, graph: &JumpGraph, out: &mut HashMap<BlockName, Block>) {
    for (name, block) in &region.blocks {
        if is_decidable_candidate(block) && !in_jump_cycle(name, graph) {
            out.insert(name.clone(), block.clone());
        }
        collect_deciding(&block.region, graph, out);
    }
}

/// A block whose tail an incoming edge can decide: a `Match` (the edge fixes
/// the operand), or an `Indirect` call whose callee is a block parameter (the
/// edge fixes the callee). A callee already bound *inside* the block is a known
/// closure [`closure_lifting`](super::closure_lifting) devirtualizes directly —
/// only a parameter join needs per-edge specialization.
fn is_decidable_candidate(block: &Block) -> bool {
    match &block.region.tail {
        Tail::Match(_) => true,
        Tail::Call(CallTarget::Indirect { target, .. }) => block.params.contains(target),
        _ => false,
    }
}

/// The control-flow graph over block names: an edge `B → T` for every target
/// `T` of a tail sitting directly in `B`'s region (nested blocks own their own
/// tails). The function's root tail has no owning block and no name to cycle
/// through, so it contributes no edges.
type JumpGraph = HashMap<BlockName, Vec<BlockName>>;

fn jump_graph(region: &Region) -> JumpGraph {
    let mut graph = JumpGraph::new();
    for (name, block) in &region.blocks {
        build_jump_graph(name, &block.region, &mut graph);
    }
    graph
}

fn build_jump_graph(owner: &BlockName, region: &Region, graph: &mut JumpGraph) {
    graph
        .entry(owner.clone())
        .or_default()
        .extend(tail_targets(&region.tail).into_iter().cloned());

    for (name, block) in &region.blocks {
        build_jump_graph(name, &block.region, graph);
    }
}

/// Whether `start` can reach itself through jump edges.
fn in_jump_cycle(start: &BlockName, graph: &JumpGraph) -> bool {
    let Some(edges) = graph.get(start) else {
        return false;
    };
    let mut visited = HashSet::<&BlockName>::new();
    let mut stack = edges.iter().collect::<Vec<_>>();
    while let Some(name) = stack.pop() {
        if name == start {
            return true;
        }
        if !visited.insert(name) {
            continue;
        }
        if let Some(next) = graph.get(name) {
            stack.extend(next.iter());
        }
    }
    false
}

// --- Threading ----------------------------------------------------------------

/// Find the first `Jump` tail whose target's match its arguments decide, splice
/// the specialized clone there, and report whether anything changed.
fn thread_first(
    region: &mut Region,
    lits: &Lits,
    candidates: &HashMap<BlockName, Block>,
    counter: &mut usize,
) -> bool {
    let decided = if let Tail::Jump(jump) = &region.tail
        && let Some(block) = candidates.get(&jump.target)
    {
        decide_edge(block, &jump.params, lits).map(|decision| (block, jump.params.clone(), decision))
    } else {
        None
    };

    if let Some((block, args, decision)) = decided {
        *counter += 1;
        splice_decided(region, block, &args, decision, &mangle::thread_suffix(*counter));
        return true;
    }

    for (_, block) in &mut region.blocks {
        if thread_first(&mut block.region, lits, candidates, counter) {
            return true;
        }
    }

    false
}

/// How an edge decides a candidate block's tail.
enum Decision {
    /// A `Match` the edge picks the arm of: splice with the tail set to the arm,
    /// the untaken arms pruned.
    Arm(JumpTarget),
    /// An `Indirect` call whose callee the edge fixes to a known closure: splice
    /// as-is, leaving the now-monomorphic call for `lift_closures` to devirtualize.
    Devirtualize,
}

/// How `block`'s tail is decided when entered with `args`, if the literal
/// environment determines it. The check is the constant folder's machinery run
/// against `lits` extended with the parameter bindings: the prelude is chased
/// through [`simplify`] so a `TplGet` of a known constructor surfaces its tag (or
/// a callee parameter surfaces its closure), then the tail is read — [`decide_match`]
/// picks a `Match`'s arm; a known `Data::Clsr` at an `Indirect` callee selects
/// devirtualization.
fn decide_edge(block: &Block, args: &[ValueName], lits: &Lits) -> Option<Decision> {
    debug_assert_eq!(
        block.params.len(),
        args.len(),
        "jump passes {} args for {} params",
        args.len(),
        block.params.len(),
    );
    if block.params.len() != args.len() {
        return None;
    }

    let mut env = lits.clone();
    for (param, arg) in block.params.iter().zip(args) {
        if let Some(data) = lits.get(arg) {
            env.insert(param.clone(), data.clone());
        }
    }

    for (name, value) in &block.region.values {
        let known = match value {
            Value::Pure(data) => Some(data.clone()),
            Value::Alias(source) => env.get(source).cloned(),
            Value::Eval(code) => simplify(code, &env).and_then(|evaluated| match evaluated {
                Evaluated::Scalar(scalar) => Some(scalar.into()),
                Evaluated::Arr(elems) => Some(Data::Arr(elems)),
                Evaluated::Elem(source) => env.get(&source).cloned(),
            }),
        };

        if let Some(data) = known {
            env.insert(name.clone(), data);
        }
    }

    match &block.region.tail {
        Tail::Call(CallTarget::Indirect { target, .. }) => {
            matches!(env.get(target), Some(Data::Clsr(..))).then_some(Decision::Devirtualize)
        }
        _ => decide_match(&block.region.tail, &env).map(Decision::Arm),
    }
}

/// Splice a clone of `block`, specialized to the `decision`, into `host` (whose
/// tail is the jump being threaded): bind each parameter to its argument, keep
/// the whole prelude (its traps must survive), and freshen every bound name with
/// `suffix`. A decided `Match` has its tail replaced by the taken arm and the
/// untaken arms pruned; a devirtualized call keeps its indirect tail, now with a
/// monomorphic (parameter-aliased) callee.
fn splice_decided(
    host: &mut Region,
    block: &Block,
    args: &[ValueName],
    decision: Decision,
    suffix: &str,
) {
    let mut clone = block.region.clone();
    if let Decision::Arm(arm) = decision {
        clone.tail = Tail::Jump(arm);
    }
    prune_unreachable_blocks(&mut clone);

    let mut bound = harvest::bound_values(&block.region);
    bound.extend(block.params.iter().cloned());
    let bound_blocks = harvest::bound_blocks(&block.region);

    walk_region_mut(
        &mut clone,
        &mut FreshenUses {
            bound: &bound,
            suffix,
        },
    );
    freshen_structure(&mut clone, &bound, &bound_blocks, suffix);

    let param_aliases = block.params.iter().zip(args).map(|(param, arg)| {
        (
            mangle::suffixed_value(param, suffix),
            Value::Alias(arg.clone()),
        )
    });

    host.preallocs.extend(clone.preallocs);
    host.values.extend(param_aliases);
    host.values.extend(clone.values);
    host.blocks.extend(clone.blocks);
    host.tail = clone.tail;
}

/// Drop the top-level blocks the (already decided) tail can no longer reach —
/// the arms not taken. Reachability is computed over the clone's own blocks;
/// references to outside blocks pass through untouched.
fn prune_unreachable_blocks(region: &mut Region) {
    let defined: HashSet<BlockName> = region.blocks.iter().map(|(name, _)| name.clone()).collect();

    let mut reachable = HashSet::new();
    let mut frontier: Vec<BlockName> = tail_targets(&region.tail).into_iter().cloned().collect();

    while let Some(name) = frontier.pop() {
        if !defined.contains(&name) || !reachable.insert(name.clone()) {
            continue;
        }
        let (_, block) = region
            .blocks
            .iter()
            .find(|(block_name, _)| block_name == &name)
            .expect("reachable name is defined");
        frontier.extend(subtree_targets(&block.region));
    }

    region.blocks.retain(|(name, _)| reachable.contains(name));
}

/// Every tail target anywhere in a region tree.
fn subtree_targets(region: &Region) -> Vec<BlockName> {
    let mut targets: Vec<BlockName> = tail_targets(&region.tail).into_iter().cloned().collect();
    for (_, block) in &region.blocks {
        targets.extend(subtree_targets(&block.region));
    }
    targets
}

/// Renames what the use-walker does not reach: value binders, and block names
/// (definitions and tail references) — gated on the bound sets, so references
/// to enclosing-scope blocks and values stay intact.
fn freshen_structure(
    region: &mut Region,
    bound: &HashSet<ValueName>,
    bound_blocks: &HashSet<BlockName>,
    suffix: &str,
) {
    for (name, _) in &mut region.preallocs {
        freshen_value(name, bound, suffix);
    }
    for (name, _) in &mut region.values {
        freshen_value(name, bound, suffix);
    }

    freshen_tail_blocks(&mut region.tail, bound_blocks, suffix);

    for (name, block) in &mut region.blocks {
        freshen_block(name, bound_blocks, suffix);
        for param in &mut block.params {
            freshen_value(param, bound, suffix);
        }
        freshen_structure(&mut block.region, bound, bound_blocks, suffix);
    }
}

fn freshen_value(name: &mut ValueName, bound: &HashSet<ValueName>, suffix: &str) {
    if bound.contains(name) {
        *name = mangle::suffixed_value(name, suffix);
    }
}

fn freshen_block(name: &mut BlockName, bound_blocks: &HashSet<BlockName>, suffix: &str) {
    if bound_blocks.contains(name) {
        *name = mangle::suffixed_block(name, suffix);
    }
}

fn freshen_tail_blocks(tail: &mut Tail, bound_blocks: &HashSet<BlockName>, suffix: &str) {
    match tail {
        Tail::Jump(target) => freshen_block(&mut target.target, bound_blocks, suffix),
        Tail::Match(target) => {
            for jump in target.cases.values_mut() {
                freshen_block(&mut jump.target, bound_blocks, suffix);
            }
            if let Some(jump) = &mut target.default {
                freshen_block(&mut jump.target, bound_blocks, suffix);
            }
        }
        Tail::Call(CallTarget::Direct { resume, .. })
        | Tail::Call(CallTarget::Indirect { resume, .. }) => {
            freshen_block(resume, bound_blocks, suffix);
        }
        Tail::Host(host) => {
            freshen_block(host.resume_mut(), bound_blocks, suffix);
        }
        Tail::Cell(cell) => {
            freshen_block(cell.resume_mut(), bound_blocks, suffix);
        }
        Tail::Unreachable => {}
    }
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

    fn region(
        values: Vec<(ValueName, Value)>,
        blocks: Vec<(BlockName, Block)>,
        tail: Tail,
    ) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks,
            tail,
        }
    }

    fn block(params: Vec<ValueName>, region: Region) -> Block {
        Block { params, region }
    }

    fn jump(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(target),
            params: args,
        })
    }

    fn match_tail(operand: &str, cases: Vec<(u32, &str)>) -> Tail {
        Tail::Match(MatchTarget {
            operand: v(operand),
            cases: cases
                .into_iter()
                .map(|(tag, target)| {
                    (
                        tag,
                        JumpTarget {
                            target: b(target),
                            params: vec![],
                        },
                    )
                })
                .collect(),
            default: None,
        })
    }

    fn run(top: Region) -> Region {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: top,
            },
        );
        thread_decided_dispatch(&mut module);
        module.funcs().iter().next().unwrap().1.region.clone()
    }

    fn block_named<'a>(region: &'a Region, name: &str) -> &'a Block {
        region
            .blocks
            .iter()
            .find(|(block_name, _)| block_name == &b(name))
            .map(|(_, block)| block)
            .unwrap_or_else(|| panic!("block `{name}` not found"))
    }

    fn has_block(region: &Region, name: &str) -> bool {
        region
            .blocks
            .iter()
            .any(|(block_name, _)| block_name == &b(name))
    }

    /// The Parse shape: two arms build tagged tuples and jump to a join `J`
    /// that projects the tag and matches on it. `g` is a free top-level value
    /// the selected arm uses, to check free names survive unrenamed.
    fn join_shape() -> Region {
        let a0 = block(
            vec![],
            region(
                vec![
                    (v("t0"), Value::Pure(Data::Nat(0))),
                    (v("p0"), Value::Pure(Data::Nat(7))),
                    (v("r0"), Value::Pure(Data::Tpl(vec![v("t0"), v("p0")]))),
                ],
                vec![],
                jump("J", vec![v("r0")]),
            ),
        );
        let a1 = block(
            vec![],
            region(
                vec![
                    (v("t1"), Value::Pure(Data::Nat(1))),
                    (v("r1"), Value::Pure(Data::Tpl(vec![v("t1"), v("g")]))),
                ],
                vec![],
                jump("J", vec![v("r1")]),
            ),
        );

        let s0 = block(
            vec![],
            region(
                vec![
                    (v("pay"), Value::Eval(Code::TplGet(v("x"), 1))),
                    (v("out"), Value::Eval(Code::NatAdd(v("pay"), v("g")))),
                ],
                vec![],
                jump("rm", vec![v("out")]),
            ),
        );
        let s1 = block(vec![], region(vec![], vec![], jump("rm", vec![v("x")])));

        let join = block(
            vec![v("x")],
            region(
                vec![(v("tag"), Value::Eval(Code::TplGet(v("x"), 0)))],
                vec![(b("s0"), s0), (b("s1"), s1)],
                match_tail("tag", vec![(0, "s0"), (1, "s1")]),
            ),
        );

        region(
            vec![(v("g"), Value::Pure(Data::Nat(100)))],
            vec![(b("a0"), a0), (b("a1"), a1), (b("J"), join)],
            match_tail("c", vec![(0, "a0"), (1, "a1")]),
        )
    }

    #[test]
    fn threads_each_edge_to_its_decided_arm() {
        let result = run(join_shape());

        // a0 (tag 0) is threaded first: it now holds a specialized clone of J
        // whose tail jumps straight to the cloned `s0` arm.
        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("s0@thread#1")),
            other => panic!("expected a0 to jump to the decided arm, got {other:?}"),
        }

        // The parameter is bound to the edge's argument as an alias.
        assert!(
            a0.values
                .iter()
                .any(|(name, value)| name == &v("x@thread#1")
                    && matches!(value, Value::Alias(source) if source == &v("r0")))
        );

        // The prelude (tag projection) is kept, operands renamed to the clone.
        assert!(a0.values.iter().any(|(name, value)| name == &v("tag@thread#1")
            && matches!(value, Value::Eval(Code::TplGet(source, 0)) if source == &v("x@thread#1"))));

        // Only the taken arm is spliced; the untaken one is pruned.
        assert!(has_block(a0, "s0@thread#1"));
        assert!(!has_block(a0, "s1@thread#1"));

        // Inside the cloned arm: bound uses renamed, free uses (`g`) and the
        // outside jump target (`rm`) untouched.
        let s0 = &block_named(a0, "s0@thread#1").region;
        assert!(s0.values.iter().any(|(name, value)| name == &v("pay@thread#1")
            && matches!(value, Value::Eval(Code::TplGet(source, 1)) if source == &v("x@thread#1"))));
        assert!(
            s0.values
                .iter()
                .any(|(name, value)| name == &v("out@thread#1")
                    && matches!(value, Value::Eval(Code::NatAdd(left, right))
                if left == &v("pay@thread#1") && right == &v("g")))
        );
        match &s0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("rm")),
            other => panic!("expected the arm to keep its outside jump, got {other:?}"),
        }

        // a1 (tag 1) threads on the next iteration with its own suffix.
        let a1 = &block_named(&result, "a1").region;
        match &a1.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("s1@thread#2")),
            other => panic!("expected a1 to jump to the decided arm, got {other:?}"),
        }

        // The original join survives for DCE to reclaim, untouched.
        let join = &block_named(&result, "J").region;
        assert!(matches!(join.tail, Tail::Match(_)));
        assert!(has_block(join, "s0"));
        assert!(has_block(join, "s1"));
    }

    #[test]
    fn leaves_unknown_tag_edges_alone() {
        // The edge's argument is computed, not literal: nothing threads.
        let a0 = block(
            vec![],
            region(
                vec![(v("r0"), Value::Eval(Code::ArrGet(v("arr"), v("i"))))],
                vec![],
                jump("J", vec![v("r0")]),
            ),
        );
        let s0 = block(vec![], region(vec![], vec![], jump("rm", vec![v("x")])));
        let join = block(
            vec![v("x")],
            region(
                vec![(v("tag"), Value::Eval(Code::TplGet(v("x"), 0)))],
                vec![(b("s0"), s0)],
                match_tail("tag", vec![(0, "s0")]),
            ),
        );
        let top = region(
            vec![],
            vec![(b("a0"), a0), (b("J"), join)],
            match_tail("c", vec![(0, "a0")]),
        );

        let result = run(top);

        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("J")),
            other => panic!("expected the edge untouched, got {other:?}"),
        }
    }

    #[test]
    fn leaves_looping_match_blocks_alone() {
        // J's arm jumps back to J: a loop. Even though the entry edge carries a
        // known tag, threading would be unrolling — the cycle guard refuses.
        let a0 = block(
            vec![],
            region(
                vec![
                    (v("t0"), Value::Pure(Data::Nat(0))),
                    (v("r0"), Value::Pure(Data::Tpl(vec![v("t0")]))),
                ],
                vec![],
                jump("J", vec![v("r0")]),
            ),
        );
        let s0 = block(vec![], region(vec![], vec![], jump("J", vec![v("x")])));
        let join = block(
            vec![v("x")],
            region(
                vec![(v("tag"), Value::Eval(Code::TplGet(v("x"), 0)))],
                vec![(b("s0"), s0)],
                match_tail("tag", vec![(0, "s0")]),
            ),
        );
        let top = region(
            vec![],
            vec![(b("a0"), a0), (b("J"), join)],
            match_tail("c", vec![(0, "a0")]),
        );

        let result = run(top);

        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("J")),
            other => panic!("expected the loop edge untouched, got {other:?}"),
        }
    }

    #[test]
    fn threads_chains_across_iterations() {
        // a0 → J1 decides to s0, whose body builds a *new* tagged tuple and
        // jumps to J2 — which the next iteration decides too, inside the clone.
        let a0 = block(
            vec![],
            region(
                vec![
                    (v("tA"), Value::Pure(Data::Nat(0))),
                    (v("pA"), Value::Pure(Data::Nat(7))),
                    (v("rA"), Value::Pure(Data::Tpl(vec![v("tA"), v("pA")]))),
                ],
                vec![],
                jump("J1", vec![v("rA")]),
            ),
        );

        let s0 = block(
            vec![],
            region(
                vec![
                    (v("tB"), Value::Pure(Data::Nat(1))),
                    (v("rB"), Value::Pure(Data::Tpl(vec![v("tB"), v("x")]))),
                ],
                vec![],
                jump("J2", vec![v("rB")]),
            ),
        );
        let s1 = block(vec![], region(vec![], vec![], jump("rm", vec![v("x")])));
        let join1 = block(
            vec![v("x")],
            region(
                vec![(v("tagX"), Value::Eval(Code::TplGet(v("x"), 0)))],
                vec![(b("s0"), s0), (b("s1"), s1)],
                match_tail("tagX", vec![(0, "s0"), (1, "s1")]),
            ),
        );

        let u0 = block(vec![], region(vec![], vec![], jump("rm", vec![v("y")])));
        let u1 = block(vec![], region(vec![], vec![], jump("rm", vec![v("y")])));
        let join2 = block(
            vec![v("y")],
            region(
                vec![(v("tagY"), Value::Eval(Code::TplGet(v("y"), 0)))],
                vec![(b("u0"), u0), (b("u1"), u1)],
                match_tail("tagY", vec![(0, "u0"), (1, "u1")]),
            ),
        );

        let top = region(
            vec![],
            vec![(b("a0"), a0), (b("J1"), join1), (b("J2"), join2)],
            match_tail("c", vec![(0, "a0")]),
        );

        let result = run(top);

        // First splice: a0 holds J1's clone, decided to s0.
        let a0 = &block_named(&result, "a0").region;
        assert!(has_block(a0, "s0@thread#1"));

        // Second splice, inside the clone: s0's rebuilt constructor decides J2
        // to its tag-1 arm.
        let s0 = &block_named(a0, "s0@thread#1").region;
        match &s0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("u1@thread#2")),
            other => panic!("expected the chained join threaded too, got {other:?}"),
        }
        let u1 = &block_named(s0, "u1@thread#2").region;
        match &u1.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("rm"));
                assert_eq!(target.params, vec![v("y@thread#2")]);
            }
            other => panic!("expected the final arm to return, got {other:?}"),
        }
    }

    #[test]
    fn decides_through_the_default_arm() {
        // The known tag matches no case; the default arm is selected.
        let a0 = block(
            vec![],
            region(
                vec![
                    (v("t0"), Value::Pure(Data::Nat(9))),
                    (v("r0"), Value::Pure(Data::Tpl(vec![v("t0")]))),
                ],
                vec![],
                jump("J", vec![v("r0")]),
            ),
        );
        let s0 = block(vec![], region(vec![], vec![], jump("rm", vec![v("x")])));
        let fallback = block(vec![], region(vec![], vec![], jump("rm", vec![v("x")])));
        let join = block(
            vec![v("x")],
            region(
                vec![(v("tag"), Value::Eval(Code::TplGet(v("x"), 0)))],
                vec![(b("s0"), s0), (b("fb"), fallback)],
                Tail::Match(MatchTarget {
                    operand: v("tag"),
                    cases: [(
                        0,
                        JumpTarget {
                            target: b("s0"),
                            params: vec![],
                        },
                    )]
                    .into(),
                    default: Some(JumpTarget {
                        target: b("fb"),
                        params: vec![],
                    }),
                }),
            ),
        );
        let top = region(
            vec![],
            vec![(b("a0"), a0), (b("J"), join)],
            match_tail("c", vec![(0, "a0")]),
        );

        let result = run(top);

        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("fb@thread#1")),
            other => panic!("expected the default arm selected, got {other:?}"),
        }
        assert!(!has_block(a0, "s0@thread#1"));
    }

    fn indirect(target: &str, args: Vec<ValueName>, resume: &str) -> Tail {
        Tail::Call(CallTarget::Indirect {
            target: v(target),
            params: args,
            resume: b(resume),
        })
    }

    /// The closure-returning-match residue: two arms pass *different* known
    /// closures into a shared join whose tail calls the joined parameter. Each
    /// edge threads to a clone whose callee aliases that edge's closure —
    /// monomorphic, so the lift round that follows in the pipeline can
    /// devirtualize the indirect call.
    #[test]
    fn devirtualizes_each_edge_to_its_known_closure() {
        let a0 = block(
            vec![],
            region(
                vec![(
                    v("f0"),
                    Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![])),
                )],
                vec![],
                jump("J", vec![v("f0")]),
            ),
        );
        let a1 = block(
            vec![],
            region(
                vec![(
                    v("f1"),
                    Value::Pure(Data::Clsr(ClsrName::from("c1"), vec![])),
                )],
                vec![],
                jump("J", vec![v("f1")]),
            ),
        );
        let join = block(
            vec![v("f")],
            region(vec![], vec![], indirect("f", vec![v("arg")], "rm")),
        );
        let top = region(
            vec![(v("arg"), Value::Pure(Data::Nat(5)))],
            vec![(b("a0"), a0), (b("a1"), a1), (b("J"), join)],
            match_tail("c", vec![(0, "a0"), (1, "a1")]),
        );

        let result = run(top);

        // a0 threads first: it holds a clone of J whose indirect callee aliases
        // c0's closure (the tail stays a Call::Indirect — only the join split).
        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Call(CallTarget::Indirect { target, params, resume }) => {
                assert_eq!(target, &v("f@thread#1"));
                assert_eq!(params, &vec![v("arg")]);
                assert_eq!(resume, &b("rm"));
            }
            other => panic!("expected the indirect call kept, got {other:?}"),
        }
        assert!(a0.values.iter().any(|(name, value)| name == &v("f@thread#1")
            && matches!(value, Value::Alias(source) if source == &v("f0"))));

        // a1 threads on the next iteration with its own closure and suffix.
        let a1 = &block_named(&result, "a1").region;
        match &a1.tail {
            Tail::Call(CallTarget::Indirect { target, .. }) => {
                assert_eq!(target, &v("f@thread#2"))
            }
            other => panic!("expected the indirect call kept, got {other:?}"),
        }
        assert!(a1.values.iter().any(|(name, value)| name == &v("f@thread#2")
            && matches!(value, Value::Alias(source) if source == &v("f1"))));

        // The original join survives for DCE to reclaim, untouched.
        assert!(has_block(&result, "J"));
    }

    #[test]
    fn leaves_dynamic_callee_indirect() {
        // The edge passes a *computed* value (not a known closure) for the
        // callee: no decision, no clone — the indirect call stays as dynamic
        // dispatch.
        let a0 = block(
            vec![],
            region(
                vec![(v("f0"), Value::Eval(Code::ArrGet(v("table"), v("i"))))],
                vec![],
                jump("J", vec![v("f0")]),
            ),
        );
        let join = block(
            vec![v("f")],
            region(vec![], vec![], indirect("f", vec![], "rm")),
        );
        let top = region(
            vec![],
            vec![(b("a0"), a0), (b("J"), join)],
            match_tail("c", vec![(0, "a0")]),
        );

        let result = run(top);

        let a0 = &block_named(&result, "a0").region;
        match &a0.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("J")),
            other => panic!("expected the edge untouched, got {other:?}"),
        }
    }
}
