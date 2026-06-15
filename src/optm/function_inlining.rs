use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// A callee whose body is at most this many region "items" — values, preallocs,
/// and tails counted recursively through nested blocks — qualifies for Tier 2
/// multi-site inlining. Single-instruction primitive wrappers (e.g. `Nat.add`)
/// sit at 2–3, the wrappers `closure_lifting` synthesises sit at 3–5; the
/// threshold caps emitted bloat per callee.
const SIZE_THRESHOLD: usize = 8;

/// Which rule selected a callee for inlining.
enum Tier {
    /// Exactly one call site. Bare `@{callee}` suffix; matches the historical
    /// IR-dump format and keeps existing tests stable.
    Single,
    /// `≥1` call site and body size ≤ [`SIZE_THRESHOLD`]. Each site is freshened
    /// with `@{callee}#{n}` where `n` is a per-callee counter, so the inlined
    /// copies in the same caller cannot collide.
    Multi,
}

/// General function inlining: splice a `Func` body into a `Direct` call site.
///
/// A `Direct` call carries the callee's whole body's worth of computation behind
/// a call boundary; inlining removes the boundary, bringing the callee's code —
/// and crucially its literal arguments — into the caller's region, where the
/// other passes (copy propagation, constant folding, dead-code elimination) can
/// act on them.
///
/// # Two tiers
///
/// - **Tier 1 (single-site)** — a callee with exactly one `Direct` call site
///   is spliced at that one site with the bare `@{callee}` suffix. The callee
///   becomes dead and is dropped in the same iteration.
/// - **Tier 2 (multi-site, size-bounded)** — a callee whose body size is at
///   most [`SIZE_THRESHOLD`] and that does not participate in a direct-call
///   cycle is spliced at *every* site with a per-site suffix `@{callee}#{n}`,
///   then dropped. This dissolves the dozen tiny primitive wrappers
///   (`Bin.concat`, `Nat.to_str`, …) the higher-level passes leave behind.
///
/// Tier 1 is preferred when applicable: it keeps the suffix format stable for
/// IR dumps and avoids the counter for callees that are already single-site.
///
/// # Name freshening by suffix
///
/// Value and block names are unique only *within* a body (the lowerer restarts
/// its `v`/`b` counters per region), so a verbatim splice would collide the
/// callee's `v0` with the caller's. Every name the callee *binds* is suffixed
/// (Tier 1: `@{callee}`; Tier 2: `@{callee}#{n}`); since caller names are
/// always `v\d+`/`b\d+`, a suffixed name can never equal one. Free names in
/// the callee are module-level consts (and closure/function names) — those are
/// left untouched so their references still resolve.
///
/// For Tier 2 the per-site counter guarantees uniqueness when the same callee
/// is spliced into the same caller more than once.
///
/// # Resume stitching is a rename
///
/// This IR has no `Return`: a function returns `v` by `Jump(resume, [v])`, where
/// `resume` is a virtual sentinel block name. A `Direct { resume: R }` call names
/// the caller's continuation block `R`. So threading the callee's result into the
/// caller's continuation is just one more entry in the block rename: the callee's
/// resume sentinel maps to `R` instead of being suffixed, turning every
/// `Jump(resume, [v])` into `Jump(R, [v])`.
///
/// # Termination
///
/// Callees in a direct-call cycle (self-recursive or mutually recursive) are
/// excluded from both tiers. Every other iteration either deletes one callee
/// (Tier 1 splices once; Tier 2 splices `count` times) and strictly shrinks
/// `Module::funcs`, or selects no candidate and returns.
pub fn inline_calls(module: &mut Module) {
    let mut counter: HashMap<FuncName, usize> = HashMap::new();
    loop {
        let counts = direct_call_counts(module);
        let graph = call_graph(module);

        let Some((callee_name, tier)) = pick_callee(module, &counts, &graph) else {
            return;
        };

        // Clone the callee's *current* body (no stale snapshots: we recompute and
        // re-clone every round) so the splice can borrow the module mutably.
        let callee = find_func(module, &callee_name)
            .cloned()
            .expect("picked callee is present");

        match tier {
            Tier::Single => {
                let suffix = mangle::inline_suffix(&callee_name);
                if !splice_in_module(module, &callee_name, &callee, &suffix) {
                    return;
                }
            }
            Tier::Multi => {
                let total = counts.get(&callee_name).copied().unwrap_or(0);
                for _ in 0..total {
                    let n = counter.entry(callee_name.clone()).or_insert(0);
                    *n += 1;
                    let suffix = mangle::inline_site_suffix(&callee_name, *n);
                    if !splice_in_module(module, &callee_name, &callee, &suffix) {
                        break;
                    }
                }
            }
        }

        module.funcs_mut().retain(|(name, _)| name != &callee_name);
    }
}

/// Splice the first remaining `Direct` call to `callee_name` in the module —
/// searching funcs then closures — using `suffix` for freshening.
fn splice_in_module(
    module: &mut Module,
    callee_name: &FuncName,
    callee: &Func,
    suffix: &str,
) -> bool {
    module
        .funcs_mut()
        .iter_mut()
        .any(|(_, func)| splice_first(&mut func.region, callee_name, callee, suffix))
        || module
            .clsrs_mut()
            .iter_mut()
            .any(|(_, clsr)| splice_first(&mut clsr.region, callee_name, callee, suffix))
}

// --- Candidate selection ----------------------------------------------------

/// Count, per function name, how many `Direct` calls target it across the whole
/// module. (Functions are referenced *only* by `Direct` calls, so this is the
/// complete picture of a function's call sites.)
fn direct_call_counts(module: &Module) -> HashMap<FuncName, usize> {
    let mut counts = Counts(HashMap::new());

    for (_, func) in module.funcs() {
        walk_region(&func.region, &mut counts);
    }
    for (_, clsr) in module.clsrs() {
        walk_region(&clsr.region, &mut counts);
    }

    counts.0
}

struct Counts(HashMap<FuncName, usize>);

impl Sink for Counts {
    fn func_ref(&mut self, name: &FuncName) {
        *self.0.entry(name.clone()).or_insert(0) += 1;
    }
}

/// Pick the next callee to inline. Tier 1 (single call site, not in a cycle) is
/// preferred over Tier 2 (size ≤ [`SIZE_THRESHOLD`], ≥1 call site, not in a
/// cycle) so the cheaper, history-preserving path always wins when applicable.
/// The entrypoint is never picked: it is the host's entry contract, and both
/// tiers delete the callee after splicing.
fn pick_callee(
    module: &Module,
    counts: &HashMap<FuncName, usize>,
    graph: &CallGraph,
) -> Option<(FuncName, Tier)> {
    let entry = module.entry();

    if let Some((name, _)) = module.funcs().iter().find(|(name, _)| {
        Some(name) != entry && counts.get(name) == Some(&1) && !is_in_direct_call_cycle(name, graph)
    }) {
        return Some((name.clone(), Tier::Single));
    }

    module
        .funcs()
        .iter()
        .find(|(name, func)| {
            Some(name) != entry
                && counts.get(name).copied().unwrap_or(0) >= 1
                && body_size(func) <= SIZE_THRESHOLD
                && !is_in_direct_call_cycle(name, graph)
        })
        .map(|(name, _)| (name.clone(), Tier::Multi))
}

/// Count the items in a function's region tree: every value and prealloc
/// binding, plus one per tail. Cheap, monotonic, and tracks emitted code size
/// closely enough to gate Tier 2 inlining.
fn body_size(func: &Func) -> usize {
    fn region_size(region: &Region) -> usize {
        region.values.len()
            + region.preallocs.len()
            + 1
            + region
                .blocks
                .iter()
                .map(|(_, block)| region_size(&block.region))
                .sum::<usize>()
    }
    region_size(&func.region)
}

/// The direct-call graph over `Func`s: each function's `Tail::Call(Direct)`
/// targets, harvested once per `inline_calls` iteration so cycle checks walk
/// hash sets instead of re-harvesting region trees per candidate.
type CallGraph = HashMap<FuncName, HashSet<FuncName>>;

fn call_graph(module: &Module) -> CallGraph {
    module
        .funcs()
        .iter()
        .map(|(name, func)| (name.clone(), harvest::region_refs(&func.region).funcs))
        .collect()
}

/// Whether `start` sits in a non-trivial SCC of the direct-call graph (or has a
/// self-loop). Nodes are `Func`s only; edges are `Tail::Call(Direct)` targets
/// in a func's region tree. Closures (called indirectly) are not nodes, so
/// cycles routed through a closure do not exclude a callee — only direct-call
/// cycles do.
fn is_in_direct_call_cycle(start: &FuncName, graph: &CallGraph) -> bool {
    let Some(edges) = graph.get(start) else {
        return false;
    };
    let mut visited = HashSet::<&FuncName>::new();
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

fn find_func<'a>(module: &'a Module, name: &FuncName) -> Option<&'a Func> {
    module
        .funcs()
        .iter()
        .find(|(func_name, _)| func_name == name)
        .map(|(_, func)| func)
}

// --- Splicing ---------------------------------------------------------------

/// Find the first region in the tree whose tail is a `Direct` call to `callee`
/// and inline there using `suffix` to freshen the callee's bindings. Returns
/// whether a site was found.
fn splice_first(region: &mut Region, callee_name: &FuncName, callee: &Func, suffix: &str) -> bool {
    let here = matches!(
        &region.tail,
        Tail::Call(CallTarget::Direct { target, .. }) if target == callee_name,
    );

    if here {
        inline_at(region, callee, suffix);
        return true;
    }

    for (_, block) in &mut region.blocks {
        if splice_first(&mut block.region, callee_name, callee, suffix) {
            return true;
        }
    }

    false
}

/// Inline `callee` at `host`, whose tail is the `Direct` call to it, freshening
/// every binding in the spliced body with `suffix`.
fn inline_at(host: &mut Region, callee: &Func, suffix: &str) {
    let (args, resume) = match &host.tail {
        Tail::Call(CallTarget::Direct { params, resume, .. }) => (params.clone(), resume.clone()),
        _ => unreachable!("inline_at called on a non-Direct tail"),
    };

    debug_assert_eq!(
        callee.params.len(),
        args.len(),
        "arity mismatch inlining callee with suffix `{suffix}`",
    );

    let bound = bound_values(callee);

    // Clone and freshen: value uses first (reusing the shared walker, which
    // covers the big `Code` operand match), then binders, block names, and the
    // resume sentinel.
    let mut body = callee.region.clone();
    walk_region_mut(
        &mut body,
        &mut FreshenUses {
            bound: &bound,
            suffix,
        },
    );
    freshen_structure(&mut body, &bound, suffix, &callee.resume, &resume);

    // Bind each (freshened) parameter to its argument; copy propagation collapses
    // these aliases on its next run.
    let param_aliases = callee.params.iter().zip(&args).map(|(param, arg)| {
        (
            mangle::suffixed_value(&param.name, suffix),
            Value::Alias(arg.clone()),
        )
    });

    host.preallocs.extend(body.preallocs);
    host.values.extend(param_aliases);
    host.values.extend(body.values);
    host.blocks.extend(body.blocks);
    host.tail = body.tail;
}

/// Every value name *bound* in a function body: its parameters plus every
/// prealloc, value, and block-parameter binder in the tree (the latter via the
/// shared [`harvest::bound_values`]). A name not in this set is free — a module
/// const — and must not be renamed.
fn bound_values(func: &Func) -> HashSet<ValueName> {
    let mut bound = harvest::bound_values(&func.region);
    bound.extend(func.params.iter().map(|p| p.name.clone()));
    bound
}

/// Renames everything the use-walker does not reach: value binders, block names
/// (binders and references), and the resume sentinel — which maps to the call's
/// continuation rather than being suffixed.
fn freshen_structure(
    region: &mut Region,
    bound: &HashSet<ValueName>,
    suffix: &str,
    sentinel: &BlockName,
    resume: &BlockName,
) {
    for (name, _) in &mut region.preallocs {
        freshen_value(name, bound, suffix);
    }
    for (name, _) in &mut region.values {
        freshen_value(name, bound, suffix);
    }

    freshen_tail_blocks(&mut region.tail, sentinel, resume, suffix);

    for (name, block) in &mut region.blocks {
        freshen_block(name, sentinel, resume, suffix);
        for param in &mut block.params {
            freshen_value(param, bound, suffix);
        }
        freshen_structure(&mut block.region, bound, suffix, sentinel, resume);
    }
}

fn freshen_value(name: &mut ValueName, bound: &HashSet<ValueName>, suffix: &str) {
    if bound.contains(name) {
        *name = mangle::suffixed_value(name, suffix);
    }
}

fn freshen_block(name: &mut BlockName, sentinel: &BlockName, resume: &BlockName, suffix: &str) {
    *name = if name == sentinel {
        resume.clone()
    } else {
        mangle::suffixed_block(name, suffix)
    };
}

fn freshen_tail_blocks(tail: &mut Tail, sentinel: &BlockName, resume: &BlockName, suffix: &str) {
    match tail {
        Tail::Jump(target) => freshen_block(&mut target.target, sentinel, resume, suffix),
        Tail::Match(target) => {
            for jump in target.cases.values_mut() {
                freshen_block(&mut jump.target, sentinel, resume, suffix);
            }
            if let Some(jump) = &mut target.default {
                freshen_block(&mut jump.target, sentinel, resume, suffix);
            }
        }
        Tail::Call(CallTarget::Direct { resume: r, .. })
        | Tail::Call(CallTarget::Indirect { resume: r, .. }) => {
            freshen_block(r, sentinel, resume, suffix);
        }
        Tail::Host(HostTarget::IoRead { resume: r, .. })
        | Tail::Host(HostTarget::IoWrite { resume: r, .. })
        | Tail::Host(HostTarget::IoOpen { resume: r, .. })
        | Tail::Host(HostTarget::IoClose { resume: r, .. })
        | Tail::Host(HostTarget::IoClockWall { resume: r })
        | Tail::Host(HostTarget::IoClockMono { resume: r })
        | Tail::Host(HostTarget::IoRandom { resume: r, .. }) => {
            freshen_block(r, sentinel, resume, suffix);
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

    fn func(params: Vec<ValueName>, resume: &str, region: Region) -> Func {
        Func {
            params: params.into_iter().map(Into::into).collect(),
            resume: b(resume),
            region,
        }
    }

    fn ret(resume: &str, value: ValueName) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(resume),
            params: vec![value],
        })
    }

    fn direct(target: &str, args: Vec<ValueName>, resume: &str) -> Tail {
        Tail::Call(CallTarget::Direct {
            target: FuncName::from(target),
            params: args,
            resume: b(resume),
        })
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> Option<&'a Func> {
        find_func(module, &FuncName::from(name))
    }

    fn main_region(module: &Module) -> &Region {
        &func_named(module, "main").expect("main").region
    }

    /// A `main` whose tail calls `f`, with a continuation block `cont` that
    /// returns the call's result out of `main`.
    fn main_calling(callee: &str, args: Vec<ValueName>) -> Func {
        let cont = Block {
            params: vec![v("res")],
            region: region(vec![], vec![], ret("rm", v("res"))),
        };
        func(
            vec![],
            "rm",
            region(
                vec![(v("a"), Value::Pure(Data::Nat(3)))],
                vec![(b("cont"), cont)],
                direct(callee, args, "cont"),
            ),
        )
    }

    #[test]
    fn inlines_single_site_freshening_and_stitching_resume() {
        // f(p) = p + p, returning via its resume sentinel "r".
        let f = func(
            vec![v("p")],
            "r",
            region(
                vec![(v("v0"), Value::Eval(Code::NatAdd(v("p"), v("p"))))],
                vec![],
                ret("r", v("v0")),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main_calling("f", vec![v("a")]));
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        // f is consumed and removed.
        assert!(func_named(&module, "f").is_none());

        let region = main_region(&module);

        // The parameter is bound to the argument as a fresh alias.
        assert!(
            region
                .values
                .iter()
                .any(|(n, val)| n == &v("p@f") && matches!(val, Value::Alias(a) if a == &v("a")))
        );

        // The body's compute is freshened and its operands point at the bound param.
        assert!(region.values.iter().any(|(n, val)| n == &v("v0@f")
            && matches!(val, Value::Eval(Code::NatAdd(x, y)) if x == &v("p@f") && y == &v("p@f"))));

        // The callee's return now jumps to the call's continuation block "cont".
        match &region.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("cont"));
                assert_eq!(target.params, vec![v("v0@f")]);
            }
            other => panic!("expected jump to continuation, got {other:?}"),
        }
    }

    #[test]
    fn merges_and_freshens_nested_blocks() {
        // g(n) jumps to an internal block that computes n + n and returns.
        let body = Block {
            params: vec![],
            region: region(
                vec![(v("s"), Value::Eval(Code::NatAdd(v("n"), v("n"))))],
                vec![],
                ret("r", v("s")),
            ),
        };
        let g = func(
            vec![v("n")],
            "r",
            region(
                vec![],
                vec![(b("body"), body)],
                Tail::Jump(JumpTarget {
                    target: b("body"),
                    params: vec![],
                }),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main_calling("g", vec![v("a")]));
        module.add_func(FuncName::from("g"), g);

        inline_calls(&mut module);

        let region = main_region(&module);

        // main now jumps into the freshened internal block.
        match &region.tail {
            Tail::Jump(target) => assert_eq!(target.target, b("body@g")),
            other => panic!("expected jump to merged block, got {other:?}"),
        }

        // The merged block returns to the call's continuation, not g's sentinel.
        let (_, merged) = region
            .blocks
            .iter()
            .find(|(name, _)| name == &b("body@g"))
            .expect("merged block present");
        match &merged.region.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("cont"));
                assert_eq!(target.params, vec![v("s@g")]);
            }
            other => panic!("expected stitched return, got {other:?}"),
        }
    }

    #[test]
    fn inlines_multi_site_size_three_function() {
        // f(p) computes p+p, then (p+p)+p — body size 3 (two values + one tail).
        // Called from three nested sites in `main`: main's tail, k1's tail,
        // and k2's tail. Each splice is freshened with a per-site suffix
        // `@f#{n}` so the inlined copies cannot collide.
        let f = func(
            vec![v("p")],
            "rf",
            region(
                vec![
                    (v("v0"), Value::Eval(Code::NatAdd(v("p"), v("p")))),
                    (v("v1"), Value::Eval(Code::NatAdd(v("v0"), v("p")))),
                ],
                vec![],
                ret("rf", v("v1")),
            ),
        );

        let k2 = Block {
            params: vec![v("res2")],
            region: region(vec![], vec![], direct("f", vec![v("res2")], "rm")),
        };
        let k1 = Block {
            params: vec![v("res1")],
            region: region(
                vec![],
                vec![(b("k2"), k2)],
                direct("f", vec![v("res1")], "k2"),
            ),
        };
        let main = func(
            vec![],
            "rm",
            region(
                vec![(v("a"), Value::Pure(Data::Nat(3)))],
                vec![(b("k1"), k1)],
                direct("f", vec![v("a")], "k1"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        // f is consumed and dropped.
        assert!(func_named(&module, "f").is_none());

        // Site 1 spliced into main's region with suffix `@f#1`.
        let main_r = main_region(&module);
        assert!(matches!(main_r.tail, Tail::Jump(_)));
        assert!(main_r.values.iter().any(|(n, _)| n == &v("v1@f#1")));

        // Site 2 spliced into k1's region with suffix `@f#2`.
        let (_, k1_block) = main_r
            .blocks
            .iter()
            .find(|(n, _)| n == &b("k1"))
            .expect("k1 present");
        assert!(matches!(k1_block.region.tail, Tail::Jump(_)));
        assert!(
            k1_block
                .region
                .values
                .iter()
                .any(|(n, _)| n == &v("v1@f#2"))
        );

        // Site 3 spliced into k2's region with suffix `@f#3`.
        let (_, k2_block) = k1_block
            .region
            .blocks
            .iter()
            .find(|(n, _)| n == &b("k2"))
            .expect("k2 present");
        assert!(matches!(k2_block.region.tail, Tail::Jump(_)));
        assert!(
            k2_block
                .region
                .values
                .iter()
                .any(|(n, _)| n == &v("v1@f#3"))
        );
    }

    #[test]
    fn leaves_large_multi_site_callee_alone() {
        // f's body has 12 items (11 values + 1 tail), above SIZE_THRESHOLD = 8.
        // Called from two sites, so Tier 1 (count != 1) does not apply. Tier 2
        // (size-bounded) skips it too. Both call sites survive.
        let values: Vec<(ValueName, Value)> = (0..11)
            .map(|i| (v(&format!("v{i}")), Value::Pure(Data::Nat(i as u32))))
            .collect();
        let f = func(vec![], "rf", region(values, vec![], ret("rf", v("v10"))));

        let k = Block {
            params: vec![v("res")],
            region: region(vec![], vec![], direct("f", vec![], "rm")),
        };
        let main = func(
            vec![],
            "rm",
            region(vec![], vec![(b("k"), k)], direct("f", vec![], "k")),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        assert!(
            func_named(&module, "f").is_some(),
            "f exceeds the size threshold and should survive"
        );
        assert!(matches!(
            main_region(&module).tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
        let (_, k_block) = main_region(&module)
            .blocks
            .iter()
            .find(|(n, _)| n == &b("k"))
            .expect("k present");
        assert!(matches!(
            k_block.region.tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
    }

    #[test]
    fn leaves_mutually_recursive_callees_alone() {
        // a → b → a forms a non-trivial direct-call cycle. Both bodies are
        // small enough for Tier 2 by size, but the cycle excludes them from
        // both tiers. Neither is inlined.
        let a = func(
            vec![],
            "ra",
            region(vec![], vec![], direct("b", vec![], "ra")),
        );
        let b = func(
            vec![],
            "rb",
            region(vec![], vec![], direct("a", vec![], "rb")),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("a"), a);
        module.add_func(FuncName::from("b"), b);

        inline_calls(&mut module);

        assert!(func_named(&module, "a").is_some());
        assert!(func_named(&module, "b").is_some());
    }

    #[test]
    fn inlines_same_callee_twice_into_same_caller_with_distinct_names() {
        // f is called from two sites that both live in `main`'s region tree:
        // once from main's tail, once from continuation block `k`. After Tier 2
        // inlining the two splices share the same caller but get distinct
        // `@f#1` / `@f#2` suffixes — no name collision.
        let f = func(
            vec![v("p")],
            "rf",
            region(
                vec![(v("v0"), Value::Eval(Code::NatAdd(v("p"), v("p"))))],
                vec![],
                ret("rf", v("v0")),
            ),
        );

        let k = Block {
            params: vec![v("res")],
            region: region(vec![], vec![], direct("f", vec![v("res")], "rm")),
        };
        let main = func(
            vec![],
            "rm",
            region(
                vec![(v("a"), Value::Pure(Data::Nat(3)))],
                vec![(b("k"), k)],
                direct("f", vec![v("a")], "k"),
            ),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        assert!(func_named(&module, "f").is_none());

        let main_r = main_region(&module);

        // Site 1 (main's tail) spliced with @f#1.
        assert!(main_r.values.iter().any(|(n, _)| n == &v("v0@f#1")));
        assert!(main_r.values.iter().any(|(n, _)| n == &v("p@f#1")));

        // Site 2 (continuation block `k`) spliced with @f#2.
        let (_, k_block) = main_r
            .blocks
            .iter()
            .find(|(n, _)| n == &b("k"))
            .expect("k present");
        assert!(k_block.region.values.iter().any(|(n, _)| n == &v("v0@f#2")));
        assert!(k_block.region.values.iter().any(|(n, _)| n == &v("p@f#2")));
    }

    #[test]
    fn never_inlines_the_entrypoint() {
        // `helper` direct-calls the entry `main`, giving it exactly one call
        // site — Tier 1 territory by count. Inlining would splice and *delete*
        // the entrypoint, so it must be skipped.
        let main = func(
            vec![],
            "rm",
            region(
                vec![(v("r"), Value::Pure(Data::Nat(0)))],
                vec![],
                ret("rm", v("r")),
            ),
        );
        let helper = func(
            vec![],
            "rh",
            region(vec![], vec![], direct("main", vec![], "rh")),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("helper"), helper);
        module.set_entry(FuncName::from("main"));

        inline_calls(&mut module);

        assert!(
            func_named(&module, "main").is_some(),
            "the entrypoint must survive inlining"
        );
    }

    #[test]
    fn leaves_self_recursive_callee_alone() {
        // f's single call site is its own recursive tail; inlining would not
        // terminate, so it is skipped.
        let f = func(
            vec![],
            "r",
            region(vec![], vec![], direct("f", vec![], "r")),
        );

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        assert!(func_named(&module, "f").is_some());
    }

    #[test]
    fn inlines_chain_after_dce() {
        // main → f → g, plus a dead closure `c_dead` whose region also calls `g`.
        // `direct_call_counts` walks both funcs and clsrs, so without DCE first
        // `g`'s count is 2 (one from `f`, one from `c_dead`) and the single-call-site
        // rule refuses to inline it. The early `eliminate_dead_code` in the pipeline
        // sweeps `c_dead` (nothing reachable from `main` names it via `Pure(Data::Clsr)`),
        // dropping `g`'s count to 1 so both `f` and `g` inline into `main`. This
        // mirrors the residue `specialize_calls` leaves after rewriting a closure's
        // allocation site to a direct call to the lifted clone.

        let g = func(
            vec![],
            "rg",
            region(
                vec![(v("v0"), Value::Pure(Data::Nat(7)))],
                vec![],
                ret("rg", v("v0")),
            ),
        );

        let f_cont = Block {
            params: vec![v("res")],
            region: region(vec![], vec![], ret("rf", v("res"))),
        };
        let f = func(
            vec![],
            "rf",
            region(vec![], vec![(b("kf"), f_cont)], direct("g", vec![], "kf")),
        );

        let c_dead_cont = Block {
            params: vec![v("res")],
            region: region(vec![], vec![], ret("rcd", v("res"))),
        };
        let c_dead = Clsr {
            fields: vec![],
            params: vec![],
            resume: b("rcd"),
            region: region(
                vec![],
                vec![(b("kcd"), c_dead_cont)],
                direct("g", vec![], "kcd"),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main_calling("f", vec![]));
        module.add_func(FuncName::from("f"), f);
        module.add_func(FuncName::from("g"), g);
        module.add_clsr(ClsrName::from("c_dead"), c_dead);
        module.set_entry(FuncName::from("main"));

        // Pipeline order from src/optm.rs::optimize.
        eliminate_dead_code(&mut module);
        inline_calls(&mut module);

        assert!(
            func_named(&module, "f").is_none(),
            "f should have been inlined"
        );
        assert!(
            func_named(&module, "g").is_none(),
            "g should have been inlined"
        );
        assert!(
            !module
                .clsrs()
                .iter()
                .any(|(name, _)| name == &ClsrName::from("c_dead")),
            "c_dead should have been swept by DCE"
        );

        let region = main_region(&module);
        assert!(
            region
                .values
                .iter()
                .any(|(_, val)| matches!(val, Value::Pure(Data::Nat(7)))),
            "expected g's Pure(Data::Nat(7)) to be spliced into main, got {:?}",
            region.values,
        );
    }
}
