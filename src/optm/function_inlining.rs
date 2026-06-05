use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// General function inlining: splice a `Func` body into a `Direct` call site.
///
/// A `Direct` call carries the callee's whole body's worth of computation behind
/// a call boundary; inlining removes the boundary, bringing the callee's code —
/// and crucially its literal arguments — into the caller's region, where the
/// other passes (copy propagation, constant folding, dead-code elimination) can
/// act on them.
///
/// # Name freshening by suffix
///
/// Value and block names are unique only *within* a body (the lowerer restarts
/// its `v`/`b` counters per region), so a verbatim splice would collide the
/// callee's `v0` with the caller's. Every name the callee *binds* is suffixed
/// with `@{callee}`; since caller names are always `v\d+`/`b\d+`, a suffixed name
/// can never equal one. Free names in the callee are module-level consts (and
/// closure/function names) — those are left untouched so their references still
/// resolve.
///
/// Suffixing alone is not enough: two inlined copies of the same callee would
/// both produce `v0@f`. That is ruled out by only inlining callees with **exactly
/// one** `Direct` call site, so a given callee is unfolded at most once — and its
/// definition, now dead, is removed in the same step. (The callee name is
/// therefore a globally unique suffix across the whole run.)
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
/// Self-recursive callees are skipped, and each round inlines one callee and then
/// deletes it, so `Module::funcs` strictly shrinks until no single-call-site,
/// non-recursive callee remains.
pub fn inline_calls(module: &mut Module) {
    loop {
        let counts = direct_call_counts(module);

        let Some(callee_name) = pick_callee(module, &counts) else {
            return;
        };

        // Clone the callee's *current* body (no stale snapshots: we recompute and
        // re-clone every round) so the splice can borrow the module mutably.
        let callee = find_func(module, &callee_name)
            .cloned()
            .expect("picked callee is present");

        let spliced = module
            .funcs_mut()
            .iter_mut()
            .any(|(_, func)| splice_first(&mut func.region, &callee_name, &callee))
            || module
                .clsrs_mut()
                .iter_mut()
                .any(|(_, clsr)| splice_first(&mut clsr.region, &callee_name, &callee));

        // A single-call-site, non-self-recursive callee always has its one site
        // in some *other* body, so the search above finds it. Guard anyway.
        if !spliced {
            return;
        }

        module.funcs_mut().retain(|(name, _)| name != &callee_name);
    }
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

/// The first function called from exactly one site and not self-recursive.
fn pick_callee(module: &Module, counts: &HashMap<FuncName, usize>) -> Option<FuncName> {
    module
        .funcs()
        .iter()
        .find(|(name, func)| counts.get(name) == Some(&1) && !is_self_recursive(name, func))
        .map(|(name, _)| name.clone())
}

fn is_self_recursive(name: &FuncName, func: &Func) -> bool {
    harvest::region_refs(&func.region).funcs.contains(name)
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
/// and inline there. Returns whether a site was found.
fn splice_first(region: &mut Region, callee_name: &FuncName, callee: &Func) -> bool {
    let here = matches!(
        &region.tail,
        Tail::Call(CallTarget::Direct { target, .. }) if target == callee_name,
    );

    if here {
        inline_at(region, callee_name, callee);
        return true;
    }

    for (_, block) in &mut region.blocks {
        if splice_first(&mut block.region, callee_name, callee) {
            return true;
        }
    }

    false
}

/// Inline `callee` at `host`, whose tail is the `Direct` call to it.
fn inline_at(host: &mut Region, callee_name: &FuncName, callee: &Func) {
    let (args, resume) = match &host.tail {
        Tail::Call(CallTarget::Direct { params, resume, .. }) => (params.clone(), resume.clone()),
        _ => unreachable!("inline_at called on a non-Direct tail"),
    };

    debug_assert_eq!(
        callee.params.len(),
        args.len(),
        "arity mismatch inlining `{callee_name}`",
    );

    let suffix = format!("@{callee_name}");
    let bound = bound_values(callee);

    // Clone and freshen: value uses first (reusing the shared walker, which
    // covers the big `Code` operand match), then binders, block names, and the
    // resume sentinel.
    let mut body = callee.region.clone();
    walk_region_mut(&mut body, &mut Freshen { bound: &bound, suffix: &suffix });
    freshen_structure(&mut body, &bound, &suffix, &callee.resume, &resume);

    // Bind each (freshened) parameter to its argument; copy propagation collapses
    // these aliases on its next run.
    let param_aliases = callee.params.iter().zip(&args).map(|(param, arg)| {
        (suffixed_value(&param.name, &suffix), Value::Alias(arg.clone()))
    });

    host.preallocs.extend(body.preallocs);
    host.values.extend(param_aliases);
    host.values.extend(body.values);
    host.blocks.extend(body.blocks);
    host.tail = body.tail;
}

/// Every value name *bound* in a function body: its parameters plus every
/// prealloc, value, and block-parameter binder in the tree. A name not in this
/// set is free — a module const — and must not be renamed.
fn bound_values(func: &Func) -> HashSet<ValueName> {
    let mut bound: HashSet<ValueName> = func.params.iter().map(|p| p.name.clone()).collect();
    collect_bound(&func.region, &mut bound);
    bound
}

fn collect_bound(region: &Region, bound: &mut HashSet<ValueName>) {
    for (name, _) in &region.preallocs {
        bound.insert(name.clone());
    }
    for (name, _) in &region.values {
        bound.insert(name.clone());
    }
    for (_, block) in &region.blocks {
        for param in &block.params {
            bound.insert(param.clone());
        }
        collect_bound(&block.region, bound);
    }
}

fn suffixed_value(name: &ValueName, suffix: &str) -> ValueName {
    ValueName::from(format!("{name}{suffix}"))
}

/// Renames value *uses* via the shared mutable walker.
struct Freshen<'a> {
    bound: &'a HashSet<ValueName>,
    suffix: &'a str,
}

impl SinkMut for Freshen<'_> {
    fn value_use(&mut self, name: &mut ValueName) {
        if self.bound.contains(name) {
            *name = suffixed_value(name, self.suffix);
        }
    }
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
        *name = suffixed_value(name, suffix);
    }
}

fn freshen_block(name: &mut BlockName, sentinel: &BlockName, resume: &BlockName, suffix: &str) {
    *name = if name == sentinel {
        resume.clone()
    } else {
        BlockName::from(format!("{name}{suffix}"))
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

    fn region(values: Vec<(ValueName, Value)>, blocks: Vec<(BlockName, Block)>, tail: Tail) -> Region {
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
        assert!(region
            .values
            .iter()
            .any(|(n, val)| n == &v("p@f") && matches!(val, Value::Alias(a) if a == &v("a"))));

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
    fn leaves_multi_site_callee_alone() {
        // f is called from two sites, so suffixing cannot keep copies distinct;
        // it must not be inlined.
        let f = func(vec![], "r", region(vec![], vec![], ret("r", v("a"))));

        let mut module = Module::new();
        let caller = func(
            vec![],
            "rm",
            region(
                vec![],
                vec![(
                    b("k"),
                    Block {
                        params: vec![v("res")],
                        region: region(vec![], vec![], direct("f", vec![], "rm")),
                    },
                )],
                direct("f", vec![], "k"),
            ),
        );
        module.add_func(FuncName::from("main"), caller);
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        assert!(func_named(&module, "f").is_some());
        assert!(matches!(
            main_region(&module).tail,
            Tail::Call(CallTarget::Direct { .. })
        ));
    }

    #[test]
    fn leaves_self_recursive_callee_alone() {
        // f's single call site is its own recursive tail; inlining would not
        // terminate, so it is skipped.
        let f = func(vec![], "r", region(vec![], vec![], direct("f", vec![], "r")));

        let mut module = Module::new();
        module.add_func(FuncName::from("f"), f);

        inline_calls(&mut module);

        assert!(func_named(&module, "f").is_some());
    }
}
