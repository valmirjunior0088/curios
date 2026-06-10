use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// Dead-code elimination over a Cont module, in two layers:
///
/// 1. **Intra-region** — within every function and closure body, first drop
///    blocks no edge reaches (a folded `Match` arm, a relocated jump), then drop
///    value and prealloc bindings whose name is never used, iterated to a fixed
///    point so transitively-dead chains collapse.
/// 2. **Inter-module** — drop functions, closures, and consts that are not
///    reachable from the `main` entry point. A const's data is followed too: a
///    const aggregate can name other consts and a const `Data::Clsr` a closure.
///
/// Layer 1 runs first: removing a dead `let v = c{}` deletes the only reference
/// to closure `c`, which layer 2 then sweeps away. This is what collapses the
/// whole prelude — every entrypoint materializes all builtin closures, but a
/// program touches only a handful.
///
/// ## Conservatism: only pure bindings are removed
///
/// A binding is eligible for removal only when its value is a [`Value::Pure`]
/// (data construction — never observable, never trapping) or a [`Value::Alias`]
/// (a rename). [`Value::Eval`] bindings are kept regardless of liveness, because
/// some primitives have side effects (`Io.print`, `Io.read`) or can trap
/// (division, out-of-bounds `get`/`slice`, `Flt`→`Int` truncation), and removing
/// a dead-but-trapping op would erase an observable trap. This still eliminates
/// the entire prelude (every builtin is a `Pure` closure value). A later pass can
/// relax this with a precise effect/totality classification of `Code`.
pub fn eliminate_dead_code(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        dce_region_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        dce_region_tree(&mut clsr.region);
    }

    dce_module(module);
}

/// Whether a value may be dropped when its binding is dead. See the conservatism
/// note above.
fn is_removable(value: &Value) -> bool {
    matches!(value, Value::Pure(_) | Value::Alias(_))
}

// --- Layer 1: intra-region liveness -----------------------------------------

/// Eliminate dead bindings in a region and all its nested blocks, to a fixed
/// point. A prealloc and its same-named fill (a `rec` backpatch cell) share a
/// name, so they are kept or dropped together automatically.
///
/// Unreachable blocks are pruned first, so their value uses no longer pin live
/// values in the liveness loop below (and their func/closure references no longer
/// pin code in the module sweep). Constant folding decides a `Match` to a single
/// arm and jump threading relocates edges, both of which can orphan whole blocks.
fn dce_region_tree(region: &mut Region) {
    prune_unreachable_blocks(region);

    loop {
        let used = harvest::value_uses(region);

        if !retain_live(region, &used) {
            break;
        }
    }
}

/// Drop every block not reachable from the body's entry. A folded `Match` arm or a
/// relocated jump can leave a block (or a whole cycle of them) with no remaining
/// edge into it; nothing else removes those.
fn prune_unreachable_blocks(region: &mut Region) {
    let reachable = reachable_blocks(region);
    retain_reachable(region, &reachable);
}

/// The block names reachable from the root tail, transitively following each
/// reachable block's own tail edges. A plain reference count is not enough — a dead
/// two-block cycle keeps each other's count at one — so this is a fixed-point walk.
fn reachable_blocks(region: &Region) -> HashSet<BlockName> {
    // Block names are unique within a body, so a single tree-wide index is exact.
    let mut index: HashMap<BlockName, &Region> = HashMap::new();
    index_blocks(region, &mut index);

    let mut reachable: HashSet<BlockName> = HashSet::new();
    let mut work: Vec<BlockName> = tail_targets(&region.tail).into_iter().cloned().collect();

    while let Some(name) = work.pop() {
        if !reachable.insert(name.clone()) {
            continue;
        }

        if let Some(block_region) = index.get(&name) {
            work.extend(tail_targets(&block_region.tail).into_iter().cloned());
        }
    }

    reachable
}

fn index_blocks<'a>(region: &'a Region, index: &mut HashMap<BlockName, &'a Region>) {
    for (name, block) in &region.blocks {
        index.insert(name.clone(), &block.region);
        index_blocks(&block.region, index);
    }
}

/// Retain only reachable blocks at every level. Nesting cannot be orphaned: a jump
/// only targets a lexically-visible block, so a reachable nested block implies its
/// enclosing block is reachable too — a flat membership test is sound.
fn retain_reachable(region: &mut Region, reachable: &HashSet<BlockName>) {
    region.blocks.retain(|(name, _)| reachable.contains(name));

    for (_, block) in &mut region.blocks {
        retain_reachable(&mut block.region, reachable);
    }
}

/// Drop dead, removable bindings throughout the region tree. Returns whether
/// anything was removed.
fn retain_live(region: &mut Region, used: &HashSet<ValueName>) -> bool {
    let mut changed = false;

    let before = region.values.len();
    region
        .values
        .retain(|(name, value)| used.contains(name) || !is_removable(value));
    changed |= region.values.len() != before;

    let before = region.preallocs.len();
    region.preallocs.retain(|(name, _)| used.contains(name));
    changed |= region.preallocs.len() != before;

    for (_, block) in &mut region.blocks {
        changed |= retain_live(&mut block.region, used);
    }

    changed
}

// --- Layer 2: inter-module reachability -------------------------------------

/// Drop functions, closures, and consts unreachable from the entrypoint.
fn dce_module(module: &mut Module) {
    let entry = module.entry().expect("module has an entrypoint").clone();

    // A const's data can name other values (a const aggregate's elements) and a
    // closure (a const `Data::Clsr`), edges a region walk never reaches; index the
    // consts so the worklist can follow them.
    let consts: HashMap<ValueName, Data> = module
        .consts()
        .iter()
        .map(|(name, data)| (name.clone(), data.clone()))
        .collect();

    // Index funcs and clsrs by name so each worklist item is one hash lookup
    // instead of a linear scan of the module.
    let funcs = module
        .funcs()
        .iter()
        .map(|(n, f)| (n, f))
        .collect::<HashMap<_, _>>();
    let clsrs = module
        .clsrs()
        .iter()
        .map(|(n, c)| (n, c))
        .collect::<HashMap<_, _>>();

    let mut keep_funcs: HashSet<FuncName> = HashSet::new();
    let mut keep_clsrs: HashSet<ClsrName> = HashSet::new();
    let mut used_values: HashSet<ValueName> = HashSet::new();

    let mut work_funcs: Vec<FuncName> = vec![entry.clone()];
    let mut work_clsrs: Vec<ClsrName> = vec![];
    let mut work_consts: Vec<ValueName> = vec![];
    keep_funcs.insert(entry);

    loop {
        let refs = if let Some(name) = work_funcs.pop() {
            funcs
                .get(&name)
                .map(|func| harvest::region_refs(&func.region))
        } else if let Some(name) = work_clsrs.pop() {
            clsrs
                .get(&name)
                .map(|clsr| harvest::region_refs(&clsr.region))
        } else if let Some(name) = work_consts.pop() {
            consts.get(&name).map(harvest::data_refs)
        } else {
            break;
        };

        let Some(refs) = refs else {
            continue;
        };

        for value in refs.values {
            see_value(value, &consts, &mut used_values, &mut work_consts);
        }
        for func in refs.funcs {
            if keep_funcs.insert(func.clone()) {
                work_funcs.push(func);
            }
        }
        for clsr in refs.clsrs {
            if keep_clsrs.insert(clsr.clone()) {
                work_clsrs.push(clsr);
            }
        }
    }

    module
        .funcs_mut()
        .retain(|(name, _)| keep_funcs.contains(name));
    module
        .clsrs_mut()
        .retain(|(name, _)| keep_clsrs.contains(name));
    module
        .consts_mut()
        .retain(|(name, _)| used_values.contains(name));
}

// Record a discovered value use, and if it names a const, enqueue that const so
// its own references are followed in turn.
fn see_value(
    name: ValueName,
    consts: &HashMap<ValueName, Data>,
    used: &mut HashSet<ValueName>,
    work: &mut Vec<ValueName>,
) {
    if used.insert(name.clone()) && consts.contains_key(&name) {
        work.push(name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    /// A region whose tail jumps to `b0` passing `tail_args`.
    fn region(values: Vec<(ValueName, Value)>, tail_args: Vec<ValueName>) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks: vec![],
            tail: Tail::Jump(JumpTarget {
                target: BlockName::from("b0"),
                params: tail_args,
            }),
        }
    }

    fn main_func(region: Region) -> Module {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region,
            },
        );
        module.set_entry(FuncName::from("main"));
        module
    }

    fn trivial_clsr() -> Clsr {
        Clsr {
            fields: vec![],
            params: vec![v("x").into()],
            resume: BlockName::from("b0"),
            region: region(vec![], vec![v("x")]),
        }
    }

    fn value_names(module: &Module) -> Vec<String> {
        module.funcs()[0]
            .1
            .region
            .values
            .iter()
            .map(|(name, _)| name.as_string())
            .collect()
    }

    #[test]
    fn drops_dead_pure_value_keeps_live_one() {
        let mut module = main_func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Pure(Data::Nat(2))),
            ],
            vec![v("v1")],
        ));

        eliminate_dead_code(&mut module);

        assert_eq!(value_names(&module), vec!["v1"]);
    }

    #[test]
    fn collapses_transitively_dead_chain_to_a_fixed_point() {
        // v1 = (v0,) is dead; once removed, v0's only use is gone too.
        let mut module = main_func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Pure(Data::Tpl(vec![v("v0")]))),
            ],
            vec![],
        ));

        eliminate_dead_code(&mut module);

        assert!(value_names(&module).is_empty());
    }

    #[test]
    fn keeps_dead_eval_bindings() {
        // An effectful/trapping op is never removed, even when its result is dead.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![v("a").into(), v("b").into()],
                resume: BlockName::from("b0"),
                region: region(
                    vec![(v("v0"), Value::Eval(Code::NatDiv(v("a"), v("b"))))],
                    vec![],
                ),
            },
        );
        module.set_entry(FuncName::from("main"));

        eliminate_dead_code(&mut module);

        assert_eq!(value_names(&module), vec!["v0"]);
    }

    #[test]
    fn drops_closures_unreachable_from_main() {
        let mut module = Module::new();

        // main calls through a closure value bound to `c_used`.
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(
                        v("v0"),
                        Value::Pure(Data::Clsr(ClsrName::from("c_used"), vec![])),
                    )],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Indirect {
                        target: v("v0"),
                        params: vec![],
                        resume: BlockName::from("b0"),
                    }),
                },
            },
        );

        module.add_clsr(ClsrName::from("c_used"), trivial_clsr());
        module.add_clsr(ClsrName::from("c_dead"), trivial_clsr());
        module.set_entry(FuncName::from("main"));

        eliminate_dead_code(&mut module);

        let kept: Vec<String> = module
            .clsrs()
            .iter()
            .map(|(name, _)| name.as_string())
            .collect();
        assert_eq!(kept, vec!["c_used"]);
    }

    fn jump(target: &str) -> Tail {
        Tail::Jump(JumpTarget {
            target: BlockName::from(target),
            params: vec![],
        })
    }

    fn empty_block(tail: Tail) -> Block {
        Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail,
            },
        }
    }

    fn block_names(module: &Module) -> Vec<String> {
        module.funcs()[0]
            .1
            .region
            .blocks
            .iter()
            .map(|(name, _)| name.as_string())
            .collect()
    }

    #[test]
    fn prunes_orphaned_and_cyclic_dead_blocks() {
        // Only `live` is reached from the tail; `dead1`/`dead2` form an unreachable
        // cycle (each keeps the other's predecessor count at one), so a reference
        // count would miss them — a reachability sweep removes both.
        let mut module = main_func(Region {
            preallocs: vec![],
            values: vec![],
            blocks: vec![
                (BlockName::from("live"), empty_block(jump("rm"))),
                (BlockName::from("dead1"), empty_block(jump("dead2"))),
                (BlockName::from("dead2"), empty_block(jump("dead1"))),
            ],
            tail: jump("live"),
        });

        eliminate_dead_code(&mut module);

        assert_eq!(block_names(&module), vec!["live"]);
    }

    #[test]
    fn keeps_a_block_reached_only_through_a_call_resume() {
        // `k` has no jump/match edge — its sole predecessor is the call's resume.
        let mut module = main_func(Region {
            preallocs: vec![],
            values: vec![],
            blocks: vec![(BlockName::from("k"), empty_block(jump("rm")))],
            tail: Tail::Call(CallTarget::Direct {
                target: FuncName::from("f"),
                params: vec![],
                resume: BlockName::from("k"),
            }),
        });
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![],
                resume: BlockName::from("rf"),
                region: region(vec![(v("r"), Value::Pure(Data::Nat(0)))], vec![v("r")]),
            },
        );

        eliminate_dead_code(&mut module);

        assert_eq!(block_names(&module), vec!["k"]);
    }

    #[test]
    fn keeps_consts_and_closures_reachable_through_const_data() {
        // main returns const `agg`; agg = (scalar, clo); clo = c{}. Following the
        // const's data is what keeps `scalar`, `clo`, and the closure `c` alive.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Jump(JumpTarget {
                        target: BlockName::from("rm"),
                        params: vec![v("agg")],
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));

        module.add_const(v("agg"), Data::Tpl(vec![v("scalar"), v("clo")]));
        module.add_const(v("scalar"), Data::Nat(5));
        module.add_const(v("clo"), Data::Clsr(ClsrName::from("c"), vec![]));
        module.add_const(v("orphan"), Data::Nat(9));
        module.add_clsr(
            ClsrName::from("c"),
            Clsr {
                fields: vec![],
                params: vec![v("x").into()],
                resume: BlockName::from("rc"),
                region: region(vec![], vec![v("x")]),
            },
        );

        eliminate_dead_code(&mut module);

        let mut kept_consts: Vec<String> = module
            .consts()
            .iter()
            .map(|(name, _)| name.as_string())
            .collect();
        kept_consts.sort();
        assert_eq!(kept_consts, vec!["agg", "clo", "scalar"]);
        assert_eq!(module.clsrs().len(), 1);
    }

    #[test]
    fn drops_a_const_chain_no_body_references() {
        // agg = (inner) but nothing references agg → both consts go.
        let mut module = main_func(region(vec![], vec![]));
        module.add_const(v("agg"), Data::Tpl(vec![v("inner")]));
        module.add_const(v("inner"), Data::Nat(5));

        eliminate_dead_code(&mut module);

        assert!(module.consts().is_empty());
    }
}
