use {
    super::{harvest, *},
    std::collections::HashSet,
};

/// Dead-code elimination over a Cont module, in two layers:
///
/// 1. **Intra-region** — within every function and closure body, drop value and
///    prealloc bindings whose name is never used, iterated to a fixed point so
///    transitively-dead chains collapse.
/// 2. **Inter-module** — drop functions, closures, and consts that are not
///    reachable from the `main` entry point.
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
fn dce_region_tree(region: &mut Region) {
    loop {
        let used = harvest::value_uses(region);

        if !retain_live(region, &used) {
            break;
        }
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

/// Drop functions, closures, and consts unreachable from `main`.
fn dce_module(module: &mut Module) {
    let entry = FuncName::from("main");

    let mut keep_funcs: HashSet<FuncName> = HashSet::new();
    let mut keep_clsrs: HashSet<ClsrName> = HashSet::new();
    let mut used_values: HashSet<ValueName> = HashSet::new();

    let mut work_funcs: Vec<FuncName> = vec![entry.clone()];
    let mut work_clsrs: Vec<ClsrName> = vec![];
    keep_funcs.insert(entry);

    loop {
        let region = if let Some(name) = work_funcs.pop() {
            module
                .funcs()
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, func)| &func.region)
        } else if let Some(name) = work_clsrs.pop() {
            module
                .clsrs()
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, clsr)| &clsr.region)
        } else {
            break;
        };

        let Some(region) = region else {
            continue;
        };

        let refs = harvest::region_refs(region);
        used_values.extend(refs.values);

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

    module.funcs_mut().retain(|(name, _)| keep_funcs.contains(name));
    module.clsrs_mut().retain(|(name, _)| keep_clsrs.contains(name));
    module
        .consts_mut()
        .retain(|(name, _)| used_values.contains(name));
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
        module
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
                params: vec![v("a"), v("b")],
                resume: BlockName::from("b0"),
                region: region(
                    vec![(v("v0"), Value::Eval(Code::NatDiv(v("a"), v("b"))))],
                    vec![],
                ),
            },
        );

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

        let trivial_clsr = || Clsr {
            fields: vec![],
            params: vec![v("x")],
            resume: BlockName::from("b0"),
            region: region(vec![], vec![v("x")]),
        };
        module.add_clsr(ClsrName::from("c_used"), trivial_clsr());
        module.add_clsr(ClsrName::from("c_dead"), trivial_clsr());

        eliminate_dead_code(&mut module);

        let kept: Vec<String> = module
            .clsrs()
            .iter()
            .map(|(name, _)| name.as_string())
            .collect();
        assert_eq!(kept, vec!["c_used"]);
    }
}
