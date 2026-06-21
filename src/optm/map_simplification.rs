use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Map simplification: rewrite an `Arr.map` whose closure is provably the
/// identity into a copy of its source.
///
/// `Arr/map` is a primitive that codegen lowers to an opaque O(n) fill loop, so
/// every reasoning pass otherwise treats it as a black box that uses both its
/// source and its closure. When the closure is the identity, the map yields an
/// array equal to its source; arrays are immutable here, so the fresh allocation
/// is unobservable and the binding collapses to a plain alias.
///
/// The alias is what lets the rest of the pipeline see through the map: copy
/// propagation threads the source into the map's consumers — which is what fuses
/// `Bin/flatten(Arr/map(to_bin, xs))` into `Bin/flatten(xs)` once the newtype
/// projection `to_bin` has erased to the identity — and dead-code elimination
/// then drops the now-unreferenced closure value and its lifted function.
pub fn simplify_maps(module: &mut Module) {
    let identity = collect_identity_closures(module);

    if identity.is_empty() {
        return;
    }

    let const_clsrs = capture_free_clsr_consts(module);

    for (_, func) in module.funcs_mut() {
        simplify_in_tree(&mut func.region, &identity, &const_clsrs);
    }
    for (_, clsr) in module.clsrs_mut() {
        simplify_in_tree(&mut clsr.region, &identity, &const_clsrs);
    }
}

/// The names of every closure definition that is the identity function.
fn collect_identity_closures(module: &Module) -> HashSet<ClsrName> {
    module
        .clsrs()
        .iter()
        .filter(|(_, clsr)| is_identity(clsr))
        .map(|(name, _)| name.clone())
        .collect()
}

/// A closure is the identity when it captures nothing, binds a single parameter,
/// and its body immediately resumes with that parameter — no intervening values,
/// blocks, or preallocations. A stricter shape than necessary, but a sound
/// sufficient condition: anything that fails the match is simply left alone.
fn is_identity(clsr: &Clsr) -> bool {
    clsr.fields.is_empty()
        && clsr.params.len() == 1
        && clsr.region.preallocs.is_empty()
        && clsr.region.values.is_empty()
        && clsr.region.blocks.is_empty()
        && matches!(
            &clsr.region.tail,
            Tail::Jump(target)
                if target.target == clsr.resume
                    && target.params.len() == 1
                    && clsr.params[0] == target.params[0]
        )
}

/// Resolve a closure-valued const name to its `ClsrName`. Only capture-free
/// closures qualify — which is no loss, since an identity closure ignores
/// everything but its argument and so never captures.
fn capture_free_clsr_consts(module: &Module) -> HashMap<ValueName, ClsrName> {
    module
        .consts()
        .iter()
        .filter_map(|(name, data)| match data {
            Data::Clsr(clsr, captures) if captures.is_empty() => Some((name.clone(), clsr.clone())),
            _ => None,
        })
        .collect()
}

fn simplify_in_tree(
    region: &mut Region,
    identity: &HashSet<ClsrName>,
    const_clsrs: &HashMap<ValueName, ClsrName>,
) {
    // Closures built locally in this region tree resolve too, so a map over a
    // not-yet-hoisted identity closure is still caught.
    let mut clsrs = const_clsrs.clone();
    collect_local_clsrs(region, &mut clsrs);
    rewrite_maps(region, identity, &clsrs);
}

fn collect_local_clsrs(region: &Region, clsrs: &mut HashMap<ValueName, ClsrName>) {
    for (name, value) in &region.values {
        if let Value::Pure(Data::Clsr(clsr, captures)) = value
            && captures.is_empty()
        {
            clsrs.insert(name.clone(), clsr.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_local_clsrs(&block.region, clsrs);
    }
}

fn rewrite_maps(
    region: &mut Region,
    identity: &HashSet<ClsrName>,
    clsrs: &HashMap<ValueName, ClsrName>,
) {
    for (_, value) in &mut region.values {
        let source = match value {
            Value::Eval(Code::ArrMap(source, f))
                if clsrs.get(f).is_some_and(|clsr| identity.contains(clsr)) =>
            {
                source.clone()
            }
            _ => continue,
        };

        *value = Value::Alias(source);
    }

    for (_, block) in &mut region.blocks {
        rewrite_maps(&mut block.region, identity, clsrs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn resume_with(param: &str) -> Tail {
        Tail::Jump(JumpTarget {
            target: BlockName::from("b0"),
            params: vec![v(param)],
        })
    }

    fn identity_clsr() -> Clsr {
        Clsr {
            fields: vec![],
            params: vec![Argument::from(v("p"))],
            resume: BlockName::from("b0"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: resume_with("p"),
            },
        }
    }

    /// A closure that does real work — adds one to its argument — so the
    /// identity match must reject it.
    fn increment_clsr() -> Clsr {
        Clsr {
            fields: vec![],
            params: vec![Argument::from(v("p"))],
            resume: BlockName::from("b0"),
            region: Region {
                preallocs: vec![],
                values: vec![(v("q"), Value::Eval(Code::NatAdd(v("p"), v("p"))))],
                blocks: vec![],
                tail: resume_with("q"),
            },
        }
    }

    /// `main` maps the closure named `clsr` (a capture-free const `fn`) over a
    /// literal array and returns the result.
    fn map_module(clsr: ClsrName, definition: Clsr) -> Module {
        let mut module = Module::new();

        module.add_const(v("fn"), Data::Clsr(clsr.clone(), vec![]));
        module.add_clsr(clsr, definition);
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values: vec![
                        (v("arr"), Value::Pure(Data::Arr(vec![]))),
                        (v("mapped"), Value::Eval(Code::ArrMap(v("arr"), v("fn")))),
                    ],
                    blocks: vec![],
                    tail: Tail::Jump(JumpTarget {
                        target: BlockName::from("b0"),
                        params: vec![v("mapped")],
                    }),
                },
            },
        );

        module
    }

    fn mapped_binding(module: &Module) -> &Value {
        &module.funcs()[0].1.region.values[1].1
    }

    #[test]
    fn identity_map_becomes_an_alias_to_its_source() {
        let mut module = map_module(ClsrName::from("id"), identity_clsr());

        simplify_maps(&mut module);

        match mapped_binding(&module) {
            Value::Alias(source) => assert_eq!(source, &v("arr")),
            other => panic!("expected alias to source, got {other:?}"),
        }
    }

    #[test]
    fn non_identity_map_is_left_alone() {
        let mut module = map_module(ClsrName::from("inc"), increment_clsr());

        simplify_maps(&mut module);

        match mapped_binding(&module) {
            Value::Eval(Code::ArrMap(source, f)) => {
                assert_eq!((source, f), (&v("arr"), &v("fn")));
            }
            other => panic!("expected the map to survive, got {other:?}"),
        }
    }
}
