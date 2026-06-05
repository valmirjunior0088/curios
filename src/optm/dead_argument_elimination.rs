use {
    super::{harvest, *},
    std::collections::{HashMap, HashSet},
};

/// Dead argument elimination: drop parameters and captures a body never uses,
/// along with the matching arguments at every coordinated site.
///
/// After type erasure, erased type arguments survive as unit values that are
/// passed but never used. This pass removes them, finishing what erasure starts.
/// Two argument kinds can be removed soundly, because each is positional and its
/// reference sites are findable by name:
///
/// - **Function parameters.** A `Func` is referenced only by `Direct` calls, so a
///   parameter unused in its body can be dropped from the definition and from the
///   argument list of every `Direct` call to that name.
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
/// The pass runs to a fixed point so that removals cascade through wrappers:
/// dropping `f`'s unused parameter trims `f(x)` to `f()` in a caller `g`, which
/// can leave `g`'s own `x` unused for the next round. Deadness is "never appears
/// in a use position", recomputed each round — exactly the discarded unit values,
/// with no need for full liveness. The trailing dead-code pass then sweeps the
/// now-unreferenced argument bindings.
pub fn eliminate_dead_arguments(module: &mut Module) {
    loop {
        let dropped_params = drop_dead_func_params(module);
        let dropped_fields = drop_dead_clsr_fields(module);

        if !(dropped_params || dropped_fields) {
            return;
        }
    }
}

// --- Function parameters ----------------------------------------------------

fn drop_dead_func_params(module: &mut Module) -> bool {
    let drops = dead_func_params(module);

    if drops.is_empty() {
        return false;
    }

    for (name, func) in module.funcs_mut() {
        if let Some(dead) = drops.get(name) {
            retain_kept(&mut func.params, dead);
        }
    }

    for (_, func) in module.funcs_mut() {
        trim_direct_args(&mut func.region, &drops);
    }
    for (_, clsr) in module.clsrs_mut() {
        trim_direct_args(&mut clsr.region, &drops);
    }

    true
}

/// The unused parameter positions of every function but the entrypoint, whose
/// signature is the host's entry contract.
fn dead_func_params(module: &Module) -> HashMap<FuncName, HashSet<usize>> {
    let mut drops = HashMap::new();

    for (name, func) in module.funcs() {
        if Some(name) == module.entry() {
            continue;
        }

        let used = harvest::value_uses(&func.region);
        let dead = dead_positions(&func.params, &used);

        if !dead.is_empty() {
            drops.insert(name.clone(), dead);
        }
    }

    drops
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
    args
        .iter()
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
        let f = func(vec![v("a"), v("b")], "rf", region(vec![], ret("rf", v("b"))));

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
        let g = func(vec![v("x")], "rg", region(vec![], direct("f", vec![v("x")], "rg")));
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
