use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Closure lifting + known-closure devirtualization.
///
/// A closure is *code + a captured environment*; a function is code alone. This
/// pass removes the environment from any call site where it is statically known,
/// turning the captures into ordinary leading arguments:
///
/// ```text
///   let v = c{a, b};        Clsr c { fields: [x, y], params: [p], .. }
///   v(arg)  (indirect)  ->  c@lifted(a, b, arg)  (direct, to)
///                           Func c@lifted { params: [x, y, p], .. }
/// ```
///
/// Because `call_direct_instrs` and `call_indirect_instrs` share their `resume`
/// convention, the rewrite preserves `resume` verbatim, and the lifted function's
/// arity (`fields + params`) matches the rewritten call's argument count
/// (`captures + args`). Lifting the body itself is free: a closure region is
/// already written against its `fields` and `params` as bound names, so promoting
/// the captures to leading parameters leaves the region unchanged.
///
/// The pass is a strict improvement on its own: a call is rewritten only when its
/// callee traces to a known `Data::Clsr` binding in the same region tree, so
/// escaping closures (whose value flows somewhere other than a call) keep their
/// indirect call and their closure. Once a closure is called only at known sites,
/// its closure value goes dead and dead-code elimination drops the now-orphaned
/// `Clsr`, leaving just the lifted `Func`.
///
/// # The self-capture seed
///
/// One known closure is not expressed as a `let` binding: a recursive closure
/// captures *itself* (the lowerer emits `prealloc %v: c` plus a fill
/// `%v = c{…, %v, …}`), so inside `c`'s own body the self-capture field holds a
/// closure whose code is `c` and whose captures are exactly the current fields.
/// When every allocation of `c` in the module self-captures at the same field
/// index, that fact is universal, and `c`'s body is devirtualized against it:
/// the indirect self-call through the field becomes a direct call to `c@lifted`
/// with the fields threaded as leading arguments. This is what later lets the
/// recursion be seen as a direct self-cycle (and converted to a loop) instead
/// of hiding behind the closure value forever.
pub fn lift_closures(module: &mut Module) {
    let self_captures = self_capture_fields(module);
    let mut to_lift = HashSet::new();

    for (_, func) in module.funcs_mut() {
        devirtualize_tree(&mut func.region, None, &mut to_lift);
    }
    for (name, clsr) in module.clsrs_mut() {
        let seed = self_captures.get(name).map(|&index| {
            let fields = clsr.fields.iter().map(|f| f.name.clone()).collect();
            (clsr.fields[index].name.clone(), (name.clone(), fields))
        });
        devirtualize_tree(&mut clsr.region, seed, &mut to_lift);
    }

    // Lifted bodies are cloned *after* devirtualization, so they already carry
    // the rewritten call sites of the closure they came from.
    for (name, func) in lift_funcs(module, &to_lift) {
        module.add_func(name, func);
    }
}

// --- Devirtualization -------------------------------------------------------

/// Maps a value name to the closure it is statically bound to, with its captures.
type Known = HashMap<ValueName, (ClsrName, Vec<ValueName>)>;

fn devirtualize_tree(
    region: &mut Region,
    seed: Option<(ValueName, (ClsrName, Vec<ValueName>))>,
    to_lift: &mut HashSet<ClsrName>,
) {
    let mut known = Known::new();

    // The self-capture seed goes in first, so a (theoretical) same-named `let`
    // binding collected from the body would override it.
    if let Some((name, entry)) = seed {
        known.insert(name, entry);
    }
    collect_known(region, &mut known);

    if known.is_empty() {
        return;
    }

    rewrite_calls(region, &known, to_lift);
}

/// The field index of each closure's *universal* self-capture: the position
/// that holds the allocation's own binding name in **every** allocation of that
/// closure across the module. Universality is the soundness gate — the body
/// rewrite must hold for every instance of the closure, so one allocation
/// without the self-capture (or a const allocation, which has no binding name
/// to capture) disqualifies it.
fn self_capture_fields(module: &Module) -> HashMap<ClsrName, usize> {
    // Per closure: the self-capturing field indices common to all allocations
    // seen so far; `None` once disqualified.
    let mut indices: HashMap<ClsrName, Option<HashSet<usize>>> = HashMap::new();

    fn scan(region: &Region, indices: &mut HashMap<ClsrName, Option<HashSet<usize>>>) {
        for (name, value) in &region.values {
            if let Value::Pure(Data::Clsr(clsr, captures)) = value {
                let here: HashSet<usize> = captures
                    .iter()
                    .enumerate()
                    .filter(|(_, capture)| *capture == name)
                    .map(|(index, _)| index)
                    .collect();

                indices
                    .entry(clsr.clone())
                    .and_modify(|entry| {
                        if let Some(common) = entry {
                            common.retain(|index| here.contains(index));
                            if common.is_empty() {
                                *entry = None;
                            }
                        }
                    })
                    .or_insert_with(|| (!here.is_empty()).then_some(here));
            }
        }

        for (_, block) in &region.blocks {
            scan(&block.region, indices);
        }
    }

    for (_, func) in module.funcs() {
        scan(&func.region, &mut indices);
    }
    for (_, clsr) in module.clsrs() {
        scan(&clsr.region, &mut indices);
    }
    for (_, data) in module.consts() {
        if let Data::Clsr(clsr, _) = data {
            indices.insert(clsr.clone(), None);
        }
    }

    indices
        .into_iter()
        .filter_map(|(name, common)| {
            let index = common.and_then(|set| set.into_iter().min())?;
            Some((name, index))
        })
        .collect()
}

fn collect_known(region: &Region, known: &mut Known) {
    for (name, value) in &region.values {
        if let Value::Pure(Data::Clsr(clsr, captures)) = value {
            known.insert(name.clone(), (clsr.clone(), captures.clone()));
        }
    }

    for (_, block) in &region.blocks {
        collect_known(&block.region, known);
    }
}

fn rewrite_calls(region: &mut Region, known: &Known, to_lift: &mut HashSet<ClsrName>) {
    rewrite_tail(&mut region.tail, known, to_lift);

    for (_, block) in &mut region.blocks {
        rewrite_calls(&mut block.region, known, to_lift);
    }
}

fn rewrite_tail(tail: &mut Tail, known: &Known, to_lift: &mut HashSet<ClsrName>) {
    let replacement = match tail {
        Tail::Call(CallTarget::Indirect {
            target,
            params,
            resume,
        }) => known.get(target).map(|(clsr, captures)| {
            let mut lifted_params = captures.clone();
            lifted_params.extend(params.iter().cloned());

            to_lift.insert(clsr.clone());

            Tail::Call(CallTarget::Direct {
                target: mangle::lifted(clsr),
                params: lifted_params,
                resume: resume.clone(),
            })
        }),
        _ => None,
    };

    if let Some(new_tail) = replacement {
        *tail = new_tail;
    }
}

// --- Lifting ----------------------------------------------------------------

/// Build a lifted function for each closure that was devirtualized, by folding
/// its captured `fields` into leading parameters and cloning its (already
/// devirtualized) region.
fn lift_funcs(module: &Module, to_lift: &HashSet<ClsrName>) -> Vec<(FuncName, Func)> {
    module
        .clsrs()
        .iter()
        .filter(|(name, _)| {
            // A closure lifted on an earlier run already has its function —
            // `add_func` is a plain push, so re-adding would shadow it.
            to_lift.contains(name)
                && !module
                    .funcs()
                    .iter()
                    .any(|(present, _)| present == &mangle::lifted(name))
        })
        .map(|(name, clsr)| {
            // Captures become leading parameters — as `Argument`s, their
            // candidate flags ride along with the names for free.
            let mut params = clsr.fields.clone();
            params.extend(clsr.params.iter().cloned());

            let func = Func {
                params,
                resume: clsr.resume.clone(),
                region: clsr.region.clone(),
            };

            (mangle::lifted(name), func)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn clsr(fields: Vec<ValueName>, params: Vec<ValueName>) -> Clsr {
        let tail = Tail::Jump(JumpTarget {
            target: BlockName::from("b0"),
            params: params.clone(),
        });
        Clsr {
            fields: fields.into_iter().map(Into::into).collect(),
            params: params.into_iter().map(Into::into).collect(),
            resume: BlockName::from("b0"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail,
            },
        }
    }

    fn main_with(values: Vec<(ValueName, Value)>, tail: Tail) -> Func {
        Func {
            params: vec![],
            resume: BlockName::from("b0"),
            region: Region {
                preallocs: vec![],
                values,
                blocks: vec![],
                tail,
            },
        }
    }

    fn indirect(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Call(CallTarget::Indirect {
            target: v(target),
            params: args,
            resume: BlockName::from("b1"),
        })
    }

    fn main_tail(module: &Module) -> &Tail {
        &module
            .funcs()
            .iter()
            .find(|(name, _)| name.as_str() == "main")
            .unwrap()
            .1
            .region
            .tail
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> Option<&'a Func> {
        module
            .funcs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, func)| func)
    }

    #[test]
    fn devirtualizes_known_call_and_lifts_captures_into_params() {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            main_with(
                vec![
                    (v("a"), Value::Pure(Data::Nat(1))),
                    (v("arg"), Value::Pure(Data::Nat(2))),
                    (
                        v("clo"),
                        Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![v("a")])),
                    ),
                ],
                indirect("clo", vec![v("arg")]),
            ),
        );
        module.add_clsr(ClsrName::from("c0"), clsr(vec![v("f")], vec![v("p")]));

        lift_closures(&mut module);

        // The indirect call becomes a direct call to the lifted function, with the
        // captured `a` prepended to the original `arg`.
        match main_tail(&module) {
            Tail::Call(CallTarget::Direct {
                target,
                params,
                resume,
            }) => {
                assert_eq!(target.as_str(), "c0@lifted");
                assert_eq!(params, &vec![v("a"), v("arg")]);
                assert_eq!(resume.as_str(), "b1");
            }
            other => panic!("expected direct call, got {other:?}"),
        }

        // The lifted function takes the captured fields as leading params.
        let lifted = func_named(&module, "c0@lifted").expect("lifted func");
        assert_eq!(lifted.params, vec![v("f"), v("p")]);
    }

    #[test]
    fn second_lift_does_not_duplicate_an_existing_lifted_func() {
        // Run 1 lifts c0. A later pass exposes a *new* known call site of the
        // same closure; run 2 devirtualizes it but must not push a second
        // c0@lifted — `add_func` is a plain push, so a twin would shadow.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            main_with(
                vec![
                    (v("a"), Value::Pure(Data::Nat(1))),
                    (
                        v("clo"),
                        Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![v("a")])),
                    ),
                ],
                indirect("clo", vec![]),
            ),
        );
        module.add_clsr(ClsrName::from("c0"), clsr(vec![v("f")], vec![]));

        lift_closures(&mut module);

        module.add_func(
            FuncName::from("late"),
            main_with(
                vec![
                    (v("b"), Value::Pure(Data::Nat(2))),
                    (
                        v("clo2"),
                        Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![v("b")])),
                    ),
                ],
                indirect("clo2", vec![]),
            ),
        );

        lift_closures(&mut module);

        let lifted_count = module
            .funcs()
            .iter()
            .filter(|(name, _)| name.as_str() == "c0@lifted")
            .count();
        assert_eq!(lifted_count, 1, "c0@lifted must exist exactly once");

        // Both call sites target the one lifted function.
        for func_name in ["main", "late"] {
            match &func_named(&module, func_name).unwrap().region.tail {
                Tail::Call(CallTarget::Direct { target, .. }) => {
                    assert_eq!(target.as_str(), "c0@lifted");
                }
                other => panic!("expected direct call in {func_name}, got {other:?}"),
            }
        }
    }

    /// A rec closure `c{s, e}(x)` whose body tail-calls its self-capture field
    /// `s` indirectly — the shape the lowerer emits for `rec go(...)`.
    fn self_calling_clsr() -> Clsr {
        Clsr {
            fields: vec![v("s").into(), v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("b0"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: indirect("s", vec![v("x")]),
            },
        }
    }

    #[test]
    fn devirtualizes_recursive_self_call_through_the_self_capture() {
        // main allocates `rec = c{rec, n}` — the self-capture at field 0. (The
        // prealloc shell is irrelevant to the scan, which keys on the fill.)
        // Inside c's body the field `s` is therefore a known closure of family
        // c with captures [s, e], so its indirect self-call devirtualizes.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            main_with(
                vec![
                    (v("n"), Value::Pure(Data::Nat(1))),
                    (v("k"), Value::Pure(Data::Nat(2))),
                    (
                        v("rec"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("rec"), v("n")])),
                    ),
                ],
                indirect("rec", vec![v("k")]),
            ),
        );
        module.add_clsr(ClsrName::from("c"), self_calling_clsr());

        lift_closures(&mut module);

        // main's call devirtualizes through the ordinary known-binding path.
        match main_tail(&module) {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "c@lifted");
                assert_eq!(params, &vec![v("rec"), v("n"), v("k")]);
            }
            other => panic!("expected direct call in main, got {other:?}"),
        }

        // The lifted function's body carries a *direct self-call* with the
        // fields threaded through — the recursion is no longer behind the
        // closure value. (Lifting clones the body after devirtualization.)
        let lifted = func_named(&module, "c@lifted").expect("lifted func");
        assert_eq!(lifted.params, vec![v("s"), v("e"), v("x")]);
        match &lifted.region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "c@lifted");
                assert_eq!(params, &vec![v("s"), v("e"), v("x")]);
            }
            other => panic!("expected direct self-call in lifted body, got {other:?}"),
        }
    }

    #[test]
    fn leaves_self_call_alone_when_an_allocation_lacks_the_self_capture() {
        // A second allocation of c binds something else at field 0, so the
        // self-capture is not universal and the body rewrite would be unsound
        // for that instance.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            main_with(
                vec![
                    (v("n"), Value::Pure(Data::Nat(1))),
                    (
                        v("rec"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("rec"), v("n")])),
                    ),
                    (
                        v("other"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("n"), v("n")])),
                    ),
                ],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("b0"),
                    params: vec![v("other")],
                }),
            ),
        );
        module.add_clsr(ClsrName::from("c"), self_calling_clsr());

        lift_closures(&mut module);

        let clsr = module
            .clsrs()
            .iter()
            .find(|(name, _)| name.as_str() == "c")
            .map(|(_, clsr)| clsr)
            .expect("c present");
        assert!(
            matches!(clsr.region.tail, Tail::Call(CallTarget::Indirect { .. })),
            "the self-call must stay indirect without a universal self-capture"
        );
        assert!(func_named(&module, "c@lifted").is_none());
    }

    #[test]
    fn leaves_self_call_alone_when_self_capture_indices_disagree() {
        // Both allocations self-capture, but at different field positions, so
        // no single index is universal.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            main_with(
                vec![
                    (v("n"), Value::Pure(Data::Nat(1))),
                    (
                        v("rec1"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("rec1"), v("n")])),
                    ),
                    (
                        v("rec2"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("n"), v("rec2")])),
                    ),
                ],
                Tail::Jump(JumpTarget {
                    target: BlockName::from("b0"),
                    params: vec![v("rec2")],
                }),
            ),
        );
        module.add_clsr(ClsrName::from("c"), self_calling_clsr());

        lift_closures(&mut module);

        let clsr = module
            .clsrs()
            .iter()
            .find(|(name, _)| name.as_str() == "c")
            .map(|(_, clsr)| clsr)
            .expect("c present");
        assert!(
            matches!(clsr.region.tail, Tail::Call(CallTarget::Indirect { .. })),
            "disagreeing self-capture indices must disqualify the seed"
        );
        assert!(func_named(&module, "c@lifted").is_none());
    }

    #[test]
    fn leaves_unknown_callee_as_indirect() {
        // The callee is not bound to a `Data::Clsr` in this body, so it escapes
        // analysis and is left untouched.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![v("g").into()],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: indirect("g", vec![]),
                },
            },
        );

        lift_closures(&mut module);

        assert!(matches!(
            main_tail(&module),
            Tail::Call(CallTarget::Indirect { .. })
        ));
        assert!(func_named(&module, "g@lifted").is_none());
        assert_eq!(module.funcs().len(), 1);
    }
}
