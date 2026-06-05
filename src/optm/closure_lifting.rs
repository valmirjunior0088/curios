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
///   v(arg)  (indirect)  ->  c_lifted(a, b, arg)  (direct, to)
///                           Func c_lifted { params: [x, y, p], .. }
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
pub fn lift_closures(module: &mut Module) {
    let mut to_lift = HashSet::new();

    for (_, func) in module.funcs_mut() {
        devirtualize_tree(&mut func.region, &mut to_lift);
    }
    for (_, clsr) in module.clsrs_mut() {
        devirtualize_tree(&mut clsr.region, &mut to_lift);
    }

    // Lifted bodies are cloned *after* devirtualization, so they already carry
    // the rewritten call sites of the closure they came from.
    for (name, func) in lift_funcs(module, &to_lift) {
        module.add_func(name, func);
    }
}

/// The function a closure is lifted to.
fn lifted_name(clsr: &ClsrName) -> FuncName {
    FuncName::from(format!("{clsr}_lifted"))
}

// --- Devirtualization -------------------------------------------------------

/// Maps a value name to the closure it is statically bound to, with its captures.
type Known = HashMap<ValueName, (ClsrName, Vec<ValueName>)>;

fn devirtualize_tree(region: &mut Region, to_lift: &mut HashSet<ClsrName>) {
    let known = known_closures(region);

    if known.is_empty() {
        return;
    }

    rewrite_calls(region, &known, to_lift);
}

/// Collect every `let v = c{captures}` binding in the tree. Names are unique
/// within a body and scoping is lexical, so a single tree-wide map is sound: a
/// call can only name a closure that is actually in scope at the call.
fn known_closures(region: &Region) -> Known {
    let mut known = Known::new();
    collect_known(region, &mut known);
    known
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
                target: lifted_name(clsr),
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
        .filter(|(name, _)| to_lift.contains(name))
        .map(|(name, clsr)| {
            let mut params = clsr.fields.clone();
            params.extend(clsr.params.iter().cloned());

            let func = Func {
                params,
                resume: clsr.resume.clone(),
                region: clsr.region.clone(),
            };

            (lifted_name(name), func)
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
            fields,
            params,
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
                    (v("clo"), Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![v("a")]))),
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
                assert_eq!(target.as_str(), "c0_lifted");
                assert_eq!(params, &vec![v("a"), v("arg")]);
                assert_eq!(resume.as_str(), "b1");
            }
            other => panic!("expected direct call, got {other:?}"),
        }

        // The lifted function takes the captured fields as leading params.
        let lifted = func_named(&module, "c0_lifted").expect("lifted func");
        assert_eq!(lifted.params, vec![v("f"), v("p")]);
    }

    #[test]
    fn leaves_unknown_callee_as_indirect() {
        // The callee is not bound to a `Data::Clsr` in this body, so it escapes
        // analysis and is left untouched.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![v("g")],
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
        assert!(func_named(&module, "g_lifted").is_none());
        assert_eq!(module.funcs().len(), 1);
    }
}
