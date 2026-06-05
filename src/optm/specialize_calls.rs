use {
    super::*,
    std::collections::HashMap,
};

/// Closure specialization — monomorphization on first-class-function arguments.
///
/// Type-directed erasure flagged every parameter whose pre-erasure type was a
/// function, a `Type`, or unit as a specialization *candidate* (`Argument::candidate`)
/// — each a compile-time-constant shape. This pass uses that flag to clone a function
/// per distinct shape passed into a candidate position, baking the shape into the
/// clone so the abstract parameter becomes a statically-known value: a known closure
/// (captures threaded through), or unit (the erasure of a `Type`/unit argument,
/// baked as the constant `{}`). The closure case below is the motivating one; unit
/// is the same machinery with nothing to thread.
///
/// ```text
///   Clsr c { fields:[e], params:[x], .. }       // captures one env value
///   Func f { params:[*p, n], .. }               //  *p is a candidate
///       ... p(n) ...  (indirect, through p)
///
///   let clo = c{env};                           // p's argument is a known closure
///   f(clo, k)   (direct)
/// ```
/// becomes
/// ```text
///   Func f@spec__0_c { params:[e, n], .. }      // candidate param dropped...
///       let p = c{e};                           //  ...rebuilt from a threaded capture,
///       ... p(n) ...                            //     now a KNOWN closure.
///   f@spec__0_c(env, k)   (direct)
/// ```
///
/// The pass does not devirtualize `p(n)` itself: it only makes `p` a known
/// `Data::Clsr`, which the later [`lift_closures`] pass then turns into a direct
/// call. Specialization's whole job is to feed closure-lifting the known closures
/// it cannot otherwise see — the cases [`inline_calls`] gives up on, namely
/// multi-call-site and recursive higher-order combinators (`map`, `fold`).
///
/// # Threading captures instead of baking them
///
/// A closure's captured environment lives in the *caller's* scope, so it cannot be
/// baked into the callee as a constant. Only the closure's *identity* `c{·}` is
/// baked; the captures are threaded through as ordinary (non-candidate) leading
/// parameters in place of the dropped candidate one. Within a single clone the
/// `ClsrName` is fixed, so its capture arity is fixed — call sites that pass the
/// same closure *shape* but different captured values share the clone. That is
/// monomorphization keyed on shape, and it is what makes `p` known inside the
/// clone without its captures having to be compile-time constants.
///
/// # Recursion ties its own knot
///
/// A recursive combinator passes its own candidate parameter back to itself. In
/// the original body that parameter is abstract, so its self-call does not
/// specialize. In a clone the parameter is rebound to a known closure, so the
/// *clone's* self-call now sees a known closure in that position and specializes
/// on the next round — to a name that encodes the same shape key, which already
/// exists, so the self-call is simply retargeted to the clone. The clone thus
/// recurses into itself with the closure fully devirtualized, with no special
/// handling of self-calls: the fixed-point loop plus the name-as-key memo do it.
///
/// # Termination
///
/// Each round either retargets a call to a more-specialized callee or mints a new
/// clone, and a clone's name is a pure function of `(base, shape key)`, so a given
/// specialization is built at most once. The statically-known closures in any one
/// body form a finite acyclic set (a name's binding precedes its uses), so the
/// reachable family of clones is finite and the loop converges.
pub fn specialize_calls(module: &mut Module) {
    loop {
        let candidates = candidate_positions(module);
        let fields = closure_fields(module);
        let bodies = func_bodies(module);

        let mut needed: HashMap<FuncName, SpecPlan> = HashMap::new();
        let mut changed = false;

        for (_, func) in module.funcs_mut() {
            changed |= specialize_body(&mut func.region, &candidates, &mut needed);
        }
        for (_, clsr) in module.clsrs_mut() {
            changed |= specialize_body(&mut clsr.region, &candidates, &mut needed);
        }

        for (name, plan) in needed {
            if module.funcs().iter().any(|(present, _)| present == &name) {
                continue;
            }

            let base = bodies.get(&plan.base).expect("base function present");
            module.add_func(name, build_specialized(base, &plan, &fields));
            changed = true;
        }

        if !changed {
            return;
        }
    }
}

/// The compile-time value a candidate position resolved to — the two shapes erasure
/// leaves at a candidate parameter:
///
/// - a known closure, baked by identity with its captures threaded through;
/// - unit (`{}`), the erasure of a `Type` or unit argument, baked as a constant.
#[derive(Clone, PartialEq)]
enum Shape {
    Clsr(ClsrName),
    Unit,
}

impl std::fmt::Display for Shape {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Shape::Clsr(clsr) => write!(formatter, "{clsr}"),
            Shape::Unit => write!(formatter, "unit"),
        }
    }
}

/// The plan for one specialized clone: which base function it comes from, and the
/// shape each candidate position resolved to (in ascending position order).
struct SpecPlan {
    base: FuncName,
    resolved: Vec<(usize, Shape)>,
}

// --- Read-only snapshots ----------------------------------------------------

/// The candidate parameter positions of every function — the only positions worth
/// keying a specialization on.
fn candidate_positions(module: &Module) -> HashMap<FuncName, Vec<usize>> {
    module
        .funcs()
        .iter()
        .map(|(name, func)| {
            let positions = func
                .params
                .iter()
                .enumerate()
                .filter(|(_, arg)| arg.candidate)
                .map(|(index, _)| index)
                .collect();

            (name.clone(), positions)
        })
        .collect()
}

/// Every closure's captured fields, for building the threaded capture parameters
/// and the rebinding closure shape of each clone.
fn closure_fields(module: &Module) -> HashMap<ClsrName, Vec<Argument>> {
    module
        .clsrs()
        .iter()
        .map(|(name, clsr)| (name.clone(), clsr.fields.clone()))
        .collect()
}

/// A snapshot of every function body, so clones can be built from a base while the
/// module is borrowed mutably for the rewrite.
fn func_bodies(module: &Module) -> HashMap<FuncName, Func> {
    module
        .funcs()
        .iter()
        .map(|(name, func)| (name.clone(), func.clone()))
        .collect()
}

// --- Site rewriting ---------------------------------------------------------

/// The statically-known value a name is bound to. A closure carries the captures to
/// thread at the call site; unit carries nothing.
enum KnownValue {
    Clsr(ClsrName, Vec<ValueName>),
    Unit,
}

/// Maps a value name to the compile-time value it is statically bound to.
type Known = HashMap<ValueName, KnownValue>;

fn specialize_body(
    region: &mut Region,
    candidates: &HashMap<FuncName, Vec<usize>>,
    needed: &mut HashMap<FuncName, SpecPlan>,
) -> bool {
    let known = known_values(region);

    if known.is_empty() {
        return false;
    }

    rewrite_region(region, &known, candidates, needed)
}

/// Collect every binding to a bakeable compile-time value — a `let v = c{captures}`
/// closure or a `let v = {}` unit — in the tree. Names are unique within a body and
/// scoping is lexical, so a single tree-wide map is sound: a call can only name a
/// value that is actually in scope at the call.
fn known_values(region: &Region) -> Known {
    let mut known = Known::new();
    collect_known(region, &mut known);
    known
}

fn collect_known(region: &Region, known: &mut Known) {
    for (name, value) in &region.values {
        match value {
            Value::Pure(Data::Clsr(clsr, captures)) => {
                known.insert(name.clone(), KnownValue::Clsr(clsr.clone(), captures.clone()));
            }
            Value::Pure(Data::Tpl(fields)) if fields.is_empty() => {
                known.insert(name.clone(), KnownValue::Unit);
            }
            _ => {}
        }
    }

    for (_, block) in &region.blocks {
        collect_known(&block.region, known);
    }
}

fn rewrite_region(
    region: &mut Region,
    known: &Known,
    candidates: &HashMap<FuncName, Vec<usize>>,
    needed: &mut HashMap<FuncName, SpecPlan>,
) -> bool {
    let mut changed = rewrite_tail(&mut region.tail, known, candidates, needed);

    for (_, block) in &mut region.blocks {
        changed |= rewrite_region(&mut block.region, known, candidates, needed);
    }

    changed
}

/// Retarget a `Direct` call whose candidate positions carry known shapes to the
/// matching specialized clone, expanding each known-closure argument into its
/// captures and dropping each unit argument.
fn rewrite_tail(
    tail: &mut Tail,
    known: &Known,
    candidates: &HashMap<FuncName, Vec<usize>>,
    needed: &mut HashMap<FuncName, SpecPlan>,
) -> bool {
    let Tail::Call(CallTarget::Direct { target, params, resume }) = tail else {
        return false;
    };

    let Some(positions) = candidates.get(target) else {
        return false;
    };

    // Resolve the candidate positions that carry a statically-known shape, pairing
    // each with the arguments to splice in its place (a closure's captures, or
    // nothing for unit).
    let resolved: Vec<(usize, Shape, Vec<ValueName>)> = positions
        .iter()
        .filter_map(|&index| match known.get(&params[index])? {
            KnownValue::Clsr(clsr, captures) => {
                Some((index, Shape::Clsr(clsr.clone()), captures.clone()))
            }
            KnownValue::Unit => Some((index, Shape::Unit, vec![])),
        })
        .collect();

    if resolved.is_empty() {
        return false;
    }

    let key: Vec<(usize, Shape)> = resolved
        .iter()
        .map(|(index, shape, _)| (*index, shape.clone()))
        .collect();
    let name = specialized_name(target, &key);

    // Splice each resolved argument's replacement in place; keep every other
    // argument verbatim. Mirrors the clone's parameter construction.
    let expansions: HashMap<usize, &Vec<ValueName>> =
        resolved.iter().map(|(index, _, args)| (*index, args)).collect();

    let mut args = Vec::new();
    for (index, arg) in params.iter().enumerate() {
        match expansions.get(&index) {
            Some(spliced) => args.extend(spliced.iter().cloned()),
            None => args.push(arg.clone()),
        }
    }

    needed.entry(name.clone()).or_insert(SpecPlan {
        base: target.clone(),
        resolved: key,
    });

    *tail = Tail::Call(CallTarget::Direct {
        target: name,
        params: args,
        resume: resume.clone(),
    });

    true
}

/// A clone's name is a pure function of its base and shape key, so equal keys map
/// to one clone (the memo) and a self-call's key resolves back to its own clone.
fn specialized_name(base: &FuncName, resolved: &[(usize, Shape)]) -> FuncName {
    let mut name = format!("{base}@spec");

    for (position, shape) in resolved {
        name.push_str(&format!("__{position}_{shape}"));
    }

    FuncName::from(name)
}

// --- Clone construction -----------------------------------------------------

/// Build the specialized clone: drop each resolved candidate parameter and prepend
/// a binding that rebuilds its shape, so the formerly-abstract parameter is now a
/// statically-known value. A known closure threads its captures through as leading
/// non-candidate parameters; unit needs nothing threaded.
fn build_specialized(
    base: &Func,
    plan: &SpecPlan,
    fields: &HashMap<ClsrName, Vec<Argument>>,
) -> Func {
    let resolved: HashMap<usize, &Shape> =
        plan.resolved.iter().map(|(index, shape)| (*index, shape)).collect();

    let mut params = Vec::new();
    let mut rebinds = Vec::new();

    for (index, param) in base.params.iter().enumerate() {
        let rebind = match resolved.get(&index) {
            None => {
                params.push(param.clone());
                continue;
            }
            Some(Shape::Clsr(clsr)) => {
                let arity = fields.get(clsr).expect("specialized closure present").len();
                let captures: Vec<ValueName> =
                    (0..arity).map(|field| capture_param(&param.name, field)).collect();

                // The threaded captures are plain non-candidate parameters.
                params.extend(captures.iter().cloned().map(Argument::from));
                Value::Pure(Data::Clsr((*clsr).clone(), captures))
            }
            Some(Shape::Unit) => Value::Pure(Data::Tpl(vec![])),
        };

        rebinds.push((param.name.clone(), rebind));
    }

    let mut region = base.region.clone();
    rebinds.extend(std::mem::take(&mut region.values));
    region.values = rebinds;

    Func {
        params,
        resume: base.resume.clone(),
        region,
    }
}

fn capture_param(param: &ValueName, index: usize) -> ValueName {
    ValueName::from(format!("{param}@cap{index}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn candidate(name: &str) -> Argument {
        Argument {
            name: v(name),
            candidate: true,
        }
    }

    fn region(values: Vec<(ValueName, Value)>, tail: Tail) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks: vec![],
            tail,
        }
    }

    fn indirect(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Call(CallTarget::Indirect {
            target: v(target),
            params: args,
            resume: BlockName::from("r"),
        })
    }

    fn direct(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Call(CallTarget::Direct {
            target: FuncName::from(target),
            params: args,
            resume: BlockName::from("r"),
        })
    }

    fn func_named<'a>(module: &'a Module, name: &str) -> Option<&'a Func> {
        module
            .funcs()
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, func)| func)
    }

    #[test]
    fn specializes_known_closure_and_threads_its_captures() {
        // f(*p, n) = p(n); main passes a known closure c{env} for p.
        let f = Func {
            params: vec![candidate("p"), v("n").into()],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![v("n")])),
        };
        let c = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(vec![], Tail::Jump(JumpTarget {
                target: BlockName::from("r"),
                params: vec![v("x")],
            })),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("env"), Value::Pure(Data::Nat(7))),
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (v("clo"), Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("env")]))),
                ],
                direct("f", vec![v("clo"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // The call is retargeted to the clone, with `clo` expanded into `env`.
        match &func_named(&module, "main").unwrap().region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_c");
                assert_eq!(params, &vec![v("env"), v("k")]);
            }
            other => panic!("expected direct call to clone, got {other:?}"),
        }

        // The clone drops `p`, takes the threaded capture as a leading param, and
        // rebuilds the closure so `p` is a known `Data::Clsr`.
        let clone = func_named(&module, "f@spec__0_c").expect("clone present");
        assert_eq!(clone.params, vec![v("p@cap0"), v("n")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Clsr(c, caps)))
                if name == &v("p") && c.as_str() == "c" && caps == &vec![v("p@cap0")]
        ));
    }

    #[test]
    fn specializes_unit_argument_as_a_constant() {
        // f(*u, n) = n; main passes unit `{}` for the candidate `u`. The clone drops
        // `u` with nothing threaded and rebinds it to the unit constant.
        let f = Func {
            params: vec![candidate("u"), v("n").into()],
            resume: BlockName::from("r"),
            region: region(vec![], Tail::Jump(JumpTarget {
                target: BlockName::from("r"),
                params: vec![v("n")],
            })),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (v("unit"), Value::Pure(Data::Tpl(vec![]))),
                ],
                direct("f", vec![v("unit"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        specialize_calls(&mut module);

        // The call drops the unit argument entirely.
        match &func_named(&module, "main").unwrap().region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_unit");
                assert_eq!(params, &vec![v("k")]);
            }
            other => panic!("expected direct call to clone, got {other:?}"),
        }

        // The clone takes only `n`, and rebinds `u` to the unit constant.
        let clone = func_named(&module, "f@spec__0_unit").expect("clone present");
        assert_eq!(clone.params, vec![v("n")]);
        assert!(matches!(
            &clone.region.values[0],
            (name, Value::Pure(Data::Tpl(fields))) if name == &v("u") && fields.is_empty()
        ));
    }

    #[test]
    fn leaves_unknown_argument_alone() {
        // main forwards its own abstract parameter `g`, not a known closure.
        let f = Func {
            params: vec![candidate("p")],
            resume: BlockName::from("r"),
            region: region(vec![], indirect("p", vec![])),
        };
        let main = Func {
            params: vec![candidate("g")],
            resume: BlockName::from("r"),
            region: region(vec![], direct("f", vec![v("g")])),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);

        specialize_calls(&mut module);

        assert!(func_named(&module, "f@spec__0_c").is_none());
        assert_eq!(module.funcs().len(), 2);
        assert!(matches!(
            &func_named(&module, "main").unwrap().region.tail,
            Tail::Call(CallTarget::Direct { target, .. }) if target.as_str() == "f",
        ));
    }

    #[test]
    fn specializes_recursive_self_call_through_the_clone() {
        // f(*p, n) calls itself with p; specializing on c must devirtualize the
        // recursion too — the clone's self-call retargets to the clone.
        let body = Block {
            params: vec![],
            region: region(vec![], direct("f", vec![v("p"), v("n")])),
        };
        let f = Func {
            params: vec![candidate("p"), v("n").into()],
            resume: BlockName::from("r"),
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(BlockName::from("loop"), body)],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("loop"),
                    params: vec![],
                }),
            },
        };
        let c = Clsr {
            fields: vec![v("e").into()],
            params: vec![v("x").into()],
            resume: BlockName::from("r"),
            region: region(vec![], Tail::Jump(JumpTarget {
                target: BlockName::from("r"),
                params: vec![v("x")],
            })),
        };
        let main = Func {
            params: vec![],
            resume: BlockName::from("r"),
            region: region(
                vec![
                    (v("env"), Value::Pure(Data::Nat(0))),
                    (v("k"), Value::Pure(Data::Nat(1))),
                    (v("clo"), Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("env")]))),
                ],
                direct("f", vec![v("clo"), v("k")]),
            ),
        };

        let mut module = Module::new();
        module.add_func(FuncName::from("main"), main);
        module.add_func(FuncName::from("f"), f);
        module.add_clsr(ClsrName::from("c"), c);

        specialize_calls(&mut module);

        // The clone's own recursive call targets the clone, with `p` expanded into
        // the threaded capture rather than passed as a closure.
        let clone = func_named(&module, "f@spec__0_c").expect("clone present");
        match &clone.region.blocks[0].1.region.tail {
            Tail::Call(CallTarget::Direct { target, params, .. }) => {
                assert_eq!(target.as_str(), "f@spec__0_c");
                assert_eq!(params, &vec![v("p@cap0"), v("n")]);
            }
            other => panic!("expected specialized self-call, got {other:?}"),
        }
    }
}
