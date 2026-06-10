//! Purity classifier for the partial evaluator: the fixed-point set of
//! funcs/clsrs whose bodies have no host code, no `Indirect` call, no `Direct`
//! call to an impure target, and construct no impure closure — transitively.
//! [`evaluate_pure_calls`](super::evaluate_pure_calls) interprets only callees
//! this module classifies pure.

use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// What a single region tree reveals about its enclosing func or clsr.
///
/// The fields are split so the worklist below can read each in isolation:
/// `has_host_tail` and `has_indirect_call` are *direct* reasons (terminate the
/// classifier in one step), while `func_calls` and `clsr_refs` are *edges* the
/// fixed point propagates impurity along.
#[derive(Default)]
struct BodyScan {
    /// `FuncName`s reached via a `Direct` call. Propagation edge.
    func_calls: HashSet<FuncName>,
    /// `ClsrName`s constructed (`Pure(Data::Clsr)`) or preallocated. Propagation edge.
    clsr_refs: HashSet<ClsrName>,
    /// A `Tail::Call(Indirect)` was found — the static classifier cannot follow
    /// the call, so it conservatively pins the enclosing impure. The interpreter
    /// still resolves indirect calls dynamically when the target is a known
    /// `Pure(Data::Clsr)` and that closure itself is pure.
    has_indirect_call: bool,
    /// A `Tail::Host(_)` was found — the impure boundary lives at the tail
    /// position, so any region tree whose tails include a host primitive is
    /// classified impure. (`Code`-level ops are now all pure: arithmetic and
    /// conversions are deterministic and folded by `scalar_eval::eval`; trap
    /// conditions are operand-dependent and handled by it returning `None`.)
    has_host_tail: bool,
}

fn scan_body(region: &Region) -> BodyScan {
    let mut scan = BodyScan::default();
    scan_region(region, &mut scan);
    scan
}

fn scan_region(region: &Region, scan: &mut BodyScan) {
    for (_, prealloc) in &region.preallocs {
        if let Prealloc::Clsr(c) = prealloc {
            scan.clsr_refs.insert(c.clone());
        }
    }

    for (_, value) in &region.values {
        match value {
            Value::Pure(Data::Clsr(c, _)) => {
                scan.clsr_refs.insert(c.clone());
            }
            Value::Pure(_) | Value::Alias(_) | Value::Eval(_) => {}
        }
    }

    match &region.tail {
        Tail::Call(CallTarget::Direct { target, .. }) => {
            scan.func_calls.insert(target.clone());
        }
        Tail::Call(CallTarget::Indirect { .. }) => {
            scan.has_indirect_call = true;
        }
        Tail::Host(_) => {
            scan.has_host_tail = true;
        }
        Tail::Jump(_) | Tail::Match(_) | Tail::Unreachable => {}
    }

    for (_, block) in &region.blocks {
        scan_region(&block.region, scan);
    }
}

/// The pure `FuncName`s and `ClsrName`s — every body that has no host code, no
/// `Indirect` call, no `Direct` call to an impure target, and constructs no
/// impure closure (transitively). One [`classify`] fixed point serves both sets.
pub fn purity(module: &Module) -> (HashSet<FuncName>, HashSet<ClsrName>) {
    let (impure_funcs, impure_clsrs) = classify(module);

    let pure_funcs = module
        .funcs()
        .iter()
        .filter(|(name, _)| !impure_funcs.contains(name))
        .map(|(name, _)| name.clone())
        .collect();

    let pure_clsrs = module
        .clsrs()
        .iter()
        .filter(|(name, _)| !impure_clsrs.contains(name))
        .map(|(name, _)| name.clone())
        .collect();

    (pure_funcs, pure_clsrs)
}

/// Compute the impure sets jointly. Funcs and clsrs share one fixed point
/// because their impurity cross-propagates: a func calling an impure clsr
/// (after specialisation, every closure call site is `Indirect` → handled by
/// `has_indirect_call`) or *constructing* an impure clsr is itself impure, and
/// vice versa.
///
/// Mirrors the worklist pattern in `dead_code_elimination::dce_module` with
/// the sign flipped: start with the *directly* impure seeds (host code or
/// indirect call), then iterate, adding any item that calls an impure func or
/// references an impure clsr. Stabilises in O(funcs + clsrs) iterations.
fn classify(module: &Module) -> (HashSet<FuncName>, HashSet<ClsrName>) {
    let func_scans: HashMap<FuncName, BodyScan> = module
        .funcs()
        .iter()
        .map(|(name, func)| (name.clone(), scan_body(&func.region)))
        .collect();
    let clsr_scans: HashMap<ClsrName, BodyScan> = module
        .clsrs()
        .iter()
        .map(|(name, clsr)| (name.clone(), scan_body(&clsr.region)))
        .collect();

    let mut impure_funcs: HashSet<FuncName> = func_scans
        .iter()
        .filter(|(_, scan)| scan.has_host_tail || scan.has_indirect_call)
        .map(|(name, _)| name.clone())
        .collect();
    let mut impure_clsrs: HashSet<ClsrName> = clsr_scans
        .iter()
        .filter(|(_, scan)| scan.has_host_tail || scan.has_indirect_call)
        .map(|(name, _)| name.clone())
        .collect();

    loop {
        let mut changed = false;

        for (name, scan) in &func_scans {
            if impure_funcs.contains(name) {
                continue;
            }
            if propagates(scan, &impure_funcs, &impure_clsrs) {
                impure_funcs.insert(name.clone());
                changed = true;
            }
        }

        for (name, scan) in &clsr_scans {
            if impure_clsrs.contains(name) {
                continue;
            }
            if propagates(scan, &impure_funcs, &impure_clsrs) {
                impure_clsrs.insert(name.clone());
                changed = true;
            }
        }

        if !changed {
            return (impure_funcs, impure_clsrs);
        }
    }
}

/// Whether the scanned body's edges reach an already-impure func or clsr.
fn propagates(
    scan: &BodyScan,
    impure_funcs: &HashSet<FuncName>,
    impure_clsrs: &HashSet<ClsrName>,
) -> bool {
    scan.func_calls.iter().any(|t| impure_funcs.contains(t))
        || scan.clsr_refs.iter().any(|c| impure_clsrs.contains(c))
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

    fn jump(target: &str, params: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(target),
            params,
        })
    }

    fn io_write(bytes: ValueName, resume: &str) -> Tail {
        Tail::Host(HostTarget::IoWrite {
            handle: bytes.clone(),
            bytes,
            resume: b(resume),
        })
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
            resume: b(resume),
            region,
        }
    }

    fn clsr(fields: Vec<ValueName>, params: Vec<ValueName>, resume: &str, region: Region) -> Clsr {
        Clsr {
            fields: fields.into_iter().map(Into::into).collect(),
            params: params.into_iter().map(Into::into).collect(),
            resume: b(resume),
            region,
        }
    }

    #[test]
    fn purely_arithmetic_func_is_pure() {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                region(
                    vec![
                        (v("v0"), Value::Pure(Data::Nat(1))),
                        (v("v1"), Value::Eval(Code::NatAdd(v("p"), v("v0")))),
                    ],
                    jump("r", vec![v("v1")]),
                ),
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn host_tail_demotes_to_impure() {
        // A body whose tail is `Tail::Host(_)` is impure regardless of its
        // values. The `*ToStr` ops are no longer impure (see
        // `pure_to_str_code_is_pure`); `Io.print`/`Io.read` are the real
        // impurity boundary and they live at tail position.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(vec![v("p")], "r", region(vec![], io_write(v("p"), "r"))),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn pure_to_str_code_is_pure() {
        // `Code::NatToStr` is deterministic and folded by `scalar_eval`. A
        // body that only does conversions is now classified pure — the
        // interpreter materialises the converted string at compile time.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                region(
                    vec![(v("v0"), Value::Eval(Code::NatToStr(v("p"))))],
                    jump("r", vec![v("v0")]),
                ),
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn indirect_call_demotes_to_impure() {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("p")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Indirect {
                        target: v("p"),
                        params: vec![],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("f")));
    }

    #[test]
    fn impurity_propagates_through_direct_calls() {
        // `caller` calls `host`, which contains IoPrint; `caller` must also be impure.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("host"),
            func(vec![v("p")], "r", region(vec![], io_write(v("p"), "r"))),
        );
        module.add_func(
            FuncName::from("caller"),
            func(
                vec![v("p")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("host"),
                        params: vec![v("p")],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(!pure.contains(&FuncName::from("host")));
        assert!(!pure.contains(&FuncName::from("caller")));
    }

    #[test]
    fn impurity_propagates_through_closure_construction() {
        // A pure-looking func that *constructs* an impure closure is itself impure.
        let mut module = Module::new();
        module.add_clsr(
            ClsrName::from("c"),
            clsr(
                vec![],
                vec![v("p")],
                "r",
                region(vec![], io_write(v("p"), "r")),
            ),
        );
        module.add_func(
            FuncName::from("builder"),
            func(
                vec![],
                "r",
                region(
                    vec![(
                        v("v0"),
                        Value::Pure(Data::Clsr(ClsrName::from("c"), vec![])),
                    )],
                    jump("r", vec![v("v0")]),
                ),
            ),
        );

        let (pure_funcs, pure_clsrs) = purity(&module);
        assert!(!pure_clsrs.contains(&ClsrName::from("c")));
        assert!(!pure_funcs.contains(&FuncName::from("builder")));
    }

    #[test]
    fn mutually_recursive_pure_funcs_stay_pure() {
        // `even`/`odd` call each other only on the pure arithmetic path.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("even"),
            func(
                vec![v("n")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("odd"),
                        params: vec![v("n")],
                        resume: b("r"),
                    }),
                },
            ),
        );
        module.add_func(
            FuncName::from("odd"),
            func(
                vec![v("n")],
                "r",
                Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("even"),
                        params: vec![v("n")],
                        resume: b("r"),
                    }),
                },
            ),
        );

        let (pure, _) = purity(&module);
        assert!(pure.contains(&FuncName::from("even")));
        assert!(pure.contains(&FuncName::from("odd")));
    }
}
