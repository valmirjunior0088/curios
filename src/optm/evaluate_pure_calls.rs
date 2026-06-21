//! CPS-level partial evaluator for pure-callee call sites.
//!
//! `constant_folding` reduces `Value::Eval(code)` against literals in the *same*
//! region; `inline_calls` is single-call-site, so it cannot reach a recursive
//! callee — which is what the parser combinator in `examples/crs_printf.rs`
//! collapses to after early DCE and inlining. This pass closes that gap: when a
//! `Direct` call's target is **pure** (no `Io`/`*ToStr`, no `Indirect` calls, no
//! impure callees, transitively) and every argument is a literal, the call is
//! interpreted at compile time and replaced by the materialised result plus a
//! `Jump` to the original resume.
//!
//! The structure mirrors the spec in OPTM.md §2, split across three modules:
//!
//! 1. The **purity classifier** ([`purity`](super::purity)) computes the
//!    fixed-point set of pure funcs/clsrs.
//! 2. The **interpreter** ([`interp`](super::interp)) runs a pure body against
//!    a frame of `Snapshot` values, sharing leaf semantics with
//!    `constant_folding` via `scalar_eval` so the trap and host-boundary set is
//!    identical between the two passes.
//! 3. The **rewriter** (this module) finds eligible `Direct`/`Indirect` call
//!    sites in every region tree, runs the interpreter, and materialises the
//!    result back into the host region as fresh `Pure(Data)` bindings plus a
//!    `Jump` to the original resume continuation.

use {
    super::*,
    crate::Entropy,
    std::{
        collections::{HashMap, HashSet},
        rc::Rc,
    },
};

/// Rewrite every `Direct` call to a pure target with literal arguments into the
/// materialised result of compile-time interpretation, plus a `Jump` to the
/// original resume continuation. Indirect calls through a known pure-closure
/// value are handled symmetrically.
pub fn evaluate_pure_calls(module: &mut Module) {
    let (pure_funcs, pure_clsrs) = purity(module);

    // Snapshot the callable population so the interpreter can keep reading bodies
    // while the rewriter mutates host tails in place. Cheap (one `clone` per
    // func/clsr) compared to the alternative of plumbing a separate index across
    // every mutable borrow on the rewrite path.
    let funcs: HashMap<FuncName, Func> = module
        .funcs()
        .iter()
        .map(|(n, f)| (n.clone(), f.clone()))
        .collect();
    let clsrs: HashMap<ClsrName, Clsr> = module
        .clsrs()
        .iter()
        .map(|(n, c)| (n.clone(), c.clone()))
        .collect();

    let ctx = Ctx {
        funcs: &funcs,
        clsrs: &clsrs,
        pure_funcs: &pure_funcs,
        pure_clsrs: &pure_clsrs,
    };

    // One counter for the whole pass: materialised result names must be unique
    // module-wide, because two rewritten call sites can share a function body and
    // every downstream pass keys its flat per-body maps by value name.
    let counter = Entropy::<usize>::new();

    for (_, func) in module.funcs_mut() {
        rewrite_region(&mut func.region, &ctx, &counter);
    }
    for (_, clsr) in module.clsrs_mut() {
        rewrite_region(&mut clsr.region, &ctx, &counter);
    }
}

// --- Rewriter ---------------------------------------------------------------

/// Walk `region` and every nested block, attempting to rewrite the tail of each
/// one whose tail is a literal-argument call to a pure target.
fn rewrite_region(region: &mut Region, ctx: &Ctx<'_>, counter: &Entropy) {
    if let Some(rewrite) = try_evaluate_tail(region, ctx, counter) {
        region.values.extend(rewrite.new_values);
        region.tail = rewrite.new_tail;
    }
    for (_, block) in &mut region.blocks {
        rewrite_region(&mut block.region, ctx, counter);
    }
}

/// The replacement for a rewritten tail: any number of fresh `Pure(_)` bindings
/// (the materialised result, deepest-first) plus the new `Jump` tail.
struct Rewrite {
    new_values: Vec<(ValueName, Value)>,
    new_tail: Tail,
}

/// If the host region's tail is a Direct/Indirect call whose target is pure
/// and every argument is a known literal, interpret the call to a single
/// `Snapshot` and materialise that as a list of fresh bindings plus a jump to
/// the original resume.
fn try_evaluate_tail(region: &Region, ctx: &Ctx<'_>, counter: &Entropy) -> Option<Rewrite> {
    let lits = literals(region);

    let (callee_outcome, resume) = match &region.tail {
        Tail::Call(CallTarget::Direct {
            target,
            params,
            resume,
        }) => {
            if !ctx.pure_funcs.contains(target) {
                return None;
            }
            let callee = ctx.funcs.get(target)?;
            let arg_data = collect_literal_args(params, &lits)?;
            let snaps = arg_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            let callee_frame = seed_frame(&callee.params, snaps)?;
            let mut interp = Interp::new(ctx);
            (
                interp.run_body(&callee.region, &callee.resume, callee_frame),
                resume.clone(),
            )
        }
        Tail::Call(CallTarget::Indirect {
            target,
            params,
            resume,
        }) => {
            // The closure value must itself be a literal in this region.
            let Data::Clsr(clsr_name, capture_names) = lits.get(target)? else {
                return None;
            };
            if !ctx.pure_clsrs.contains(clsr_name) {
                return None;
            }
            let callee = ctx.clsrs.get(clsr_name)?;
            let cap_data = collect_literal_args(capture_names, &lits)?;
            let arg_data = collect_literal_args(params, &lits)?;
            let captures = cap_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            let args = arg_data
                .into_iter()
                .map(|d| materialise_data(&d, &Frame::new()))
                .collect::<Option<Vec<_>>>()?;
            if callee.fields.len() != captures.len() || callee.params.len() != args.len() {
                return None;
            }
            let mut callee_frame: Frame = HashMap::new();
            for (field, cap) in callee.fields.iter().zip(captures) {
                callee_frame.insert(field.name.clone(), cap);
            }
            for (param, arg) in callee.params.iter().zip(args) {
                callee_frame.insert(param.name.clone(), arg);
            }
            let mut interp = Interp::new(ctx);
            (
                interp.run_body(&callee.region, &callee.resume, callee_frame),
                resume.clone(),
            )
        }
        _ => return None,
    };

    let Outcome::Returned(snap) = callee_outcome else {
        return None;
    };

    // Materialise the result snapshot. Fresh names use the pass-wide counter
    // suffixed `@eval#N`: the suffix keeps them clear of the host region's `vN`,
    // and the shared counter keeps two rewrites in one body from colliding.
    let mut new_values = Vec::<(ValueName, Value)>::new();
    let mut visited = HashSet::<*const ()>::new();
    let top = materialise_snapshot(&snap, counter, &mut new_values, &mut visited)?;

    Some(Rewrite {
        new_values,
        new_tail: Tail::Jump(JumpTarget {
            target: resume,
            params: vec![top],
        }),
    })
}

/// Collect the literal `Data` behind each name in `params`. Returns `None` if
/// any name is not a literal (the call isn't a candidate).
fn collect_literal_args(params: &[ValueName], lits: &Lits) -> Option<Vec<Data>> {
    params.iter().map(|p| lits.get(p).cloned()).collect()
}

/// Materialise a `Snapshot` into a chain of fresh `Pure(_)` bindings, emitted
/// deepest-first so each element is named before any aggregate that references
/// it. Returns the top-level binding's name (the value the new `Jump` carries).
///
/// Cycles abort: if the same aggregate `Rc` is visited twice on one path, no
/// finite IR representation exists, so the rewrite is discarded.
fn materialise_snapshot(
    snap: &Snapshot,
    counter: &Entropy,
    out: &mut Vec<(ValueName, Value)>,
    visited: &mut HashSet<*const ()>,
) -> Option<ValueName> {
    let data = match snap {
        Snapshot::Nat(n) => Data::Nat(*n),
        Snapshot::Int(i) => Data::Int(*i),
        Snapshot::Flt(f) => Data::Flt(*f),
        Snapshot::Bin(bytes) => Data::Bin((**bytes).clone()),
        Snapshot::Arr(elems) => {
            let key = Rc::as_ptr(elems) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let names: Option<Vec<ValueName>> = elems
                .iter()
                .map(|e| materialise_snapshot(e, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Arr(names?)
        }
        Snapshot::Tpl(elems) => {
            let key = Rc::as_ptr(elems) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let names: Option<Vec<ValueName>> = elems
                .iter()
                .map(|e| materialise_snapshot(e, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Tpl(names?)
        }
        Snapshot::Clsr(c, captures) => {
            let key = Rc::as_ptr(captures) as *const ();
            if !visited.insert(key) {
                return None;
            }
            let cap_snaps = captures.borrow();
            let names: Option<Vec<ValueName>> = cap_snaps
                .iter()
                .map(|cap| materialise_snapshot(cap, counter, out, visited))
                .collect();
            visited.remove(&key);
            Data::Clsr(c.clone(), names?)
        }
    };

    let name = mangle::eval_result(counter.fresh());
    out.push((name.clone(), Value::Pure(data)));
    Some(name)
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

    fn module_with_main_calling(callee: &str, args: Vec<(ValueName, Data)>) -> Module {
        let arg_names: Vec<ValueName> = args.iter().map(|(n, _)| n.clone()).collect();
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: args
                        .into_iter()
                        .map(|(name, data)| (name, Value::Pure(data)))
                        .collect(),
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rm", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from(callee),
                        params: arg_names,
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module
    }

    fn main_region(module: &Module) -> &Region {
        &module
            .funcs()
            .iter()
            .find(|(n, _)| n == &FuncName::from("main"))
            .expect("main present")
            .1
            .region
    }

    fn find_pure(values: &[(ValueName, Value)], data: &Data) -> bool {
        values.iter().any(|(_, v)| match (v, data) {
            (Value::Pure(Data::Nat(a)), Data::Nat(b)) => a == b,
            (Value::Pure(Data::Int(a)), Data::Int(b)) => a == b,
            (Value::Pure(Data::Tpl(a)), Data::Tpl(b)) => a.len() == b.len(),
            _ => false,
        })
    }

    #[test]
    fn evaluates_pure_direct_call_with_literal_arg() {
        // f(n) = n + 1; main calls f(41) → 42, residual is Pure(Nat(42)) + Jump(cont).
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(41))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("one"), Value::Pure(Data::Nat(1))),
                        (v("sum"), Value::Eval(Code::NatAdd(v("n"), v("one")))),
                    ],
                    jump("rf", vec![v("sum")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        assert!(
            find_pure(&main.values, &Data::Nat(42)),
            "expected Pure(Nat(42)) in main, got {:?}",
            main.values,
        );
        match &main.tail {
            Tail::Jump(target) => {
                assert_eq!(target.target, b("cont"));
                assert_eq!(target.params.len(), 1);
            }
            other => panic!("expected Jump to cont, got {other:?}"),
        }
    }

    #[test]
    fn evaluates_recursive_direct_call() {
        // sum(n) = if n == 0 then 0 else n + sum(n - 1); main calls sum(3) → 6.
        let sum_region = Region {
            preallocs: vec![],
            values: vec![(v("zero"), Value::Pure(Data::Nat(0)))],
            blocks: vec![
                (
                    b("rec"),
                    Block {
                        params: vec![],
                        region: region(
                            vec![
                                (v("one"), Value::Pure(Data::Nat(1))),
                                (v("m"), Value::Eval(Code::NatSub(v("n"), v("one")))),
                            ],
                            Tail::Call(CallTarget::Direct {
                                target: FuncName::from("sum"),
                                params: vec![v("m")],
                                resume: b("k"),
                            }),
                        ),
                    },
                ),
                (
                    b("k"),
                    Block {
                        params: vec![v("rec_r")],
                        region: region(
                            vec![(v("total"), Value::Eval(Code::NatAdd(v("n"), v("rec_r"))))],
                            jump("rsum", vec![v("total")]),
                        ),
                    },
                ),
            ],
            tail: Tail::Match(MatchTarget {
                operand: v("n"),
                cases: [(
                    0u32,
                    JumpTarget {
                        target: b("rsum"),
                        params: vec![v("zero")],
                    },
                )]
                .into_iter()
                .collect(),
                default: Some(JumpTarget {
                    target: b("rec"),
                    params: vec![],
                }),
            }),
        };
        let mut module = module_with_main_calling("sum", vec![(v("three"), Data::Nat(3))]);
        module.add_func(
            FuncName::from("sum"),
            Func {
                params: vec![v("n").into()],
                resume: b("rsum"),
                region: sum_region,
            },
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        assert!(
            find_pure(&main.values, &Data::Nat(6)),
            "expected Pure(Nat(6)) in main, got {:?}",
            main.values,
        );
        assert!(
            matches!(&main.tail, Tail::Jump(_)),
            "expected Jump in main, got {:?}",
            main.tail,
        );
    }

    #[test]
    fn evaluates_aggregate_construction_into_tpl() {
        // pair(a, b) = (a, b); main calls pair(3, 4). Residual: a Tpl with two
        // scalar literals plus a Jump.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![
                        (v("three"), Value::Pure(Data::Nat(3))),
                        (v("four"), Value::Pure(Data::Nat(4))),
                    ],
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rm", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("pair"),
                        params: vec![v("three"), v("four")],
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module.add_func(
            FuncName::from("pair"),
            func(
                vec![v("a"), v("b")],
                "rp",
                region(
                    vec![(v("t"), Value::Pure(Data::Tpl(vec![v("a"), v("b")])))],
                    jump("rp", vec![v("t")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        // The materialised tuple has length 2.
        assert!(
            find_pure(&main.values, &Data::Tpl(vec![v("_a"), v("_b")])),
            "expected materialised Tpl(2) in main, got {:?}",
            main.values,
        );
        // Two fresh scalar bindings (3 and 4) for the tuple's elements.
        assert!(find_pure(&main.values, &Data::Nat(3)));
        assert!(find_pure(&main.values, &Data::Nat(4)));
        assert!(matches!(&main.tail, Tail::Jump(_)));
    }

    #[test]
    fn evaluates_projection_of_a_non_scalar_element() {
        // f(a, b) nests a tuple inside a tuple, projects the *aggregate* element
        // back out, then projects a scalar from it. The frame-as-EvalEnv evaluator
        // forwards the inner tuple as a snapshot; the old Lits projection of the
        // frame could only forward scalars and gave up here.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![
                        (v("x"), Value::Pure(Data::Nat(20))),
                        (v("y"), Value::Pure(Data::Nat(22))),
                    ],
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rm", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("f"),
                        params: vec![v("x"), v("y")],
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("a"), v("b")],
                "rf",
                region(
                    vec![
                        (v("inner"), Value::Pure(Data::Tpl(vec![v("a"), v("b")]))),
                        (v("outer"), Value::Pure(Data::Tpl(vec![v("inner")]))),
                        (v("proj"), Value::Eval(Code::TplGet(v("outer"), 0))),
                        (v("first"), Value::Eval(Code::TplGet(v("proj"), 0))),
                        (v("second"), Value::Eval(Code::TplGet(v("proj"), 1))),
                        (v("sum"), Value::Eval(Code::NatAdd(v("first"), v("second")))),
                    ],
                    jump("rf", vec![v("sum")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let main = main_region(&module);
        assert!(
            find_pure(&main.values, &Data::Nat(42)),
            "expected Pure(Nat(42)) in main, got {:?}",
            main.values,
        );
        assert!(matches!(&main.tail, Tail::Jump(_)));
    }

    #[test]
    fn leaves_impure_call_intact() {
        // f does IoPrint; calls to it must not be folded — the trap/effect
        // observability would be lost.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(1))]);
        module.add_func(
            FuncName::from("f"),
            func(vec![v("n")], "rf", region(vec![], io_write(v("n"), "rf"))),
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn leaves_budget_overrun_intact() {
        // f(n) = f(n) — pure but non-terminating. The budget is consumed and
        // the original call survives.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(1))]);
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![v("n").into()],
                resume: b("rf"),
                region: Region {
                    preallocs: vec![],
                    values: vec![],
                    blocks: vec![(
                        b("k"),
                        Block {
                            params: vec![v("res")],
                            region: region(vec![], jump("rf", vec![v("res")])),
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("f"),
                        params: vec![v("n")],
                        resume: b("k"),
                    }),
                },
            },
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn leaves_trap_call_intact() {
        // f(n) = n / 0 — pure but traps. `scalar_eval::eval` returns None on
        // the divide-by-zero, the interpreter aborts, the original call stays.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Nat(5))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("zero"), Value::Pure(Data::Nat(0))),
                        (v("q"), Value::Eval(Code::NatDiv(v("n"), v("zero")))),
                    ],
                    jump("rf", vec![v("q")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        match &main_region(&module).tail {
            Tail::Call(CallTarget::Direct { target, .. }) => {
                assert_eq!(target, &FuncName::from("f"));
            }
            other => panic!("expected the Direct call to survive, got {other:?}"),
        }
    }

    #[test]
    fn folds_conversion_call_at_compile_time() {
        // f(n) = Flt::to_le_bin(n) — pure under the classification (see
        // `pure_conversion_code_is_pure`). With a literal argument, partial eval
        // folds the call to a `Pure(Data::Bin(..))` plus a jump to the original
        // resume; the runtime conversion disappears.
        let mut module = module_with_main_calling("f", vec![(v("a"), Data::Flt(7.0))]);
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![(v("s"), Value::Eval(Code::FltToLeBin(v("n"))))],
                    jump("rf", vec![v("s")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        let region = main_region(&module);
        assert!(
            matches!(&region.tail, Tail::Jump(_)),
            "expected the Direct call to fold to a Jump, got {:?}",
            region.tail,
        );
        let expected = 7.0f32.to_le_bytes();
        assert!(
            region
                .values
                .iter()
                .any(|(_, v)| matches!(v, Value::Pure(Data::Bin(b)) if b.as_slice() == expected.as_slice())),
            "expected a Pure(Bin) binding from the folded FltToLeBin, got {:?}",
            region.values,
        );
    }

    #[test]
    fn two_rewrites_in_one_body_mint_distinct_names() {
        // main's root tail calls f(41), and the continuation block's tail calls
        // f(41) again. Both sites are rewritten in one pass; the materialised
        // bindings land in two regions of the *same* body, so their names must
        // differ — every downstream pass keys flat per-body maps by value name.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(v("a"), Value::Pure(Data::Nat(41)))],
                    blocks: vec![(
                        b("cont"),
                        Block {
                            params: vec![v("res")],
                            region: Region {
                                preallocs: vec![],
                                values: vec![(v("a2"), Value::Pure(Data::Nat(41)))],
                                blocks: vec![(
                                    b("cont2"),
                                    Block {
                                        params: vec![v("res2")],
                                        region: region(vec![], jump("rm", vec![v("res2")])),
                                    },
                                )],
                                tail: Tail::Call(CallTarget::Direct {
                                    target: FuncName::from("f"),
                                    params: vec![v("a2")],
                                    resume: b("cont2"),
                                }),
                            },
                        },
                    )],
                    tail: Tail::Call(CallTarget::Direct {
                        target: FuncName::from("f"),
                        params: vec![v("a")],
                        resume: b("cont"),
                    }),
                },
            },
        );
        module.set_entry(FuncName::from("main"));
        module.add_func(
            FuncName::from("f"),
            func(
                vec![v("n")],
                "rf",
                region(
                    vec![
                        (v("one"), Value::Pure(Data::Nat(1))),
                        (v("sum"), Value::Eval(Code::NatAdd(v("n"), v("one")))),
                    ],
                    jump("rf", vec![v("sum")]),
                ),
            ),
        );

        evaluate_pure_calls(&mut module);

        // Both call sites folded to jumps.
        let main = main_region(&module);
        assert!(matches!(&main.tail, Tail::Jump(_)));
        let (_, cont) = &main.blocks[0];
        assert!(matches!(&cont.region.tail, Tail::Jump(_)));

        // No value name is bound twice anywhere in the body.
        fn collect_names(region: &Region, names: &mut Vec<ValueName>) {
            names.extend(region.values.iter().map(|(n, _)| n.clone()));
            for (_, block) in &region.blocks {
                collect_names(&block.region, names);
            }
        }
        let mut names = Vec::new();
        collect_names(main, &mut names);
        let unique: HashSet<&ValueName> = names.iter().collect();
        assert_eq!(
            unique.len(),
            names.len(),
            "duplicate value names in one body: {names:?}",
        );
    }
}
