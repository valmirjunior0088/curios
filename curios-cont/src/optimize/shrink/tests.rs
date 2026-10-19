use {
    super::shrink,
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Intrinsic, Literal,
        Module, Node, NodeId, Row, RowId, Slot, ValueExpr, ValueId,
        test_support::unary_intrinsic_module,
    },
    curios_num::{Floating, Integer, Natural, Rounding},
    std::collections::BTreeMap,
};

fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

fn jump(module: &mut Module, target: ContinuationId, args: Vec<Atom>) -> NodeId {
    module.add_node(Node::ApplyCont(Edge { target, args }))
}

fn continuation(module: &mut Module, params: Vec<ValueId>, body: NodeId) -> ContinuationId {
    module.add_continuation(Continuation {
        debug_name: None,
        params,
        body,
    })
}

fn holding(module: &mut Module, continuations: Vec<ContinuationId>, body: NodeId) -> NodeId {
    module.add_node(Node::LetCont {
        continuations,
        body,
    })
}

/// `main` with `params`, returning to `return_cont`, as the entry.
fn entry(
    module: &mut Module,
    main: FunctionId,
    params: Vec<ValueId>,
    return_cont: ContinuationId,
    body: NodeId,
) {
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
}

fn switches(module: &Module) -> usize {
    module
        .live_nodes()
        .filter(|(_, node)| matches!(node, Node::Switch { .. }))
        .count()
}

#[test]
fn a_dead_trap_stays_where_a_dead_total_binding_goes() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let return_node = jump(&mut module, return_cont, vec![nat(0)]);
    let dead_total = module.add_value(Some("dead total".into()));
    let total_node = module.add_node(Node::LetIntrinsic {
        result: dead_total,
        op: Intrinsic::NatEql,
        args: vec![nat(1), nat(2)],
        next: return_node,
    });
    let dead_trap = module.add_value(Some("dead trap".into()));
    let trap_node = module.add_node(Node::LetIntrinsic {
        result: dead_trap,
        op: Intrinsic::NatDiv,
        args: vec![nat(1), nat(0)],
        next: total_node,
    });
    entry(&mut module, main, vec![], return_cont, trap_node);

    assert!(shrink(&mut module));
    assert!(module.node(total_node).is_none());
    assert!(matches!(
        module.node(trap_node),
        Some(Node::LetIntrinsic {
            op: Intrinsic::NatDiv,
            next,
            ..
        }) if *next == return_node
    ));
    module.verify().unwrap();
}

/// `main(a)`: `t1 = (a, 1); p = t1.0; t2 = (p, 2); q = t2.0; return q`. The return carries `a`, the origin of the chain, and both projections are gone.
#[test]
fn a_chain_of_projections_forwards_to_its_origin() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let t1 = module.add_value(Some("t1".into()));
    let p = module.add_value(Some("p".into()));
    let t2 = module.add_value(Some("t2".into()));
    let q = module.add_value(Some("q".into()));

    let deliver = jump(&mut module, main_return, vec![Atom::Value(q)]);
    let read_q = module.add_node(Node::LetIntrinsic {
        result: q,
        op: Intrinsic::TupleGet(0),
        args: vec![Atom::Value(t2)],
        next: deliver,
    });
    let build_t2 = module.add_node(Node::LetValue {
        result: t2,
        value: ValueExpr::Tuple(vec![Atom::Value(p), nat(2)]),
        next: read_q,
    });
    let read_p = module.add_node(Node::LetIntrinsic {
        result: p,
        op: Intrinsic::TupleGet(0),
        args: vec![Atom::Value(t1)],
        next: build_t2,
    });
    let build_t1 = module.add_node(Node::LetValue {
        result: t1,
        value: ValueExpr::Tuple(vec![Atom::Value(a), nat(1)]),
        next: read_p,
    });
    entry(&mut module, main, vec![a], main_return, build_t1);
    module.verify().unwrap();

    assert!(shrink(&mut module));
    module.verify().unwrap();
    assert!(
        matches!(
            module.node(deliver),
            Some(Node::ApplyCont(Edge { args, .. })) if args == &[Atom::Value(a)]
        ),
        "the return carries the origin of the chain:\n{module}"
    );
    assert!(
        module.node(read_p).is_none() && module.node(read_q).is_none(),
        "both projections are spliced out"
    );
    assert!(
        !shrink(&mut module),
        "and nothing is left for a second pass"
    );
}

#[test]
fn identity_folds_forward_the_surviving_operand() {
    let cases = [
        (Intrinsic::NatAdd, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatAdd, Literal::Nat(Natural::from(0u32)), false),
        (Intrinsic::NatSub, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatMul, Literal::Nat(Natural::from(1u32)), true),
        (Intrinsic::NatMul, Literal::Nat(Natural::from(1u32)), false),
        (Intrinsic::NatDiv, Literal::Nat(Natural::from(1u32)), true),
        (Intrinsic::NatOr, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::NatXor, Literal::Nat(Natural::from(0u32)), false),
        (Intrinsic::NatShl, Literal::Nat(Natural::from(0u32)), true),
        (Intrinsic::IntAdd, Literal::Int(Integer::from(0)), false),
        (Intrinsic::IntSub, Literal::Int(Integer::from(0)), true),
        (Intrinsic::IntMul, Literal::Int(Integer::from(1)), true),
        (Intrinsic::IntShr, Literal::Nat(Natural::from(0u32)), true),
    ];
    for (op, literal, literal_on_right) in cases {
        let x = ValueId(0);
        let args = if literal_on_right {
            vec![Atom::Value(x), Atom::Literal(literal.clone())]
        } else {
            vec![Atom::Literal(literal.clone()), Atom::Value(x)]
        };
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(shrink(&mut module), "{op:?} with {literal:?} must fold");
        assert!(module.node(intrinsic).is_none(), "{op:?} binding survives");
        let returns_x =
            module.nodes().iter().flatten().any(
                |node| matches!(node, Node::ApplyCont(edge) if edge.args == vec![Atom::Value(x)]),
            );
        assert!(returns_x, "{op:?} must forward the surviving operand");
        module.verify().unwrap();
    }
}

/// An absorbing law pins its result, which the binding hands its reader as the literal.
#[test]
fn identity_folds_hand_an_absorbing_result_to_its_reader() {
    let cases = [
        (
            Intrinsic::NatMul,
            Literal::Nat(Natural::from(0u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::NatAnd,
            Literal::Nat(Natural::from(0u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::NatRem,
            Literal::Nat(Natural::from(1u32)),
            Literal::Nat(Natural::from(0u32)),
        ),
        (
            Intrinsic::IntMul,
            Literal::Int(Integer::from(0)),
            Literal::Int(Integer::from(0)),
        ),
        (
            Intrinsic::IntRem,
            Literal::Int(Integer::from(1)),
            Literal::Int(Integer::from(0)),
        ),
    ];
    for (op, literal, expected) in cases {
        let x = ValueId(0);
        let args = vec![Atom::Value(x), Atom::Literal(literal.clone())];
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(shrink(&mut module), "{op:?} with {literal:?} must fold");
        assert!(module.node(intrinsic).is_none(), "{op:?} binding survives");
        let returns_expected = module.nodes().iter().flatten().any(
            |node| matches!(node, Node::ApplyCont(edge) if edge.args == vec![Atom::Literal(expected.clone())]),
        );
        assert!(returns_expected, "{op:?} must hand back {expected:?}");
        module.verify().unwrap();
    }
}

#[test]
fn identity_folds_leave_traps_and_flt_untouched() {
    let x = ValueId(0);
    let cases = [
        (Intrinsic::NatDiv, vec![Atom::Value(x), nat(0)]),
        (Intrinsic::NatAdd, vec![Atom::Value(x), nat(2)]),
        (
            Intrinsic::FltAdd(Rounding::TiesToEven),
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Flt(Floating::from(0.0))),
            ],
        ),
        (
            Intrinsic::FltMul(Rounding::TiesToEven),
            vec![
                Atom::Value(x),
                Atom::Literal(Literal::Flt(Floating::from(1.0))),
            ],
        ),
    ];
    for (op, args) in cases {
        let (mut module, intrinsic) = unary_intrinsic_module(op, args);

        assert!(!shrink(&mut module), "{op:?} must stay");
        assert!(
            matches!(module.node(intrinsic), Some(Node::LetIntrinsic { .. })),
            "{op:?} binding must survive"
        );
    }
}

#[test]
fn continuation_beta_rewrites_parameters_captured_by_nested_functions() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let captured = module.add_value(Some("captured".into()));

    let nested = module.reserve_function();
    let nested_return = module.reserve_continuation();
    let nested_body = jump(&mut module, nested_return, vec![Atom::Value(captured)]);
    module.define_function(
        nested,
        Function {
            debug_name: Some("nested".into()),
            params: vec![],
            return_cont: nested_return,
            body: nested_body,
            droppable: false,
        },
    );
    let return_nested = jump(&mut module, return_cont, vec![Atom::Fun(nested)]);
    let scope_body = module.add_node(Node::LetFun {
        functions: vec![nested],
        body: return_nested,
    });
    let scope = continuation(&mut module, vec![captured], scope_body);
    let call = jump(&mut module, scope, vec![nat(7)]);
    let body = holding(&mut module, vec![scope], call);
    entry(&mut module, main, vec![], return_cont, body);

    assert!(shrink(&mut module));
    assert!(matches!(
        module.node(nested_body),
        Some(Node::ApplyCont(Edge { args, .. })) if args == &[nat(7)]
    ));
    module.verify().unwrap();
}

/// The payload of `option`, read into a value `next` continues with.
fn payload(
    module: &mut Module,
    row: RowId,
    option: ValueId,
    next: impl FnOnce(&mut Module, ValueId) -> NodeId,
) -> NodeId {
    let x = module.add_value(None);
    let rest = next(module, x);
    module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::RowGet(row, 1),
        args: vec![Atom::Value(option)],
        next: rest,
    })
}

/// `main(a)`: `o0 = some(a)`, then `links` matches each on the one before, `some(x) => some(x + 1) | none() => none()`, each joined into the next, and a last one returning the payload or zero.
fn match_chain(links: usize) -> Module {
    let mut module = Module::new();
    let row = module.add_row(Row {
        debug_name: Some("option".into()),
        slots: vec![Slot::Tag, Slot::Opaque],
    });
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let options = (0..=links)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();

    // The match on `option`: its tag decides the arms `some` and `none`.
    let matching =
        |module: &mut Module, option: ValueId, some: ContinuationId, none: ContinuationId| {
            let tag = module.add_value(None);
            let switch = module.add_node(Node::Switch {
                scrutinee: Atom::Value(tag),
                cases: BTreeMap::from([
                    (
                        0,
                        Edge {
                            target: some,
                            args: vec![],
                        },
                    ),
                    (
                        1,
                        Edge {
                            target: none,
                            args: vec![],
                        },
                    ),
                ]),
                default: None,
            });
            let read = module.add_node(Node::LetIntrinsic {
                result: tag,
                op: Intrinsic::RowGet(row, 0),
                args: vec![Atom::Value(option)],
                next: switch,
            });
            holding(module, vec![some, none], read)
        };
    let last = options[links];
    let some_body = payload(&mut module, row, last, |module, x| {
        jump(module, main_return, vec![Atom::Value(x)])
    });
    let some = continuation(&mut module, vec![], some_body);
    let none_body = jump(&mut module, main_return, vec![nat(0)]);
    let none = continuation(&mut module, vec![], none_body);
    let mut body = matching(&mut module, last, some, none);

    for link in (1..=links).rev() {
        let join = continuation(&mut module, vec![options[link]], body);
        let before = options[link - 1];
        let some_body = payload(&mut module, row, before, |module, x| {
            let y = module.add_value(None);
            let built = module.add_value(None);
            let deliver = jump(module, join, vec![Atom::Value(built)]);
            let build = module.add_node(Node::LetValue {
                result: built,
                value: ValueExpr::Row(row, vec![nat(0), Atom::Value(y)]),
                next: deliver,
            });
            module.add_node(Node::LetIntrinsic {
                result: y,
                op: Intrinsic::NatAdd,
                args: vec![Atom::Value(x), nat(1)],
                next: build,
            })
        });
        let some = continuation(&mut module, vec![], some_body);
        let empty = module.add_value(None);
        let deliver = jump(&mut module, join, vec![Atom::Value(empty)]);
        let none_body = module.add_node(Node::LetValue {
            result: empty,
            value: ValueExpr::Row(row, vec![nat(1), Atom::Filler]),
            next: deliver,
        });
        let none = continuation(&mut module, vec![], none_body);
        let matched = matching(&mut module, before, some, none);
        body = holding(&mut module, vec![join], matched);
    }
    let start = module.add_node(Node::LetValue {
        result: options[0],
        value: ValueExpr::Row(row, vec![nat(0), Atom::Value(a)]),
        next: body,
    });
    entry(&mut module, main, vec![a], main_return, start);
    module.verify().unwrap();
    module
}

/// Each match folds on the construction before it, which leaves its join one jump, and the join put in place hands the next match its construction: the whole chain reduces in one pass.
#[test]
fn a_chain_of_matches_on_known_constructions_reduces_in_one_pass() {
    let mut module = match_chain(64);

    assert!(shrink(&mut module));
    module.verify().unwrap();
    assert_eq!(switches(&module), 0, "every match folds:\n{module}");
    assert_eq!(
        module.live_continuations().count(),
        0,
        "every join is put in place"
    );
    assert!(
        !shrink(&mut module),
        "and nothing is left for a second pass"
    );
}

/// `main(a) = g0(a)(a)…(a)`, `levels` applications of `g0(x0) = (x1) => … => x0`, each lambda bound in the one before and applied through a continuation.
fn function_nest(levels: usize) -> (Module, ValueId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let lambdas = (0..levels)
        .map(|_| {
            (
                module.reserve_function(),
                module.reserve_continuation(),
                module.add_value(None),
            )
        })
        .collect::<Vec<_>>();

    let (innermost, innermost_return, innermost_param) = lambdas[levels - 1];
    let body = jump(
        &mut module,
        innermost_return,
        vec![Atom::Value(lambdas[0].2)],
    );
    module.define_function(
        innermost,
        Function {
            debug_name: None,
            params: vec![innermost_param],
            return_cont: innermost_return,
            body,
            droppable: false,
        },
    );
    for level in (0..levels - 1).rev() {
        let (lambda, lambda_return, param) = lambdas[level];
        let inner = lambdas[level + 1].0;
        let deliver = jump(&mut module, lambda_return, vec![Atom::Fun(inner)]);
        let body = module.add_node(Node::LetFun {
            functions: vec![inner],
            body: deliver,
        });
        module.define_function(
            lambda,
            Function {
                debug_name: None,
                params: vec![param],
                return_cont: lambda_return,
                body,
                droppable: false,
            },
        );
    }

    let results = (0..levels)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    let mut resume = {
        let deliver = jump(
            &mut module,
            main_return,
            vec![Atom::Value(results[levels - 1])],
        );
        continuation(&mut module, vec![results[levels - 1]], deliver)
    };
    for level in (1..levels).rev() {
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Closure(results[level - 1]),
            args: vec![Atom::Value(a)],
            return_to: resume,
        });
        let body = holding(&mut module, vec![resume], call);
        resume = continuation(&mut module, vec![results[level - 1]], body);
    }
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(lambdas[0].0),
        args: vec![Atom::Value(a)],
        return_to: resume,
    });
    let held = holding(&mut module, vec![resume], call);
    let body = module.add_node(Node::LetFun {
        functions: vec![lambdas[0].0],
        body: held,
    });
    entry(&mut module, main, vec![a], main_return, body);
    module.verify().unwrap();
    (module, a)
}

/// Each lambda has one call once the one before is moved into its own: the call returns it to a continuation of one jump, which put in place makes the next closure call a call of it. The whole nest reduces in one pass, to the argument it returns.
#[test]
fn a_nest_of_lambdas_applied_in_turn_reduces_in_one_pass() {
    let (mut module, a) = function_nest(64);

    assert!(shrink(&mut module));
    module.verify().unwrap();
    assert_eq!(
        module.live_functions().count(),
        1,
        "every lambda is moved into its call:\n{module}"
    );
    assert_eq!(module.live_continuations().count(), 0);
    let main = module.entry().unwrap();
    assert!(matches!(
        module.node(module.function(main).unwrap().body),
        Some(Node::ApplyCont(Edge { args, .. })) if args == &[Atom::Value(a)]
    ));
    assert!(
        !shrink(&mut module),
        "and nothing is left for a second pass"
    );
}

/// `main(a)`: a switch on the literal one takes `exit`, returning `a`, and drops `enter`, the only jump into a counting loop from outside it. The loop is left with its back edge alone, which is no entrance: it is dropped, never put in place at a jump inside itself.
#[test]
fn a_loop_whose_entrance_is_dropped_is_dropped_with_it() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let counter = module.add_value(Some("i".into()));
    let below = module.add_value(None);
    let after = module.add_value(None);

    let lp = module.reserve_continuation();
    let back = jump(&mut module, lp, vec![Atom::Value(after)]);
    let step_body = module.add_node(Node::LetIntrinsic {
        result: after,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(counter), nat(1)],
        next: back,
    });
    let step = continuation(&mut module, vec![], step_body);
    let done_body = jump(&mut module, main_return, vec![Atom::Value(counter)]);
    let done = continuation(&mut module, vec![], done_body);
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(below),
        cases: BTreeMap::from([
            (
                1,
                Edge {
                    target: step,
                    args: vec![],
                },
            ),
            (
                0,
                Edge {
                    target: done,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let arms = holding(&mut module, vec![step, done], switch);
    let test = module.add_node(Node::LetIntrinsic {
        result: below,
        op: Intrinsic::NatLt,
        args: vec![Atom::Value(counter), Atom::Value(a)],
        next: arms,
    });
    module.define_continuation(
        lp,
        Continuation {
            debug_name: Some("loop".into()),
            params: vec![counter],
            body: test,
        },
    );

    let enter_body = jump(&mut module, lp, vec![nat(0)]);
    let enter = continuation(&mut module, vec![], enter_body);
    let exit_body = jump(&mut module, main_return, vec![Atom::Value(a)]);
    let exit = continuation(&mut module, vec![], exit_body);
    let choose = module.add_node(Node::Switch {
        scrutinee: nat(1),
        cases: BTreeMap::from([
            (
                0,
                Edge {
                    target: enter,
                    args: vec![],
                },
            ),
            (
                1,
                Edge {
                    target: exit,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let chosen = holding(&mut module, vec![enter, exit], choose);
    let body = holding(&mut module, vec![lp], chosen);
    entry(&mut module, main, vec![a], main_return, body);
    module.verify().unwrap();

    assert!(shrink(&mut module));
    module.verify().unwrap();
    assert!(
        module.continuation(lp).is_none(),
        "the loop is dropped:\n{module}"
    );
    assert!(matches!(
        module.node(module.function(main).unwrap().body),
        Some(Node::ApplyCont(Edge { args, .. })) if args == &[Atom::Value(a)]
    ));
}

/// `f(x) = f(x + 1)`, called by nothing else and named nowhere: one call, and it is in `f` itself. Moved into it, `f` would hold itself.
#[test]
fn a_function_whose_one_call_is_its_own_stays_where_it_is() {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let a = module.add_value(Some("a".into()));
    let f = module.reserve_function();
    let f_return = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let next = module.add_value(None);

    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(f),
        args: vec![Atom::Value(next)],
        return_to: f_return,
    });
    let step = module.add_node(Node::LetIntrinsic {
        result: next,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(x), nat(1)],
        next: call,
    });
    module.define_function(
        f,
        Function {
            debug_name: Some("f".into()),
            params: vec![x],
            return_cont: f_return,
            body: step,
            droppable: false,
        },
    );
    let deliver = jump(&mut module, main_return, vec![Atom::Value(a)]);
    let body = module.add_node(Node::LetFun {
        functions: vec![f],
        body: deliver,
    });
    entry(&mut module, main, vec![a], main_return, body);
    module.verify().unwrap();

    shrink(&mut module);
    module.verify().unwrap();
    assert!(
        module
            .function(f)
            .is_some_and(|definition| definition.body == step)
    );
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyFun { callee: Callee::Known(callee), .. }) if *callee == f
    ));
}
