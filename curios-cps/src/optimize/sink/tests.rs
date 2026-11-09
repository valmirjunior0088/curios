use {super::*, crate::*, curios_num::Natural};

fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

fn value(module: &mut Module, name: &str) -> ValueId {
    module.add_value(Some(name.into()))
}

fn jump(module: &mut Module, target: ContinuationId, args: Vec<Atom>) -> NodeId {
    module.add_node(Node::ApplyCont(Edge { target, args }))
}

fn continuation(
    module: &mut Module,
    name: &str,
    params: Vec<ValueId>,
    body: NodeId,
) -> ContinuationId {
    module.add_continuation(Continuation {
        debug_name: Some(name.into()),
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

fn switch(module: &mut Module, scrutinee: Atom, edges: Vec<(u32, Edge)>) -> NodeId {
    module.add_node(Node::Switch {
        scrutinee,
        cases: edges.into_iter().collect(),
        default: None,
    })
}

fn edge(target: ContinuationId, args: Vec<Atom>) -> Edge {
    Edge { target, args }
}

/// `name(param) = return param`, droppable as `droppable` says.
fn identity(module: &mut Module, name: &str, droppable: bool) -> FunctionId {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let param = value(module, "m");
    let body = jump(module, return_cont, vec![Atom::Value(param)]);
    module.define_function(
        function,
        Function {
            debug_name: Some(name.into()),
            params: vec![param],
            return_cont,
            body,
            droppable,
        },
    );
    function
}

/// The entry `main(params)` returning to `return_cont`, its body `body`, which binds `functions` first.
fn main(
    module: &mut Module,
    params: Vec<ValueId>,
    return_cont: ContinuationId,
    functions: Vec<FunctionId>,
    body: NodeId,
) -> FunctionId {
    let body = match functions.is_empty() {
        true => body,
        false => module.add_node(Node::LetFun { functions, body }),
    };
    let main = module.add_function(Function {
        debug_name: Some("main".into()),
        params,
        return_cont,
        body,
        droppable: false,
    });
    module.set_entry(main);
    main
}

/// The continuation whose body chain reaches `node`, past the bindings standing before it.
fn holder_of(module: &Module, node: NodeId) -> Option<ContinuationId> {
    module
        .live_continuations()
        .find(|(_, definition)| {
            let mut at = definition.body;
            loop {
                if at == node {
                    return true;
                }
                match module.node(at) {
                    Some(
                        Node::LetValue { next, .. }
                        | Node::LetIntrinsic { next, .. }
                        | Node::LetFun { body: next, .. }
                        | Node::LetCont { body: next, .. },
                    ) => at = *next,
                    _ => return false,
                }
            }
        })
        .map(|(id, _)| id)
}

/// `main(a)`: `t = (a, a)`, then `a` chooses between returning `0` and returning `t`. The tuple moves into the arm that returns it.
#[test]
fn a_construction_one_arm_reads_moves_into_that_arm() {
    let mut module = Module::new();
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let t = value(&mut module, "t");
    let zero = jump(&mut module, return_cont, vec![nat(0)]);
    let returns_zero = continuation(&mut module, "arm", vec![], zero);
    let tuple = jump(&mut module, return_cont, vec![Atom::Value(t)]);
    let returns_tuple = continuation(&mut module, "arm", vec![], tuple);
    let choice = switch(
        &mut module,
        Atom::Value(a),
        vec![
            (0, edge(returns_zero, vec![])),
            (1, edge(returns_tuple, vec![])),
        ],
    );
    let arms = holding(&mut module, vec![returns_zero, returns_tuple], choice);
    let build = module.add_node(Node::LetValue {
        result: t,
        value: ValueExpr::Tuple(vec![Atom::Value(a), Atom::Value(a)]),
        next: arms,
    });
    main(&mut module, vec![a], return_cont, vec![], build);

    assert!(sink_computations(&mut module));
    assert_eq!(module.continuation(returns_tuple).unwrap().body, build);
    assert!(matches!(module.node(build), Some(Node::LetValue { next, .. }) if *next == tuple));
    module.verify().unwrap();
}

/// `main(a, n)`: `r = g(n)`, then `return a && r`. The call runs only on the arm `a` takes when it is `true`, where it is the tail call, and `false` returns at once; `g`'s definition, which only the call reads, goes with it.
#[test]
fn a_call_a_conjunctions_second_operand_reads_runs_only_where_the_first_holds() {
    let mut module = Module::new();
    let g = identity(&mut module, "g", true);
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let n = value(&mut module, "n");
    let r = value(&mut module, "r");
    let x = value(&mut module, "x");
    let returned = jump(&mut module, return_cont, vec![Atom::Value(x)]);
    let both = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::BoolAnd,
        args: vec![Atom::Value(a), Atom::Value(r)],
        next: returned,
    });
    let resume = continuation(&mut module, "resume", vec![r], both);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![Atom::Value(n)],
        return_to: resume,
    });
    let held = holding(&mut module, vec![resume], call);
    main(&mut module, vec![a, n], return_cont, vec![g], held);

    assert!(sink_computations(&mut module));
    assert!(matches!(
        module.node(call),
        Some(Node::ApplyFun { return_to, .. }) if *return_to == return_cont
    ));
    let arm = holder_of(&module, call).expect("the call is an arm's body");
    let Some(Node::LetCont { body, .. }) = module.node(both) else {
        panic!("the conjunction is a branch");
    };
    let Some(Node::Switch {
        scrutinee, cases, ..
    }) = module.node(*body)
    else {
        panic!("the branch switches on the first operand");
    };
    assert_eq!(*scrutinee, Atom::Value(a));
    assert_eq!(cases[&1].target, arm);
    let decided = module
        .continuation(cases[&0].target)
        .expect("an edge enters a continuation");
    assert!(matches!(
        module.node(decided.body),
        Some(Node::ApplyCont(Edge { target, args })) if *target == return_cont && *args == vec![nat(0)]
    ));
    module.verify().unwrap();
}

/// `main(a, n)`: `r = g(n)`, then `return a || r`. The call runs on the arm `a` takes when it is `false`, and `true` returns at once.
#[test]
fn a_disjunction_runs_its_second_operand_where_the_first_is_false() {
    let mut module = Module::new();
    let g = identity(&mut module, "g", true);
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let n = value(&mut module, "n");
    let r = value(&mut module, "r");
    let x = value(&mut module, "x");
    let returned = jump(&mut module, return_cont, vec![Atom::Value(x)]);
    let either = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::BoolOr,
        args: vec![Atom::Value(a), Atom::Value(r)],
        next: returned,
    });
    let resume = continuation(&mut module, "resume", vec![r], either);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(g),
        args: vec![Atom::Value(n)],
        return_to: resume,
    });
    let held = holding(&mut module, vec![resume], call);
    main(&mut module, vec![a, n], return_cont, vec![g], held);

    assert!(sink_computations(&mut module));
    let arm = holder_of(&module, call).expect("the call is an arm's body");
    let Some(Node::LetCont { body, .. }) = module.node(either) else {
        panic!("the disjunction is a branch");
    };
    let Some(Node::Switch { cases, .. }) = module.node(*body) else {
        panic!("the branch switches on the first operand");
    };
    assert_eq!(cases[&0].target, arm);
    let decided = module
        .continuation(cases[&1].target)
        .expect("an edge enters a continuation");
    assert!(matches!(
        module.node(decided.body),
        Some(Node::ApplyCont(Edge { target, args })) if *target == return_cont && *args == vec![nat(1)]
    ));
    module.verify().unwrap();
}

/// `main(a, b)`: `return a && b`, both operands already values. Nothing moves, so no branch is made.
#[test]
fn a_conjunction_of_values_keeps_its_one_instruction() {
    let mut module = Module::new();
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let b = value(&mut module, "b");
    let x = value(&mut module, "x");
    let returned = jump(&mut module, return_cont, vec![Atom::Value(x)]);
    let both = module.add_node(Node::LetIntrinsic {
        result: x,
        op: Intrinsic::BoolAnd,
        args: vec![Atom::Value(a), Atom::Value(b)],
        next: returned,
    });
    main(&mut module, vec![a, b], return_cont, vec![], both);

    assert!(!sink_computations(&mut module));
    assert!(matches!(
        module.node(both),
        Some(Node::LetIntrinsic {
            op: Intrinsic::BoolAnd,
            ..
        })
    ));
    module.verify().unwrap();
}

/// `main(a)`: `t = (a, a)`, then a loop over `i` from `a` whose step builds `(t, t)` on every turn. The tuple stays where it was built once.
#[test]
fn work_stays_out_of_a_loop_it_ran_before() {
    let mut module = Module::new();
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let t = value(&mut module, "t");
    let i = value(&mut module, "i");
    let w = value(&mut module, "w");
    let lap = module.reserve_continuation();
    let stop_body = jump(&mut module, return_cont, vec![nat(0)]);
    let stop = continuation(&mut module, "stop", vec![], stop_body);
    let again_jump = jump(&mut module, lap, vec![Atom::Value(w)]);
    let rebuild = module.add_node(Node::LetValue {
        result: w,
        value: ValueExpr::Tuple(vec![Atom::Value(t), Atom::Value(t)]),
        next: again_jump,
    });
    let again = continuation(&mut module, "again", vec![], rebuild);
    let choice = switch(
        &mut module,
        Atom::Value(i),
        vec![(0, edge(stop, vec![])), (1, edge(again, vec![]))],
    );
    let lap_body = holding(&mut module, vec![stop, again], choice);
    module.define_continuation(
        lap,
        Continuation {
            debug_name: Some("loop".into()),
            params: vec![i],
            body: lap_body,
        },
    );
    let enter = jump(&mut module, lap, vec![Atom::Value(a)]);
    let held = holding(&mut module, vec![lap], enter);
    let build = module.add_node(Node::LetValue {
        result: t,
        value: ValueExpr::Tuple(vec![Atom::Value(a), Atom::Value(a)]),
        next: held,
    });
    let entry = main(&mut module, vec![a], return_cont, vec![], build);

    sink_computations(&mut module);
    assert_eq!(module.function(entry).unwrap().body, build);
    module.verify().unwrap();
}

/// `main(a)`: `t = (a, a)` and `f() = return t`, then `a` chooses between returning `0` and calling `f`. The function moves into the arm that calls it, and the tuple it captures follows it there.
#[test]
fn a_closure_and_what_it_captures_move_into_the_arm_that_calls_it() {
    let mut module = Module::new();
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let t = value(&mut module, "t");
    let f = module.reserve_function();
    let f_return = module.reserve_continuation();
    let f_body = jump(&mut module, f_return, vec![Atom::Value(t)]);
    module.define_function(
        f,
        Function {
            debug_name: Some("f".into()),
            params: vec![],
            return_cont: f_return,
            body: f_body,
            droppable: false,
        },
    );
    let zero = jump(&mut module, return_cont, vec![nat(0)]);
    let returns_zero = continuation(&mut module, "arm", vec![], zero);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(f),
        args: vec![],
        return_to: return_cont,
    });
    let calls_f = continuation(&mut module, "arm", vec![], call);
    let choice = switch(
        &mut module,
        Atom::Value(a),
        vec![(0, edge(returns_zero, vec![])), (1, edge(calls_f, vec![]))],
    );
    let arms = holding(&mut module, vec![returns_zero, calls_f], choice);
    let defines = module.add_node(Node::LetFun {
        functions: vec![f],
        body: arms,
    });
    let build = module.add_node(Node::LetValue {
        result: t,
        value: ValueExpr::Tuple(vec![Atom::Value(a), Atom::Value(a)]),
        next: defines,
    });
    main(&mut module, vec![a], return_cont, vec![], build);

    assert!(sink_computations(&mut module));
    assert_eq!(module.continuation(calls_f).unwrap().body, build);
    assert!(matches!(module.node(build), Some(Node::LetValue { next, .. }) if *next == defines));
    module.verify().unwrap();
}

/// `main(a)`: `q = a / 2` and `s = a + 2`, then `r = h(a)` for an `h` that is no droppable function, whose resume returns `0` where `r` is `0` and `(q, s)` otherwise. The sum, which cannot fail, moves past the call into that arm; the quotient, which may trap, stays before it.
#[test]
fn work_that_may_trap_does_not_move_past_a_call_where_total_work_does() {
    let mut module = Module::new();
    let h = identity(&mut module, "h", false);
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let q = value(&mut module, "q");
    let s = value(&mut module, "s");
    let r = value(&mut module, "r");
    let both = value(&mut module, "both");
    let zero = jump(&mut module, return_cont, vec![nat(0)]);
    let returns_zero = continuation(&mut module, "arm", vec![], zero);
    let returned = jump(&mut module, return_cont, vec![Atom::Value(both)]);
    let gather = module.add_node(Node::LetValue {
        result: both,
        value: ValueExpr::Tuple(vec![Atom::Value(q), Atom::Value(s)]),
        next: returned,
    });
    let returns_both = continuation(&mut module, "arm", vec![], gather);
    let choice = switch(
        &mut module,
        Atom::Value(r),
        vec![
            (0, edge(returns_zero, vec![])),
            (1, edge(returns_both, vec![])),
        ],
    );
    let arms = holding(&mut module, vec![returns_zero, returns_both], choice);
    let resume = continuation(&mut module, "resume", vec![r], arms);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(h),
        args: vec![Atom::Value(a)],
        return_to: resume,
    });
    let held = holding(&mut module, vec![resume], call);
    let sum = module.add_node(Node::LetIntrinsic {
        result: s,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(a), nat(2)],
        next: held,
    });
    let quotient = module.add_node(Node::LetIntrinsic {
        result: q,
        op: Intrinsic::NatDiv,
        args: vec![Atom::Value(a), nat(2)],
        next: sum,
    });
    let entry = main(&mut module, vec![a], return_cont, vec![h], quotient);

    assert!(sink_computations(&mut module));
    assert_eq!(module.continuation(returns_both).unwrap().body, sum);
    let Some(Node::LetFun { body, .. }) = module.node(module.function(entry).unwrap().body) else {
        panic!("main binds h first");
    };
    assert_eq!(*body, quotient);
    assert!(
        matches!(module.node(quotient), Some(Node::LetIntrinsic { next, .. }) if *next == held)
    );
    module.verify().unwrap();
}

/// `main(a)`: `s = a + 2`, then `r = h(a)` for an `h` that is no droppable function, whose resume returns `(s, r)`. Every run that computes the sum reads it after the call, so it stays where it is.
#[test]
fn work_every_run_reads_stays_where_it_is() {
    let mut module = Module::new();
    let h = identity(&mut module, "h", false);
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let s = value(&mut module, "s");
    let r = value(&mut module, "r");
    let both = value(&mut module, "both");
    let returned = jump(&mut module, return_cont, vec![Atom::Value(both)]);
    let gather = module.add_node(Node::LetValue {
        result: both,
        value: ValueExpr::Tuple(vec![Atom::Value(s), Atom::Value(r)]),
        next: returned,
    });
    let resume = continuation(&mut module, "resume", vec![r], gather);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(h),
        args: vec![Atom::Value(a)],
        return_to: resume,
    });
    let held = holding(&mut module, vec![resume], call);
    let sum = module.add_node(Node::LetIntrinsic {
        result: s,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(a), nat(2)],
        next: held,
    });
    main(&mut module, vec![a], return_cont, vec![h], sum);

    assert!(!sink_computations(&mut module));
    assert_eq!(module.continuation(resume).unwrap().body, gather);
    module.verify().unwrap();
}

/// `main(a, n)`: `r = h(n)`, then `a` chooses between returning `0` and returning `r`. A call of a function that may not be dropped stays where it was made; one of a function that may be moves into the arm, as its tail call, and `h`'s definition with it.
#[test]
fn only_a_droppable_call_moves_into_the_arm_that_reads_it() {
    for droppable in [false, true] {
        let mut module = Module::new();
        let h = identity(&mut module, "h", droppable);
        let return_cont = module.reserve_continuation();
        let a = value(&mut module, "a");
        let n = value(&mut module, "n");
        let r = value(&mut module, "r");
        let zero = jump(&mut module, return_cont, vec![nat(0)]);
        let returns_zero = continuation(&mut module, "arm", vec![], zero);
        let result = jump(&mut module, return_cont, vec![Atom::Value(r)]);
        let returns_result = continuation(&mut module, "arm", vec![], result);
        let choice = switch(
            &mut module,
            Atom::Value(a),
            vec![
                (0, edge(returns_zero, vec![])),
                (1, edge(returns_result, vec![])),
            ],
        );
        let arms = holding(&mut module, vec![returns_zero, returns_result], choice);
        let resume = continuation(&mut module, "resume", vec![r], arms);
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(h),
            args: vec![Atom::Value(n)],
            return_to: resume,
        });
        let held = holding(&mut module, vec![resume], call);
        main(&mut module, vec![a, n], return_cont, vec![h], held);

        assert_eq!(sink_computations(&mut module), droppable);
        assert_eq!(
            holder_of(&module, call) == Some(returns_result),
            droppable,
            "droppable: {droppable}"
        );
        module.verify().unwrap();
    }
}

/// `main(a)`: `t = (a, a)`, then `a` jumps to a join with `t` on one edge and `a` on the other. The tuple moves onto a landing made for its edge.
#[test]
fn an_argument_of_one_switch_edge_moves_onto_a_landing_on_that_edge() {
    let mut module = Module::new();
    let return_cont = module.reserve_continuation();
    let a = value(&mut module, "a");
    let t = value(&mut module, "t");
    let v = value(&mut module, "v");
    let returned = jump(&mut module, return_cont, vec![Atom::Value(v)]);
    let join = continuation(&mut module, "join", vec![v], returned);
    let choice = switch(
        &mut module,
        Atom::Value(a),
        vec![
            (0, edge(join, vec![Atom::Value(t)])),
            (1, edge(join, vec![Atom::Value(a)])),
        ],
    );
    let held = holding(&mut module, vec![join], choice);
    let build = module.add_node(Node::LetValue {
        result: t,
        value: ValueExpr::Tuple(vec![Atom::Value(a), Atom::Value(a)]),
        next: held,
    });
    main(&mut module, vec![a], return_cont, vec![], build);

    assert!(sink_computations(&mut module));
    let Some(Node::Switch { cases, .. }) = module.node(choice) else {
        panic!("the switch stays a switch");
    };
    assert_eq!(cases[&1].target, join);
    let landing = cases[&0].target;
    assert_ne!(landing, join);
    assert!(cases[&0].args.is_empty());
    assert_eq!(module.continuation(landing).unwrap().body, build);
    module.verify().unwrap();
}
