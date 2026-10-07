use {
    super::{Storage, storage},
    crate::{
        Atom, Callee, Continuation, Edge, Function, Intrinsic, Literal, Module, Node, NodeId, Repr,
        ValueId,
    },
    curios_num::{Floating, Natural, Rounding},
};

/// Make `body` the entry function's body, with `params` as its parameters. The return continuation is reserved and never defined, which is exactly what a function's return sentinel is.
fn entry(module: &mut Module, params: Vec<ValueId>, body: NodeId) {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();

    module.define_function(
        function,
        Function {
            debug_name: Some("main".into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(function);
}

/// Route `body` through a continuation taking `params`, and answer the node that enters it. A filler stands in for the incoming values: it inhabits whatever carrier the parameter settles at, so entering the continuation demands nothing and withholds nothing of its own.
fn through_continuation(module: &mut Module, params: Vec<ValueId>, body: NodeId) -> NodeId {
    let head = module.reserve_continuation();
    let args = params.iter().map(|_| Atom::Filler).collect();

    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params,
            body,
        },
    );

    module.add_node(Node::ApplyCont(Edge { target: head, args }))
}

/// Jump to a reserved-but-undefined continuation — the return sentinel, which demands nothing of what it carries.
fn finish(module: &mut Module, args: Vec<Atom>) -> NodeId {
    let sentinel = module.reserve_continuation();
    module.add_node(Node::ApplyCont(Edge {
        target: sentinel,
        args,
    }))
}

#[test]
fn an_intrinsic_operand_position_demands_the_raw_carrier() {
    let mut module = Module::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![Atom::Value(result)]);
    let body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(storage(&module)[&param], Storage::Raw(Repr::Nat));
}

/// The scrutinee is read as an unsigned tag, so it is a raw position even though nothing arithmetic touches it.
#[test]
fn a_switch_scrutinee_demands_the_raw_carrier() {
    let mut module = Module::new();
    let param = module.add_value(Some("tag".into()));

    let done = finish(&mut module, vec![]);
    let body = module.add_node(Node::Switch {
        scrutinee: Atom::Value(param),
        cases: Default::default(),
        default: Some(Edge {
            target: match module.node(done) {
                Some(Node::ApplyCont(edge)) => edge.target,
                _ => unreachable!("`finish` builds an `ApplyCont`"),
            },
            args: vec![],
        }),
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(storage(&module)[&param], Storage::Raw(Repr::Nat));
}

/// A call argument crosses a `func/N` signature that is uniformly `anyref`, so nothing about it is raw.
#[test]
fn a_call_argument_alone_stays_boxed() {
    let mut module = Module::new();
    let param = module.add_value(Some("a".into()));
    let callee = module.reserve_function();
    let resume = module.reserve_continuation();

    let body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(param)],
        return_to: resume,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(storage(&module)[&param], Storage::Boxed);
}

/// The rule the whole analysis turns on. `carried` is *only* ever passed round an edge — it has no raw use of its own — so a scan over use positions would leave it boxed and the loop would coerce every iteration. Its demand is the storage of the parameter it feeds, and that parameter is raw because an intrinsic reads it, so the decision has to travel backwards across the edge to reach it.
#[test]
fn an_edge_argument_inherits_the_storage_of_the_parameter_it_feeds() {
    let mut module = Module::new();
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    // The loop head reads its parameter arithmetically, which is what makes the parameter raw.
    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(result)]);
    let head_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );

    // `carried` is a remainder its literal divisor bounds, so it may ride a word, and it reaches the head only as an edge argument.
    let jump = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(carried)],
    }));
    let body = module.add_node(Node::LetIntrinsic {
        result: carried,
        op: Intrinsic::NatRem,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(9u32))),
            Atom::Literal(Literal::Nat(Natural::from(7u32))),
        ],
        next: jump,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&carried], Storage::Raw(Repr::Nat));
}

/// The soundness half of the edge rule. A sum is unbounded, so it is a reference that narrowing to a word would saturate; a parameter it reaches must therefore stay a reference however its uses read it, or the edge would coerce a value past the i31 into a word and lose it.
#[test]
fn a_parameter_an_unbounded_value_reaches_is_never_held_in_a_word() {
    let mut module = Module::new();
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(result)]);
    let head_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );

    let jump = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(carried)],
    }));
    let body = module.add_node(Node::LetIntrinsic {
        result: carried,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ],
        next: jump,
    });
    entry(&mut module, vec![], body);

    let decided = storage(&module);
    assert_eq!(decided[&param], Storage::Boxed);
    assert_eq!(decided[&carried], Storage::Boxed);
}

/// The top of the lattice. A continuation parameter is the one value with no producer to fix its carrier, so it is the one value two uses can disagree about — and the disagreement has to settle *above* both, because the solver stops when nothing changed, not when nothing is left to change.
#[test]
fn disagreeing_raw_carriers_settle_at_conflict_rather_than_oscillating() {
    let mut module = Module::new();
    let shared = module.add_value(Some("shared".into()));
    let first = module.add_value(Some("a".into()));
    let second = module.add_value(Some("b".into()));

    let done = finish(&mut module, vec![Atom::Value(second)]);
    // Read as a float here...
    let float = module.add_node(Node::LetIntrinsic {
        result: second,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(shared),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next: done,
    });
    // ...and as a word here.
    let body = module.add_node(Node::LetIntrinsic {
        result: first,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(shared),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: float,
    });
    let enter = through_continuation(&mut module, vec![shared], body);
    entry(&mut module, vec![], enter);

    let decided = storage(&module);
    assert_eq!(decided[&shared], Storage::Conflict);
    assert_eq!(decided[&shared].raw_carrier(), None);
}

/// A function is entered through a `func/N` signature whose parameters are uniformly `anyref`. There is no store site the analysis controls, so however arithmetically the body reads the parameter, it is held as it arrived and each use unboxes.
#[test]
fn a_function_parameter_stays_boxed_however_its_body_reads_it() {
    let mut module = Module::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![Atom::Value(result)]);
    let body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    entry(&mut module, vec![param], body);

    assert_eq!(storage(&module)[&param], Storage::Boxed);
}

/// `escaping` is bound in the entry function and read arithmetically inside *another* function's body, so `curios-emit`'s `machine::lower` lambda-lifts it onto that function as an extra `anyref` parameter. Deciding it from its binding scope alone would answer `Raw(Nat)` — and the callee, which holds it as a parameter, would load it with no unboxing and hand a `(ref any)` to an `i32.sub`.
#[test]
fn a_value_free_in_another_function_stays_boxed() {
    let mut module = Module::new();
    let escaping = module.add_value(Some("escaping".into()));
    let result = module.add_value(Some("r".into()));
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let resume = module.reserve_continuation();

    // The callee reads a value it does not bind, which is what makes it free there.
    let callee_done = finish(&mut module, vec![Atom::Value(result)]);
    let callee_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(escaping),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: callee_done,
    });
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );

    // The entry binds it, and binds it at the very carrier the callee wants.
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: escaping,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
            Atom::Literal(Literal::Nat(Natural::from(2u32))),
        ],
        next: call,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&escaping], Storage::Boxed);
}

/// The same argument one step further out: a call's result reaches its continuation's parameter as a reference, because that is what the callee returned. The parameter is open to its uses in general, and this is what withdraws it.
#[test]
fn a_call_result_stays_boxed_however_its_continuation_reads_it() {
    let mut module = Module::new();
    let returned = module.add_value(Some("returned".into()));
    let result = module.add_value(Some("r".into()));
    let callee = module.reserve_function();
    let resume = module.reserve_continuation();

    let done = finish(&mut module, vec![Atom::Value(result)]);
    let resume_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(returned),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    module.define_continuation(
        resume,
        Continuation {
            debug_name: Some("resume".into()),
            params: vec![returned],
            body: resume_body,
        },
    );

    let body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&returned], Storage::Boxed);
}

/// A join whose parameter is read as a float, entered on one edge with a float a definition produced and on the other with `other`; `params` are the entry's own. Answers the parameter.
fn float_join(module: &mut Module, params: Vec<ValueId>, flag: ValueId, other: Atom) -> ValueId {
    let joined = module.add_value(Some("joined".into()));
    let read = module.add_value(Some("read".into()));
    let float = module.add_value(Some("float".into()));

    let join = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(read)]);
    let join_body = module.add_node(Node::LetIntrinsic {
        result: read,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(joined),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next: done,
    });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![joined],
            body: join_body,
        },
    );

    let branch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(flag),
        cases: [(
            0,
            Edge {
                target: join,
                args: vec![Atom::Value(float)],
            },
        )]
        .into(),
        default: Some(Edge {
            target: join,
            args: vec![other],
        }),
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: float,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Literal(Literal::Flt(Floating::from(2.0))),
            Atom::Literal(Literal::Flt(Floating::from(3.0))),
        ],
        next: branch,
    });
    entry(module, params, body);

    joined
}

/// The soundness half of the float rule. A reference states no shape: the one reaching this join may be the error a variant's other constructor holds in the payload slot the float shares, or the string a type computes on the other branch. The join reads a float under one branch only and the edge coerces before any branch is taken, so a register would cast the reference, and the parameter stays one.
#[test]
fn a_parameter_a_reference_reaches_is_never_held_in_a_float_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let reference = module.add_value(Some("reference".into()));
    let joined = float_join(
        &mut module,
        vec![flag, reference],
        flag,
        Atom::Value(reference),
    );

    assert_eq!(storage(&module)[&joined], Storage::Boxed);
}

/// The precision half: where every edge hands the parameter a float, a definition's on one and a literal on the other, the register is kept.
#[test]
fn a_parameter_every_edge_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let joined = float_join(
        &mut module,
        vec![flag],
        flag,
        Atom::Literal(Literal::Flt(Floating::from(4.0))),
    );

    assert_eq!(storage(&module)[&joined], Storage::Raw(Repr::Flt));
}
