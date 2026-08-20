use {
    super::{Storage, storage},
    crate::Repr,
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsic, CpsLiteral,
        CpsModule, CpsNode, CpsNodeId, CpsValueId,
    },
};

/// Make `body` the entry function's body, with `params` as its parameters. The return continuation is reserved and never defined, which is exactly what a function's return sentinel is.
fn entry(module: &mut CpsModule, params: Vec<CpsValueId>, body: CpsNodeId) {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();

    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params,
            return_cont,
            body,
        },
    );
    module.set_entry(function);
}

/// Route `body` through a continuation taking `params`, and answer the node that enters it. A literal argument stands in for the incoming values, so entering the continuation demands nothing of its own.
fn through_continuation(
    module: &mut CpsModule,
    params: Vec<CpsValueId>,
    body: CpsNodeId,
) -> CpsNodeId {
    let head = module.reserve_continuation();
    let args = params
        .iter()
        .map(|_| CpsAtom::Literal(CpsLiteral::Nat(0)))
        .collect();

    module.define_continuation(
        head,
        CpsContinuation {
            debug_name: Some("head".into()),
            params,
            body,
        },
    );

    module.add_node(CpsNode::ApplyCont(CpsEdge { target: head, args }))
}

/// Jump to a reserved-but-undefined continuation — the return sentinel, which demands nothing of what it carries.
fn finish(module: &mut CpsModule, args: Vec<CpsAtom>) -> CpsNodeId {
    let sentinel = module.reserve_continuation();
    module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: sentinel,
        args,
    }))
}

#[test]
fn an_intrinsic_operand_position_demands_the_raw_carrier() {
    let mut module = CpsModule::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![CpsAtom::Value(result)]);
    let body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(param), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: done,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(storage(&module)[&param], Storage::Raw(Repr::Nat));
}

/// The scrutinee is read as an unsigned tag, so it is a raw position even though nothing arithmetic touches it.
#[test]
fn a_switch_scrutinee_demands_the_raw_carrier() {
    let mut module = CpsModule::new();
    let param = module.add_value(Some("tag".into()));

    let done = finish(&mut module, vec![]);
    let body = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(param),
        cases: Default::default(),
        default: Some(CpsEdge {
            target: match module.node(done) {
                Some(CpsNode::ApplyCont(edge)) => edge.target,
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
    let mut module = CpsModule::new();
    let param = module.add_value(Some("a".into()));
    let callee = module.reserve_function();
    let resume = module.reserve_continuation();

    let body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(param)],
        return_to: resume,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(storage(&module)[&param], Storage::Boxed);
}

/// The rule the whole analysis turns on. `carried` is *only* ever passed round an edge — it has no raw use of its own — so a scan over use positions would leave it boxed and the loop would coerce every iteration. Its demand is the storage of the parameter it feeds, and that parameter is raw because an intrinsic reads it, so the decision has to travel backwards across the edge to reach it.
#[test]
fn an_edge_argument_inherits_the_storage_of_the_parameter_it_feeds() {
    let mut module = CpsModule::new();
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    // The loop head reads its parameter arithmetically, which is what makes the parameter raw.
    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![CpsAtom::Value(result)]);
    let head_body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(param), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: done,
    });
    module.define_continuation(
        head,
        CpsContinuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );

    // `carried` is produced arithmetically and then reaches the head only as an edge argument.
    let jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: head,
        args: vec![CpsAtom::Value(carried)],
    }));
    let body = module.add_node(CpsNode::LetIntrinsic {
        result: carried,
        op: CpsIntrinsic::NatAdd,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ],
        next: jump,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&carried], Storage::Raw(Repr::Nat));
}

/// The top of the lattice. A continuation parameter is the one value with no producer to fix its carrier, so it is the one value two uses can disagree about — and the disagreement has to settle *above* both, because the solver stops when nothing changed, not when nothing is left to change.
#[test]
fn disagreeing_raw_carriers_settle_at_conflict_rather_than_oscillating() {
    let mut module = CpsModule::new();
    let shared = module.add_value(Some("shared".into()));
    let first = module.add_value(Some("a".into()));
    let second = module.add_value(Some("b".into()));

    let done = finish(&mut module, vec![CpsAtom::Value(second)]);
    // Read as a signed carrier here...
    let signed = module.add_node(CpsNode::LetIntrinsic {
        result: second,
        op: CpsIntrinsic::IntAdd,
        args: vec![CpsAtom::Value(shared), CpsAtom::Literal(CpsLiteral::Int(1))],
        next: done,
    });
    // ...and as an unsigned one here.
    let body = module.add_node(CpsNode::LetIntrinsic {
        result: first,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(shared), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: signed,
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
    let mut module = CpsModule::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![CpsAtom::Value(result)]);
    let body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(param), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: done,
    });
    entry(&mut module, vec![param], body);

    assert_eq!(storage(&module)[&param], Storage::Boxed);
}

/// The case the emitter caught and the unit tests did not. `escaping` is bound in the entry function and read arithmetically inside *another* function's body, so `machine::lower` lambda-lifts it onto that function as an extra `anyref` parameter. Deciding it from its binding scope alone answers `Raw(Nat)` — and then the callee, which holds it as a parameter, loads it with no unboxing and hands a `(ref any)` to an `i32.sub`. `trees` miscompiled exactly this way, in `/std/Str/fold/2`.
#[test]
fn a_value_free_in_another_function_stays_boxed() {
    let mut module = CpsModule::new();
    let escaping = module.add_value(Some("escaping".into()));
    let result = module.add_value(Some("r".into()));
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let resume = module.reserve_continuation();

    // The callee reads a value it does not bind, which is what makes it free there.
    let callee_done = finish(&mut module, vec![CpsAtom::Value(result)]);
    let callee_body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![
            CpsAtom::Value(escaping),
            CpsAtom::Literal(CpsLiteral::Nat(1)),
        ],
        next: callee_done,
    });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
        },
    );

    // The entry binds it, and binds it at the very carrier the callee wants.
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    let body = module.add_node(CpsNode::LetIntrinsic {
        result: escaping,
        op: CpsIntrinsic::NatAdd,
        args: vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ],
        next: call,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&escaping], Storage::Boxed);
}

/// The same argument one step further out: a call's result reaches its continuation's parameter as a reference, because that is what the callee returned. The parameter is open to its uses in general, and this is what withdraws it.
#[test]
fn a_call_result_stays_boxed_however_its_continuation_reads_it() {
    let mut module = CpsModule::new();
    let returned = module.add_value(Some("returned".into()));
    let result = module.add_value(Some("r".into()));
    let callee = module.reserve_function();
    let resume = module.reserve_continuation();

    let done = finish(&mut module, vec![CpsAtom::Value(result)]);
    let resume_body = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![
            CpsAtom::Value(returned),
            CpsAtom::Literal(CpsLiteral::Nat(1)),
        ],
        next: done,
    });
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![returned],
            body: resume_body,
        },
    );

    let body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    entry(&mut module, vec![], body);

    assert_eq!(storage(&module)[&returned], Storage::Boxed);
}
