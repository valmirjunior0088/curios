use {
    super::split_parameters,
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsicOp, CpsLiteral, CpsModule, CpsNode, CpsValueExpr, CpsValueId, FieldGroup,
        optimize,
    },
};

/// The canonical loop-carried product: a seed pair enters a header, one arm projects field 0 and jumps back with a fresh pair, the other hands the parameter to an exit that projects field 0 and nothing else. The accumulator of `/std/Str/fold`, in miniature.
fn loop_module() -> (CpsModule, CpsFunId, CpsContId, CpsValueId) {
    let mut module = CpsModule::default();
    let seed = module.add_value(Some("seed".into()));
    let carried = module.add_value(Some("carried".into()));
    let read = module.add_value(Some("read".into()));
    let bumped = module.add_value(Some("bumped".into()));
    let next = module.add_value(Some("next".into()));
    let out = module.add_value(Some("out".into()));
    let result = module.add_value(Some("result".into()));
    let scrutinee = module.add_value(Some("scrutinee".into()));

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let header = module.reserve_continuation();
    let exit = module.reserve_continuation();

    // exit(out): result = out.0; return result
    let deliver = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(result)],
    }));
    let take = module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(out)],
        next: deliver,
    });
    module.define_continuation(
        exit,
        CpsContinuation {
            debug_name: Some("exit".into()),
            params: vec![out],
            body: take,
        },
    );

    // header(carried): read = carried.0; bumped = read + 1; next = (bumped, 7); switch scrutinee { 0 => header(next), _ => exit(carried) }
    let spin = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(scrutinee),
        cases: [(
            0,
            CpsEdge {
                target: header,
                args: vec![CpsAtom::Value(next)],
            },
        )]
        .into(),
        default: Some(CpsEdge {
            target: exit,
            args: vec![CpsAtom::Value(carried)],
        }),
    });
    let build = module.add_node(CpsNode::LetValue {
        result: next,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Value(bumped),
            CpsAtom::Literal(CpsLiteral::Nat(7)),
        ]),
        next: spin,
    });
    let bump = module.add_node(CpsNode::LetIntrinsic {
        result: bumped,
        op: CpsIntrinsicOp::NatAdd,
        args: vec![CpsAtom::Value(read), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: build,
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: read,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(carried)],
        next: bump,
    });
    module.define_continuation(
        header,
        CpsContinuation {
            debug_name: Some("header".into()),
            params: vec![carried],
            body: project,
        },
    );

    // main(scrutinee): seed = (0, 7); header(seed)
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: header,
        args: vec![CpsAtom::Value(seed)],
    }));
    let plant = module.add_node(CpsNode::LetValue {
        result: seed,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(7)),
        ]),
        next: enter,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![header, exit],
        body: plant,
    });
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![scrutinee],
            return_cont,
            body,
        },
    );
    module.set_entry(function);
    (module, function, header, carried)
}

/// One invocation splits the loop header: two field parameters, the group recorded, and the module still verifying — the backedge alias included.
#[test]
fn a_split_records_the_group_and_verifies() {
    let (mut module, _, header, carried) = loop_module();
    module.verify().expect("the fixture is well-formed");

    assert!(split_parameters(&mut module));
    module
        .verify()
        .expect("the split preserves well-formedness");

    let definition = module.continuation(header).expect("the header survives");
    assert_eq!(
        definition.params.len(),
        2,
        "one aggregate became two fields"
    );
    assert!(
        !definition.params.contains(&carried),
        "the aggregate parameter is gone",
    );
    assert_eq!(
        module.field_groups().get(&header),
        Some(&vec![FieldGroup { start: 0, width: 2 }]),
        "and the split is recorded, not conventional",
    );
}

/// The full chain erases the product: after optimization no two-field tuple construction survives anywhere — not the seed, not the arm's rebuild, not the split's own head materialization. This is the campaign's focused acceptance fixture for continuation scalar replacement.
#[test]
fn a_loop_carried_product_erases_through_the_chain() {
    let (mut module, ..) = loop_module();
    optimize(&mut module);

    let survivors = module
        .nodes()
        .iter()
        .flatten()
        .filter(|node| {
            matches!(
                node,
                CpsNode::LetValue {
                    value: CpsValueExpr::Tuple(atoms),
                    ..
                } if atoms.len() == 2
            )
        })
        .count();
    assert_eq!(
        survivors, 0,
        "the product travels as fields on every path:\n{module}",
    );
}

/// A parameter that sometimes receives a call result is not a candidate: the forward half declines what the backward half would admit.
#[test]
fn a_mixed_origin_is_declined() {
    let mut module = CpsModule::default();
    let built = module.add_value(Some("built".into()));
    let landed = module.add_value(Some("landed".into()));
    let read = module.add_value(Some("read".into()));
    let received = module.add_value(Some("received".into()));
    let argument = module.add_value(Some("argument".into()));

    let callee_param = module.add_value(Some("callee/param".into()));
    let callee = module.reserve_function();
    let callee_ret = module.reserve_continuation();
    let callee_exit = module.add_node(CpsNode::Exit { value: None });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![callee_param],
            return_cont: callee_ret,
            body: callee_exit,
        },
    );

    let caller = module.reserve_function();
    let caller_ret = module.reserve_continuation();
    let join = module.reserve_continuation();
    let resume = module.reserve_continuation();

    let join_exit = module.add_node(CpsNode::Exit { value: None });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: read,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(landed)],
        next: join_exit,
    });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: Some("join".into()),
            params: vec![landed],
            body: project,
        },
    );
    let forward = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: join,
        args: vec![CpsAtom::Value(received)],
    }));
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: forward,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let split_paths = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(argument),
        cases: [(
            0,
            CpsEdge {
                target: join,
                args: vec![CpsAtom::Value(built)],
            },
        )]
        .into(),
        default: None,
    });
    let _ = call;
    let build = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(2)),
        ]),
        next: split_paths,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![join, resume],
        body: build,
    });
    module.define_function(
        caller,
        CpsFunction {
            debug_name: Some("caller".into()),
            params: vec![argument],
            return_cont: caller_ret,
            body,
        },
    );
    module.set_entry(caller);

    assert!(
        !split_parameters(&mut module),
        "a call result reaches the join, so no split is admissible",
    );
}

/// The verifier holds the record to the parameter list: a group past the parameters is an invariant break, not a curiosity.
#[test]
fn the_verifier_rejects_a_stale_record() {
    let (mut module, _, header, _) = loop_module();
    module.record_field_group(header, FieldGroup { start: 0, width: 9 });
    assert!(module.verify().is_err());
}

/// The record survives the optimizer's own reshaping: after the full fixpoint — dead parameters removed, continuations pruned or inlined — every surviving group still lies inside its continuation's parameter list, which is exactly what the verifier holds.
#[test]
fn optimization_maintains_every_record() {
    let (mut module, ..) = loop_module();
    assert!(split_parameters(&mut module));
    optimize(&mut module);
    module.verify().expect("the optimized module verifies");
}

/// The suffix walk in miniature: a rope enters a loop as a whole window, each iteration reads its head and recurses on its tail slice. After the window split the loop carries `(base, offset, length)`, the slice is an extent guard plus an offset sum, and no physical view is ever constructed.
fn walk_module() -> (CpsModule, crate::CpsContId) {
    let mut module = CpsModule::default();
    let rope = module.add_value(Some("rope".into()));
    let scrutinee = module.add_value(Some("scrutinee".into()));
    let window = module.add_value(Some("window".into()));
    let length = module.add_value(Some("length".into()));
    let head = module.add_value(Some("head".into()));
    let tail = module.add_value(Some("tail".into()));

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let header = module.reserve_continuation();

    let spin = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(scrutinee),
        cases: [(
            0,
            CpsEdge {
                target: header,
                args: vec![CpsAtom::Value(tail)],
            },
        )]
        .into(),
        default: Some(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(head)],
        }),
    });
    let slice = module.add_node(CpsNode::LetIntrinsic {
        result: tail,
        op: CpsIntrinsicOp::BinSlice(curios_utilities::Grain::X),
        args: vec![
            CpsAtom::Value(window),
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Value(length),
        ],
        next: spin,
    });
    let read = module.add_node(CpsNode::LetIntrinsic {
        result: head,
        op: CpsIntrinsicOp::BinGet(curios_utilities::Grain::X),
        args: vec![CpsAtom::Value(window), CpsAtom::Literal(CpsLiteral::Nat(0))],
        next: slice,
    });
    let measure = module.add_node(CpsNode::LetIntrinsic {
        result: length,
        op: CpsIntrinsicOp::BinLen(curios_utilities::Grain::X),
        args: vec![CpsAtom::Value(window)],
        next: read,
    });
    module.define_continuation(
        header,
        CpsContinuation {
            debug_name: Some("header".into()),
            params: vec![window],
            body: measure,
        },
    );
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: header,
        args: vec![CpsAtom::Value(rope)],
    }));
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![header],
        body: enter,
    });
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![rope, scrutinee],
            return_cont,
            body,
        },
    );
    module.set_entry(function);
    (module, header)
}

/// One invocation virtualizes the whole region: three window parameters under a recorded group, the slice gone in favour of a guarded extent, the read reaching the base, and the entry opening the rope as its own whole window.
#[test]
fn a_suffix_walk_virtualizes_its_windows() {
    let (mut module, header) = walk_module();
    module.verify().expect("the fixture is well-formed");

    assert!(super::split_windows(&mut module));
    module
        .verify()
        .expect("the window split preserves well-formedness");

    let definition = module.continuation(header).expect("the header survives");
    assert_eq!(definition.params.len(), 3, "one window became three fields");
    assert_eq!(
        module.field_groups().get(&header),
        Some(&vec![FieldGroup { start: 0, width: 3 }]),
        "and the split is recorded",
    );

    let mut slices = 0;
    let mut extents = 0;
    for node in module.nodes().iter().flatten() {
        if let CpsNode::LetIntrinsic { op, .. } = node {
            match op {
                CpsIntrinsicOp::BinSlice(_) => slices += 1,
                CpsIntrinsicOp::WindowExtent => extents += 1,
                _ => {}
            }
        }
    }
    assert_eq!(slices, 0, "no physical view is ever prepared:\n{module}");
    assert_eq!(
        extents, 1,
        "the eager bounds trap stays, as the extent guard"
    );
}

/// A region with one hostile use — the window escaping into a return — is declined whole, the first implementation's documented limit.
#[test]
fn a_window_region_with_a_hostile_use_declines() {
    let (mut module, header) = walk_module();
    // Return the window itself on the default edge instead of the head: an escape through the return interface.
    let escape = module
        .continuation(header)
        .expect("the header is live")
        .params[0];
    let mut hostile = None;
    for (id, node) in module.nodes().iter().enumerate() {
        if let Some(CpsNode::Switch {
            default: Some(_), ..
        }) = node
        {
            hostile = Some(crate::CpsNodeId(id as u32));
        }
    }
    let hostile = hostile.expect("the fixture switches");
    let Some(CpsNode::Switch {
        scrutinee,
        cases,
        default: Some(default),
    }) = module.node(hostile).cloned()
    else {
        unreachable!()
    };
    let mut default = default;
    default.args = vec![CpsAtom::Value(escape)];
    module.nodes.set(
        hostile,
        CpsNode::Switch {
            scrutinee,
            cases,
            default: Some(default),
        },
    );

    assert!(
        !super::split_windows(&mut module),
        "a window returned through the sentinel is not a candidate",
    );
}
