use {
    super::{split_parameters, split_workers},
    crate::{
        CpsAtom, CpsCallee, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction,
        CpsIntrinsic, CpsLiteral, CpsModule, CpsNode, CpsValueExpr, CpsValueId, FieldGroup,
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
        op: CpsIntrinsic::TupleGet(0),
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
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Value(read), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: build,
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: read,
        op: CpsIntrinsic::TupleGet(0),
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
        op: CpsIntrinsic::TupleGet(0),
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

/// The loop-carried *variant*: a one-tuple nullary constructor enters the header and a four-tuple payload constructor circulates through it, so no exact product ever described the parameter. The UTF-8 scan state of `/syn/Str`, in miniature.
fn variant_loop_module() -> (CpsModule, CpsContId, CpsValueId) {
    let mut module = CpsModule::default();
    let narrow = module.add_value(Some("narrow".into()));
    let wide = module.add_value(Some("wide".into()));
    let carried = module.add_value(Some("carried".into()));
    let tag = module.add_value(Some("tag".into()));
    let out = module.add_value(Some("out".into()));
    let result = module.add_value(Some("result".into()));

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
        op: CpsIntrinsic::TupleGet(0),
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

    // header(carried): tag = carried.0; switch tag { 0 => wide = (1, 7, 8, 9); header(wide), _ => exit(carried) }
    let spin = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(tag),
        cases: [(
            0,
            CpsEdge {
                target: header,
                args: vec![CpsAtom::Value(wide)],
            },
        )]
        .into(),
        default: Some(CpsEdge {
            target: exit,
            args: vec![CpsAtom::Value(carried)],
        }),
    });
    let build = module.add_node(CpsNode::LetValue {
        result: wide,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(1)),
            CpsAtom::Literal(CpsLiteral::Nat(7)),
            CpsAtom::Literal(CpsLiteral::Nat(8)),
            CpsAtom::Literal(CpsLiteral::Nat(9)),
        ]),
        next: spin,
    });
    let dispatch = module.add_node(CpsNode::LetIntrinsic {
        result: tag,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(carried)],
        next: build,
    });
    module.define_continuation(
        header,
        CpsContinuation {
            debug_name: Some("header".into()),
            params: vec![carried],
            body: dispatch,
        },
    );

    // main(): narrow = (0); header(narrow)
    let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: header,
        args: vec![CpsAtom::Value(narrow)],
    }));
    let plant = module.add_node(CpsNode::LetValue {
        result: narrow,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Literal(CpsLiteral::Nat(0))]),
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
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(function);
    (module, header, narrow)
}

/// One invocation splits the variant header at the widest constructor: a discriminant slot and three payload slots, the group recorded, and the narrow entry edge carrying its one field followed by filler.
#[test]
fn a_variant_splits_at_its_widest_constructor_with_per_edge_filler() {
    let (mut module, header, narrow) = variant_loop_module();
    module.verify().expect("the fixture is well-formed");

    assert!(split_parameters(&mut module));
    module
        .verify()
        .expect("the split preserves well-formedness");

    assert_eq!(
        module
            .continuation(header)
            .expect("the header survives")
            .params
            .len(),
        4,
        "the region travels as its widest constructor: one discriminant slot and three payload slots",
    );
    assert_eq!(
        module.field_groups().get(&header),
        Some(&vec![FieldGroup { start: 0, width: 4 }]),
    );

    // The entry edge reads the one field its own construction carries, and fills the other three.
    let entry = module
        .nodes()
        .iter()
        .flatten()
        .find_map(|node| match node {
            CpsNode::ApplyCont(edge) if edge.target == header => Some(edge.args.clone()),
            _ => None,
        })
        .expect("the entry edge survives");
    assert_eq!(entry.len(), 4, "the edge carries the region's width");
    assert!(
        matches!(entry[0], CpsAtom::Value(_)),
        "slot zero is the narrow constructor's own field: {entry:?}",
    );
    assert!(
        entry[1..]
            .iter()
            .all(|atom| matches!(atom, CpsAtom::Filler)),
        "and the slots it does not carry are filler: {entry:?}",
    );

    // Nothing projects past what the narrow construction holds — the reads inserted above the jump are exactly its own.
    let past = module
        .nodes()
        .iter()
        .flatten()
        .filter(|node| {
            matches!(
                node,
                CpsNode::LetIntrinsic { op: CpsIntrinsic::TupleGet(index), args, .. }
                    if *index >= 1 && matches!(args.as_slice(), [CpsAtom::Value(value)] if *value == narrow)
            )
        })
        .count();
    assert_eq!(
        past, 0,
        "no read reaches past the narrow constructor:\n{module}"
    );
}

/// The full chain erases the variant: after optimization no tuple construction survives at either width, the discriminant having become a parameter the switch reads directly.
#[test]
fn a_loop_carried_variant_erases_through_the_chain() {
    let (mut module, ..) = variant_loop_module();
    optimize(&mut module);

    let survivors = module
        .nodes()
        .iter()
        .flatten()
        .filter(|node| {
            matches!(
                node,
                CpsNode::LetValue {
                    value: CpsValueExpr::Tuple(_),
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        survivors, 0,
        "the variant travels as fields on every path:\n{module}",
    );
}

/// A known function whose variant argument is *itself* a merged flow — `Handle/Read`'s three constructors joining at a `choose` before the call, which is where this fixture comes from.
fn merged_argument_module() -> (CpsModule, CpsFunId, CpsValueId) {
    let mut module = CpsModule::default();
    let callee_param = module.add_value(Some("callee/param".into()));
    let callee_read = module.add_value(Some("callee/read".into()));
    let callee = module.reserve_function();
    let callee_ret = module.reserve_continuation();

    // callee(r): x = r.0; return x
    let callee_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: callee_ret,
        args: vec![CpsAtom::Value(callee_read)],
    }));
    let callee_body = module.add_node(CpsNode::LetIntrinsic {
        result: callee_read,
        op: CpsIntrinsic::TupleGet(0),
        args: vec![CpsAtom::Value(callee_param)],
        next: callee_return,
    });
    module.define_function(
        callee,
        CpsFunction {
            debug_name: Some("callee".into()),
            params: vec![callee_param],
            return_cont: callee_ret,
            body: callee_body,
        },
    );

    let narrow = module.add_value(Some("narrow".into()));
    let wide = module.add_value(Some("wide".into()));
    let merged = module.add_value(Some("merged".into()));
    let received = module.add_value(Some("received".into()));
    let scrutinee = module.add_value(Some("scrutinee".into()));
    let caller = module.reserve_function();
    let caller_ret = module.reserve_continuation();
    let join = module.reserve_continuation();
    let resume = module.reserve_continuation();

    let done = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: caller_ret,
        args: vec![CpsAtom::Value(received)],
    }));
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: done,
        },
    );

    // join(merged): callee(merged) -> resume
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(callee),
        args: vec![CpsAtom::Value(merged)],
        return_to: resume,
    });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: Some("join".into()),
            params: vec![merged],
            body: call,
        },
    );

    // caller(scrutinee): narrow = (1); wide = (0, 5); switch scrutinee { 0 => join(wide), _ => join(narrow) }
    let pick = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(scrutinee),
        cases: [(
            0,
            CpsEdge {
                target: join,
                args: vec![CpsAtom::Value(wide)],
            },
        )]
        .into(),
        default: Some(CpsEdge {
            target: join,
            args: vec![CpsAtom::Value(narrow)],
        }),
    });
    let build_wide = module.add_node(CpsNode::LetValue {
        result: wide,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(5)),
        ]),
        next: pick,
    });
    let build_narrow = module.add_node(CpsNode::LetValue {
        result: narrow,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Literal(CpsLiteral::Nat(1))]),
        next: build_wide,
    });
    let scope = module.add_node(CpsNode::LetCont {
        continuations: vec![join, resume],
        body: build_narrow,
    });
    let body = module.add_node(CpsNode::LetFun {
        functions: vec![callee],
        body: scope,
    });
    module.define_function(
        caller,
        CpsFunction {
            debug_name: Some("caller".into()),
            params: vec![scrutinee],
            return_cont: caller_ret,
            body,
        },
    );
    module.set_entry(caller);
    (module, callee, callee_param)
}

/// Every projection in `module` that reads a visible construction, paired with that construction's arity. A read past the arity is the miscompile a site-blind width would produce: `$tuple/n` extends `$tuple/(n-1)`, so the emitter casts a projection's operand to the tuple type of `index + 1` and a narrower object fails that cast at runtime rather than at build time.
fn projections_within_bounds(module: &CpsModule) -> bool {
    let built = module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            CpsNode::LetValue {
                result,
                value: CpsValueExpr::Tuple(atoms),
                ..
            } => Some((*result, atoms.len())),
            _ => None,
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    module.nodes().iter().flatten().all(|node| match node {
        CpsNode::LetIntrinsic {
            op: CpsIntrinsic::TupleGet(index),
            args,
            ..
        } => match args.as_slice() {
            [CpsAtom::Value(value)] => built.get(value).is_none_or(|arity| *index < *arity),
            _ => true,
        },
        _ => true,
    })
}

/// A call site whose argument merges two widths cannot be taken apart there, because no fixed number of projections is right on both paths — and the narrow one would be read past its end.
///
/// **This is a regression fixture with a runtime failure behind it.** Splitting at the region's widest width and projecting every edge at that width compiled cleanly, verified cleanly, and trapped `programs`-level `Handle/Read` handling at run time, where `eof()` rides a one-tuple beside `chunk(b)`'s two. The decline is not permanent: the merging join is a region of its own, and once it is split the call's argument is a materialization of one settled width.
#[test]
fn a_call_whose_argument_merges_widths_is_declined_until_the_merge_is_split() {
    let (mut module, callee, param) = merged_argument_module();
    module.verify().expect("the fixture is well-formed");
    assert!(
        !split_workers(&mut module),
        "the argument's own flow merges a one-tuple and a two-tuple, so the call site cannot say how many fields to read",
    );

    // Splitting the merging join first settles the argument, and the worker split then follows.
    assert!(split_parameters(&mut module));
    assert!(
        split_workers(&mut module),
        "with the merge materialized at one width, the callee's parameter travels as fields",
    );
    assert_eq!(
        module
            .function(callee)
            .expect("the callee survives")
            .params
            .len(),
        2,
    );
    assert!(
        !module
            .function(callee)
            .expect("the callee survives")
            .params
            .contains(&param),
    );
    module.verify().expect("both splits preserve the module");
    assert!(
        projections_within_bounds(&module),
        "no projection reads past the construction it is applied to:\n{module}",
    );
}

/// The whole chain over the same fixture: it optimizes, it verifies, and nothing reads past a construction it can see.
#[test]
fn a_merged_argument_optimizes_without_reading_past_a_construction() {
    let (mut module, ..) = merged_argument_module();
    optimize(&mut module);
    module.verify().expect("the optimized module verifies");
    assert!(
        projections_within_bounds(&module),
        "no projection reads past the construction it is applied to:\n{module}",
    );
}

/// The verifier holds the record to the parameter list: a group past the parameters is an invariant break, not a curiosity.
#[test]
fn the_verifier_rejects_a_stale_record() {
    let (mut module, _, header, _) = loop_module();
    module.record_split(header, 0, 9);
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
    // A *suffix*, which is what `into_cont`'s peel emits: no count operand, so the fixture exercises the shape the compiler actually produces rather than one it no longer can.
    let slice = module.add_node(CpsNode::LetIntrinsic {
        result: tail,
        op: CpsIntrinsic::BinRest(curios_utilities::Grain::X),
        args: vec![CpsAtom::Value(window), CpsAtom::Literal(CpsLiteral::Nat(1))],
        next: spin,
    });
    let read = module.add_node(CpsNode::LetIntrinsic {
        result: head,
        op: CpsIntrinsic::BinGet(curios_utilities::Grain::X),
        args: vec![CpsAtom::Value(window), CpsAtom::Literal(CpsLiteral::Nat(0))],
        next: slice,
    });
    let measure = module.add_node(CpsNode::LetIntrinsic {
        result: length,
        op: CpsIntrinsic::BinLen(curios_utilities::Grain::X),
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

    // Counted over *every* window-producing shape, not just the one this fixture happens to build: an assertion that a form has vanished passes vacuously the moment the lowering stops emitting that form, which is the inertness this codebase keeps recording.
    let mut slices = 0;
    let mut extents = 0;
    for node in module.nodes().iter().flatten() {
        if let CpsNode::LetIntrinsic { op, .. } = node {
            match op {
                CpsIntrinsic::BinSlice(_)
                | CpsIntrinsic::BinRest(_)
                | CpsIntrinsic::ListSlice
                | CpsIntrinsic::ListRest => slices += 1,
                CpsIntrinsic::WindowExtent => extents += 1,
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
