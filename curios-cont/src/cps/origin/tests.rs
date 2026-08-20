use {
    super::{Origin, origins},
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunction, CpsLiteral, CpsModule, CpsNode,
        CpsSlot, CpsValueExpr, CpsValueId,
    },
    std::collections::BTreeSet,
};

/// One function whose body is `body_of(&mut module)`, so each test states only the flow it is about.
fn module_with(body_of: impl FnOnce(&mut CpsModule) -> crate::CpsNodeId) -> CpsModule {
    let mut module = CpsModule::default();
    let body = body_of(&mut module);
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
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
    module
}

/// A two-field construction jumped into a join point reaches its parameter exactly.
#[test]
fn a_construction_reaches_its_parameter_exactly() {
    let mut built = CpsValueId(0);
    let mut param = CpsValueId(0);
    let module = module_with(|module| {
        built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(CpsNode::Exit { value: None });
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: Some("target".into()),
                params: vec![param],
                body: exit,
            },
        );
        let jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target,
            args: vec![CpsAtom::Value(built)],
        }));
        let build = module.add_node(CpsNode::LetValue {
            result: built,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ]),
            next: jump,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![target],
            body: build,
        })
    });

    let origins = origins(&module);
    assert_eq!(origins[&built], Origin::of_width(2));
    assert_eq!(origins[&param], Origin::of_width(2));
}

/// The loop the specification exists for: one edge enters the join with a construction and the backedge passes the join's own parameter back unchanged. The alias contributes the parameter's own fact, so the region stays exact rather than demanding a construction on every edge.
#[test]
fn a_loop_alias_edge_keeps_the_region_exact() {
    let mut built = CpsValueId(0);
    let mut param = CpsValueId(0);
    let module = module_with(|module| {
        built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let scrutinee = module.add_value(Some("scrutinee".into()));
        let header = module.reserve_continuation();
        // The backedge: the loop hands its own parameter back to itself.
        let spin = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(scrutinee),
            cases: [(
                0,
                CpsEdge {
                    target: header,
                    args: vec![CpsAtom::Value(param)],
                },
            )]
            .into(),
            default: None,
        });
        module.define_continuation(
            header,
            CpsContinuation {
                debug_name: Some("header".into()),
                params: vec![param],
                body: spin,
            },
        );
        let enter = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: header,
            args: vec![CpsAtom::Value(built)],
        }));
        let build = module.add_node(CpsNode::LetValue {
            result: built,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ]),
            next: enter,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![header],
            body: build,
        })
    });

    let origins = origins(&module);
    assert_eq!(origins[&param], Origin::of_width(2));
}

/// Two constructions of different arities merging at one parameter: the flow is a variant, and the fact carries both widths so the rewrite can travel it at the wider one and fill the narrower edge. This read *replaced* one that answered `Opaque` — merging widths is what the variant-width capability is, and the pair below is the shape of every tagged family whose constructors carry different payload counts.
#[test]
fn merged_arities_travel_as_a_variant() {
    let mut param = CpsValueId(0);
    let module = module_with(|module| {
        let pair = module.add_value(Some("pair".into()));
        let triple = module.add_value(Some("triple".into()));
        param = module.add_value(Some("param".into()));
        let scrutinee = module.add_value(Some("scrutinee".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(CpsNode::Exit { value: None });
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: Some("target".into()),
                params: vec![param],
                body: exit,
            },
        );
        let split = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Value(scrutinee),
            cases: [(
                0,
                CpsEdge {
                    target,
                    args: vec![CpsAtom::Value(pair)],
                },
            )]
            .into(),
            default: Some(CpsEdge {
                target,
                args: vec![CpsAtom::Value(triple)],
            }),
        });
        let build_triple = module.add_node(CpsNode::LetValue {
            result: triple,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
                CpsAtom::Literal(CpsLiteral::Nat(3)),
            ]),
            next: split,
        });
        let build_pair = module.add_node(CpsNode::LetValue {
            result: pair,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ]),
            next: build_triple,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![target],
            body: build_pair,
        })
    });

    assert_eq!(
        origins(&module)[&param],
        Origin::Constructed(BTreeSet::from([2, 3]))
    );
}

/// A resume parameter receives whatever an unsplit return interface delivers, and forwarding it poisons the join it lands in.
#[test]
fn a_call_result_is_opaque_and_poisons_what_it_reaches() {
    let mut module = CpsModule::default();
    let callee_param = module.add_value(Some("callee/param".into()));
    let callee_built = module.add_value(Some("callee/built".into()));
    let callee = module.reserve_function();
    let callee_ret = module.reserve_continuation();
    let callee_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: callee_ret,
        args: vec![CpsAtom::Value(callee_built)],
    }));
    let callee_body = module.add_node(CpsNode::LetValue {
        result: callee_built,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(callee_param)]),
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

    let received = module.add_value(Some("received".into()));
    let landed = module.add_value(Some("landed".into()));
    let argument = module.add_value(Some("argument".into()));
    let caller = module.reserve_function();
    let caller_ret = module.reserve_continuation();
    let resume = module.reserve_continuation();
    let join = module.reserve_continuation();
    let exit = module.add_node(CpsNode::Exit { value: None });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: Some("join".into()),
            params: vec![landed],
            body: exit,
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
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![join, resume],
        body: call,
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

    let origins = origins(&module);
    assert_eq!(origins[&received], Origin::Opaque);
    assert_eq!(origins[&landed], Origin::Opaque);
}

/// A known call's argument reaches the callee's parameter; an escaping callee's parameters read opaque instead, because unknown callers reach them too.
#[test]
fn a_known_call_argument_reaches_the_callee_parameter_unless_it_escapes() {
    for escapes in [false, true] {
        let mut module = CpsModule::default();
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

        let built = module.add_value(Some("built".into()));
        let caller = module.reserve_function();
        let caller_ret = module.reserve_continuation();
        let resume = module.reserve_continuation();
        let received = module.add_value(Some("received".into()));
        let resume_exit = module.add_node(CpsNode::Exit {
            value: escapes.then_some(CpsAtom::Fun(callee)),
        });
        module.define_continuation(
            resume,
            CpsContinuation {
                debug_name: Some("resume".into()),
                params: vec![received],
                body: resume_exit,
            },
        );
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args: vec![CpsAtom::Value(built)],
            return_to: resume,
        });
        let build = module.add_node(CpsNode::LetValue {
            result: built,
            value: CpsValueExpr::Tuple(vec![
                CpsAtom::Literal(CpsLiteral::Nat(1)),
                CpsAtom::Literal(CpsLiteral::Nat(2)),
            ]),
            next: call,
        });
        let body = module.add_node(CpsNode::LetCont {
            continuations: vec![resume],
            body: build,
        });
        module.define_function(
            caller,
            CpsFunction {
                debug_name: Some("caller".into()),
                params: vec![],
                return_cont: caller_ret,
                body,
            },
        );
        module.set_entry(caller);

        let expected = if escapes {
            Origin::Opaque
        } else {
            Origin::of_width(2)
        };
        assert_eq!(
            origins(&module)[&callee_param],
            expected,
            "escapes: {escapes}"
        );
    }
}

/// A variant construction reaches its parameter as its own family, settled by construction — the door pads every construction to the family's width, so a variant flow never has the several-width shape a structural variant region does.
#[test]
fn a_variant_construction_carries_its_family() {
    let mut param = CpsValueId(0);
    let mut family = crate::CpsFamilyId(0);
    let module = module_with(|module| {
        family = module.add_family(crate::CpsFamily {
            debug_name: Some("Shape".into()),
            slots: vec![CpsSlot::Tag, CpsSlot::Opaque, CpsSlot::Opaque],
        });
        let built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(CpsNode::Exit { value: None });
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: None,
                params: vec![param],
                body: exit,
            },
        );
        let jump = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target,
            args: vec![CpsAtom::Value(built)],
        }));
        let build = module.add_node(CpsNode::LetValue {
            result: built,
            value: CpsValueExpr::Variant(
                family,
                vec![
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                    CpsAtom::Literal(CpsLiteral::Nat(1)),
                    CpsAtom::Filler,
                ],
            ),
            next: jump,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![target],
            body: build,
        })
    });

    let origin = &origins(&module)[&param];
    assert_eq!(*origin, Origin::Variant(family, 3));
    assert_eq!(origin.family(), Some(family));
    assert_eq!(
        origin.settled_width(),
        Some(3),
        "a padded family flow is takeable at its width"
    );
}

/// Two constructors of one family merge to that family at its width — the padding is what makes this a single point rather than the width *set* a structural variant region merges to.
#[test]
fn two_constructors_of_a_family_merge_to_the_family() {
    let mut param = CpsValueId(0);
    let mut family = crate::CpsFamilyId(0);
    let module = module_with(|module| {
        family = module.add_family(crate::CpsFamily {
            debug_name: Some("Shape".into()),
            slots: vec![CpsSlot::Tag, CpsSlot::Opaque],
        });
        let wide = module.add_value(Some("wide".into()));
        let narrow = module.add_value(Some("narrow".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(CpsNode::Exit { value: None });
        module.define_continuation(
            target,
            CpsContinuation {
                debug_name: None,
                params: vec![param],
                body: exit,
            },
        );
        // One switch, one edge per constructor, so both constructions flow into the single join.
        let switch = module.add_node(CpsNode::Switch {
            scrutinee: CpsAtom::Literal(CpsLiteral::Nat(0)),
            cases: [
                (
                    0,
                    CpsEdge {
                        target,
                        args: vec![CpsAtom::Value(wide)],
                    },
                ),
                (
                    1,
                    CpsEdge {
                        target,
                        args: vec![CpsAtom::Value(narrow)],
                    },
                ),
            ]
            .into_iter()
            .collect(),
            default: None,
        });
        let build_narrow = module.add_node(CpsNode::LetValue {
            result: narrow,
            value: CpsValueExpr::Variant(
                family,
                vec![CpsAtom::Literal(CpsLiteral::Nat(1)), CpsAtom::Filler],
            ),
            next: switch,
        });
        let build_wide = module.add_node(CpsNode::LetValue {
            result: wide,
            value: CpsValueExpr::Variant(
                family,
                vec![
                    CpsAtom::Literal(CpsLiteral::Nat(0)),
                    CpsAtom::Literal(CpsLiteral::Nat(7)),
                ],
            ),
            next: build_narrow,
        });
        module.add_node(CpsNode::LetCont {
            continuations: vec![target],
            body: build_wide,
        })
    });

    assert_eq!(origins(&module)[&param], Origin::Variant(family, 2));
}
