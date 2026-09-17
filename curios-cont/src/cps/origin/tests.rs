use curios_num::Natural;

use {
    super::{Origin, origins},
    crate::cps::test_support::module_with,
    crate::{
        Atom, Callee, Continuation, Edge, Function, Literal, Module, Node, Slot, ValueExpr, ValueId,
    },
    std::collections::BTreeSet,
};

/// A two-field construction jumped into a join point reaches its parameter exactly.
#[test]
fn a_construction_reaches_its_parameter_exactly() {
    let mut built = ValueId(0);
    let mut param = ValueId(0);
    let module = module_with(|module| {
        built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            target,
            Continuation {
                debug_name: Some("target".into()),
                params: vec![param],
                body: exit,
            },
        );
        let jump = module.add_node(Node::ApplyCont(Edge {
            target,
            args: vec![Atom::Value(built)],
        }));
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Tuple(vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ]),
            next: jump,
        });
        module.add_node(Node::LetCont {
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
    let mut built = ValueId(0);
    let mut param = ValueId(0);
    let module = module_with(|module| {
        built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let scrutinee = module.add_value(Some("scrutinee".into()));
        let header = module.reserve_continuation();
        // The backedge: the loop hands its own parameter back to itself.
        let spin = module.add_node(Node::Switch {
            scrutinee: Atom::Value(scrutinee),
            cases: [(
                0,
                Edge {
                    target: header,
                    args: vec![Atom::Value(param)],
                },
            )]
            .into(),
            default: None,
        });
        module.define_continuation(
            header,
            Continuation {
                debug_name: Some("header".into()),
                params: vec![param],
                body: spin,
            },
        );
        let enter = module.add_node(Node::ApplyCont(Edge {
            target: header,
            args: vec![Atom::Value(built)],
        }));
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Tuple(vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ]),
            next: enter,
        });
        module.add_node(Node::LetCont {
            continuations: vec![header],
            body: build,
        })
    });

    let origins = origins(&module);
    assert_eq!(origins[&param], Origin::of_width(2));
}

/// Two constructions of different arities merging at one parameter: the flow is a variant, and the fact carries both widths so the rewrite can travel it at the wider one and fill the narrower edge. This read *replaced* one that answered `Opaque` — merging widths is what the variant-width capability is, and the pair below is the shape of every tagged row whose constructors carry different payload counts.
#[test]
fn merged_arities_travel_as_a_variant() {
    let mut param = ValueId(0);
    let module = module_with(|module| {
        let pair = module.add_value(Some("pair".into()));
        let triple = module.add_value(Some("triple".into()));
        param = module.add_value(Some("param".into()));
        let scrutinee = module.add_value(Some("scrutinee".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            target,
            Continuation {
                debug_name: Some("target".into()),
                params: vec![param],
                body: exit,
            },
        );
        let split = module.add_node(Node::Switch {
            scrutinee: Atom::Value(scrutinee),
            cases: [(
                0,
                Edge {
                    target,
                    args: vec![Atom::Value(pair)],
                },
            )]
            .into(),
            default: Some(Edge {
                target,
                args: vec![Atom::Value(triple)],
            }),
        });
        let build_triple = module.add_node(Node::LetValue {
            result: triple,
            value: ValueExpr::Tuple(vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
                Atom::Literal(Literal::Nat(Natural::from(3u32))),
            ]),
            next: split,
        });
        let build_pair = module.add_node(Node::LetValue {
            result: pair,
            value: ValueExpr::Tuple(vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ]),
            next: build_triple,
        });
        module.add_node(Node::LetCont {
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
    let mut module = Module::default();
    let callee_param = module.add_value(Some("callee/param".into()));
    let callee_built = module.add_value(Some("callee/built".into()));
    let callee = module.reserve_function();
    let callee_ret = module.reserve_continuation();
    let callee_return = module.add_node(Node::ApplyCont(Edge {
        target: callee_ret,
        args: vec![Atom::Value(callee_built)],
    }));
    let callee_body = module.add_node(Node::LetValue {
        result: callee_built,
        value: ValueExpr::Tuple(vec![Atom::Value(callee_param)]),
        next: callee_return,
    });
    module.define_function(
        callee,
        Function {
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
    let exit = module.add_node(Node::Exit { value: None });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![landed],
            body: exit,
        },
    );
    let forward = module.add_node(Node::ApplyCont(Edge {
        target: join,
        args: vec![Atom::Value(received)],
    }));
    module.define_continuation(
        resume,
        Continuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: forward,
        },
    );
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(argument)],
        return_to: resume,
    });
    let body = module.add_node(Node::LetCont {
        continuations: vec![join, resume],
        body: call,
    });
    module.define_function(
        caller,
        Function {
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
        let mut module = Module::default();
        let callee_param = module.add_value(Some("callee/param".into()));
        let callee = module.reserve_function();
        let callee_ret = module.reserve_continuation();
        let callee_exit = module.add_node(Node::Exit { value: None });
        module.define_function(
            callee,
            Function {
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
        let resume_exit = module.add_node(Node::Exit {
            value: escapes.then_some(Atom::Fun(callee)),
        });
        module.define_continuation(
            resume,
            Continuation {
                debug_name: Some("resume".into()),
                params: vec![received],
                body: resume_exit,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(callee),
            args: vec![Atom::Value(built)],
            return_to: resume,
        });
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Tuple(vec![
                Atom::Literal(Literal::Nat(Natural::from(1u32))),
                Atom::Literal(Literal::Nat(Natural::from(2u32))),
            ]),
            next: call,
        });
        let body = module.add_node(Node::LetCont {
            continuations: vec![resume],
            body: build,
        });
        module.define_function(
            caller,
            Function {
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

/// A variant construction reaches its parameter as its own row, settled by construction — the door pads every construction to the row's width, so a variant flow never has the several-width shape a structural variant region does.
#[test]
fn a_variant_construction_carries_its_family() {
    let mut param = ValueId(0);
    let mut row = crate::RowId(0);
    let module = module_with(|module| {
        row = module.add_row(crate::Row {
            debug_name: Some("Shape".into()),
            slots: vec![Slot::Tag, Slot::Opaque, Slot::Opaque],
        });
        let built = module.add_value(Some("built".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            target,
            Continuation {
                debug_name: None,
                params: vec![param],
                body: exit,
            },
        );
        let jump = module.add_node(Node::ApplyCont(Edge {
            target,
            args: vec![Atom::Value(built)],
        }));
        let build = module.add_node(Node::LetValue {
            result: built,
            value: ValueExpr::Row(
                row,
                vec![
                    Atom::Literal(Literal::Nat(Natural::from(0u32))),
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                    Atom::Filler,
                ],
            ),
            next: jump,
        });
        module.add_node(Node::LetCont {
            continuations: vec![target],
            body: build,
        })
    });

    let origin = &origins(&module)[&param];
    assert_eq!(*origin, Origin::Row(row, 3));
    assert_eq!(origin.row(), Some(row));
    assert_eq!(
        origin.settled_width(),
        Some(3),
        "a padded row flow is takeable at its width"
    );
}

/// Two constructors of one row merge to that row at its width — the padding is what makes this a single point rather than the width *set* a structural variant region merges to.
#[test]
fn two_constructors_of_a_family_merge_to_the_family() {
    let mut param = ValueId(0);
    let mut row = crate::RowId(0);
    let module = module_with(|module| {
        row = module.add_row(crate::Row {
            debug_name: Some("Shape".into()),
            slots: vec![Slot::Tag, Slot::Opaque],
        });
        let wide = module.add_value(Some("wide".into()));
        let narrow = module.add_value(Some("narrow".into()));
        param = module.add_value(Some("param".into()));
        let target = module.reserve_continuation();
        let exit = module.add_node(Node::Exit { value: None });
        module.define_continuation(
            target,
            Continuation {
                debug_name: None,
                params: vec![param],
                body: exit,
            },
        );
        // One switch, one edge per constructor, so both constructions flow into the single join.
        let switch = module.add_node(Node::Switch {
            scrutinee: Atom::Literal(Literal::Nat(Natural::from(0u32))),
            cases: [
                (
                    0,
                    Edge {
                        target,
                        args: vec![Atom::Value(wide)],
                    },
                ),
                (
                    1,
                    Edge {
                        target,
                        args: vec![Atom::Value(narrow)],
                    },
                ),
            ]
            .into_iter()
            .collect(),
            default: None,
        });
        let build_narrow = module.add_node(Node::LetValue {
            result: narrow,
            value: ValueExpr::Row(
                row,
                vec![
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                    Atom::Filler,
                ],
            ),
            next: switch,
        });
        let build_wide = module.add_node(Node::LetValue {
            result: wide,
            value: ValueExpr::Row(
                row,
                vec![
                    Atom::Literal(Literal::Nat(Natural::from(0u32))),
                    Atom::Literal(Literal::Nat(Natural::from(7u32))),
                ],
            ),
            next: build_narrow,
        });
        module.add_node(Node::LetCont {
            continuations: vec![target],
            body: build_wide,
        })
    });

    assert_eq!(origins(&module)[&param], Origin::Row(row, 2));
}
