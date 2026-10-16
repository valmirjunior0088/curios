//! Entry capability follows returned rows, including recursive and opaque flows, without changing the representation vocabulary.

use {
    crate::{
        Atom, Callee, CellOp, ChannelOp, Continuation, ContinuationId, Edge, Function, FunctionId,
        Intrinsic, Literal, Module, Node, NodeId, RowId, ValueExpr, ValueId,
    },
    curios_num::Natural,
    std::collections::BTreeMap,
};

fn nat(n: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(n)))
}

fn returning(module: &mut Module, target: ContinuationId, value: Atom) -> NodeId {
    module.add_node(Node::ApplyCont(Edge {
        target,
        args: vec![value],
    }))
}

fn entry_with(
    params: usize,
    body_of: impl FnOnce(&mut Module, &[ValueId], ContinuationId) -> NodeId,
) -> Module {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let ret = module.reserve_continuation();
    let params = (0..params)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    let body = body_of(&mut module, &params, ret);
    module.define_function(
        entry,
        Function {
            debug_name: None,
            params,
            return_cont: ret,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);
    module.verify().expect("fixture is valid Cont");
    module
}

fn define(
    module: &mut Module,
    body_of: impl FnOnce(&mut Module, ContinuationId) -> NodeId,
) -> FunctionId {
    let function = module.reserve_function();
    let ret = module.reserve_continuation();
    let body = body_of(module, ret);
    module.define_function(
        function,
        Function {
            debug_name: None,
            params: vec![],
            return_cont: ret,
            body,
            droppable: false,
        },
    );
    function
}

fn bind_functions(module: &mut Module, functions: Vec<FunctionId>, body: NodeId) -> NodeId {
    module.add_node(Node::LetFun { functions, body })
}

fn row_return(module: &mut Module, row: RowId, ret: ContinuationId) -> NodeId {
    let result = module.add_value(None);
    let next = returning(module, ret, Atom::Value(result));
    module.add_node(Node::LetValue {
        result,
        value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
        next,
    })
}

#[test]
fn registration_alone_does_not_require_turns_and_queries_follow_mutation() {
    let mut module = entry_with(0, |module, _, ret| {
        module.suspension_row();
        returning(module, ret, nat(0))
    });
    let row = module.suspension().unwrap();
    assert_eq!(module.entry_suspension(), None);
    let entry = module.entry().unwrap();
    let ret = module.function(entry).unwrap().return_cont;
    let suspended = row_return(&mut module, row, ret);
    module.function_mut(entry).unwrap().body = suspended;
    crate::prune_unreachable(&mut module);
    module.verify().unwrap();
    assert_eq!(module.entry_suspension(), Some(row));
    let immediate = returning(&mut module, ret, nat(0));
    module.function_mut(entry).unwrap().body = immediate;
    crate::prune_unreachable(&mut module);
    module.verify().unwrap();
    assert_eq!(module.entry_suspension(), None);
    assert_eq!(module.suspension(), Some(row));
}

#[test]
fn no_entry_or_no_registered_suspension_needs_no_turns() {
    let mut empty = Module::new();
    empty.suspension_row();
    assert_eq!(empty.entry_suspension(), None);
    let immediate = entry_with(0, |module, _, ret| returning(module, ret, nat(0)));
    assert_eq!(immediate.entry_suspension(), None);
}

#[test]
fn direct_and_tail_calls_deliver_their_suspension_results() {
    for tail in [false, true] {
        let module = entry_with(0, |module, _, ret| {
            let row = module.suspension_row();
            let producer = define(module, |module, ret| row_return(module, row, ret));
            let called = if tail {
                define(module, |module, ret| {
                    module.add_node(Node::ApplyFun {
                        callee: Callee::Known(producer),
                        args: vec![],
                        return_to: ret,
                    })
                })
            } else {
                producer
            };
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(called),
                args: vec![],
                return_to: ret,
            });
            let functions = if tail {
                vec![producer, called]
            } else {
                vec![producer]
            };
            bind_functions(module, functions, call)
        });
        assert_eq!(
            module.entry_suspension(),
            module.suspension(),
            "tail: {tail}"
        );
    }
}

#[test]
fn a_suspension_consumed_by_an_internal_resume_does_not_escape() {
    let module = entry_with(0, |module, _, ret| {
        let row = module.suspension_row();
        let producer = define(module, |module, ret| row_return(module, row, ret));
        let received = module.add_value(None);
        let resume = module.reserve_continuation();
        let body = returning(module, ret, nat(0));
        module.define_continuation(
            resume,
            Continuation {
                debug_name: None,
                params: vec![received],
                body,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(producer),
            args: vec![],
            return_to: resume,
        });
        let scope = module.add_node(Node::LetCont {
            continuations: vec![resume],
            body: call,
        });
        bind_functions(module, vec![producer], scope)
    });
    assert!(module.suspension().is_some());
    assert_eq!(module.entry_suspension(), None);
}

#[test]
fn split_results_keep_their_return_positions_distinct() {
    for kept in [0, 1] {
        let module = entry_with(0, |module, _, ret| {
            let row = module.suspension_row();
            let producer = define(module, |module, ret| {
                let result = module.add_value(None);
                let next = module.add_node(Node::ApplyCont(Edge {
                    target: ret,
                    args: vec![nat(0), Atom::Value(result)],
                }));
                module.add_node(Node::LetValue {
                    result,
                    value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
                    next,
                })
            });
            let params = vec![module.add_value(None), module.add_value(None)];
            let resume = module.reserve_continuation();
            let body = returning(module, ret, Atom::Value(params[kept]));
            module.define_continuation(
                resume,
                Continuation {
                    debug_name: None,
                    params,
                    body,
                },
            );
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(producer),
                args: vec![],
                return_to: resume,
            });
            let scope = module.add_node(Node::LetCont {
                continuations: vec![resume],
                body: call,
            });
            bind_functions(module, vec![producer], scope)
        });
        let expected = if kept == 1 { module.suspension() } else { None };
        assert_eq!(module.entry_suspension(), expected, "kept: {kept}");
    }
}

#[test]
fn a_recursive_component_propagates_its_returned_row() {
    let module = entry_with(0, |module, _, ret| {
        let row = module.suspension_row();
        let first = module.reserve_function();
        let second = module.reserve_function();
        let first_ret = module.reserve_continuation();
        let second_ret = module.reserve_continuation();
        let again = module.reserve_continuation();
        let tail = module.add_node(Node::ApplyFun {
            callee: Callee::Known(second),
            args: vec![],
            return_to: first_ret,
        });
        module.define_continuation(
            again,
            Continuation {
                debug_name: None,
                params: vec![],
                body: tail,
            },
        );
        let result = module.add_value(None);
        let choice = module.add_node(Node::Switch {
            scrutinee: nat(0),
            cases: BTreeMap::from([(
                0,
                Edge {
                    target: first_ret,
                    args: vec![Atom::Value(result)],
                },
            )]),
            default: Some(Edge {
                target: again,
                args: vec![],
            }),
        });
        let choice = module.add_node(Node::LetValue {
            result,
            value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
            next: choice,
        });
        let body = module.add_node(Node::LetCont {
            continuations: vec![again],
            body: choice,
        });
        module.define_function(
            first,
            Function {
                debug_name: None,
                params: vec![],
                return_cont: first_ret,
                body,
                droppable: false,
            },
        );
        let body = module.add_node(Node::ApplyFun {
            callee: Callee::Known(first),
            args: vec![],
            return_to: second_ret,
        });
        module.define_function(
            second,
            Function {
                debug_name: None,
                params: vec![],
                return_cont: second_ret,
                body,
                droppable: false,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(second),
            args: vec![],
            return_to: ret,
        });
        bind_functions(module, vec![first, second], call)
    });
    assert_eq!(module.entry_suspension(), module.suspension());
}

#[test]
fn a_recursive_component_that_never_returns_contributes_no_row() {
    let module = entry_with(0, |module, _, ret| {
        module.suspension_row();
        let function = module.reserve_function();
        let sentinel = module.reserve_continuation();
        let body = module.add_node(Node::ApplyFun {
            callee: Callee::Known(function),
            args: vec![],
            return_to: sentinel,
        });
        module.define_function(
            function,
            Function {
                debug_name: None,
                params: vec![],
                return_cont: sentinel,
                body,
                droppable: false,
            },
        );
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(function),
            args: vec![],
            return_to: ret,
        });
        bind_functions(module, vec![function], call)
    });
    assert_eq!(module.entry_suspension(), None);
}

#[test]
fn one_suspending_branch_is_enough_to_require_turns() {
    let module = entry_with(1, |module, params, ret| {
        let row = module.suspension_row();
        let value = module.add_value(None);
        let choice = module.add_node(Node::Switch {
            scrutinee: Atom::Value(params[0]),
            cases: BTreeMap::from([(
                0,
                Edge {
                    target: ret,
                    args: vec![nat(0)],
                },
            )]),
            default: Some(Edge {
                target: ret,
                args: vec![Atom::Value(value)],
            }),
        });
        module.add_node(Node::LetValue {
            result: value,
            value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
            next: choice,
        })
    });
    assert_eq!(module.entry_suspension(), module.suspension());
}

#[test]
fn opaque_results_retain_turns_through_direct_and_tail_returns() {
    for boundary in 0..4 {
        for tail in [false, true] {
            let module = entry_with(1, |module, params, ret| {
                module.suspension_row();
                let result = module.add_value(None);
                let resume = module.reserve_continuation();
                let body = returning(module, ret, Atom::Value(result));
                let params_of_resume = if boundary >= 2 {
                    vec![module.add_value(None), result]
                } else {
                    vec![result]
                };
                module.define_continuation(
                    resume,
                    Continuation {
                        debug_name: None,
                        params: params_of_resume,
                        body,
                    },
                );
                let target = if tail && boundary < 2 { ret } else { resume };
                let operation = match boundary {
                    0 => module.add_node(Node::ApplyFun {
                        callee: Callee::Closure(params[0]),
                        args: vec![],
                        return_to: target,
                    }),
                    1 => {
                        let projected = module.add_value(None);
                        let next = if tail {
                            returning(module, ret, Atom::Value(projected))
                        } else {
                            module.add_node(Node::ApplyCont(Edge {
                                target: resume,
                                args: vec![Atom::Value(projected)],
                            }))
                        };
                        module.add_node(Node::LetIntrinsic {
                            result: projected,
                            op: Intrinsic::TupleGet(0),
                            args: vec![Atom::Value(params[0])],
                            next,
                        })
                    }
                    2 => module.add_node(Node::Cell {
                        op: CellOp::Poll,
                        args: vec![Atom::Value(params[0])],
                        return_to: resume,
                    }),
                    _ => module.add_node(Node::Channel {
                        op: ChannelOp::Take,
                        args: vec![Atom::Value(params[0])],
                        return_to: resume,
                    }),
                };
                module.add_node(Node::LetCont {
                    continuations: vec![resume],
                    body: operation,
                })
            });
            assert_eq!(
                module.entry_suspension(),
                module.suspension(),
                "boundary: {boundary}, tail: {tail}"
            );
        }
    }
}

#[test]
fn escaping_parameters_are_unknown_even_at_a_known_call() {
    let module = entry_with(0, |module, _, ret| {
        module.suspension_row();
        let function = module.reserve_function();
        let sentinel = module.reserve_continuation();
        let param = module.add_value(None);
        let body = returning(module, sentinel, Atom::Value(param));
        module.define_function(
            function,
            Function {
                debug_name: None,
                params: vec![param],
                return_cont: sentinel,
                body,
                droppable: false,
            },
        );
        let escaped = module.add_value(None);
        let call = module.add_node(Node::ApplyFun {
            callee: Callee::Known(function),
            args: vec![nat(0)],
            return_to: ret,
        });
        let body = module.add_node(Node::LetValue {
            result: escaped,
            value: ValueExpr::List(vec![Atom::Fun(function)]),
            next: call,
        });
        bind_functions(module, vec![function], body)
    });
    assert_eq!(module.entry_suspension(), module.suspension());
}

#[test]
fn mutable_payloads_remain_unknown_through_a_helpers_tail_return() {
    for channel in [false, true] {
        let module = entry_with(1, |module, params, ret| {
            module.suspension_row();
            let worker = define(module, |module, sentinel| {
                if channel {
                    module.add_node(Node::Channel {
                        op: ChannelOp::Take,
                        args: vec![Atom::Value(params[0])],
                        return_to: sentinel,
                    })
                } else {
                    module.add_node(Node::Cell {
                        op: CellOp::Poll,
                        args: vec![Atom::Value(params[0])],
                        return_to: sentinel,
                    })
                }
            });
            let status = module.add_value(None);
            let payload = module.add_value(None);
            let resume = module.reserve_continuation();
            let body = returning(module, ret, Atom::Value(payload));
            module.define_continuation(
                resume,
                Continuation {
                    debug_name: None,
                    params: vec![status, payload],
                    body,
                },
            );
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(worker),
                args: vec![],
                return_to: resume,
            });
            let body = module.add_node(Node::LetCont {
                continuations: vec![resume],
                body: call,
            });
            bind_functions(module, vec![worker], body)
        });
        assert_eq!(
            module.entry_suspension(),
            module.suspension(),
            "channel: {channel}"
        );
    }
}

#[test]
fn foreign_wire_results_cannot_fabricate_a_nominal_suspension() {
    let module = entry_with(0, |module, _, ret| {
        module.suspension_row();
        let value = module.add_value(None);
        let lane = module.add_value(None);
        let resume = module.reserve_continuation();
        let body = returning(module, ret, Atom::Value(value));
        module.define_continuation(
            resume,
            Continuation {
                debug_name: None,
                params: vec![value, lane],
                body,
            },
        );
        let call = module.add_node(Node::Foreign {
            function: std::sync::Arc::new(curios_abi::ForeignFunction {
                name: "/wire".into(),
                signature: curios_abi::WireSignature {
                    params: vec![],
                    returns: curios_abi::WireReturn::Answers(curios_abi::WireResults::single(
                        curios_abi::WireType::Nat,
                    )),
                },
            }),
            args: vec![Atom::Filler, nat(0)],
            return_to: resume,
        });
        module.add_node(Node::LetCont {
            continuations: vec![resume],
            body: call,
        })
    });
    assert_eq!(module.entry_suspension(), None);
}
