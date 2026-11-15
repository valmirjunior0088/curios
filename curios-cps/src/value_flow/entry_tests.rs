//! Entry capability follows returned rows, through recursive flows and through what the module does not see, without changing the representation vocabulary.

use {crate::*, curios_num::Natural, std::collections::BTreeMap};

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
            droppable: Droppable::Never,
        },
    );
    module.set_entry(entry);
    module.verify().expect("fixture is valid CPS");
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
            droppable: Droppable::Never,
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
                droppable: Droppable::Never,
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
                droppable: Droppable::Never,
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
                droppable: Droppable::Never,
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

/// A nullary function answering a suspension, named in a list ahead of `next`, so a callee the module does not see may be it.
fn named_suspending(module: &mut Module, row: RowId, next: NodeId) -> NodeId {
    let producer = define(module, |module, ret| row_return(module, row, ret));
    let named = module.add_value(None);
    let body = module.add_node(Node::LetValue {
        result: named,
        value: ValueExpr::List(vec![Atom::Fun(producer)]),
        next,
    });
    bind_functions(module, vec![producer], body)
}

/// A suspension built ahead of `next` and held in a tuple's field.
fn tupled_suspension(module: &mut Module, row: RowId, next: NodeId) -> NodeId {
    let suspension = module.add_value(None);
    let tuple = module.add_value(None);
    let tupled = module.add_node(Node::LetValue {
        result: tuple,
        value: ValueExpr::Tuple(vec![Atom::Value(suspension)]),
        next,
    });
    module.add_node(Node::LetValue {
        result: suspension,
        value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
        next: tupled,
    })
}

/// A suspension built ahead of `next` and filled into `into`, a cell, or pushed onto it, a channel.
fn stored_suspension(
    module: &mut Module,
    row: RowId,
    into: ValueId,
    channel: bool,
    next: NodeId,
) -> NodeId {
    let suspension = module.add_value(None);
    let outcome = module.add_value(None);
    let after = module.add_continuation(Continuation {
        debug_name: None,
        params: vec![outcome],
        body: next,
    });
    let args = vec![Atom::Value(into), Atom::Value(suspension)];
    let store = match channel {
        true => module.add_node(Node::Channel {
            op: ChannelOp::Push,
            args,
            return_to: after,
        }),
        false => module.add_node(Node::Cell {
            op: CellOp::Fill,
            args,
            return_to: after,
        }),
    };
    let store = module.add_node(Node::LetCont {
        continuations: vec![after],
        body: store,
    });
    module.add_node(Node::LetValue {
        result: suspension,
        value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
        next: store,
    })
}

/// What the entry is handed may be any function some atom names, and any construction, cell or channel content of its kind the module builds or stores: read through a closure call, a field, a cell or a channel, it carries the turns where the module holds a suspension there, and none where it holds none.
#[test]
fn what_the_entry_is_handed_carries_the_turns_the_module_holds_where_it_is_read() {
    for boundary in 0..4 {
        for tail in [false, true] {
            for holds in [false, true] {
                let module = entry_with(1, |module, params, ret| {
                    let row = module.suspension_row();
                    let read = read_handed(module, boundary, tail, params[0], ret);
                    match (holds, boundary) {
                        (false, _) => read,
                        (true, 0) => named_suspending(module, row, read),
                        (true, 1) => tupled_suspension(module, row, read),
                        (true, _) => stored_suspension(module, row, params[0], boundary == 3, read),
                    }
                });
                assert_eq!(
                    module.entry_suspension(),
                    module.suspension().filter(|_| holds),
                    "boundary: {boundary}, tail: {tail}, holds: {holds}"
                );
            }
        }
    }
}

/// The entry's answer read out of `handed` by a closure call, a field, a cell's poll or a channel's take, by `boundary`, returned directly or, where `tail`, by the read's own return.
fn read_handed(
    module: &mut Module,
    boundary: usize,
    tail: bool,
    handed: ValueId,
    ret: ContinuationId,
) -> NodeId {
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
            callee: Callee::Closure(handed),
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
                args: vec![Atom::Value(handed)],
                next,
            })
        }
        2 => module.add_node(Node::Cell {
            op: CellOp::Poll,
            args: vec![Atom::Value(handed)],
            return_to: resume,
        }),
        _ => module.add_node(Node::Channel {
            op: ChannelOp::Take,
            args: vec![Atom::Value(handed)],
            return_to: resume,
        }),
    };
    module.add_node(Node::LetCont {
        continuations: vec![resume],
        body: operation,
    })
}

/// A function named as a value takes at its parameters what every call of it hands, a closure call's among them, and no more: handed a number by its known call alone it answers no suspension, and handed one by a call through the list naming it it does.
#[test]
fn an_escaping_functions_parameters_take_what_every_call_hands() {
    for through_a_closure in [false, true] {
        let module = entry_with(0, |module, _, ret| {
            let row = module.suspension_row();
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
                    droppable: Droppable::Never,
                },
            );
            let escaped = module.add_value(None);
            let call = module.add_node(Node::ApplyFun {
                callee: Callee::Known(function),
                args: vec![nat(0)],
                return_to: ret,
            });
            let call = match through_a_closure {
                false => call,
                true => {
                    let element = module.add_value(None);
                    let suspension = module.add_value(None);
                    let answer = module.add_value(None);
                    let after = module.add_continuation(Continuation {
                        debug_name: None,
                        params: vec![answer],
                        body: call,
                    });
                    let applied = module.add_node(Node::ApplyFun {
                        callee: Callee::Closure(element),
                        args: vec![Atom::Value(suspension)],
                        return_to: after,
                    });
                    let applied = module.add_node(Node::LetCont {
                        continuations: vec![after],
                        body: applied,
                    });
                    let built = module.add_node(Node::LetValue {
                        result: suspension,
                        value: ValueExpr::Row(row, vec![Atom::Filler; module.row(row).width()]),
                        next: applied,
                    });
                    module.add_node(Node::LetIntrinsic {
                        result: element,
                        op: Intrinsic::ListGet,
                        args: vec![Atom::Value(escaped), nat(0)],
                        next: built,
                    })
                }
            };
            let body = module.add_node(Node::LetValue {
                result: escaped,
                value: ValueExpr::List(vec![Atom::Fun(function)]),
                next: call,
            });
            bind_functions(module, vec![function], body)
        });
        assert_eq!(
            module.entry_suspension(),
            module.suspension().filter(|_| through_a_closure),
            "through a closure: {through_a_closure}"
        );
    }
}

/// A cell's or a channel's payload is what any fill or push stores, through a helper's tail return: none where nothing stores a suspension, and the turns where something does.
#[test]
fn a_cells_or_a_channels_payload_is_what_any_fill_or_push_stores() {
    for channel in [false, true] {
        for stored in [false, true] {
            let module = entry_with(1, |module, params, ret| {
                let row = module.suspension_row();
                let read = poll_or_take(module, channel, params[0], ret);
                match stored {
                    false => read,
                    true => stored_suspension(module, row, params[0], channel, read),
                }
            });
            assert_eq!(
                module.entry_suspension(),
                module.suspension().filter(|_| stored),
                "channel: {channel}, stored: {stored}"
            );
        }
    }
}

/// The entry's answer, the payload a helper polls out of the cell or takes off the channel `handed` and hands back by its tail return.
fn poll_or_take(
    module: &mut Module,
    channel: bool,
    handed: ValueId,
    ret: ContinuationId,
) -> NodeId {
    let worker = define(module, |module, sentinel| {
        let args = vec![Atom::Value(handed)];
        match channel {
            true => module.add_node(Node::Channel {
                op: ChannelOp::Take,
                args,
                return_to: sentinel,
            }),
            false => module.add_node(Node::Cell {
                op: CellOp::Poll,
                args,
                return_to: sentinel,
            }),
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
