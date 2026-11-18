//! The functions a value may be a closure of, through every flow: what a closure call and a list map reach.

use {
    super::*,
    crate::{Continuation, Droppable, Edge, Function, Literal, Row, Slot, test_support::*},
    curios_num::Natural,
};

/// A function of `params` parameters, its body what `body_of` builds from them and its return continuation.
fn define(
    module: &mut Module,
    name: &str,
    params: usize,
    body_of: impl FnOnce(&mut Module, &[ValueId], ContinuationId) -> NodeId,
) -> FunctionId {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let params = (0..params)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    let body = body_of(module, &params, return_cont);
    module.define_function(
        function,
        Function {
            debug_name: Some(name.into()),
            params,
            return_cont,
            body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
    function
}

/// A module whose entry takes `params` values and runs what `body_of` builds from them.
fn program(params: usize, body_of: impl FnOnce(&mut Module, &[ValueId]) -> NodeId) -> Module {
    let mut module = Module::default();
    let entry = define(&mut module, "main", params, |module, params, _| {
        body_of(module, params)
    });
    module.set_entry(entry);
    module
}

/// A function of no parameters answering `answer`.
fn answering(module: &mut Module, answer: Atom) -> FunctionId {
    define(module, "answering", 0, |module, _, ret| {
        module.add_node(Node::ApplyCont(Edge {
            target: ret,
            args: vec![answer],
        }))
    })
}

/// A function of one parameter answering it.
fn identity(module: &mut Module) -> FunctionId {
    define(module, "identity", 1, |module, params, ret| {
        module.add_node(Node::ApplyCont(Edge {
            target: ret,
            args: vec![Atom::Value(params[0])],
        }))
    })
}

/// `value` bound, its result handed to `rest` for what follows.
fn binding(
    module: &mut Module,
    value: ValueExpr,
    rest: impl FnOnce(&mut Module, ValueId) -> NodeId,
) -> NodeId {
    let result = module.add_value(None);
    let next = rest(module, result);
    module.add_node(Node::LetValue {
        result,
        value,
        next,
    })
}

/// `op` over `args` bound, its value handed to `rest` for what follows.
fn operation(
    module: &mut Module,
    op: Intrinsic,
    args: Vec<Atom>,
    rest: impl FnOnce(&mut Module, ValueId) -> NodeId,
) -> NodeId {
    let result = module.add_value(None);
    let next = rest(module, result);
    module.add_node(Node::LetIntrinsic {
        result,
        op,
        args,
        next,
    })
}

/// A continuation of `count` parameters whose body `body_of` builds from them, bound around the node `transfer` builds to it.
fn resumed(
    module: &mut Module,
    count: usize,
    body_of: impl FnOnce(&mut Module, &[ValueId]) -> NodeId,
    transfer: impl FnOnce(&mut Module, ContinuationId) -> NodeId,
) -> NodeId {
    let params = (0..count)
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    let body = body_of(module, &params);
    let continuation = module.add_continuation(Continuation {
        debug_name: None,
        params,
        body,
        terminates: false,
    });
    let transfer = transfer(module, continuation);
    module.add_node(Node::LetCont {
        continuations: vec![continuation],
        body: transfer,
    })
}

/// A run's end, after `value` is read where `probe` keeps it.
fn read(module: &mut Module, value: ValueId, probe: &mut Option<ValueId>) -> NodeId {
    *probe = Some(value);
    module.add_node(halt_zero())
}

fn one_slot_row(module: &mut Module) -> RowId {
    module.add_row(Row {
        debug_name: Some("row".into()),
        slots: vec![Slot::Opaque],
    })
}

/// What `value` may be a closure of in `module`.
fn functions(module: &Module, value: Option<ValueId>) -> MayBe<FunctionId> {
    ValueFlow::of(module).functions(&Atom::Value(value.expect("the fixture reads its value")))
}

fn of(functions: &[FunctionId]) -> MayBe<FunctionId> {
    MayBe::Of(functions.iter().copied().collect())
}

#[test]
fn a_closure_handed_to_a_known_call_reaches_its_parameter() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let handed = identity(module);
        let taking = define(module, "taking", 1, |module, params, _| {
            read(module, params[0], &mut probe)
        });
        lambda = Some(handed);
        resumed(
            module,
            1,
            |module, _| module.add_node(halt_zero()),
            |module, return_to| {
                module.add_node(Node::ApplyFun {
                    callee: Callee::Known(taking),
                    args: vec![Atom::Fun(handed)],
                    return_to,
                })
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_closure_handed_to_a_jump_reaches_the_continuations_parameter() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let handed = identity(module);
        lambda = Some(handed);
        resumed(
            module,
            1,
            |module, params| read(module, params[0], &mut probe),
            |module, target| {
                module.add_node(Node::ApplyCont(Edge {
                    target,
                    args: vec![Atom::Fun(handed)],
                }))
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_closure_a_function_returns_reaches_its_calls_resume() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let handed = identity(module);
        let returning = answering(module, Atom::Fun(handed));
        lambda = Some(handed);
        resumed(
            module,
            1,
            |module, params| read(module, params[0], &mut probe),
            |module, return_to| {
                module.add_node(Node::ApplyFun {
                    callee: Callee::Known(returning),
                    args: vec![],
                    return_to,
                })
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_closure_call_hands_its_arguments_to_every_function_its_callee_may_be() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let handed = identity(module);
        let callee = define(module, "callee", 1, |module, params, _| {
            read(module, params[0], &mut probe)
        });
        lambda = Some(handed);
        resumed(
            module,
            1,
            |module, params| {
                let closure = params[0];
                resumed(
                    module,
                    1,
                    |module, _| module.add_node(halt_zero()),
                    |module, return_to| {
                        module.add_node(Node::ApplyFun {
                            callee: Callee::Closure(closure),
                            args: vec![Atom::Fun(handed)],
                            return_to,
                        })
                    },
                )
            },
            |module, target| {
                module.add_node(Node::ApplyCont(Edge {
                    target,
                    args: vec![Atom::Fun(callee)],
                }))
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_closure_calls_answer_is_what_every_function_its_callee_may_be_returns() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let handed = identity(module);
        let returning = answering(module, Atom::Fun(handed));
        lambda = Some(handed);
        resumed(
            module,
            1,
            |module, params| {
                let closure = params[0];
                resumed(
                    module,
                    1,
                    |module, answers| read(module, answers[0], &mut probe),
                    |module, return_to| {
                        module.add_node(Node::ApplyFun {
                            callee: Callee::Closure(closure),
                            args: vec![],
                            return_to,
                        })
                    },
                )
            },
            |module, target| {
                module.add_node(Node::ApplyCont(Edge {
                    target,
                    args: vec![Atom::Fun(returning)],
                }))
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_list_map_hands_each_element_to_its_mapper_and_its_answers_to_the_elements() {
    let (mut element, mut mapped) = (None, None);
    let (mut held, mut answered) = (None, None);
    let module = program(0, |module, _| {
        let stored = identity(module);
        let answer = identity(module);
        let mapper = define(module, "mapper", 1, |module, params, ret| {
            element = Some(params[0]);
            module.add_node(Node::ApplyCont(Edge {
                target: ret,
                args: vec![Atom::Fun(answer)],
            }))
        });
        (held, answered) = (Some(stored), Some(answer));
        binding(
            module,
            ValueExpr::List(vec![Atom::Fun(stored)]),
            |module, list| {
                resumed(
                    module,
                    1,
                    |module, params| {
                        operation(
                            module,
                            Intrinsic::ListGet,
                            vec![
                                Atom::Value(params[0]),
                                Atom::Literal(Literal::Nat(Natural::from(0u32))),
                            ],
                            |module, got| read(module, got, &mut mapped),
                        )
                    },
                    |module, return_to| {
                        module.add_node(Node::Intrinsic {
                            op: IntrinsicCall::ListMap,
                            args: vec![Atom::Value(list), Atom::Fun(mapper)],
                            return_to,
                        })
                    },
                )
            },
        )
    });
    // Every list's elements are one key, so the mapper is handed what it answers as well.
    for (value, function) in [(element, held), (mapped, answered)] {
        let MayBe::Of(may_be) = functions(&module, value) else {
            panic!("an element is no unknown");
        };
        assert!(may_be.contains(&function.unwrap()));
    }
}

#[test]
fn a_row_slot_holds_what_its_construction_stores() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let row = one_slot_row(module);
        let stored = identity(module);
        lambda = Some(stored);
        binding(
            module,
            ValueExpr::Row(row, vec![Atom::Fun(stored)]),
            |module, built| {
                operation(
                    module,
                    Intrinsic::RowGet(row, 0),
                    vec![Atom::Value(built)],
                    |module, got| read(module, got, &mut probe),
                )
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_tuple_field_holds_what_its_construction_stores() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let stored = identity(module);
        lambda = Some(stored);
        binding(
            module,
            ValueExpr::Tuple(vec![Atom::Fun(stored)]),
            |module, built| {
                operation(
                    module,
                    Intrinsic::TupleGet(0),
                    vec![Atom::Value(built)],
                    |module, got| read(module, got, &mut probe),
                )
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_list_element_is_what_any_list_stores_or_appends() {
    let mut probe = None;
    let (mut built_in, mut appended) = (None, None);
    let module = program(0, |module, _| {
        let first = identity(module);
        let second = identity(module);
        (built_in, appended) = (Some(first), Some(second));
        binding(
            module,
            ValueExpr::List(vec![Atom::Fun(first)]),
            |module, list| {
                operation(
                    module,
                    Intrinsic::ListAppend,
                    vec![Atom::Value(list), Atom::Fun(second)],
                    |module, longer| {
                        operation(
                            module,
                            Intrinsic::ListGet,
                            vec![
                                Atom::Value(longer),
                                Atom::Literal(Literal::Nat(Natural::from(0u32))),
                            ],
                            |module, got| read(module, got, &mut probe),
                        )
                    },
                )
            },
        )
    });
    assert_eq!(
        functions(&module, probe),
        of(&[built_in.unwrap(), appended.unwrap()])
    );
}

#[test]
fn a_cell_holds_what_any_fill_stores() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let stored = identity(module);
        lambda = Some(stored);
        resumed(
            module,
            1,
            |module, cell| {
                let cell = cell[0];
                resumed(
                    module,
                    1,
                    |module, _| {
                        resumed(
                            module,
                            2,
                            |module, polled| read(module, polled[1], &mut probe),
                            |module, return_to| {
                                module.add_node(Node::Cell {
                                    op: CellOp::Poll,
                                    args: vec![Atom::Value(cell)],
                                    return_to,
                                })
                            },
                        )
                    },
                    |module, return_to| {
                        module.add_node(Node::Cell {
                            op: CellOp::Fill,
                            args: vec![Atom::Value(cell), Atom::Fun(stored)],
                            return_to,
                        })
                    },
                )
            },
            |module, return_to| {
                module.add_node(Node::Cell {
                    op: CellOp::Reserve,
                    args: vec![],
                    return_to,
                })
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_channel_holds_what_any_push_stores() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let stored = identity(module);
        lambda = Some(stored);
        resumed(
            module,
            1,
            |module, channel| {
                let channel = channel[0];
                resumed(
                    module,
                    1,
                    |module, _| {
                        resumed(
                            module,
                            2,
                            |module, taken| read(module, taken[1], &mut probe),
                            |module, return_to| {
                                module.add_node(Node::Channel {
                                    op: ChannelOp::Take,
                                    args: vec![Atom::Value(channel)],
                                    return_to,
                                })
                            },
                        )
                    },
                    |module, return_to| {
                        module.add_node(Node::Channel {
                            op: ChannelOp::Push,
                            args: vec![Atom::Value(channel), Atom::Fun(stored)],
                            return_to,
                        })
                    },
                )
            },
            |module, return_to| {
                module.add_node(Node::Channel {
                    op: ChannelOp::New,
                    args: vec![Atom::Literal(Literal::Nat(Natural::from(1u32)))],
                    return_to,
                })
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn two_constructions_of_one_row_keep_their_slots_apart() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(0, |module, _| {
        let row = one_slot_row(module);
        let kept = identity(module);
        let apart = identity(module);
        lambda = Some(kept);
        binding(
            module,
            ValueExpr::Row(row, vec![Atom::Fun(apart)]),
            |module, _| {
                binding(
                    module,
                    ValueExpr::Row(row, vec![Atom::Fun(kept)]),
                    |module, built| {
                        operation(
                            module,
                            Intrinsic::RowGet(row, 0),
                            vec![Atom::Value(built)],
                            |module, got| read(module, got, &mut probe),
                        )
                    },
                )
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_rebuild_writes_the_slots_of_every_construction_its_of_may_be() {
    let mut probe = None;
    let (mut first, mut written) = (None, None);
    let module = program(0, |module, _| {
        let row = one_slot_row(module);
        let built_with = identity(module);
        let rebuilt_with = identity(module);
        (first, written) = (Some(built_with), Some(rebuilt_with));
        binding(
            module,
            ValueExpr::Row(row, vec![Atom::Fun(built_with)]),
            |module, built| {
                binding(
                    module,
                    ValueExpr::Rebuilt {
                        of: Atom::Value(built),
                        row,
                        slots: vec![(0, Atom::Fun(rebuilt_with))],
                    },
                    |module, rebuilt| {
                        operation(
                            module,
                            Intrinsic::RowGet(row, 0),
                            vec![Atom::Value(rebuilt)],
                            |module, got| read(module, got, &mut probe),
                        )
                    },
                )
            },
        )
    });
    assert_eq!(
        functions(&module, probe),
        of(&[first.unwrap(), written.unwrap()])
    );
}

#[test]
fn what_the_entry_is_handed_is_any() {
    let mut probe = None;
    let module = program(1, |module, params| read(module, params[0], &mut probe));
    assert_eq!(functions(&module, probe), MayBe::Any);
}

#[test]
fn a_read_out_of_what_the_entry_is_handed_takes_every_construction_of_its_row() {
    let mut probe = None;
    let mut lambda = None;
    let module = program(1, |module, params| {
        let row = one_slot_row(module);
        let stored = identity(module);
        lambda = Some(stored);
        let handed = params[0];
        binding(
            module,
            ValueExpr::Row(row, vec![Atom::Fun(stored)]),
            |module, _| {
                operation(
                    module,
                    Intrinsic::RowGet(row, 0),
                    vec![Atom::Value(handed)],
                    |module, got| read(module, got, &mut probe),
                )
            },
        )
    });
    assert_eq!(functions(&module, probe), of(&[lambda.unwrap()]));
}

#[test]
fn a_heap_call_answers_any() {
    let mut probe = None;
    let module = program(0, |module, _| {
        let callee = identity(module);
        resumed(
            module,
            1,
            |module, answers| read(module, answers[0], &mut probe),
            |module, return_to| {
                module.add_node(Node::Intrinsic {
                    op: IntrinsicCall::HeapApply,
                    args: vec![Atom::Fun(callee), Atom::Filler, Atom::Fun(callee)],
                    return_to,
                })
            },
        )
    });
    assert_eq!(functions(&module, probe), MayBe::Any);
}
