use {
    super::*,
    curios_base::{Grain, PackedBin},
};

#[test]
fn lowers_unreachable_tail_to_trap() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: curios_cont::Tail::Unreachable,
            },
        },
    );

    assert!(traps(&module));
}

// Cyclic tuples/arrays are intentionally unrepresentable: `Region::preallocs` only holds
// closure shells now (cyclic data is rejected in `into_cont`), which keeps every `tpl`/`lst`
// wasm field immutable. Mutual recursion is exercised through closures below.

#[test]
fn lowers_and_runs_mutually_recursive_closures() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Int(0),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Int(1),
    );
    module.add_const(
        curios_cont::ValueName::from("EVEN"),
        curios_cont::Data::Int(11),
    );
    module.add_const(
        curios_cont::ValueName::from("ODD"),
        curios_cont::Data::Int(22),
    );

    module.add_clsr(
        curios_cont::ClsrName::from("even"),
        curios_cont::Clsr {
            fields: vec![curios_cont::ValueName::from("odd").into()],
            params: vec![curios_cont::ValueName::from("n").into()],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("is_zero"),
                    curios_cont::Value::Eval(curios_cont::Code::IntEql(
                        curios_cont::ValueName::from("n"),
                        curios_cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![
                    (
                        curios_cont::BlockName::from("on_zero"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("r"),
                                    params: vec![curios_cont::ValueName::from("EVEN")],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("on_non_zero"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    curios_cont::ValueName::from("prev"),
                                    curios_cont::Value::Eval(curios_cont::Code::IntSub(
                                        curios_cont::ValueName::from("n"),
                                        curios_cont::ValueName::from("ONE"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: curios_cont::Tail::Call(curios_cont::CallTarget::Indirect {
                                    target: curios_cont::ValueName::from("odd"),
                                    params: vec![curios_cont::ValueName::from("prev")],
                                    resume: curios_cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                    operand: curios_cont::ValueName::from("is_zero"),
                    cases: std::collections::BTreeMap::from([(
                        0,
                        curios_cont::JumpTarget {
                            target: curios_cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        },
                    )]),
                    default: Some(curios_cont::JumpTarget {
                        target: curios_cont::BlockName::from("on_zero"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    module.add_clsr(
        curios_cont::ClsrName::from("odd"),
        curios_cont::Clsr {
            fields: vec![curios_cont::ValueName::from("even").into()],
            params: vec![curios_cont::ValueName::from("n").into()],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("is_zero"),
                    curios_cont::Value::Eval(curios_cont::Code::IntEql(
                        curios_cont::ValueName::from("n"),
                        curios_cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![
                    (
                        curios_cont::BlockName::from("on_zero"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("r"),
                                    params: vec![curios_cont::ValueName::from("ODD")],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("on_non_zero"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    curios_cont::ValueName::from("prev"),
                                    curios_cont::Value::Eval(curios_cont::Code::IntSub(
                                        curios_cont::ValueName::from("n"),
                                        curios_cont::ValueName::from("ONE"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: curios_cont::Tail::Call(curios_cont::CallTarget::Indirect {
                                    target: curios_cont::ValueName::from("even"),
                                    params: vec![curios_cont::ValueName::from("prev")],
                                    resume: curios_cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                    operand: curios_cont::ValueName::from("is_zero"),
                    cases: std::collections::BTreeMap::from([(
                        0,
                        curios_cont::JumpTarget {
                            target: curios_cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        },
                    )]),
                    default: Some(curios_cont::JumpTarget {
                        target: curios_cont::BlockName::from("on_zero"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![
                    (
                        curios_cont::ValueName::from("even"),
                        curios_cont::ClsrName::from("even"),
                    ),
                    (
                        curios_cont::ValueName::from("odd"),
                        curios_cont::ClsrName::from("odd"),
                    ),
                ],
                values: vec![
                    (
                        curios_cont::ValueName::from("even"),
                        curios_cont::Value::Pure(curios_cont::Data::Clsr(
                            curios_cont::ClsrName::from("even"),
                            vec![curios_cont::ValueName::from("odd")],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("odd"),
                        curios_cont::Value::Pure(curios_cont::Data::Clsr(
                            curios_cont::ClsrName::from("odd"),
                            vec![curios_cont::ValueName::from("even")],
                        )),
                    ),
                ],
                blocks: vec![(
                    curios_cont::BlockName::from("after"),
                    curios_cont::Block {
                        params: vec![curios_cont::ValueName::from("out")],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                code: curios_cont::ValueName::from("out"),
                                resume: curios_cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Call(curios_cont::CallTarget::Indirect {
                    target: curios_cont::ValueName::from("even"),
                    params: vec![curios_cont::ValueName::from("ONE")],
                    resume: curios_cont::BlockName::from("after"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 22);
}

#[test]
fn lowers_and_runs_direct_call() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Int(1),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Int(2),
    );

    module.add_func(
        curios_cont::FuncName::from("add_one"),
        curios_cont::Func {
            params: vec![curios_cont::ValueName::from("x").into()],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("sum"),
                    curios_cont::Value::Eval(curios_cont::Code::IntAdd(
                        curios_cont::ValueName::from("x"),
                        curios_cont::ValueName::from("ONE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("r"),
                    params: vec![curios_cont::ValueName::from("sum")],
                }),
            },
        },
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    curios_cont::BlockName::from("after_call"),
                    curios_cont::Block {
                        params: vec![curios_cont::ValueName::from("out")],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                code: curios_cont::ValueName::from("out"),
                                resume: curios_cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Call(curios_cont::CallTarget::Direct {
                    target: curios_cont::FuncName::from("add_one"),
                    params: vec![curios_cont::ValueName::from("TWO")],
                    resume: curios_cont::BlockName::from("after_call"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_unit_result() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("unit"),
                    curios_cont::Value::Pure(curios_cont::Data::Tpl(vec![])),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("r"),
                    params: vec![curios_cont::ValueName::from("unit")],
                }),
            },
        },
    );

    printed(&module);
}

#[test]
fn lowers_and_runs_float_result() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("LEFT"),
        curios_cont::Data::Flt(1.25),
    );

    module.add_const(
        curios_cont::ValueName::from("RIGHT"),
        curios_cont::Data::Flt(2.5),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        curios_cont::ValueName::from("sum"),
                        curios_cont::Value::Eval(curios_cont::Code::FltAdd(
                            curios_cont::ValueName::from("LEFT"),
                            curios_cont::ValueName::from("RIGHT"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBytes(
                            curios_cont::ValueName::from("sum"),
                        )),
                    ),
                ],
                blocks: vec![(
                    curios_cont::BlockName::from("io_done"),
                    curios_cont::Block {
                        params: vec![
                            curios_cont::ValueName::from("io_status"),
                            curios_cont::ValueName::from("io_written"),
                        ],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                target: curios_cont::BlockName::from("r"),
                                params: vec![curios_cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        curios_cont::ValueName::from("STDOUT"),
                        curios_cont::ValueName::from("str"),
                    ],
                    resume: curios_cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 3.75);
}

#[test]
fn lowers_and_runs_global_tuple() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Int(1),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Int(2),
    );
    module.add_const(
        curios_cont::ValueName::from("PAIR"),
        curios_cont::Data::Tpl(vec![
            curios_cont::ValueName::from("ONE"),
            curios_cont::ValueName::from("TWO"),
        ]),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("out"),
                    curios_cont::Value::Eval(curios_cont::Code::TplGet(
                        curios_cont::ValueName::from("PAIR"),
                        1,
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("out"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_global_closure() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("BIAS"),
        curios_cont::Data::Int(5),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Int(3),
    );

    module.add_clsr(
        curios_cont::ClsrName::from("add_bias"),
        curios_cont::Clsr {
            fields: vec![curios_cont::ValueName::from("bias").into()],
            params: vec![curios_cont::ValueName::from("x").into()],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::IntAdd(
                        curios_cont::ValueName::from("x"),
                        curios_cont::ValueName::from("bias"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("r"),
                    params: vec![curios_cont::ValueName::from("result")],
                }),
            },
        },
    );

    module.add_const(
        curios_cont::ValueName::from("K"),
        curios_cont::Data::Clsr(
            curios_cont::ClsrName::from("add_bias"),
            vec![curios_cont::ValueName::from("BIAS")],
        ),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    curios_cont::BlockName::from("after"),
                    curios_cont::Block {
                        params: vec![curios_cont::ValueName::from("out")],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                code: curios_cont::ValueName::from("out"),
                                resume: curios_cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Call(curios_cont::CallTarget::Indirect {
                    target: curios_cont::ValueName::from("K"),
                    params: vec![curios_cont::ValueName::from("THREE")],
                    resume: curios_cont::BlockName::from("after"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 8);
}

#[test]
fn lowers_and_runs_sparse_match() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("BYTE"),
        curios_cont::Data::Nat(123),
    ); // '{'
    module.add_const(
        curios_cont::ValueName::from("R0"),
        curios_cont::Data::Nat(0),
    );
    module.add_const(
        curios_cont::ValueName::from("R1"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("R2"),
        curios_cont::Data::Nat(2),
    );
    module.add_const(
        curios_cont::ValueName::from("R3"),
        curios_cont::Data::Nat(3),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![
                    (
                        curios_cont::BlockName::from("after"),
                        curios_cont::Block {
                            params: vec![curios_cont::ValueName::from("out")],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                    code: curios_cont::ValueName::from("out"),
                                    resume: curios_cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("b_quote"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("after"),
                                    params: vec![curios_cont::ValueName::from("R1")],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("b_lbracket"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("after"),
                                    params: vec![curios_cont::ValueName::from("R2")],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("b_lbrace"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("after"),
                                    params: vec![curios_cont::ValueName::from("R3")],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("b_default"),
                        curios_cont::Block {
                            params: vec![],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("after"),
                                    params: vec![curios_cont::ValueName::from("R0")],
                                }),
                            },
                        },
                    ),
                ],
                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                    operand: curios_cont::ValueName::from("BYTE"),
                    cases: std::collections::BTreeMap::from([
                        (
                            34,
                            curios_cont::JumpTarget {
                                target: curios_cont::BlockName::from("b_quote"),
                                params: vec![],
                            },
                        ),
                        (
                            91,
                            curios_cont::JumpTarget {
                                target: curios_cont::BlockName::from("b_lbracket"),
                                params: vec![],
                            },
                        ),
                        (
                            123,
                            curios_cont::JumpTarget {
                                target: curios_cont::BlockName::from("b_lbrace"),
                                params: vec![],
                            },
                        ),
                    ]),
                    default: Some(curios_cont::JumpTarget {
                        target: curios_cont::BlockName::from("b_default"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}
