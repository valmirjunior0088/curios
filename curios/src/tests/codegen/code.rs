use {super::*, curios_cont::to_wasm};

#[test]
fn lowers_and_runs_nat_add() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("FOUR"),
        curios_cont::Data::Nat(4),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                        curios_cont::ValueName::from("THREE"),
                        curios_cont::ValueName::from("FOUR"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_lst_len() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("LST"),
        curios_cont::Data::Lst(vec![
            curios_cont::ValueName::from("THREE"),
            curios_cont::ValueName::from("SEVEN"),
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
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::LstLen(
                        curios_cont::ValueName::from("LST"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_lst_get() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("LST"),
        curios_cont::Data::Lst(vec![
            curios_cont::ValueName::from("THREE"),
            curios_cont::ValueName::from("SEVEN"),
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
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::LstGet(
                        curios_cont::ValueName::from("LST"),
                        curios_cont::ValueName::from("ONE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_lst_slice() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE_IDX"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("LST"),
        curios_cont::Data::Lst(vec![
            curios_cont::ValueName::from("THREE"),
            curios_cont::ValueName::from("SEVEN"),
            curios_cont::ValueName::from("FIVE"),
        ]),
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
                        curios_cont::ValueName::from("slice"),
                        curios_cont::Value::Eval(curios_cont::Code::LstSlice(
                            curios_cont::ValueName::from("LST"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("THREE_IDX"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::LstLen(
                            curios_cont::ValueName::from("slice"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_lst_concat() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Nat(2),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("LST1"),
        curios_cont::Data::Lst(vec![curios_cont::ValueName::from("ONE")]),
    );
    module.add_const(
        curios_cont::ValueName::from("LST2"),
        curios_cont::Data::Lst(vec![
            curios_cont::ValueName::from("TWO"),
            curios_cont::ValueName::from("THREE"),
        ]),
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
                        curios_cont::ValueName::from("concat"),
                        curios_cont::Value::Eval(curios_cont::Code::LstConcat(vec![
                            curios_cont::ValueName::from("LST1"),
                            curios_cont::ValueName::from("LST2"),
                        ])),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::LstLen(
                            curios_cont::ValueName::from("concat"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_flt_floor() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("X"),
        curios_cont::Data::Flt(2.9),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltFloor(
                            curios_cont::ValueName::from("X"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_flt_ceil() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("X"),
        curios_cont::Data::Flt(2.1),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltCeil(
                            curios_cont::ValueName::from("X"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_flt_trunc() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("X"),
        curios_cont::Data::Flt(-2.9),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltTrunc(
                            curios_cont::ValueName::from("X"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), -2.0);
}

#[test]
fn lowers_and_runs_flt_nearest() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("X"),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltNearest(
                            curios_cont::ValueName::from("X"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_nat_div() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("TEN"),
        curios_cont::Data::Nat(10),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatDiv(
                        curios_cont::ValueName::from("TEN"),
                        curios_cont::ValueName::from("THREE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_rem() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("TEN"),
        curios_cont::Data::Nat(10),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatRem(
                        curios_cont::ValueName::from("TEN"),
                        curios_cont::ValueName::from("THREE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_nat_lt() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatLt(
                        curios_cont::ValueName::from("THREE"),
                        curios_cont::ValueName::from("FIVE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neg() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Int(5),
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
                        curios_cont::ValueName::from("zero"),
                        curios_cont::Value::Pure(curios_cont::Data::Int(0)),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::IntSub(
                            curios_cont::ValueName::from("zero"),
                            curios_cont::ValueName::from("FIVE"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(int_result(&module), -5);
}

#[test]
fn lowers_and_runs_int_div() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("NEG7"),
        curios_cont::Data::Int(-7),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Int(2),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::IntDiv(
                        curios_cont::ValueName::from("NEG7"),
                        curios_cont::ValueName::from("TWO"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(int_result(&module), -3);
}

#[test]
fn lowers_and_runs_int_lt() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("NEG1"),
        curios_cont::Data::Int(-1),
    );
    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Int(0),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::IntLt(
                        curios_cont::ValueName::from("NEG1"),
                        curios_cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_div() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Flt(1.0),
    );
    module.add_const(
        curios_cont::ValueName::from("FOUR"),
        curios_cont::Data::Flt(4.0),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltDiv(
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("FOUR"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 0.25);
}

#[test]
fn lowers_and_runs_flt_eql() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Flt(1.5),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Flt(1.5),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::FltEql(
                        curios_cont::ValueName::from("A"),
                        curios_cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_sqrt() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("FOUR"),
        curios_cont::Data::Flt(4.0),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltSqrt(
                            curios_cont::ValueName::from("FOUR"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_int_to_flt() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Int(3),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::IntToFlt(
                            curios_cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_nat_to_flt() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::NatToFlt(
                            curios_cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 5.0);
}

#[test]
fn lowers_and_runs_flt_to_int() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE_SEVEN"),
        curios_cont::Data::Flt(3.7),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::FltToInt(
                        curios_cont::ValueName::from("THREE_SEVEN"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_to_int() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatToInt(
                        curios_cont::ValueName::from("SEVEN"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_neq() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::NatNeq(
                        curios_cont::ValueName::from("THREE"),
                        curios_cont::ValueName::from("FIVE"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neq() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("NEG1"),
        curios_cont::Data::Int(-1),
    );
    module.add_const(
        curios_cont::ValueName::from("NEG1B"),
        curios_cont::Data::Int(-1),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::IntNeq(
                        curios_cont::ValueName::from("NEG1"),
                        curios_cont::ValueName::from("NEG1B"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_flt_neq() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Flt(1.0),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Flt(2.0),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::FltNeq(
                        curios_cont::ValueName::from("ONE"),
                        curios_cont::ValueName::from("TWO"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_min() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Flt(1.5),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltMin(
                            curios_cont::ValueName::from("A"),
                            curios_cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 1.5);
}

#[test]
fn lowers_and_runs_flt_max() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Flt(1.5),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
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
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::FltMax(
                            curios_cont::ValueName::from("A"),
                            curios_cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("str"),
                        curios_cont::Value::Eval(curios_cont::Code::FltToLeBin(
                            curios_cont::ValueName::from("result"),
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

    assert_eq!(f32_result(&module), 2.5);
}

#[test]
fn lowers_and_runs_bin_len() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::BinLen(
                        curios_cont::ValueName::from("HELLO"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 5);
}

#[test]
fn lowers_and_runs_bin_get() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("IDX"),
        curios_cont::Data::Nat(1),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::BinGet(
                        curios_cont::ValueName::from("HELLO"),
                        curios_cont::ValueName::from("IDX"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'e' as i32);
}

#[test]
fn lowers_and_runs_bin_append() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("BANG"),
        curios_cont::Data::Nat(b'!' as u32),
    );
    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
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
                        curios_cont::ValueName::from("appended"),
                        curios_cont::Value::Eval(curios_cont::Code::BinAppend(
                            curios_cont::ValueName::from("HELLO"),
                            curios_cont::ValueName::from("BANG"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            curios_cont::ValueName::from("appended"),
                            curios_cont::ValueName::from("FIVE"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'!' as i32);
}

#[test]
fn lowers_and_runs_bin_eql_equal() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::BinEql(
                        curios_cont::ValueName::from("A"),
                        curios_cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_bin_eql_unequal() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(b"world".to_vec()),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::BinEql(
                        curios_cont::ValueName::from("A"),
                        curios_cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_lst_append() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("NINE"),
        curios_cont::Data::Nat(9),
    );
    module.add_const(
        curios_cont::ValueName::from("LST"),
        curios_cont::Data::Lst(vec![
            curios_cont::ValueName::from("THREE"),
            curios_cont::ValueName::from("SEVEN"),
        ]),
    );
    module.add_const(
        curios_cont::ValueName::from("TWO"),
        curios_cont::Data::Nat(2),
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
                        curios_cont::ValueName::from("appended"),
                        curios_cont::Value::Eval(curios_cont::Code::LstAppend(
                            curios_cont::ValueName::from("LST"),
                            curios_cont::ValueName::from("NINE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::LstGet(
                            curios_cont::ValueName::from("appended"),
                            curios_cont::ValueName::from("TWO"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 9);
}

fn nat_op_module(op: curios_cont::Code, left: u32, right: u32) -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );
    module.add_const(
        curios_cont::ValueName::from("LEFT"),
        curios_cont::Data::Nat(left),
    );
    module.add_const(
        curios_cont::ValueName::from("RIGHT"),
        curios_cont::Data::Nat(right),
    );
    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(op),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );
    module
}

const MAX_I31: u32 = i32::MAX as u32;

#[test]
fn lowers_and_runs_nat_mul() {
    let module = nat_op_module(
        curios_cont::Code::NatMul(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        6,
        7,
    );
    assert_eq!(i32_result(&module), 42);
}

#[test]
fn lowers_and_runs_nat_sub_monus() {
    let module = nat_op_module(
        curios_cont::Code::NatSub(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        3,
        7,
    );
    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_nat_sub() {
    let module = nat_op_module(
        curios_cont::Code::NatSub(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        10,
        3,
    );
    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_add_at_boundary() {
    let module = nat_op_module(
        curios_cont::Code::NatAdd(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_I31 - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn lowers_and_runs_nat_mul_at_boundary() {
    let module = nat_op_module(
        curios_cont::Code::NatMul(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn nat_add_overflow_traps() {
    let module = nat_op_module(
        curios_cont::Code::NatAdd(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn nat_mul_overflow_traps() {
    let module = nat_op_module(
        curios_cont::Code::NatMul(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        2,
    );
    assert!(traps(&module));
}

fn int_op_module(op: curios_cont::Code, left: i32, right: i32) -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );
    module.add_const(
        curios_cont::ValueName::from("LEFT"),
        curios_cont::Data::Int(left),
    );
    module.add_const(
        curios_cont::ValueName::from("RIGHT"),
        curios_cont::Data::Int(right),
    );
    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(op),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );
    module
}

const MAX_INT: i32 = (1 << 30) - 1;
const MIN_INT: i32 = -(1 << 30);

#[test]
fn lowers_and_runs_int_add_at_boundary() {
    let module = int_op_module(
        curios_cont::Code::IntAdd(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_INT - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_add_overflow_traps() {
    let module = int_op_module(
        curios_cont::Code::IntAdd(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn int_sub_overflow_traps() {
    let module = int_op_module(
        curios_cont::Code::IntSub(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn lowers_and_runs_int_mul_at_boundary() {
    let module = int_op_module(
        curios_cont::Code::IntMul(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_mul_overflow_traps() {
    let module = int_op_module(
        curios_cont::Code::IntMul(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        2,
    );
    assert!(traps(&module));
}

#[test]
fn int_div_overflow_traps() {
    // MIN_INT / -1 = 2^30, which exceeds the 31-bit signed maximum.
    let module = int_op_module(
        curios_cont::Code::IntDiv(
            curios_cont::ValueName::from("LEFT"),
            curios_cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        -1,
    );
    assert!(traps(&module));
}

#[test]
fn flt_to_int_overflow_traps() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );
    module.add_const(
        curios_cont::ValueName::from("TOO_BIG"),
        curios_cont::Data::Flt((MAX_INT as f32) + 1.0),
    );
    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("result"),
                    curios_cont::Value::Eval(curios_cont::Code::FltToInt(
                        curios_cont::ValueName::from("TOO_BIG"),
                    )),
                )],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("result"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );
    assert!(traps(&module));
}

// A region with a single block whose body never branches back into it has no
// back-edge, so the dispatcher `loop` + `br_table` + `-1` seed collapse to a
// plain `block` the entry branches out of. The block here is reached by a
// forward `Tail::Jump`, and its own body only writes-and-returns, so direct
// lowering must fire: the emitted code carries the block but no loop or seed.
#[test]
fn single_block_region_lowers_without_dispatch_loop() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
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
                    curios_cont::BlockName::from("b"),
                    curios_cont::Block {
                        params: vec![curios_cont::ValueName::from("x")],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                code: curios_cont::ValueName::from("x"),
                                resume: curios_cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("b"),
                    params: vec![curios_cont::ValueName::from("SEVEN")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);

    let wat = to_wasm(&module).to_string();
    assert!(wat.contains("block $$b"), "expected a direct block:\n{wat}");
    assert!(
        !wat.contains("loop "),
        "dispatch loop not collapsed:\n{wat}"
    );
    assert!(
        !wat.contains("i32.const -1"),
        "dispatcher seed not removed:\n{wat}",
    );
}

/// A 100k-deep append chain (a left spine of `bin/node`s) reads back through
/// the *iterative* force walk — a recursive force would overflow the wasm
/// stack orders of magnitude earlier. `len` answers without forcing; the
/// final `get` forces once, exercising the worklist's grow-by-doubling path.
#[test]
fn forces_deep_rope_chains_iteratively() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("DEPTH"),
        curios_cont::Data::Nat(100_000),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("EMPTY"),
        curios_cont::Data::Bin(vec![]),
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
                        curios_cont::BlockName::from("step"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("i"),
                                curios_cont::ValueName::from("acc"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                                    operand: curios_cont::ValueName::from("i"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        curios_cont::JumpTarget {
                                            target: curios_cont::BlockName::from("read"),
                                            params: vec![curios_cont::ValueName::from("acc")],
                                        },
                                    )]),
                                    default: Some(curios_cont::JumpTarget {
                                        target: curios_cont::BlockName::from("grow"),
                                        params: vec![
                                            curios_cont::ValueName::from("i"),
                                            curios_cont::ValueName::from("acc"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("grow"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("j"),
                                curios_cont::ValueName::from("cur"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        curios_cont::ValueName::from("next_i"),
                                        curios_cont::Value::Eval(curios_cont::Code::NatSub(
                                            curios_cont::ValueName::from("j"),
                                            curios_cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("next_acc"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinAppend(
                                            curios_cont::ValueName::from("cur"),
                                            curios_cont::ValueName::from("SEVEN"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("step"),
                                    params: vec![
                                        curios_cont::ValueName::from("next_i"),
                                        curios_cont::ValueName::from("next_acc"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("read"),
                        curios_cont::Block {
                            params: vec![curios_cont::ValueName::from("built")],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        curios_cont::ValueName::from("len"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                                            curios_cont::ValueName::from("built"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("idx"),
                                        curios_cont::Value::Eval(curios_cont::Code::NatSub(
                                            curios_cont::ValueName::from("len"),
                                            curios_cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("byte"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                                            curios_cont::ValueName::from("built"),
                                            curios_cont::ValueName::from("idx"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                    code: curios_cont::ValueName::from("byte"),
                                    resume: curios_cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("step"),
                    params: vec![
                        curios_cont::ValueName::from("DEPTH"),
                        curios_cont::ValueName::from("EMPTY"),
                    ],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

/// Reads after a concat hit the memoized payload: the first `get` forces and
/// caches, the second reads the cache, and `len` never forces at all. Pins
/// the read-after-concat roundtrip: 2 + 4 + 5 = 11.
#[test]
fn rereads_a_concat_through_the_memo() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(vec![1, 2]),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(vec![3, 4, 5]),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
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
                        curios_cont::ValueName::from("cat"),
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(vec![
                            curios_cont::ValueName::from("B1"),
                            curios_cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        curios_cont::ValueName::from("a"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("b"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("len"),
                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                            curios_cont::ValueName::from("cat"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("ab"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("a"),
                            curios_cont::ValueName::from("b"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("sum"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("ab"),
                            curios_cont::ValueName::from("len"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("sum"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 11);
}

/// A never-read rope crosses the host boundary directly: the wire force at
/// the `io_write` call site flattens the node tree, so the host sees the
/// plain payload bytes.
#[test]
fn writes_an_unforced_rope_to_the_host() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(b"hel".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(b"lo".to_vec()),
    );

    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![(
                    curios_cont::ValueName::from("cat"),
                    curios_cont::Value::Eval(curios_cont::Code::BinConcat(vec![
                        curios_cont::ValueName::from("B1"),
                        curios_cont::ValueName::from("B2"),
                    ])),
                )],
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
                        curios_cont::ValueName::from("cat"),
                    ],
                    resume: curios_cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(printed(&module), "hello");
}

/// A head/tail peel loop over a 100k-byte rope is linear by construction: the
/// first read forces (and memoizes) once, and from then on every `slice` tail
/// is an O(1) `sub` window that collapses onto the settled base, every `get`
/// an O(1) read-through. A copying slice would make this quadratic — ~5×10⁹
/// byte moves — and hang the suite.
#[test]
fn peels_a_rope_through_o1_windows() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("DEPTH"),
        curios_cont::Data::Nat(100_000),
    );
    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Nat(0),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("SEVEN"),
        curios_cont::Data::Nat(7),
    );
    module.add_const(
        curios_cont::ValueName::from("EMPTY"),
        curios_cont::Data::Bin(vec![]),
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
                        curios_cont::BlockName::from("step"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("i"),
                                curios_cont::ValueName::from("acc"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                                    operand: curios_cont::ValueName::from("i"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        curios_cont::JumpTarget {
                                            target: curios_cont::BlockName::from("peel"),
                                            params: vec![
                                                curios_cont::ValueName::from("acc"),
                                                curios_cont::ValueName::from("ZERO"),
                                            ],
                                        },
                                    )]),
                                    default: Some(curios_cont::JumpTarget {
                                        target: curios_cont::BlockName::from("grow"),
                                        params: vec![
                                            curios_cont::ValueName::from("i"),
                                            curios_cont::ValueName::from("acc"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("grow"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("j"),
                                curios_cont::ValueName::from("cur"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        curios_cont::ValueName::from("next_i"),
                                        curios_cont::Value::Eval(curios_cont::Code::NatSub(
                                            curios_cont::ValueName::from("j"),
                                            curios_cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("next_acc"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinAppend(
                                            curios_cont::ValueName::from("cur"),
                                            curios_cont::ValueName::from("SEVEN"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("step"),
                                    params: vec![
                                        curios_cont::ValueName::from("next_i"),
                                        curios_cont::ValueName::from("next_acc"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("peel"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("b"),
                                curios_cont::ValueName::from("sum"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    curios_cont::ValueName::from("len"),
                                    curios_cont::Value::Eval(curios_cont::Code::BinLen(
                                        curios_cont::ValueName::from("b"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                                    operand: curios_cont::ValueName::from("len"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        curios_cont::JumpTarget {
                                            target: curios_cont::BlockName::from("done"),
                                            params: vec![curios_cont::ValueName::from("sum")],
                                        },
                                    )]),
                                    default: Some(curios_cont::JumpTarget {
                                        target: curios_cont::BlockName::from("chop"),
                                        params: vec![
                                            curios_cont::ValueName::from("b"),
                                            curios_cont::ValueName::from("sum"),
                                            curios_cont::ValueName::from("len"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("chop"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("b2"),
                                curios_cont::ValueName::from("s2"),
                                curios_cont::ValueName::from("l2"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        curios_cont::ValueName::from("byte"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                                            curios_cont::ValueName::from("b2"),
                                            curios_cont::ValueName::from("ZERO"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("tail"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                                            curios_cont::ValueName::from("b2"),
                                            curios_cont::ValueName::from("ONE"),
                                            curios_cont::ValueName::from("l2"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("s3"),
                                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                                            curios_cont::ValueName::from("s2"),
                                            curios_cont::ValueName::from("byte"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("peel"),
                                    params: vec![
                                        curios_cont::ValueName::from("tail"),
                                        curios_cont::ValueName::from("s3"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("done"),
                        curios_cont::Block {
                            params: vec![curios_cont::ValueName::from("total")],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                    code: curios_cont::ValueName::from("total"),
                                    resume: curios_cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("step"),
                    params: vec![
                        curios_cont::ValueName::from("DEPTH"),
                        curios_cont::ValueName::from("EMPTY"),
                    ],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 700_000);
}

/// A window over a window collapses onto the shared base, and reads go
/// through it without copying: slice a concat (forcing it once), slice the
/// slice, and check `get`s and `len` land on the right elements.
/// 3 + 4 + 2 = 9.
#[test]
fn windows_collapse_and_read_through() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(vec![1, 2, 3]),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(vec![4, 5, 6]),
    );
    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Nat(0),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("THREE"),
        curios_cont::Data::Nat(3),
    );
    module.add_const(
        curios_cont::ValueName::from("FIVE"),
        curios_cont::Data::Nat(5),
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
                        curios_cont::ValueName::from("cat"),
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(vec![
                            curios_cont::ValueName::from("B1"),
                            curios_cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        curios_cont::ValueName::from("s1"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("s2"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            curios_cont::ValueName::from("s1"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("a"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            curios_cont::ValueName::from("s2"),
                            curios_cont::ValueName::from("ZERO"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("b"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            curios_cont::ValueName::from("s2"),
                            curios_cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("l"),
                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                            curios_cont::ValueName::from("s2"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("ab"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("a"),
                            curios_cont::ValueName::from("b"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("sum"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("ab"),
                            curios_cont::ValueName::from("l"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                    code: curios_cont::ValueName::from("sum"),
                    resume: curios_cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 9);
}

/// A never-materialized window crosses the host boundary: the wire force at
/// the `io_write` call site copies exactly the window out of the base, so the
/// host sees just those bytes.
#[test]
fn writes_a_window_to_the_host() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(vec![1]),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(b"hel".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(b"lo".to_vec()),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("FOUR"),
        curios_cont::Data::Nat(4),
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
                        curios_cont::ValueName::from("cat"),
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(vec![
                            curios_cont::ValueName::from("B1"),
                            curios_cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        curios_cont::ValueName::from("win"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("FOUR"),
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
                        curios_cont::ValueName::from("win"),
                    ],
                    resume: curios_cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(printed(&module), "ell");
}

// Regression: `Code::BinEql`'s inline byte loop must re-zero its cursor local
// on every execution. Wasm zeroes locals once per function *activation*, so a
// `Bin.eql` sitting inside a converted loop (or any re-entered block) starts
// its second run with the previous run's cursor — here the first compare
// advances it past the equal leading byte, and a stale cursor then judges the
// distinct one-byte operands equal because it starts at (not below) their
// length.
#[test]
fn bin_eql_rezeroes_its_cursor_across_block_reentries() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(vec![1, 2]),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(vec![1, 3]),
    );
    module.add_const(
        curios_cont::ValueName::from("C"),
        curios_cont::Data::Bin(vec![4]),
    );
    module.add_const(
        curios_cont::ValueName::from("D"),
        curios_cont::Data::Bin(vec![5]),
    );
    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Nat(0),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
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
                    curios_cont::BlockName::from("step"),
                    curios_cont::Block {
                        params: vec![
                            curios_cont::ValueName::from("x"),
                            curios_cont::ValueName::from("y"),
                            curios_cont::ValueName::from("again"),
                        ],
                        region: curios_cont::Region {
                            preallocs: vec![],
                            values: vec![(
                                curios_cont::ValueName::from("eq"),
                                curios_cont::Value::Eval(curios_cont::Code::BinEql(
                                    curios_cont::ValueName::from("x"),
                                    curios_cont::ValueName::from("y"),
                                )),
                            )],
                            blocks: vec![
                                (
                                    curios_cont::BlockName::from("rerun"),
                                    curios_cont::Block {
                                        params: vec![],
                                        region: curios_cont::Region {
                                            preallocs: vec![],
                                            values: vec![],
                                            blocks: vec![],
                                            tail: curios_cont::Tail::Jump(
                                                curios_cont::JumpTarget {
                                                    target: curios_cont::BlockName::from("step"),
                                                    params: vec![
                                                        curios_cont::ValueName::from("C"),
                                                        curios_cont::ValueName::from("D"),
                                                        curios_cont::ValueName::from("ZERO"),
                                                    ],
                                                },
                                            ),
                                        },
                                    },
                                ),
                                (
                                    curios_cont::BlockName::from("out"),
                                    curios_cont::Block {
                                        params: vec![],
                                        region: curios_cont::Region {
                                            preallocs: vec![],
                                            values: vec![],
                                            blocks: vec![],
                                            tail: curios_cont::Tail::Host(
                                                curios_cont::HostTarget::IoExit {
                                                    code: curios_cont::ValueName::from("eq"),
                                                    resume: curios_cont::BlockName::from("r"),
                                                },
                                            ),
                                        },
                                    },
                                ),
                            ],
                            tail: curios_cont::Tail::Match(curios_cont::MatchTarget {
                                operand: curios_cont::ValueName::from("again"),
                                cases: [(
                                    0,
                                    curios_cont::JumpTarget {
                                        target: curios_cont::BlockName::from("out"),
                                        params: vec![],
                                    },
                                )]
                                .into_iter()
                                .collect(),
                                default: Some(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("rerun"),
                                    params: vec![],
                                }),
                            }),
                        },
                    },
                )],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("step"),
                    params: vec![
                        curios_cont::ValueName::from("A"),
                        curios_cont::ValueName::from("B"),
                        curios_cont::ValueName::from("ONE"),
                    ],
                }),
            },
        },
    );

    // `[1, 2] == [1, 3]` advances the cursor past the shared leading byte;
    // `[4] == [5]` must still answer false (exit code 0).
    assert_eq!(i32_result(&module), 0);
}
