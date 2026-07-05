use {
    super::*,
    crate::cont::{self, to_wasm},
};

#[test]
fn lowers_and_runs_nat_add() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Nat(4));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatAdd(
                        cont::ValueName::from("THREE"),
                        cont::ValueName::from("FOUR"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_arr_len() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Lst(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
        ]),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::LstLen(cont::ValueName::from("LST"))),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_arr_get() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Lst(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
        ]),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::LstGet(
                        cont::ValueName::from("LST"),
                        cont::ValueName::from("ONE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_arr_slice() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("THREE_IDX"), cont::Data::Nat(3));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Lst(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
            cont::ValueName::from("FIVE"),
        ]),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("slice"),
                        cont::Value::Eval(cont::Code::LstSlice(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("THREE_IDX"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::LstLen(cont::ValueName::from("slice"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_arr_concat() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Nat(2));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(
        cont::ValueName::from("LST1"),
        cont::Data::Lst(vec![cont::ValueName::from("ONE")]),
    );
    module.add_const(
        cont::ValueName::from("LST2"),
        cont::Data::Lst(vec![
            cont::ValueName::from("TWO"),
            cont::ValueName::from("THREE"),
        ]),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("concat"),
                        cont::Value::Eval(cont::Code::LstConcat(vec![
                            cont::ValueName::from("LST1"),
                            cont::ValueName::from("LST2"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::LstLen(cont::ValueName::from("concat"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_flt_floor() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.9));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltFloor(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_flt_ceil() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltCeil(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_flt_trunc() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(-2.9));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltTrunc(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), -2.0);
}

#[test]
fn lowers_and_runs_flt_nearest() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltNearest(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_nat_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatDiv(
                        cont::ValueName::from("TEN"),
                        cont::ValueName::from("THREE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_rem() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatRem(
                        cont::ValueName::from("TEN"),
                        cont::ValueName::from("THREE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_nat_lt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatLt(
                        cont::ValueName::from("THREE"),
                        cont::ValueName::from("FIVE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neg() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Int(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("zero"),
                        cont::Value::Pure(cont::Data::Int(0)),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntSub(
                            cont::ValueName::from("zero"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(int_result(&module), -5);
}

#[test]
fn lowers_and_runs_int_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("NEG7"), cont::Data::Int(-7));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::IntDiv(
                        cont::ValueName::from("NEG7"),
                        cont::ValueName::from("TWO"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(int_result(&module), -3);
}

#[test]
fn lowers_and_runs_int_lt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::IntLt(
                        cont::ValueName::from("NEG1"),
                        cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltDiv(
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("FOUR"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 0.25);
}

#[test]
fn lowers_and_runs_flt_eql() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(1.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::FltEql(
                        cont::ValueName::from("A"),
                        cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_sqrt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltSqrt(cont::ValueName::from("FOUR"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_int_to_flt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Int(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntToFlt(cont::ValueName::from("THREE"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_nat_to_flt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatToFlt(cont::ValueName::from("FIVE"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 5.0);
}

#[test]
fn lowers_and_runs_flt_to_int() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE_SEVEN"), cont::Data::Flt(3.7));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::FltToInt(cont::ValueName::from("THREE_SEVEN"))),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_to_int() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatToInt(cont::ValueName::from("SEVEN"))),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::NatNeq(
                        cont::ValueName::from("THREE"),
                        cont::ValueName::from("FIVE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
    module.add_const(cont::ValueName::from("NEG1B"), cont::Data::Int(-1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::IntNeq(
                        cont::ValueName::from("NEG1"),
                        cont::ValueName::from("NEG1B"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_flt_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Flt(2.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::FltNeq(
                        cont::ValueName::from("ONE"),
                        cont::ValueName::from("TWO"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_min() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMin(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 1.5);
}

#[test]
fn lowers_and_runs_flt_max() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMax(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToLeBin(cont::ValueName::from("result"))),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("str"),
                    ],
                    resume: cont::BlockName::from("io_done"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.5);
}

#[test]
fn lowers_and_runs_bin_len() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from("HELLO"))),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 5);
}

#[test]
fn lowers_and_runs_bin_get() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(cont::ValueName::from("IDX"), cont::Data::Nat(1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::BinGet(
                        cont::ValueName::from("HELLO"),
                        cont::ValueName::from("IDX"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'e' as i32);
}

#[test]
fn lowers_and_runs_bin_append() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(cont::ValueName::from("BANG"), cont::Data::Nat(b'!' as u32));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("appended"),
                        cont::Value::Eval(cont::Code::BinAppend(
                            cont::ValueName::from("HELLO"),
                            cont::ValueName::from("BANG"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("appended"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'!' as i32);
}

#[test]
fn lowers_and_runs_bin_eql_equal() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("A"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        cont::ValueName::from("B"),
        cont::Data::Bin(b"hello".to_vec()),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::BinEql(
                        cont::ValueName::from("A"),
                        cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_bin_eql_unequal() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("A"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        cont::ValueName::from("B"),
        cont::Data::Bin(b"world".to_vec()),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::BinEql(
                        cont::ValueName::from("A"),
                        cont::ValueName::from("B"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_arr_append() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("NINE"), cont::Data::Nat(9));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Lst(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
        ]),
    );
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Nat(2));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("appended"),
                        cont::Value::Eval(cont::Code::LstAppend(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("NINE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::LstGet(
                            cont::ValueName::from("appended"),
                            cont::ValueName::from("TWO"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 9);
}

fn nat_op_module(op: cont::Code, left: u32, right: u32) -> cont::Module {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));
    module.add_const(cont::ValueName::from("LEFT"), cont::Data::Nat(left));
    module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Nat(right));
    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(cont::ValueName::from("result"), cont::Value::Eval(op))],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
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
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        6,
        7,
    );
    assert_eq!(i32_result(&module), 42);
}

#[test]
fn lowers_and_runs_nat_sub_monus() {
    let module = nat_op_module(
        cont::Code::NatSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        3,
        7,
    );
    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_nat_sub() {
    let module = nat_op_module(
        cont::Code::NatSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        10,
        3,
    );
    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_add_at_boundary() {
    let module = nat_op_module(
        cont::Code::NatAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31 - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn lowers_and_runs_nat_mul_at_boundary() {
    let module = nat_op_module(
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn nat_add_overflow_traps() {
    let module = nat_op_module(
        cont::Code::NatAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn nat_mul_overflow_traps() {
    let module = nat_op_module(
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        2,
    );
    assert!(traps(&module));
}

fn int_op_module(op: cont::Code, left: i32, right: i32) -> cont::Module {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));
    module.add_const(cont::ValueName::from("LEFT"), cont::Data::Int(left));
    module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Int(right));
    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(cont::ValueName::from("result"), cont::Value::Eval(op))],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
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
        cont::Code::IntAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_add_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn int_sub_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn lowers_and_runs_int_mul_at_boundary() {
    let module = int_op_module(
        cont::Code::IntMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_mul_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
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
        cont::Code::IntDiv(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        -1,
    );
    assert!(traps(&module));
}

#[test]
fn flt_to_int_overflow_traps() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));
    module.add_const(
        cont::ValueName::from("TOO_BIG"),
        cont::Data::Flt((MAX_INT as f32) + 1.0),
    );
    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::FltToInt(cont::ValueName::from("TOO_BIG"))),
                )],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("result"),
                    resume: cont::BlockName::from("r"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("b"),
                    cont::Block {
                        params: vec![cont::ValueName::from("x")],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Host(cont::HostTarget::IoExit {
                                code: cont::ValueName::from("x"),
                                resume: cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("b"),
                    params: vec![cont::ValueName::from("SEVEN")],
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("DEPTH"), cont::Data::Nat(100_000));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("EMPTY"), cont::Data::Bin(vec![]));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![
                    (
                        cont::BlockName::from("step"),
                        cont::Block {
                            params: vec![cont::ValueName::from("i"), cont::ValueName::from("acc")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Match(cont::MatchTarget {
                                    operand: cont::ValueName::from("i"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        cont::JumpTarget {
                                            target: cont::BlockName::from("read"),
                                            params: vec![cont::ValueName::from("acc")],
                                        },
                                    )]),
                                    default: Some(cont::JumpTarget {
                                        target: cont::BlockName::from("grow"),
                                        params: vec![
                                            cont::ValueName::from("i"),
                                            cont::ValueName::from("acc"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("grow"),
                        cont::Block {
                            params: vec![cont::ValueName::from("j"), cont::ValueName::from("cur")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("next_i"),
                                        cont::Value::Eval(cont::Code::NatSub(
                                            cont::ValueName::from("j"),
                                            cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("next_acc"),
                                        cont::Value::Eval(cont::Code::BinAppend(
                                            cont::ValueName::from("cur"),
                                            cont::ValueName::from("SEVEN"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("step"),
                                    params: vec![
                                        cont::ValueName::from("next_i"),
                                        cont::ValueName::from("next_acc"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("read"),
                        cont::Block {
                            params: vec![cont::ValueName::from("built")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("len"),
                                        cont::Value::Eval(cont::Code::BinLen(
                                            cont::ValueName::from("built"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("idx"),
                                        cont::Value::Eval(cont::Code::NatSub(
                                            cont::ValueName::from("len"),
                                            cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("byte"),
                                        cont::Value::Eval(cont::Code::BinGet(
                                            cont::ValueName::from("built"),
                                            cont::ValueName::from("idx"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                                    code: cont::ValueName::from("byte"),
                                    resume: cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("step"),
                    params: vec![
                        cont::ValueName::from("DEPTH"),
                        cont::ValueName::from("EMPTY"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("B1"), cont::Data::Bin(vec![1, 2]));
    module.add_const(cont::ValueName::from("B2"), cont::Data::Bin(vec![3, 4, 5]));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("cat"),
                        cont::Value::Eval(cont::Code::BinConcat(vec![
                            cont::ValueName::from("B1"),
                            cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("a"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("cat"),
                            cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("b"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("cat"),
                            cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("len"),
                        cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from("cat"))),
                    ),
                    (
                        cont::ValueName::from("ab"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("a"),
                            cont::ValueName::from("b"),
                        )),
                    ),
                    (
                        cont::ValueName::from("sum"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("ab"),
                            cont::ValueName::from("len"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("sum"),
                    resume: cont::BlockName::from("r"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("B1"),
        cont::Data::Bin(b"hel".to_vec()),
    );
    module.add_const(cont::ValueName::from("B2"), cont::Data::Bin(b"lo".to_vec()));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("cat"),
                    cont::Value::Eval(cont::Code::BinConcat(vec![
                        cont::ValueName::from("B1"),
                        cont::ValueName::from("B2"),
                    ])),
                )],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("cat"),
                    ],
                    resume: cont::BlockName::from("io_done"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("DEPTH"), cont::Data::Nat(100_000));
    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Nat(0));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("EMPTY"), cont::Data::Bin(vec![]));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![
                    (
                        cont::BlockName::from("step"),
                        cont::Block {
                            params: vec![cont::ValueName::from("i"), cont::ValueName::from("acc")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Match(cont::MatchTarget {
                                    operand: cont::ValueName::from("i"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        cont::JumpTarget {
                                            target: cont::BlockName::from("peel"),
                                            params: vec![
                                                cont::ValueName::from("acc"),
                                                cont::ValueName::from("ZERO"),
                                            ],
                                        },
                                    )]),
                                    default: Some(cont::JumpTarget {
                                        target: cont::BlockName::from("grow"),
                                        params: vec![
                                            cont::ValueName::from("i"),
                                            cont::ValueName::from("acc"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("grow"),
                        cont::Block {
                            params: vec![cont::ValueName::from("j"), cont::ValueName::from("cur")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("next_i"),
                                        cont::Value::Eval(cont::Code::NatSub(
                                            cont::ValueName::from("j"),
                                            cont::ValueName::from("ONE"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("next_acc"),
                                        cont::Value::Eval(cont::Code::BinAppend(
                                            cont::ValueName::from("cur"),
                                            cont::ValueName::from("SEVEN"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("step"),
                                    params: vec![
                                        cont::ValueName::from("next_i"),
                                        cont::ValueName::from("next_acc"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("peel"),
                        cont::Block {
                            params: vec![cont::ValueName::from("b"), cont::ValueName::from("sum")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    cont::ValueName::from("len"),
                                    cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from(
                                        "b",
                                    ))),
                                )],
                                blocks: vec![],
                                tail: cont::Tail::Match(cont::MatchTarget {
                                    operand: cont::ValueName::from("len"),
                                    cases: std::collections::BTreeMap::from([(
                                        0,
                                        cont::JumpTarget {
                                            target: cont::BlockName::from("done"),
                                            params: vec![cont::ValueName::from("sum")],
                                        },
                                    )]),
                                    default: Some(cont::JumpTarget {
                                        target: cont::BlockName::from("chop"),
                                        params: vec![
                                            cont::ValueName::from("b"),
                                            cont::ValueName::from("sum"),
                                            cont::ValueName::from("len"),
                                        ],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("chop"),
                        cont::Block {
                            params: vec![
                                cont::ValueName::from("b2"),
                                cont::ValueName::from("s2"),
                                cont::ValueName::from("l2"),
                            ],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("byte"),
                                        cont::Value::Eval(cont::Code::BinGet(
                                            cont::ValueName::from("b2"),
                                            cont::ValueName::from("ZERO"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("tail"),
                                        cont::Value::Eval(cont::Code::BinSlice(
                                            cont::ValueName::from("b2"),
                                            cont::ValueName::from("ONE"),
                                            cont::ValueName::from("l2"),
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("s3"),
                                        cont::Value::Eval(cont::Code::NatAdd(
                                            cont::ValueName::from("s2"),
                                            cont::ValueName::from("byte"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("peel"),
                                    params: vec![
                                        cont::ValueName::from("tail"),
                                        cont::ValueName::from("s3"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("done"),
                        cont::Block {
                            params: vec![cont::ValueName::from("total")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                                    code: cont::ValueName::from("total"),
                                    resume: cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("step"),
                    params: vec![
                        cont::ValueName::from("DEPTH"),
                        cont::ValueName::from("EMPTY"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(cont::ValueName::from("B1"), cont::Data::Bin(vec![1, 2, 3]));
    module.add_const(cont::ValueName::from("B2"), cont::Data::Bin(vec![4, 5, 6]));
    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Nat(0));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("cat"),
                        cont::Value::Eval(cont::Code::BinConcat(vec![
                            cont::ValueName::from("B1"),
                            cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("s1"),
                        cont::Value::Eval(cont::Code::BinSlice(
                            cont::ValueName::from("cat"),
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("s2"),
                        cont::Value::Eval(cont::Code::BinSlice(
                            cont::ValueName::from("s1"),
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("a"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("s2"),
                            cont::ValueName::from("ZERO"),
                        )),
                    ),
                    (
                        cont::ValueName::from("b"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("s2"),
                            cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("l"),
                        cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from("s2"))),
                    ),
                    (
                        cont::ValueName::from("ab"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("a"),
                            cont::ValueName::from("b"),
                        )),
                    ),
                    (
                        cont::ValueName::from("sum"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("ab"),
                            cont::ValueName::from("l"),
                        )),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoExit {
                    code: cont::ValueName::from("sum"),
                    resume: cont::BlockName::from("r"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Bin(vec![1]));

    module.add_const(
        cont::ValueName::from("B1"),
        cont::Data::Bin(b"hel".to_vec()),
    );
    module.add_const(cont::ValueName::from("B2"), cont::Data::Bin(b"lo".to_vec()));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Nat(4));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("cat"),
                        cont::Value::Eval(cont::Code::BinConcat(vec![
                            cont::ValueName::from("B1"),
                            cont::ValueName::from("B2"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("win"),
                        cont::Value::Eval(cont::Code::BinSlice(
                            cont::ValueName::from("cat"),
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("FOUR"),
                        )),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("io_done"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("io_status"),
                            cont::ValueName::from("io_written"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("io_status")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Host(cont::HostTarget::Foreign {
                    function: foreign_write(),
                    operands: vec![
                        cont::ValueName::from("STDOUT"),
                        cont::ValueName::from("win"),
                    ],
                    resume: cont::BlockName::from("io_done"),
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
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("A"), cont::Data::Bin(vec![1, 2]));
    module.add_const(cont::ValueName::from("B"), cont::Data::Bin(vec![1, 3]));
    module.add_const(cont::ValueName::from("C"), cont::Data::Bin(vec![4]));
    module.add_const(cont::ValueName::from("D"), cont::Data::Bin(vec![5]));
    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Nat(0));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("step"),
                    cont::Block {
                        params: vec![
                            cont::ValueName::from("x"),
                            cont::ValueName::from("y"),
                            cont::ValueName::from("again"),
                        ],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![(
                                cont::ValueName::from("eq"),
                                cont::Value::Eval(cont::Code::BinEql(
                                    cont::ValueName::from("x"),
                                    cont::ValueName::from("y"),
                                )),
                            )],
                            blocks: vec![
                                (
                                    cont::BlockName::from("rerun"),
                                    cont::Block {
                                        params: vec![],
                                        region: cont::Region {
                                            preallocs: vec![],
                                            values: vec![],
                                            blocks: vec![],
                                            tail: cont::Tail::Jump(cont::JumpTarget {
                                                target: cont::BlockName::from("step"),
                                                params: vec![
                                                    cont::ValueName::from("C"),
                                                    cont::ValueName::from("D"),
                                                    cont::ValueName::from("ZERO"),
                                                ],
                                            }),
                                        },
                                    },
                                ),
                                (
                                    cont::BlockName::from("out"),
                                    cont::Block {
                                        params: vec![],
                                        region: cont::Region {
                                            preallocs: vec![],
                                            values: vec![],
                                            blocks: vec![],
                                            tail: cont::Tail::Host(cont::HostTarget::IoExit {
                                                code: cont::ValueName::from("eq"),
                                                resume: cont::BlockName::from("r"),
                                            }),
                                        },
                                    },
                                ),
                            ],
                            tail: cont::Tail::Match(cont::MatchTarget {
                                operand: cont::ValueName::from("again"),
                                cases: [(
                                    0,
                                    cont::JumpTarget {
                                        target: cont::BlockName::from("out"),
                                        params: vec![],
                                    },
                                )]
                                .into_iter()
                                .collect(),
                                default: Some(cont::JumpTarget {
                                    target: cont::BlockName::from("rerun"),
                                    params: vec![],
                                }),
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("step"),
                    params: vec![
                        cont::ValueName::from("A"),
                        cont::ValueName::from("B"),
                        cont::ValueName::from("ONE"),
                    ],
                }),
            },
        },
    );

    // `[1, 2] == [1, 3]` advances the cursor past the shared leading byte;
    // `[4] == [5]` must still answer false (exit code 0).
    assert_eq!(i32_result(&module), 0);
}
