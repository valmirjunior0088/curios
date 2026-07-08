use super::*;

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
