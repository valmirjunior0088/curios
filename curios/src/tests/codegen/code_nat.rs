use super::*;

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
