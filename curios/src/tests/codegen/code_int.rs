use super::*;

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
