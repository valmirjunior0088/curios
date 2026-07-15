use {
    super::*,
    curios_base::{Grain, PackedBin},
};

#[test]
fn lowers_and_runs_lst_len() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
fn lowers_and_runs_lst_append() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
