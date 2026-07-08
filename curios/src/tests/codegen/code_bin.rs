use super::*;

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
