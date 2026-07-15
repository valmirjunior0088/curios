use {
    super::*,
    curios_base::{Grain, PackedBin},
};

#[test]
fn lowers_and_runs_bin_len() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
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
                        Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
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
                        Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("HELLO"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
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
                            Grain::X,
                            curios_cont::ValueName::from("HELLO"),
                            curios_cont::ValueName::from("BANG"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
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
                        Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hello".to_vec())),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"world".to_vec())),
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
                        Grain::X,
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
fn lowers_and_runs_packed_bits_get_across_byte_boundary() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("BITS"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([
                false, true, false, true, false, true, false, true, true, false,
            ]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("IDX"),
        curios_cont::Data::Nat(8),
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
                        Grain::B,
                        curios_cont::ValueName::from("BITS"),
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

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_packed_bits_windows_and_nodes() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("LEFT"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([false, true, false, true, false]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("RIGHT"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([true, true, false, false, true, false]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("EXPECTED"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([true, false, true, true, false]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("START"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("END"),
        curios_cont::Data::Nat(9),
    );
    module.add_const(
        curios_cont::ValueName::from("INNER_START"),
        curios_cont::Data::Nat(2),
    );
    module.add_const(
        curios_cont::ValueName::from("INNER_END"),
        curios_cont::Data::Nat(7),
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
                        curios_cont::ValueName::from("joined"),
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                            Grain::B,
                            vec![
                                curios_cont::ValueName::from("LEFT"),
                                curios_cont::ValueName::from("RIGHT"),
                            ],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("window"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            Grain::B,
                            curios_cont::ValueName::from("joined"),
                            curios_cont::ValueName::from("START"),
                            curios_cont::ValueName::from("END"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("subwindow"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            Grain::B,
                            curios_cont::ValueName::from("window"),
                            curios_cont::ValueName::from("INNER_START"),
                            curios_cont::ValueName::from("INNER_END"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::BinEql(
                            Grain::B,
                            curios_cont::ValueName::from("subwindow"),
                            curios_cont::ValueName::from("EXPECTED"),
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

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_function_local_non_byte_aligned_bit_literal() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("IDX"),
        curios_cont::Data::Nat(8),
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
                        curios_cont::ValueName::from("bits"),
                        curios_cont::Value::Pure(curios_cont::Data::Bin(
                            Grain::B,
                            PackedBin::from_bits([
                                false, true, false, true, false, true, false, true, true, false,
                            ]),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("len"),
                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                            Grain::B,
                            curios_cont::ValueName::from("bits"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("bit"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::B,
                            curios_cont::ValueName::from("bits"),
                            curios_cont::ValueName::from("IDX"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("len"),
                            curios_cont::ValueName::from("bit"),
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

    assert_eq!(i32_result(&module), 11);
}

#[test]
fn packed_bits_equality_ignores_final_byte_padding() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bytes(vec![0xff]).window(0, 1).unwrap(),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(Grain::B, PackedBin::from_bits([true])),
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
                        Grain::B,
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
fn packed_bits_reuse_node_cache_after_first_read() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("LEFT"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([true, false, true, false, true]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("RIGHT"),
        curios_cont::Data::Bin(
            Grain::B,
            PackedBin::from_bits([false, true, false, true, true]),
        ),
    );
    module.add_const(
        curios_cont::ValueName::from("ZERO"),
        curios_cont::Data::Nat(0),
    );
    module.add_const(
        curios_cont::ValueName::from("NINE"),
        curios_cont::Data::Nat(9),
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
                        curios_cont::ValueName::from("joined"),
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                            Grain::B,
                            vec![
                                curios_cont::ValueName::from("LEFT"),
                                curios_cont::ValueName::from("RIGHT"),
                            ],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("first"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::B,
                            curios_cont::ValueName::from("joined"),
                            curios_cont::ValueName::from("ZERO"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("last"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::B,
                            curios_cont::ValueName::from("joined"),
                            curios_cont::ValueName::from("NINE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::NatAdd(
                            curios_cont::ValueName::from("first"),
                            curios_cont::ValueName::from("last"),
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

    let wat = curios_cont::into_wasm(&module).to_string();
    assert!(
        wat.contains("func $bits/force"),
        "bit forcing helper was not emitted:\n{wat}"
    );
    assert!(
        !wat.contains("func $bin/force"),
        "unused byte forcing helper was emitted:\n{wat}"
    );
    assert_eq!(i32_result(&module), 2);
}

#[test]
fn bits_eql_resets_scratch_on_repeated_helper_entry() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    for (name, bits) in [
        ("A", vec![true, false, true, true]),
        ("B", vec![true, false, true, false]),
        ("C", vec![true]),
        ("D", vec![false]),
    ] {
        module.add_const(
            curios_cont::ValueName::from(name),
            curios_cont::Data::Bin(Grain::B, PackedBin::from_bits(bits)),
        );
    }
    module.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        curios_cont::ValueName::from("first"),
                        curios_cont::Value::Eval(curios_cont::Code::BinEql(
                            Grain::B,
                            curios_cont::ValueName::from("A"),
                            curios_cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("result"),
                        curios_cont::Value::Eval(curios_cont::Code::BinEql(
                            Grain::B,
                            curios_cont::ValueName::from("C"),
                            curios_cont::ValueName::from("D"),
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

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn bin_eql_rezeroes_its_cursor_across_block_reentries() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("A"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])),
    );
    module.add_const(
        curios_cont::ValueName::from("B"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 3])),
    );
    module.add_const(
        curios_cont::ValueName::from("C"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![4])),
    );
    module.add_const(
        curios_cont::ValueName::from("D"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![5])),
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
                                    Grain::X,
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
