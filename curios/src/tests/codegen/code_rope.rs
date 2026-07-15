use {
    super::*,
    curios_base::{Grain, PackedBin},
    curios_cont::into_wasm,
};

#[test]
fn single_block_region_lowers_without_dispatch_loop() {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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

    let wat = into_wasm(&module).to_string();
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

fn assert_forces_deep_rope_chain(grain: Grain, atom: u32, read_early: bool) {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
        curios_cont::Data::Nat(atom),
    );
    module.add_const(
        curios_cont::ValueName::from("EMPTY"),
        curios_cont::Data::Bin(grain, PackedBin::empty()),
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
                                            grain,
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
                                            grain,
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
                                            grain,
                                            curios_cont::ValueName::from("built"),
                                            curios_cont::ValueName::from(if read_early {
                                                "ONE"
                                            } else {
                                                "idx"
                                            }),
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

    assert_eq!(i32_result(&module), atom as i32);
}

/// A 100k-deep byte append chain reads back through the iterative force walk.
#[test]
fn forces_deep_rope_chains_iteratively() {
    assert_forces_deep_rope_chain(Grain::X, 7, false);
}

/// A 100k-deep bit append chain forces through the explicit worklist and reads
/// an early logical bit without touching the wasm call stack recursively.
#[test]
fn forces_deep_bit_rope_chains_iteratively() {
    assert_forces_deep_rope_chain(Grain::B, 1, true);
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![3, 4, 5])),
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
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                            Grain::X,
                            vec![
                                curios_cont::ValueName::from("B1"),
                                curios_cont::ValueName::from("B2"),
                            ],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("a"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::X,
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("b"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::X,
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("len"),
                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                            Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hel".to_vec())),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"lo".to_vec())),
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
                    curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                        Grain::X,
                        vec![
                            curios_cont::ValueName::from("B1"),
                            curios_cont::ValueName::from("B2"),
                        ],
                    )),
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

fn assert_peels_rope_through_o1_windows(grain: Grain, atom: u32) {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));

    module.add_const(
        curios_cont::ValueName::from("STDOUT"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
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
        curios_cont::Data::Nat(atom),
    );
    module.add_const(
        curios_cont::ValueName::from("EMPTY"),
        curios_cont::Data::Bin(grain, PackedBin::empty()),
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
                                            grain,
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
                                        grain,
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
                                            grain,
                                            curios_cont::ValueName::from("b2"),
                                            curios_cont::ValueName::from("ZERO"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("tail"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                                            grain,
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

    assert_eq!(i32_result(&module), (100_000 * atom) as i32);
}

/// A head/tail peel loop over a 100k-byte rope is linear by construction: the
/// first read forces once, then every tail is an O(1) settled window.
#[test]
fn peels_a_rope_through_o1_windows() {
    assert_peels_rope_through_o1_windows(Grain::X, 7);
}

/// The bit-grain mirror peels all 100k logical bits without recursive forcing
/// or copying each suffix.
#[test]
fn peels_a_bit_rope_through_o1_windows() {
    assert_peels_rope_through_o1_windows(Grain::B, 1);
}

fn assert_compares_deep_rope(grain: Grain, atom: u32) {
    let mut module = curios_cont::Module::new();
    module.set_entry(curios_cont::FuncName::from("main"));
    module.add_const(
        curios_cont::ValueName::from("DEPTH"),
        curios_cont::Data::Nat(100_000),
    );
    module.add_const(
        curios_cont::ValueName::from("ONE"),
        curios_cont::Data::Nat(1),
    );
    module.add_const(
        curios_cont::ValueName::from("ATOM"),
        curios_cont::Data::Nat(atom),
    );
    module.add_const(
        curios_cont::ValueName::from("EMPTY"),
        curios_cont::Data::Bin(grain, PackedBin::empty()),
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
                                curios_cont::ValueName::from("left"),
                                curios_cont::ValueName::from("right"),
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
                                            target: curios_cont::BlockName::from("compare"),
                                            params: vec![
                                                curios_cont::ValueName::from("left"),
                                                curios_cont::ValueName::from("right"),
                                            ],
                                        },
                                    )]),
                                    default: Some(curios_cont::JumpTarget {
                                        target: curios_cont::BlockName::from("grow"),
                                        params: vec![
                                            curios_cont::ValueName::from("i"),
                                            curios_cont::ValueName::from("left"),
                                            curios_cont::ValueName::from("right"),
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
                                curios_cont::ValueName::from("a"),
                                curios_cont::ValueName::from("b"),
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
                                        curios_cont::ValueName::from("next_a"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinAppend(
                                            grain,
                                            curios_cont::ValueName::from("a"),
                                            curios_cont::ValueName::from("ATOM"),
                                        )),
                                    ),
                                    (
                                        curios_cont::ValueName::from("next_b"),
                                        curios_cont::Value::Eval(curios_cont::Code::BinAppend(
                                            grain,
                                            curios_cont::ValueName::from("b"),
                                            curios_cont::ValueName::from("ATOM"),
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                                    target: curios_cont::BlockName::from("step"),
                                    params: vec![
                                        curios_cont::ValueName::from("next_i"),
                                        curios_cont::ValueName::from("next_a"),
                                        curios_cont::ValueName::from("next_b"),
                                    ],
                                }),
                            },
                        },
                    ),
                    (
                        curios_cont::BlockName::from("compare"),
                        curios_cont::Block {
                            params: vec![
                                curios_cont::ValueName::from("x"),
                                curios_cont::ValueName::from("y"),
                            ],
                            region: curios_cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    curios_cont::ValueName::from("equal"),
                                    curios_cont::Value::Eval(curios_cont::Code::BinEql(
                                        grain,
                                        curios_cont::ValueName::from("x"),
                                        curios_cont::ValueName::from("y"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: curios_cont::Tail::Host(curios_cont::HostTarget::IoExit {
                                    code: curios_cont::ValueName::from("equal"),
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
                        curios_cont::ValueName::from("EMPTY"),
                    ],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn compares_deep_byte_ropes_iteratively() {
    assert_compares_deep_rope(Grain::X, 7);
}

#[test]
fn compares_deep_bit_ropes_iteratively() {
    assert_compares_deep_rope(Grain::B, 1);
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2, 3])),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![4, 5, 6])),
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
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                            Grain::X,
                            vec![
                                curios_cont::ValueName::from("B1"),
                                curios_cont::ValueName::from("B2"),
                            ],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("s1"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            Grain::X,
                            curios_cont::ValueName::from("cat"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("s2"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            Grain::X,
                            curios_cont::ValueName::from("s1"),
                            curios_cont::ValueName::from("ONE"),
                            curios_cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("a"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::X,
                            curios_cont::ValueName::from("s2"),
                            curios_cont::ValueName::from("ZERO"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("b"),
                        curios_cont::Value::Eval(curios_cont::Code::BinGet(
                            Grain::X,
                            curios_cont::ValueName::from("s2"),
                            curios_cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("l"),
                        curios_cont::Value::Eval(curios_cont::Code::BinLen(
                            Grain::X,
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
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(vec![1])),
    );

    module.add_const(
        curios_cont::ValueName::from("B1"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"hel".to_vec())),
    );
    module.add_const(
        curios_cont::ValueName::from("B2"),
        curios_cont::Data::Bin(Grain::X, PackedBin::from_bytes(b"lo".to_vec())),
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
                        curios_cont::Value::Eval(curios_cont::Code::BinConcat(
                            Grain::X,
                            vec![
                                curios_cont::ValueName::from("B1"),
                                curios_cont::ValueName::from("B2"),
                            ],
                        )),
                    ),
                    (
                        curios_cont::ValueName::from("win"),
                        curios_cont::Value::Eval(curios_cont::Code::BinSlice(
                            Grain::X,
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
