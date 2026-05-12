use curios::cont::{
    Block, BlockName, CallTarget, CaseTarget, Clsr, ClsrName, Code, Data, Func, FuncName,
    JumpTarget, Module, Region, Tail, Value, ValueName, to_wasm,
};

fn main() {
    let mut module = Module::new();

    module.add_const(ValueName::from("ZERO"), Data::Int(0));

    module.add_const(ValueName::from("ONE"), Data::Int(1));

    module.add_const(ValueName::from("ONE_HALF"), Data::Flt(0.5));

    module.add_const(ValueName::from("HELLO"), Data::Bin(b"hello".to_vec()));

    module.add_clsr(
        ClsrName::from("RouteByZero"),
        Clsr {
            fields: vec![],
            params: vec![ValueName::from("x")],
            resume: BlockName::from("r"),
            region: Region {
                values: vec![(
                    ValueName::from("is_zero"),
                    Value::Eval(Code::IntEql(ValueName::from("x"), ValueName::from("ZERO"))),
                )],
                blocks: vec![
                    (
                        BlockName::from("on_zero"),
                        Block {
                            params: vec![ValueName::from("tag")],
                            region: Region {
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Jump(JumpTarget {
                                    target: BlockName::from("join"),
                                    params: vec![ValueName::from("ZERO")],
                                }),
                            },
                        },
                    ),
                    (
                        BlockName::from("on_non_zero"),
                        Block {
                            params: vec![ValueName::from("tag")],
                            region: Region {
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Jump(JumpTarget {
                                    target: BlockName::from("join"),
                                    params: vec![ValueName::from("x")],
                                }),
                            },
                        },
                    ),
                    (
                        BlockName::from("join"),
                        Block {
                            params: vec![ValueName::from("selected")],
                            region: Region {
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Jump(JumpTarget {
                                    target: BlockName::from("r"),
                                    params: vec![ValueName::from("selected")],
                                }),
                            },
                        },
                    ),
                ],
                tail: Tail::Case(CaseTarget {
                    operand: ValueName::from("is_zero"),
                    targets: vec![JumpTarget {
                        target: BlockName::from("on_zero"),
                        params: vec![ValueName::from("x")],
                    }],
                    default: Some(JumpTarget {
                        target: BlockName::from("on_non_zero"),
                        params: vec![ValueName::from("x")],
                    }),
                }),
            },
        },
    );

    module.add_func(
        FuncName::from("main"),
        Func {
            params: vec![ValueName::from("n")],
            resume: BlockName::from("r"),
            region: Region {
                values: vec![
                    (
                        ValueName::from("thk"),
                        Value::Pure(Data::Clsr(ClsrName::from("RouteByZero"), vec![])),
                    ),
                    (
                        ValueName::from("scale"),
                        Value::Eval(Code::FltMul(
                            ValueName::from("ONE_HALF"),
                            ValueName::from("ONE_HALF"),
                        )),
                    ),
                ],
                blocks: vec![
                    (
                        BlockName::from("after_call"),
                        Block {
                            params: vec![ValueName::from("result")],
                            region: Region {
                                values: vec![
                                    (
                                        ValueName::from("pair"),
                                        Value::Pure(Data::Tpl(vec![
                                            ValueName::from("result"),
                                            ValueName::from("ONE"),
                                        ])),
                                    ),
                                    (
                                        ValueName::from("result_again"),
                                        Value::Eval(Code::TplGet(0, ValueName::from("pair"))),
                                    ),
                                    (
                                        ValueName::from("fallback_one"),
                                        Value::Eval(Code::TplGet(1, ValueName::from("pair"))),
                                    ),
                                    (
                                        ValueName::from("result_is_zero"),
                                        Value::Eval(Code::IntEql(
                                            ValueName::from("result_again"),
                                            ValueName::from("ZERO"),
                                        )),
                                    ),
                                ],
                                blocks: vec![
                                    (
                                        BlockName::from("pick_zero"),
                                        Block {
                                            params: vec![ValueName::from("tag")],
                                            region: Region {
                                                values: vec![],
                                                blocks: vec![],
                                                tail: Tail::Jump(JumpTarget {
                                                    target: BlockName::from("finalize"),
                                                    params: vec![ValueName::from("ZERO")],
                                                }),
                                            },
                                        },
                                    ),
                                    (
                                        BlockName::from("pick_one"),
                                        Block {
                                            params: vec![ValueName::from("tag")],
                                            region: Region {
                                                values: vec![],
                                                blocks: vec![],
                                                tail: Tail::Jump(JumpTarget {
                                                    target: BlockName::from("finalize"),
                                                    params: vec![ValueName::from("fallback_one")],
                                                }),
                                            },
                                        },
                                    ),
                                    (
                                        BlockName::from("finalize"),
                                        Block {
                                            params: vec![ValueName::from("out")],
                                            region: Region {
                                                values: vec![],
                                                blocks: vec![],
                                                tail: Tail::Jump(JumpTarget {
                                                    target: BlockName::from("r"),
                                                    params: vec![ValueName::from("out")],
                                                }),
                                            },
                                        },
                                    ),
                                ],
                                tail: Tail::Case(CaseTarget {
                                    operand: ValueName::from("result_is_zero"),
                                    targets: vec![JumpTarget {
                                        target: BlockName::from("pick_zero"),
                                        params: vec![ValueName::from("result")],
                                    }],
                                    default: Some(JumpTarget {
                                        target: BlockName::from("pick_one"),
                                        params: vec![ValueName::from("result")],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        BlockName::from("dead_path"),
                        Block {
                            params: vec![],
                            region: Region {
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Jump(JumpTarget {
                                    target: BlockName::from("r"),
                                    params: vec![ValueName::from("ZERO")],
                                }),
                            },
                        },
                    ),
                ],
                tail: Tail::Call(CallTarget::Indirect {
                    target: ValueName::from("thk"),
                    params: vec![ValueName::from("n")],
                    resume: BlockName::from("after_call"),
                }),
            },
        },
    );

    println!("{}", to_wasm(&module));
}
