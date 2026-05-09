use curios::cont::{
    Block, BlockName, CallTarget, CaseTarget, Clsr, ClsrName, Code, Data, Func, FuncName,
    JumpTarget, Module, Region, Tail, Value, ValueName,
};

fn main() {
    let mut module = Module::new();

    module.add_const(ValueName::from("BIAS"), Data::Int(10));

    module.add_const(ValueName::from("ZERO"), Data::Int(0));

    module.add_clsr(
        ClsrName::from("AddBias"),
        Clsr {
            fields: vec![ValueName::from("bias")],
            params: vec![ValueName::from("x")],
            resume: BlockName::from("r"),
            region: Region {
                values: vec![(
                    ValueName::from("y"),
                    Value::Eval(
                        Code::IntAdd,
                        vec![ValueName::from("x"), ValueName::from("bias")],
                    ),
                )],
                blocks: vec![],
                tail: Tail::Jump(JumpTarget {
                    target: BlockName::from("r"),
                    params: vec![ValueName::from("y")],
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
                values: vec![(
                    ValueName::from("k"),
                    Value::Pure(Data::Clsr(
                        ClsrName::from("AddBias"),
                        vec![ValueName::from("BIAS")],
                    )),
                )],
                blocks: vec![
                    (
                        BlockName::from("Zero"),
                        Block {
                            params: vec![ValueName::from("flag")],
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
                    (
                        BlockName::from("NonZero"),
                        Block {
                            params: vec![ValueName::from("flag")],
                            region: Region {
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Jump(JumpTarget {
                                    target: BlockName::from("r"),
                                    params: vec![ValueName::from("1")],
                                }),
                            },
                        },
                    ),
                    (
                        BlockName::from("After"),
                        Block {
                            params: vec![ValueName::from("out")],
                            region: Region {
                                values: vec![
                                    (
                                        ValueName::from("pair"),
                                        Value::Pure(Data::Tpl(vec![
                                            ValueName::from("out"),
                                            ValueName::from("ZERO"),
                                        ])),
                                    ),
                                    (
                                        ValueName::from("out_again"),
                                        Value::Eval(
                                            Code::TplProj(0),
                                            vec![ValueName::from("pair")],
                                        ),
                                    ),
                                    (
                                        ValueName::from("is_zero"),
                                        Value::Eval(
                                            Code::TplProj(1),
                                            vec![ValueName::from("pair")],
                                        ),
                                    ),
                                ],
                                blocks: vec![],
                                tail: Tail::Case(CaseTarget {
                                    operand: ValueName::from("is_zero"),
                                    targets: vec![JumpTarget {
                                        target: BlockName::from("Zero"),
                                        params: vec![ValueName::from("out_again")],
                                    }],
                                    default: Some(JumpTarget {
                                        target: BlockName::from("NonZero"),
                                        params: vec![ValueName::from("out_again")],
                                    }),
                                }),
                            },
                        },
                    ),
                ],
                tail: Tail::Call(CallTarget::Indirect {
                    target: ValueName::from("k"),
                    params: vec![ValueName::from("n")],
                    resume: BlockName::from("After"),
                }),
            },
        },
    );

    println!("{}", module);
}
