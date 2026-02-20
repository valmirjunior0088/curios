use curios::cont::{
    Block, BlockName, CallTarget, CaseTarget, Clsr, ClsrName, ConstOp, ConstValue, Func, FuncName,
    JumpTarget, Module, Region, Tail, Value, ValueName,
};

fn main() {
    let mut module = Module::new();

    module.add_const(ValueName::from("BIAS"), ConstValue::Int(10));

    module.add_const(ValueName::from("ZERO"), ConstValue::Int(0));

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
                        ConstOp::IntAdd,
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
                    Value::Clsr(ClsrName::from("AddBias"), vec![ValueName::from("BIAS")]),
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
                                values: vec![],
                                blocks: vec![],
                                tail: Tail::Case(CaseTarget {
                                    operand: ValueName::from("out"),
                                    targets: vec![JumpTarget {
                                        target: BlockName::from("Zero"),
                                        params: vec![ValueName::from("out")],
                                    }],
                                    default: Some(JumpTarget {
                                        target: BlockName::from("NonZero"),
                                        params: vec![ValueName::from("out")],
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
