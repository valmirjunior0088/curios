use {curios::cont, std::collections::BTreeMap};

fn main() {
    let mut cont_module = cont::Module::new();

    cont_module.add_const(cont::ValueName::from("BIAS"), cont::Data::Int(10));

    cont_module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));

    cont_module.add_clsr(
        cont::ClsrName::from("AddBias"),
        cont::Clsr {
            fields: vec![cont::ValueName::from("bias").into()],
            params: vec![cont::ValueName::from("x").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("y"),
                    cont::Value::Eval(cont::Code::IntAdd(
                        cont::ValueName::from("x"),
                        cont::ValueName::from("bias"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("y")],
                }),
            },
        },
    );

    cont_module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("n"),
                        cont::Value::Pure(cont::Data::Int(5)),
                    ),
                    (
                        cont::ValueName::from("k"),
                        cont::Value::Pure(cont::Data::Clsr(
                            cont::ClsrName::from("AddBias"),
                            vec![cont::ValueName::from("BIAS")],
                        )),
                    ),
                ],
                blocks: vec![
                    (
                        cont::BlockName::from("Zero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("flag")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("r"),
                                    params: vec![cont::ValueName::from("ZERO")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("NonZero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("flag")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("r"),
                                    params: vec![cont::ValueName::from("flag")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("After"),
                        cont::Block {
                            params: vec![cont::ValueName::from("out")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("pair"),
                                        cont::Value::Pure(cont::Data::Tpl(vec![
                                            cont::ValueName::from("out"),
                                            cont::ValueName::from("ZERO"),
                                        ])),
                                    ),
                                    (
                                        cont::ValueName::from("out_again"),
                                        cont::Value::Eval(cont::Code::TplGet(
                                            cont::ValueName::from("pair"),
                                            0,
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("is_zero"),
                                        cont::Value::Eval(cont::Code::TplGet(
                                            cont::ValueName::from("pair"),
                                            1,
                                        )),
                                    ),
                                ],
                                blocks: vec![],
                                tail: cont::Tail::Match(cont::MatchTarget {
                                    operand: cont::ValueName::from("is_zero"),
                                    cases: BTreeMap::from([(
                                        0,
                                        cont::JumpTarget {
                                            target: cont::BlockName::from("Zero"),
                                            params: vec![cont::ValueName::from("out_again")],
                                        },
                                    )]),
                                    default: Some(cont::JumpTarget {
                                        target: cont::BlockName::from("NonZero"),
                                        params: vec![cont::ValueName::from("out_again")],
                                    }),
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                    target: cont::ValueName::from("k"),
                    params: vec![cont::ValueName::from("n")],
                    resume: cont::BlockName::from("After"),
                }),
            },
        },
    );

    cont_module.set_entry(cont::FuncName::from("main"));

    println!("=== cont ===");
    println!("{cont_module}");

    let wasm_module = cont::to_wasm(&cont_module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios_rt::OsHost::new()).unwrap();
}
