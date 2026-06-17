use {curios::cont, std::collections::BTreeMap};

fn main() {
    let mut cont_module = cont::Module::new();

    cont_module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));

    cont_module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));

    cont_module.add_const(cont::ValueName::from("ONE_HALF"), cont::Data::Flt(0.5));

    cont_module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );

    cont_module.add_clsr(
        cont::ClsrName::from("RouteByZero"),
        cont::Clsr {
            fields: vec![],
            params: vec![cont::ValueName::from("x").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("is_zero"),
                    cont::Value::Eval(cont::Code::IntEql(
                        cont::ValueName::from("x"),
                        cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![
                    (
                        cont::BlockName::from("on_zero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("tag")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("join"),
                                    params: vec![cont::ValueName::from("ZERO")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("on_non_zero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("tag")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("join"),
                                    params: vec![cont::ValueName::from("x")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("join"),
                        cont::Block {
                            params: vec![cont::ValueName::from("selected")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("r"),
                                    params: vec![cont::ValueName::from("selected")],
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Match(cont::MatchTarget {
                    operand: cont::ValueName::from("is_zero"),
                    cases: BTreeMap::from([(
                        0,
                        cont::JumpTarget {
                            target: cont::BlockName::from("on_zero"),
                            params: vec![cont::ValueName::from("x")],
                        },
                    )]),
                    default: Some(cont::JumpTarget {
                        target: cont::BlockName::from("on_non_zero"),
                        params: vec![cont::ValueName::from("x")],
                    }),
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
                        cont::Value::Pure(cont::Data::Int(3)),
                    ),
                    (
                        cont::ValueName::from("thk"),
                        cont::Value::Pure(cont::Data::Clsr(
                            cont::ClsrName::from("RouteByZero"),
                            vec![],
                        )),
                    ),
                    (
                        cont::ValueName::from("scale"),
                        cont::Value::Eval(cont::Code::FltMul(
                            cont::ValueName::from("ONE_HALF"),
                            cont::ValueName::from("ONE_HALF"),
                        )),
                    ),
                ],
                blocks: vec![
                    (
                        cont::BlockName::from("after_call"),
                        cont::Block {
                            params: vec![cont::ValueName::from("result")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![
                                    (
                                        cont::ValueName::from("pair"),
                                        cont::Value::Pure(cont::Data::Tpl(vec![
                                            cont::ValueName::from("result"),
                                            cont::ValueName::from("ONE"),
                                        ])),
                                    ),
                                    (
                                        cont::ValueName::from("result_again"),
                                        cont::Value::Eval(cont::Code::TplGet(
                                            cont::ValueName::from("pair"),
                                            0,
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("fallback_one"),
                                        cont::Value::Eval(cont::Code::TplGet(
                                            cont::ValueName::from("pair"),
                                            1,
                                        )),
                                    ),
                                    (
                                        cont::ValueName::from("result_is_zero"),
                                        cont::Value::Eval(cont::Code::IntEql(
                                            cont::ValueName::from("result_again"),
                                            cont::ValueName::from("ZERO"),
                                        )),
                                    ),
                                ],
                                blocks: vec![
                                    (
                                        cont::BlockName::from("pick_zero"),
                                        cont::Block {
                                            params: vec![cont::ValueName::from("tag")],
                                            region: cont::Region {
                                                preallocs: vec![],
                                                values: vec![],
                                                blocks: vec![],
                                                tail: cont::Tail::Jump(cont::JumpTarget {
                                                    target: cont::BlockName::from("finalize"),
                                                    params: vec![cont::ValueName::from("ZERO")],
                                                }),
                                            },
                                        },
                                    ),
                                    (
                                        cont::BlockName::from("pick_one"),
                                        cont::Block {
                                            params: vec![cont::ValueName::from("tag")],
                                            region: cont::Region {
                                                preallocs: vec![],
                                                values: vec![],
                                                blocks: vec![],
                                                tail: cont::Tail::Jump(cont::JumpTarget {
                                                    target: cont::BlockName::from("finalize"),
                                                    params: vec![cont::ValueName::from(
                                                        "fallback_one",
                                                    )],
                                                }),
                                            },
                                        },
                                    ),
                                    (
                                        cont::BlockName::from("finalize"),
                                        cont::Block {
                                            params: vec![cont::ValueName::from("out")],
                                            region: cont::Region {
                                                preallocs: vec![],
                                                values: vec![],
                                                blocks: vec![],
                                                tail: cont::Tail::Jump(cont::JumpTarget {
                                                    target: cont::BlockName::from("r"),
                                                    params: vec![cont::ValueName::from("out")],
                                                }),
                                            },
                                        },
                                    ),
                                ],
                                tail: cont::Tail::Match(cont::MatchTarget {
                                    operand: cont::ValueName::from("result_is_zero"),
                                    cases: BTreeMap::from([(
                                        0,
                                        cont::JumpTarget {
                                            target: cont::BlockName::from("pick_zero"),
                                            params: vec![cont::ValueName::from("result")],
                                        },
                                    )]),
                                    default: Some(cont::JumpTarget {
                                        target: cont::BlockName::from("pick_one"),
                                        params: vec![cont::ValueName::from("result")],
                                    }),
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("dead_path"),
                        cont::Block {
                            params: vec![],
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
                ],
                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                    target: cont::ValueName::from("thk"),
                    params: vec![cont::ValueName::from("n")],
                    resume: cont::BlockName::from("after_call"),
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
    curios::run_wasm(&wasm_module, curios::OsHost::new()).unwrap();
}
