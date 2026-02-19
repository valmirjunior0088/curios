use {
    curios::{cont, wasm},
    wasmtime::{Config, Engine, Instance, Module, Rooted, Store, StructRef},
};

fn main() {
    let mut cont_module = cont::Module::new();

    cont_module.add_const(cont::ValueName::from("ZERO"), cont::ConstValue::Int(0));

    cont_module.add_const(cont::ValueName::from("ONE"), cont::ConstValue::Int(1));

    cont_module.add_const(cont::ValueName::from("TWO"), cont::ConstValue::Int(2));

    cont_module.add_const(cont::ValueName::from("FIVE"), cont::ConstValue::Int(5));

    cont_module.add_const(cont::ValueName::from("ANSWER"), cont::ConstValue::Int(42));

    cont_module.add_const(cont::ValueName::from("EXPECTED"), cont::ConstValue::Int(84));

    cont_module.add_clsr(
        cont::ClsrName::from("add_bias"),
        cont::Clsr {
            fields: vec![cont::ValueName::from("bias")],
            params: vec![cont::ValueName::from("x")],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                values: vec![(
                    cont::ValueName::from("sum"),
                    cont::Value::Eval(
                        cont::ConstOp::IntAdd,
                        vec![cont::ValueName::from("x"), cont::ValueName::from("bias")],
                    ),
                )],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("sum")],
                }),
            },
        },
    );

    cont_module.add_func(
        cont::FuncName::from("normalize"),
        cont::Func {
            params: vec![cont::ValueName::from("x")],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                values: vec![(
                    cont::ValueName::from("is_zero"),
                    cont::Value::Eval(
                        cont::ConstOp::IntEql,
                        vec![cont::ValueName::from("x"), cont::ValueName::from("ZERO")],
                    ),
                )],
                blocks: vec![
                    (
                        cont::BlockName::from("on_zero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("tag")],
                            region: cont::Region {
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("join"),
                                    params: vec![cont::ValueName::from("ONE")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("on_non_zero"),
                        cont::Block {
                            params: vec![cont::ValueName::from("tag")],
                            region: cont::Region {
                                values: vec![(
                                    cont::ValueName::from("is_expected"),
                                    cont::Value::Eval(
                                        cont::ConstOp::IntEql,
                                        vec![
                                            cont::ValueName::from("x"),
                                            cont::ValueName::from("EXPECTED"),
                                        ],
                                    ),
                                )],
                                blocks: vec![
                                    (
                                        cont::BlockName::from("on_expected"),
                                        cont::Block {
                                            params: vec![cont::ValueName::from("tag")],
                                            region: cont::Region {
                                                values: vec![(
                                                    cont::ValueName::from("bumped"),
                                                    cont::Value::Eval(
                                                        cont::ConstOp::IntAdd,
                                                        vec![
                                                            cont::ValueName::from("x"),
                                                            cont::ValueName::from("ONE"),
                                                        ],
                                                    ),
                                                )],
                                                blocks: vec![],
                                                tail: cont::Tail::Jump(cont::JumpTarget {
                                                    target: cont::BlockName::from("join"),
                                                    params: vec![cont::ValueName::from("bumped")],
                                                }),
                                            },
                                        },
                                    ),
                                    (
                                        cont::BlockName::from("on_other"),
                                        cont::Block {
                                            params: vec![cont::ValueName::from("tag")],
                                            region: cont::Region {
                                                values: vec![(
                                                    cont::ValueName::from("shrunk"),
                                                    cont::Value::Eval(
                                                        cont::ConstOp::IntSub,
                                                        vec![
                                                            cont::ValueName::from("x"),
                                                            cont::ValueName::from("ONE"),
                                                        ],
                                                    ),
                                                )],
                                                blocks: vec![],
                                                tail: cont::Tail::Jump(cont::JumpTarget {
                                                    target: cont::BlockName::from("join"),
                                                    params: vec![cont::ValueName::from("shrunk")],
                                                }),
                                            },
                                        },
                                    ),
                                ],
                                tail: cont::Tail::Case(cont::CaseTarget {
                                    operand: cont::ValueName::from("is_expected"),
                                    targets: vec![cont::JumpTarget {
                                        target: cont::BlockName::from("on_other"),
                                        params: vec![cont::ValueName::from("x")],
                                    }],
                                    default: cont::JumpTarget {
                                        target: cont::BlockName::from("on_expected"),
                                        params: vec![cont::ValueName::from("x")],
                                    },
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("join"),
                        cont::Block {
                            params: vec![cont::ValueName::from("out")],
                            region: cont::Region {
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
                tail: cont::Tail::Case(cont::CaseTarget {
                    operand: cont::ValueName::from("is_zero"),
                    targets: vec![cont::JumpTarget {
                        target: cont::BlockName::from("on_non_zero"),
                        params: vec![cont::ValueName::from("x")],
                    }],
                    default: cont::JumpTarget {
                        target: cont::BlockName::from("on_zero"),
                        params: vec![cont::ValueName::from("x")],
                    },
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
                values: vec![(
                    cont::ValueName::from("thunk"),
                    cont::Value::Clsr(
                        cont::ClsrName::from("add_bias"),
                        vec![cont::ValueName::from("ANSWER")],
                    ),
                )],
                blocks: vec![(
                    cont::BlockName::from("after_indirect"),
                    cont::Block {
                        params: vec![cont::ValueName::from("out")],
                        region: cont::Region {
                            values: vec![(
                                cont::ValueName::from("is_expected"),
                                cont::Value::Eval(
                                    cont::ConstOp::IntEql,
                                    vec![
                                        cont::ValueName::from("out"),
                                        cont::ValueName::from("EXPECTED"),
                                    ],
                                ),
                            )],
                            blocks: vec![
                                (
                                    cont::BlockName::from("invoke_normalize"),
                                    cont::Block {
                                        params: vec![cont::ValueName::from("tag")],
                                        region: cont::Region {
                                            values: vec![],
                                            blocks: vec![],
                                            tail: cont::Tail::Call(cont::CallTarget::Direct {
                                                target: cont::FuncName::from("normalize"),
                                                params: vec![cont::ValueName::from("out")],
                                                resume: cont::BlockName::from("after_direct"),
                                            }),
                                        },
                                    },
                                ),
                                (
                                    cont::BlockName::from("fallback"),
                                    cont::Block {
                                        params: vec![cont::ValueName::from("tag")],
                                        region: cont::Region {
                                            values: vec![(
                                                cont::ValueName::from("fallback_out"),
                                                cont::Value::Eval(
                                                    cont::ConstOp::IntAdd,
                                                    vec![
                                                        cont::ValueName::from("out"),
                                                        cont::ValueName::from("ONE"),
                                                    ],
                                                ),
                                            )],
                                            blocks: vec![],
                                            tail: cont::Tail::Jump(cont::JumpTarget {
                                                target: cont::BlockName::from("r"),
                                                params: vec![cont::ValueName::from("fallback_out")],
                                            }),
                                        },
                                    },
                                ),
                                (
                                    cont::BlockName::from("after_direct"),
                                    cont::Block {
                                        params: vec![cont::ValueName::from("normalized")],
                                        region: cont::Region {
                                            values: vec![(
                                                cont::ValueName::from("final_out"),
                                                cont::Value::Eval(
                                                    cont::ConstOp::IntMul,
                                                    vec![
                                                        cont::ValueName::from("normalized"),
                                                        cont::ValueName::from("TWO"),
                                                    ],
                                                ),
                                            )],
                                            blocks: vec![],
                                            tail: cont::Tail::Jump(cont::JumpTarget {
                                                target: cont::BlockName::from("r"),
                                                params: vec![cont::ValueName::from("final_out")],
                                            }),
                                        },
                                    },
                                ),
                            ],
                            tail: cont::Tail::Case(cont::CaseTarget {
                                operand: cont::ValueName::from("is_expected"),
                                targets: vec![cont::JumpTarget {
                                    target: cont::BlockName::from("fallback"),
                                    params: vec![cont::ValueName::from("out")],
                                }],
                                default: cont::JumpTarget {
                                    target: cont::BlockName::from("invoke_normalize"),
                                    params: vec![cont::ValueName::from("out")],
                                },
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                    target: cont::ValueName::from("thunk"),
                    params: vec![cont::ValueName::from("ANSWER")],
                    resume: cont::BlockName::from("after_indirect"),
                }),
            },
        },
    );

    cont_module.add_func(
        cont::FuncName::from("main_zero"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("after_normalize"),
                    cont::Block {
                        params: vec![cont::ValueName::from("normalized")],
                        region: cont::Region {
                            values: vec![(
                                cont::ValueName::from("out"),
                                cont::Value::Eval(
                                    cont::ConstOp::IntMul,
                                    vec![
                                        cont::ValueName::from("normalized"),
                                        cont::ValueName::from("TWO"),
                                    ],
                                ),
                            )],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("out")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Direct {
                    target: cont::FuncName::from("normalize"),
                    params: vec![cont::ValueName::from("ZERO")],
                    resume: cont::BlockName::from("after_normalize"),
                }),
            },
        },
    );

    cont_module.add_func(
        cont::FuncName::from("main_other"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("after_normalize"),
                    cont::Block {
                        params: vec![cont::ValueName::from("normalized")],
                        region: cont::Region {
                            values: vec![(
                                cont::ValueName::from("out"),
                                cont::Value::Eval(
                                    cont::ConstOp::IntMul,
                                    vec![
                                        cont::ValueName::from("normalized"),
                                        cont::ValueName::from("TWO"),
                                    ],
                                ),
                            )],
                            blocks: vec![],
                            tail: cont::Tail::Jump(cont::JumpTarget {
                                target: cont::BlockName::from("r"),
                                params: vec![cont::ValueName::from("out")],
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Direct {
                    target: cont::FuncName::from("normalize"),
                    params: vec![cont::ValueName::from("FIVE")],
                    resume: cont::BlockName::from("after_normalize"),
                }),
            },
        },
    );

    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine = Engine::new(&config).expect("expected wasmtime engine");

    let module = Module::from_binary(&engine, &wasm::to_bytes(&cont::to_wasm(&cont_module)))
        .expect("expected wasm module");

    let mut store = Store::new(&engine, ());

    let instance = Instance::new(&mut store, &module, &[]).expect("expected instance");

    let run = instance
        .get_typed_func::<(), Rooted<StructRef>>(&mut store, "func/main")
        .expect("expected exported func/main");

    let run_zero = instance
        .get_typed_func::<(), Rooted<StructRef>>(&mut store, "func/main_zero")
        .expect("expected exported func/main_zero");

    let run_other = instance
        .get_typed_func::<(), Rooted<StructRef>>(&mut store, "func/main_other")
        .expect("expected exported func/main_other");

    let result = run.call(&mut store, ()).expect("expected call result");

    let zero_result = run_zero
        .call(&mut store, ())
        .expect("expected call result for main_zero");

    let other_result = run_other
        .call(&mut store, ())
        .expect("expected call result for main_other");

    let value = result
        .field(&mut store, 0)
        .expect("expected struct field")
        .unwrap_i32();
    let zero_value = zero_result
        .field(&mut store, 0)
        .expect("expected struct field")
        .unwrap_i32();
    let other_value = other_result
        .field(&mut store, 0)
        .expect("expected struct field")
        .unwrap_i32();

    assert_eq!(value, 170);
    assert_eq!(zero_value, 2);
    assert_eq!(other_value, 8);
}
