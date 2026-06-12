use super::*;

#[test]
fn lowers_unreachable_tail_to_trap() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: cont::Tail::Unreachable,
            },
        },
    );

    assert!(traps(&module));
}

#[test]
fn lowers_and_runs_mutually_recursive_tuple() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![
                    (cont::ValueName::from("x"), cont::Prealloc::Tpl(2)),
                    (cont::ValueName::from("y"), cont::Prealloc::Tpl(2)),
                ],
                values: vec![
                    (
                        cont::ValueName::from("x"),
                        cont::Value::Pure(cont::Data::Tpl(vec![
                            cont::ValueName::from("y"),
                            cont::ValueName::from("ONE"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("y"),
                        cont::Value::Pure(cont::Data::Tpl(vec![
                            cont::ValueName::from("TWO"),
                            cont::ValueName::from("x"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("left"),
                        cont::Value::Eval(cont::Code::TplGet(cont::ValueName::from("x"), 0)),
                    ),
                    (
                        cont::ValueName::from("out"),
                        cont::Value::Eval(cont::Code::TplGet(cont::ValueName::from("left"), 0)),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("out"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                    handle: cont::ValueName::from("STDOUT"),
                    bytes: cont::ValueName::from("str"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_mutually_recursive_closures() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
    module.add_const(cont::ValueName::from("EVEN"), cont::Data::Int(11));
    module.add_const(cont::ValueName::from("ODD"), cont::Data::Int(22));

    module.add_clsr(
        cont::ClsrName::from("even"),
        cont::Clsr {
            fields: vec![cont::ValueName::from("odd").into()],
            params: vec![cont::ValueName::from("n").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("is_zero"),
                    cont::Value::Eval(cont::Code::IntEql(
                        cont::ValueName::from("n"),
                        cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![
                    (
                        cont::BlockName::from("on_zero"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("r"),
                                    params: vec![cont::ValueName::from("EVEN")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("on_non_zero"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    cont::ValueName::from("prev"),
                                    cont::Value::Eval(cont::Code::IntSub(
                                        cont::ValueName::from("n"),
                                        cont::ValueName::from("ONE"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                                    target: cont::ValueName::from("odd"),
                                    params: vec![cont::ValueName::from("prev")],
                                    resume: cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Match(cont::MatchTarget {
                    operand: cont::ValueName::from("is_zero"),
                    cases: std::collections::BTreeMap::from([(
                        0,
                        cont::JumpTarget {
                            target: cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        },
                    )]),
                    default: Some(cont::JumpTarget {
                        target: cont::BlockName::from("on_zero"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    module.add_clsr(
        cont::ClsrName::from("odd"),
        cont::Clsr {
            fields: vec![cont::ValueName::from("even").into()],
            params: vec![cont::ValueName::from("n").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("is_zero"),
                    cont::Value::Eval(cont::Code::IntEql(
                        cont::ValueName::from("n"),
                        cont::ValueName::from("ZERO"),
                    )),
                )],
                blocks: vec![
                    (
                        cont::BlockName::from("on_zero"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("r"),
                                    params: vec![cont::ValueName::from("ODD")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("on_non_zero"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    cont::ValueName::from("prev"),
                                    cont::Value::Eval(cont::Code::IntSub(
                                        cont::ValueName::from("n"),
                                        cont::ValueName::from("ONE"),
                                    )),
                                )],
                                blocks: vec![],
                                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                                    target: cont::ValueName::from("even"),
                                    params: vec![cont::ValueName::from("prev")],
                                    resume: cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Match(cont::MatchTarget {
                    operand: cont::ValueName::from("is_zero"),
                    cases: std::collections::BTreeMap::from([(
                        0,
                        cont::JumpTarget {
                            target: cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        },
                    )]),
                    default: Some(cont::JumpTarget {
                        target: cont::BlockName::from("on_zero"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![
                    (
                        cont::ValueName::from("even"),
                        cont::Prealloc::Clsr(cont::ClsrName::from("even")),
                    ),
                    (
                        cont::ValueName::from("odd"),
                        cont::Prealloc::Clsr(cont::ClsrName::from("odd")),
                    ),
                ],
                values: vec![
                    (
                        cont::ValueName::from("even"),
                        cont::Value::Pure(cont::Data::Clsr(
                            cont::ClsrName::from("even"),
                            vec![cont::ValueName::from("odd")],
                        )),
                    ),
                    (
                        cont::ValueName::from("odd"),
                        cont::Value::Pure(cont::Data::Clsr(
                            cont::ClsrName::from("odd"),
                            vec![cont::ValueName::from("even")],
                        )),
                    ),
                ],
                blocks: vec![(
                    cont::BlockName::from("after"),
                    cont::Block {
                        params: vec![cont::ValueName::from("out")],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![(
                                cont::ValueName::from("str"),
                                cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from(
                                    "out",
                                ))),
                            )],
                            blocks: vec![],
                            tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                                handle: cont::ValueName::from("STDOUT"),
                                bytes: cont::ValueName::from("str"),
                                resume: cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                    target: cont::ValueName::from("even"),
                    params: vec![cont::ValueName::from("ONE")],
                    resume: cont::BlockName::from("after"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 22);
}

#[test]
fn lowers_and_runs_direct_call() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

    module.add_func(
        cont::FuncName::from("add_one"),
        cont::Func {
            params: vec![cont::ValueName::from("x").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("sum"),
                    cont::Value::Eval(cont::Code::IntAdd(
                        cont::ValueName::from("x"),
                        cont::ValueName::from("ONE"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("sum")],
                }),
            },
        },
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("after_call"),
                    cont::Block {
                        params: vec![cont::ValueName::from("out")],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![(
                                cont::ValueName::from("str"),
                                cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from(
                                    "out",
                                ))),
                            )],
                            blocks: vec![],
                            tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                                handle: cont::ValueName::from("STDOUT"),
                                bytes: cont::ValueName::from("str"),
                                resume: cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Direct {
                    target: cont::FuncName::from("add_one"),
                    params: vec![cont::ValueName::from("TWO")],
                    resume: cont::BlockName::from("after_call"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_unit_result() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("unit"),
                    cont::Value::Pure(cont::Data::Tpl(vec![])),
                )],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    printed(&module);
}

#[test]
fn lowers_and_runs_float_result() {
    let mut cont_module = cont::Module::new();
    cont_module.set_entry(cont::FuncName::from("main"));

    cont_module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    cont_module.add_const(cont::ValueName::from("LEFT"), cont::Data::Flt(1.25));

    cont_module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Flt(2.5));

    cont_module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("sum"),
                        cont::Value::Eval(cont::Code::FltAdd(
                            cont::ValueName::from("LEFT"),
                            cont::ValueName::from("RIGHT"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("sum"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                    handle: cont::ValueName::from("STDOUT"),
                    bytes: cont::ValueName::from("str"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(f32_result(&cont_module), 3.75);
}

#[test]
fn lowers_and_runs_global_tuple() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));
    module.add_const(
        cont::ValueName::from("PAIR"),
        cont::Data::Tpl(vec![
            cont::ValueName::from("ONE"),
            cont::ValueName::from("TWO"),
        ]),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("out"),
                        cont::Value::Eval(cont::Code::TplGet(cont::ValueName::from("PAIR"), 1)),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("out"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                    handle: cont::ValueName::from("STDOUT"),
                    bytes: cont::ValueName::from("str"),
                    resume: cont::BlockName::from("r"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_global_closure() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("BIAS"), cont::Data::Int(5));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Int(3));

    module.add_clsr(
        cont::ClsrName::from("add_bias"),
        cont::Clsr {
            fields: vec![cont::ValueName::from("bias").into()],
            params: vec![cont::ValueName::from("x").into()],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![(
                    cont::ValueName::from("result"),
                    cont::Value::Eval(cont::Code::IntAdd(
                        cont::ValueName::from("x"),
                        cont::ValueName::from("bias"),
                    )),
                )],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("result")],
                }),
            },
        },
    );

    module.add_const(
        cont::ValueName::from("K"),
        cont::Data::Clsr(
            cont::ClsrName::from("add_bias"),
            vec![cont::ValueName::from("BIAS")],
        ),
    );

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(
                    cont::BlockName::from("after"),
                    cont::Block {
                        params: vec![cont::ValueName::from("out")],
                        region: cont::Region {
                            preallocs: vec![],
                            values: vec![(
                                cont::ValueName::from("str"),
                                cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from(
                                    "out",
                                ))),
                            )],
                            blocks: vec![],
                            tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                                handle: cont::ValueName::from("STDOUT"),
                                bytes: cont::ValueName::from("str"),
                                resume: cont::BlockName::from("r"),
                            }),
                        },
                    },
                )],
                tail: cont::Tail::Call(cont::CallTarget::Indirect {
                    target: cont::ValueName::from("K"),
                    params: vec![cont::ValueName::from("THREE")],
                    resume: cont::BlockName::from("after"),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 8);
}

#[test]
fn lowers_and_runs_sparse_match() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("STDOUT"), cont::Data::Nat(1));

    module.add_const(cont::ValueName::from("BYTE"), cont::Data::Nat(123)); // '{'
    module.add_const(cont::ValueName::from("R0"), cont::Data::Nat(0));
    module.add_const(cont::ValueName::from("R1"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("R2"), cont::Data::Nat(2));
    module.add_const(cont::ValueName::from("R3"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![
                    (
                        cont::BlockName::from("after"),
                        cont::Block {
                            params: vec![cont::ValueName::from("out")],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![(
                                    cont::ValueName::from("str"),
                                    cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from(
                                        "out",
                                    ))),
                                )],
                                blocks: vec![],
                                tail: cont::Tail::Host(cont::HostTarget::IoWrite {
                                    handle: cont::ValueName::from("STDOUT"),
                                    bytes: cont::ValueName::from("str"),
                                    resume: cont::BlockName::from("r"),
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("b_quote"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("after"),
                                    params: vec![cont::ValueName::from("R1")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("b_lbracket"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("after"),
                                    params: vec![cont::ValueName::from("R2")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("b_lbrace"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("after"),
                                    params: vec![cont::ValueName::from("R3")],
                                }),
                            },
                        },
                    ),
                    (
                        cont::BlockName::from("b_default"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
                                values: vec![],
                                blocks: vec![],
                                tail: cont::Tail::Jump(cont::JumpTarget {
                                    target: cont::BlockName::from("after"),
                                    params: vec![cont::ValueName::from("R0")],
                                }),
                            },
                        },
                    ),
                ],
                tail: cont::Tail::Match(cont::MatchTarget {
                    operand: cont::ValueName::from("BYTE"),
                    cases: std::collections::BTreeMap::from([
                        (
                            34,
                            cont::JumpTarget {
                                target: cont::BlockName::from("b_quote"),
                                params: vec![],
                            },
                        ),
                        (
                            91,
                            cont::JumpTarget {
                                target: cont::BlockName::from("b_lbracket"),
                                params: vec![],
                            },
                        ),
                        (
                            123,
                            cont::JumpTarget {
                                target: cont::BlockName::from("b_lbrace"),
                                params: vec![],
                            },
                        ),
                    ]),
                    default: Some(cont::JumpTarget {
                        target: cont::BlockName::from("b_default"),
                        params: vec![],
                    }),
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}
