use super::*;

#[test]
fn lowers_and_runs_nat_add() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Nat(4));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FOUR"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_arr_len() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Arr(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrLen(cont::ValueName::from("LST"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_arr_get() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Arr(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrGet(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("ONE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_arr_slice() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));
    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("THREE_IDX"), cont::Data::Nat(3));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Arr(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
            cont::ValueName::from("FIVE"),
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
                        cont::ValueName::from("slice"),
                        cont::Value::Eval(cont::Code::ArrSlice(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("THREE_IDX"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrLen(cont::ValueName::from("slice"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 2);
}

#[test]
fn lowers_and_runs_arr_concat() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Nat(1));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Nat(2));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(
        cont::ValueName::from("LST1"),
        cont::Data::Arr(vec![cont::ValueName::from("ONE")]),
    );
    module.add_const(
        cont::ValueName::from("LST2"),
        cont::Data::Arr(vec![
            cont::ValueName::from("TWO"),
            cont::ValueName::from("THREE"),
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
                        cont::ValueName::from("concat"),
                        cont::Value::Eval(cont::Code::ArrConcat(vec![
                            cont::ValueName::from("LST1"),
                            cont::ValueName::from("LST2"),
                        ])),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrLen(cont::ValueName::from("concat"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_flt_floor() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.9));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltFloor(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_flt_ceil() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltCeil(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_flt_trunc() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(-2.9));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltTrunc(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), -2.0);
}

#[test]
fn lowers_and_runs_flt_nearest() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltNearest(cont::ValueName::from("X"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_nat_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatDiv(
                            cont::ValueName::from("TEN"),
                            cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_rem() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatRem(
                            cont::ValueName::from("TEN"),
                            cont::ValueName::from("THREE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_nat_lt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatLt(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neg() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Int(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("zero"),
                        cont::Value::Pure(cont::Data::Int(0)),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntSub(
                            cont::ValueName::from("zero"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), -5);
}

#[test]
fn lowers_and_runs_int_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("NEG7"), cont::Data::Int(-7));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntDiv(
                            cont::ValueName::from("NEG7"),
                            cont::ValueName::from("TWO"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), -3);
}

#[test]
fn lowers_and_runs_int_lt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
    module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntLt(
                            cont::ValueName::from("NEG1"),
                            cont::ValueName::from("ZERO"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_div() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltDiv(
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("FOUR"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 0.25);
}

#[test]
fn lowers_and_runs_flt_eql() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(1.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_sqrt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltSqrt(cont::ValueName::from("FOUR"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.0);
}

#[test]
fn lowers_and_runs_int_to_flt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Int(3));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntToFlt(cont::ValueName::from("THREE"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 3.0);
}

#[test]
fn lowers_and_runs_nat_to_flt() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatToFlt(cont::ValueName::from("FIVE"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 5.0);
}

#[test]
fn lowers_and_runs_flt_to_int() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE_SEVEN"), cont::Data::Flt(3.7));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltToInt(cont::ValueName::from(
                            "THREE_SEVEN",
                        ))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 3);
}

#[test]
fn lowers_and_runs_nat_to_int() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatToInt(cont::ValueName::from("SEVEN"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatNeq(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_int_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
    module.add_const(cont::ValueName::from("NEG1B"), cont::Data::Int(-1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntNeq(
                            cont::ValueName::from("NEG1"),
                            cont::ValueName::from("NEG1B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_flt_neq() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Flt(2.0));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltNeq(
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("TWO"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_flt_min() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMin(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 1.5);
}

#[test]
fn lowers_and_runs_flt_max() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
    module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMax(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::FltToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(f32_result(&module), 2.5);
}

#[test]
fn lowers_and_runs_bin_len() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from("HELLO"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 5);
}

#[test]
fn lowers_and_runs_bin_get() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(cont::ValueName::from("IDX"), cont::Data::Nat(1));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("HELLO"),
                            cont::ValueName::from("IDX"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'e' as i32);
}

#[test]
fn lowers_and_runs_bin_append() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(
        cont::ValueName::from("HELLO"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(cont::ValueName::from("BANG"), cont::Data::Nat(b'!' as u32));
    module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("appended"),
                        cont::Value::Eval(cont::Code::BinAppend(
                            cont::ValueName::from("HELLO"),
                            cont::ValueName::from("BANG"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("appended"),
                            cont::ValueName::from("FIVE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), b'!' as i32);
}

#[test]
fn lowers_and_runs_bin_eql_equal() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(
        cont::ValueName::from("A"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        cont::ValueName::from("B"),
        cont::Data::Bin(b"hello".to_vec()),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 1);
}

#[test]
fn lowers_and_runs_bin_eql_unequal() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(
        cont::ValueName::from("A"),
        cont::Data::Bin(b"hello".to_vec()),
    );
    module.add_const(
        cont::ValueName::from("B"),
        cont::Data::Bin(b"world".to_vec()),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_arr_append() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));

    module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
    module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));
    module.add_const(cont::ValueName::from("NINE"), cont::Data::Nat(9));
    module.add_const(
        cont::ValueName::from("LST"),
        cont::Data::Arr(vec![
            cont::ValueName::from("THREE"),
            cont::ValueName::from("SEVEN"),
        ]),
    );
    module.add_const(cont::ValueName::from("TWO"), cont::Data::Nat(2));

    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (
                        cont::ValueName::from("appended"),
                        cont::Value::Eval(cont::Code::ArrAppend(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("NINE"),
                        )),
                    ),
                    (
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrGet(
                            cont::ValueName::from("appended"),
                            cont::ValueName::from("TWO"),
                        )),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );

    assert_eq!(i32_result(&module), 9);
}

fn nat_op_module(op: cont::Code, left: u32, right: u32) -> cont::Module {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));
    module.add_const(cont::ValueName::from("LEFT"), cont::Data::Nat(left));
    module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Nat(right));
    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (cont::ValueName::from("result"), cont::Value::Eval(op)),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::NatToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );
    module
}

const MAX_I31: u32 = i32::MAX as u32;

#[test]
fn lowers_and_runs_nat_mul() {
    let module = nat_op_module(
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        6,
        7,
    );
    assert_eq!(i32_result(&module), 42);
}

#[test]
fn lowers_and_runs_nat_sub_monus() {
    let module = nat_op_module(
        cont::Code::NatSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        3,
        7,
    );
    assert_eq!(i32_result(&module), 0);
}

#[test]
fn lowers_and_runs_nat_sub() {
    let module = nat_op_module(
        cont::Code::NatSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        10,
        3,
    );
    assert_eq!(i32_result(&module), 7);
}

#[test]
fn lowers_and_runs_nat_add_at_boundary() {
    let module = nat_op_module(
        cont::Code::NatAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31 - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn lowers_and_runs_nat_mul_at_boundary() {
    let module = nat_op_module(
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert_eq!(i32_result(&module), MAX_I31 as i32);
}

#[test]
fn nat_add_overflow_traps() {
    let module = nat_op_module(
        cont::Code::NatAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn nat_mul_overflow_traps() {
    let module = nat_op_module(
        cont::Code::NatMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_I31,
        2,
    );
    assert!(traps(&module));
}

fn int_op_module(op: cont::Code, left: i32, right: i32) -> cont::Module {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));
    module.add_const(cont::ValueName::from("LEFT"), cont::Data::Int(left));
    module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Int(right));
    module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![
                    (cont::ValueName::from("result"), cont::Value::Eval(op)),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );
    module
}

const MAX_INT: i32 = (1 << 30) - 1;
const MIN_INT: i32 = -(1 << 30);

#[test]
fn lowers_and_runs_int_add_at_boundary() {
    let module = int_op_module(
        cont::Code::IntAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT - 1,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_add_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntAdd(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn int_sub_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntSub(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        1,
    );
    assert!(traps(&module));
}

#[test]
fn lowers_and_runs_int_mul_at_boundary() {
    let module = int_op_module(
        cont::Code::IntMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        1,
    );
    assert_eq!(i32_result(&module), MAX_INT);
}

#[test]
fn int_mul_overflow_traps() {
    let module = int_op_module(
        cont::Code::IntMul(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MAX_INT,
        2,
    );
    assert!(traps(&module));
}

#[test]
fn int_div_overflow_traps() {
    // MIN_INT / -1 = 2^30, which exceeds the 31-bit signed maximum.
    let module = int_op_module(
        cont::Code::IntDiv(
            cont::ValueName::from("LEFT"),
            cont::ValueName::from("RIGHT"),
        ),
        MIN_INT,
        -1,
    );
    assert!(traps(&module));
}

#[test]
fn flt_to_int_overflow_traps() {
    let mut module = cont::Module::new();
    module.set_entry(cont::FuncName::from("main"));
    module.add_const(
        cont::ValueName::from("TOO_BIG"),
        cont::Data::Flt((MAX_INT as f32) + 1.0),
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
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltToInt(cont::ValueName::from("TOO_BIG"))),
                    ),
                    (
                        cont::ValueName::from("str"),
                        cont::Value::Eval(cont::Code::IntToStr(cont::ValueName::from("result"))),
                    ),
                    (
                        cont::ValueName::from("unit"),
                        cont::Value::Eval(cont::Code::IoPrint(cont::ValueName::from("str"))),
                    ),
                ],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("unit")],
                }),
            },
        },
    );
    assert!(traps(&module));
}
