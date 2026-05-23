mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod expr_emitter;
use expr_emitter::*;

mod module_emitter;
use module_emitter::*;

use crate::{cont, wasm};

pub fn to_wasm(cont_module: &cont::Module) -> wasm::Module {
    let mut wasm_module = wasm::Module::new("module");

    ModuleEmitter::new(&Table::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(module: &cont::Module) -> String {
        crate::run_wasm(&to_wasm(module)).expect("expected result")
    }

    fn i32_result(module: &cont::Module) -> i32 {
        run(module)
            .split("value=")
            .nth(1)
            .unwrap()
            .trim_end_matches(')')
            .parse()
            .unwrap()
    }

    fn f32_result(module: &cont::Module) -> f32 {
        run(module)
            .split("value=")
            .nth(1)
            .unwrap()
            .split(')')
            .next()
            .unwrap()
            .parse()
            .unwrap()
    }

    #[test]
    fn lowers_and_runs_mutually_recursive_tuple() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
        module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("out")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 2);
    }

    #[test]
    fn lowers_and_runs_mutually_recursive_closures() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));
        module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
        module.add_const(cont::ValueName::from("EVEN"), cont::Data::Int(11));
        module.add_const(cont::ValueName::from("ODD"), cont::Data::Int(22));

        module.add_clsr(
            cont::ClsrName::from("even"),
            cont::Clsr {
                fields: vec![cont::ValueName::from("odd")],
                params: vec![cont::ValueName::from("n")],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                fields: vec![cont::ValueName::from("even")],
                params: vec![cont::ValueName::from("n")],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                    blocks: vec![],
                    tail: cont::Tail::Call(cont::CallTarget::Indirect {
                        target: cont::ValueName::from("even"),
                        params: vec![cont::ValueName::from("ONE")],
                        resume: cont::BlockName::from("r"),
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 22);
    }

    #[test]
    fn lowers_and_runs_direct_call() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("ONE"), cont::Data::Int(1));
        module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

        module.add_func(
            cont::FuncName::from("add_one"),
            cont::Func {
                params: vec![cont::ValueName::from("x")],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                    values: vec![],
                    blocks: vec![(
                        cont::BlockName::from("after_call"),
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

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("unit"),
                        cont::Value::Pure(cont::Data::Unit),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("unit")],
                    }),
                },
            },
        );

        assert_eq!(run(&module), "#0 = struct {}");
    }

    #[test]
    fn lowers_and_runs_float_result() {
        let mut cont_module = cont::Module::new();

        cont_module.add_const(cont::ValueName::from("LEFT"), cont::Data::Flt(1.25));

        cont_module.add_const(cont::ValueName::from("RIGHT"), cont::Data::Flt(2.5));

        cont_module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("sum"),
                        cont::Value::Eval(cont::Code::FltAdd(
                            cont::ValueName::from("LEFT"),
                            cont::ValueName::from("RIGHT"),
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

        assert_eq!(f32_result(&cont_module), 3.75);
    }

    #[test]
    fn lowers_and_runs_global_tuple() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("out"),
                        cont::Value::Eval(cont::Code::TplGet(cont::ValueName::from("PAIR"), 1)),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("out")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 2);
    }

    #[test]
    fn lowers_and_runs_global_closure() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("BIAS"), cont::Data::Int(5));
        module.add_const(cont::ValueName::from("THREE"), cont::Data::Int(3));

        module.add_clsr(
            cont::ClsrName::from("add_bias"),
            cont::Clsr {
                fields: vec![cont::ValueName::from("bias")],
                params: vec![cont::ValueName::from("x")],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                    values: vec![],
                    blocks: vec![(
                        cont::BlockName::from("after"),
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
    fn lowers_and_runs_nat_add() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
        module.add_const(cont::ValueName::from("FOUR"), cont::Data::Nat(4));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatAdd(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FOUR"),
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

        assert_eq!(i32_result(&module), 7);
    }

    #[test]
    fn lowers_and_runs_arr_len() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrLen(cont::ValueName::from("LST"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 2);
    }

    #[test]
    fn lowers_and_runs_arr_get() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::ArrGet(
                            cont::ValueName::from("LST"),
                            cont::ValueName::from("ONE"),
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

        assert_eq!(i32_result(&module), 7);
    }

    #[test]
    fn lowers_and_runs_arr_slice() {
        let mut module = cont::Module::new();

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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 2);
    }

    #[test]
    fn lowers_and_runs_arr_concat() {
        let mut module = cont::Module::new();

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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 3);
    }

    #[test]
    fn lowers_and_runs_flt_floor() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.9));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltFloor(cont::ValueName::from("X"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 2.0);
    }

    #[test]
    fn lowers_and_runs_flt_ceil() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.1));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltCeil(cont::ValueName::from("X"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 3.0);
    }

    #[test]
    fn lowers_and_runs_flt_trunc() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("X"), cont::Data::Flt(-2.9));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltTrunc(cont::ValueName::from("X"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), -2.0);
    }

    #[test]
    fn lowers_and_runs_flt_nearest() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("X"), cont::Data::Flt(2.5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltNearest(cont::ValueName::from("X"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 2.0);
    }

    #[test]
    fn lowers_and_runs_nat_div() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
        module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatDiv(
                            cont::ValueName::from("TEN"),
                            cont::ValueName::from("THREE"),
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

        assert_eq!(i32_result(&module), 3);
    }

    #[test]
    fn lowers_and_runs_nat_rem() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("TEN"), cont::Data::Nat(10));
        module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatRem(
                            cont::ValueName::from("TEN"),
                            cont::ValueName::from("THREE"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_nat_lt() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
        module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatLt(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FIVE"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_int_neg() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("FIVE"), cont::Data::Int(5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), -5);
    }

    #[test]
    fn lowers_and_runs_int_div() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("NEG7"), cont::Data::Int(-7));
        module.add_const(cont::ValueName::from("TWO"), cont::Data::Int(2));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntDiv(
                            cont::ValueName::from("NEG7"),
                            cont::ValueName::from("TWO"),
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

        assert_eq!(i32_result(&module), -3);
    }

    #[test]
    fn lowers_and_runs_int_lt() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
        module.add_const(cont::ValueName::from("ZERO"), cont::Data::Int(0));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntLt(
                            cont::ValueName::from("NEG1"),
                            cont::ValueName::from("ZERO"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_flt_div() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
        module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltDiv(
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("FOUR"),
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

        assert_eq!(f32_result(&module), 0.25);
    }

    #[test]
    fn lowers_and_runs_flt_eql() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
        module.add_const(cont::ValueName::from("B"), cont::Data::Flt(1.5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_flt_sqrt() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("FOUR"), cont::Data::Flt(4.0));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltSqrt(cont::ValueName::from("FOUR"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 2.0);
    }

    #[test]
    fn lowers_and_runs_int_to_flt() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("THREE"), cont::Data::Int(3));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntToFlt(cont::ValueName::from("THREE"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 3.0);
    }

    #[test]
    fn lowers_and_runs_nat_to_flt() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatToFlt(cont::ValueName::from("FIVE"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(f32_result(&module), 5.0);
    }

    #[test]
    fn lowers_and_runs_flt_to_int() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("THREE_SEVEN"), cont::Data::Flt(3.7));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltToInt(cont::ValueName::from(
                            "THREE_SEVEN",
                        ))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 3);
    }

    #[test]
    fn lowers_and_runs_nat_to_int() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("SEVEN"), cont::Data::Nat(7));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatToInt(cont::ValueName::from("SEVEN"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 7);
    }

    #[test]
    fn lowers_and_runs_nat_neq() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("THREE"), cont::Data::Nat(3));
        module.add_const(cont::ValueName::from("FIVE"), cont::Data::Nat(5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::NatNeq(
                            cont::ValueName::from("THREE"),
                            cont::ValueName::from("FIVE"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_int_neq() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("NEG1"), cont::Data::Int(-1));
        module.add_const(cont::ValueName::from("NEG1B"), cont::Data::Int(-1));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::IntNeq(
                            cont::ValueName::from("NEG1"),
                            cont::ValueName::from("NEG1B"),
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

        assert_eq!(i32_result(&module), 0);
    }

    #[test]
    fn lowers_and_runs_flt_neq() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("ONE"), cont::Data::Flt(1.0));
        module.add_const(cont::ValueName::from("TWO"), cont::Data::Flt(2.0));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltNeq(
                            cont::ValueName::from("ONE"),
                            cont::ValueName::from("TWO"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_flt_min() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
        module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMin(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
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

        assert_eq!(f32_result(&module), 1.5);
    }

    #[test]
    fn lowers_and_runs_flt_max() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("A"), cont::Data::Flt(1.5));
        module.add_const(cont::ValueName::from("B"), cont::Data::Flt(2.5));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::FltMax(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
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

        assert_eq!(f32_result(&module), 2.5);
    }

    #[test]
    fn lowers_and_runs_bin_len() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinLen(cont::ValueName::from("HELLO"))),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 5);
    }

    #[test]
    fn lowers_and_runs_bin_get() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinGet(
                            cont::ValueName::from("HELLO"),
                            cont::ValueName::from("IDX"),
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

        assert_eq!(i32_result(&module), b'e' as i32);
    }

    #[test]
    fn lowers_and_runs_bin_append() {
        let mut module = cont::Module::new();

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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), b'!' as i32);
    }

    #[test]
    fn lowers_and_runs_bin_eql_equal() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
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

        assert_eq!(i32_result(&module), 1);
    }

    #[test]
    fn lowers_and_runs_bin_eql_unequal() {
        let mut module = cont::Module::new();

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
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(cont::Code::BinEql(
                            cont::ValueName::from("A"),
                            cont::ValueName::from("B"),
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

        assert_eq!(i32_result(&module), 0);
    }

    #[test]
    fn lowers_and_runs_arr_append() {
        let mut module = cont::Module::new();

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
                    ],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        assert_eq!(i32_result(&module), 9);
    }

    #[test]
    fn lowers_and_runs_sparse_match() {
        let mut module = cont::Module::new();

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
                    values: vec![],
                    blocks: vec![
                        (
                            cont::BlockName::from("b_quote"),
                            cont::Block {
                                params: vec![],
                                region: cont::Region {
                                    values: vec![],
                                    blocks: vec![],
                                    tail: cont::Tail::Jump(cont::JumpTarget {
                                        target: cont::BlockName::from("r"),
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
                                    values: vec![],
                                    blocks: vec![],
                                    tail: cont::Tail::Jump(cont::JumpTarget {
                                        target: cont::BlockName::from("r"),
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
                                    values: vec![],
                                    blocks: vec![],
                                    tail: cont::Tail::Jump(cont::JumpTarget {
                                        target: cont::BlockName::from("r"),
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
                                    values: vec![],
                                    blocks: vec![],
                                    tail: cont::Tail::Jump(cont::JumpTarget {
                                        target: cont::BlockName::from("r"),
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
}
