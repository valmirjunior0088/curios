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
    use {
        super::*,
        wasmtime::{AnyRef, Config, Engine, Instance, Module, Rooted, Store},
    };

    fn run_main(module: &cont::Module) -> (Store<()>, Rooted<AnyRef>) {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_function_references(true);
        config.wasm_gc(true);
        config.wasm_tail_call(true);

        let engine = Engine::new(&config).expect("expected wasmtime engine");

        let module = Module::from_binary(&engine, &wasm::to_bytes(&to_wasm(module)))
            .expect("expected wasm module");

        let mut store = Store::new(&engine, ());

        let instance = Instance::new(&mut store, &module, &[]).expect("expected instance");

        let run = instance
            .get_typed_func::<(), Rooted<AnyRef>>(&mut store, "func/main")
            .expect("expected exported func/main");

        let result = run.call(&mut store, ()).expect("expected call result");

        (store, result)
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
                            cont::Value::Eval(cont::Code::TplProj(0), vec![cont::ValueName::from("x")]),
                        ),
                        (
                            cont::ValueName::from("out"),
                            cont::Value::Eval(cont::Code::TplProj(0), vec![cont::ValueName::from("left")]),
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

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 2);
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
                        cont::Value::Eval(
                            cont::Code::IntEql,
                            vec![cont::ValueName::from("n"), cont::ValueName::from("ZERO")],
                        ),
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
                                        cont::Value::Eval(
                                            cont::Code::IntSub,
                                            vec![
                                                cont::ValueName::from("n"),
                                                cont::ValueName::from("ONE"),
                                            ],
                                        ),
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
                    tail: cont::Tail::Case(cont::CaseTarget {
                        operand: cont::ValueName::from("is_zero"),
                        targets: vec![cont::JumpTarget {
                            target: cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        }],
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
                        cont::Value::Eval(
                            cont::Code::IntEql,
                            vec![cont::ValueName::from("n"), cont::ValueName::from("ZERO")],
                        ),
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
                                        cont::Value::Eval(
                                            cont::Code::IntSub,
                                            vec![
                                                cont::ValueName::from("n"),
                                                cont::ValueName::from("ONE"),
                                            ],
                                        ),
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
                    tail: cont::Tail::Case(cont::CaseTarget {
                        operand: cont::ValueName::from("is_zero"),
                        targets: vec![cont::JumpTarget {
                            target: cont::BlockName::from("on_non_zero"),
                            params: vec![],
                        }],
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

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 22);
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
                        cont::Value::Eval(
                            cont::Code::IntAdd,
                            vec![cont::ValueName::from("x"), cont::ValueName::from("ONE")],
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

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 3);
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

        let (store, result) = run_main(&module);

        let result = result.unwrap_struct(&store).expect("expected unit struct");

        assert_eq!(
            result
                .ty(&store)
                .expect("expected struct type")
                .fields()
                .len(),
            0
        );
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
                        cont::Value::Eval(
                            cont::Code::FltAdd,
                            vec![
                                cont::ValueName::from("LEFT"),
                                cont::ValueName::from("RIGHT"),
                            ],
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

        let (mut store, result) = run_main(&cont_module);

        let result = result
            .unwrap_struct(&store)
            .expect("expected float struct")
            .field(&mut store, 0)
            .expect("expected float field")
            .unwrap_f32();

        assert_eq!(result, 3.75);
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
                        cont::Value::Eval(
                            cont::Code::TplProj(1),
                            vec![cont::ValueName::from("PAIR")],
                        ),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("out")],
                    }),
                },
            },
        );

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 2);
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
                        cont::Value::Eval(
                            cont::Code::IntAdd,
                            vec![
                                cont::ValueName::from("x"),
                                cont::ValueName::from("bias"),
                            ],
                        ),
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

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 8);
    }

    #[test]
    fn lowers_and_runs_bln_not() {
        let mut module = cont::Module::new();

        module.add_const(cont::ValueName::from("T"), cont::Data::Bln(true));

        module.add_func(
            cont::FuncName::from("main"),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from("r"),
                region: cont::Region {
                    values: vec![(
                        cont::ValueName::from("result"),
                        cont::Value::Eval(
                            cont::Code::BlnNot,
                            vec![cont::ValueName::from("T")],
                        ),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 0);
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
                        cont::Value::Eval(
                            cont::Code::NatAdd,
                            vec![
                                cont::ValueName::from("THREE"),
                                cont::ValueName::from("FOUR"),
                            ],
                        ),
                    )],
                    blocks: vec![],
                    tail: cont::Tail::Jump(cont::JumpTarget {
                        target: cont::BlockName::from("r"),
                        params: vec![cont::ValueName::from("result")],
                    }),
                },
            },
        );

        let (store, result) = run_main(&module);

        let result = result
            .unwrap_i31(&store)
            .expect("expected i31 result")
            .get_i32();

        assert_eq!(result, 7);
    }
}
