use {curios::cont, std::collections::BTreeMap};

// Demonstrates that MatchTarget with non-sequential discriminants compiles to
// binary search (nested if/i32.lt_u/i32.eq) rather than br_table.
//
// Dispatches on a byte value using three cases drawn from JSON-relevant ASCII
// codes: 34 ('"'), 91 ('['), 123 ('{'). These are sparse and non-sequential,
// so is_sequential_from_zero returns false and binary_search_instrs is used.
//
// With BYTE = 123 ('{'), the expected result is 3.
fn main() {
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
                preallocs: vec![],
                values: vec![],
                blocks: vec![
                    (
                        cont::BlockName::from("b_quote"),
                        cont::Block {
                            params: vec![],
                            region: cont::Region {
                                preallocs: vec![],
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
                                preallocs: vec![],
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
                                preallocs: vec![],
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
                                preallocs: vec![],
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
                    cases: BTreeMap::from([
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

    module.set_entry(cont::FuncName::from("main"));

    println!("=== cont ===");
    println!("{module}");

    let wasm_module = cont::to_wasm(&module);

    println!();
    println!("=== wasm ===");
    println!("{wasm_module}");

    println!();
    println!("=== result ===");
    curios::run_wasm(&wasm_module, curios_rt::OsHost::new()).unwrap();
}
