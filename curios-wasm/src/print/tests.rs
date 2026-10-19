use crate::*;

/// Code nests as deep as the program it was emitted from, and its text is built a level at a time: ten thousand nested blocks print on the default test thread.
#[test]
fn a_function_nested_ten_thousand_blocks_deep_prints() {
    let mut module = Module::new("module");
    let type_name = TypeName::from("type");
    module.add_type(
        type_name.clone(),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from([]),
                outputs: ResultType::from([]),
            }),
        },
    );
    let mut instrs = vec![Instr::Nop];
    for _ in 0..10_000 {
        instrs = vec![Instr::Block {
            label_name: LabelName::from("block"),
            block_type: BlockType::Empty,
            instructions: instrs,
        }];
    }
    module.add_func(
        FuncName::from("func"),
        Func {
            type_name,
            params: vec![],
            locals: vec![],
            expr: Expr::from(instrs),
        },
    );

    assert!(module.to_string().contains("nop"));
}
