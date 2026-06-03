use curios::wasm;

fn main() {
    let mut wasm_module = wasm::Module::new("display");

    wasm_module.add_type(
        wasm::TypeName::from("id"),
        wasm::SubType {
            is_final: true,
            super_types: vec![],
            comp_type: wasm::CompType::Func(wasm::FuncType {
                inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
                outputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
            }),
        },
    );

    wasm_module.add_type(
        wasm::TypeName::from("point"),
        wasm::SubType {
            is_final: true,
            super_types: vec![],
            comp_type: wasm::CompType::Struct(wasm::StructType::from([
                (
                    wasm::FieldName::from("x"),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(wasm::ValType::Num(
                            wasm::NumType::I32,
                        )),
                        mutability: wasm::Mutability::Const,
                    },
                ),
                (
                    wasm::FieldName::from("y"),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(wasm::ValType::Num(
                            wasm::NumType::I32,
                        )),
                        mutability: wasm::Mutability::Var,
                    },
                ),
            ])),
        },
    );

    wasm_module.add_type(
        wasm::TypeName::from("bytes"),
        wasm::SubType {
            is_final: true,
            super_types: vec![],
            comp_type: wasm::CompType::Array(wasm::ArrayType {
                field_type: wasm::FieldType {
                    storage_type: wasm::StorageType::Packed(wasm::PackedType::I8),
                    mutability: wasm::Mutability::Var,
                },
            }),
        },
    );

    wasm_module.add_import(
        "env",
        "ext_add",
        wasm::Import::Func {
            func_name: wasm::FuncName::from("ext_add"),
            type_name: wasm::TypeName::from("id"),
        },
    );

    wasm_module.add_func(
        wasm::FuncName::from("demo"),
        wasm::Func {
            type_name: wasm::TypeName::from("id"),
            params: vec![wasm::LocalName::from("x")],
            locals: vec![(
                wasm::LocalName::from("tmp"),
                wasm::ValType::Num(wasm::NumType::I32),
            )],
            expr: wasm::Expr::from([
                wasm::Instr::I32Const { value: 41 },
                wasm::Instr::LocalSet {
                    local_name: wasm::LocalName::from("tmp"),
                },
                wasm::Instr::LocalGet {
                    local_name: wasm::LocalName::from("x"),
                },
                wasm::Instr::I32Const { value: 1 },
                wasm::Instr::I32Add,
                wasm::Instr::Block {
                    label_name: wasm::LabelName::from("outer"),
                    block_type: wasm::BlockType::Empty,
                    instructions: vec![wasm::Instr::Block {
                        label_name: wasm::LabelName::from("inner"),
                        block_type: wasm::BlockType::Empty,
                        instructions: vec![wasm::Instr::Nop],
                    }],
                },
                wasm::Instr::I32Const { value: 2 },
                wasm::Instr::StructNew {
                    type_name: wasm::TypeName::from("point"),
                },
                wasm::Instr::Drop,
                wasm::Instr::I32Const { value: 3 },
                wasm::Instr::ArrayNewFixed {
                    type_name: wasm::TypeName::from("bytes"),
                    length: 1,
                },
                wasm::Instr::Drop,
                wasm::Instr::LocalGet {
                    local_name: wasm::LocalName::from("tmp"),
                },
            ]),
        },
    );

    wasm_module.add_global(
        wasm::GlobalName::from("answer"),
        wasm::Global {
            global_type: wasm::GlobalType {
                val_type: wasm::ValType::Num(wasm::NumType::I32),
                mutability: wasm::Mutability::Var,
            },
            expr: wasm::Expr::from([wasm::Instr::I32Const { value: 41 }]),
        },
    );

    wasm_module.add_export("demo", wasm::Export::Func(wasm::FuncName::from("demo")));

    wasm_module.add_export(
        "answer",
        wasm::Export::Global(wasm::GlobalName::from("answer")),
    );

    println!("=== wasm ===");
    println!("{wasm_module}");
}
