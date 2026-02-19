use curios::wasm::{
    ArrayType, BlockType, CompType, Export, Expr, FieldName, FieldType, Func, FuncName, FuncType,
    Global, GlobalName, GlobalType, Instr, LabelName, LocalName, Module, Mutability, NumType,
    PackedType, ResultType, StorageType, SubType, TypeName, ValType,
};

fn main() {
    let mut module = Module::new("display");

    module.add_type(
        TypeName::from("id"),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from([ValType::Num(NumType::I32)]),
                outputs: ResultType::from([ValType::Num(NumType::I32)]),
            }),
        },
    );

    module.add_type(
        TypeName::from("point"),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Struct(curios::wasm::StructType::from([
                (
                    FieldName::from("x"),
                    FieldType {
                        storage_type: StorageType::Val(ValType::Num(NumType::I32)),
                        mutability: Mutability::Const,
                    },
                ),
                (
                    FieldName::from("y"),
                    FieldType {
                        storage_type: StorageType::Val(ValType::Num(NumType::I32)),
                        mutability: Mutability::Var,
                    },
                ),
            ])),
        },
    );

    module.add_type(
        TypeName::from("bytes"),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Array(ArrayType::from(FieldType {
                storage_type: StorageType::Packed(PackedType::I8),
                mutability: Mutability::Var,
            })),
        },
    );

    module.add_import(
        "env",
        "ext_add",
        curios::wasm::Import::Func {
            func_name: FuncName::from("ext_add"),
            type_name: TypeName::from("id"),
        },
    );

    module.add_func(
        FuncName::from("demo"),
        Func {
            type_name: TypeName::from("id"),
            params: vec![LocalName::from("x")],
            locals: vec![(LocalName::from("tmp"), ValType::Num(NumType::I32))],
            expr: Expr::from([
                Instr::I32Const { value: 41 },
                Instr::LocalSet {
                    local_name: LocalName::from("tmp"),
                },
                Instr::LocalGet {
                    local_name: LocalName::from("x"),
                },
                Instr::I32Const { value: 1 },
                Instr::I32Add,
                Instr::Block {
                    label_name: LabelName::from("outer"),
                    block_type: BlockType::Empty,
                    instructions: vec![Instr::Block {
                        label_name: LabelName::from("inner"),
                        block_type: BlockType::Empty,
                        instructions: vec![Instr::Nop],
                    }],
                },
                Instr::I32Const { value: 2 },
                Instr::StructNew {
                    type_name: TypeName::from("point"),
                },
                Instr::Drop,
                Instr::I32Const { value: 3 },
                Instr::ArrayNewFixed {
                    type_name: TypeName::from("bytes"),
                    length: 1,
                },
                Instr::Drop,
                Instr::LocalGet {
                    local_name: LocalName::from("tmp"),
                },
            ]),
        },
    );

    module.add_global(
        GlobalName::from("answer"),
        Global {
            global_type: GlobalType {
                val_type: ValType::Num(NumType::I32),
                mutability: Mutability::Var,
            },
            expr: Expr::from([Instr::I32Const { value: 41 }]),
        },
    );

    module.add_export("demo", Export::Func(FuncName::from("demo")));

    module.add_export("answer", Export::Global(GlobalName::from("answer")));

    println!("{}", module);
}
