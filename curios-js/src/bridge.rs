//! The Bin bridge: a tiny GC module giving JavaScript accessors over the
//! compiler's `$bytes` heap type — the flat payload every `Bin` crosses the
//! host boundary as. JS cannot touch wasm-GC arrays directly, so the harness
//! instantiates this module and reads/builds byte strings through its
//! exports. It declares the compiler's `array (mut i8)` payload shape locally
//! — wasm-GC canonicalizes structural types, so the refs it produces and
//! consumes are interchangeable with a compiled program's, no matter that the
//! two modules were instantiated separately.

use curios_wasm::{
    ArrayType, CompType, Export, Expr, FieldType, Func, FuncName, FuncType, HeapType, Instr,
    LocalName, Module, Mutability, NumType, PackedType, RefType, ResultType, StorageType, SubType,
    TypeName, ValType,
};

/// One accessor's name, parameters, outputs, and array op.
type Accessor = (
    &'static str,
    Vec<(&'static str, ValType)>,
    Vec<ValType>,
    Instr,
);

/// The compiler's `$bytes` host-boundary shape: `array (mut i8)`.
pub(crate) fn bytes_sub_type() -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Array(ArrayType {
            field_type: FieldType {
                storage_type: StorageType::Packed(PackedType::I8),
                mutability: Mutability::Var,
            },
        }),
    }
}

/// The bridge as a `curios_wasm::Module`: the canonical `bytes` type plus the
/// four accessor exports (`bin_len`, `bin_get`, `bin_new`, `bin_set`). Each
/// accessor body is its parameters' `local.get`s followed by one array op.
pub(crate) fn bridge_module() -> Module {
    let mut module = Module::new("bridge");

    let bin = TypeName::from("bytes");

    module.add_type(bin.clone(), bytes_sub_type());

    let bin_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(bin.clone()),
    });

    let i32_val = ValType::Num(NumType::I32);

    let accessors: [Accessor; 4] = [
        (
            "bin_len",
            vec![("b", bin_ref.clone())],
            vec![i32_val.clone()],
            Instr::ArrayLen,
        ),
        (
            "bin_get",
            vec![("b", bin_ref.clone()), ("i", i32_val.clone())],
            vec![i32_val.clone()],
            Instr::ArrayGetU {
                type_name: bin.clone(),
            },
        ),
        (
            "bin_new",
            vec![("n", i32_val.clone())],
            vec![bin_ref.clone()],
            Instr::ArrayNewDefault {
                type_name: bin.clone(),
            },
        ),
        (
            "bin_set",
            vec![
                ("b", bin_ref.clone()),
                ("i", i32_val.clone()),
                ("v", i32_val.clone()),
            ],
            vec![],
            Instr::ArraySet {
                type_name: bin.clone(),
            },
        ),
    ];

    for (name, params, outputs, array_op) in accessors {
        let type_name = TypeName::from(name);
        let func_name = FuncName::from(name);

        module.add_type(
            type_name.clone(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Func(FuncType {
                    inputs: ResultType::from(params.iter().map(|(_, val_type)| val_type.clone())),
                    outputs: ResultType::from(outputs),
                }),
            },
        );

        module.add_func(
            func_name.clone(),
            Func {
                type_name,
                params: params
                    .iter()
                    .map(|(param, _)| LocalName::from(*param))
                    .collect(),
                locals: vec![],
                expr: Expr::from(
                    params
                        .iter()
                        .map(|(param, _)| Instr::LocalGet {
                            local_name: LocalName::from(*param),
                        })
                        .chain([array_op]),
                ),
            },
        );

        module.add_export(name, Export::Func(func_name));
    }

    module
}

/// The encoded bridge, ready for `WebAssembly.instantiate`.
pub(crate) fn bridge_bytes() -> Vec<u8> {
    curios_wasm::to_bytes(&bridge_module())
}
