//! The fixed runtime heap-type shapes every emitted module declares: the data
//! representations (`Flt`, `Bin`, `Arr`, `Cell`) whose structure is
//! program-independent, unlike the per-program families (`tpl/N`, closures,
//! environments, `func/N`) the emitter derives from the module. Kept in one
//! file, exported at the crate root, so each shape has a single spelling —
//! curios-js's bridge builder declares [`bin_sub_type`] verbatim: wasm-GC
//! canonicalizes structural types, so any module declaring the exact shape
//! can exchange `Bin` refs with a compiled program. curios-rt's
//! `host_func_type` mirrors `Bin` and `Arr` in wasmtime's type universe —
//! keep the two ends in sync.

use curios_wasm::{
    ArrayType, CompType, FieldName, FieldType, Mutability, NumType, PackedType, StorageType,
    StructType, SubType, ValType,
};

/// `Flt` — a boxed `f32`: `struct (field $special (f32))`.
pub fn flt_sub_type(special_field: FieldName) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Struct(StructType::from([(
            special_field,
            FieldType {
                storage_type: StorageType::Val(ValType::Num(NumType::F32)),
                mutability: Mutability::Const,
            },
        )])),
    }
}

/// `Bin` — a byte string: `array (mut i8)`.
pub fn bin_sub_type() -> SubType {
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

/// `Arr` — an array of boxed values: `array (mut <top>)`. The element field
/// stays mutable regardless of cyclicity: the array primitives
/// (append/concat/slice) build results with `array.new_default` +
/// per-element `array.set`, so it must be writable.
pub fn arr_sub_type(top_type: ValType) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Array(ArrayType {
            field_type: FieldType {
                storage_type: StorageType::Val(top_type),
                mutability: Mutability::Var,
            },
        }),
    }
}

/// `Cell` — a mutable reference cell: `struct (field $special (mut <top>))`.
pub fn cell_sub_type(special_field: FieldName, top_type: ValType) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Struct(StructType::from([(
            special_field,
            FieldType {
                storage_type: StorageType::Val(top_type),
                mutability: Mutability::Var,
            },
        )])),
    }
}
