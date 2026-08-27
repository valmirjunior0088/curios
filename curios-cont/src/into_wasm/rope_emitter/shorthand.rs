//! The shorthands the rope emitters write wasm in: the two reference-type constructors, the local get/set pair, the cast, the struct field accessors, the immediate layout, and the typed null.
//!
//! Nothing here decides anything — each is one `curios_wasm` value spelled in one line instead of four. They live apart from the emitters only so the emitters read as the instruction sequences they are.

use curios_utilities::Grain;

pub(super) fn concrete_ref(
    type_name: curios_wasm::TypeName,
    is_nullable: bool,
) -> curios_wasm::RefType {
    curios_wasm::RefType {
        is_nullable,
        heap_type: curios_wasm::HeapType::Concrete(type_name),
    }
}

pub(super) fn concrete_val(
    type_name: curios_wasm::TypeName,
    is_nullable: bool,
) -> curios_wasm::ValType {
    curios_wasm::ValType::Ref(concrete_ref(type_name, is_nullable))
}

pub(super) fn get(local: &curios_wasm::LocalName) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalGet {
        local_name: local.clone(),
    }
}

pub(super) fn set(local: &curios_wasm::LocalName) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalSet {
        local_name: local.clone(),
    }
}

pub(super) fn cast(type_name: &curios_wasm::TypeName) -> curios_wasm::Instr {
    curios_wasm::Instr::RefCast {
        ref_type: concrete_ref(type_name.clone(), false),
    }
}

pub(super) fn field_get(
    type_name: &curios_wasm::TypeName,
    field_name: &curios_wasm::FieldName,
) -> curios_wasm::Instr {
    curios_wasm::Instr::StructGet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

pub(super) fn field_set(
    type_name: &curios_wasm::TypeName,
    field_name: &curios_wasm::FieldName,
) -> curios_wasm::Instr {
    curios_wasm::Instr::StructSet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

/// One packed grain's immediate layout: where the length lives, what masks the payload, how many payload bytes can be occupied, and how many length units one byte holds.
pub(super) fn immediate_layout(grain: Grain) -> (i32, i32, i32, i32) {
    match grain {
        Grain::X => (29, 0x00FF_FFFF, 3, 1),
        Grain::B => (26, 0x03FF_FFFF, 4, 8),
    }
}

pub(super) fn null(type_name: &curios_wasm::TypeName) -> curios_wasm::Instr {
    curios_wasm::Instr::RefNull {
        heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
    }
}
