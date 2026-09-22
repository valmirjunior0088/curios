//! The shorthands the helper emitters — the rope helpers and the big-number helpers — write wasm in: the reference-type constructors, the local get/set/tee trio, the cast, the struct field accessors, the typed null, the constants, the calls and the structured control.
//!
//! Nothing here decides anything — each is one `curios_wasm` value spelled in one line instead of four. They live apart from the emitters only so the emitters read as the instruction sequences they are.

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

pub(super) fn null(type_name: &curios_wasm::TypeName) -> curios_wasm::Instr {
    curios_wasm::Instr::RefNull {
        heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
    }
}

pub(super) fn tee(local: &curios_wasm::LocalName) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalTee {
        local_name: local.clone(),
    }
}

pub(super) fn i32_const(value: i32) -> curios_wasm::Instr {
    curios_wasm::Instr::I32Const { value }
}

pub(super) fn i64_const(value: i64) -> curios_wasm::Instr {
    curios_wasm::Instr::I64Const { value }
}

pub(super) fn call(func_name: &curios_wasm::FuncName) -> curios_wasm::Instr {
    curios_wasm::Instr::Call {
        func_name: func_name.clone(),
    }
}

pub(super) fn br(label: &str) -> curios_wasm::Instr {
    curios_wasm::Instr::Br {
        label_name: curios_wasm::LabelName::from(label),
    }
}

pub(super) fn br_if(label: &str) -> curios_wasm::Instr {
    curios_wasm::Instr::BrIf {
        label_name: curios_wasm::LabelName::from(label),
    }
}

/// A `block` producing nothing, which a `br` to `label` leaves.
pub(super) fn block(label: &str, instructions: Vec<curios_wasm::Instr>) -> curios_wasm::Instr {
    curios_wasm::Instr::Block {
        label_name: curios_wasm::LabelName::from(label),
        block_type: curios_wasm::BlockType::Empty,
        instructions,
    }
}

/// A `loop` producing nothing, which a `br` to `label` restarts.
pub(super) fn repeat(label: &str, instructions: Vec<curios_wasm::Instr>) -> curios_wasm::Instr {
    curios_wasm::Instr::Loop {
        label_name: curios_wasm::LabelName::from(label),
        block_type: curios_wasm::BlockType::Empty,
        instructions,
    }
}

/// An `if` with no `else` and no result, over the condition already on the stack.
pub(super) fn when(then_instructions: Vec<curios_wasm::Instr>) -> curios_wasm::Instr {
    curios_wasm::Instr::If {
        label_name: curios_wasm::LabelName::from("when"),
        block_type: curios_wasm::BlockType::Empty,
        then_instructions,
        else_instructions: vec![],
    }
}

/// An `if` whose two arms each leave one `result`, over the condition already on the stack.
pub(super) fn either(
    result: curios_wasm::ValType,
    then_instructions: Vec<curios_wasm::Instr>,
    else_instructions: Vec<curios_wasm::Instr>,
) -> curios_wasm::Instr {
    curios_wasm::Instr::If {
        label_name: curios_wasm::LabelName::from("either"),
        block_type: curios_wasm::BlockType::Inline(result),
        then_instructions,
        else_instructions,
    }
}
