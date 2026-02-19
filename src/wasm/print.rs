use {
    super::{
        AbsHeapType, ArrayType, BlockType, CompType, Export, Expr, FieldName, FieldType, Func,
        FuncName, FuncType, Global, GlobalName, GlobalType, HeapType, Import, Instr, LabelName,
        LocalName, Module, Mutability, NumType, PackedType, RecType, RefType, ResultType,
        StorageType, StructType, SubType, TypeName, ValType,
    },
    crate::monads::printer::{Printer, flat, indent, print, pure, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_dollar_ident<'a>(name: &'a str) -> Printer<'a> {
    flat([pure("$"), pure(name)])
}

fn print_type_name<'a>(type_name: &'a TypeName) -> Printer<'a> {
    print_dollar_ident(&type_name.string)
}

fn print_field_name<'a>(field_name: &'a FieldName) -> Printer<'a> {
    print_dollar_ident(&field_name.string)
}

fn print_func_name<'a>(func_name: &'a FuncName) -> Printer<'a> {
    print_dollar_ident(&func_name.string)
}

fn print_global_name<'a>(global_name: &'a GlobalName) -> Printer<'a> {
    print_dollar_ident(&global_name.string)
}

fn print_local_name<'a>(local_name: &'a LocalName) -> Printer<'a> {
    print_dollar_ident(&local_name.string)
}

fn print_label_name<'a>(label_name: &'a LabelName) -> Printer<'a> {
    print_dollar_ident(&label_name.string)
}

fn print_quoted_ident<'a>(string: &'a str) -> Printer<'a> {
    flat([pure("\""), pure(string), pure("\"")])
}

fn print_num_type<'a>(num_type: &'a NumType) -> Printer<'a> {
    pure(match num_type {
        NumType::I32 => "i32",
        NumType::I64 => "i64",
        NumType::F32 => "f32",
        NumType::F64 => "f64",
    })
}

fn print_abs_heap_type<'a>(abs_heap_type: &'a AbsHeapType) -> Printer<'a> {
    pure(match abs_heap_type {
        AbsHeapType::NoFunc => "nofunc",
        AbsHeapType::NoExtern => "noextern",
        AbsHeapType::None => "none",
        AbsHeapType::Func => "func",
        AbsHeapType::Extern => "extern",
        AbsHeapType::Any => "any",
        AbsHeapType::Eq => "eq",
        AbsHeapType::I31 => "i31",
        AbsHeapType::Struct => "struct",
        AbsHeapType::Array => "array",
    })
}

fn print_heap_type<'a>(heap_type: &'a HeapType) -> Printer<'a> {
    match heap_type {
        HeapType::Abstract(abs_heap_type) => print_abs_heap_type(abs_heap_type),
        HeapType::Concrete(type_name) => print_type_name(type_name),
    }
}

fn print_ref_type<'a>(ref_type: &'a RefType) -> Printer<'a> {
    flat([
        pure("(ref "),
        flat(match ref_type.is_nullable {
            true => Some(pure("null ")),
            false => None,
        }),
        print_heap_type(&ref_type.heap_type),
        pure(")"),
    ])
}

fn print_val_type<'a>(val_type: &'a ValType) -> Printer<'a> {
    match val_type {
        ValType::Num(num_type) => print_num_type(num_type),
        ValType::Ref(ref_type) => print_ref_type(ref_type),
    }
}

fn print_result_type<'a>(keyword: &'a str, result_type: &'a ResultType) -> Printer<'a> {
    flat([
        pure("("),
        pure(keyword),
        flat(
            result_type
                .val_types
                .iter()
                .map(|val_type| flat([pure(" "), print_val_type(val_type)])),
        ),
        pure(")"),
    ])
}

fn print_packed_type<'a>(packed_type: &'a PackedType) -> Printer<'a> {
    pure(match packed_type {
        PackedType::I8 => "i8",
        PackedType::I16 => "i16",
    })
}

fn print_storage_type<'a>(storage_type: &'a StorageType) -> Printer<'a> {
    match storage_type {
        StorageType::Val(val_type) => print_val_type(val_type),
        StorageType::Packed(packed_type) => print_packed_type(packed_type),
    }
}

fn print_field_type<'a>(field_type: &'a FieldType) -> Printer<'a> {
    match field_type.mutability {
        Mutability::Const => print_storage_type(&field_type.storage_type),
        Mutability::Var => flat([
            pure("(mut "),
            print_storage_type(&field_type.storage_type),
            pure(")"),
        ]),
    }
}

fn print_array_type<'a>(array_type: &'a ArrayType) -> Printer<'a> {
    flat([
        pure("(array "),
        print_field_type(&array_type.field_type),
        pure(")"),
    ])
}

fn print_struct_type<'a>(struct_type: &'a StructType) -> Printer<'a> {
    flat([
        pure("(struct"),
        indent(flat(struct_type.fields.iter().map(
            |(field_name, field_type)| {
                flat([
                    pure("\n(field "),
                    print_field_name(field_name),
                    pure(" "),
                    print_field_type(field_type),
                    pure(")"),
                ])
            },
        ))),
        pure(")"),
    ])
}

fn print_func_type<'a>(func_type: &'a FuncType) -> Printer<'a> {
    flat([
        pure("(func"),
        flat(match func_type.inputs.is_empty() {
            true => None,
            false => Some(flat([
                pure(" "),
                print_result_type("param", &func_type.inputs),
            ])),
        }),
        flat(match func_type.outputs.is_empty() {
            true => None,
            false => Some(flat([
                pure(" "),
                print_result_type("result", &func_type.outputs),
            ])),
        }),
        pure(")"),
    ])
}

fn print_comp_type<'a>(comp_type: &'a CompType) -> Printer<'a> {
    match comp_type {
        CompType::Func(func_type) => print_func_type(func_type),
        CompType::Array(array_type) => print_array_type(array_type),
        CompType::Struct(struct_type) => print_struct_type(struct_type),
    }
}

fn print_sub_type<'a>(type_name: &'a TypeName, sub_type: &'a SubType) -> Printer<'a> {
    flat([
        pure("(type "),
        print_type_name(type_name),
        if !sub_type.is_final || !sub_type.super_types.is_empty() {
            flat([
                pure(" (sub"),
                flat(match sub_type.is_final {
                    true => Some(pure(" final")),
                    false => None,
                }),
                flat(
                    sub_type
                        .super_types
                        .iter()
                        .map(|super_type| flat([pure(" "), print_type_name(super_type)])),
                ),
                pure(" "),
                print_comp_type(&sub_type.comp_type),
                pure(")"),
            ])
        } else {
            flat([pure(" "), print_comp_type(&sub_type.comp_type)])
        },
        pure(")"),
    ])
}

fn print_rec_type<'a>(rec_type: &'a RecType) -> Printer<'a> {
    if let [(type_name, sub_type)] = &rec_type.sub_types[..] {
        print_sub_type(type_name, sub_type)
    } else {
        flat([
            pure("(rec"),
            indent(flat(rec_type.sub_types.iter().map(
                |(type_name, sub_type)| flat([pure("\n"), print_sub_type(type_name, sub_type)]),
            ))),
            pure(")"),
        ])
    }
}

fn print_global_type<'a>(global_type: &'a GlobalType) -> Printer<'a> {
    match global_type.mutability {
        Mutability::Const => print_val_type(&global_type.val_type),
        Mutability::Var => flat([
            pure("(mut "),
            print_val_type(&global_type.val_type),
            pure(")"),
        ]),
    }
}

fn print_block_type<'a>(block_type: &'a BlockType) -> Printer<'a> {
    flat(match block_type {
        BlockType::Empty => None,
        BlockType::Inline(val_type) => Some(flat([
            pure("(result "),
            print_val_type(val_type),
            pure(")"),
        ])),
        BlockType::Concrete(type_name) => Some(flat([
            pure("(type "),
            print_type_name(type_name),
            pure(")"),
        ])),
    })
}

fn print_instr<'a>(instr: &'a Instr) -> Printer<'a> {
    match instr {
        Instr::Unreachable => pure("unreachable"),
        Instr::Nop => pure("nop"),
        Instr::Block {
            label_name,
            block_type,
            instructions,
        } => flat([
            pure("block "),
            print_label_name(label_name),
            pure(" "),
            print_block_type(block_type),
            pure("\n"),
            indent(print_instrs(instructions)),
            pure("\nend"),
        ]),
        Instr::Loop {
            label_name,
            block_type,
            instructions,
        } => flat([
            pure("loop "),
            print_label_name(label_name),
            pure(" "),
            print_block_type(block_type),
            pure("\n"),
            indent(print_instrs(instructions)),
            pure("\nend"),
        ]),
        Instr::If {
            label_name,
            block_type,
            then_instructions,
            else_instructions,
        } => flat([
            pure("if "),
            print_label_name(label_name),
            pure(" "),
            print_block_type(block_type),
            pure("\n"),
            indent(print_instrs(then_instructions)),
            flat(match else_instructions.is_empty() {
                true => None,
                false => Some(flat([
                    pure("\nelse\n"),
                    indent(print_instrs(else_instructions)),
                ])),
            }),
            pure("\nend"),
        ]),
        Instr::Br { label_name } => flat([pure("br "), print_label_name(label_name)]),
        Instr::BrIf { label_name } => flat([pure("br_if "), print_label_name(label_name)]),
        Instr::BrTable {
            label_names,
            label_name,
        } => flat([
            pure("br_table"),
            flat(
                label_names
                    .iter()
                    .map(|label_name| flat([pure(" "), print_label_name(label_name)])),
            ),
            pure(" "),
            print_label_name(label_name),
        ]),
        Instr::Return => pure("return"),
        Instr::Call { func_name } => flat([pure("call "), print_func_name(func_name)]),
        Instr::CallRef { type_name } => flat([pure("call_ref "), print_type_name(type_name)]),
        Instr::ReturnCall { func_name } => flat([pure("return_call "), print_func_name(func_name)]),
        Instr::ReturnCallRef { type_name } => {
            flat([pure("return_call_ref "), print_type_name(type_name)])
        }
        Instr::BrOnNull { label_name } => flat([pure("br_on_null "), print_label_name(label_name)]),
        Instr::BrOnNonNull { label_name } => {
            flat([pure("br_on_non_null "), print_label_name(label_name)])
        }
        Instr::BrOnCast {
            label_name,
            source_type,
            target_type,
        } => flat([
            pure("br_on_cast "),
            print_label_name(label_name),
            pure(" "),
            print_ref_type(source_type),
            pure(" "),
            print_ref_type(target_type),
        ]),
        Instr::BrOnCastFail {
            label_name,
            source_type,
            target_type,
        } => flat([
            pure("br_on_cast_fail "),
            print_label_name(label_name),
            pure(" "),
            print_ref_type(source_type),
            pure(" "),
            print_ref_type(target_type),
        ]),
        Instr::RefNull { heap_type } => flat([pure("ref.null "), print_heap_type(heap_type)]),
        Instr::RefIsNull => pure("ref.is_null"),
        Instr::RefFunc { func_name } => flat([pure("ref.func "), print_func_name(func_name)]),
        Instr::RefEq => pure("ref.eq"),
        Instr::RefAsNonNull => pure("ref.as_non_null"),
        Instr::StructNew { type_name } => flat([pure("struct.new "), print_type_name(type_name)]),
        Instr::StructNewDefault { type_name } => {
            flat([pure("struct.new_default "), print_type_name(type_name)])
        }
        Instr::StructGet {
            type_name,
            field_name,
        } => flat([
            pure("struct.get "),
            print_type_name(type_name),
            pure(" "),
            print_field_name(field_name),
        ]),
        Instr::StructGetS {
            type_name,
            field_name,
        } => flat([
            pure("struct.get_s "),
            print_type_name(type_name),
            pure(" "),
            print_field_name(field_name),
        ]),
        Instr::StructGetU {
            type_name,
            field_name,
        } => flat([
            pure("struct.get_u "),
            print_type_name(type_name),
            pure(" "),
            print_field_name(field_name),
        ]),
        Instr::StructSet {
            type_name,
            field_name,
        } => flat([
            pure("struct.set "),
            print_type_name(type_name),
            pure(" "),
            print_field_name(field_name),
        ]),
        Instr::ArrayNew { type_name } => flat([pure("array.new "), print_type_name(type_name)]),
        Instr::ArrayNewDefault { type_name } => {
            flat([pure("array.new_default "), print_type_name(type_name)])
        }
        Instr::ArrayNewFixed { type_name, length } => flat([
            pure("array.new_fixed "),
            print_type_name(type_name),
            pure(" "),
            pure(length.to_string()),
        ]),
        Instr::ArrayGet { type_name } => flat([pure("array.get "), print_type_name(type_name)]),
        Instr::ArrayGetS { type_name } => flat([pure("array.get_s "), print_type_name(type_name)]),
        Instr::ArrayGetU { type_name } => flat([pure("array.get_u "), print_type_name(type_name)]),
        Instr::ArraySet { type_name } => flat([pure("array.set "), print_type_name(type_name)]),
        Instr::ArrayLen => pure("array.len"),
        Instr::ArrayFill { type_name } => flat([pure("array.fill "), print_type_name(type_name)]),
        Instr::ArrayCopy {
            source_name,
            target_name,
        } => flat([
            pure("array.copy "),
            print_type_name(source_name),
            pure(" "),
            print_type_name(target_name),
        ]),
        Instr::RefTest { ref_type } => flat([pure("ref.test "), print_ref_type(ref_type)]),
        Instr::RefCast { ref_type } => flat([pure("ref.cast "), print_ref_type(ref_type)]),
        Instr::AnyConvertExtern => pure("any.convert_extern"),
        Instr::ExternConvertAny => pure("extern.convert_any"),
        Instr::RefI31 => pure("ref.i31"),
        Instr::I31GetS => pure("i31.get_s"),
        Instr::I31GetU => pure("i31.get_u"),
        Instr::Drop => pure("drop"),
        Instr::Select { val_types } => flat([
            pure("select"),
            flat(match val_types.is_empty() {
                true => None,
                false => Some(flat([
                    pure(" (result"),
                    flat(
                        val_types
                            .iter()
                            .map(|val_type| flat([pure(" "), print_val_type(val_type)])),
                    ),
                    pure(")"),
                ])),
            }),
        ]),
        Instr::LocalGet { local_name } => flat([pure("local.get "), print_local_name(local_name)]),
        Instr::LocalSet { local_name } => flat([pure("local.set "), print_local_name(local_name)]),
        Instr::LocalTee { local_name } => flat([pure("local.tee "), print_local_name(local_name)]),
        Instr::GlobalGet { global_name } => {
            flat([pure("global.get "), print_global_name(global_name)])
        }
        Instr::GlobalSet { global_name } => {
            flat([pure("global.set "), print_global_name(global_name)])
        }
        Instr::I32Const { value } => flat([pure("i32.const "), pure(value.to_string())]),
        Instr::I64Const { value } => flat([pure("i64.const "), pure(value.to_string())]),
        Instr::F32Const { value } => flat([pure("f32.const "), pure(value.to_string())]),
        Instr::F64Const { value } => flat([pure("f64.const "), pure(value.to_string())]),
        Instr::I32Eqz => pure("i32.eqz"),
        Instr::I32Eq => pure("i32.eq"),
        Instr::I32Ne => pure("i32.ne"),
        Instr::I32LtS => pure("i32.lt_s"),
        Instr::I32LtU => pure("i32.lt_u"),
        Instr::I32GtS => pure("i32.gt_s"),
        Instr::I32GtU => pure("i32.gt_u"),
        Instr::I32LeS => pure("i32.le_s"),
        Instr::I32LeU => pure("i32.le_u"),
        Instr::I32GeS => pure("i32.ge_s"),
        Instr::I32GeU => pure("i32.ge_u"),
        Instr::I64Eqz => pure("i64.eqz"),
        Instr::I64Eq => pure("i64.eq"),
        Instr::I64Ne => pure("i64.ne"),
        Instr::I64LtS => pure("i64.lt_s"),
        Instr::I64LtU => pure("i64.lt_u"),
        Instr::I64GtS => pure("i64.gt_s"),
        Instr::I64GtU => pure("i64.gt_u"),
        Instr::I64LeS => pure("i64.le_s"),
        Instr::I64LeU => pure("i64.le_u"),
        Instr::I64GeS => pure("i64.ge_s"),
        Instr::I64GeU => pure("i64.ge_u"),
        Instr::F32Eq => pure("f32.eq"),
        Instr::F32Ne => pure("f32.ne"),
        Instr::F32Lt => pure("f32.lt"),
        Instr::F32Gt => pure("f32.gt"),
        Instr::F32Le => pure("f32.le"),
        Instr::F32Ge => pure("f32.ge"),
        Instr::F64Eq => pure("f64.eq"),
        Instr::F64Ne => pure("f64.ne"),
        Instr::F64Lt => pure("f64.lt"),
        Instr::F64Gt => pure("f64.gt"),
        Instr::F64Le => pure("f64.le"),
        Instr::F64Ge => pure("f64.ge"),
        Instr::I32Clz => pure("i32.clz"),
        Instr::I32Ctz => pure("i32.ctz"),
        Instr::I32Popcnt => pure("i32.popcnt"),
        Instr::I32Add => pure("i32.add"),
        Instr::I32Sub => pure("i32.sub"),
        Instr::I32Mul => pure("i32.mul"),
        Instr::I32DivS => pure("i32.div_s"),
        Instr::I32DivU => pure("i32.div_u"),
        Instr::I32RemS => pure("i32.rem_s"),
        Instr::I32RemU => pure("i32.rem_u"),
        Instr::I32And => pure("i32.and"),
        Instr::I32Or => pure("i32.or"),
        Instr::I32Xor => pure("i32.xor"),
        Instr::I32Shl => pure("i32.shl"),
        Instr::I32ShrS => pure("i32.shr_s"),
        Instr::I32ShrU => pure("i32.shr_u"),
        Instr::I32Rotl => pure("i32.rotl"),
        Instr::I32Rotr => pure("i32.rotr"),
        Instr::I64Clz => pure("i64.clz"),
        Instr::I64Ctz => pure("i64.ctz"),
        Instr::I64Popcnt => pure("i64.popcnt"),
        Instr::I64Add => pure("i64.add"),
        Instr::I64Sub => pure("i64.sub"),
        Instr::I64Mul => pure("i64.mul"),
        Instr::I64DivS => pure("i64.div_s"),
        Instr::I64DivU => pure("i64.div_u"),
        Instr::I64RemS => pure("i64.rem_s"),
        Instr::I64RemU => pure("i64.rem_u"),
        Instr::I64And => pure("i64.and"),
        Instr::I64Or => pure("i64.or"),
        Instr::I64Xor => pure("i64.xor"),
        Instr::I64Shl => pure("i64.shl"),
        Instr::I64ShrS => pure("i64.shr_s"),
        Instr::I64ShrU => pure("i64.shr_u"),
        Instr::I64Rotl => pure("i64.rotl"),
        Instr::I64Rotr => pure("i64.rotr"),
        Instr::F32Abs => pure("f32.abs"),
        Instr::F32Neg => pure("f32.neg"),
        Instr::F32Ceil => pure("f32.ceil"),
        Instr::F32Floor => pure("f32.floor"),
        Instr::F32Trunc => pure("f32.trunc"),
        Instr::F32Nearest => pure("f32.nearest"),
        Instr::F32Sqrt => pure("f32.sqrt"),
        Instr::F32Add => pure("f32.add"),
        Instr::F32Sub => pure("f32.sub"),
        Instr::F32Mul => pure("f32.mul"),
        Instr::F32Div => pure("f32.div"),
        Instr::F32Min => pure("f32.min"),
        Instr::F32Max => pure("f32.max"),
        Instr::F32Copysign => pure("f32.copysign"),
        Instr::F64Abs => pure("f64.abs"),
        Instr::F64Neg => pure("f64.neg"),
        Instr::F64Ceil => pure("f64.ceil"),
        Instr::F64Floor => pure("f64.floor"),
        Instr::F64Trunc => pure("f64.trunc"),
        Instr::F64Nearest => pure("f64.nearest"),
        Instr::F64Sqrt => pure("f64.sqrt"),
        Instr::F64Add => pure("f64.add"),
        Instr::F64Sub => pure("f64.sub"),
        Instr::F64Mul => pure("f64.mul"),
        Instr::F64Div => pure("f64.div"),
        Instr::F64Min => pure("f64.min"),
        Instr::F64Max => pure("f64.max"),
        Instr::F64Copysign => pure("f64.copysign"),
        Instr::I32WrapI64 => pure("i32.wrap_i64"),
        Instr::I32TruncF32S => pure("i32.trunc_f32_s"),
        Instr::I32TruncF32U => pure("i32.trunc_f32_u"),
        Instr::I32TruncF64S => pure("i32.trunc_f64_s"),
        Instr::I32TruncF64U => pure("i32.trunc_f64_u"),
        Instr::I64ExtendI32S => pure("i64.extend_i32_s"),
        Instr::I64ExtendI32U => pure("i64.extend_i32_u"),
        Instr::I64TruncF32S => pure("i64.trunc_f32_s"),
        Instr::I64TruncF32U => pure("i64.trunc_f32_u"),
        Instr::I64TruncF64S => pure("i64.trunc_f64_s"),
        Instr::I64TruncF64U => pure("i64.trunc_f64_u"),
        Instr::F32ConvertI32S => pure("f32.convert_i32_s"),
        Instr::F32ConvertI32U => pure("f32.convert_i32_u"),
        Instr::F32ConvertI64S => pure("f32.convert_i64_s"),
        Instr::F32ConvertI64U => pure("f32.convert_i64_u"),
        Instr::F32DemoteF64 => pure("f32.demote_f64"),
        Instr::F64ConvertI32S => pure("f64.convert_i32_s"),
        Instr::F64ConvertI32U => pure("f64.convert_i32_u"),
        Instr::F64ConvertI64S => pure("f64.convert_i64_s"),
        Instr::F64ConvertI64U => pure("f64.convert_i64_u"),
        Instr::F64PromoteF32 => pure("f64.promote_f32"),
        Instr::I32ReinterpretF32 => pure("i32.reinterpret_f32"),
        Instr::I64ReinterpretF64 => pure("i64.reinterpret_f64"),
        Instr::F32ReinterpretI32 => pure("f32.reinterpret_i32"),
        Instr::F64ReinterpretI64 => pure("f64.reinterpret_i64"),
        Instr::I32Extend8S => pure("i32.extend8_s"),
        Instr::I32Extend16S => pure("i32.extend16_s"),
        Instr::I64Extend8S => pure("i64.extend8_s"),
        Instr::I64Extend16S => pure("i64.extend16_s"),
        Instr::I64Extend32S => pure("i64.extend32_s"),
        Instr::I32TruncSatF32S => pure("i32.trunc_sat_f32_s"),
        Instr::I32TruncSatF32U => pure("i32.trunc_sat_f32_u"),
        Instr::I32TruncSatF64S => pure("i32.trunc_sat_f64_s"),
        Instr::I32TruncSatF64U => pure("i32.trunc_sat_f64_u"),
        Instr::I64TruncSatF32S => pure("i64.trunc_sat_f32_s"),
        Instr::I64TruncSatF32U => pure("i64.trunc_sat_f32_u"),
        Instr::I64TruncSatF64S => pure("i64.trunc_sat_f64_s"),
        Instr::I64TruncSatF64U => pure("i64.trunc_sat_f64_u"),
    }
}

fn print_instrs<'a>(instrs: &'a [Instr]) -> Printer<'a> {
    sep_flat(instrs.iter().map(print_instr), || pure("\n"))
}

fn print_expr<'a>(expr: &'a Expr) -> Printer<'a> {
    print_instrs(&expr.instrs)
}

fn print_import<'a>(module_name: &'a str, name: &'a str, import: &'a Import) -> Printer<'a> {
    flat([
        pure("(import "),
        print_quoted_ident(module_name),
        pure(" "),
        print_quoted_ident(name),
        match import {
            Import::Func {
                func_name,
                type_name,
            } => flat([
                pure(" (func "),
                print_func_name(func_name),
                pure(" (type "),
                print_type_name(type_name),
                pure(")"),
                pure(")"),
            ]),
            Import::Global {
                global_name,
                global_type,
            } => flat([
                pure(" (global "),
                print_global_name(global_name),
                pure(" "),
                print_global_type(global_type),
                pure(")"),
            ]),
        },
        pure(")"),
    ])
}

fn print_func<'a>(module: &'a Module, func_name: &'a FuncName, func: &'a Func) -> Printer<'a> {
    let func_type = module
        .get_type(&func.type_name)
        .and_then(|sub_type| sub_type.func_type())
        .expect(&format!(
            "Unexpected error while getting func type `{}`",
            &func_name.string
        ));

    flat([
        pure("(func "),
        print_func_name(func_name),
        pure(" (type "),
        print_type_name(&func.type_name),
        pure(")"),
        indent(flat([
            flat({
                let mut iter = func.params.iter().zip(func_type.inputs());

                iter.next()
                    .into_iter()
                    .map(|(local_name, val_type)| {
                        flat([
                            pure("\n(param "),
                            print_local_name(local_name),
                            pure(" "),
                            print_val_type(val_type),
                            pure(")"),
                        ])
                    })
                    .chain(iter.map(|(local_name, val_type)| {
                        flat([
                            pure(" (param "),
                            print_local_name(local_name),
                            pure(" "),
                            print_val_type(val_type),
                            pure(")"),
                        ])
                    }))
            }),
            flat(match func_type.outputs.is_empty() {
                true => None,
                false => Some(flat([
                    pure(" "),
                    print_result_type("result", &func_type.outputs),
                ])),
            }),
            flat({
                let mut iter = func.locals.iter();

                iter.next()
                    .into_iter()
                    .map(|(local_name, val_type)| {
                        flat([
                            pure("\n(local "),
                            print_local_name(local_name),
                            pure(" "),
                            print_val_type(val_type),
                            pure(")"),
                        ])
                    })
                    .chain(iter.map(|(local_name, val_type)| {
                        flat([
                            pure(" (local "),
                            print_local_name(local_name),
                            pure(" "),
                            print_val_type(val_type),
                            pure(")"),
                        ])
                    }))
            }),
            pure("\n"),
            print_expr(&func.expr),
        ])),
        pure(")"),
    ])
}

fn print_global<'a>(global_name: &'a GlobalName, global: &'a Global) -> Printer<'a> {
    flat([
        pure("(global "),
        print_global_name(global_name),
        pure(" "),
        print_global_type(&global.global_type),
        pure("\n"),
        indent(print_expr(&global.expr)),
        pure(")"),
    ])
}

fn print_export<'a>(name: &'a str, export: &'a Export) -> Printer<'a> {
    flat([
        pure("(export "),
        print_quoted_ident(name),
        match export {
            Export::Func(func_name) => {
                flat([pure(" (func "), print_func_name(func_name), pure(")")])
            }
            Export::Global(global_name) => {
                flat([pure(" (global "), print_global_name(global_name), pure(")")])
            }
        },
        pure(")"),
    ])
}

fn print_module<'a>(module: &'a Module) -> Printer<'a> {
    flat([
        pure("(module "),
        print_dollar_ident(module.name()),
        indent(flat(
            (module
                .types()
                .iter()
                .map(|rec_type| flat([pure("\n"), print_rec_type(rec_type)])))
            .chain(module.imports().iter().map(|(module_name, name, import)| {
                flat([pure("\n"), print_import(module_name, name, import)])
            }))
            .chain(
                module.funcs().iter().map(|(func_name, func)| {
                    flat([pure("\n"), print_func(module, func_name, func)])
                }),
            )
            .chain(
                module.globals().iter().map(|(global_name, global)| {
                    flat([pure("\n"), print_global(global_name, global)])
                }),
            )
            .chain(
                module
                    .exports()
                    .iter()
                    .map(|(name, export)| flat([pure("\n"), print_export(name, export)])),
            ),
        )),
        pure(")"),
    ])
}

impl Display for Module {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        print(print_module(self), formatter)?;

        Ok(())
    }
}
