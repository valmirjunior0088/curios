use {
    super::{
        AbsHeapType, ArrayType, BlockType, CompType, DataName, DataSegment, Export, Expr,
        FieldName, FieldType, Func, FuncName, FuncType, Global, GlobalName, GlobalType, HeapType,
        Import, Instr, LabelName, LocalName, Module, Mutability, NumType, PackedType, RecType,
        RefType, ResultType, StorageType, StructType, SubType, TypeName, ValType,
    },
    curios_base::{
        Source,
        parser::{
            Parser, ParserError, catch, fail, many0, many1, pure, run_parser, take_eof, take_exact,
            take_while,
        },
    },
    std::str::FromStr,
};

fn parse_whitespace<'a>() -> Parser<'a, &'a str> {
    take_while(|char| char.is_whitespace())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_string<'a>() -> Parser<'a, &'a str> {
    take_exact("\"")
        .and_keep(take_while(|char| char != '"'))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
}

fn is_delimiter(char: char) -> bool {
    char.is_whitespace() || ['(', ')', '$', '"'].contains(&char)
}

/// Parses any `FromStr` numeric type from a delimiter-bounded token, failing
/// with the type's own name (via `type_name`, which for a primitive like
/// `u32`/`f64` is exactly that name) when the token doesn't parse — the one
/// shape every numeric literal (`u32`, `i32`, `i64`, `f32`, `f64`) parses by.
fn parse_number<'a, T>() -> Parser<'a, T>
where
    T: FromStr + 'a,
{
    take_while(|char| !is_delimiter(char))
        .flat_map(|value| match T::from_str(value) {
            Ok(value) => pure(value),
            Err(_) => fail(format!(
                "Expected '{}', obtained '{value}'",
                std::any::type_name::<T>()
            )),
        })
        .and_drop(parse_whitespace())
}

fn parse_name<'a>() -> Parser<'a, &'a str> {
    take_exact("$")
        .and_keep(take_while(|char| !is_delimiter(char)))
        .flat_map(|string| match string.is_empty() {
            true => fail("Expected 'non-empty name'"),
            false => pure(string),
        })
        .and_drop(parse_whitespace())
}

fn parse_type_name<'a>() -> Parser<'a, TypeName> {
    parse_name().map(TypeName::from)
}

fn parse_label_name<'a>() -> Parser<'a, LabelName> {
    parse_name().map(LabelName::from)
}

fn parse_local_name<'a>() -> Parser<'a, LocalName> {
    parse_name().map(LocalName::from)
}

fn parse_global_name<'a>() -> Parser<'a, GlobalName> {
    parse_name().map(GlobalName::from)
}

fn parse_func_name<'a>() -> Parser<'a, FuncName> {
    parse_name().map(FuncName::from)
}

fn parse_field_name<'a>() -> Parser<'a, FieldName> {
    parse_name().map(FieldName::from)
}

fn parse_data_name<'a>() -> Parser<'a, DataName> {
    parse_name().map(DataName::from)
}

fn parse_bytes<'a>() -> Parser<'a, Vec<u8>> {
    take_exact("\"")
        .and_keep(take_while(|c| c != '"').flat_map(|s: &str| {
            let mut bytes = Vec::new();
            let mut chars = s.chars();

            while let Some(c) = chars.next() {
                match c {
                    '\\' => {
                        let hi = match chars.next().and_then(|c| c.to_digit(16)) {
                            Some(d) => d,
                            None => return fail("expected hex digit after '\\'"),
                        };
                        let lo = match chars.next().and_then(|c| c.to_digit(16)) {
                            Some(d) => d,
                            None => return fail("expected second hex digit"),
                        };
                        bytes.push((hi * 16 + lo) as u8);
                    }
                    c => {
                        let mut buf = [0u8; 4];
                        bytes.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                    }
                }
            }

            pure(bytes)
        }))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
}

fn parse_num_type<'a>() -> Parser<'a, NumType> {
    (parse_literal("i32").map(|()| NumType::I32))
        .or(parse_literal("i64").map(|()| NumType::I64))
        .or(parse_literal("f32").map(|()| NumType::F32))
        .or(parse_literal("f64").map(|()| NumType::F64))
}

fn parse_is_nullable<'a>() -> Parser<'a, bool> {
    (parse_literal("null").map(|()| true)).or(pure(false))
}

fn parse_abs_heap_type<'a>() -> Parser<'a, AbsHeapType> {
    (parse_literal("nofunc").map(|()| AbsHeapType::NoFunc))
        .or(parse_literal("noextern").map(|()| AbsHeapType::NoExtern))
        .or(parse_literal("none").map(|()| AbsHeapType::None))
        .or(parse_literal("func").map(|()| AbsHeapType::Func))
        .or(parse_literal("extern").map(|()| AbsHeapType::Extern))
        .or(parse_literal("any").map(|()| AbsHeapType::Any))
        .or(parse_literal("eq").map(|()| AbsHeapType::Eq))
        .or(parse_literal("i31").map(|()| AbsHeapType::I31))
        .or(parse_literal("struct").map(|()| AbsHeapType::Struct))
        .or(parse_literal("array").map(|()| AbsHeapType::Array))
}

fn parse_heap_type<'a>() -> Parser<'a, HeapType> {
    (parse_abs_heap_type().map(HeapType::Abstract)).or(parse_type_name().map(HeapType::Concrete))
}

fn parse_ref_type<'a>() -> Parser<'a, RefType> {
    catch(parse_literal("(").and_drop(parse_literal("ref")))
        .and_keep(parse_is_nullable())
        .and(parse_heap_type())
        .and_drop(parse_literal(")"))
        .map(|(is_nullable, heap_type)| RefType {
            is_nullable,
            heap_type,
        })
}

fn parse_val_type<'a>() -> Parser<'a, ValType> {
    (parse_num_type().map(ValType::Num)).or(parse_ref_type().map(ValType::Ref))
}

fn parse_packed_type<'a>() -> Parser<'a, PackedType> {
    (parse_literal("i8").map(|()| PackedType::I8))
        .or(parse_literal("i16").map(|()| PackedType::I16))
}

fn parse_storage_type<'a>() -> Parser<'a, StorageType> {
    (parse_val_type().map(StorageType::Val)).or(parse_packed_type().map(StorageType::Packed))
}

fn parse_field_type<'a>() -> Parser<'a, FieldType> {
    (catch(parse_literal("(").and_drop(parse_literal("mut")))
        .and_keep(parse_storage_type())
        .and_drop(parse_whitespace())
        .and_drop(parse_literal(")"))
        .map(|storage_type| FieldType {
            storage_type,
            mutability: Mutability::Var,
        }))
    .or(parse_storage_type().map(|storage_type| FieldType {
        storage_type,
        mutability: Mutability::Const,
    }))
}

fn parse_result_type<'a>(keyword: &'static str) -> Parser<'a, ResultType> {
    catch(parse_literal("(").and_drop(parse_literal(keyword)))
        .and_keep(many0(parse_val_type))
        .and_drop(parse_literal(")"))
        .map(ResultType::from)
}

fn parse_func_type<'a>() -> Parser<'a, FuncType> {
    catch(parse_literal("(").and_drop(parse_literal("func")))
        .and_keep(parse_result_type("param").or(pure(ResultType::from([]))))
        .and(parse_result_type("result").or(pure(ResultType::from([]))))
        .and_drop(parse_literal(")"))
        .map(|(inputs, outputs)| FuncType { inputs, outputs })
}

fn parse_array_type<'a>() -> Parser<'a, ArrayType> {
    catch(parse_literal("(").and_drop(parse_literal("array")))
        .and_keep(parse_field_type())
        .and_drop(parse_literal(")"))
        .map(|field_type| ArrayType { field_type })
}

fn parse_field<'a>() -> Parser<'a, (FieldName, FieldType)> {
    catch(parse_literal("(").and_drop(parse_literal("field")))
        .and_keep(parse_field_name())
        .and(parse_field_type())
        .and_drop(parse_literal(")"))
        .map(|(field_name, field_type)| (field_name, field_type))
}

fn parse_struct_type<'a>() -> Parser<'a, StructType> {
    catch(parse_literal("(").and_drop(parse_literal("struct")))
        .and_keep(many0(parse_field))
        .and_drop(parse_literal(")"))
        .map(StructType::from)
}

fn parse_comp_type<'a>() -> Parser<'a, CompType> {
    (parse_func_type().map(CompType::Func))
        .or(parse_struct_type().map(CompType::Struct))
        .or(parse_array_type().map(CompType::Array))
}

fn parse_sub_type<'a>() -> Parser<'a, SubType> {
    (catch(parse_literal("(").and_drop(parse_literal("sub")))
        .and_keep(
            ((parse_literal("final").map(|()| true)).or(pure(false)))
                .and(many0(parse_type_name))
                .and(parse_comp_type()),
        )
        .and_drop(parse_literal(")"))
        .map(|((is_final, super_types), comp_type)| SubType {
            is_final,
            super_types,
            comp_type,
        }))
    .or(parse_comp_type().map(|comp_type| SubType {
        is_final: true,
        super_types: Vec::new(),
        comp_type,
    }))
}

fn parse_type_def<'a>() -> Parser<'a, (TypeName, SubType)> {
    catch(parse_literal("(").and_drop(parse_literal("type")))
        .and_keep(parse_type_name())
        .and(parse_sub_type())
        .and_drop(parse_literal(")"))
        .map(|(type_name, sub_type)| (type_name, sub_type))
}

fn parse_rec_type<'a>() -> Parser<'a, RecType> {
    (catch(parse_literal("(").and_drop(parse_literal("rec")))
        .and_keep(many1(parse_type_def))
        .and_drop(parse_literal(")"))
        .map(RecType::from))
    .or(parse_type_def().map(|(type_name, sub_type)| RecType::from([(type_name, sub_type)])))
}

fn parse_global_type<'a>() -> Parser<'a, GlobalType> {
    (catch(parse_literal("(").and_drop(parse_literal("mut")))
        .and_keep(parse_val_type())
        .and_drop(parse_literal(")"))
        .map(|val_type| GlobalType {
            val_type,
            mutability: Mutability::Var,
        }))
    .or(parse_val_type().map(|val_type| GlobalType {
        val_type,
        mutability: Mutability::Const,
    }))
}

fn parse_block_type<'a>() -> Parser<'a, BlockType> {
    (catch(parse_literal("(").and_drop(parse_literal("result")))
        .and_keep(parse_val_type())
        .and_drop(parse_literal(")"))
        .map(BlockType::Inline))
    .or(catch(parse_literal("(").and_drop(parse_literal("type")))
        .and_keep(parse_type_name())
        .and_drop(parse_literal(")"))
        .map(BlockType::Concrete))
    .or(pure(BlockType::Empty))
}

fn parse_control_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("block")
        .and_keep(parse_label_name())
        .and(parse_block_type())
        .and(many1(parse_instr))
        .and_drop(parse_literal("end"))
        .map(|((label_name, block_type), instructions)| Instr::Block {
            label_name,
            block_type,
            instructions,
        }))
    .or(parse_literal("loop")
        .and_keep(parse_label_name())
        .and(parse_block_type())
        .and(many1(parse_instr))
        .and_drop(parse_literal("end"))
        .map(|((label_name, block_type), instructions)| Instr::Loop {
            label_name,
            block_type,
            instructions,
        }))
    .or(parse_literal("if")
        .and_keep(parse_label_name())
        .and(parse_block_type())
        .and(many1(parse_instr))
        .and(catch(parse_literal("else").and_keep(many1(parse_instr))).or(pure(vec![])))
        .and_drop(parse_literal("end"))
        .map(
            |(((label_name, block_type), then_instructions), else_instructions)| Instr::If {
                label_name,
                block_type,
                then_instructions,
                else_instructions,
            },
        ))
    .or(parse_literal("br_on_cast_fail")
        .and_keep(parse_label_name())
        .and(parse_ref_type())
        .and(parse_ref_type())
        .map(
            |((label_name, source_type), target_type)| Instr::BrOnCastFail {
                label_name,
                source_type,
                target_type,
            },
        ))
    .or(parse_literal("br_on_cast")
        .and_keep(parse_label_name())
        .and(parse_ref_type())
        .and(parse_ref_type())
        .map(|((label_name, source_type), target_type)| Instr::BrOnCast {
            label_name,
            source_type,
            target_type,
        }))
    .or(parse_literal("br_on_non_null")
        .and_keep(parse_label_name())
        .map(|label_name| Instr::BrOnNonNull { label_name }))
    .or(parse_literal("br_on_null")
        .and_keep(parse_label_name())
        .map(|label_name| Instr::BrOnNull { label_name }))
    .or(parse_literal("br_table")
        .and_keep(many1(parse_label_name))
        .map(|mut label_names| (label_names.pop().expect("many1"), label_names))
        .map(|(label_name, label_names)| Instr::BrTable {
            label_names,
            label_name,
        }))
    .or(parse_literal("br_if")
        .and_keep(parse_label_name())
        .map(|label_name| Instr::BrIf { label_name }))
    .or(parse_literal("br")
        .and_keep(parse_label_name())
        .map(|label_name| Instr::Br { label_name }))
    .or(parse_literal("return_call_ref")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ReturnCallRef { type_name }))
    .or(parse_literal("return_call")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::ReturnCall { func_name }))
    .or(parse_literal("return").map(|()| Instr::Return))
    .or(parse_literal("call_ref")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::CallRef { type_name }))
    .or(parse_literal("call")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::Call { func_name }))
    .or(parse_literal("unreachable").map(|()| Instr::Unreachable))
    .or(parse_literal("nop").map(|()| Instr::Nop))
}

fn parse_reference_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("ref.null")
        .and_keep(parse_heap_type())
        .map(|heap_type| Instr::RefNull { heap_type }))
    .or(parse_literal("ref.is_null").map(|()| Instr::RefIsNull))
    .or(parse_literal("ref.func")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::RefFunc { func_name }))
    .or(parse_literal("ref.eq").map(|()| Instr::RefEq))
    .or(parse_literal("ref.as_non_null").map(|()| Instr::RefAsNonNull))
    .or(parse_literal("ref.test")
        .and_keep(parse_ref_type())
        .map(|ref_type| Instr::RefTest { ref_type }))
    .or(parse_literal("ref.cast")
        .and_keep(parse_ref_type())
        .map(|ref_type| Instr::RefCast { ref_type }))
}

fn parse_aggregate_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("struct.new_default")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::StructNewDefault { type_name }))
    .or(parse_literal("struct.new")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::StructNew { type_name }))
    .or(parse_literal("struct.get_s")
        .and_keep(parse_type_name())
        .and(parse_field_name())
        .map(|(type_name, field_name)| Instr::StructGetS {
            type_name,
            field_name,
        }))
    .or(parse_literal("struct.get_u")
        .and_keep(parse_type_name())
        .and(parse_field_name())
        .map(|(type_name, field_name)| Instr::StructGetU {
            type_name,
            field_name,
        }))
    .or(parse_literal("struct.get")
        .and_keep(parse_type_name())
        .and(parse_field_name())
        .map(|(type_name, field_name)| Instr::StructGet {
            type_name,
            field_name,
        }))
    .or(parse_literal("struct.set")
        .and_keep(parse_type_name())
        .and(parse_field_name())
        .map(|(type_name, field_name)| Instr::StructSet {
            type_name,
            field_name,
        }))
    .or(parse_literal("array.new_fixed")
        .and_keep(parse_type_name())
        .and(parse_number::<u32>())
        .map(|(type_name, length)| Instr::ArrayNewFixed { type_name, length }))
    .or(parse_literal("array.new_data")
        .and_keep(parse_type_name())
        .and(parse_data_name())
        .map(|(type_name, data_name)| Instr::ArrayNewData {
            type_name,
            data_name,
        }))
    .or(parse_literal("array.new_default")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayNewDefault { type_name }))
    .or(parse_literal("array.new")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayNew { type_name }))
    .or(parse_literal("array.get_s")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayGetS { type_name }))
    .or(parse_literal("array.get_u")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayGetU { type_name }))
    .or(parse_literal("array.get")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayGet { type_name }))
    .or(parse_literal("array.set")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArraySet { type_name }))
    .or(parse_literal("array.len").map(|()| Instr::ArrayLen))
    .or(parse_literal("array.fill")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayFill { type_name }))
    .or(parse_literal("array.copy")
        .and_keep(parse_type_name())
        .and(parse_type_name())
        .map(|(source_name, target_name)| Instr::ArrayCopy {
            source_name,
            target_name,
        }))
    .or(parse_literal("ref.i31").map(|()| Instr::RefI31))
    .or(parse_literal("i31.get_s").map(|()| Instr::I31GetS))
    .or(parse_literal("i31.get_u").map(|()| Instr::I31GetU))
    .or(parse_literal("any.convert_extern").map(|()| Instr::AnyConvertExtern))
    .or(parse_literal("extern.convert_any").map(|()| Instr::ExternConvertAny))
}

fn parse_select_result<'a>() -> Parser<'a, Vec<ValType>> {
    (parse_result_type("result").map(|ResultType { val_types }| val_types)).or(pure(vec![]))
}

fn parse_select_instr<'a>() -> Parser<'a, Instr> {
    parse_literal("select")
        .and_keep(parse_select_result())
        .map(|val_types| Instr::Select { val_types })
}

fn parse_parametric_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("drop").map(|()| Instr::Drop)).or(parse_select_instr())
}

fn parse_variable_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("local.get")
        .and_keep(parse_local_name())
        .map(|local_name| Instr::LocalGet { local_name }))
    .or(parse_literal("local.set")
        .and_keep(parse_local_name())
        .map(|local_name| Instr::LocalSet { local_name }))
    .or(parse_literal("local.tee")
        .and_keep(parse_local_name())
        .map(|local_name| Instr::LocalTee { local_name }))
    .or(parse_literal("global.get")
        .and_keep(parse_global_name())
        .map(|global_name| Instr::GlobalGet { global_name }))
    .or(parse_literal("global.set")
        .and_keep(parse_global_name())
        .map(|global_name| Instr::GlobalSet { global_name }))
}

fn parse_numeric_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("i32.const")
        .and_keep(parse_number::<i32>())
        .map(|value| Instr::I32Const { value }))
    .or(parse_literal("i64.const")
        .and_keep(parse_number::<i64>())
        .map(|value| Instr::I64Const { value }))
    .or(parse_literal("f32.const")
        .and_keep(parse_number::<f32>())
        .map(|value| Instr::F32Const { value }))
    .or(parse_literal("f64.const")
        .and_keep(parse_number::<f64>())
        .map(|value| Instr::F64Const { value }))
    .or(parse_literal("i32.eqz").map(|()| Instr::I32Eqz))
    .or(parse_literal("i32.eq").map(|()| Instr::I32Eq))
    .or(parse_literal("i32.ne").map(|()| Instr::I32Ne))
    .or(parse_literal("i32.lt_s").map(|()| Instr::I32LtS))
    .or(parse_literal("i32.lt_u").map(|()| Instr::I32LtU))
    .or(parse_literal("i32.gt_s").map(|()| Instr::I32GtS))
    .or(parse_literal("i32.gt_u").map(|()| Instr::I32GtU))
    .or(parse_literal("i32.le_s").map(|()| Instr::I32LeS))
    .or(parse_literal("i32.le_u").map(|()| Instr::I32LeU))
    .or(parse_literal("i32.ge_s").map(|()| Instr::I32GeS))
    .or(parse_literal("i32.ge_u").map(|()| Instr::I32GeU))
    .or(parse_literal("i64.eqz").map(|()| Instr::I64Eqz))
    .or(parse_literal("i64.eq").map(|()| Instr::I64Eq))
    .or(parse_literal("i64.ne").map(|()| Instr::I64Ne))
    .or(parse_literal("i64.lt_s").map(|()| Instr::I64LtS))
    .or(parse_literal("i64.lt_u").map(|()| Instr::I64LtU))
    .or(parse_literal("i64.gt_s").map(|()| Instr::I64GtS))
    .or(parse_literal("i64.gt_u").map(|()| Instr::I64GtU))
    .or(parse_literal("i64.le_s").map(|()| Instr::I64LeS))
    .or(parse_literal("i64.le_u").map(|()| Instr::I64LeU))
    .or(parse_literal("i64.ge_s").map(|()| Instr::I64GeS))
    .or(parse_literal("i64.ge_u").map(|()| Instr::I64GeU))
    .or(parse_literal("f32.eq").map(|()| Instr::F32Eq))
    .or(parse_literal("f32.nearest").map(|()| Instr::F32Nearest))
    .or(parse_literal("f32.neg").map(|()| Instr::F32Neg))
    .or(parse_literal("f32.ne").map(|()| Instr::F32Ne))
    .or(parse_literal("f32.lt").map(|()| Instr::F32Lt))
    .or(parse_literal("f32.gt").map(|()| Instr::F32Gt))
    .or(parse_literal("f32.le").map(|()| Instr::F32Le))
    .or(parse_literal("f32.ge").map(|()| Instr::F32Ge))
    .or(parse_literal("f64.eq").map(|()| Instr::F64Eq))
    .or(parse_literal("f64.nearest").map(|()| Instr::F64Nearest))
    .or(parse_literal("f64.neg").map(|()| Instr::F64Neg))
    .or(parse_literal("f64.ne").map(|()| Instr::F64Ne))
    .or(parse_literal("f64.lt").map(|()| Instr::F64Lt))
    .or(parse_literal("f64.gt").map(|()| Instr::F64Gt))
    .or(parse_literal("f64.le").map(|()| Instr::F64Le))
    .or(parse_literal("f64.ge").map(|()| Instr::F64Ge))
    .or(parse_literal("i32.clz").map(|()| Instr::I32Clz))
    .or(parse_literal("i32.ctz").map(|()| Instr::I32Ctz))
    .or(parse_literal("i32.popcnt").map(|()| Instr::I32Popcnt))
    .or(parse_literal("i32.add").map(|()| Instr::I32Add))
    .or(parse_literal("i32.sub").map(|()| Instr::I32Sub))
    .or(parse_literal("i32.mul").map(|()| Instr::I32Mul))
    .or(parse_literal("i32.div_s").map(|()| Instr::I32DivS))
    .or(parse_literal("i32.div_u").map(|()| Instr::I32DivU))
    .or(parse_literal("i32.rem_s").map(|()| Instr::I32RemS))
    .or(parse_literal("i32.rem_u").map(|()| Instr::I32RemU))
    .or(parse_literal("i32.and").map(|()| Instr::I32And))
    .or(parse_literal("i32.or").map(|()| Instr::I32Or))
    .or(parse_literal("i32.xor").map(|()| Instr::I32Xor))
    .or(parse_literal("i32.shl").map(|()| Instr::I32Shl))
    .or(parse_literal("i32.shr_s").map(|()| Instr::I32ShrS))
    .or(parse_literal("i32.shr_u").map(|()| Instr::I32ShrU))
    .or(parse_literal("i32.rotl").map(|()| Instr::I32Rotl))
    .or(parse_literal("i32.rotr").map(|()| Instr::I32Rotr))
    .or(parse_literal("i64.clz").map(|()| Instr::I64Clz))
    .or(parse_literal("i64.ctz").map(|()| Instr::I64Ctz))
    .or(parse_literal("i64.popcnt").map(|()| Instr::I64Popcnt))
    .or(parse_literal("i64.add").map(|()| Instr::I64Add))
    .or(parse_literal("i64.sub").map(|()| Instr::I64Sub))
    .or(parse_literal("i64.mul").map(|()| Instr::I64Mul))
    .or(parse_literal("i64.div_s").map(|()| Instr::I64DivS))
    .or(parse_literal("i64.div_u").map(|()| Instr::I64DivU))
    .or(parse_literal("i64.rem_s").map(|()| Instr::I64RemS))
    .or(parse_literal("i64.rem_u").map(|()| Instr::I64RemU))
    .or(parse_literal("i64.and").map(|()| Instr::I64And))
    .or(parse_literal("i64.or").map(|()| Instr::I64Or))
    .or(parse_literal("i64.xor").map(|()| Instr::I64Xor))
    .or(parse_literal("i64.shl").map(|()| Instr::I64Shl))
    .or(parse_literal("i64.shr_s").map(|()| Instr::I64ShrS))
    .or(parse_literal("i64.shr_u").map(|()| Instr::I64ShrU))
    .or(parse_literal("i64.rotl").map(|()| Instr::I64Rotl))
    .or(parse_literal("i64.rotr").map(|()| Instr::I64Rotr))
    .or(parse_literal("f32.abs").map(|()| Instr::F32Abs))
    .or(parse_literal("f32.ceil").map(|()| Instr::F32Ceil))
    .or(parse_literal("f32.floor").map(|()| Instr::F32Floor))
    .or(parse_literal("f32.trunc").map(|()| Instr::F32Trunc))
    .or(parse_literal("f32.sqrt").map(|()| Instr::F32Sqrt))
    .or(parse_literal("f32.add").map(|()| Instr::F32Add))
    .or(parse_literal("f32.sub").map(|()| Instr::F32Sub))
    .or(parse_literal("f32.mul").map(|()| Instr::F32Mul))
    .or(parse_literal("f32.div").map(|()| Instr::F32Div))
    .or(parse_literal("f32.min").map(|()| Instr::F32Min))
    .or(parse_literal("f32.max").map(|()| Instr::F32Max))
    .or(parse_literal("f32.copysign").map(|()| Instr::F32Copysign))
    .or(parse_literal("f64.abs").map(|()| Instr::F64Abs))
    .or(parse_literal("f64.ceil").map(|()| Instr::F64Ceil))
    .or(parse_literal("f64.floor").map(|()| Instr::F64Floor))
    .or(parse_literal("f64.trunc").map(|()| Instr::F64Trunc))
    .or(parse_literal("f64.sqrt").map(|()| Instr::F64Sqrt))
    .or(parse_literal("f64.add").map(|()| Instr::F64Add))
    .or(parse_literal("f64.sub").map(|()| Instr::F64Sub))
    .or(parse_literal("f64.mul").map(|()| Instr::F64Mul))
    .or(parse_literal("f64.div").map(|()| Instr::F64Div))
    .or(parse_literal("f64.min").map(|()| Instr::F64Min))
    .or(parse_literal("f64.max").map(|()| Instr::F64Max))
    .or(parse_literal("f64.copysign").map(|()| Instr::F64Copysign))
    .or(parse_literal("i32.wrap_i64").map(|()| Instr::I32WrapI64))
    .or(parse_literal("i32.trunc_f32_s").map(|()| Instr::I32TruncF32S))
    .or(parse_literal("i32.trunc_f32_u").map(|()| Instr::I32TruncF32U))
    .or(parse_literal("i32.trunc_f64_s").map(|()| Instr::I32TruncF64S))
    .or(parse_literal("i32.trunc_f64_u").map(|()| Instr::I32TruncF64U))
    .or(parse_literal("i64.extend_i32_s").map(|()| Instr::I64ExtendI32S))
    .or(parse_literal("i64.extend_i32_u").map(|()| Instr::I64ExtendI32U))
    .or(parse_literal("i64.trunc_f32_s").map(|()| Instr::I64TruncF32S))
    .or(parse_literal("i64.trunc_f32_u").map(|()| Instr::I64TruncF32U))
    .or(parse_literal("i64.trunc_f64_s").map(|()| Instr::I64TruncF64S))
    .or(parse_literal("i64.trunc_f64_u").map(|()| Instr::I64TruncF64U))
    .or(parse_literal("f32.convert_i32_s").map(|()| Instr::F32ConvertI32S))
    .or(parse_literal("f32.convert_i32_u").map(|()| Instr::F32ConvertI32U))
    .or(parse_literal("f32.convert_i64_s").map(|()| Instr::F32ConvertI64S))
    .or(parse_literal("f32.convert_i64_u").map(|()| Instr::F32ConvertI64U))
    .or(parse_literal("f32.demote_f64").map(|()| Instr::F32DemoteF64))
    .or(parse_literal("f64.convert_i32_s").map(|()| Instr::F64ConvertI32S))
    .or(parse_literal("f64.convert_i32_u").map(|()| Instr::F64ConvertI32U))
    .or(parse_literal("f64.convert_i64_s").map(|()| Instr::F64ConvertI64S))
    .or(parse_literal("f64.convert_i64_u").map(|()| Instr::F64ConvertI64U))
    .or(parse_literal("f64.promote_f32").map(|()| Instr::F64PromoteF32))
    .or(parse_literal("i32.reinterpret_f32").map(|()| Instr::I32ReinterpretF32))
    .or(parse_literal("i64.reinterpret_f64").map(|()| Instr::I64ReinterpretF64))
    .or(parse_literal("f32.reinterpret_i32").map(|()| Instr::F32ReinterpretI32))
    .or(parse_literal("f64.reinterpret_i64").map(|()| Instr::F64ReinterpretI64))
    .or(parse_literal("i32.extend8_s").map(|()| Instr::I32Extend8S))
    .or(parse_literal("i32.extend16_s").map(|()| Instr::I32Extend16S))
    .or(parse_literal("i64.extend8_s").map(|()| Instr::I64Extend8S))
    .or(parse_literal("i64.extend16_s").map(|()| Instr::I64Extend16S))
    .or(parse_literal("i64.extend32_s").map(|()| Instr::I64Extend32S))
    .or(parse_literal("i32.trunc_sat_f32_s").map(|()| Instr::I32TruncSatF32S))
    .or(parse_literal("i32.trunc_sat_f32_u").map(|()| Instr::I32TruncSatF32U))
    .or(parse_literal("i32.trunc_sat_f64_s").map(|()| Instr::I32TruncSatF64S))
    .or(parse_literal("i32.trunc_sat_f64_u").map(|()| Instr::I32TruncSatF64U))
    .or(parse_literal("i64.trunc_sat_f32_s").map(|()| Instr::I64TruncSatF32S))
    .or(parse_literal("i64.trunc_sat_f32_u").map(|()| Instr::I64TruncSatF32U))
    .or(parse_literal("i64.trunc_sat_f64_s").map(|()| Instr::I64TruncSatF64S))
    .or(parse_literal("i64.trunc_sat_f64_u").map(|()| Instr::I64TruncSatF64U))
}

fn parse_instr<'a>() -> Parser<'a, Instr> {
    parse_numeric_instr()
        .or(parse_reference_instr())
        .or(parse_aggregate_instr())
        .or(parse_parametric_instr())
        .or(parse_variable_instr())
        .or(parse_control_instr())
}

fn parse_expr<'a>() -> Parser<'a, Expr> {
    many0(parse_instr).map(Expr::from)
}

fn parse_func_import_desc<'a>() -> Parser<'a, Import> {
    catch(parse_literal("(").and_drop(parse_literal("func")))
        .and_keep(parse_func_name())
        .and(
            parse_literal("(")
                .and_drop(parse_literal("type"))
                .and_keep(parse_type_name())
                .and_drop(parse_literal(")")),
        )
        .and_drop(parse_literal(")"))
        .map(|(func_name, type_name)| Import::Func {
            func_name,
            type_name,
        })
}

fn parse_global_import_desc<'a>() -> Parser<'a, Import> {
    catch(parse_literal("(").and_drop(parse_literal("global")))
        .and_keep(parse_global_name())
        .and(parse_global_type())
        .and_drop(parse_literal(")"))
        .map(|(global_name, global_type)| Import::Global {
            global_name,
            global_type,
        })
}

fn parse_import<'a>() -> Parser<'a, (String, String, Import)> {
    catch(parse_literal("(").and_drop(parse_literal("import")))
        .and_keep(parse_string().map(str::to_string))
        .and(parse_string().map(str::to_string))
        .and(parse_func_import_desc().or(parse_global_import_desc()))
        .and_drop(parse_literal(")"))
        .map(|((module_name, name), import)| (module_name, name, import))
}

fn parse_param<'a>() -> Parser<'a, LocalName> {
    catch(parse_literal("(").and_drop(parse_literal("param")))
        .and_keep(parse_local_name())
        .and_drop(parse_val_type())
        .and_drop(parse_literal(")"))
}

fn parse_local<'a>() -> Parser<'a, (LocalName, ValType)> {
    catch(parse_literal("(").and_drop(parse_literal("local")))
        .and_keep(parse_local_name())
        .and(parse_val_type())
        .and_drop(parse_literal(")"))
}

fn parse_func<'a>() -> Parser<'a, (FuncName, Func)> {
    catch(parse_literal("(").and_drop(parse_literal("func")))
        .and_keep(parse_func_name())
        .and(
            parse_literal("(")
                .and_drop(parse_literal("type"))
                .and_keep(parse_type_name())
                .and_drop(parse_literal(")")),
        )
        .and(many0(parse_param))
        .and_drop(parse_result_type("result").or(pure(ResultType::from([]))))
        .and(many0(parse_local))
        .and(parse_expr())
        .and_drop(parse_literal(")"))
        .map(|((((func_name, type_name), params), locals), expr)| {
            (
                func_name,
                Func {
                    type_name,
                    params,
                    locals,
                    expr,
                },
            )
        })
}

fn parse_data_segment<'a>() -> Parser<'a, (DataName, DataSegment)> {
    catch(parse_literal("(").and_drop(parse_literal("data")))
        .and_keep(parse_data_name())
        .and(parse_bytes().map(|bytes| DataSegment { bytes }))
        .and_drop(parse_literal(")"))
}

fn parse_global<'a>() -> Parser<'a, (GlobalName, Global)> {
    catch(parse_literal("(").and_drop(parse_literal("global")))
        .and_keep(parse_global_name())
        .and(parse_global_type())
        .and(parse_expr())
        .and_drop(parse_literal(")"))
        .map(|((global_name, global_type), expr)| (global_name, Global { global_type, expr }))
}

fn parse_func_export_desc<'a>() -> Parser<'a, Export> {
    catch(parse_literal("(").and_drop(parse_literal("func")))
        .and_keep(parse_func_name())
        .and_drop(parse_literal(")"))
        .map(Export::Func)
}

fn parse_global_export_desc<'a>() -> Parser<'a, Export> {
    catch(parse_literal("(").and_drop(parse_literal("global")))
        .and_keep(parse_global_name())
        .and_drop(parse_literal(")"))
        .map(Export::Global)
}

fn parse_export<'a>() -> Parser<'a, (String, Export)> {
    catch(parse_literal("(").and_drop(parse_literal("export")))
        .and_keep(parse_string().map(str::to_string))
        .and((parse_func_export_desc()).or(parse_global_export_desc()))
        .and_drop(parse_literal(")"))
}

fn parse_start<'a>() -> Parser<'a, FuncName> {
    catch(parse_literal("(").and_drop(parse_literal("start")))
        .and_keep(parse_func_name())
        .and_drop(parse_literal(")"))
}

/// A declarative element segment: `(elem declare func $a $b ...)`.
fn parse_elem<'a>() -> Parser<'a, Vec<FuncName>> {
    catch(parse_literal("(").and_drop(parse_literal("elem")))
        .and_drop(parse_literal("declare"))
        .and_drop(parse_literal("func"))
        .and_keep(many0(parse_func_name))
        .and_drop(parse_literal(")"))
}

enum ModuleItem {
    RecType(RecType),
    Import(String, String, Import),
    Func(FuncName, Func),
    Global(GlobalName, Global),
    DataSegment(DataName, DataSegment),
    Export(String, Export),
    Start(FuncName),
    Elem(Vec<FuncName>),
}

fn parse_module_item<'a>() -> Parser<'a, ModuleItem> {
    parse_rec_type()
        .map(ModuleItem::RecType)
        .or(parse_import()
            .map(|(module_name, name, import)| ModuleItem::Import(module_name, name, import)))
        .or(parse_func().map(|(func_name, func)| ModuleItem::Func(func_name, func)))
        .or(parse_global().map(|(global_name, global)| ModuleItem::Global(global_name, global)))
        .or(parse_data_segment()
            .map(|(data_name, data_segment)| ModuleItem::DataSegment(data_name, data_segment)))
        .or(parse_export().map(|(name, export)| ModuleItem::Export(name, export)))
        .or(parse_start().map(ModuleItem::Start))
        .or(parse_elem().map(ModuleItem::Elem))
}

fn parse_module<'a>() -> Parser<'a, Module> {
    catch(parse_literal("(").and_drop(parse_literal("module")))
        .and_keep(parse_name())
        .map(Module::new)
        .and(many0(parse_module_item))
        .map(|(mut module, items)| {
            for item in items {
                match item {
                    ModuleItem::RecType(rec_type) => module.add_types(rec_type),
                    ModuleItem::Import(module_name, name, import) => {
                        module.add_import(module_name, name, import)
                    }
                    ModuleItem::Func(func_name, func) => module.add_func(func_name, func),
                    ModuleItem::Global(global_name, global) => {
                        module.add_global(global_name, global)
                    }
                    ModuleItem::DataSegment(data_name, data_segment) => {
                        module.add_data(data_name, data_segment)
                    }
                    ModuleItem::Export(name, export) => module.add_export(name, export),
                    ModuleItem::Start(func_name) => module.set_start(func_name),
                    ModuleItem::Elem(func_names) => {
                        for func_name in func_names {
                            module.declare_func(func_name);
                        }
                    }
                }
            }

            module
        })
        .and_drop(parse_literal(")"))
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        run_parser(
            parse_whitespace()
                .and_keep(parse_module())
                .and_drop(take_eof()),
            &Source::inline(input),
        )
    }
}
