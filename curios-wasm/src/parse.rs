#[cfg(test)]
mod tests;

use {
    super::{
        AbsHeapType, AddressType, ArrayType, BlockType, CompType, DataMode, DataName, DataSegment,
        ElemList, ElemMode, ElemName, ElemSegment, Export, Expr, FieldName, FieldType, Func,
        FuncName, FuncType, Global, GlobalName, GlobalType, HeapType, Import, Instr, LabelName,
        Limits, LocalName, MemArg, MemName, MemType, Module, Mutability, NumType, PackedType,
        RecType, RefType, ResultType, StorageType, StructType, SubType, Table, TableName,
        TableType, TypeName, ValType,
    },
    curios_parse::{
        Parser, ParserError, catch, fail, many0, many1, pure, run_parser, take_eof, take_exact,
        take_while,
    },
    curios_utilities::Source,
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

// `$` is only a sigil, never a delimiter: emitted names carry interior `$`s (data segments are `{value-name}${index}`, hints are `$`-separated), and printed names are always whitespace- or paren-terminated.
fn is_delimiter(char: char) -> bool {
    char.is_whitespace() || ['(', ')', '"'].contains(&char)
}

/// Parses any `FromStr` numeric type from a delimiter-bounded token, failing with the type's own name (via `type_name`, which for an intrinsic like `u32`/`f64` is exactly that name) when the token doesn't parse — the one shape every numeric literal (`u32`, `i32`, `i64`, `f32`, `f64`) parses by.
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

fn parse_table_name<'a>() -> Parser<'a, TableName> {
    parse_name().map(TableName::from)
}

fn parse_elem_name<'a>() -> Parser<'a, ElemName> {
    parse_name().map(ElemName::from)
}

fn parse_mem_name<'a>() -> Parser<'a, MemName> {
    parse_name().map(MemName::from)
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

fn parse_address_type<'a>() -> Parser<'a, AddressType> {
    (parse_literal("i32").map(|()| AddressType::I32))
        .or(parse_literal("i64").map(|()| AddressType::I64))
}

/// A minimum and an optional maximum. The maximum's absence is read off the next token failing to be a number, which is why every construct spelling limits puts something that is not one — a reference type, or a closing paren — immediately after them.
fn parse_limits<'a>() -> Parser<'a, Limits> {
    parse_number::<u64>()
        .and(catch(parse_number::<u64>()).map(Some).or(pure(None)))
        .map(|(min, max)| Limits { min, max })
}

fn parse_table_type<'a>() -> Parser<'a, TableType> {
    parse_address_type()
        .and(parse_limits())
        .and(parse_ref_type())
        .map(|((address_type, limits), ref_type)| TableType {
            address_type,
            ref_type,
            limits,
        })
}

fn parse_mem_type<'a>() -> Parser<'a, MemType> {
    parse_address_type()
        .and(parse_limits())
        .map(|(address_type, limits)| MemType {
            address_type,
            limits,
        })
}

/// An `align=` byte count, as the log2 exponent the model carries. The text form spells bytes because that is what a wasm reader expects to see; anything but a power of two is a text the printer could not have produced.
fn parse_align<'a>() -> Parser<'a, u32> {
    catch(take_exact("align="))
        .and_keep(parse_number::<u64>())
        .flat_map(|bytes| match bytes.is_power_of_two() {
            true => pure(bytes.trailing_zeros()),
            false => fail(format!("Expected 'a power of two', obtained '{bytes}'")),
        })
}

/// A load or store's immediate, in the order the printer writes it: the memory, then an offset defaulting to zero, then an alignment defaulting to the access width's natural one.
fn parse_mem_arg<'a>(natural_align: u32) -> Parser<'a, MemArg> {
    parse_mem_name()
        .and((catch(take_exact("offset=")).and_keep(parse_number::<u64>())).or(pure(0)))
        .and(parse_align().or(pure(natural_align)))
        .map(|((mem_name, offset), align)| MemArg {
            mem_name,
            align,
            offset,
        })
}

/// Any load or store: one delimiter-bounded token looked up in the memory-access table beside `Instr`, then its immediate — whole-token dispatch, so no spelling can prefix-shadow another the way an ordered chain of literal probes can.
fn parse_mem_access_instr<'a>() -> Parser<'a, Instr> {
    catch(
        take_while(|char| !is_delimiter(char))
            .flat_map(|token| match Instr::from_mem_mnemonic(token) {
                Some(access) => pure(access),
                None => fail(format!("Expected 'memory instruction', obtained '{token}'")),
            })
            .and_drop(parse_whitespace()),
    )
    .flat_map(|(natural_align, build)| parse_mem_arg(natural_align).map(build))
}

fn parse_memory_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("memory.size")
        .and_keep(parse_mem_name())
        .map(|mem_name| Instr::MemorySize { mem_name }))
    .or(parse_literal("memory.grow")
        .and_keep(parse_mem_name())
        .map(|mem_name| Instr::MemoryGrow { mem_name }))
    .or(parse_literal("memory.fill")
        .and_keep(parse_mem_name())
        .map(|mem_name| Instr::MemoryFill { mem_name }))
    .or(parse_literal("memory.copy")
        .and_keep(parse_mem_name())
        .and(parse_mem_name())
        .map(|(target_name, source_name)| Instr::MemoryCopy {
            target_name,
            source_name,
        }))
    .or(parse_literal("memory.init")
        .and_keep(parse_mem_name())
        .and(parse_data_name())
        .map(|(mem_name, data_name)| Instr::MemoryInit {
            mem_name,
            data_name,
        }))
    .or(parse_literal("data.drop")
        .and_keep(parse_data_name())
        .map(|data_name| Instr::DataDrop { data_name }))
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

/// Any operand-less instruction: one delimiter-bounded token, looked up in the mnemonic table beside `Instr`. Whole-token equality means no mnemonic can prefix-shadow another, unlike the literal probes below, whose order matters. The `catch` keeps a miss recoverable — the token was consumed before the lookup could reject it, and the operand-carrying alternatives still deserve their probe.
fn parse_plain_instr<'a>() -> Parser<'a, Instr> {
    catch(
        take_while(|char| !is_delimiter(char))
            .flat_map(|token| match Instr::from_mnemonic(token) {
                Some(instr) => pure(instr),
                None => fail(format!("Expected 'instruction', obtained '{token}'")),
            })
            .and_drop(parse_whitespace()),
    )
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
    .or(parse_literal("return_call_indirect")
        .and_keep(parse_table_name())
        .and(parse_type_name())
        .map(|(table_name, type_name)| Instr::ReturnCallIndirect {
            table_name,
            type_name,
        }))
    .or(parse_literal("return_call_ref")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ReturnCallRef { type_name }))
    .or(parse_literal("return_call")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::ReturnCall { func_name }))
    .or(parse_literal("call_indirect")
        .and_keep(parse_table_name())
        .and(parse_type_name())
        .map(|(table_name, type_name)| Instr::CallIndirect {
            table_name,
            type_name,
        }))
    .or(parse_literal("call_ref")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::CallRef { type_name }))
    .or(parse_literal("call")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::Call { func_name }))
}

fn parse_table_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("table.get")
        .and_keep(parse_table_name())
        .map(|table_name| Instr::TableGet { table_name }))
    .or(parse_literal("table.set")
        .and_keep(parse_table_name())
        .map(|table_name| Instr::TableSet { table_name }))
    .or(parse_literal("table.size")
        .and_keep(parse_table_name())
        .map(|table_name| Instr::TableSize { table_name }))
    .or(parse_literal("table.grow")
        .and_keep(parse_table_name())
        .map(|table_name| Instr::TableGrow { table_name }))
    .or(parse_literal("table.fill")
        .and_keep(parse_table_name())
        .map(|table_name| Instr::TableFill { table_name }))
    .or(parse_literal("table.copy")
        .and_keep(parse_table_name())
        .and(parse_table_name())
        .map(|(target_name, source_name)| Instr::TableCopy {
            target_name,
            source_name,
        }))
    .or(parse_literal("table.init")
        .and_keep(parse_table_name())
        .and(parse_elem_name())
        .map(|(table_name, elem_name)| Instr::TableInit {
            table_name,
            elem_name,
        }))
    .or(parse_literal("elem.drop")
        .and_keep(parse_elem_name())
        .map(|elem_name| Instr::ElemDrop { elem_name }))
}

fn parse_reference_instr<'a>() -> Parser<'a, Instr> {
    (parse_literal("ref.null")
        .and_keep(parse_heap_type())
        .map(|heap_type| Instr::RefNull { heap_type }))
    .or(parse_literal("ref.func")
        .and_keep(parse_func_name())
        .map(|func_name| Instr::RefFunc { func_name }))
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
    .or(parse_literal("array.new_elem")
        .and_keep(parse_type_name())
        .and(parse_elem_name())
        .map(|(type_name, elem_name)| Instr::ArrayNewElem {
            type_name,
            elem_name,
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
    .or(parse_literal("array.fill")
        .and_keep(parse_type_name())
        .map(|type_name| Instr::ArrayFill { type_name }))
    .or(parse_literal("array.copy")
        .and_keep(parse_type_name())
        .and(parse_type_name())
        .map(|(target_name, source_name)| Instr::ArrayCopy {
            target_name,
            source_name,
        }))
    .or(parse_literal("array.init_data")
        .and_keep(parse_type_name())
        .and(parse_data_name())
        .map(|(type_name, data_name)| Instr::ArrayInitData {
            type_name,
            data_name,
        }))
    .or(parse_literal("array.init_elem")
        .and_keep(parse_type_name())
        .and(parse_elem_name())
        .map(|(type_name, elem_name)| Instr::ArrayInitElem {
            type_name,
            elem_name,
        }))
}

fn parse_select_result<'a>() -> Parser<'a, Vec<ValType>> {
    (parse_result_type("result").map(|ResultType { val_types }| val_types)).or(pure(vec![]))
}

fn parse_select_instr<'a>() -> Parser<'a, Instr> {
    parse_literal("select")
        .and_keep(parse_select_result())
        .map(|val_types| Instr::Select { val_types })
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

fn parse_const_instr<'a>() -> Parser<'a, Instr> {
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
}

fn parse_instr<'a>() -> Parser<'a, Instr> {
    parse_plain_instr()
        .or(parse_const_instr())
        .or(parse_reference_instr())
        .or(parse_aggregate_instr())
        .or(parse_mem_access_instr())
        .or(parse_memory_instr())
        .or(parse_table_instr())
        .or(parse_select_instr())
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

fn parse_table_import_desc<'a>() -> Parser<'a, Import> {
    catch(parse_literal("(").and_drop(parse_literal("table")))
        .and_keep(parse_table_name())
        .and(parse_table_type())
        .and_drop(parse_literal(")"))
        .map(|(table_name, table_type)| Import::Table {
            table_name,
            table_type,
        })
}

fn parse_memory_import_desc<'a>() -> Parser<'a, Import> {
    catch(parse_literal("(").and_drop(parse_literal("memory")))
        .and_keep(parse_mem_name())
        .and(parse_mem_type())
        .and_drop(parse_literal(")"))
        .map(|(mem_name, mem_type)| Import::Memory { mem_name, mem_type })
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
        .and(
            (parse_func_import_desc())
                .or(parse_table_import_desc())
                .or(parse_memory_import_desc())
                .or(parse_global_import_desc()),
        )
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

fn parse_table<'a>() -> Parser<'a, (TableName, Table)> {
    catch(parse_literal("(").and_drop(parse_literal("table")))
        .and_keep(parse_table_name())
        .and(parse_table_type())
        .and(parse_expr())
        .and_drop(parse_literal(")"))
        .map(|((table_name, table_type), expr)| {
            (
                table_name,
                Table {
                    table_type,
                    expr: (!expr.instrs.is_empty()).then_some(expr),
                },
            )
        })
}

/// A parenthesized constant expression in an operand position — `(offset …)` or `(item …)`, holding a flat instruction sequence like every other body.
fn parse_const_expr<'a>(keyword: &'static str) -> Parser<'a, Expr> {
    catch(parse_literal("(").and_drop(parse_literal(keyword)))
        .and_keep(parse_expr())
        .and_drop(parse_literal(")"))
}

fn parse_elem_mode<'a>() -> Parser<'a, ElemMode> {
    (parse_literal("passive").map(|()| ElemMode::Passive))
        .or(parse_literal("declare").map(|()| ElemMode::Declarative))
        .or(catch(parse_literal("(").and_drop(parse_literal("table")))
            .and_keep(parse_table_name())
            .and_drop(parse_literal(")"))
            .and(parse_const_expr("offset"))
            .map(|(table_name, offset)| ElemMode::Active { table_name, offset }))
}

fn parse_elem_list<'a>() -> Parser<'a, ElemList> {
    (parse_literal("func")
        .and_keep(many0(parse_func_name))
        .map(ElemList::Funcs))
    .or(parse_ref_type()
        .and(many0(|| parse_const_expr("item")))
        .map(|(ref_type, exprs)| ElemList::Exprs(ref_type, exprs)))
}

fn parse_elem_segment<'a>() -> Parser<'a, (ElemName, ElemSegment)> {
    catch(parse_literal("(").and_drop(parse_literal("elem")))
        .and_keep(parse_elem_name())
        .and(parse_elem_mode())
        .and(parse_elem_list())
        .and_drop(parse_literal(")"))
        .map(|((elem_name, mode), list)| (elem_name, ElemSegment { mode, list }))
}

fn parse_memory<'a>() -> Parser<'a, (MemName, MemType)> {
    catch(parse_literal("(").and_drop(parse_literal("memory")))
        .and_keep(parse_mem_name())
        .and(parse_mem_type())
        .and_drop(parse_literal(")"))
}

fn parse_data_mode<'a>() -> Parser<'a, DataMode> {
    (parse_literal("passive").map(|()| DataMode::Passive)).or(catch(
        parse_literal("(").and_drop(parse_literal("memory")),
    )
    .and_keep(parse_mem_name())
    .and_drop(parse_literal(")"))
    .and(parse_const_expr("offset"))
    .map(|(mem_name, offset)| DataMode::Active { mem_name, offset }))
}

fn parse_data_segment<'a>() -> Parser<'a, (DataName, DataSegment)> {
    catch(parse_literal("(").and_drop(parse_literal("data")))
        .and_keep(parse_data_name())
        .and(parse_data_mode())
        .and(parse_bytes())
        .and_drop(parse_literal(")"))
        .map(|((data_name, mode), bytes)| (data_name, DataSegment { mode, bytes }))
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

fn parse_table_export_desc<'a>() -> Parser<'a, Export> {
    catch(parse_literal("(").and_drop(parse_literal("table")))
        .and_keep(parse_table_name())
        .and_drop(parse_literal(")"))
        .map(Export::Table)
}

fn parse_memory_export_desc<'a>() -> Parser<'a, Export> {
    catch(parse_literal("(").and_drop(parse_literal("memory")))
        .and_keep(parse_mem_name())
        .and_drop(parse_literal(")"))
        .map(Export::Memory)
}

fn parse_export<'a>() -> Parser<'a, (String, Export)> {
    catch(parse_literal("(").and_drop(parse_literal("export")))
        .and_keep(parse_string().map(str::to_string))
        .and(
            (parse_func_export_desc())
                .or(parse_table_export_desc())
                .or(parse_global_export_desc())
                .or(parse_memory_export_desc()),
        )
        .and_drop(parse_literal(")"))
}

fn parse_start<'a>() -> Parser<'a, FuncName> {
    catch(parse_literal("(").and_drop(parse_literal("start")))
        .and_keep(parse_func_name())
        .and_drop(parse_literal(")"))
}

enum ModuleItem {
    RecType(RecType),
    Import(String, String, Import),
    Func(FuncName, Func),
    Table(TableName, Table),
    Memory(MemName, MemType),
    Global(GlobalName, Global),
    ElemSegment(ElemName, ElemSegment),
    DataSegment(DataName, DataSegment),
    Export(String, Export),
    Start(FuncName),
}

fn parse_module_item<'a>() -> Parser<'a, ModuleItem> {
    parse_rec_type()
        .map(ModuleItem::RecType)
        .or(parse_import()
            .map(|(module_name, name, import)| ModuleItem::Import(module_name, name, import)))
        .or(parse_func().map(|(func_name, func)| ModuleItem::Func(func_name, func)))
        .or(parse_table().map(|(table_name, table)| ModuleItem::Table(table_name, table)))
        .or(parse_memory().map(|(mem_name, mem_type)| ModuleItem::Memory(mem_name, mem_type)))
        .or(parse_global().map(|(global_name, global)| ModuleItem::Global(global_name, global)))
        .or(parse_elem_segment()
            .map(|(elem_name, elem_segment)| ModuleItem::ElemSegment(elem_name, elem_segment)))
        .or(parse_data_segment()
            .map(|(data_name, data_segment)| ModuleItem::DataSegment(data_name, data_segment)))
        .or(parse_export().map(|(name, export)| ModuleItem::Export(name, export)))
        .or(parse_start().map(ModuleItem::Start))
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
                    ModuleItem::Table(table_name, table) => module.add_table(table_name, table),
                    ModuleItem::Memory(mem_name, mem_type) => module.add_memory(mem_name, mem_type),
                    ModuleItem::Global(global_name, global) => {
                        module.add_global(global_name, global)
                    }
                    ModuleItem::ElemSegment(elem_name, elem_segment) => {
                        module.add_elem(elem_name, elem_segment)
                    }
                    ModuleItem::DataSegment(data_name, data_segment) => {
                        module.add_data(data_name, data_segment)
                    }
                    ModuleItem::Export(name, export) => module.add_export(name, export),
                    ModuleItem::Start(func_name) => module.set_start(func_name),
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
