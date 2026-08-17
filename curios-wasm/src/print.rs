use {
    super::{
        AbsHeapType, AddressType, ArrayType, BlockType, CompType, DataMode, DataName, DataSegment,
        ElemList, ElemMode, ElemName, ElemSegment, Export, Expr, FieldName, FieldType, Func,
        FuncName, FuncType, Global, GlobalName, GlobalType, HeapType, Import, Instr, LabelName,
        Limits, LocalName, MemAccess, MemName, MemType, Module, Mutability, NumType, PackedType,
        RecType, RefType, ResultType, StorageType, StructType, SubType, Table, TableName,
        TableType, TypeName, ValType,
    },
    curios_print::{Printer, flat, group, hard_line, indent, line, pure, sep_flat},
};

fn print_dollar_ident(name: &str) -> Printer {
    flat([pure("$"), pure(name)])
}

fn print_type_name(type_name: &TypeName) -> Printer {
    print_dollar_ident(type_name.as_str())
}

fn print_field_name(field_name: &FieldName) -> Printer {
    print_dollar_ident(field_name.as_str())
}

fn print_func_name(func_name: &FuncName) -> Printer {
    print_dollar_ident(func_name.as_str())
}

fn print_global_name(global_name: &GlobalName) -> Printer {
    print_dollar_ident(global_name.as_str())
}

fn print_local_name(local_name: &LocalName) -> Printer {
    print_dollar_ident(local_name.as_str())
}

fn print_label_name(label_name: &LabelName) -> Printer {
    print_dollar_ident(label_name.as_str())
}

fn print_table_name(table_name: &TableName) -> Printer {
    print_dollar_ident(table_name.as_str())
}

fn print_elem_name(elem_name: &ElemName) -> Printer {
    print_dollar_ident(elem_name.as_str())
}

fn print_mem_name(mem_name: &MemName) -> Printer {
    print_dollar_ident(mem_name.as_str())
}

fn print_data_name(data_name: &DataName) -> Printer {
    print_dollar_ident(data_name.as_str())
}

fn print_quoted_ident(string: &str) -> Printer {
    flat([pure("\""), pure(string), pure("\"")])
}

fn print_num_type(num_type: &NumType) -> Printer {
    pure(match num_type {
        NumType::I32 => "i32",
        NumType::I64 => "i64",
        NumType::F32 => "f32",
        NumType::F64 => "f64",
    })
}

fn print_abs_heap_type(abs_heap_type: &AbsHeapType) -> Printer {
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

fn print_heap_type(heap_type: &HeapType) -> Printer {
    match heap_type {
        HeapType::Abstract(abs_heap_type) => print_abs_heap_type(abs_heap_type),
        HeapType::Concrete(type_name) => print_type_name(type_name),
    }
}

fn print_ref_type(ref_type: &RefType) -> Printer {
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

fn print_val_type(val_type: &ValType) -> Printer {
    match val_type {
        ValType::Num(num_type) => print_num_type(num_type),
        ValType::Ref(ref_type) => print_ref_type(ref_type),
    }
}

/// A `(param …)`/`(result …)` list as a group: flat when it fits, one value type per line when a wide signature overflows the width.
fn print_result_type<'a>(keyword: &'a str, result_type: &'a ResultType) -> Printer {
    group(flat([
        pure("("),
        pure(keyword),
        indent(flat(
            result_type
                .val_types
                .iter()
                .map(|val_type| flat([line(), print_val_type(val_type)])),
        )),
        pure(")"),
    ]))
}

fn print_packed_type(packed_type: &PackedType) -> Printer {
    pure(match packed_type {
        PackedType::I8 => "i8",
        PackedType::I16 => "i16",
    })
}

fn print_storage_type(storage_type: &StorageType) -> Printer {
    match storage_type {
        StorageType::Val(val_type) => print_val_type(val_type),
        StorageType::Packed(packed_type) => print_packed_type(packed_type),
    }
}

fn print_field_type(field_type: &FieldType) -> Printer {
    match field_type.mutability {
        Mutability::Const => print_storage_type(&field_type.storage_type),
        Mutability::Var => flat([
            pure("(mut "),
            print_storage_type(&field_type.storage_type),
            pure(")"),
        ]),
    }
}

fn print_array_type(array_type: &ArrayType) -> Printer {
    flat([
        pure("(array "),
        print_field_type(&array_type.field_type),
        pure(")"),
    ])
}

/// A struct type as a group: a small field list shares the type's line, a wide one breaks one field per line.
fn print_struct_type(struct_type: &StructType) -> Printer {
    group(flat([
        pure("(struct"),
        indent(flat(struct_type.fields.iter().map(
            |(field_name, field_type)| {
                flat([
                    line(),
                    pure("(field "),
                    print_field_name(field_name),
                    pure(" "),
                    print_field_type(field_type),
                    pure(")"),
                ])
            },
        ))),
        pure(")"),
    ]))
}

fn print_func_type(func_type: &FuncType) -> Printer {
    flat([
        pure("(func"),
        flat(match func_type.inputs.val_types.is_empty() {
            true => None,
            false => Some(flat([
                pure(" "),
                print_result_type("param", &func_type.inputs),
            ])),
        }),
        flat(match func_type.outputs.val_types.is_empty() {
            true => None,
            false => Some(flat([
                pure(" "),
                print_result_type("result", &func_type.outputs),
            ])),
        }),
        pure(")"),
    ])
}

fn print_comp_type(comp_type: &CompType) -> Printer {
    match comp_type {
        CompType::Func(func_type) => print_func_type(func_type),
        CompType::Array(array_type) => print_array_type(array_type),
        CompType::Struct(struct_type) => print_struct_type(struct_type),
    }
}

fn print_sub_type<'a>(type_name: &'a TypeName, sub_type: &'a SubType) -> Printer {
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

fn print_rec_type(rec_type: &RecType) -> Printer {
    if let [(type_name, sub_type)] = &rec_type.sub_types[..] {
        print_sub_type(type_name, sub_type)
    } else {
        // A genuine recursion group as a group: small families share the `rec` line, wide ones break one member per line.
        group(flat([
            pure("(rec"),
            indent(flat(rec_type.sub_types.iter().map(
                |(type_name, sub_type)| flat([line(), print_sub_type(type_name, sub_type)]),
            ))),
            pure(")"),
        ]))
    }
}

fn print_global_type(global_type: &GlobalType) -> Printer {
    match global_type.mutability {
        Mutability::Const => print_val_type(&global_type.val_type),
        Mutability::Var => flat([
            pure("(mut "),
            print_val_type(&global_type.val_type),
            pure(")"),
        ]),
    }
}

/// The address type is always spelled, rather than left implicit at `i32` the way the binary format's flag bit is: a resizable item's text form then says its own address width, and the parser needs no default to reconstruct.
fn print_address_type(address_type: &AddressType) -> Printer {
    pure(match address_type {
        AddressType::I32 => "i32",
        AddressType::I64 => "i64",
    })
}

fn print_limits(limits: &Limits) -> Printer {
    flat([
        pure(limits.min.to_string()),
        flat(
            limits
                .max
                .map(|max| flat([pure(" "), pure(max.to_string())])),
        ),
    ])
}

fn print_table_type(table_type: &TableType) -> Printer {
    flat([
        print_address_type(&table_type.address_type),
        pure(" "),
        print_limits(&table_type.limits),
        pure(" "),
        print_ref_type(&table_type.ref_type),
    ])
}

fn print_mem_type(mem_type: &MemType) -> Printer {
    flat([
        print_address_type(&mem_type.address_type),
        pure(" "),
        print_limits(&mem_type.limits),
    ])
}

/// A load or store: its spelling, then the memory it reaches, then only the parts of its immediate that are not the default — an offset of zero and the access width's natural alignment are omitted, the way `wat` writes them. The memory is *always* spelled, unlike the binary encoding's implicit index 0: the parser sees module items in written order and would have no first memory to default to yet.
fn print_mem_access(access: MemAccess<'_>) -> Printer {
    flat([
        pure(access.mnemonic),
        pure(" "),
        print_mem_name(&access.mem_arg.mem_name),
        flat(match access.mem_arg.offset {
            0 => None,
            offset => Some(pure(format!(" offset={offset}"))),
        }),
        flat(match access.mem_arg.align == access.natural_align {
            true => None,
            false => Some(pure(format!(" align={}", 1_u64 << access.mem_arg.align))),
        }),
    ])
}

fn print_block_type(block_type: &BlockType) -> Printer {
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

fn print_instr(instr: &Instr) -> Printer {
    match instr {
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
        // The label list as a group: a wide table breaks one label per line instead of running off the dump's edge.
        Instr::BrTable {
            label_names,
            label_name,
        } => group(flat([
            pure("br_table"),
            indent(flat(label_names.iter().chain([label_name]).map(
                |label_name| flat([line(), print_label_name(label_name)]),
            ))),
        ])),
        Instr::Call { func_name } => flat([pure("call "), print_func_name(func_name)]),
        Instr::CallRef { type_name } => flat([pure("call_ref "), print_type_name(type_name)]),
        Instr::CallIndirect {
            table_name,
            type_name,
        } => flat([
            pure("call_indirect "),
            print_table_name(table_name),
            pure(" "),
            print_type_name(type_name),
        ]),
        Instr::ReturnCall { func_name } => flat([pure("return_call "), print_func_name(func_name)]),
        Instr::ReturnCallRef { type_name } => {
            flat([pure("return_call_ref "), print_type_name(type_name)])
        }
        Instr::ReturnCallIndirect {
            table_name,
            type_name,
        } => flat([
            pure("return_call_indirect "),
            print_table_name(table_name),
            pure(" "),
            print_type_name(type_name),
        ]),
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
        Instr::RefFunc { func_name } => flat([pure("ref.func "), print_func_name(func_name)]),
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
        Instr::ArrayNewData {
            type_name,
            data_name,
        } => flat([
            pure("array.new_data "),
            print_type_name(type_name),
            pure(" "),
            print_data_name(data_name),
        ]),
        Instr::ArrayInitData {
            type_name,
            data_name,
        } => flat([
            pure("array.init_data "),
            print_type_name(type_name),
            pure(" "),
            print_data_name(data_name),
        ]),
        Instr::ArrayNewElem {
            type_name,
            elem_name,
        } => flat([
            pure("array.new_elem "),
            print_type_name(type_name),
            pure(" "),
            print_elem_name(elem_name),
        ]),
        Instr::ArrayInitElem {
            type_name,
            elem_name,
        } => flat([
            pure("array.init_elem "),
            print_type_name(type_name),
            pure(" "),
            print_elem_name(elem_name),
        ]),
        Instr::ArrayGet { type_name } => flat([pure("array.get "), print_type_name(type_name)]),
        Instr::ArrayGetS { type_name } => flat([pure("array.get_s "), print_type_name(type_name)]),
        Instr::ArrayGetU { type_name } => flat([pure("array.get_u "), print_type_name(type_name)]),
        Instr::ArraySet { type_name } => flat([pure("array.set "), print_type_name(type_name)]),
        Instr::ArrayFill { type_name } => flat([pure("array.fill "), print_type_name(type_name)]),
        Instr::ArrayCopy {
            target_name,
            source_name,
        } => flat([
            pure("array.copy "),
            print_type_name(target_name),
            pure(" "),
            print_type_name(source_name),
        ]),
        Instr::MemorySize { mem_name } => flat([pure("memory.size "), print_mem_name(mem_name)]),
        Instr::MemoryGrow { mem_name } => flat([pure("memory.grow "), print_mem_name(mem_name)]),
        Instr::MemoryFill { mem_name } => flat([pure("memory.fill "), print_mem_name(mem_name)]),
        Instr::MemoryCopy {
            target_name,
            source_name,
        } => flat([
            pure("memory.copy "),
            print_mem_name(target_name),
            pure(" "),
            print_mem_name(source_name),
        ]),
        Instr::MemoryInit {
            mem_name,
            data_name,
        } => flat([
            pure("memory.init "),
            print_mem_name(mem_name),
            pure(" "),
            print_data_name(data_name),
        ]),
        Instr::DataDrop { data_name } => flat([pure("data.drop "), print_data_name(data_name)]),
        Instr::TableGet { table_name } => flat([pure("table.get "), print_table_name(table_name)]),
        Instr::TableSet { table_name } => flat([pure("table.set "), print_table_name(table_name)]),
        Instr::TableSize { table_name } => {
            flat([pure("table.size "), print_table_name(table_name)])
        }
        Instr::TableGrow { table_name } => {
            flat([pure("table.grow "), print_table_name(table_name)])
        }
        Instr::TableFill { table_name } => {
            flat([pure("table.fill "), print_table_name(table_name)])
        }
        Instr::TableCopy {
            target_name,
            source_name,
        } => flat([
            pure("table.copy "),
            print_table_name(target_name),
            pure(" "),
            print_table_name(source_name),
        ]),
        Instr::TableInit {
            table_name,
            elem_name,
        } => flat([
            pure("table.init "),
            print_table_name(table_name),
            pure(" "),
            print_elem_name(elem_name),
        ]),
        Instr::ElemDrop { elem_name } => flat([pure("elem.drop "), print_elem_name(elem_name)]),
        Instr::RefTest { ref_type } => flat([pure("ref.test "), print_ref_type(ref_type)]),
        Instr::RefCast { ref_type } => flat([pure("ref.cast "), print_ref_type(ref_type)]),
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
        // Everything else is table-driven beside `Instr`: a load or store from the memory-access table, and otherwise an operand-less instruction from the mnemonic table. A variant in neither place is a bug in those tables.
        other => match other.mem_access() {
            Some(access) => print_mem_access(access),
            None => pure(other.mnemonic().unwrap_or_else(|| {
                panic!("instruction missing from the mnemonic table: {other:?}")
            })),
        },
    }
}

// Hard breaks, not literal newlines: a group holding a multi-instruction sequence (a global's initializer) must refuse to flatten rather than ride its first instruction.
fn print_instrs(instrs: &[Instr]) -> Printer {
    sep_flat(instrs.iter().map(print_instr), hard_line)
}

fn print_expr(expr: &Expr) -> Printer {
    print_instrs(&expr.instrs)
}

fn print_import<'a>(module_name: &'a str, name: &'a str, import: &'a Import) -> Printer {
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
            Import::Table {
                table_name,
                table_type,
            } => flat([
                pure(" (table "),
                print_table_name(table_name),
                pure(" "),
                print_table_type(table_type),
                pure(")"),
            ]),
            Import::Memory { mem_name, mem_type } => flat([
                pure(" (memory "),
                print_mem_name(mem_name),
                pure(" "),
                print_mem_type(mem_type),
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

/// Prints a `(param $name type)`/`(local $name type)` run: the first binding leads with a newline (so it starts its own line under the enclosing `indent`), and the run is one group — every later binding shares that line while the run fits, and each takes its own line when it does not — the one layout `param`s and `local`s both use in `print_func`. Behavior-neutral on the unbounded `Display` path; [`Module::display_within`](super::Module::display_within) is where the width bites.
fn print_bindings<'a>(
    keyword: &'static str,
    mut bindings: impl Iterator<Item = (&'a LocalName, &'a ValType)> + 'a,
) -> Printer {
    let binding = move |(local_name, val_type): (&'a LocalName, &'a ValType)| {
        flat([
            pure(format!("({keyword} ")),
            print_local_name(local_name),
            pure(" "),
            print_val_type(val_type),
            pure(")"),
        ])
    };
    match bindings.next() {
        None => flat([]),
        Some(first) => flat([
            pure("\n"),
            group(sep_flat(
                std::iter::once(first).chain(bindings).map(binding),
                line,
            )),
        ]),
    }
}

fn print_func<'a>(module: &'a Module, func_name: &'a FuncName, func: &'a Func) -> Printer {
    let func_type = module
        .get_type(&func.type_name)
        .and_then(|sub_type| sub_type.func_type())
        .unwrap_or_else(|| {
            panic!(
                "Unexpected error while getting func type `{}`",
                func_name.as_str()
            )
        });

    flat([
        pure("(func "),
        print_func_name(func_name),
        pure(" (type "),
        print_type_name(&func.type_name),
        pure(")"),
        indent(flat([
            print_bindings("param", func.params.iter().zip(func_type.inputs())),
            flat(match func_type.outputs.val_types.is_empty() {
                true => None,
                false => Some(flat([
                    pure(" "),
                    print_result_type("result", &func_type.outputs),
                ])),
            }),
            print_bindings(
                "local",
                func.locals
                    .iter()
                    .map(|(local_name, val_type)| (local_name, val_type)),
            ),
            pure("\n"),
            print_expr(&func.expr),
        ])),
        pure(")"),
    ])
}

/// A global as a group: a constant-sized initializer shares the declaration's line, a wide one breaks onto the next.
fn print_global<'a>(global_name: &'a GlobalName, global: &'a Global) -> Printer {
    group(flat([
        pure("(global "),
        print_global_name(global_name),
        pure(" "),
        print_global_type(&global.global_type),
        indent(flat([line(), print_expr(&global.expr)])),
        pure(")"),
    ]))
}

/// A table as a group: the initializer, when there is one, shares the declaration's line while it fits and breaks onto the next when it does not — the layout [`print_global`] uses for the same shape.
fn print_table<'a>(table_name: &'a TableName, table: &'a Table) -> Printer {
    group(flat([
        pure("(table "),
        print_table_name(table_name),
        pure(" "),
        print_table_type(&table.table_type),
        indent(flat(
            table
                .expr
                .as_ref()
                .map(|expr| flat([line(), print_expr(expr)])),
        )),
        pure(")"),
    ]))
}

/// A constant expression in an operand position — a segment's offset, or one element of an expression list — parenthesized under its keyword so the flat instruction sequence inside has an end the parser can find.
fn print_const_expr(keyword: &'static str, expr: &Expr) -> Printer {
    flat([
        pure("("),
        pure(keyword),
        pure(" "),
        print_expr(expr),
        pure(")"),
    ])
}

/// An element segment as a group: the elements share the segment's line while they fit and break one per line when they do not, the layout `br_table` uses for its labels.
fn print_elem_segment<'a>(elem_name: &'a ElemName, segment: &'a ElemSegment) -> Printer {
    group(flat([
        pure("(elem "),
        print_elem_name(elem_name),
        pure(" "),
        match &segment.mode {
            ElemMode::Passive => pure("passive"),
            ElemMode::Declarative => pure("declare"),
            ElemMode::Active { table_name, offset } => flat([
                pure("(table "),
                print_table_name(table_name),
                pure(") "),
                print_const_expr("offset", offset),
            ]),
        },
        match &segment.list {
            ElemList::Funcs(func_names) => flat([
                pure(" func"),
                indent(flat(
                    func_names
                        .iter()
                        .map(|func_name| flat([line(), print_func_name(func_name)])),
                )),
            ]),
            ElemList::Exprs(ref_type, exprs) => flat([
                pure(" "),
                print_ref_type(ref_type),
                indent(flat(
                    exprs
                        .iter()
                        .map(|expr| flat([line(), print_const_expr("item", expr)])),
                )),
            ]),
        },
        pure(")"),
    ]))
}

fn print_memory<'a>(mem_name: &'a MemName, mem_type: &'a MemType) -> Printer {
    flat([
        pure("(memory "),
        print_mem_name(mem_name),
        pure(" "),
        print_mem_type(mem_type),
        pure(")"),
    ])
}

fn print_data_segment<'a>(name: &'a DataName, segment: &'a DataSegment) -> Printer {
    let encoded: String = segment
        .bytes
        .iter()
        .map(|b| format!("\\{:02x}", b))
        .collect();
    flat([
        pure("(data "),
        print_data_name(name),
        pure(" "),
        match &segment.mode {
            DataMode::Passive => pure("passive"),
            DataMode::Active { mem_name, offset } => flat([
                pure("(memory "),
                print_mem_name(mem_name),
                pure(") "),
                print_const_expr("offset", offset),
            ]),
        },
        pure(" \""),
        pure(encoded),
        pure("\")"),
    ])
}

fn print_export<'a>(name: &'a str, export: &'a Export) -> Printer {
    flat([
        pure("(export "),
        print_quoted_ident(name),
        match export {
            Export::Func(func_name) => {
                flat([pure(" (func "), print_func_name(func_name), pure(")")])
            }
            Export::Table(table_name) => {
                flat([pure(" (table "), print_table_name(table_name), pure(")")])
            }
            Export::Memory(mem_name) => {
                flat([pure(" (memory "), print_mem_name(mem_name), pure(")")])
            }
            Export::Global(global_name) => {
                flat([pure(" (global "), print_global_name(global_name), pure(")")])
            }
        },
        pure(")"),
    ])
}

pub(crate) fn print_module(module: &Module) -> Printer {
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
                module
                    .tables()
                    .iter()
                    .map(|(table_name, table)| flat([pure("\n"), print_table(table_name, table)])),
            )
            .chain(
                module.mems().iter().map(|(mem_name, mem_type)| {
                    flat([pure("\n"), print_memory(mem_name, mem_type)])
                }),
            )
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
                    .datas()
                    .iter()
                    .map(|(name, segment)| flat([pure("\n"), print_data_segment(name, segment)])),
            )
            .chain(
                module
                    .exports()
                    .iter()
                    .map(|(name, export)| flat([pure("\n"), print_export(name, export)])),
            )
            .chain(
                module
                    .start()
                    .map(|start| flat([pure("\n(start "), print_func_name(start), pure(")")])),
            )
            .chain(module.elems().iter().map(|(elem_name, segment)| {
                flat([pure("\n"), print_elem_segment(elem_name, segment)])
            })),
        )),
        pure(")"),
    ])
}
