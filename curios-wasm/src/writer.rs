mod buffer;
use buffer::*;

mod encoding;
use encoding::*;

mod state;
use state::*;

mod indices;
use indices::*;

use {
    super::{
        AbsHeapType, AddressType, ArrayType, BlockType, CompType, DataMode, DataName, DataSegment,
        ElemList, ElemMode, ElemName, ElemSegment, Export, Expr, FieldName, FieldType, Func,
        FuncName, FuncType, Global, GlobalName, GlobalType, HeapType, Import, Instr, LabelName,
        Limits, LocalName, MemArg, MemName, MemType, Module, Mutability, NumType, PackedType,
        RecType, RefType, ResultType, StorageType, StructType, SubType, Table, TableName,
        TableType, TypeName, ValType,
    },
    std::io::{Result, Write},
};

#[derive(Debug)]
struct Writer<'t, 'w, W> {
    indices: &'t Indices<'t>,
    buffer: Buffer<'w, W>,
}

impl<'t, 'w, W> Writer<'t, 'w, W>
where
    W: Write,
{
    fn new(indices: &'t Indices<'t>, writer: &'w mut W) -> Self {
        Self {
            indices,
            buffer: Buffer::new(writer),
        }
    }

    fn fork<'u, U>(&self, writer: &'u mut U) -> Writer<'t, 'u, U>
    where
        U: Write,
    {
        Writer::new(self.indices, writer)
    }

    fn write_vec<'a, A, I, F>(&mut self, i: I, mut f: F) -> Result<()>
    where
        A: 'a,
        I: IntoIterator<Item = &'a A>,
        I::IntoIter: ExactSizeIterator,
        F: FnMut(&mut Self, &'a A) -> Result<()>,
    {
        let items = i.into_iter();

        self.buffer.push_leb128_unsigned(items.len() as u64)?;

        for item in items {
            f(self, item)?;
        }

        Ok(())
    }

    fn write_name(&mut self, name: &str) -> Result<()> {
        self.buffer.push_vec_bytes(&encode_utf8(name))?;

        Ok(())
    }

    fn write_index(&mut self, index: usize) -> Result<()> {
        self.buffer.push_leb128_unsigned(index as u64)
    }

    fn write_type_name(&mut self, type_name: &TypeName) -> Result<()> {
        self.write_index(self.indices.resolve_type(type_name))
    }

    fn write_type_name_signed(&mut self, type_name: &TypeName) -> Result<()> {
        self.buffer
            .push_leb128_signed(self.indices.resolve_type(type_name) as i64)?;

        Ok(())
    }

    fn write_field_name(&mut self, type_name: &TypeName, field_name: &FieldName) -> Result<()> {
        self.write_index(self.indices.resolve_field(type_name, field_name))
    }

    /// Emit the `0xfb` GC-opcode prefix and its sub-opcode.
    fn gc_op(&mut self, sub: u64) -> Result<()> {
        self.buffer.push_byte(0xfb)?;
        self.buffer.push_leb128_unsigned(sub)
    }

    /// Emit the `0xfc` miscellaneous-opcode prefix and its sub-opcode: the saturating truncations (0–7) and the bulk-memory and table families (8–17).
    fn fc_op(&mut self, sub: u64) -> Result<()> {
        self.buffer.push_byte(0xfc)?;
        self.buffer.push_leb128_unsigned(sub)
    }

    /// A GC op with one type operand (`struct.new`, `array.get`, …).
    fn gc_type_op(&mut self, sub: u64, type_name: &TypeName) -> Result<()> {
        self.gc_op(sub)?;
        self.write_type_name(type_name)
    }

    /// A GC op with a struct-field operand (`struct.get`, `struct.set`, …).
    fn gc_field_op(
        &mut self,
        sub: u64,
        type_name: &TypeName,
        field_name: &FieldName,
    ) -> Result<()> {
        self.gc_op(sub)?;
        self.write_type_name(type_name)?;
        self.write_field_name(type_name, field_name)
    }

    fn write_func_name(&mut self, func_name: &FuncName) -> Result<()> {
        self.write_index(self.indices.resolve_func(func_name))
    }

    fn write_local_name(&mut self, func_name: &FuncName, local_name: &LocalName) -> Result<()> {
        self.write_index(self.indices.resolve_local(func_name, local_name))
    }

    fn write_table_name(&mut self, table_name: &TableName) -> Result<()> {
        self.write_index(self.indices.resolve_table(table_name))
    }

    fn write_global_name(&mut self, global_name: &GlobalName) -> Result<()> {
        self.write_index(self.indices.resolve_global(global_name))
    }

    fn write_elem_name(&mut self, elem_name: &ElemName) -> Result<()> {
        self.write_index(self.indices.resolve_elem(elem_name))
    }

    fn write_mem_name(&mut self, mem_name: &MemName) -> Result<()> {
        self.write_index(self.indices.resolve_mem(mem_name))
    }

    fn write_data_name(&mut self, data_name: &DataName) -> Result<()> {
        self.write_index(self.indices.resolve_data(data_name))
    }

    fn write_name_map<'a, I>(&mut self, i: I) -> Result<()>
    where
        I: IntoIterator<Item = (u64, &'a str)>,
        I::IntoIter: ExactSizeIterator,
    {
        let names = i.into_iter();

        self.buffer.push_leb128_unsigned(names.len() as u64)?;

        for (index, name) in names {
            self.buffer.push_leb128_unsigned(index)?;
            self.write_name(name)?;
        }

        Ok(())
    }

    fn write_indirect_name_map<'a, I, J>(&mut self, i: I) -> Result<()>
    where
        I: IntoIterator<Item = (u64, J)>,
        I::IntoIter: ExactSizeIterator,
        J: IntoIterator<Item = (u64, &'a str)>,
        J::IntoIter: ExactSizeIterator,
    {
        let indirect_names = i.into_iter();

        self.buffer
            .push_leb128_unsigned(indirect_names.len() as u64)?;

        for (index, names) in indirect_names {
            self.buffer.push_leb128_unsigned(index)?;
            self.write_name_map(names)?;
        }

        Ok(())
    }

    fn write_section(&mut self, id: u8, bytes: Vec<u8>) -> Result<()> {
        self.buffer.push_leb128_unsigned(id as u64)?;
        self.buffer.push_vec_bytes(&bytes)?;

        Ok(())
    }

    /// Build a section's payload into a forked buffer, then emit it as section `id`.
    fn write_section_with<F>(&mut self, id: u8, build: F) -> Result<()>
    where
        F: for<'u> FnOnce(&mut Writer<'t, 'u, Vec<u8>>) -> Result<()>,
    {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);
            build(&mut writer)?;
        }

        self.write_section(id, bytes)
    }

    fn write_magic(&mut self) -> Result<()> {
        self.buffer.push_bytes(b"\0asm")?;

        Ok(())
    }

    fn write_version(&mut self) -> Result<()> {
        self.buffer.push_bytes(&1_i32.to_le_bytes())?;

        Ok(())
    }

    fn write_number_type(&mut self, num_type: &NumType) -> Result<()> {
        match num_type {
            NumType::I32 => self.buffer.push_byte(0x7f)?,
            NumType::I64 => self.buffer.push_byte(0x7e)?,
            NumType::F32 => self.buffer.push_byte(0x7d)?,
            NumType::F64 => self.buffer.push_byte(0x7c)?,
        }

        Ok(())
    }

    fn write_abs_heap_type(&mut self, abs_heap_type: &AbsHeapType) -> Result<()> {
        match abs_heap_type {
            AbsHeapType::NoFunc => self.buffer.push_byte(0x73)?,
            AbsHeapType::NoExtern => self.buffer.push_byte(0x72)?,
            AbsHeapType::None => self.buffer.push_byte(0x71)?,
            AbsHeapType::Func => self.buffer.push_byte(0x70)?,
            AbsHeapType::Extern => self.buffer.push_byte(0x6f)?,
            AbsHeapType::Any => self.buffer.push_byte(0x6e)?,
            AbsHeapType::Eq => self.buffer.push_byte(0x6d)?,
            AbsHeapType::I31 => self.buffer.push_byte(0x6c)?,
            AbsHeapType::Struct => self.buffer.push_byte(0x6b)?,
            AbsHeapType::Array => self.buffer.push_byte(0x6a)?,
        }

        Ok(())
    }

    fn write_heap_type(&mut self, heap_type: &HeapType) -> Result<()> {
        match heap_type {
            HeapType::Abstract(abs_heap_type) => self.write_abs_heap_type(abs_heap_type)?,
            HeapType::Concrete(name) => self.write_type_name_signed(name)?,
        }

        Ok(())
    }

    fn write_ref_type(&mut self, ref_type: &RefType) -> Result<()> {
        match (ref_type.is_nullable, &ref_type.heap_type) {
            (true, HeapType::Abstract(abs_heap_type)) => {
                self.write_abs_heap_type(abs_heap_type)?;
            }
            (true, HeapType::Concrete(type_name)) => {
                self.buffer.push_byte(0x63)?;
                self.write_type_name_signed(type_name)?;
            }
            (false, HeapType::Abstract(abs_heap_type)) => {
                self.buffer.push_byte(0x64)?;
                self.write_abs_heap_type(abs_heap_type)?;
            }
            (false, HeapType::Concrete(type_name)) => {
                self.buffer.push_byte(0x64)?;
                self.write_type_name_signed(type_name)?;
            }
        }

        Ok(())
    }

    fn write_val_type(&mut self, val_type: &ValType) -> Result<()> {
        match val_type {
            ValType::Num(num_type) => self.write_number_type(num_type)?,
            ValType::Ref(ref_type) => self.write_ref_type(ref_type)?,
        }

        Ok(())
    }

    fn write_result_type(&mut self, result_type: &ResultType) -> Result<()> {
        self.write_vec(&result_type.val_types, |writer, val_type| {
            writer.write_val_type(val_type)?;

            Ok(())
        })?;

        Ok(())
    }

    fn write_packed_type(&mut self, packed_type: &PackedType) -> Result<()> {
        match packed_type {
            PackedType::I8 => self.buffer.push_byte(0x78)?,
            PackedType::I16 => self.buffer.push_byte(0x77)?,
        }

        Ok(())
    }

    fn write_storage_type(&mut self, storage_type: &StorageType) -> Result<()> {
        match storage_type {
            StorageType::Val(val_type) => self.write_val_type(val_type)?,
            StorageType::Packed(packed_type) => self.write_packed_type(packed_type)?,
        }

        Ok(())
    }

    fn write_mutability(&mut self, mutability: &Mutability) -> Result<()> {
        match mutability {
            Mutability::Const => self.buffer.push_byte(0x00)?,
            Mutability::Var => self.buffer.push_byte(0x01)?,
        }

        Ok(())
    }

    fn write_field_type(&mut self, field_type: &FieldType) -> Result<()> {
        self.write_storage_type(&field_type.storage_type)?;
        self.write_mutability(&field_type.mutability)?;

        Ok(())
    }

    fn write_array_type(&mut self, array_type: &ArrayType) -> Result<()> {
        self.write_field_type(&array_type.field_type)?;

        Ok(())
    }

    fn write_struct_type(&mut self, struct_type: &StructType) -> Result<()> {
        self.write_vec(&struct_type.fields, |writer, (_, field_type)| {
            writer.write_field_type(field_type)?;

            Ok(())
        })?;

        Ok(())
    }

    fn write_func_type(&mut self, func_type: &FuncType) -> Result<()> {
        self.write_result_type(&func_type.inputs)?;
        self.write_result_type(&func_type.outputs)?;

        Ok(())
    }

    fn write_comp_type(&mut self, comp_type: &CompType) -> Result<()> {
        match comp_type {
            CompType::Func(func_type) => {
                self.buffer.push_byte(0x60)?;
                self.write_func_type(func_type)?;
            }
            CompType::Array(array_type) => {
                self.buffer.push_byte(0x5e)?;
                self.write_array_type(array_type)?;
            }
            CompType::Struct(struct_type) => {
                self.buffer.push_byte(0x5f)?;
                self.write_struct_type(struct_type)?;
            }
        }

        Ok(())
    }

    fn write_sub_type(&mut self, sub_type: &SubType) -> Result<()> {
        if !sub_type.is_final {
            self.buffer.push_byte(0x50)?;

            self.write_vec(&sub_type.super_types, |writer, type_name| {
                writer.write_type_name(type_name)?;

                Ok(())
            })?;

            self.write_comp_type(&sub_type.comp_type)?;
        } else if !sub_type.super_types.is_empty() {
            self.buffer.push_byte(0x4f)?;

            self.write_vec(&sub_type.super_types, |writer, type_name| {
                writer.write_type_name(type_name)?;

                Ok(())
            })?;

            self.write_comp_type(&sub_type.comp_type)?;
        } else {
            self.write_comp_type(&sub_type.comp_type)?;
        }

        Ok(())
    }

    fn write_rec_type(&mut self, rec_type: &RecType) -> Result<()> {
        if let [(_, sub_type)] = &rec_type.sub_types[..] {
            self.write_sub_type(sub_type)?;
        } else {
            self.buffer.push_byte(0x4e)?;

            self.write_vec(&rec_type.sub_types, |writer, (_, sub_type)| {
                writer.write_sub_type(sub_type)?;

                Ok(())
            })?;
        }

        Ok(())
    }

    fn write_global_type(&mut self, global_type: &GlobalType) -> Result<()> {
        self.write_val_type(&global_type.val_type)?;
        self.write_mutability(&global_type.mutability)?;

        Ok(())
    }

    /// A resizable item's bounds: the flag byte (bit 0 a maximum follows, bit 2 the addresses are 64-bit — bit 1 is the shared-memory flag, outside the envelope), then the minimum and any maximum.
    fn write_limits(&mut self, address_type: &AddressType, limits: &Limits) -> Result<()> {
        let has_max = u8::from(limits.max.is_some());

        let is_64_bit = match address_type {
            AddressType::I32 => 0,
            AddressType::I64 => 0b100,
        };

        self.buffer.push_byte(has_max | is_64_bit)?;
        self.buffer.push_leb128_unsigned(limits.min)?;

        if let Some(max) = limits.max {
            self.buffer.push_leb128_unsigned(max)?;
        }

        Ok(())
    }

    fn write_table_type(&mut self, table_type: &TableType) -> Result<()> {
        self.write_ref_type(&table_type.ref_type)?;
        self.write_limits(&table_type.address_type, &table_type.limits)?;

        Ok(())
    }

    fn write_mem_type(&mut self, mem_type: &MemType) -> Result<()> {
        self.write_limits(&mem_type.address_type, &mem_type.limits)?;

        Ok(())
    }

    /// A load or store's immediate. The alignment field's bit 6 says an explicit memory index follows it, which is how multi-memory extended a field that had no room left; the index is left implicit at memory 0 so a module reaching only its first memory encodes exactly as it did before the proposal.
    fn write_mem_arg(&mut self, mem_arg: &MemArg) -> Result<()> {
        let index = self.indices.resolve_mem(&mem_arg.mem_name);

        match index {
            0 => {
                self.buffer.push_leb128_unsigned(mem_arg.align as u64)?;
            }
            index => {
                self.buffer
                    .push_leb128_unsigned(mem_arg.align as u64 | 0x40)?;

                self.write_index(index)?;
            }
        }

        self.buffer.push_leb128_unsigned(mem_arg.offset)?;

        Ok(())
    }

    /// A load or store: its opcode, then its immediate.
    fn mem_op(&mut self, opcode: u8, mem_arg: &MemArg) -> Result<()> {
        self.buffer.push_byte(opcode)?;
        self.write_mem_arg(mem_arg)
    }

    fn write_import(&mut self, module_name: &str, name: &str, import: &Import) -> Result<()> {
        self.write_name(module_name)?;
        self.write_name(name)?;

        match import {
            Import::Func { type_name, .. } => {
                self.buffer.push_byte(0x00)?;
                self.write_type_name(type_name)?;
            }
            Import::Table { table_type, .. } => {
                self.buffer.push_byte(0x01)?;
                self.write_table_type(table_type)?;
            }
            Import::Memory { mem_type, .. } => {
                self.buffer.push_byte(0x02)?;
                self.write_mem_type(mem_type)?;
            }
            Import::Global { global_type, .. } => {
                self.buffer.push_byte(0x03)?;
                self.write_global_type(global_type)?;
            }
        }

        Ok(())
    }

    fn write_func(&mut self, func: &Func) -> Result<()> {
        self.write_type_name(&func.type_name)?;

        Ok(())
    }

    fn write_block_type(&mut self, block_type: &BlockType) -> Result<()> {
        match block_type {
            BlockType::Empty => self.buffer.push_byte(0x40)?,
            BlockType::Inline(val_type) => self.write_val_type(val_type)?,
            BlockType::Concrete(type_name) => self.write_type_name_signed(type_name)?,
        }

        Ok(())
    }

    fn write_cast_flags(&mut self, source_type: &RefType, target_type: &RefType) -> Result<()> {
        self.buffer
            .push_byte(match (source_type.is_nullable, target_type.is_nullable) {
                (false, false) => 0,
                (true, false) => 1,
                (false, true) => 2,
                (true, true) => 3,
            })?;

        Ok(())
    }

    fn write_instr<'f, 'l>(&mut self, state: &mut State<'f, 'l>, instr: &'l Instr) -> Result<()> {
        match instr {
            Instr::Unreachable => self.buffer.push_byte(0x00)?,
            Instr::Nop => self.buffer.push_byte(0x01)?,
            Instr::Block {
                label_name,
                block_type,
                instructions,
            } => {
                self.buffer.push_byte(0x02)?;
                self.write_block_type(block_type)?;

                state.scoped(label_name, |state| {
                    for instr in instructions {
                        self.write_instr(state, instr)?;
                    }

                    Ok(())
                })?;
                self.buffer.push_byte(0x0b)?;
            }
            Instr::Loop {
                label_name,
                block_type,
                instructions,
            } => {
                self.buffer.push_byte(0x03)?;
                self.write_block_type(block_type)?;

                state.scoped(label_name, |state| {
                    for instr in instructions {
                        self.write_instr(state, instr)?;
                    }

                    Ok(())
                })?;
                self.buffer.push_byte(0x0b)?;
            }
            Instr::If {
                label_name,
                block_type,
                then_instructions,
                else_instructions,
            } => {
                self.buffer.push_byte(0x04)?;
                self.write_block_type(block_type)?;

                state.scoped(label_name, |state| {
                    for instr in then_instructions {
                        self.write_instr(state, instr)?;
                    }

                    Ok(())
                })?;

                if !else_instructions.is_empty() {
                    self.buffer.push_byte(0x05)?;

                    state.scoped(label_name, |state| {
                        for instr in else_instructions {
                            self.write_instr(state, instr)?;
                        }

                        Ok(())
                    })?;
                }

                self.buffer.push_byte(0x0b)?;
            }
            Instr::Br { label_name } => {
                self.buffer.push_byte(0x0c)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;
            }
            Instr::BrIf { label_name } => {
                self.buffer.push_byte(0x0d)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;
            }
            Instr::BrTable {
                label_names,
                label_name,
            } => {
                self.buffer.push_byte(0x0e)?;
                self.buffer.push_leb128_unsigned(label_names.len() as u64)?;

                for label_name in label_names {
                    self.buffer
                        .push_leb128_unsigned(state.resolve(label_name) as u64)?;
                }

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;
            }
            Instr::Return => self.buffer.push_byte(0x0f)?,
            Instr::Call { func_name } => {
                self.buffer.push_byte(0x10)?;
                self.write_func_name(func_name)?;
            }
            Instr::CallRef { type_name } => {
                self.buffer.push_byte(0x14)?;
                self.write_type_name(type_name)?;
            }
            Instr::CallIndirect {
                table_name,
                type_name,
            } => {
                self.buffer.push_byte(0x11)?;
                self.write_type_name(type_name)?;
                self.write_table_name(table_name)?;
            }
            Instr::ReturnCall { func_name } => {
                self.buffer.push_byte(0x12)?;
                self.write_func_name(func_name)?;
            }
            Instr::ReturnCallRef { type_name } => {
                self.buffer.push_byte(0x15)?;
                self.write_type_name(type_name)?;
            }
            Instr::ReturnCallIndirect {
                table_name,
                type_name,
            } => {
                self.buffer.push_byte(0x13)?;
                self.write_type_name(type_name)?;
                self.write_table_name(table_name)?;
            }
            Instr::BrOnNull { label_name } => {
                self.buffer.push_byte(0xd5)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;
            }
            Instr::BrOnNonNull { label_name } => {
                self.buffer.push_byte(0xd6)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;
            }
            Instr::BrOnCast {
                label_name,
                source_type,
                target_type,
            } => {
                self.gc_op(24)?;
                self.write_cast_flags(source_type, target_type)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;

                self.write_heap_type(&source_type.heap_type)?;
                self.write_heap_type(&target_type.heap_type)?;
            }
            Instr::BrOnCastFail {
                label_name,
                source_type,
                target_type,
            } => {
                self.gc_op(25)?;
                self.write_cast_flags(source_type, target_type)?;

                self.buffer
                    .push_leb128_unsigned(state.resolve(label_name) as u64)?;

                self.write_heap_type(&source_type.heap_type)?;
                self.write_heap_type(&target_type.heap_type)?;
            }
            Instr::RefNull { heap_type } => {
                self.buffer.push_byte(0xd0)?;
                self.write_heap_type(heap_type)?;
            }
            Instr::RefIsNull => self.buffer.push_byte(0xd1)?,
            Instr::RefFunc { func_name } => {
                self.buffer.push_byte(0xd2)?;
                self.write_func_name(func_name)?;
            }
            Instr::RefEq => self.buffer.push_byte(0xd3)?,
            Instr::RefAsNonNull => self.buffer.push_byte(0xd4)?,
            Instr::StructNew { type_name } => self.gc_type_op(0, type_name)?,
            Instr::StructNewDefault { type_name } => self.gc_type_op(1, type_name)?,
            Instr::StructGet {
                type_name,
                field_name,
            } => self.gc_field_op(2, type_name, field_name)?,
            Instr::StructGetS {
                type_name,
                field_name,
            } => self.gc_field_op(3, type_name, field_name)?,
            Instr::StructGetU {
                type_name,
                field_name,
            } => self.gc_field_op(4, type_name, field_name)?,
            Instr::StructSet {
                type_name,
                field_name,
            } => self.gc_field_op(5, type_name, field_name)?,
            Instr::ArrayNew { type_name } => self.gc_type_op(6, type_name)?,
            Instr::ArrayNewDefault { type_name } => self.gc_type_op(7, type_name)?,
            Instr::ArrayNewFixed { type_name, length } => {
                self.gc_type_op(8, type_name)?;
                self.buffer.push_leb128_unsigned(*length as u64)?;
            }
            Instr::ArrayNewData {
                type_name,
                data_name,
            } => {
                self.gc_type_op(9, type_name)?;
                self.write_data_name(data_name)?;
            }
            Instr::ArrayNewElem {
                type_name,
                elem_name,
            } => {
                self.gc_type_op(10, type_name)?;
                self.write_elem_name(elem_name)?;
            }
            Instr::ArrayGet { type_name } => self.gc_type_op(11, type_name)?,
            Instr::ArrayGetS { type_name } => self.gc_type_op(12, type_name)?,
            Instr::ArrayGetU { type_name } => self.gc_type_op(13, type_name)?,
            Instr::ArraySet { type_name } => self.gc_type_op(14, type_name)?,
            Instr::ArrayLen => self.gc_op(15)?,
            Instr::ArrayFill { type_name } => self.gc_type_op(16, type_name)?,
            Instr::ArrayCopy {
                source_name,
                target_name,
            } => {
                self.gc_op(17)?;
                self.write_type_name(source_name)?;
                self.write_type_name(target_name)?;
            }
            Instr::ArrayInitData {
                type_name,
                data_name,
            } => {
                self.gc_type_op(18, type_name)?;
                self.write_data_name(data_name)?;
            }
            Instr::ArrayInitElem {
                type_name,
                elem_name,
            } => {
                self.gc_type_op(19, type_name)?;
                self.write_elem_name(elem_name)?;
            }
            Instr::RefTest { ref_type } => {
                self.gc_op(if ref_type.is_nullable { 21 } else { 20 })?;
                self.write_heap_type(&ref_type.heap_type)?;
            }
            Instr::RefCast { ref_type } => {
                self.gc_op(if ref_type.is_nullable { 23 } else { 22 })?;
                self.write_heap_type(&ref_type.heap_type)?;
            }
            Instr::AnyConvertExtern => self.gc_op(26)?,
            Instr::ExternConvertAny => self.gc_op(27)?,
            Instr::RefI31 => self.gc_op(28)?,
            Instr::I31GetS => self.gc_op(29)?,
            Instr::I31GetU => self.gc_op(30)?,
            Instr::I32Load { mem_arg } => self.mem_op(0x28, mem_arg)?,
            Instr::I64Load { mem_arg } => self.mem_op(0x29, mem_arg)?,
            Instr::F32Load { mem_arg } => self.mem_op(0x2a, mem_arg)?,
            Instr::F64Load { mem_arg } => self.mem_op(0x2b, mem_arg)?,
            Instr::I32Load8S { mem_arg } => self.mem_op(0x2c, mem_arg)?,
            Instr::I32Load8U { mem_arg } => self.mem_op(0x2d, mem_arg)?,
            Instr::I32Load16S { mem_arg } => self.mem_op(0x2e, mem_arg)?,
            Instr::I32Load16U { mem_arg } => self.mem_op(0x2f, mem_arg)?,
            Instr::I64Load8S { mem_arg } => self.mem_op(0x30, mem_arg)?,
            Instr::I64Load8U { mem_arg } => self.mem_op(0x31, mem_arg)?,
            Instr::I64Load16S { mem_arg } => self.mem_op(0x32, mem_arg)?,
            Instr::I64Load16U { mem_arg } => self.mem_op(0x33, mem_arg)?,
            Instr::I64Load32S { mem_arg } => self.mem_op(0x34, mem_arg)?,
            Instr::I64Load32U { mem_arg } => self.mem_op(0x35, mem_arg)?,
            Instr::I32Store { mem_arg } => self.mem_op(0x36, mem_arg)?,
            Instr::I64Store { mem_arg } => self.mem_op(0x37, mem_arg)?,
            Instr::F32Store { mem_arg } => self.mem_op(0x38, mem_arg)?,
            Instr::F64Store { mem_arg } => self.mem_op(0x39, mem_arg)?,
            Instr::I32Store8 { mem_arg } => self.mem_op(0x3a, mem_arg)?,
            Instr::I32Store16 { mem_arg } => self.mem_op(0x3b, mem_arg)?,
            Instr::I64Store8 { mem_arg } => self.mem_op(0x3c, mem_arg)?,
            Instr::I64Store16 { mem_arg } => self.mem_op(0x3d, mem_arg)?,
            Instr::I64Store32 { mem_arg } => self.mem_op(0x3e, mem_arg)?,
            Instr::MemorySize { mem_name } => {
                self.buffer.push_byte(0x3f)?;
                self.write_mem_name(mem_name)?;
            }
            Instr::MemoryGrow { mem_name } => {
                self.buffer.push_byte(0x40)?;
                self.write_mem_name(mem_name)?;
            }
            Instr::MemoryFill { mem_name } => {
                self.fc_op(11)?;
                self.write_mem_name(mem_name)?;
            }
            Instr::MemoryCopy {
                target_name,
                source_name,
            } => {
                self.fc_op(10)?;
                self.write_mem_name(target_name)?;
                self.write_mem_name(source_name)?;
            }
            Instr::MemoryInit {
                mem_name,
                data_name,
            } => {
                self.fc_op(8)?;
                self.write_data_name(data_name)?;
                self.write_mem_name(mem_name)?;
            }
            Instr::DataDrop { data_name } => {
                self.fc_op(9)?;
                self.write_data_name(data_name)?;
            }
            Instr::TableGet { table_name } => {
                self.buffer.push_byte(0x25)?;
                self.write_table_name(table_name)?;
            }
            Instr::TableSet { table_name } => {
                self.buffer.push_byte(0x26)?;
                self.write_table_name(table_name)?;
            }
            Instr::TableSize { table_name } => {
                self.fc_op(16)?;
                self.write_table_name(table_name)?;
            }
            Instr::TableGrow { table_name } => {
                self.fc_op(15)?;
                self.write_table_name(table_name)?;
            }
            Instr::TableFill { table_name } => {
                self.fc_op(17)?;
                self.write_table_name(table_name)?;
            }
            Instr::TableCopy {
                target_name,
                source_name,
            } => {
                self.fc_op(14)?;
                self.write_table_name(target_name)?;
                self.write_table_name(source_name)?;
            }
            Instr::TableInit {
                table_name,
                elem_name,
            } => {
                self.fc_op(12)?;
                self.write_elem_name(elem_name)?;
                self.write_table_name(table_name)?;
            }
            Instr::ElemDrop { elem_name } => {
                self.fc_op(13)?;
                self.write_elem_name(elem_name)?;
            }
            Instr::Drop => self.buffer.push_byte(0x1a)?,
            Instr::Select { val_types } => {
                if val_types.is_empty() {
                    self.buffer.push_byte(0x1b)?;
                } else {
                    self.buffer.push_byte(0x1c)?;

                    self.write_vec(val_types, |writer, val_type| {
                        writer.write_val_type(val_type)?;

                        Ok(())
                    })?;
                }
            }
            Instr::LocalGet { local_name } => {
                self.buffer.push_byte(0x20)?;
                self.write_local_name(state.owner(), local_name)?;
            }
            Instr::LocalSet { local_name } => {
                self.buffer.push_byte(0x21)?;
                self.write_local_name(state.owner(), local_name)?;
            }
            Instr::LocalTee { local_name } => {
                self.buffer.push_byte(0x22)?;
                self.write_local_name(state.owner(), local_name)?;
            }
            Instr::GlobalGet { global_name } => {
                self.buffer.push_byte(0x23)?;
                self.write_global_name(global_name)?;
            }
            Instr::GlobalSet { global_name } => {
                self.buffer.push_byte(0x24)?;
                self.write_global_name(global_name)?;
            }
            Instr::I32Const { value } => {
                self.buffer.push_byte(0x41)?;
                self.buffer.push_leb128_signed(*value as i64)?;
            }
            Instr::I64Const { value } => {
                self.buffer.push_byte(0x42)?;
                self.buffer.push_leb128_signed(*value)?;
            }
            Instr::F32Const { value } => {
                self.buffer.push_byte(0x43)?;
                self.buffer.push_ieee754_single(*value)?;
            }
            Instr::F64Const { value } => {
                self.buffer.push_byte(0x44)?;
                self.buffer.push_ieee754_double(*value)?;
            }
            Instr::I32Eqz => self.buffer.push_byte(0x45)?,
            Instr::I32Eq => self.buffer.push_byte(0x46)?,
            Instr::I32Ne => self.buffer.push_byte(0x47)?,
            Instr::I32LtS => self.buffer.push_byte(0x48)?,
            Instr::I32LtU => self.buffer.push_byte(0x49)?,
            Instr::I32GtS => self.buffer.push_byte(0x4a)?,
            Instr::I32GtU => self.buffer.push_byte(0x4b)?,
            Instr::I32LeS => self.buffer.push_byte(0x4c)?,
            Instr::I32LeU => self.buffer.push_byte(0x4d)?,
            Instr::I32GeS => self.buffer.push_byte(0x4e)?,
            Instr::I32GeU => self.buffer.push_byte(0x4f)?,
            Instr::I64Eqz => self.buffer.push_byte(0x50)?,
            Instr::I64Eq => self.buffer.push_byte(0x51)?,
            Instr::I64Ne => self.buffer.push_byte(0x52)?,
            Instr::I64LtS => self.buffer.push_byte(0x53)?,
            Instr::I64LtU => self.buffer.push_byte(0x54)?,
            Instr::I64GtS => self.buffer.push_byte(0x55)?,
            Instr::I64GtU => self.buffer.push_byte(0x56)?,
            Instr::I64LeS => self.buffer.push_byte(0x57)?,
            Instr::I64LeU => self.buffer.push_byte(0x58)?,
            Instr::I64GeS => self.buffer.push_byte(0x59)?,
            Instr::I64GeU => self.buffer.push_byte(0x5a)?,
            Instr::F32Eq => self.buffer.push_byte(0x5b)?,
            Instr::F32Ne => self.buffer.push_byte(0x5c)?,
            Instr::F32Lt => self.buffer.push_byte(0x5d)?,
            Instr::F32Gt => self.buffer.push_byte(0x5e)?,
            Instr::F32Le => self.buffer.push_byte(0x5f)?,
            Instr::F32Ge => self.buffer.push_byte(0x60)?,
            Instr::F64Eq => self.buffer.push_byte(0x61)?,
            Instr::F64Ne => self.buffer.push_byte(0x62)?,
            Instr::F64Lt => self.buffer.push_byte(0x63)?,
            Instr::F64Gt => self.buffer.push_byte(0x64)?,
            Instr::F64Le => self.buffer.push_byte(0x65)?,
            Instr::F64Ge => self.buffer.push_byte(0x66)?,
            Instr::I32Clz => self.buffer.push_byte(0x67)?,
            Instr::I32Ctz => self.buffer.push_byte(0x68)?,
            Instr::I32Popcnt => self.buffer.push_byte(0x69)?,
            Instr::I32Add => self.buffer.push_byte(0x6a)?,
            Instr::I32Sub => self.buffer.push_byte(0x6b)?,
            Instr::I32Mul => self.buffer.push_byte(0x6c)?,
            Instr::I32DivS => self.buffer.push_byte(0x6d)?,
            Instr::I32DivU => self.buffer.push_byte(0x6e)?,
            Instr::I32RemS => self.buffer.push_byte(0x6f)?,
            Instr::I32RemU => self.buffer.push_byte(0x70)?,
            Instr::I32And => self.buffer.push_byte(0x71)?,
            Instr::I32Or => self.buffer.push_byte(0x72)?,
            Instr::I32Xor => self.buffer.push_byte(0x73)?,
            Instr::I32Shl => self.buffer.push_byte(0x74)?,
            Instr::I32ShrS => self.buffer.push_byte(0x75)?,
            Instr::I32ShrU => self.buffer.push_byte(0x76)?,
            Instr::I32Rotl => self.buffer.push_byte(0x77)?,
            Instr::I32Rotr => self.buffer.push_byte(0x78)?,
            Instr::I64Clz => self.buffer.push_byte(0x79)?,
            Instr::I64Ctz => self.buffer.push_byte(0x7a)?,
            Instr::I64Popcnt => self.buffer.push_byte(0x7b)?,
            Instr::I64Add => self.buffer.push_byte(0x7c)?,
            Instr::I64Sub => self.buffer.push_byte(0x7d)?,
            Instr::I64Mul => self.buffer.push_byte(0x7e)?,
            Instr::I64DivS => self.buffer.push_byte(0x7f)?,
            Instr::I64DivU => self.buffer.push_byte(0x80)?,
            Instr::I64RemS => self.buffer.push_byte(0x81)?,
            Instr::I64RemU => self.buffer.push_byte(0x82)?,
            Instr::I64And => self.buffer.push_byte(0x83)?,
            Instr::I64Or => self.buffer.push_byte(0x84)?,
            Instr::I64Xor => self.buffer.push_byte(0x85)?,
            Instr::I64Shl => self.buffer.push_byte(0x86)?,
            Instr::I64ShrS => self.buffer.push_byte(0x87)?,
            Instr::I64ShrU => self.buffer.push_byte(0x88)?,
            Instr::I64Rotl => self.buffer.push_byte(0x89)?,
            Instr::I64Rotr => self.buffer.push_byte(0x8a)?,
            Instr::F32Abs => self.buffer.push_byte(0x8b)?,
            Instr::F32Neg => self.buffer.push_byte(0x8c)?,
            Instr::F32Ceil => self.buffer.push_byte(0x8d)?,
            Instr::F32Floor => self.buffer.push_byte(0x8e)?,
            Instr::F32Trunc => self.buffer.push_byte(0x8f)?,
            Instr::F32Nearest => self.buffer.push_byte(0x90)?,
            Instr::F32Sqrt => self.buffer.push_byte(0x91)?,
            Instr::F32Add => self.buffer.push_byte(0x92)?,
            Instr::F32Sub => self.buffer.push_byte(0x93)?,
            Instr::F32Mul => self.buffer.push_byte(0x94)?,
            Instr::F32Div => self.buffer.push_byte(0x95)?,
            Instr::F32Min => self.buffer.push_byte(0x96)?,
            Instr::F32Max => self.buffer.push_byte(0x97)?,
            Instr::F32Copysign => self.buffer.push_byte(0x98)?,
            Instr::F64Abs => self.buffer.push_byte(0x99)?,
            Instr::F64Neg => self.buffer.push_byte(0x9a)?,
            Instr::F64Ceil => self.buffer.push_byte(0x9b)?,
            Instr::F64Floor => self.buffer.push_byte(0x9c)?,
            Instr::F64Trunc => self.buffer.push_byte(0x9d)?,
            Instr::F64Nearest => self.buffer.push_byte(0x9e)?,
            Instr::F64Sqrt => self.buffer.push_byte(0x9f)?,
            Instr::F64Add => self.buffer.push_byte(0xa0)?,
            Instr::F64Sub => self.buffer.push_byte(0xa1)?,
            Instr::F64Mul => self.buffer.push_byte(0xa2)?,
            Instr::F64Div => self.buffer.push_byte(0xa3)?,
            Instr::F64Min => self.buffer.push_byte(0xa4)?,
            Instr::F64Max => self.buffer.push_byte(0xa5)?,
            Instr::F64Copysign => self.buffer.push_byte(0xa6)?,
            Instr::I32WrapI64 => self.buffer.push_byte(0xa7)?,
            Instr::I32TruncF32S => self.buffer.push_byte(0xa8)?,
            Instr::I32TruncF32U => self.buffer.push_byte(0xa9)?,
            Instr::I32TruncF64S => self.buffer.push_byte(0xaa)?,
            Instr::I32TruncF64U => self.buffer.push_byte(0xab)?,
            Instr::I64ExtendI32S => self.buffer.push_byte(0xac)?,
            Instr::I64ExtendI32U => self.buffer.push_byte(0xad)?,
            Instr::I64TruncF32S => self.buffer.push_byte(0xae)?,
            Instr::I64TruncF32U => self.buffer.push_byte(0xaf)?,
            Instr::I64TruncF64S => self.buffer.push_byte(0xb0)?,
            Instr::I64TruncF64U => self.buffer.push_byte(0xb1)?,
            Instr::F32ConvertI32S => self.buffer.push_byte(0xb2)?,
            Instr::F32ConvertI32U => self.buffer.push_byte(0xb3)?,
            Instr::F32ConvertI64S => self.buffer.push_byte(0xb4)?,
            Instr::F32ConvertI64U => self.buffer.push_byte(0xb5)?,
            Instr::F32DemoteF64 => self.buffer.push_byte(0xb6)?,
            Instr::F64ConvertI32S => self.buffer.push_byte(0xb7)?,
            Instr::F64ConvertI32U => self.buffer.push_byte(0xb8)?,
            Instr::F64ConvertI64S => self.buffer.push_byte(0xb9)?,
            Instr::F64ConvertI64U => self.buffer.push_byte(0xba)?,
            Instr::F64PromoteF32 => self.buffer.push_byte(0xbb)?,
            Instr::I32ReinterpretF32 => self.buffer.push_byte(0xbc)?,
            Instr::I64ReinterpretF64 => self.buffer.push_byte(0xbd)?,
            Instr::F32ReinterpretI32 => self.buffer.push_byte(0xbe)?,
            Instr::F64ReinterpretI64 => self.buffer.push_byte(0xbf)?,
            Instr::I32Extend8S => self.buffer.push_byte(0xc0)?,
            Instr::I32Extend16S => self.buffer.push_byte(0xc1)?,
            Instr::I64Extend8S => self.buffer.push_byte(0xc2)?,
            Instr::I64Extend16S => self.buffer.push_byte(0xc3)?,
            Instr::I64Extend32S => self.buffer.push_byte(0xc4)?,
            Instr::I32TruncSatF32S => self.fc_op(0)?,
            Instr::I32TruncSatF32U => self.fc_op(1)?,
            Instr::I32TruncSatF64S => self.fc_op(2)?,
            Instr::I32TruncSatF64U => self.fc_op(3)?,
            Instr::I64TruncSatF32S => self.fc_op(4)?,
            Instr::I64TruncSatF32U => self.fc_op(5)?,
            Instr::I64TruncSatF64S => self.fc_op(6)?,
            Instr::I64TruncSatF64U => self.fc_op(7)?,
        }

        Ok(())
    }

    fn write_instrs<'f, 'l>(
        &mut self,
        state: &mut State<'f, 'l>,
        instrs: &'l [Instr],
    ) -> Result<()> {
        for instr in instrs {
            self.write_instr(state, instr)?;
        }

        self.buffer.push_byte(0x0b)?;

        Ok(())
    }

    fn write_global_expr(&mut self, expr: &Expr) -> Result<()> {
        self.write_instrs(&mut State::new_const(), &expr.instrs)?;

        Ok(())
    }

    fn write_code_expr(&mut self, func_name: &FuncName, expr: &Expr) -> Result<()> {
        self.write_instrs(
            &mut State::new_func(func_name, &LabelName::from(func_name.as_str())),
            &expr.instrs,
        )?;

        Ok(())
    }

    fn write_global(&mut self, global: &Global) -> Result<()> {
        self.write_global_type(&global.global_type)?;
        self.write_global_expr(&global.expr)?;

        Ok(())
    }

    fn write_export(&mut self, name: &str, export: &Export) -> Result<()> {
        self.write_name(name)?;

        match export {
            Export::Func(func_name) => {
                self.buffer.push_byte(0x00)?;
                self.write_func_name(func_name)?;
            }
            Export::Table(table_name) => {
                self.buffer.push_byte(0x01)?;
                self.write_table_name(table_name)?;
            }
            Export::Memory(mem_name) => {
                self.buffer.push_byte(0x02)?;
                self.write_mem_name(mem_name)?;
            }
            Export::Global(global_name) => {
                self.buffer.push_byte(0x03)?;
                self.write_global_name(global_name)?;
            }
        }

        Ok(())
    }

    fn write_code(&mut self, func_name: &FuncName, func: &Func) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(
                &encode_rle(func.locals.iter().map(|(_, val_type)| val_type)),
                |writer, &(count, val_type)| {
                    writer.buffer.push_leb128_unsigned(count)?;
                    writer.write_val_type(val_type)?;

                    Ok(())
                },
            )?;

            writer.write_code_expr(func_name, &func.expr)?;
        }

        self.buffer.push_vec_bytes(&bytes)?;

        Ok(())
    }

    fn write_type_section(&mut self, types: &[RecType]) -> Result<()> {
        self.write_section_with(1, |writer| {
            writer.write_vec(types, |writer, rec_type| writer.write_rec_type(rec_type))
        })
    }

    fn write_import_section(&mut self, imports: &[(String, String, Import)]) -> Result<()> {
        self.write_section_with(2, |writer| {
            writer.write_vec(imports, |writer, (module_name, name, import)| {
                writer.write_import(module_name, name, import)
            })
        })
    }

    fn write_func_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        self.write_section_with(3, |writer| {
            writer.write_vec(funcs, |writer, (_, func)| writer.write_func(func))
        })
    }

    /// A table with no initializer takes the plain type encoding; one with an initializer takes the function-references form, whose `0x40 0x00` prefix distinguishes it from a reference type's first byte. An initializer is mandatory for a non-defaultable element type and optional otherwise, so the model's `Option` decides which form is written.
    fn write_table(&mut self, table: &Table) -> Result<()> {
        match &table.expr {
            None => self.write_table_type(&table.table_type)?,
            Some(expr) => {
                self.buffer.push_byte(0x40)?;
                self.buffer.push_byte(0x00)?;
                self.write_table_type(&table.table_type)?;
                self.write_global_expr(expr)?;
            }
        }

        Ok(())
    }

    fn write_table_section(&mut self, tables: &[(TableName, Table)]) -> Result<()> {
        if tables.is_empty() {
            return Ok(());
        }

        self.write_section_with(4, |writer| {
            writer.write_vec(tables, |writer, (_, table)| writer.write_table(table))
        })
    }

    fn write_memory_section(&mut self, mems: &[(MemName, MemType)]) -> Result<()> {
        if mems.is_empty() {
            return Ok(());
        }

        self.write_section_with(5, |writer| {
            writer.write_vec(mems, |writer, (_, mem_type)| {
                writer.write_mem_type(mem_type)
            })
        })
    }

    fn write_global_section(&mut self, globals: &[(GlobalName, Global)]) -> Result<()> {
        self.write_section_with(6, |writer| {
            writer.write_vec(globals, |writer, (_, global)| writer.write_global(global))
        })
    }

    fn write_export_section(&mut self, exports: &[(String, Export)]) -> Result<()> {
        self.write_section_with(7, |writer| {
            writer.write_vec(exports, |writer, (name, export)| {
                writer.write_export(name, export)
            })
        })
    }

    fn write_start_section(&mut self, start: &FuncName) -> Result<()> {
        self.write_section_with(8, |writer| writer.write_func_name(start))
    }

    /// One element segment. Its leading flag byte is the mode in bits 0–1 plus bit 2 for an expression list, and the flag decides which operands follow:
    ///
    /// | flag | mode | list | operands, in order |
    /// | --- | --- | --- | --- |
    /// | `0x00` | active at table 0 | func indices | offset |
    /// | `0x01` | passive | func indices | element kind |
    /// | `0x02` | active at a named table | func indices | table index, offset, element kind |
    /// | `0x03` | declarative | func indices | element kind |
    /// | `0x04` | active at table 0 | expressions | offset |
    /// | `0x05` | passive | expressions | element type |
    /// | `0x06` | active at a named table | expressions | table index, offset, element type |
    /// | `0x07` | declarative | expressions | element type |
    ///
    /// The two table-0 forms spell no element type at all, so they are reachable only for a `funcref` list. That is what makes preferring them the smallest *correct* encoding rather than a preference: an expression list of any other reference type must take `0x06` even at table 0, because `0x04` has nowhere to put the type.
    fn write_elem_segment(&mut self, segment: &ElemSegment) -> Result<()> {
        let expr_bit = match &segment.list {
            ElemList::Funcs(_) => 0x00,
            ElemList::Exprs(..) => 0x04,
        };

        let is_typed = match &segment.mode {
            ElemMode::Passive => {
                self.buffer.push_byte(0x01 | expr_bit)?;

                true
            }
            ElemMode::Declarative => {
                self.buffer.push_byte(0x03 | expr_bit)?;

                true
            }
            ElemMode::Active { table_name, offset } => {
                let is_implicit = self.indices.resolve_table(table_name) == 0
                    && match &segment.list {
                        ElemList::Funcs(_) => true,
                        ElemList::Exprs(ref_type, _) => matches!(
                            (ref_type.is_nullable, &ref_type.heap_type),
                            (true, HeapType::Abstract(AbsHeapType::Func))
                        ),
                    };

                match is_implicit {
                    true => {
                        self.buffer.push_byte(expr_bit)?;
                        self.write_global_expr(offset)?;

                        false
                    }
                    false => {
                        self.buffer.push_byte(0x02 | expr_bit)?;
                        self.write_table_name(table_name)?;
                        self.write_global_expr(offset)?;

                        true
                    }
                }
            }
        };

        match &segment.list {
            ElemList::Funcs(func_names) => {
                if is_typed {
                    // Element kind `0x00` is `funcref`, the only kind the format defines.
                    self.buffer.push_byte(0x00)?;
                }

                self.write_vec(func_names, |writer, func_name| {
                    writer.write_func_name(func_name)
                })?;
            }
            ElemList::Exprs(ref_type, exprs) => {
                if is_typed {
                    self.write_ref_type(ref_type)?;
                }

                self.write_vec(exprs, |writer, expr| writer.write_global_expr(expr))?;
            }
        }

        Ok(())
    }

    fn write_element_section(&mut self, elems: &[(ElemName, ElemSegment)]) -> Result<()> {
        if elems.is_empty() {
            return Ok(());
        }

        self.write_section_with(9, |writer| {
            writer.write_vec(elems, |writer, (_, segment)| {
                writer.write_elem_segment(segment)
            })
        })
    }

    fn write_code_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        self.write_section_with(10, |writer| {
            writer.write_vec(funcs, |writer, (func_name, func)| {
                writer.write_code(func_name, func)
            })
        })
    }

    fn write_data_count_section(&mut self, datas: &[(DataName, DataSegment)]) -> Result<()> {
        self.write_section_with(12, |writer| {
            writer.buffer.push_leb128_unsigned(datas.len() as u64)
        })
    }

    /// One data segment. Its leading flag byte is `0x00` for active at memory 0, `0x01` for passive, and `0x02` for active at a named memory whose index follows — the smallest correct encoding, since `0x00` has nowhere to put an index.
    fn write_data_segment(&mut self, segment: &DataSegment) -> Result<()> {
        match &segment.mode {
            DataMode::Passive => self.buffer.push_byte(0x01)?,
            DataMode::Active { mem_name, offset } => {
                match self.indices.resolve_mem(mem_name) {
                    0 => self.buffer.push_byte(0x00)?,
                    index => {
                        self.buffer.push_byte(0x02)?;
                        self.write_index(index)?;
                    }
                }

                self.write_global_expr(offset)?;
            }
        }

        self.buffer.push_vec_bytes(&segment.bytes)?;

        Ok(())
    }

    fn write_data_section(&mut self, datas: &[(DataName, DataSegment)]) -> Result<()> {
        self.write_section_with(11, |writer| {
            writer.write_vec(datas, |writer, (_, segment)| {
                writer.write_data_segment(segment)
            })
        })
    }

    fn write_module_name_subsection(&mut self, module_name: &str) -> Result<()> {
        self.write_section_with(0, |writer| writer.write_name(module_name))
    }

    fn write_func_name_section(
        &mut self,
        imports: &[(String, String, Import)],
        funcs: &[(FuncName, Func)],
    ) -> Result<()> {
        self.write_section_with(1, |writer| {
            writer.write_name_map(
                imports
                    .iter()
                    .flat_map(|(_, _, import)| import.func_name())
                    .chain(funcs.iter().map(|(func_name, _)| func_name))
                    .map(|func_name| {
                        (
                            writer.indices.resolve_func(func_name) as u64,
                            func_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_local_name_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        self.write_section_with(2, |writer| {
            writer.write_indirect_name_map(
                funcs
                    .iter()
                    .map(|(func_name, func)| {
                        (
                            writer.indices.resolve_func(func_name) as u64,
                            func.local_names()
                                .map(|local_name| {
                                    (
                                        writer.indices.resolve_local(func_name, local_name) as u64,
                                        local_name.as_str(),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_table_name_section(
        &mut self,
        imports: &[(String, String, Import)],
        tables: &[(TableName, Table)],
    ) -> Result<()> {
        self.write_section_with(5, |writer| {
            writer.write_name_map(
                imports
                    .iter()
                    .flat_map(|(_, _, import)| import.table_name())
                    .chain(tables.iter().map(|(table_name, _)| table_name))
                    .map(|table_name| {
                        (
                            writer.indices.resolve_table(table_name) as u64,
                            table_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_memory_name_section(
        &mut self,
        imports: &[(String, String, Import)],
        mems: &[(MemName, MemType)],
    ) -> Result<()> {
        self.write_section_with(6, |writer| {
            writer.write_name_map(
                imports
                    .iter()
                    .flat_map(|(_, _, import)| import.mem_name())
                    .chain(mems.iter().map(|(mem_name, _)| mem_name))
                    .map(|mem_name| {
                        (
                            writer.indices.resolve_mem(mem_name) as u64,
                            mem_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_global_name_section(
        &mut self,
        imports: &[(String, String, Import)],
        globals: &[(GlobalName, Global)],
    ) -> Result<()> {
        self.write_section_with(7, |writer| {
            writer.write_name_map(
                imports
                    .iter()
                    .flat_map(|(_, _, import)| import.global_name())
                    .chain(globals.iter().map(|(global_name, _)| global_name))
                    .map(|global_name| {
                        (
                            writer.indices.resolve_global(global_name) as u64,
                            global_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_elem_name_section(&mut self, elems: &[(ElemName, ElemSegment)]) -> Result<()> {
        self.write_section_with(8, |writer| {
            writer.write_name_map(
                elems
                    .iter()
                    .map(|(elem_name, _)| {
                        (
                            writer.indices.resolve_elem(elem_name) as u64,
                            elem_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_data_name_section(&mut self, datas: &[(DataName, DataSegment)]) -> Result<()> {
        self.write_section_with(9, |writer| {
            writer.write_name_map(
                datas
                    .iter()
                    .map(|(data_name, _)| {
                        (
                            writer.indices.resolve_data(data_name) as u64,
                            data_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_type_name_section(&mut self, types: &[RecType]) -> Result<()> {
        self.write_section_with(4, |writer| {
            writer.write_name_map(
                types
                    .iter()
                    .flat_map(|rec_type| rec_type.sub_types.iter())
                    .map(|(type_name, _)| {
                        (
                            writer.indices.resolve_type(type_name) as u64,
                            type_name.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_field_name_section(&mut self, types: &[RecType]) -> Result<()> {
        self.write_section_with(10, |writer| {
            writer.write_indirect_name_map(
                types
                    .iter()
                    .flat_map(|rec_type| rec_type.sub_types.iter())
                    .filter_map(|(type_name, sub_type)| {
                        sub_type.struct_type().map(|struct_type| {
                            (
                                writer.indices.resolve_type(type_name) as u64,
                                struct_type
                                    .fields
                                    .iter()
                                    .map(|(field_name, _)| {
                                        (
                                            writer.indices.resolve_field(type_name, field_name)
                                                as u64,
                                            field_name.as_str(),
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        })
                    })
                    .collect::<Vec<_>>(),
            )
        })
    }

    fn write_name_section(&mut self, module: &Module) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_name("name")?;
            writer.write_module_name_subsection(module.name())?;
            writer.write_func_name_section(module.imports(), module.funcs())?;
            writer.write_local_name_section(module.funcs())?;
            writer.write_type_name_section(module.types())?;
            writer.write_table_name_section(module.imports(), module.tables())?;
            writer.write_memory_name_section(module.imports(), module.mems())?;
            writer.write_global_name_section(module.imports(), module.globals())?;
            writer.write_elem_name_section(module.elems())?;
            writer.write_data_name_section(module.datas())?;
            writer.write_field_name_section(module.types())?;
        }

        self.write_section(0, bytes)?;

        Ok(())
    }

    fn write_module(&mut self, module: &Module) -> Result<()> {
        self.write_magic()?;
        self.write_version()?;
        self.write_type_section(module.types())?;
        self.write_import_section(module.imports())?;
        self.write_func_section(module.funcs())?;
        self.write_table_section(module.tables())?;
        self.write_memory_section(module.mems())?;
        self.write_global_section(module.globals())?;
        self.write_export_section(module.exports())?;
        if let Some(start) = module.start() {
            self.write_start_section(start)?;
        }
        self.write_element_section(module.elems())?;
        self.write_data_count_section(module.datas())?;
        self.write_code_section(module.funcs())?;
        self.write_data_section(module.datas())?;
        self.write_name_section(module)?;

        Ok(())
    }
}

/// Encodes a [`Module`] to the wasm binary format. This is where the crate's symbolic names become numbers: every index space is derived once from the module's declaration order (imports leading), every name in the tree is resolved through it — a dangling reference panics with the missing name — and a trailing custom name section records every index space's names, so disassemblers and profilers show the same identifiers the compiler emitted.
pub fn to_bytes(module: &Module) -> Vec<u8> {
    let mut bytes = Vec::new();

    Writer::new(&Indices::new(module), &mut bytes)
        .write_module(module)
        .unwrap();

    bytes
}
