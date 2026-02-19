use {
    super::{
        AbsHeapType, ArrayType, BlockType, CompType, Export, Expr, FieldName, FieldType, Func,
        FuncName, FuncType, Global, GlobalName, GlobalType, HeapType, Import, Instr, LabelName,
        LocalName, Module, Mutability, NumType, PackedType, RecType, RefType, ResultType,
        StorageType, StructType, SubType, TypeName, ValType,
    },
    std::{
        collections::HashMap,
        io::{Result, Write},
    },
};

pub fn encode_uleb128_unsigned(mut number: u64) -> Vec<u8> {
    let mut bytes = Vec::new();

    loop {
        let byte = (number & 0x7f) as u8;
        let next = number >> 7;

        if next == 0 {
            bytes.push(byte);
            break;
        } else {
            bytes.push(byte | 0x80);
            number = next;
        }
    }

    bytes
}

pub fn encode_leb128_signed(mut number: i64) -> Vec<u8> {
    let mut bytes = Vec::new();

    loop {
        let byte = (number & 0x7f) as u8;
        let sign = byte & 0x40;
        let next = number >> 7;

        if (next == 0 && sign == 0) || (next == -1 && sign != 0) {
            bytes.push(byte);
            break;
        } else {
            bytes.push(byte | 0x80);
            number = next;
        }
    }

    bytes
}

pub fn encode_ieee754_single(number: f32) -> Vec<u8> {
    number.to_bits().to_le_bytes().to_vec()
}

pub fn encode_ieee754_double(number: f64) -> Vec<u8> {
    number.to_bits().to_le_bytes().to_vec()
}

pub fn encode_utf8(string: &str) -> Vec<u8> {
    string.as_bytes().to_vec()
}

pub fn encode_rle<T, I>(values: I) -> Vec<(u64, T)>
where
    T: PartialEq,
    I: IntoIterator<Item = T>,
{
    let mut values = values.into_iter();
    let mut counts = Vec::new();

    let mut current = match values.next() {
        Some(current) => current,
        None => return counts,
    };

    let mut count = 1;

    for value in values {
        if current == value {
            count += 1;
        } else {
            counts.push((count, current));
            current = value;
            count = 1;
        }
    }

    counts.push((count, current));

    counts
}

#[derive(Debug, Clone)]
struct Table<'a> {
    types: HashMap<&'a TypeName, usize>,
    fields: HashMap<(&'a TypeName, &'a FieldName), usize>,
    funcs: HashMap<&'a FuncName, usize>,
    locals: HashMap<(&'a FuncName, &'a LocalName), usize>,
    globals: HashMap<&'a GlobalName, usize>,
}

impl<'a> Table<'a> {
    fn new(module: &'a Module) -> Self {
        let mut types = HashMap::new();
        let mut fields = HashMap::new();

        for (index, (type_name, sub_type)) in module
            .types()
            .iter()
            .flat_map(|rec_type| rec_type.sub_types.iter())
            .enumerate()
        {
            types.insert(type_name, index);

            if let Some(struct_type) = sub_type.struct_type() {
                for (index, (field_name, _)) in struct_type.fields.iter().enumerate() {
                    fields.insert((type_name, field_name), index);
                }
            }
        }

        let mut funcs = HashMap::new();
        let mut locals = HashMap::new();

        for (index, func_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.func_name())
            .enumerate()
        {
            funcs.insert(func_name, index);
        }

        for (index, (func_name, func)) in (funcs.len()..).zip(module.funcs()) {
            funcs.insert(func_name, index);

            for (index, local_name) in func.local_names().enumerate() {
                locals.insert((func_name, local_name), index);
            }
        }

        let mut globals = HashMap::new();

        for (index, global_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.global_name())
            .enumerate()
        {
            globals.insert(global_name, index);
        }

        for (index, (global_name, _)) in (globals.len()..).zip(module.globals()) {
            globals.insert(global_name, index);
        }

        Self {
            types,
            fields,
            funcs,
            locals,
            globals,
        }
    }

    fn resolve_type(&self, name: &'a TypeName) -> usize {
        self.types
            .get(name)
            .cloned()
            .expect(&format!("`Table` lacks type `{}`", name.string))
    }

    fn resolve_field(&self, parent_name: &'a TypeName, name: &'a FieldName) -> usize {
        self.fields
            .get(&(parent_name, name))
            .expect(&format!(
                "`Table` lacks field `{}` of type `{}`",
                name.string, parent_name.string
            ))
            .clone()
    }

    fn resolve_func(&self, name: &'a FuncName) -> usize {
        self.funcs
            .get(name)
            .cloned()
            .expect(&format!("`Table` lacks func `{}`", name.string))
    }

    fn resolve_local(&self, parent_name: &'a FuncName, name: &'a LocalName) -> usize {
        self.locals
            .get(&(parent_name, name))
            .expect(&format!(
                "`Table` lacks local `{}` of func `{}`",
                name.string, parent_name.string
            ))
            .clone()
    }

    fn resolve_global(&self, name: &'a GlobalName) -> usize {
        self.globals
            .get(name)
            .cloned()
            .expect(&format!("`Table` lacks global `{}`", name.string))
    }
}

#[derive(Debug)]
struct Buffer<'w, W> {
    writer: &'w mut W,
}

impl<'w, W> Buffer<'w, W>
where
    W: Write,
{
    fn new(writer: &'w mut W) -> Self {
        Self { writer }
    }

    fn push_byte(&mut self, byte: u8) -> Result<()> {
        self.writer.write_all(&[byte])?;

        Ok(())
    }

    fn push_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.writer.write_all(bytes)?;

        Ok(())
    }

    fn push_leb128_unsigned(&mut self, number: u64) -> Result<()> {
        self.push_bytes(&encode_uleb128_unsigned(number))?;

        Ok(())
    }

    fn push_leb128_signed(&mut self, number: i64) -> Result<()> {
        self.push_bytes(&encode_leb128_signed(number))?;

        Ok(())
    }

    fn push_ieee754_single(&mut self, number: f32) -> Result<()> {
        self.push_bytes(&encode_ieee754_single(number))?;

        Ok(())
    }

    fn push_ieee754_double(&mut self, number: f64) -> Result<()> {
        self.push_bytes(&encode_ieee754_double(number))?;

        Ok(())
    }

    fn push_vec_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.push_leb128_unsigned(bytes.len() as u64)?;
        self.push_bytes(bytes)?;

        Ok(())
    }
}

enum State<'f, 'l> {
    Const,
    Func {
        func_name: &'f FuncName,
        label_names: Vec<&'l LabelName>,
    },
}

impl<'f, 'l> State<'f, 'l> {
    fn new_const() -> Self {
        Self::Const
    }

    fn new_func(func_name: &'f FuncName, label_name: &'l LabelName) -> Self {
        Self::Func {
            func_name,
            label_names: vec![label_name],
        }
    }

    fn owner(&self) -> &'f FuncName {
        match self {
            Self::Const => panic!("`State` is const"),
            Self::Func { func_name, .. } => func_name,
        }
    }

    fn enter_scope(&mut self, label_name: &'l LabelName) {
        match self {
            Self::Const => {
                panic!("`State` is const");
            }
            Self::Func { label_names, .. } => {
                label_names.push(label_name);
            }
        }
    }

    fn leave_scope(&mut self) {
        match self {
            Self::Const => {
                panic!("`State` is const");
            }
            Self::Func { label_names, .. } => {
                label_names.pop();
            }
        }
    }

    fn resolve(&self, target_name: &LabelName) -> usize {
        match self {
            Self::Const => panic!("`State` is const"),
            Self::Func { label_names, .. } => label_names
                .iter()
                .rev()
                .position(|&label_name| target_name == label_name)
                .expect(&format!("`State` lacks label `{}`", target_name.string)),
        }
    }
}

struct Writer<'t, 'w, W> {
    table: &'t Table<'t>,
    buffer: Buffer<'w, W>,
}

impl<'t, 'w, W> Writer<'t, 'w, W>
where
    W: Write,
{
    fn new(table: &'t Table<'t>, writer: &'w mut W) -> Self {
        Self {
            table,
            buffer: Buffer::new(writer),
        }
    }

    fn fork<'u, U>(&self, writer: &'u mut U) -> Writer<'t, 'u, U>
    where
        U: Write,
    {
        Writer::new(self.table, writer)
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

    fn write_type_name(&mut self, type_name: &TypeName) -> Result<()> {
        self.buffer
            .push_leb128_unsigned(self.table.resolve_type(type_name) as u64)?;

        Ok(())
    }

    fn write_type_name_signed(&mut self, type_name: &TypeName) -> Result<()> {
        self.buffer
            .push_leb128_signed(self.table.resolve_type(type_name) as i64)?;

        Ok(())
    }

    fn write_field_name(&mut self, type_name: &TypeName, field_name: &FieldName) -> Result<()> {
        self.buffer
            .push_leb128_unsigned(self.table.resolve_field(type_name, field_name) as u64)?;

        Ok(())
    }

    fn write_func_name(&mut self, func_name: &FuncName) -> Result<()> {
        self.buffer
            .push_leb128_unsigned(self.table.resolve_func(func_name) as u64)?;

        Ok(())
    }

    fn write_local_name(&mut self, func_name: &FuncName, local_name: &LocalName) -> Result<()> {
        self.buffer
            .push_leb128_unsigned(self.table.resolve_local(func_name, local_name) as u64)?;

        Ok(())
    }

    fn write_global_name(&mut self, global_name: &GlobalName) -> Result<()> {
        self.buffer
            .push_leb128_unsigned(self.table.resolve_global(global_name) as u64)?;

        Ok(())
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

    fn write_magic(&mut self) -> Result<()> {
        self.buffer.push_bytes(&[b'\0', b'a', b's', b'm'])?;

        Ok(())
    }

    fn write_version(&mut self) -> Result<()> {
        self.buffer.push_bytes(&1_i32.to_le_bytes())?;

        Ok(())
    }

    fn write_number_type(&mut self, num_type: &NumType) -> Result<()> {
        match num_type {
            NumType::I32 => {
                self.buffer.push_byte(0x7f)?;
            }
            NumType::I64 => {
                self.buffer.push_byte(0x7e)?;
            }
            NumType::F32 => {
                self.buffer.push_byte(0x7d)?;
            }
            NumType::F64 => {
                self.buffer.push_byte(0x7c)?;
            }
        }

        Ok(())
    }

    fn write_abs_heap_type(&mut self, abs_heap_type: &AbsHeapType) -> Result<()> {
        match abs_heap_type {
            AbsHeapType::NoFunc => {
                self.buffer.push_byte(0x73)?;
            }
            AbsHeapType::NoExtern => {
                self.buffer.push_byte(0x72)?;
            }
            AbsHeapType::None => {
                self.buffer.push_byte(0x71)?;
            }
            AbsHeapType::Func => {
                self.buffer.push_byte(0x70)?;
            }
            AbsHeapType::Extern => {
                self.buffer.push_byte(0x6f)?;
            }
            AbsHeapType::Any => {
                self.buffer.push_byte(0x6e)?;
            }
            AbsHeapType::Eq => {
                self.buffer.push_byte(0x6d)?;
            }
            AbsHeapType::I31 => {
                self.buffer.push_byte(0x6c)?;
            }
            AbsHeapType::Struct => {
                self.buffer.push_byte(0x6b)?;
            }
            AbsHeapType::Array => {
                self.buffer.push_byte(0x6a)?;
            }
        }

        Ok(())
    }

    fn write_heap_type(&mut self, heap_type: &HeapType) -> Result<()> {
        match heap_type {
            HeapType::Abstract(abs_heap_type) => {
                self.write_abs_heap_type(abs_heap_type)?;
            }
            HeapType::Concrete(name) => {
                self.write_type_name_signed(&name)?;
            }
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
                self.write_type_name_signed(&type_name)?;
            }
            (false, HeapType::Abstract(abs_heap_type)) => {
                self.buffer.push_byte(0x64)?;
                self.write_abs_heap_type(abs_heap_type)?;
            }
            (false, HeapType::Concrete(type_name)) => {
                self.buffer.push_byte(0x64)?;
                self.write_type_name_signed(&type_name)?;
            }
        }

        Ok(())
    }

    fn write_val_type(&mut self, val_type: &ValType) -> Result<()> {
        match val_type {
            ValType::Num(num_type) => {
                self.write_number_type(num_type)?;
            }
            ValType::Ref(ref_type) => {
                self.write_ref_type(ref_type)?;
            }
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
            PackedType::I8 => {
                self.buffer.push_byte(0x78)?;
            }
            PackedType::I16 => {
                self.buffer.push_byte(0x77)?;
            }
        }

        Ok(())
    }

    fn write_storage_type(&mut self, storage_type: &StorageType) -> Result<()> {
        match storage_type {
            StorageType::Val(val_type) => {
                self.write_val_type(val_type)?;
            }
            StorageType::Packed(packed_type) => {
                self.write_packed_type(packed_type)?;
            }
        }

        Ok(())
    }

    fn write_mutability(&mut self, mutability: &Mutability) -> Result<()> {
        match mutability {
            Mutability::Const => {
                self.buffer.push_byte(0x00)?;
            }
            Mutability::Var => {
                self.buffer.push_byte(0x01)?;
            }
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
                writer.write_type_name(&type_name)?;

                Ok(())
            })?;

            self.write_comp_type(&sub_type.comp_type)?;
        } else if !sub_type.super_types.is_empty() {
            self.buffer.push_byte(0x4f)?;

            self.write_vec(&sub_type.super_types, |writer, type_name| {
                writer.write_type_name(&type_name)?;

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

    fn write_import(&mut self, module_name: &str, name: &str, import: &Import) -> Result<()> {
        self.write_name(module_name)?;
        self.write_name(name)?;

        match import {
            Import::Func { type_name, .. } => {
                self.buffer.push_byte(0x00)?;
                self.write_type_name(type_name)?;
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
            BlockType::Empty => {
                self.buffer.push_byte(0x40)?;
            }
            BlockType::Inline(val_type) => {
                self.write_val_type(val_type)?;
            }
            BlockType::Concrete(type_name) => {
                self.write_type_name_signed(type_name)?;
            }
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
            Instr::Unreachable => {
                self.buffer.push_byte(0x00)?;
            }
            Instr::Nop => {
                self.buffer.push_byte(0x01)?;
            }
            Instr::Block {
                label_name,
                block_type,
                instructions,
            } => {
                self.buffer.push_byte(0x02)?;
                self.write_block_type(block_type)?;

                state.enter_scope(label_name);

                for instr in instructions {
                    self.write_instr(state, instr)?;
                }

                state.leave_scope();
                self.buffer.push_byte(0x0b)?;
            }
            Instr::Loop {
                label_name,
                block_type,
                instructions,
            } => {
                self.buffer.push_byte(0x03)?;
                self.write_block_type(block_type)?;

                state.enter_scope(label_name);

                for instr in instructions {
                    self.write_instr(state, instr)?;
                }

                state.leave_scope();
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

                state.enter_scope(label_name);

                for instr in then_instructions {
                    self.write_instr(state, instr)?;
                }

                state.leave_scope();

                if !else_instructions.is_empty() {
                    self.buffer.push_byte(0x05)?;

                    state.enter_scope(label_name);

                    for instr in else_instructions {
                        self.write_instr(state, instr)?;
                    }

                    state.leave_scope();
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
            Instr::Return => {
                self.buffer.push_byte(0x0f)?;
            }
            Instr::Call { func_name } => {
                self.buffer.push_byte(0x10)?;
                self.write_func_name(func_name)?;
            }
            Instr::CallRef { type_name } => {
                self.buffer.push_byte(0x14)?;
                self.write_type_name(type_name)?;
            }
            Instr::ReturnCall { func_name } => {
                self.buffer.push_byte(0x12)?;
                self.write_func_name(func_name)?;
            }
            Instr::ReturnCallRef { type_name } => {
                self.buffer.push_byte(0x15)?;
                self.write_type_name(type_name)?;
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
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(25)?;
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
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(24)?;
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
            Instr::RefIsNull => {
                self.buffer.push_byte(0xd1)?;
            }
            Instr::RefFunc { func_name } => {
                self.buffer.push_byte(0xd2)?;
                self.write_func_name(func_name)?;
            }
            Instr::RefEq => {
                self.buffer.push_byte(0xd3)?;
            }
            Instr::RefAsNonNull => {
                self.buffer.push_byte(0xd4)?;
            }
            Instr::StructNew { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(0)?;
                self.write_type_name(type_name)?;
            }
            Instr::StructNewDefault { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(1)?;
                self.write_type_name(type_name)?;
            }
            Instr::StructGet {
                type_name,
                field_name,
            } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(2)?;
                self.write_type_name(type_name)?;
                self.write_field_name(type_name, field_name)?;
            }
            Instr::StructGetS {
                type_name,
                field_name,
            } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(3)?;
                self.write_type_name(type_name)?;
                self.write_field_name(type_name, field_name)?;
            }
            Instr::StructGetU {
                type_name,
                field_name,
            } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(4)?;
                self.write_type_name(type_name)?;
                self.write_field_name(type_name, field_name)?;
            }
            Instr::StructSet {
                type_name,
                field_name,
            } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(5)?;
                self.write_type_name(type_name)?;
                self.write_field_name(type_name, field_name)?;
            }
            Instr::ArrayNew { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(6)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayNewDefault { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(7)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayNewFixed { type_name, length } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(8)?;
                self.write_type_name(type_name)?;
                self.buffer.push_leb128_unsigned(*length as u64)?;
            }
            Instr::ArrayGet { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(11)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayGetS { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(12)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayGetU { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(13)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArraySet { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(14)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayLen => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(15)?;
            }
            Instr::ArrayFill { type_name } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(16)?;
                self.write_type_name(type_name)?;
            }
            Instr::ArrayCopy {
                source_name,
                target_name,
            } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(17)?;
                self.write_type_name(source_name)?;
                self.write_type_name(target_name)?;
            }
            Instr::RefTest { ref_type } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer
                    .push_leb128_unsigned(if ref_type.is_nullable { 21 } else { 20 })?;
                self.write_heap_type(&ref_type.heap_type)?;
            }
            Instr::RefCast { ref_type } => {
                self.buffer.push_byte(0xfb)?;
                self.buffer
                    .push_leb128_unsigned(if ref_type.is_nullable { 23 } else { 22 })?;
                self.write_heap_type(&ref_type.heap_type)?;
            }
            Instr::AnyConvertExtern => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(26)?;
            }
            Instr::ExternConvertAny => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(27)?;
            }
            Instr::RefI31 => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(28)?;
            }
            Instr::I31GetS => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(29)?;
            }
            Instr::I31GetU => {
                self.buffer.push_byte(0xfb)?;
                self.buffer.push_leb128_unsigned(30)?;
            }
            Instr::Drop => {
                self.buffer.push_byte(0x1a)?;
            }
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
            Instr::I32Eqz => {
                self.buffer.push_byte(0x45)?;
            }
            Instr::I32Eq => {
                self.buffer.push_byte(0x46)?;
            }
            Instr::I32Ne => {
                self.buffer.push_byte(0x47)?;
            }
            Instr::I32LtS => {
                self.buffer.push_byte(0x48)?;
            }
            Instr::I32LtU => {
                self.buffer.push_byte(0x49)?;
            }
            Instr::I32GtS => {
                self.buffer.push_byte(0x4a)?;
            }
            Instr::I32GtU => {
                self.buffer.push_byte(0x4b)?;
            }
            Instr::I32LeS => {
                self.buffer.push_byte(0x4c)?;
            }
            Instr::I32LeU => {
                self.buffer.push_byte(0x4d)?;
            }
            Instr::I32GeS => {
                self.buffer.push_byte(0x4e)?;
            }
            Instr::I32GeU => {
                self.buffer.push_byte(0x4f)?;
            }
            Instr::I64Eqz => {
                self.buffer.push_byte(0x50)?;
            }
            Instr::I64Eq => {
                self.buffer.push_byte(0x51)?;
            }
            Instr::I64Ne => {
                self.buffer.push_byte(0x52)?;
            }
            Instr::I64LtS => {
                self.buffer.push_byte(0x53)?;
            }
            Instr::I64LtU => {
                self.buffer.push_byte(0x54)?;
            }
            Instr::I64GtS => {
                self.buffer.push_byte(0x55)?;
            }
            Instr::I64GtU => {
                self.buffer.push_byte(0x56)?;
            }
            Instr::I64LeS => {
                self.buffer.push_byte(0x57)?;
            }
            Instr::I64LeU => {
                self.buffer.push_byte(0x58)?;
            }
            Instr::I64GeS => {
                self.buffer.push_byte(0x59)?;
            }
            Instr::I64GeU => {
                self.buffer.push_byte(0x5a)?;
            }
            Instr::F32Eq => {
                self.buffer.push_byte(0x5b)?;
            }
            Instr::F32Ne => {
                self.buffer.push_byte(0x5c)?;
            }
            Instr::F32Lt => {
                self.buffer.push_byte(0x5d)?;
            }
            Instr::F32Gt => {
                self.buffer.push_byte(0x5e)?;
            }
            Instr::F32Le => {
                self.buffer.push_byte(0x5f)?;
            }
            Instr::F32Ge => {
                self.buffer.push_byte(0x60)?;
            }
            Instr::F64Eq => {
                self.buffer.push_byte(0x61)?;
            }
            Instr::F64Ne => {
                self.buffer.push_byte(0x62)?;
            }
            Instr::F64Lt => {
                self.buffer.push_byte(0x63)?;
            }
            Instr::F64Gt => {
                self.buffer.push_byte(0x64)?;
            }
            Instr::F64Le => {
                self.buffer.push_byte(0x65)?;
            }
            Instr::F64Ge => {
                self.buffer.push_byte(0x66)?;
            }
            Instr::I32Clz => {
                self.buffer.push_byte(0x67)?;
            }
            Instr::I32Ctz => {
                self.buffer.push_byte(0x68)?;
            }
            Instr::I32Popcnt => {
                self.buffer.push_byte(0x69)?;
            }
            Instr::I32Add => {
                self.buffer.push_byte(0x6a)?;
            }
            Instr::I32Sub => {
                self.buffer.push_byte(0x6b)?;
            }
            Instr::I32Mul => {
                self.buffer.push_byte(0x6c)?;
            }
            Instr::I32DivS => {
                self.buffer.push_byte(0x6d)?;
            }
            Instr::I32DivU => {
                self.buffer.push_byte(0x6e)?;
            }
            Instr::I32RemS => {
                self.buffer.push_byte(0x6f)?;
            }
            Instr::I32RemU => {
                self.buffer.push_byte(0x70)?;
            }
            Instr::I32And => {
                self.buffer.push_byte(0x71)?;
            }
            Instr::I32Or => {
                self.buffer.push_byte(0x72)?;
            }
            Instr::I32Xor => {
                self.buffer.push_byte(0x73)?;
            }
            Instr::I32Shl => {
                self.buffer.push_byte(0x74)?;
            }
            Instr::I32ShrS => {
                self.buffer.push_byte(0x75)?;
            }
            Instr::I32ShrU => {
                self.buffer.push_byte(0x76)?;
            }
            Instr::I32Rotl => {
                self.buffer.push_byte(0x77)?;
            }
            Instr::I32Rotr => {
                self.buffer.push_byte(0x78)?;
            }
            Instr::I64Clz => {
                self.buffer.push_byte(0x79)?;
            }
            Instr::I64Ctz => {
                self.buffer.push_byte(0x7a)?;
            }
            Instr::I64Popcnt => {
                self.buffer.push_byte(0x7b)?;
            }
            Instr::I64Add => {
                self.buffer.push_byte(0x7c)?;
            }
            Instr::I64Sub => {
                self.buffer.push_byte(0x7d)?;
            }
            Instr::I64Mul => {
                self.buffer.push_byte(0x7e)?;
            }
            Instr::I64DivS => {
                self.buffer.push_byte(0x7f)?;
            }
            Instr::I64DivU => {
                self.buffer.push_byte(0x80)?;
            }
            Instr::I64RemS => {
                self.buffer.push_byte(0x81)?;
            }
            Instr::I64RemU => {
                self.buffer.push_byte(0x82)?;
            }
            Instr::I64And => {
                self.buffer.push_byte(0x83)?;
            }
            Instr::I64Or => {
                self.buffer.push_byte(0x84)?;
            }
            Instr::I64Xor => {
                self.buffer.push_byte(0x85)?;
            }
            Instr::I64Shl => {
                self.buffer.push_byte(0x86)?;
            }
            Instr::I64ShrS => {
                self.buffer.push_byte(0x87)?;
            }
            Instr::I64ShrU => {
                self.buffer.push_byte(0x88)?;
            }
            Instr::I64Rotl => {
                self.buffer.push_byte(0x89)?;
            }
            Instr::I64Rotr => {
                self.buffer.push_byte(0x8a)?;
            }
            Instr::F32Abs => {
                self.buffer.push_byte(0x8b)?;
            }
            Instr::F32Neg => {
                self.buffer.push_byte(0x8c)?;
            }
            Instr::F32Ceil => {
                self.buffer.push_byte(0x8d)?;
            }
            Instr::F32Floor => {
                self.buffer.push_byte(0x8e)?;
            }
            Instr::F32Trunc => {
                self.buffer.push_byte(0x8f)?;
            }
            Instr::F32Nearest => {
                self.buffer.push_byte(0x90)?;
            }
            Instr::F32Sqrt => {
                self.buffer.push_byte(0x91)?;
            }
            Instr::F32Add => {
                self.buffer.push_byte(0x92)?;
            }
            Instr::F32Sub => {
                self.buffer.push_byte(0x93)?;
            }
            Instr::F32Mul => {
                self.buffer.push_byte(0x94)?;
            }
            Instr::F32Div => {
                self.buffer.push_byte(0x95)?;
            }
            Instr::F32Min => {
                self.buffer.push_byte(0x96)?;
            }
            Instr::F32Max => {
                self.buffer.push_byte(0x97)?;
            }
            Instr::F32Copysign => {
                self.buffer.push_byte(0x98)?;
            }
            Instr::F64Abs => {
                self.buffer.push_byte(0x99)?;
            }
            Instr::F64Neg => {
                self.buffer.push_byte(0x9a)?;
            }
            Instr::F64Ceil => {
                self.buffer.push_byte(0x9b)?;
            }
            Instr::F64Floor => {
                self.buffer.push_byte(0x9c)?;
            }
            Instr::F64Trunc => {
                self.buffer.push_byte(0x9d)?;
            }
            Instr::F64Nearest => {
                self.buffer.push_byte(0x9e)?;
            }
            Instr::F64Sqrt => {
                self.buffer.push_byte(0x9f)?;
            }
            Instr::F64Add => {
                self.buffer.push_byte(0xa0)?;
            }
            Instr::F64Sub => {
                self.buffer.push_byte(0xa1)?;
            }
            Instr::F64Mul => {
                self.buffer.push_byte(0xa2)?;
            }
            Instr::F64Div => {
                self.buffer.push_byte(0xa3)?;
            }
            Instr::F64Min => {
                self.buffer.push_byte(0xa4)?;
            }
            Instr::F64Max => {
                self.buffer.push_byte(0xa5)?;
            }
            Instr::F64Copysign => {
                self.buffer.push_byte(0xa6)?;
            }
            Instr::I32WrapI64 => {
                self.buffer.push_byte(0xa7)?;
            }
            Instr::I32TruncF32S => {
                self.buffer.push_byte(0xa8)?;
            }
            Instr::I32TruncF32U => {
                self.buffer.push_byte(0xa9)?;
            }
            Instr::I32TruncF64S => {
                self.buffer.push_byte(0xaa)?;
            }
            Instr::I32TruncF64U => {
                self.buffer.push_byte(0xab)?;
            }
            Instr::I64ExtendI32S => {
                self.buffer.push_byte(0xac)?;
            }
            Instr::I64ExtendI32U => {
                self.buffer.push_byte(0xad)?;
            }
            Instr::I64TruncF32S => {
                self.buffer.push_byte(0xae)?;
            }
            Instr::I64TruncF32U => {
                self.buffer.push_byte(0xaf)?;
            }
            Instr::I64TruncF64S => {
                self.buffer.push_byte(0xb0)?;
            }
            Instr::I64TruncF64U => {
                self.buffer.push_byte(0xb1)?;
            }
            Instr::F32ConvertI32S => {
                self.buffer.push_byte(0xb2)?;
            }
            Instr::F32ConvertI32U => {
                self.buffer.push_byte(0xb3)?;
            }
            Instr::F32ConvertI64S => {
                self.buffer.push_byte(0xb4)?;
            }
            Instr::F32ConvertI64U => {
                self.buffer.push_byte(0xb5)?;
            }
            Instr::F32DemoteF64 => {
                self.buffer.push_byte(0xb6)?;
            }
            Instr::F64ConvertI32S => {
                self.buffer.push_byte(0xb7)?;
            }
            Instr::F64ConvertI32U => {
                self.buffer.push_byte(0xb8)?;
            }
            Instr::F64ConvertI64S => {
                self.buffer.push_byte(0xb9)?;
            }
            Instr::F64ConvertI64U => {
                self.buffer.push_byte(0xba)?;
            }
            Instr::F64PromoteF32 => {
                self.buffer.push_byte(0xbb)?;
            }
            Instr::I32ReinterpretF32 => {
                self.buffer.push_byte(0xbc)?;
            }
            Instr::I64ReinterpretF64 => {
                self.buffer.push_byte(0xbd)?;
            }
            Instr::F32ReinterpretI32 => {
                self.buffer.push_byte(0xbe)?;
            }
            Instr::F64ReinterpretI64 => {
                self.buffer.push_byte(0xbf)?;
            }
            Instr::I32Extend8S => {
                self.buffer.push_byte(0xc0)?;
            }
            Instr::I32Extend16S => {
                self.buffer.push_byte(0xc1)?;
            }
            Instr::I64Extend8S => {
                self.buffer.push_byte(0xc2)?;
            }
            Instr::I64Extend16S => {
                self.buffer.push_byte(0xc3)?;
            }
            Instr::I64Extend32S => {
                self.buffer.push_byte(0xc4)?;
            }
            Instr::I32TruncSatF32S => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(0)?;
            }
            Instr::I32TruncSatF32U => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(1)?;
            }
            Instr::I32TruncSatF64S => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(2)?;
            }
            Instr::I32TruncSatF64U => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(3)?;
            }
            Instr::I64TruncSatF32S => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(4)?;
            }
            Instr::I64TruncSatF32U => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(5)?;
            }
            Instr::I64TruncSatF64S => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(6)?;
            }
            Instr::I64TruncSatF64U => {
                self.buffer.push_byte(0xfc)?;
                self.buffer.push_leb128_unsigned(7)?;
            }
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
            &mut State::new_func(func_name, &LabelName::from(&func_name.string)),
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
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(types, |writer, rec_type| {
                writer.write_rec_type(rec_type)?;

                Ok(())
            })?;
        }

        self.write_section(1, bytes)?;

        Ok(())
    }

    fn write_import_section(&mut self, imports: &[(String, String, Import)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(imports, |writer, (module_name, name, import)| {
                writer.write_import(module_name, name, import)?;

                Ok(())
            })?;
        }

        self.write_section(2, bytes)?;

        Ok(())
    }

    fn write_func_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(funcs, |writer, (_, func)| {
                writer.write_func(func)?;

                Ok(())
            })?;
        }

        self.write_section(3, bytes)?;

        Ok(())
    }

    fn write_global_section(&mut self, globals: &[(GlobalName, Global)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(globals, |writer, (_, global)| {
                writer.write_global(global)?;

                Ok(())
            })?;
        }

        self.write_section(6, bytes)?;

        Ok(())
    }

    fn write_export_section(&mut self, exports: &[(String, Export)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(exports, |writer, (name, export)| {
                writer.write_export(name, export)?;

                Ok(())
            })?;
        }

        self.write_section(7, bytes)?;

        Ok(())
    }

    fn write_code_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_vec(funcs, |writer, (func_name, func)| {
                writer.write_code(func_name, func)?;

                Ok(())
            })?;
        }

        self.write_section(10, bytes)?;

        Ok(())
    }

    fn write_module_name_subsection(&mut self, module_name: &str) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_name(module_name)?;
        }

        self.write_section(0, bytes)?;

        Ok(())
    }

    fn write_func_name_section(
        &mut self,
        imports: &[(String, String, Import)],
        funcs: &[(FuncName, Func)],
    ) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_name_map(
                imports
                    .iter()
                    .flat_map(|(_, _, import)| import.func_name())
                    .chain(funcs.iter().map(|(func_name, _)| func_name))
                    .map(|func_name| {
                        (
                            self.table.resolve_func(func_name) as u64,
                            func_name.string.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )?;
        }

        self.write_section(1, bytes)?;

        Ok(())
    }

    fn write_local_name_section(&mut self, funcs: &[(FuncName, Func)]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_indirect_name_map(
                funcs
                    .iter()
                    .map(|(func_name, func)| {
                        (
                            self.table.resolve_func(func_name) as u64,
                            func.local_names()
                                .map(|local_name| {
                                    (
                                        self.table.resolve_local(func_name, local_name) as u64,
                                        local_name.string.as_str(),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )?;
        }

        self.write_section(2, bytes)?;

        Ok(())
    }

    fn write_type_name_section(&mut self, types: &[RecType]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_name_map(
                types
                    .iter()
                    .flat_map(|rec_type| rec_type.sub_types.iter())
                    .map(|(type_name, _)| {
                        (
                            self.table.resolve_type(type_name) as u64,
                            type_name.string.as_str(),
                        )
                    })
                    .collect::<Vec<_>>(),
            )?;
        }

        self.write_section(4, bytes)?;

        Ok(())
    }

    fn write_field_name_section(&mut self, types: &[RecType]) -> Result<()> {
        let mut bytes = Vec::new();

        {
            let mut writer = self.fork(&mut bytes);

            writer.write_indirect_name_map(
                types
                    .iter()
                    .flat_map(|rec_type| rec_type.sub_types.iter())
                    .filter_map(|(type_name, sub_type)| {
                        sub_type.struct_type().map(|struct_type| {
                            (
                                self.table.resolve_type(type_name) as u64,
                                struct_type
                                    .field_names()
                                    .map(|field_name| {
                                        (
                                            self.table.resolve_field(type_name, field_name) as u64,
                                            field_name.string.as_str(),
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        })
                    })
                    .collect::<Vec<_>>(),
            )?;
        }

        self.write_section(10, bytes)?;

        Ok(())
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
        self.write_global_section(module.globals())?;
        self.write_export_section(module.exports())?;
        self.write_code_section(module.funcs())?;
        self.write_name_section(module)?;

        Ok(())
    }
}

pub fn to_bytes(module: &Module) -> Vec<u8> {
    let mut bytes = Vec::new();

    Writer::new(&Table::new(module), &mut bytes)
        .write_module(module)
        .unwrap();

    bytes
}
