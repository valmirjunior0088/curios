use super::{
    BlockType, DataName, ElemName, FieldName, FuncName, GlobalName, HeapType, LabelName, LocalName,
    MemName, RefType, TableName, TypeName, ValType,
};

/// The immediate every load and store carries: the memory it reaches, the alignment its address is promised to have — as the binary format's log2 exponent, not a byte count — and a static offset added to the dynamic address operand, wide enough for a 64-bit memory. The memory is named like every other cross-reference, so the encoder is the only place that knows one of them is index 0 and may leave the index implicit.
#[derive(Debug, Clone)]
pub struct MemArg {
    pub mem_name: MemName,
    pub align: u32,
    pub offset: u64,
}

/// The backend's instruction set, one variant per wasm opcode the crate can encode. Every operand the binary format expresses as an index — labels, functions, types, struct fields, locals, globals, tables, memories, element and data segments — is carried here as a name and resolved by the encoder, so emitters never track index spaces.
///
/// The roster covers the whole envelope's table and memory surface and enforces nothing about how it is used. That program values live in GC references rather than linear memory is the *emitter's* discipline, stated by [WebAssembly-GC is the only target](../../documentation/design/toolchain/webassembly-gc-is-the-only-target.md); it was once enforced twice, because the roster physically could not express the alternative, and this crate is no longer the second enforcer.
///
/// Field order mirrors operand order in the encoding wherever the two could disagree. The three copies — `ArrayCopy`, `MemoryCopy`, `TableCopy` — each name their *target* before their source, because that is the order the format writes the two indices in; a variant that listed them the other way would put the field called `source_name` in the destination slot, which is what `ArrayCopy` did until both of its consumers happened to pass one type for both.
#[derive(Debug, Clone)]
pub enum Instr {
    Unreachable,
    Nop,
    Block {
        label_name: LabelName,
        block_type: BlockType,
        instructions: Vec<Instr>,
    },
    Loop {
        label_name: LabelName,
        block_type: BlockType,
        instructions: Vec<Instr>,
    },
    If {
        label_name: LabelName,
        block_type: BlockType,
        then_instructions: Vec<Instr>,
        else_instructions: Vec<Instr>,
    },
    Br {
        label_name: LabelName,
    },
    BrIf {
        label_name: LabelName,
    },
    BrTable {
        label_names: Vec<LabelName>,
        label_name: LabelName,
    },
    Return,
    Call {
        func_name: FuncName,
    },
    CallRef {
        type_name: TypeName,
    },
    CallIndirect {
        table_name: TableName,
        type_name: TypeName,
    },
    ReturnCall {
        func_name: FuncName,
    },
    ReturnCallRef {
        type_name: TypeName,
    },
    ReturnCallIndirect {
        table_name: TableName,
        type_name: TypeName,
    },
    BrOnNull {
        label_name: LabelName,
    },
    BrOnNonNull {
        label_name: LabelName,
    },
    BrOnCast {
        label_name: LabelName,
        source_type: RefType,
        target_type: RefType,
    },
    BrOnCastFail {
        label_name: LabelName,
        source_type: RefType,
        target_type: RefType,
    },
    RefNull {
        heap_type: HeapType,
    },
    RefIsNull,
    RefFunc {
        func_name: FuncName,
    },
    RefEq,
    RefAsNonNull,
    StructNew {
        type_name: TypeName,
    },
    StructNewDefault {
        type_name: TypeName,
    },
    StructGet {
        type_name: TypeName,
        field_name: FieldName,
    },
    StructGetS {
        type_name: TypeName,
        field_name: FieldName,
    },
    StructGetU {
        type_name: TypeName,
        field_name: FieldName,
    },
    StructSet {
        type_name: TypeName,
        field_name: FieldName,
    },
    ArrayNew {
        type_name: TypeName,
    },
    ArrayNewDefault {
        type_name: TypeName,
    },
    ArrayNewFixed {
        type_name: TypeName,
        length: u32,
    },
    ArrayNewData {
        type_name: TypeName,
        data_name: DataName,
    },
    ArrayNewElem {
        type_name: TypeName,
        elem_name: ElemName,
    },
    ArrayGet {
        type_name: TypeName,
    },
    ArrayGetS {
        type_name: TypeName,
    },
    ArrayGetU {
        type_name: TypeName,
    },
    ArraySet {
        type_name: TypeName,
    },
    ArrayLen,
    ArrayFill {
        type_name: TypeName,
    },
    ArrayCopy {
        target_name: TypeName,
        source_name: TypeName,
    },
    ArrayInitData {
        type_name: TypeName,
        data_name: DataName,
    },
    ArrayInitElem {
        type_name: TypeName,
        elem_name: ElemName,
    },
    RefTest {
        ref_type: RefType,
    },
    RefCast {
        ref_type: RefType,
    },
    AnyConvertExtern,
    ExternConvertAny,
    RefI31,
    I31GetS,
    I31GetU,
    I32Load {
        mem_arg: MemArg,
    },
    I64Load {
        mem_arg: MemArg,
    },
    F32Load {
        mem_arg: MemArg,
    },
    F64Load {
        mem_arg: MemArg,
    },
    I32Load8S {
        mem_arg: MemArg,
    },
    I32Load8U {
        mem_arg: MemArg,
    },
    I32Load16S {
        mem_arg: MemArg,
    },
    I32Load16U {
        mem_arg: MemArg,
    },
    I64Load8S {
        mem_arg: MemArg,
    },
    I64Load8U {
        mem_arg: MemArg,
    },
    I64Load16S {
        mem_arg: MemArg,
    },
    I64Load16U {
        mem_arg: MemArg,
    },
    I64Load32S {
        mem_arg: MemArg,
    },
    I64Load32U {
        mem_arg: MemArg,
    },
    I32Store {
        mem_arg: MemArg,
    },
    I64Store {
        mem_arg: MemArg,
    },
    F32Store {
        mem_arg: MemArg,
    },
    F64Store {
        mem_arg: MemArg,
    },
    I32Store8 {
        mem_arg: MemArg,
    },
    I32Store16 {
        mem_arg: MemArg,
    },
    I64Store8 {
        mem_arg: MemArg,
    },
    I64Store16 {
        mem_arg: MemArg,
    },
    I64Store32 {
        mem_arg: MemArg,
    },
    MemorySize {
        mem_name: MemName,
    },
    MemoryGrow {
        mem_name: MemName,
    },
    MemoryFill {
        mem_name: MemName,
    },
    MemoryCopy {
        target_name: MemName,
        source_name: MemName,
    },
    MemoryInit {
        mem_name: MemName,
        data_name: DataName,
    },
    DataDrop {
        data_name: DataName,
    },
    TableGet {
        table_name: TableName,
    },
    TableSet {
        table_name: TableName,
    },
    TableSize {
        table_name: TableName,
    },
    TableGrow {
        table_name: TableName,
    },
    TableFill {
        table_name: TableName,
    },
    TableCopy {
        target_name: TableName,
        source_name: TableName,
    },
    TableInit {
        table_name: TableName,
        elem_name: ElemName,
    },
    ElemDrop {
        elem_name: ElemName,
    },
    Drop,
    Select {
        val_types: Vec<ValType>,
    },
    LocalGet {
        local_name: LocalName,
    },
    LocalSet {
        local_name: LocalName,
    },
    LocalTee {
        local_name: LocalName,
    },
    GlobalGet {
        global_name: GlobalName,
    },
    GlobalSet {
        global_name: GlobalName,
    },
    I32Const {
        value: i32,
    },
    I64Const {
        value: i64,
    },
    F32Const {
        value: f32,
    },
    F64Const {
        value: f64,
    },
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,
}

/// Declares the operand-less instructions' WAT spellings from one table, so the printer's [`Instr::mnemonic`] and the parser's [`Instr::from_mnemonic`] cannot drift apart. The operand-carrying instructions stay hand-spelled in `print` and `parse`, where the compiler still forces an arm per variant.
macro_rules! mnemonics {
    ($($variant:ident => $mnemonic:literal,)+) => {
        impl Instr {
            /// The WAT spelling of an operand-less instruction; `None` for the operand-carrying forms the printer spells by hand.
            pub(crate) fn mnemonic(&self) -> Option<&'static str> {
                match self {
                    $(Instr::$variant => Some($mnemonic),)+
                    _ => None,
                }
            }

            /// The operand-less instruction spelled `token`, if any — [`Instr::mnemonic`]'s reverse, for the parser's whole-token dispatch.
            pub(crate) fn from_mnemonic(token: &str) -> Option<Instr> {
                match token {
                    $($mnemonic => Some(Instr::$variant),)+
                    _ => None,
                }
            }
        }
    };
}

mnemonics! {
    Unreachable => "unreachable",
    Nop => "nop",
    Return => "return",
    RefIsNull => "ref.is_null",
    RefEq => "ref.eq",
    RefAsNonNull => "ref.as_non_null",
    ArrayLen => "array.len",
    AnyConvertExtern => "any.convert_extern",
    ExternConvertAny => "extern.convert_any",
    RefI31 => "ref.i31",
    I31GetS => "i31.get_s",
    I31GetU => "i31.get_u",
    Drop => "drop",
    I32Eqz => "i32.eqz",
    I32Eq => "i32.eq",
    I32Ne => "i32.ne",
    I32LtS => "i32.lt_s",
    I32LtU => "i32.lt_u",
    I32GtS => "i32.gt_s",
    I32GtU => "i32.gt_u",
    I32LeS => "i32.le_s",
    I32LeU => "i32.le_u",
    I32GeS => "i32.ge_s",
    I32GeU => "i32.ge_u",
    I64Eqz => "i64.eqz",
    I64Eq => "i64.eq",
    I64Ne => "i64.ne",
    I64LtS => "i64.lt_s",
    I64LtU => "i64.lt_u",
    I64GtS => "i64.gt_s",
    I64GtU => "i64.gt_u",
    I64LeS => "i64.le_s",
    I64LeU => "i64.le_u",
    I64GeS => "i64.ge_s",
    I64GeU => "i64.ge_u",
    F32Eq => "f32.eq",
    F32Ne => "f32.ne",
    F32Lt => "f32.lt",
    F32Gt => "f32.gt",
    F32Le => "f32.le",
    F32Ge => "f32.ge",
    F64Eq => "f64.eq",
    F64Ne => "f64.ne",
    F64Lt => "f64.lt",
    F64Gt => "f64.gt",
    F64Le => "f64.le",
    F64Ge => "f64.ge",
    I32Clz => "i32.clz",
    I32Ctz => "i32.ctz",
    I32Popcnt => "i32.popcnt",
    I32Add => "i32.add",
    I32Sub => "i32.sub",
    I32Mul => "i32.mul",
    I32DivS => "i32.div_s",
    I32DivU => "i32.div_u",
    I32RemS => "i32.rem_s",
    I32RemU => "i32.rem_u",
    I32And => "i32.and",
    I32Or => "i32.or",
    I32Xor => "i32.xor",
    I32Shl => "i32.shl",
    I32ShrS => "i32.shr_s",
    I32ShrU => "i32.shr_u",
    I32Rotl => "i32.rotl",
    I32Rotr => "i32.rotr",
    I64Clz => "i64.clz",
    I64Ctz => "i64.ctz",
    I64Popcnt => "i64.popcnt",
    I64Add => "i64.add",
    I64Sub => "i64.sub",
    I64Mul => "i64.mul",
    I64DivS => "i64.div_s",
    I64DivU => "i64.div_u",
    I64RemS => "i64.rem_s",
    I64RemU => "i64.rem_u",
    I64And => "i64.and",
    I64Or => "i64.or",
    I64Xor => "i64.xor",
    I64Shl => "i64.shl",
    I64ShrS => "i64.shr_s",
    I64ShrU => "i64.shr_u",
    I64Rotl => "i64.rotl",
    I64Rotr => "i64.rotr",
    F32Abs => "f32.abs",
    F32Neg => "f32.neg",
    F32Ceil => "f32.ceil",
    F32Floor => "f32.floor",
    F32Trunc => "f32.trunc",
    F32Nearest => "f32.nearest",
    F32Sqrt => "f32.sqrt",
    F32Add => "f32.add",
    F32Sub => "f32.sub",
    F32Mul => "f32.mul",
    F32Div => "f32.div",
    F32Min => "f32.min",
    F32Max => "f32.max",
    F32Copysign => "f32.copysign",
    F64Abs => "f64.abs",
    F64Neg => "f64.neg",
    F64Ceil => "f64.ceil",
    F64Floor => "f64.floor",
    F64Trunc => "f64.trunc",
    F64Nearest => "f64.nearest",
    F64Sqrt => "f64.sqrt",
    F64Add => "f64.add",
    F64Sub => "f64.sub",
    F64Mul => "f64.mul",
    F64Div => "f64.div",
    F64Min => "f64.min",
    F64Max => "f64.max",
    F64Copysign => "f64.copysign",
    I32WrapI64 => "i32.wrap_i64",
    I32TruncF32S => "i32.trunc_f32_s",
    I32TruncF32U => "i32.trunc_f32_u",
    I32TruncF64S => "i32.trunc_f64_s",
    I32TruncF64U => "i32.trunc_f64_u",
    I64ExtendI32S => "i64.extend_i32_s",
    I64ExtendI32U => "i64.extend_i32_u",
    I64TruncF32S => "i64.trunc_f32_s",
    I64TruncF32U => "i64.trunc_f32_u",
    I64TruncF64S => "i64.trunc_f64_s",
    I64TruncF64U => "i64.trunc_f64_u",
    F32ConvertI32S => "f32.convert_i32_s",
    F32ConvertI32U => "f32.convert_i32_u",
    F32ConvertI64S => "f32.convert_i64_s",
    F32ConvertI64U => "f32.convert_i64_u",
    F32DemoteF64 => "f32.demote_f64",
    F64ConvertI32S => "f64.convert_i32_s",
    F64ConvertI32U => "f64.convert_i32_u",
    F64ConvertI64S => "f64.convert_i64_s",
    F64ConvertI64U => "f64.convert_i64_u",
    F64PromoteF32 => "f64.promote_f32",
    I32ReinterpretF32 => "i32.reinterpret_f32",
    I64ReinterpretF64 => "i64.reinterpret_f64",
    F32ReinterpretI32 => "f32.reinterpret_i32",
    F64ReinterpretI64 => "f64.reinterpret_i64",
    I32Extend8S => "i32.extend8_s",
    I32Extend16S => "i32.extend16_s",
    I64Extend8S => "i64.extend8_s",
    I64Extend16S => "i64.extend16_s",
    I64Extend32S => "i64.extend32_s",
    I32TruncSatF32S => "i32.trunc_sat_f32_s",
    I32TruncSatF32U => "i32.trunc_sat_f32_u",
    I32TruncSatF64S => "i32.trunc_sat_f64_s",
    I32TruncSatF64U => "i32.trunc_sat_f64_u",
    I64TruncSatF32S => "i64.trunc_sat_f32_s",
    I64TruncSatF32U => "i64.trunc_sat_f32_u",
    I64TruncSatF64S => "i64.trunc_sat_f64_s",
    I64TruncSatF64U => "i64.trunc_sat_f64_u",
}

/// One memory-access instruction's text facts, from the table beside [`Instr`]: its WAT spelling, the log2 alignment its access width makes natural — which the printer omits and the parser defaults to — and the memarg it carries. Opcodes are deliberately absent: they live in the encoder beside every other instruction's, so this table holds nothing a second place also holds.
pub(crate) struct MemAccess<'a> {
    pub(crate) mnemonic: &'static str,
    pub(crate) natural_align: u32,
    pub(crate) mem_arg: &'a MemArg,
}

/// Declares the memory-access instructions' WAT spellings and natural alignments from one table, the way [`mnemonics!`] declares the operand-less ones — the printer and the parser both read it, so neither can drift from the other or from the alignment defaulting rule.
macro_rules! memory_accesses {
    ($($variant:ident => $mnemonic:literal, $natural_align:literal,)+) => {
        impl Instr {
            /// This instruction's memory-access facts, or `None` for every instruction that is not a load or a store.
            pub(crate) fn mem_access(&self) -> Option<MemAccess<'_>> {
                match self {
                    $(Instr::$variant { mem_arg } => Some(MemAccess {
                        mnemonic: $mnemonic,
                        natural_align: $natural_align,
                        mem_arg,
                    }),)+
                    _ => None,
                }
            }

            /// The load or store spelled `token`, as its natural log2 alignment and the constructor awaiting its memarg — [`Instr::mem_access`]'s reverse, for the parser's whole-token dispatch.
            pub(crate) fn from_mem_mnemonic(token: &str) -> Option<(u32, fn(MemArg) -> Instr)> {
                match token {
                    $($mnemonic => Some(($natural_align, |mem_arg| Instr::$variant { mem_arg })),)+
                    _ => None,
                }
            }
        }
    };
}

memory_accesses! {
    I32Load => "i32.load", 2,
    I64Load => "i64.load", 3,
    F32Load => "f32.load", 2,
    F64Load => "f64.load", 3,
    I32Load8S => "i32.load8_s", 0,
    I32Load8U => "i32.load8_u", 0,
    I32Load16S => "i32.load16_s", 1,
    I32Load16U => "i32.load16_u", 1,
    I64Load8S => "i64.load8_s", 0,
    I64Load8U => "i64.load8_u", 0,
    I64Load16S => "i64.load16_s", 1,
    I64Load16U => "i64.load16_u", 1,
    I64Load32S => "i64.load32_s", 2,
    I64Load32U => "i64.load32_u", 2,
    I32Store => "i32.store", 2,
    I64Store => "i64.store", 3,
    F32Store => "f32.store", 2,
    F64Store => "f64.store", 3,
    I32Store8 => "i32.store8", 0,
    I32Store16 => "i32.store16", 1,
    I64Store8 => "i64.store8", 0,
    I64Store16 => "i64.store16", 1,
    I64Store32 => "i64.store32", 2,
}

/// A flat instruction sequence — a function body, or a constant expression: a global's initializer, a table's, a segment's offset, one element of an expression list. The encoder appends the terminating `end` opcode itself, so builders supply only the instructions.
///
/// One type serves both, so the constant-expression restriction is a contract on the builder rather than something this type can refuse. A constant expression must stay inside the *base* grammar — a single `t.const`, `global.get` of an immutable import, `ref.null`, `ref.func`, or GC constructor — because the extended-constant-expressions proposal sits outside the pinned envelope: Wasmtime's engine happens to accept it, `curios-binaryen`'s mask deliberately does not, so a wider expression would validate and then abort the optimizer. `optimize`'s round-trip over the emitted corpus is what detects a breach.
#[derive(Debug, Default, Clone)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}

impl Expr {
    /// Appends a single instruction.
    pub fn push(&mut self, instr: Instr) {
        self.instrs.push(instr)
    }

    /// Appends a sequence of instructions in order.
    pub fn extend<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = Instr>,
    {
        self.instrs.extend(instrs);
    }
}

impl<I> From<I> for Expr
where
    I: IntoIterator<Item = Instr>,
{
    fn from(value: I) -> Self {
        Self {
            instrs: value.into_iter().collect(),
        }
    }
}
