use super::{
    BlockType, FieldName, FuncName, GlobalName, HeapType, LabelName, LocalName, RefType, TypeName,
    ValType,
};

#[derive(Debug)]
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
    ReturnCall {
        func_name: FuncName,
    },
    ReturnCallRef {
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
        source_name: TypeName,
        target_name: TypeName,
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

#[derive(Debug, Default)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}

impl Expr {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, instr: Instr) {
        self.instrs.push(instr)
    }

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
