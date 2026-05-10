use super::{BlockName, ClsrName, FuncName, ValueName};

#[derive(Debug)]
pub enum Data {
    Unit,
    Bln(bool),
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Vec<u8>),
    Lst(Vec<ValueName>),
    Tpl(Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
}

#[derive(Debug)]
pub enum Code {
    BlnNot(ValueName),
    BlnAnd(ValueName, ValueName),
    BlnOr(ValueName, ValueName),
    BlnEql(ValueName, ValueName),
    BlnNeq(ValueName, ValueName),
    NatEql(ValueName, ValueName),
    NatNeq(ValueName, ValueName),
    NatAdd(ValueName, ValueName),
    NatSub(ValueName, ValueName),
    NatMul(ValueName, ValueName),
    NatLt(ValueName, ValueName),
    NatDiv(ValueName, ValueName),
    NatRem(ValueName, ValueName),
    NatGt(ValueName, ValueName),
    NatLte(ValueName, ValueName),
    NatGte(ValueName, ValueName),
    IntEql(ValueName, ValueName),
    IntNeq(ValueName, ValueName),
    IntAdd(ValueName, ValueName),
    IntSub(ValueName, ValueName),
    IntMul(ValueName, ValueName),
    IntNeg(ValueName),
    IntDiv(ValueName, ValueName),
    IntRem(ValueName, ValueName),
    IntLt(ValueName, ValueName),
    IntGt(ValueName, ValueName),
    IntLte(ValueName, ValueName),
    IntGte(ValueName, ValueName),
    FltAdd(ValueName, ValueName),
    FltSub(ValueName, ValueName),
    FltMul(ValueName, ValueName),
    FltDiv(ValueName, ValueName),
    FltEql(ValueName, ValueName),
    FltNeq(ValueName, ValueName),
    FltLt(ValueName, ValueName),
    FltGt(ValueName, ValueName),
    FltLte(ValueName, ValueName),
    FltGte(ValueName, ValueName),
    FltMin(ValueName, ValueName),
    FltMax(ValueName, ValueName),
    FltNeg(ValueName),
    FltAbs(ValueName),
    FltSqrt(ValueName),
    FltFloor(ValueName),
    FltCeil(ValueName),
    FltTrunc(ValueName),
    FltNearest(ValueName),
    NatToInt(ValueName),
    IntToNat(ValueName),
    IntToFlt(ValueName),
    NatToFlt(ValueName),
    FltToInt(ValueName),
    FltToNat(ValueName),
    BinLen(ValueName),
    BinGet(ValueName, ValueName),
    BinSlice(ValueName, ValueName, ValueName),
    BinConcat(ValueName, ValueName),
    LstLen(ValueName),
    LstGet(ValueName, ValueName),
    LstSlice(ValueName, ValueName, ValueName),
    LstConcat(ValueName, ValueName),
    TplProj(usize, ValueName),
}

#[derive(Debug)]
pub enum Value {
    Pure(Data),
    Eval(Code),
    Name(ValueName),
}

#[derive(Debug)]
pub struct Block {
    pub params: Vec<ValueName>,
    pub region: Region,
}

#[derive(Debug)]
pub struct JumpTarget {
    pub target: BlockName,
    pub params: Vec<ValueName>,
}

#[derive(Debug)]
pub struct CaseTarget {
    pub operand: ValueName,
    pub targets: Vec<JumpTarget>,
    pub default: Option<JumpTarget>,
}

#[derive(Debug)]
pub enum CallTarget {
    Direct {
        target: FuncName,
        params: Vec<ValueName>,
        resume: BlockName,
    },
    Indirect {
        target: ValueName,
        params: Vec<ValueName>,
        resume: BlockName,
    },
}

#[derive(Debug)]
pub enum Tail {
    Jump(JumpTarget),
    Case(CaseTarget),
    Call(CallTarget),
}

#[derive(Debug)]
pub struct Region {
    pub values: Vec<(ValueName, Value)>,
    pub blocks: Vec<(BlockName, Block)>,
    pub tail: Tail,
}

#[derive(Debug)]
pub struct Clsr {
    pub fields: Vec<ValueName>,
    pub params: Vec<ValueName>,
    pub resume: BlockName,
    pub region: Region,
}

impl Clsr {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Debug)]
pub struct Func {
    pub params: Vec<ValueName>,
    pub resume: BlockName,
    pub region: Region,
}

impl Func {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Debug, Default)]
pub struct Module {
    consts: Vec<(ValueName, Data)>,
    clsrs: Vec<(ClsrName, Clsr)>,
    funcs: Vec<(FuncName, Func)>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn consts(&self) -> &[(ValueName, Data)] {
        &self.consts
    }

    pub fn add_const(&mut self, value_name: ValueName, value: Data) {
        self.consts.push((value_name, value));
    }

    pub fn clsrs(&self) -> &[(ClsrName, Clsr)] {
        &self.clsrs
    }

    pub fn add_clsr(&mut self, clsr_name: ClsrName, clsr: Clsr) {
        self.clsrs.push((clsr_name, clsr));
    }

    pub fn funcs(&self) -> &[(FuncName, Func)] {
        &self.funcs
    }

    pub fn add_func(&mut self, func_name: FuncName, func: Func) {
        self.funcs.push((func_name, func));
    }
}
