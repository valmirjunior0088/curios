use super::{BlockName, ClsrName, FuncName, ValueName};

#[derive(Debug)]
pub enum Data {
    Unit,
    Bln(bool),
    Nat(u32),
    Int(i32),
    Flt(f32),
    Lst(Vec<ValueName>),
    Tpl(Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
}

#[derive(Debug)]
pub enum Code {
    BlnNot,
    BlnAnd,
    BlnOr,
    BlnEql,
    BlnNeq,
    NatEql,
    NatNeq,
    NatAdd,
    NatSub,
    NatMul,
    NatLt,
    NatDiv,
    NatRem,
    NatGt,
    NatLte,
    NatGte,
    IntEql,
    IntNeq,
    IntAdd,
    IntSub,
    IntMul,
    IntNeg,
    IntDiv,
    IntRem,
    IntLt,
    IntGt,
    IntLte,
    IntGte,
    FltAdd,
    FltSub,
    FltMul,
    FltDiv,
    FltEql,
    FltNeq,
    FltLt,
    FltGt,
    FltLte,
    FltGte,
    FltMin,
    FltMax,
    FltNeg,
    FltAbs,
    FltSqrt,
    FltFloor,
    FltCeil,
    FltTrunc,
    FltNearest,
    NatToInt,
    IntToNat,
    IntToFlt,
    NatToFlt,
    FltToInt,
    FltToNat,
    LstLen,
    LstGet,
    LstSlice,
    LstConcat,
    TplProj(usize),
}

#[derive(Debug)]
pub enum Value {
    Pure(Data),
    Eval(Code, Vec<ValueName>),
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
