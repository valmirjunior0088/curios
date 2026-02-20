use super::{BlockName, ClsrName, FuncName, ValueName};

#[derive(Debug)]
pub enum ConstValue {
    Int(i32),
    Flt(f32),
}

#[derive(Debug)]
pub enum ConstOp {
    IntEql,
    IntAdd,
    IntSub,
    IntMul,
    FltAdd,
    FltSub,
    FltMul,
}

#[derive(Debug)]
pub enum Value {
    Pure(ConstValue),
    Eval(ConstOp, Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
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
    consts: Vec<(ValueName, ConstValue)>,
    clsrs: Vec<(ClsrName, Clsr)>,
    funcs: Vec<(FuncName, Func)>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn consts(&self) -> &[(ValueName, ConstValue)] {
        &self.consts
    }

    pub fn add_const(&mut self, value_name: ValueName, value: ConstValue) {
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
