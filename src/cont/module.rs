use {
    super::{BlockName, ClsrName, FuncName, ValueName},
    std::collections::BTreeMap,
};

#[derive(Debug, Clone)]
pub enum Data {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Vec<u8>),
    Arr(Vec<ValueName>),
    Tpl(Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
}

#[derive(Debug, Clone)]
pub enum Code {
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
    NatAnd(ValueName, ValueName),
    NatOr(ValueName, ValueName),
    NatXor(ValueName, ValueName),
    NatShl(ValueName, ValueName),
    NatShr(ValueName, ValueName),
    NatRotl(ValueName, ValueName),
    NatRotr(ValueName, ValueName),
    NatClz(ValueName),
    NatCtz(ValueName),
    NatPopcnt(ValueName),
    NatEqz(ValueName),
    NatToStr(ValueName),
    NatToInt(ValueName),
    NatToFlt(ValueName),
    IntEql(ValueName, ValueName),
    IntNeq(ValueName, ValueName),
    IntAdd(ValueName, ValueName),
    IntSub(ValueName, ValueName),
    IntMul(ValueName, ValueName),
    IntDiv(ValueName, ValueName),
    IntRem(ValueName, ValueName),
    IntLt(ValueName, ValueName),
    IntGt(ValueName, ValueName),
    IntLte(ValueName, ValueName),
    IntGte(ValueName, ValueName),
    IntAnd(ValueName, ValueName),
    IntOr(ValueName, ValueName),
    IntXor(ValueName, ValueName),
    IntShl(ValueName, ValueName),
    IntShr(ValueName, ValueName),
    IntRotl(ValueName, ValueName),
    IntRotr(ValueName, ValueName),
    IntClz(ValueName),
    IntCtz(ValueName),
    IntPopcnt(ValueName),
    IntEqz(ValueName),
    IntToStr(ValueName),
    IntToNat(ValueName),
    IntToFlt(ValueName),
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
    FltCopysign(ValueName, ValueName),
    FltToStr(ValueName),
    FltToNat(ValueName),
    FltToInt(ValueName),
    BinLen(ValueName),
    BinEql(ValueName, ValueName),
    BinGet(ValueName, ValueName),
    BinSlice(ValueName, ValueName, ValueName),
    BinAppend(ValueName, ValueName),
    BinConcat(Vec<ValueName>),
    ArrLen(ValueName),
    ArrGet(ValueName, ValueName),
    ArrSlice(ValueName, ValueName, ValueName),
    ArrAppend(ValueName, ValueName),
    ArrConcat(Vec<ValueName>),
    TplGet(ValueName, usize),
}

#[derive(Debug, Clone)]
pub enum Value {
    Pure(Data),
    Eval(Code),
    Alias(ValueName),
}

#[derive(Debug, Clone)]
pub enum Prealloc {
    Tpl(usize),
    Arr(usize),
    Clsr(ClsrName),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub params: Vec<ValueName>,
    pub region: Region,
}

#[derive(Debug, Clone)]
pub struct JumpTarget {
    pub target: BlockName,
    pub params: Vec<ValueName>,
}

#[derive(Debug, Clone)]
pub struct MatchTarget {
    pub operand: ValueName,
    pub cases: BTreeMap<u32, JumpTarget>,
    pub default: Option<JumpTarget>,
}

#[derive(Debug, Clone)]
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

/// A host-provided primitive call in tail position. Mirrors [`CallTarget`] for
/// user calls: every variant carries its own operands plus the `resume` block
/// to branch to once the host returns. Purity analysis treats any `Tail::Host`
/// as the impure boundary of its enclosing region tree.
#[derive(Debug, Clone)]
pub enum HostTarget {
    /// Print `value` (a `Bin`) to the host. Returns no payload; `resume` takes
    /// zero block parameters.
    IoPrint {
        value: ValueName,
        resume: BlockName,
    },
    /// Read a line of input from the host. Returns one `Bin`; `resume` takes
    /// a single block parameter bound to that `Bin`.
    IoRead { resume: BlockName },
}

impl HostTarget {
    pub fn resume(&self) -> &BlockName {
        match self {
            HostTarget::IoPrint { resume, .. } | HostTarget::IoRead { resume } => resume,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Tail {
    Jump(JumpTarget),
    Match(MatchTarget),
    Call(CallTarget),
    Host(HostTarget),
}

#[derive(Debug, Clone)]
pub struct Region {
    pub preallocs: Vec<(ValueName, Prealloc)>,
    pub values: Vec<(ValueName, Value)>,
    pub blocks: Vec<(BlockName, Block)>,
    pub tail: Tail,
}

/// A function or closure argument: its bound name, plus whether it is a
/// specialization *candidate* — i.e. its erased type was a function (a first-class
/// closure value), a `Type`, or unit, each a compile-time constant the specializer
/// can bake in. The flag is computed once by type-directed erasure and glued to
/// the name here, so it can never desync from it and rides along for free on every
/// `.clone()`, `retain`, and capture-to-parameter move.
#[derive(Debug, Clone)]
pub struct Argument {
    pub name: ValueName,
    pub candidate: bool,
}

impl From<ValueName> for Argument {
    fn from(name: ValueName) -> Self {
        Self {
            name,
            candidate: false,
        }
    }
}

impl Argument {
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

/// Compare an argument to a bare name (candidate-agnostic) — handy in tests that
/// assert on parameter lists without caring about the flag.
impl PartialEq<ValueName> for Argument {
    fn eq(&self, other: &ValueName) -> bool {
        &self.name == other
    }
}

#[derive(Debug, Clone)]
pub struct Clsr {
    pub fields: Vec<Argument>,
    pub params: Vec<Argument>,
    pub resume: BlockName,
    pub region: Region,
}

impl Clsr {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub params: Vec<Argument>,
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
    entry: Option<FuncName>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn consts(&self) -> &[(ValueName, Data)] {
        &self.consts
    }

    pub fn consts_mut(&mut self) -> &mut Vec<(ValueName, Data)> {
        &mut self.consts
    }

    pub fn add_const(&mut self, value_name: ValueName, value: Data) {
        self.consts.push((value_name, value));
    }

    pub fn clsrs(&self) -> &[(ClsrName, Clsr)] {
        &self.clsrs
    }

    pub fn clsrs_mut(&mut self) -> &mut Vec<(ClsrName, Clsr)> {
        &mut self.clsrs
    }

    pub fn add_clsr(&mut self, clsr_name: ClsrName, clsr: Clsr) {
        self.clsrs.push((clsr_name, clsr));
    }

    pub fn funcs(&self) -> &[(FuncName, Func)] {
        &self.funcs
    }

    pub fn funcs_mut(&mut self) -> &mut Vec<(FuncName, Func)> {
        &mut self.funcs
    }

    pub fn add_func(&mut self, func_name: FuncName, func: Func) {
        self.funcs.push((func_name, func));
    }

    /// The entrypoint function — the program's sole root: the value the host
    /// invokes, the only export, and the seed of dead-code reachability. Recorded
    /// here so passes consult the module instead of re-deriving a blessed name.
    pub fn entry(&self) -> Option<&FuncName> {
        self.entry.as_ref()
    }

    pub fn set_entry(&mut self, func_name: FuncName) {
        self.entry = Some(func_name);
    }
}
