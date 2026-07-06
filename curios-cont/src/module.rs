use {
    super::{BlockName, ClsrName, FuncName, ValueName},
    curios_abi::ForeignFunction,
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A constructed value: the scalar immediates, `Bin` bytes, `Lst`/`Tpl` aggregates, and `Clsr` naming its definition plus the captures filling its environment. Aggregates are flat — elements are names of already-bound values, never nested `Data` — so constructing one is a single allocation. `Data` is also the payload of module consts, where codegen materialises it into a wasm global.
#[derive(Debug, Clone)]
pub enum Data {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Vec<u8>),
    Lst(Vec<ValueName>),
    Tpl(Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
}

/// One pure primitive computation over already-bound values — the expression vocabulary of the IR. Every variant produces exactly one value and has no effects (anything stateful is a [`Tail`], per the IR's atomicity law), which is what licenses the optimizer to fold, dedupe, hoist, and drop `Eval` bindings freely. The scalar families (`Nat*`/`Int*`/`Flt*`) mirror the wasm ops they lower to one-for-one; the `Bin*`/`Lst*` families are rope operations codegen services through shared helper functions.
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
    IntToNat(ValueName),
    IntToFlt(ValueName),
    FltAdd(ValueName, ValueName),
    FltSub(ValueName, ValueName),
    FltMul(ValueName, ValueName),
    FltDiv(ValueName, ValueName),
    FltRem(ValueName, ValueName),
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
    FltToNat(ValueName),
    FltToLeBin(ValueName),
    FltToInt(ValueName),
    BinLen(ValueName),
    BinEql(ValueName, ValueName),
    BinGet(ValueName, ValueName),
    BinSlice(ValueName, ValueName, ValueName),
    BinAppend(ValueName, ValueName),
    BinConcat(Vec<ValueName>),
    LstLen(ValueName),
    LstGet(ValueName, ValueName),
    LstSlice(ValueName, ValueName, ValueName),
    LstAppend(ValueName, ValueName),
    LstConcat(Vec<ValueName>),
    // `LstMap(src, f)`: map closure `f` over array `src` into a fresh array of
    // the same length. Codegen lowers it to the shared `$lst/map` rope helper:
    // one allocation, one fill loop applying `f` per slot via `call_ref`.
    LstMap(ValueName, ValueName),
    TplGet(ValueName, usize),
}

/// The right-hand side of one binding in a region: `Pure` constructs [`Data`], `Eval` computes one [`Code`] op, and `Alias` re-binds an existing value under another name — a rename that passes like the ones common-subexpression elimination mints and copy propagation erases again, so downstream passes see a value's real identity.
#[derive(Debug, Clone)]
pub enum Value {
    Pure(Data),
    Eval(Code),
    Alias(ValueName),
}

/// A labeled join point inside a region: `params` are its block parameters — the SSA replacement for φ-nodes, bound afresh on every entry from a [`JumpTarget`] — and `region` its body. Blocks are also how calls continue: a call's `resume` names the block that receives its result, and a backward jump to a loop header block is how `tail_recursion`'s loops close.
#[derive(Debug, Clone)]
pub struct Block {
    pub params: Vec<ValueName>,
    pub region: Region,
}

/// One branch edge: the destination block plus the values feeding its parameters, positionally. This is the IR's φ-mechanism — a block parameter's value is whatever the taken edge passed for it.
#[derive(Debug, Clone)]
pub struct JumpTarget {
    pub target: BlockName,
    pub params: Vec<ValueName>,
}

/// A multi-way branch on a scalar: `operand` is read as an unsigned `u32` (a constructor tag or nat), each case maps one value to its edge, and `default` catches the rest — `None` means the cases are exhaustive, and a match with no cases and no default lowers to a trap. Codegen picks the dispatch shape (a `br_table` for dense-from-zero tags, an `if` for two-way, a binary search otherwise); the IR just states the table.
#[derive(Debug, Clone)]
pub struct MatchTarget {
    pub operand: ValueName,
    pub cases: BTreeMap<u32, JumpTarget>,
    pub default: Option<JumpTarget>,
}

/// A user-code call in tail position: `Direct` names a known function, `Indirect` invokes a closure value through the shared closure type of its arity (`call_ref`). Both name the `resume` block, which receives the callee's single result as its one parameter — except when `resume` is the enclosing body's return sentinel, where the call is a genuine tail call and codegen emits `return_call`/`return_call_ref` instead of branching.
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
    /// A store-described host call: `function`'s [`WireSignature`] fixes the
    /// operand order/types and the resume shape — `resume` takes one block
    /// parameter per signature result (the multi-result records arrive as
    /// parallel block parameters, exactly like the per-op variants did).
    ///
    /// [`WireSignature`]: curios_abi::WireSignature
    Foreign {
        function: Arc<ForeignFunction>,
        operands: Vec<ValueName>,
        resume: BlockName,
    },
    /// Terminate the process with exit `code`. The host traps, so the resume is
    /// never reached; it is kept (taking zero block parameters) only so the
    /// uniform `Tail::Host { resume }` shape holds.
    IoExit { code: ValueName, resume: BlockName },
}

impl HostTarget {
    pub(crate) fn resume(&self) -> &BlockName {
        match self {
            HostTarget::Foreign { resume, .. } | HostTarget::IoExit { resume, .. } => resume,
        }
    }

    pub(crate) fn resume_mut(&mut self) -> &mut BlockName {
        match self {
            HostTarget::Foreign { resume, .. } | HostTarget::IoExit { resume, .. } => resume,
        }
    }

    /// The value operands this host op reads, in argument order.
    pub(crate) fn operands(&self) -> Vec<&ValueName> {
        match self {
            HostTarget::Foreign { operands, .. } => operands.iter().collect(),
            HostTarget::IoExit { code, .. } => vec![code],
        }
    }

    /// The value operands this host op reads, as mutable references, in
    /// argument order.
    pub(crate) fn operands_mut(&mut self) -> Vec<&mut ValueName> {
        match self {
            HostTarget::Foreign { operands, .. } => operands.iter_mut().collect(),
            HostTarget::IoExit { code, .. } => vec![code],
        }
    }
}

/// A guest mutable-cell op in tail position. Same `resume` discipline as
/// `HostTarget`, but serviced inline in codegen (no host import). Purity
/// analysis treats any `Tail::Cell` as an impure boundary, like `Host`.
#[derive(Debug, Clone)]
pub enum CellTarget {
    New {
        init: ValueName,
        resume: BlockName,
    },
    Set {
        cell: ValueName,
        value: ValueName,
        resume: BlockName,
    },
    Get {
        cell: ValueName,
        resume: BlockName,
    },
}

impl CellTarget {
    pub(crate) fn resume(&self) -> &BlockName {
        match self {
            CellTarget::New { resume, .. }
            | CellTarget::Set { resume, .. }
            | CellTarget::Get { resume, .. } => resume,
        }
    }

    pub(crate) fn resume_mut(&mut self) -> &mut BlockName {
        match self {
            CellTarget::New { resume, .. }
            | CellTarget::Set { resume, .. }
            | CellTarget::Get { resume, .. } => resume,
        }
    }

    pub(crate) fn operands(&self) -> Vec<&ValueName> {
        match self {
            CellTarget::New { init, .. } => vec![init],
            CellTarget::Set { cell, value, .. } => vec![cell, value],
            CellTarget::Get { cell, .. } => vec![cell],
        }
    }

    pub(crate) fn operands_mut(&mut self) -> Vec<&mut ValueName> {
        match self {
            CellTarget::New { init, .. } => vec![init],
            CellTarget::Set { cell, value, .. } => vec![cell, value],
            CellTarget::Get { cell, .. } => vec![cell],
        }
    }
}

/// The sole control transfer out of a region — a region never falls through. `Jump` and `Match` stay within the body; `Call` transfers to user code; `Host` and `Cell` are the effectful primitives (the only impurity the IR admits — purity analysis marks a region tree impure exactly when one appears in it); `Unreachable` traps, marking a path that cannot be taken (an absurd match).
#[derive(Debug, Clone)]
pub enum Tail {
    Jump(JumpTarget),
    Match(MatchTarget),
    Call(CallTarget),
    Host(HostTarget),
    Cell(CellTarget),
    Unreachable,
}

/// One straight-line body fragment and the sub-structure hanging off it: bindings evaluated in order, the labeled join blocks its jumps enter, and the single [`Tail`] transfer that ends it. All control flow in a body lives in its region tree — a [`Value`] binding never branches, which is what the optimizer's freedom to fold, dedupe, and reorder bindings rests on.
#[derive(Debug, Clone)]
pub struct Region {
    /// Closure shells reserved before their captures are filled, so a self- or
    /// mutually-recursive capture can name the shell. Only closures need this; cyclic
    /// tuples/arrays are rejected upstream (`to_cont`), which keeps `tpl`/`lst` immutable.
    pub preallocs: Vec<(ValueName, ClsrName)>,
    pub values: Vec<(ValueName, Value)>,
    pub blocks: Vec<(BlockName, Block)>,
    pub tail: Tail,
}

impl Region {
    /// Collect the arity of every *indirect* call site in this region (and its nested blocks).
    /// A closure of that arity is invoked here even when the optimizer has specialized its
    /// definition away (a higher-order function's argument inlined, dropping the only closure
    /// of that arity while a `call_ref` in its body survives), so a closure type for the arity
    /// is needed even though no closure of it is defined.
    fn collect_indirect_arities(&self, out: &mut BTreeSet<usize>) {
        if let Tail::Call(CallTarget::Indirect { params, .. }) = &self.tail {
            out.insert(params.len());
        }

        for (_, block) in &self.blocks {
            block.region.collect_indirect_arities(out);
        }
    }
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

/// Compare an argument to a bare name (candidate-agnostic) — handy in tests that
/// assert on parameter lists without caring about the flag.
impl PartialEq<ValueName> for Argument {
    fn eq(&self, other: &ValueName) -> bool {
        &self.name == other
    }
}

/// A closure definition: `fields` is the captured environment — filled where a `Data::Clsr` names this definition, read inside the body like extra bindings — and `params` the call arguments. `resume` is the return sentinel, exactly as on [`Func`]. Codegen gives each definition its own environment struct type but files its function under the shared closure type for its arity, which is what lets a [`CallTarget::Indirect`] call any closure of matching arity.
#[derive(Debug, Clone)]
pub struct Clsr {
    pub fields: Vec<Argument>,
    pub params: Vec<Argument>,
    pub resume: BlockName,
    pub region: Region,
}

/// A top-level function: parameters, body, and its `resume` sentinel — the block name that means "return": a jump to `resume` with one value *is* the function's return, so a call whose resume block is the enclosing sentinel is a tail call by construction (codegen emits `return_call`), no separate tail-position analysis needed.
#[derive(Debug, Clone)]
pub struct Func {
    pub params: Vec<Argument>,
    pub resume: BlockName,
    pub region: Region,
}

/// One whole program in cont form: module-level consts (hoisted literals), the closure and function definitions, and the blessed entrypoint. Bodies reference consts and each other by name, and definition order is preserved end-to-end — it is the printed order and the wasm emission order. The collections are private: from outside the crate a module only grows through the `add_*` methods, while the mutating views the optimizer rewrites through stay crate-internal.
#[derive(Debug, Default)]
pub struct Module {
    consts: Vec<(ValueName, Data)>,
    clsrs: Vec<(ClsrName, Clsr)>,
    funcs: Vec<(FuncName, Func)>,
    entry: Option<FuncName>,
}

impl Module {
    /// An empty module: no definitions, no entrypoint.
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn consts(&self) -> &[(ValueName, Data)] {
        &self.consts
    }

    pub(crate) fn consts_mut(&mut self) -> &mut Vec<(ValueName, Data)> {
        &mut self.consts
    }

    /// Append a module-level const: a [`Data`] binding every body can reference by name. In practice only the optimizer's literal hoisting mints these; codegen materialises each into a wasm global.
    pub fn add_const(&mut self, value_name: ValueName, value: Data) {
        self.consts.push((value_name, value));
    }

    /// The closure definitions, in insertion order.
    pub fn clsrs(&self) -> &[(ClsrName, Clsr)] {
        &self.clsrs
    }

    pub(crate) fn clsrs_mut(&mut self) -> &mut Vec<(ClsrName, Clsr)> {
        &mut self.clsrs
    }

    /// Append a closure definition under its name; `Data::Clsr` values cite that name to instantiate it.
    pub fn add_clsr(&mut self, clsr_name: ClsrName, clsr: Clsr) {
        self.clsrs.push((clsr_name, clsr));
    }

    /// The function definitions, in insertion order — the order they print and emit in.
    pub fn funcs(&self) -> &[(FuncName, Func)] {
        &self.funcs
    }

    pub(crate) fn funcs_mut(&mut self) -> &mut Vec<(FuncName, Func)> {
        &mut self.funcs
    }

    /// Append a function definition under its name; the position it lands in is the position it keeps.
    pub fn add_func(&mut self, func_name: FuncName, func: Func) {
        self.funcs.push((func_name, func));
    }

    /// Every closure arity the module needs closure types for: the arities of the surviving
    /// closure definitions, unioned with the arities of indirect call sites (whose target
    /// definition may have been inlined away). Sizing closure types from definitions alone
    /// misses the latter, leaving a surviving `call_ref` with no declared type for its arity.
    pub(crate) fn clsr_arities(&self) -> BTreeSet<usize> {
        let mut arities = BTreeSet::new();

        for (_, clsr) in &self.clsrs {
            arities.insert(clsr.params.len());
            clsr.region.collect_indirect_arities(&mut arities);
        }

        for (_, func) in &self.funcs {
            func.region.collect_indirect_arities(&mut arities);
        }

        arities
    }

    /// The entrypoint function — the program's sole root: the value the host
    /// invokes, the only export, and the seed of dead-code reachability. Recorded
    /// here so passes consult the module instead of re-deriving a blessed name.
    pub fn entry(&self) -> Option<&FuncName> {
        self.entry.as_ref()
    }

    /// Bless `func_name` as the entrypoint. Set once by `to_cont` (always `main`); the module does not check the function exists — the lowerer adds it separately.
    pub fn set_entry(&mut self, func_name: FuncName) {
        self.entry = Some(func_name);
    }
}
