use {
    super::{BlockName, ClsrName, FuncName, ValueName},
    std::collections::{BTreeMap, BTreeSet},
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
    FltToLeBin(ValueName),
    FltToInt(ValueName),
    BinLen(ValueName),
    BinEql(ValueName, ValueName),
    BinGet(ValueName, ValueName),
    BinSlice(ValueName, ValueName, ValueName),
    BinAppend(ValueName, ValueName),
    BinConcat(Vec<ValueName>),
    BinFlatten(ValueName),
    ArrLen(ValueName),
    ArrGet(ValueName, ValueName),
    ArrSlice(ValueName, ValueName, ValueName),
    ArrAppend(ValueName, ValueName),
    ArrConcat(Vec<ValueName>),
    ArrFlatten(ValueName),
    TplGet(ValueName, usize),
}

#[derive(Debug, Clone)]
pub enum Value {
    Pure(Data),
    Eval(Code),
    Alias(ValueName),
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
    /// Read up to `count` bytes from `handle`. Returns (status, bytes);
    /// `resume` takes two block parameters.
    IoRead {
        handle: ValueName,
        count: ValueName,
        resume: BlockName,
    },
    /// Write `bytes` (a `Bin`) to `handle`. Returns (status, written);
    /// `resume` takes two block parameters.
    IoWrite {
        handle: ValueName,
        bytes: ValueName,
        resume: BlockName,
    },
    /// Open the file at `path` (a `Bin`) with `mode` (an i32 token). Returns
    /// (status, handle); `resume` takes two block parameters.
    IoOpen {
        path: ValueName,
        mode: ValueName,
        resume: BlockName,
    },
    /// Resolve `host`:`port` (a `Bin` and an i32) to a list of opaque address
    /// blobs. Returns (status, addresses : Arr(Bin)); `resume` takes two block
    /// parameters.
    IoResolve {
        host: ValueName,
        port: ValueName,
        resume: BlockName,
    },
    /// Create an unconnected socket for the address blob `addr`. Returns
    /// (status, handle); `resume` takes two block parameters.
    IoSocket {
        addr: ValueName,
        resume: BlockName,
    },
    /// Bind socket `handle` to local address `addr`. Returns the status scalar;
    /// `resume` takes one block parameter.
    IoBind {
        handle: ValueName,
        addr: ValueName,
        resume: BlockName,
    },
    /// Connect socket `handle` to the resolved address `addr`. Returns the
    /// status scalar; `resume` takes one block parameter.
    IoConnect {
        handle: ValueName,
        addr: ValueName,
        resume: BlockName,
    },
    /// Mark bound socket `handle` as listening with accept-queue depth
    /// `backlog`. Returns the status scalar; `resume` takes one block parameter.
    IoListen {
        handle: ValueName,
        backlog: ValueName,
        resume: BlockName,
    },
    /// Pull the next connection from the listener `handle`. Returns
    /// (status, handle); `resume` takes two block parameters.
    IoAccept {
        handle: ValueName,
        resume: BlockName,
    },
    /// Upgrade connected socket `handle` to a TLS client stream in place using
    /// the server name `sni` (a `Bin`). Returns the status scalar; `resume`
    /// takes one block parameter.
    IoStartTls {
        handle: ValueName,
        sni: ValueName,
        resume: BlockName,
    },
    /// Build an opaque server TLS config from `cert` and `key` (both `Bin`).
    /// Returns (status, handle); `resume` takes two block parameters.
    IoTlsServerConfig {
        cert: ValueName,
        key: ValueName,
        resume: BlockName,
    },
    /// Upgrade accepted socket `handle` to a TLS server stream in place using
    /// the config token `cfg`. Returns the status scalar; `resume` takes one
    /// block parameter.
    IoStartTlsServer {
        handle: ValueName,
        cfg: ValueName,
        resume: BlockName,
    },
    /// Set socket `handle`'s non-blocking flag `on` (a `Bln`). Returns the
    /// status scalar; `resume` takes one block parameter.
    IoSetNonblocking {
        handle: ValueName,
        on: ValueName,
        resume: BlockName,
    },
    /// Set socket `handle`'s receive timeout to `ms` milliseconds (`0` clears).
    /// Returns the status scalar; `resume` takes one block parameter.
    IoSetRecvTimeout {
        handle: ValueName,
        ms: ValueName,
        resume: BlockName,
    },
    /// Set socket `handle`'s send timeout to `ms` milliseconds (`0` clears).
    /// Returns the status scalar; `resume` takes one block parameter.
    IoSetSendTimeout {
        handle: ValueName,
        ms: ValueName,
        resume: BlockName,
    },
    /// Set socket `handle`'s SO_REUSEADDR flag `on` (a `Bln`). Returns the
    /// status scalar; `resume` takes one block parameter.
    IoSetReuseaddr {
        handle: ValueName,
        on: ValueName,
        resume: BlockName,
    },
    /// Poll `handles` (an `Arr(Io)`) for the parallel `events` (an `Arr(Nat)` of
    /// interest masks), waiting up to `timeout` (an `Int`: `<0` forever, `0`
    /// immediate, `>0` ms). Returns the parallel `Arr(Nat)` of revents directly;
    /// `resume` takes one block parameter.
    IoPoll {
        handles: ValueName,
        events: ValueName,
        timeout: ValueName,
        resume: BlockName,
    },
    /// Close `handle`. Returns no payload; `resume` takes zero block
    /// parameters.
    IoClose {
        handle: ValueName,
        resume: BlockName,
    },
    /// Read the wall clock. Ambient — no operands. Returns
    /// (secs_hi, secs_lo, nanos); `resume` takes three block parameters.
    IoClockWall { resume: BlockName },
    /// Read the monotonic clock. Ambient. Returns (secs, nanos);
    /// `resume` takes two block parameters.
    IoClockMono { resume: BlockName },
    /// Fill `count` random bytes. Returns a `Bin`; `resume` takes one
    /// block parameter.
    IoRandom { count: ValueName, resume: BlockName },
    /// Read the process arguments. Ambient — no operands. Returns the
    /// `Arr(Bin)` directly; `resume` takes one block parameter.
    IoArgs { resume: BlockName },
    /// Look up the environment variable `name` (a `Bin`). Returns
    /// (status, value); `resume` takes two block parameters.
    IoEnv { name: ValueName, resume: BlockName },
    /// Terminate the process with exit `code`. The host traps, so the resume is
    /// never reached; it is kept (taking zero block parameters) only so the
    /// uniform `Tail::Host { resume }` shape holds.
    IoExit { code: ValueName, resume: BlockName },
}

impl HostTarget {
    pub fn resume(&self) -> &BlockName {
        match self {
            HostTarget::IoRead { resume, .. }
            | HostTarget::IoWrite { resume, .. }
            | HostTarget::IoOpen { resume, .. }
            | HostTarget::IoResolve { resume, .. }
            | HostTarget::IoSocket { resume, .. }
            | HostTarget::IoBind { resume, .. }
            | HostTarget::IoConnect { resume, .. }
            | HostTarget::IoListen { resume, .. }
            | HostTarget::IoAccept { resume, .. }
            | HostTarget::IoStartTls { resume, .. }
            | HostTarget::IoTlsServerConfig { resume, .. }
            | HostTarget::IoStartTlsServer { resume, .. }
            | HostTarget::IoSetNonblocking { resume, .. }
            | HostTarget::IoSetRecvTimeout { resume, .. }
            | HostTarget::IoSetSendTimeout { resume, .. }
            | HostTarget::IoSetReuseaddr { resume, .. }
            | HostTarget::IoPoll { resume, .. }
            | HostTarget::IoClose { resume, .. }
            | HostTarget::IoClockWall { resume }
            | HostTarget::IoClockMono { resume }
            | HostTarget::IoRandom { resume, .. }
            | HostTarget::IoArgs { resume }
            | HostTarget::IoEnv { resume, .. }
            | HostTarget::IoExit { resume, .. } => resume,
        }
    }

    pub fn resume_mut(&mut self) -> &mut BlockName {
        match self {
            HostTarget::IoRead { resume, .. }
            | HostTarget::IoWrite { resume, .. }
            | HostTarget::IoOpen { resume, .. }
            | HostTarget::IoResolve { resume, .. }
            | HostTarget::IoSocket { resume, .. }
            | HostTarget::IoBind { resume, .. }
            | HostTarget::IoConnect { resume, .. }
            | HostTarget::IoListen { resume, .. }
            | HostTarget::IoAccept { resume, .. }
            | HostTarget::IoStartTls { resume, .. }
            | HostTarget::IoTlsServerConfig { resume, .. }
            | HostTarget::IoStartTlsServer { resume, .. }
            | HostTarget::IoSetNonblocking { resume, .. }
            | HostTarget::IoSetRecvTimeout { resume, .. }
            | HostTarget::IoSetSendTimeout { resume, .. }
            | HostTarget::IoSetReuseaddr { resume, .. }
            | HostTarget::IoPoll { resume, .. }
            | HostTarget::IoClose { resume, .. }
            | HostTarget::IoClockWall { resume }
            | HostTarget::IoClockMono { resume }
            | HostTarget::IoRandom { resume, .. }
            | HostTarget::IoArgs { resume }
            | HostTarget::IoEnv { resume, .. }
            | HostTarget::IoExit { resume, .. } => resume,
        }
    }

    /// The value operands this host op reads, in argument order.
    pub fn operands(&self) -> Vec<&ValueName> {
        match self {
            HostTarget::IoRead { handle, count, .. } => vec![handle, count],
            HostTarget::IoWrite { handle, bytes, .. } => vec![handle, bytes],
            HostTarget::IoOpen { path, mode, .. } => vec![path, mode],
            HostTarget::IoResolve { host, port, .. } => vec![host, port],
            HostTarget::IoSocket { addr, .. } => vec![addr],
            HostTarget::IoBind { handle, addr, .. } => vec![handle, addr],
            HostTarget::IoConnect { handle, addr, .. } => vec![handle, addr],
            HostTarget::IoListen {
                handle, backlog, ..
            } => vec![handle, backlog],
            HostTarget::IoAccept { handle, .. } => vec![handle],
            HostTarget::IoStartTls { handle, sni, .. } => vec![handle, sni],
            HostTarget::IoTlsServerConfig { cert, key, .. } => vec![cert, key],
            HostTarget::IoStartTlsServer { handle, cfg, .. } => vec![handle, cfg],
            HostTarget::IoSetNonblocking { handle, on, .. } => vec![handle, on],
            HostTarget::IoSetRecvTimeout { handle, ms, .. } => vec![handle, ms],
            HostTarget::IoSetSendTimeout { handle, ms, .. } => vec![handle, ms],
            HostTarget::IoSetReuseaddr { handle, on, .. } => vec![handle, on],
            HostTarget::IoPoll {
                handles,
                events,
                timeout,
                ..
            } => vec![handles, events, timeout],
            HostTarget::IoClose { handle, .. } => vec![handle],
            HostTarget::IoClockWall { .. }
            | HostTarget::IoClockMono { .. }
            | HostTarget::IoArgs { .. } => vec![],
            HostTarget::IoRandom { count, .. } => vec![count],
            HostTarget::IoEnv { name, .. } => vec![name],
            HostTarget::IoExit { code, .. } => vec![code],
        }
    }

    /// The value operands this host op reads, as mutable references, in
    /// argument order.
    pub fn operands_mut(&mut self) -> Vec<&mut ValueName> {
        match self {
            HostTarget::IoRead { handle, count, .. } => vec![handle, count],
            HostTarget::IoWrite { handle, bytes, .. } => vec![handle, bytes],
            HostTarget::IoOpen { path, mode, .. } => vec![path, mode],
            HostTarget::IoResolve { host, port, .. } => vec![host, port],
            HostTarget::IoSocket { addr, .. } => vec![addr],
            HostTarget::IoBind { handle, addr, .. } => vec![handle, addr],
            HostTarget::IoConnect { handle, addr, .. } => vec![handle, addr],
            HostTarget::IoListen {
                handle, backlog, ..
            } => vec![handle, backlog],
            HostTarget::IoAccept { handle, .. } => vec![handle],
            HostTarget::IoStartTls { handle, sni, .. } => vec![handle, sni],
            HostTarget::IoTlsServerConfig { cert, key, .. } => vec![cert, key],
            HostTarget::IoStartTlsServer { handle, cfg, .. } => vec![handle, cfg],
            HostTarget::IoSetNonblocking { handle, on, .. } => vec![handle, on],
            HostTarget::IoSetRecvTimeout { handle, ms, .. } => vec![handle, ms],
            HostTarget::IoSetSendTimeout { handle, ms, .. } => vec![handle, ms],
            HostTarget::IoSetReuseaddr { handle, on, .. } => vec![handle, on],
            HostTarget::IoPoll {
                handles,
                events,
                timeout,
                ..
            } => vec![handles, events, timeout],
            HostTarget::IoClose { handle, .. } => vec![handle],
            HostTarget::IoClockWall { .. }
            | HostTarget::IoClockMono { .. }
            | HostTarget::IoArgs { .. } => vec![],
            HostTarget::IoRandom { count, .. } => vec![count],
            HostTarget::IoEnv { name, .. } => vec![name],
            HostTarget::IoExit { code, .. } => vec![code],
        }
    }
}

/// A guest mutable-cell op in tail position. Same `resume` discipline as
/// `HostTarget`, but serviced inline in codegen (no host import). Purity
/// analysis treats any `Tail::Cell` as an impure boundary, like `Host`.
#[derive(Debug, Clone)]
pub enum CellTarget {
    New { init: ValueName, resume: BlockName },
    Set { cell: ValueName, value: ValueName, resume: BlockName },
    Get { cell: ValueName, resume: BlockName },
}

impl CellTarget {
    pub fn resume(&self) -> &BlockName {
        match self {
            CellTarget::New { resume, .. }
            | CellTarget::Set { resume, .. }
            | CellTarget::Get { resume, .. } => resume,
        }
    }

    pub fn resume_mut(&mut self) -> &mut BlockName {
        match self {
            CellTarget::New { resume, .. }
            | CellTarget::Set { resume, .. }
            | CellTarget::Get { resume, .. } => resume,
        }
    }

    pub fn operands(&self) -> Vec<&ValueName> {
        match self {
            CellTarget::New { init, .. } => vec![init],
            CellTarget::Set { cell, value, .. } => vec![cell, value],
            CellTarget::Get { cell, .. } => vec![cell],
        }
    }

    pub fn operands_mut(&mut self) -> Vec<&mut ValueName> {
        match self {
            CellTarget::New { init, .. } => vec![init],
            CellTarget::Set { cell, value, .. } => vec![cell, value],
            CellTarget::Get { cell, .. } => vec![cell],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Tail {
    Jump(JumpTarget),
    Match(MatchTarget),
    Call(CallTarget),
    Host(HostTarget),
    Cell(CellTarget),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Region {
    /// Closure shells reserved before their captures are filled, so a self- or
    /// mutually-recursive capture can name the shell. Only closures need this; cyclic
    /// tuples/arrays are rejected upstream (`to_cont`), which keeps `tpl`/`arr` immutable.
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

    /// Every closure arity the module needs closure types for: the arities of the surviving
    /// closure definitions, unioned with the arities of indirect call sites (whose target
    /// definition may have been inlined away). Sizing closure types from definitions alone
    /// misses the latter, leaving a surviving `call_ref` with no declared type for its arity.
    pub fn clsr_arities(&self) -> BTreeSet<usize> {
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

    pub fn set_entry(&mut self, func_name: FuncName) {
        self.entry = Some(func_name);
    }
}
