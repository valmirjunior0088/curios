use {super::Term, std::collections::BTreeSet};

#[derive(Debug)]
pub enum PurePrim {
    Nat(u32),
    NatEql(Term, Term),
    NatNeq(Term, Term),
    NatAdd(Term, Term),
    NatSub(Term, Term),
    NatMul(Term, Term),
    NatLt(Term, Term),
    NatDiv(Term, Term),
    NatRem(Term, Term),
    NatGt(Term, Term),
    NatLte(Term, Term),
    NatGte(Term, Term),
    NatAnd(Term, Term),
    NatOr(Term, Term),
    NatXor(Term, Term),
    NatShl(Term, Term),
    NatShr(Term, Term),
    Int(i32),
    IntEql(Term, Term),
    IntNeq(Term, Term),
    IntAdd(Term, Term),
    IntSub(Term, Term),
    IntMul(Term, Term),
    IntDiv(Term, Term),
    IntRem(Term, Term),
    IntLt(Term, Term),
    IntGt(Term, Term),
    IntLte(Term, Term),
    IntGte(Term, Term),
    IntAnd(Term, Term),
    IntOr(Term, Term),
    IntXor(Term, Term),
    IntShl(Term, Term),
    IntShr(Term, Term),
    Flt(f32),
    FltAdd(Term, Term),
    FltSub(Term, Term),
    FltMul(Term, Term),
    FltDiv(Term, Term),
    FltRem(Term, Term),
    FltEql(Term, Term),
    FltNeq(Term, Term),
    FltLt(Term, Term),
    FltGt(Term, Term),
    FltLte(Term, Term),
    FltGte(Term, Term),
    FltMin(Term, Term),
    FltMax(Term, Term),
    FltNeg(Term),
    FltAbs(Term),
    FltSqrt(Term),
    FltFloor(Term),
    FltCeil(Term),
    FltTrunc(Term),
    FltNearest(Term),
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToLeBin(Term),
    FltToInt(Term),
    Bin(Vec<u8>),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Vec<Term>),
    BinFlatten(Term),
    Arr(Vec<Term>),
    ArrLen(Term),
    ArrGet(Term, Term),
    ArrSlice(Term, Term, Term),
    ArrAppend(Term, Term),
    ArrConcat(Vec<Term>),
    ArrFlatten(Term),
    // `ArrMap(src, f)`: map closure `f` over `src`, an O(n) fill. `f` is a
    // closure value; codegen emits one alloc + a fill loop applying `f` per slot.
    ArrMap(Term, Term),
    Io(u32),
    IoEql(Term, Term),
}

#[derive(Debug)]
pub enum HostPrim {
    IoRead(Term, Term),
    IoWrite(Term, Term),
    IoOpen(Term, Term),
    IoLookup(Term, Term),
    IoResolve(Term),
    IoSocket(Term),
    IoBind(Term, Term),
    IoConnect(Term, Term),
    IoListen(Term, Term),
    IoAccept(Term),
    IoStartTls(Term, Term),
    IoTlsServerConfig(Term, Term),
    IoStartTlsServer(Term, Term),
    IoSetNonblocking(Term, Term),
    IoSetRecvTimeout(Term, Term),
    IoSetSendTimeout(Term, Term),
    IoSetReuseaddr(Term, Term),
    IoPoll(Term, Term, Term),
    IoClose(Term),
    IoClockWall,
    IoClockMono,
    IoRandom(Term),
    IoArgs,
    IoEnv(Term),
    IoExit(Term),
}

#[derive(Debug)]
pub enum CellPrim {
    New(Term),       // init
    Set(Term, Term), // cell, value
    Get(Term),       // cell
}

#[derive(Debug)]
pub enum Prim {
    Pure(PurePrim),
    Host(HostPrim),
    Cell(CellPrim),
}

impl Prim {
    /// Free names of this primitive's operands (see [`Term::free_names`]).
    pub fn free_names(&self) -> BTreeSet<String> {
        self.operands().into_iter().flat_map(Term::free_names).collect()
    }

    /// The operand terms this primitive evaluates.
    pub fn operands(&self) -> Vec<&Term> {
        match self {
            Prim::Pure(p) => p.operands(),
            Prim::Host(h) => h.operands(),
            Prim::Cell(c) => c.operands(),
        }
    }

    /// Whether evaluating this primitive performs an observable action — a host
    /// effect or a cell operation — rather than a pure computation.
    pub fn is_effectful(&self) -> bool {
        matches!(self, Prim::Host(_) | Prim::Cell(_))
    }
}

impl PurePrim {
    fn operands(&self) -> Vec<&Term> {
        use PurePrim::*;

        match self {
            Nat(_) | Int(_) | Flt(_) | Bin(_) | Io(_) => vec![],
            NatToInt(a) | NatToFlt(a) | IntToNat(a) | IntToFlt(a) | FltToNat(a) | FltToLeBin(a)
            | FltToInt(a) | FltNeg(a) | FltAbs(a) | FltSqrt(a) | FltFloor(a) | FltCeil(a)
            | FltTrunc(a) | FltNearest(a) | BinLen(a) | ArrLen(a) | BinFlatten(a)
            | ArrFlatten(a) => vec![a],
            NatEql(a, b)
            | NatNeq(a, b)
            | NatAdd(a, b)
            | NatSub(a, b)
            | NatMul(a, b)
            | NatLt(a, b)
            | NatDiv(a, b)
            | NatRem(a, b)
            | NatGt(a, b)
            | NatLte(a, b)
            | NatGte(a, b)
            | NatAnd(a, b)
            | NatOr(a, b)
            | NatXor(a, b)
            | NatShl(a, b)
            | NatShr(a, b)
            | IntEql(a, b)
            | IntNeq(a, b)
            | IntAdd(a, b)
            | IntSub(a, b)
            | IntMul(a, b)
            | IntDiv(a, b)
            | IntRem(a, b)
            | IntLt(a, b)
            | IntGt(a, b)
            | IntLte(a, b)
            | IntGte(a, b)
            | IntAnd(a, b)
            | IntOr(a, b)
            | IntXor(a, b)
            | IntShl(a, b)
            | IntShr(a, b)
            | FltAdd(a, b)
            | FltSub(a, b)
            | FltMul(a, b)
            | FltDiv(a, b)
            | FltRem(a, b)
            | FltEql(a, b)
            | FltNeq(a, b)
            | FltLt(a, b)
            | FltGt(a, b)
            | FltLte(a, b)
            | FltGte(a, b)
            | FltMin(a, b)
            | FltMax(a, b)
            | BinEql(a, b)
            | IoEql(a, b)
            | BinGet(a, b)
            | BinAppend(a, b)
            | ArrGet(a, b)
            | ArrAppend(a, b)
            | ArrMap(a, b) => vec![a, b],
            BinSlice(a, b, c) | ArrSlice(a, b, c) => vec![a, b, c],
            BinConcat(operands) | ArrConcat(operands) | Arr(operands) => operands.iter().collect(),
        }
    }
}

impl HostPrim {
    fn operands(&self) -> Vec<&Term> {
        use HostPrim::*;

        match self {
            IoClockWall | IoClockMono | IoArgs => vec![],
            IoAccept(a) | IoResolve(a) | IoSocket(a) | IoClose(a) | IoRandom(a) | IoEnv(a)
            | IoExit(a) => vec![a],
            IoRead(a, b)
            | IoWrite(a, b)
            | IoOpen(a, b)
            | IoLookup(a, b)
            | IoBind(a, b)
            | IoConnect(a, b)
            | IoStartTls(a, b)
            | IoTlsServerConfig(a, b)
            | IoStartTlsServer(a, b)
            | IoListen(a, b)
            | IoSetNonblocking(a, b)
            | IoSetRecvTimeout(a, b)
            | IoSetSendTimeout(a, b)
            | IoSetReuseaddr(a, b) => vec![a, b],
            IoPoll(a, b, c) => vec![a, b, c],
        }
    }
}

impl CellPrim {
    fn operands(&self) -> Vec<&Term> {
        match self {
            CellPrim::New(a) | CellPrim::Get(a) => vec![a],
            CellPrim::Set(a, b) => vec![a, b],
        }
    }
}
