use {
    super::{Nat, Term},
    curios_abi::ForeignFunction,
    std::sync::Arc,
};

/// One entry of a list literal `[a, ..xs, b]` — a plain element, or a
/// `..`-spread whose term contributes a whole `Lst` run. Lowering groups
/// consecutive elements into literal chunks and splices the spreads with the
/// n-ary `Lst/concat`; a spread-free literal lowers to a plain `Lst` exactly
/// as before.
#[derive(Debug, Clone, PartialEq)]
pub enum LstEntry {
    Elem(Term),
    Spread(Term),
}

/// One segment of a `Bin` literal `\00\..bytes\01` — a run of literal bytes,
/// or a `\..`-spread whose term contributes a whole `Bin` run. The literal is
/// a single whitespace-free lexical unit; a spread operand is a glued name
/// path (projections included) or a parenthesized term.
#[derive(Debug, Clone, PartialEq)]
pub enum BinSegment {
    /// A run of literal bytes. Invariant (maintained by the parser): never
    /// empty, and never adjacent to another `Bytes` run.
    Bytes(Vec<u8>),
    Spread(Term),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Prim {
    BlnType,
    Bln(bool),
    BlnAnd(Term, Term),
    BlnOr(Term, Term),
    BlnXor(Term, Term),
    BlnEql(Term, Term),
    BlnNeq(Term, Term),
    NatType,
    Nat(Nat),
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
    IntType,
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
    FltType,
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
    BinType,
    Bin(Vec<BinSegment>),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Term, Term),
    LstType(Term),
    Lst(Vec<LstEntry>),
    LstLen(Term, Term),
    LstGet(Term, Term, Term),
    LstSlice(Term, Term, Term, Term),
    LstAppend(Term, Term, Term),
    LstConcat(Term, Term, Term),
    LstMap(Term, Term, Term, Term),
    IoType,
    Io(u32),
    IoEql(Term, Term),
    // A store-described host call; the prelude bakes it into the `/sys/Io`
    // declaration whose parameters the argument terms name.
    Foreign(Arc<ForeignFunction>, Vec<Term>),
    IoExit(Term, Term),
    CellType(Term),
    Cell(Term, Term),          // type, init
    CellSet(Term, Term, Term), // type, cell, value
    CellGet(Term, Term),       // type, cell
}
