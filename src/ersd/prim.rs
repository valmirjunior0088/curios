use {super::Subterm, std::collections::BTreeSet};

#[derive(Debug)]
pub enum Prim {
    Nat(u32),
    NatEql(Subterm, Subterm),
    NatNeq(Subterm, Subterm),
    NatAdd(Subterm, Subterm),
    NatSub(Subterm, Subterm),
    NatMul(Subterm, Subterm),
    NatLt(Subterm, Subterm),
    NatDiv(Subterm, Subterm),
    NatRem(Subterm, Subterm),
    NatGt(Subterm, Subterm),
    NatLte(Subterm, Subterm),
    NatGte(Subterm, Subterm),
    NatToStr(Subterm),
    Int(i32),
    IntEql(Subterm, Subterm),
    IntNeq(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    IntDiv(Subterm, Subterm),
    IntRem(Subterm, Subterm),
    IntLt(Subterm, Subterm),
    IntGt(Subterm, Subterm),
    IntLte(Subterm, Subterm),
    IntGte(Subterm, Subterm),
    IntToStr(Subterm),
    Flt(f32),
    FltAdd(Subterm, Subterm),
    FltSub(Subterm, Subterm),
    FltMul(Subterm, Subterm),
    FltDiv(Subterm, Subterm),
    FltEql(Subterm, Subterm),
    FltNeq(Subterm, Subterm),
    FltLt(Subterm, Subterm),
    FltGt(Subterm, Subterm),
    FltLte(Subterm, Subterm),
    FltGte(Subterm, Subterm),
    FltMin(Subterm, Subterm),
    FltMax(Subterm, Subterm),
    FltNeg(Subterm),
    FltAbs(Subterm),
    FltSqrt(Subterm),
    FltFloor(Subterm),
    FltCeil(Subterm),
    FltTrunc(Subterm),
    FltNearest(Subterm),
    FltToStr(Subterm),
    NatToInt(Subterm),
    NatToFlt(Subterm),
    IntToNat(Subterm),
    IntToFlt(Subterm),
    FltToNat(Subterm),
    FltToInt(Subterm),
    Bin(Vec<u8>),
    BinLen(Subterm),
    BinEql(Subterm, Subterm),
    BinGet(Subterm, Subterm),
    BinSlice(Subterm, Subterm, Subterm),
    BinAppend(Subterm, Subterm),
    BinConcat(Vec<Subterm>),
    Arr(Vec<Subterm>),
    ArrLen(Subterm),
    ArrGet(Subterm, Subterm),
    ArrSlice(Subterm, Subterm, Subterm),
    ArrAppend(Subterm, Subterm),
    ArrConcat(Vec<Subterm>),
    Unit,
    IoPrint(Subterm),
    IoRead,
}

impl Prim {
    pub fn free_names(&self) -> BTreeSet<String> {
        use Prim::*;

        let operands: Vec<&Subterm> = match self {
            Nat(_) | Int(_) | Flt(_) | Bin(_) | Unit | IoRead => vec![],
            NatToStr(a) | IntToStr(a) | FltToStr(a) | NatToInt(a) | NatToFlt(a) | IntToNat(a)
            | IntToFlt(a) | FltToNat(a) | FltToInt(a) | FltNeg(a) | FltAbs(a) | FltSqrt(a)
            | FltFloor(a) | FltCeil(a) | FltTrunc(a) | FltNearest(a) | BinLen(a) | ArrLen(a)
            | IoPrint(a) => vec![a],
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
            | FltAdd(a, b)
            | FltSub(a, b)
            | FltMul(a, b)
            | FltDiv(a, b)
            | FltEql(a, b)
            | FltNeq(a, b)
            | FltLt(a, b)
            | FltGt(a, b)
            | FltLte(a, b)
            | FltGte(a, b)
            | FltMin(a, b)
            | FltMax(a, b)
            | BinEql(a, b)
            | BinGet(a, b)
            | BinAppend(a, b)
            | ArrGet(a, b)
            | ArrAppend(a, b) => vec![a, b],
            BinSlice(a, b, c) | ArrSlice(a, b, c) => vec![a, b, c],
            BinConcat(operands) | ArrConcat(operands) | Arr(operands) => operands.iter().collect(),
        };

        operands
            .into_iter()
            .flat_map(|operand| operand.free_names())
            .collect()
    }
}
