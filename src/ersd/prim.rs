use super::Subterm;

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
    Int(i32),
    IntEql(Subterm, Subterm),
    IntNeq(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    IntNeg(Subterm),
    IntDiv(Subterm, Subterm),
    IntRem(Subterm, Subterm),
    IntLt(Subterm, Subterm),
    IntGt(Subterm, Subterm),
    IntLte(Subterm, Subterm),
    IntGte(Subterm, Subterm),
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
    NatToInt(Subterm),
    IntToNat(Subterm),
    IntToFlt(Subterm),
    NatToFlt(Subterm),
    FltToInt(Subterm),
    FltToNat(Subterm),
}

impl From<u32> for Prim {
    fn from(value: u32) -> Self {
        Self::Nat(value)
    }
}

impl From<i32> for Prim {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<f32> for Prim {
    fn from(value: f32) -> Self {
        Self::Flt(value)
    }
}

