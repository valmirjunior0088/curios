use super::{Subterm, Term};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flt(pub u32);

impl Flt {
    pub fn from_f32(v: f32) -> Self {
        Self(v.to_bits())
    }
    pub fn to_f32(self) -> f32 {
        f32::from_bits(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    NatType,
    Nat(u32),
    NatEql(Subterm, Subterm),
    NatNeq(Subterm, Subterm),
    NatAdd(Subterm, Subterm),
    NatSub(Subterm, Subterm),
    NatMul(Subterm, Subterm),
    NatDiv(Subterm, Subterm),
    NatRem(Subterm, Subterm),
    NatLt(Subterm, Subterm),
    NatGt(Subterm, Subterm),
    NatLte(Subterm, Subterm),
    NatGte(Subterm, Subterm),
    IntType,
    Int(i32),
    IntEql(Subterm, Subterm),
    IntAdd(Subterm, Subterm),
    IntSub(Subterm, Subterm),
    IntMul(Subterm, Subterm),
    IntNeq(Subterm, Subterm),
    IntDiv(Subterm, Subterm),
    IntRem(Subterm, Subterm),
    IntLt(Subterm, Subterm),
    IntGt(Subterm, Subterm),
    IntLte(Subterm, Subterm),
    IntGte(Subterm, Subterm),
    FltType,
    Flt(Flt),
    FltAdd(Subterm, Subterm),
    FltSub(Subterm, Subterm),
    FltMul(Subterm, Subterm),
    FltNeg(Subterm),
    FltAbs(Subterm),
    FltSqrt(Subterm),
    FltFloor(Subterm),
    FltCeil(Subterm),
    FltTrunc(Subterm),
    FltNearest(Subterm),
    FltDiv(Subterm, Subterm),
    FltMin(Subterm, Subterm),
    FltMax(Subterm, Subterm),
    FltEql(Subterm, Subterm),
    FltNeq(Subterm, Subterm),
    FltLt(Subterm, Subterm),
    FltGt(Subterm, Subterm),
    FltLte(Subterm, Subterm),
    FltGte(Subterm, Subterm),
    NatToInt(Subterm),
    IntToNat(Subterm),
    IntToFlt(Subterm),
    NatToFlt(Subterm),
    FltToInt(Subterm),
    FltToNat(Subterm),
    BinType,
    Bin(Vec<u8>),
    BinLen(Subterm),
    BinEql(Subterm, Subterm),
    BinGet(Subterm, Subterm),
    BinSlice(Subterm, Subterm, Subterm),
    BinAppend(Subterm, Subterm),
    BinConcat(Vec<Subterm>),
    ArrType(Subterm),
    Arr(Vec<Subterm>),
    ArrLen(Subterm),
    ArrGet(Subterm, Subterm),
    ArrSlice(Subterm, Subterm, Subterm),
    ArrAppend(Subterm, Subterm),
    ArrConcat(Vec<Subterm>),
}

impl Prim {
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into().into(), right.into().into())
    }

    pub fn nat_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatNeq(left.into().into(), right.into().into())
    }

    pub fn nat_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatAdd(left.into().into(), right.into().into())
    }

    pub fn nat_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatSub(left.into().into(), right.into().into())
    }

    pub fn nat_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatMul(left.into().into(), right.into().into())
    }

    pub fn nat_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatDiv(left.into().into(), right.into().into())
    }

    pub fn nat_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatRem(left.into().into(), right.into().into())
    }

    pub fn nat_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLt(left.into().into(), right.into().into())
    }

    pub fn nat_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGt(left.into().into(), right.into().into())
    }

    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLte(left.into().into(), right.into().into())
    }

    pub fn nat_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGte(left.into().into(), right.into().into())
    }

    pub fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into().into(), right.into().into())
    }

    pub fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into().into(), right.into().into())
    }

    pub fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into().into(), right.into().into())
    }

    pub fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into().into(), right.into().into())
    }

    pub fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntNeq(left.into().into(), right.into().into())
    }

    pub fn int_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntDiv(left.into().into(), right.into().into())
    }

    pub fn int_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntRem(left.into().into(), right.into().into())
    }

    pub fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into().into(), right.into().into())
    }

    pub fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGt(left.into().into(), right.into().into())
    }

    pub fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLte(left.into().into(), right.into().into())
    }

    pub fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGte(left.into().into(), right.into().into())
    }

    pub fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into().into(), right.into().into())
    }

    pub fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into().into(), right.into().into())
    }

    pub fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into().into(), right.into().into())
    }

    pub fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNeg(inner.into().into())
    }

    pub fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltAbs(inner.into().into())
    }

    pub fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(inner.into().into())
    }

    pub fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltFloor(inner.into().into())
    }

    pub fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltCeil(inner.into().into())
    }

    pub fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltTrunc(inner.into().into())
    }

    pub fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNearest(inner.into().into())
    }

    pub fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(left.into().into(), right.into().into())
    }

    pub fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMin(left.into().into(), right.into().into())
    }

    pub fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMax(left.into().into(), right.into().into())
    }

    pub fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltEql(left.into().into(), right.into().into())
    }

    pub fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltNeq(left.into().into(), right.into().into())
    }

    pub fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLt(left.into().into(), right.into().into())
    }

    pub fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGt(left.into().into(), right.into().into())
    }

    pub fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLte(left.into().into(), right.into().into())
    }

    pub fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGte(left.into().into(), right.into().into())
    }

    pub fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into().into())
    }

    pub fn int_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToNat(inner.into().into())
    }

    pub fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(inner.into().into())
    }

    pub fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(inner.into().into())
    }

    pub fn flt_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToInt(inner.into().into())
    }

    pub fn flt_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToNat(inner.into().into())
    }

    pub fn bin_len<B>(bin: B) -> Self
    where
        B: Into<Term>,
    {
        Self::BinLen(bin.into().into())
    }

    pub fn bin_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::BinEql(left.into().into(), right.into().into())
    }

    pub fn bin_get<B, I>(bin: B, index: I) -> Self
    where
        B: Into<Term>,
        I: Into<Term>,
    {
        Self::BinGet(bin.into().into(), index.into().into())
    }

    pub fn bin_slice<B, S, E>(bin: B, start: S, end: E) -> Self
    where
        B: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::BinSlice(bin.into().into(), start.into().into(), end.into().into())
    }

    pub fn bin_append<B, E>(bin: B, byte: E) -> Self
    where
        B: Into<Term>,
        E: Into<Term>,
    {
        Self::BinAppend(bin.into().into(), byte.into().into())
    }

    pub fn bin_concat<I>(operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::BinConcat(operands.into_iter().map(|e| e.into().into()).collect())
    }

    pub fn arr_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ArrType(elem.into().into())
    }

    pub fn arr_len<L>(list: L) -> Self
    where
        L: Into<Term>,
    {
        Self::ArrLen(list.into().into())
    }

    pub fn arr_get<L, I>(list: L, index: I) -> Self
    where
        L: Into<Term>,
        I: Into<Term>,
    {
        Self::ArrGet(list.into().into(), index.into().into())
    }

    pub fn arr_slice<L, S, E>(list: L, start: S, end: E) -> Self
    where
        L: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrSlice(list.into().into(), start.into().into(), end.into().into())
    }

    pub fn arr_append<L, E>(list: L, elem: E) -> Self
    where
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrAppend(list.into().into(), elem.into().into())
    }

    pub fn arr_concat<I>(operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::ArrConcat(operands.into_iter().map(|e| e.into().into()).collect())
    }
}

impl<A: Into<Term>> From<Vec<A>> for Prim {
    fn from(items: Vec<A>) -> Self {
        Self::Arr(items.into_iter().map(|a| a.into().into()).collect())
    }
}
