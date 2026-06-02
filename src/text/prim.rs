use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatLiteral {
    Number(u32),
    Char(char),
}

impl From<u32> for NatLiteral {
    fn from(n: u32) -> Self {
        NatLiteral::Number(n)
    }
}

impl From<char> for NatLiteral {
    fn from(c: char) -> Self {
        NatLiteral::Char(c)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nat {
    Zero,
    Succ(NatLiteral, Term),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinLiteral {
    Bytes(Vec<u8>),
    String(String),
}

impl<'a> From<&'a str> for BinLiteral {
    fn from(s: &'a str) -> Self {
        BinLiteral::String(s.to_string())
    }
}

impl<'a> From<&'a [u8]> for BinLiteral {
    fn from(bytes: &'a [u8]) -> Self {
        BinLiteral::Bytes(bytes.to_vec())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Prim {
    BlnType,
    Bln(bool),
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
    NatToStr(Term),
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
    IntToStr(Term),
    FltType,
    Flt(f32),
    FltAdd(Term, Term),
    FltSub(Term, Term),
    FltMul(Term, Term),
    FltDiv(Term, Term),
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
    FltToStr(Term),
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToInt(Term),
    BinType,
    Bin(BinLiteral),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Term, Term),
    ArrType(Term),
    Arr(Vec<Term>),
    ArrLen(Term, Term),
    ArrGet(Term, Term, Term),
    ArrSlice(Term, Term, Term, Term),
    ArrAppend(Term, Term, Term),
    ArrConcat(Term, Term, Term),
    IoPrint(Term),
    IoRead,
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

    pub fn nat_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToStr(inner.into().into())
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

    pub fn int_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToStr(inner.into().into())
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

    pub fn flt_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToStr(inner.into().into())
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

    pub fn bin_concat<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::BinConcat(left.into().into(), right.into().into())
    }

    pub fn arr_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ArrType(elem.into().into())
    }

    pub fn arr_len<T, L>(type_: T, list: L) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
    {
        Self::ArrLen(type_.into().into(), list.into().into())
    }

    pub fn arr_get<T, L, I>(type_: T, list: L, index: I) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
    {
        Self::ArrGet(type_.into().into(), list.into().into(), index.into().into())
    }

    pub fn arr_slice<T, L, S, E>(type_: T, list: L, start: S, end: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrSlice(
            type_.into().into(),
            list.into().into(),
            start.into().into(),
            end.into().into(),
        )
    }

    pub fn arr_append<T, L, E>(type_: T, list: L, elem: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrAppend(type_.into().into(), list.into().into(), elem.into().into())
    }

    pub fn arr_concat<T, F, S>(type_: T, left: F, right: S) -> Self
    where
        T: Into<Term>,
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::ArrConcat(type_.into().into(), left.into().into(), right.into().into())
    }

    pub fn io_print<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IoPrint(inner.into().into())
    }
}

impl<A: Into<Term>> From<Vec<A>> for Prim {
    fn from(items: Vec<A>) -> Self {
        Self::Arr(items.into_iter().map(|item| item.into().into()).collect())
    }
}
